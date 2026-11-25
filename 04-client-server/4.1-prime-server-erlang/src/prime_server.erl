-module(prime_server).
-export([main/1, start/1]).

-define(TRIAL_LIMIT, 1000000000000).

main([PortAtom]) ->
    io:setopts([{encoding, unicode}]),
    start(parse_port(PortAtom));
main(_) ->
    fail("использование: erl -noshell -pa ebin -s prime_server main <порт>").

parse_port(Atom) ->
    try list_to_integer(atom_to_list(Atom)) of
        P when P > 0, P < 65536 -> P;
        _ -> fail("порт должен быть числом от 1 до 65535")
    catch
        _:_ -> fail("порт должен быть числом от 1 до 65535")
    end.

fail(Msg) ->
    io:format(standard_error, "Ошибка: ~ts~n", [Msg]),
    halt(1).

start(Port) ->
    Opts = [binary, {packet, line}, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opts) of
        {ok, Listen} ->
            io:format("сервер запущен на порту ~p~n", [Port]),
            accept(Listen, 1);
        {error, Reason} ->
            fail(io_lib:format("не удалось открыть порт ~p: ~p", [Port, Reason]))
    end.

accept(Listen, N) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    Pid = spawn(fun() ->
        receive go -> ok end,
        io:format("клиент ~p подключился~n", [N]),
        loop(Socket, N)
    end),
    ok = gen_tcp:controlling_process(Socket, Pid),
    Pid ! go,
    accept(Listen, N + 1).

loop(Socket, N) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Line = string:trim(unicode:characters_to_list(Data)),
            case Line of
                "QUIT" ->
                    gen_tcp:send(Socket, <<"BYE\n">>),
                    gen_tcp:close(Socket),
                    io:format("клиент ~p отключился~n", [N]);
                _ ->
                    Answer = answer(Line),
                    io:format("клиент ~p: ~ts -> ~s~n", [N, Line, Answer]),
                    gen_tcp:send(Socket, unicode:characters_to_binary(Answer ++ "\n")),
                    loop(Socket, N)
            end;
        {error, _} ->
            io:format("клиент ~p отключился~n", [N])
    end.

answer(Line) ->
    case string:to_integer(Line) of
        {N, []} when N < 0 -> "ERROR negative number";
        {N, []} when N < 2 -> "NEITHER";
        {N, []} -> check(N);
        _ -> "ERROR not an integer"
    end.

check(N) ->
    case divisor(N) of
        none -> "PRIME";
        unknown -> "COMPOSITE";
        D -> "COMPOSITE " ++ integer_to_list(D)
    end.

divisor(N) when N < 4 -> none;
divisor(N) when N rem 2 =:= 0 -> 2;
divisor(N) when N rem 3 =:= 0 -> 3;
divisor(N) when N < ?TRIAL_LIMIT -> trial(N, 5, isqrt(N));
divisor(N) ->
    case trial(N, 5, 1000000) of
        none ->
            case miller_rabin(N) of
                true -> none;
                false -> unknown
            end;
        D -> D
    end.

trial(_, K, Limit) when K > Limit -> none;
trial(N, K, _) when N rem K =:= 0 -> K;
trial(N, K, _) when N rem (K + 2) =:= 0 -> K + 2;
trial(N, K, Limit) -> trial(N, K + 6, Limit).

isqrt(N) -> isqrt(N, N).

isqrt(N, X) ->
    Y = (X + N div X) div 2,
    if
        Y >= X -> X;
        true -> isqrt(N, Y)
    end.

miller_rabin(N) ->
    {D, S} = split(N - 1, 0),
    Bases = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37],
    lists:all(fun(A) -> witness_ok(A, D, S, N) end, Bases).

split(D, S) when D rem 2 =:= 0 -> split(D div 2, S + 1);
split(D, S) -> {D, S}.

witness_ok(A, D, S, N) ->
    X = powmod(A, D, N),
    X =:= 1 orelse X =:= N - 1 orelse square_loop(X, S - 1, N).

square_loop(_, 0, _) -> false;
square_loop(X, K, N) ->
    Y = X * X rem N,
    Y =:= N - 1 orelse square_loop(Y, K - 1, N).

powmod(_, 0, _) -> 1;
powmod(B, E, M) when E rem 2 =:= 0 ->
    H = powmod(B, E div 2, M),
    H * H rem M;
powmod(B, E, M) ->
    B * powmod(B, E - 1, M) rem M.
