-module(prime_client).
-export([main/1]).

main([HostAtom, PortAtom]) ->
    io:setopts([{encoding, unicode}]),
    Host = atom_to_list(HostAtom),
    Port = parse_port(PortAtom),
    case gen_tcp:connect(Host, Port, [binary, {packet, line}, {active, false}], 5000) of
        {ok, Socket} ->
            io:format("подключено к ~s:~p~n", [Host, Port]),
            Tty = proplists:get_value(terminal, io:getopts()) =:= true,
            loop(Socket, Tty),
            halt(0);
        {error, Reason} ->
            fail(io_lib:format("не удалось подключиться к ~s:~p: ~p", [Host, Port, Reason]))
    end;
main(_) ->
    fail("использование: erl -noshell -pa ebin -s prime_client main <хост> <порт>").

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

read_line(true) -> io:get_line("> ");
read_line(false) -> io:get_line("").

loop(Socket, Tty) ->
    case read_line(Tty) of
        eof ->
            gen_tcp:send(Socket, <<"QUIT\n">>),
            gen_tcp:recv(Socket, 0, 1000),
            gen_tcp:close(Socket);
        {error, _} ->
            gen_tcp:close(Socket);
        Raw ->
            Line = string:trim(Raw),
            case Line of
                "" ->
                    loop(Socket, Tty);
                _ ->
                    Tty orelse io:format("> ~ts~n", [Line]),
                    gen_tcp:send(Socket, unicode:characters_to_binary(Line ++ "\n")),
                    case gen_tcp:recv(Socket, 0, 10000) of
                        {ok, Answer} ->
                            io:format("~ts~n", [string:trim(unicode:characters_to_list(Answer))]),
                            case Line of
                                "QUIT" -> gen_tcp:close(Socket);
                                _ -> loop(Socket, Tty)
                            end;
                        {error, _} ->
                            fail("сервер не отвечает")
                    end
            end
    end.
