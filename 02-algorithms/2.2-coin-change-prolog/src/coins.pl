:- encoding(utf8).
:- initialization(main, main).

:- table ways/3.
:- table fewest/3.

ways(0, _, 1).
ways(S, [], 0) :- S > 0.
ways(S, _, 0) :- S < 0.
ways(S, [C|Cs], N) :-
    S > 0,
    S1 is S - C,
    ways(S1, [C|Cs], N1),
    ways(S, Cs, N2),
    N is N1 + N2.

way(0, _, []).
way(S, [C|Cs], [C|W]) :-
    S > 0,
    S1 is S - C,
    S1 >= 0,
    way(S1, [C|Cs], W).
way(S, [_|Cs], W) :-
    S > 0,
    way(S, Cs, W).

fewest(0, _, []).
fewest(S, Coins, Best) :-
    S > 0,
    findall(L-[C|W],
            ( member(C, Coins),
              C =< S,
              S1 is S - C,
              fewest(S1, Coins, W),
              length([C|W], L) ),
            Pairs),
    Pairs \= [],
    keysort(Pairs, [_-Best|_]).

fail_with(Msg) :-
    format(user_error, "Ошибка: ~w~n", [Msg]),
    halt(1).

to_int(Text, N) :-
    catch(number_codes(N, Text), _, fail),
    integer(N).

show_way([], "(без монет)") :- !.
show_way(W, Text) :-
    atomic_list_concat(W, ' + ', Text).

read_input(Sum, Coins) :-
    format("сумма: "),
    flush_output,
    read_line_to_string(user_input, L1),
    format("номиналы через пробел: "),
    flush_output,
    read_line_to_string(user_input, L2),
    parse_input(L1, L2, Sum, Coins).

parse_input(L1, L2, [], []) :-
    ( L1 == end_of_file ; L2 == end_of_file ),
    !.
parse_input(L1, L2, Sum, Coins) :-
    split_string(L1, " ", " ", Parts1),
    exclude(==(""), Parts1, P1),
    ( P1 = [S|_] -> string_codes(S, Sum) ; Sum = [] ),
    split_string(L2, " ", " ", Parts2),
    exclude(==(""), Parts2, P2),
    maplist([X, C]>>string_codes(X, C), P2, Coins).

main :-
    set_stream(user_output, encoding(utf8)),
    set_stream(user_error, encoding(utf8)),
    set_stream(user_input, encoding(utf8)),
    current_prolog_flag(argv, Argv),
    (   Argv = []
    ->  read_input(SumCodes, CoinCodes)
    ;   Argv = [SumAtom|CoinAtoms],
        atom_codes(SumAtom, SumCodes),
        maplist(atom_codes, CoinAtoms, CoinCodes)
    ),
    run(SumCodes, CoinCodes).

run(SumCodes, CoinCodes) :-
    (   to_int(SumCodes, Sum), Sum >= 0
    ->  true
    ;   fail_with("сумма должна быть неотрицательным целым числом")
    ),
    (   CoinCodes == []
    ->  fail_with("нужно указать хотя бы один номинал")
    ;   true
    ),
    (   maplist(to_int, CoinCodes, Raw), forall(member(C, Raw), C > 0)
    ->  true
    ;   fail_with("номиналы должны быть положительными целыми числами")
    ),
    sort(0, @>, Raw, Coins),
    atomic_list_concat(Coins, ' ', CoinsText),
    format("сумма: ~w, номиналы: ~w~n", [Sum, CoinsText]),
    ways(Sum, Coins, N),
    format("способов: ~w~n", [N]),
    (   N =:= 0
    ->  format("размен невозможен~n")
    ;   N =< 30
    ->  forall(way(Sum, Coins, W), (show_way(W, T), format("~w~n", [T])))
    ;   format("список не выводится: больше 30 способов~n")
    ),
    (   N > 0, fewest(Sum, Coins, Best)
    ->  msort(Best, Up),
        reverse(Up, Down),
        length(Down, Len),
        (   Len =:= 0
        ->  format("минимум монет: 0~n")
        ;   show_way(Down, BestText),
            format("минимум монет: ~w (~w)~n", [Len, BestText])
        )
    ;   true
    ),
    (   N =< 1000
    ->  findall(W, way(Sum, Coins, W), All),
        length(All, M),
        ( M =:= N -> R = "да" ; R = "нет" ),
        format("проверка перебором: найдено ~w, совпадает: ~w~n", [M, R])
    ;   true
    ).
