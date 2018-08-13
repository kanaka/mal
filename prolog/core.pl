:- module(core, [
	      mal_count/2,
	      mal_div/2,
	      mal_emptyq/2,
	      mal_eq/2,
	      mal_gt/2,
	      mal_gte/2,
	      mal_list/2,
	      mal_listq/2,
	      mal_lt/2,
	      mal_lte/2,
	      mal_minus/2,
	      mal_plus/2,
	      mal_pr_str/2,
	      mal_println/2,
	      mal_prn/2,
	      mal_str/2,
	      mal_times/2,
              mal_ns/1
	  ]).

:- use_module(library(assoc)).
:- use_module(parser, [print_readably/1]).


mal_arithmetic(_, [V], V).
mal_arithmetic(F, [integer(V)|Vals], integer(Sum)) :-
    mal_arithmetic(F, Vals, integer(S1)),
    Calc =.. [F, V, S1],
    Sum is Calc.

mal_plus(Vals, Sum) :- mal_arithmetic('+', Vals, Sum).
mal_minus(Vals, Sum) :- mal_arithmetic('-', Vals, Sum).
mal_times(Vals, Sum) :- mal_arithmetic('*', Vals, Sum).
mal_div(Vals, Sum) :- mal_arithmetic('div', Vals, Sum).

mal_list(Vals, list(Vals)).

mal_listq([list(_)], true).
mal_listq([_], false).

mal_emptyq([list([])], true).
mal_emptyq([vector([])], true).
mal_emptyq([_], false).

mal_count([nil], integer(0)).
mal_count([list(L)], integer(N)) :- length(L, N).
mal_count([vector(L)], integer(N)) :- length(L, N).

mal_lt([integer(V1), integer(V2)], true) :-
    V1 > V2.
mal_lt(_, false).

mal_lte([integer(V1), integer(V2)], true) :-
    V1 >= V2.
mal_lte(_, false).

mal_eq([vector(V1), list(V2)], true) :-
    mal_eq([list(V2), vector(V1)], true).
mal_eq([list(V1), vector(V2)], true) :-
    mal_eq([V1, V2], true).
mal_eq([[A|As], [B|Bs]], true) :-
    mal_eq([A, B], true),
    mal_eq([As, Bs], true).
mal_eq([V1, V2], true) :- V1 = V2.
mal_eq(_, false).

mal_gt([integer(V1), integer(V2)], true) :- V1 < V2.
mal_gt(_, false).

mal_gte([integer(V1), integer(V2)], true) :- V1 =< V2.
mal_gte(_, false).

mal_pr_str([], string("")).
mal_pr_str(Forms, string(Str)) :-
    phrase(mal_forms(Forms), Chars),
    string_chars(Str, Chars).

mal_str_([], "").
mal_str_([Form | Forms], Str) :-
    mal_pr_str([Form], string(Str_1)),
    mal_str_(Forms, Str_2),
    format(string(Str), "~s~s", [Str_1, Str_2]).

mal_str(Forms, string(Str)) :-
    asserta(print_readably(false)),
    mal_str_(Forms, Str),
    retract(print_readably(false)).
mal_str(_, _) :-
    retract(print_readably(false)), !, fail.

mal_prn(Forms, nil) :-
    mal_pr_str(Forms, string(Str)),
    format('~s~n', [Str]).

mal_println([], nil) :- nl.
mal_println([Form], nil) :-
    mal_str([Form], string(Str)),
    format('~s~n', [Str]).
mal_println([Form | Forms], nil) :-
    mal_str([Form], string(Str)),
    format('~s ', [Str]),
    mal_println(Forms, _).


mal_atom([Arg], atom(Atom)) :-
    nb_current('atom#', N),
    N1 is N + 1,
    format(atom(Atom), 'atom#~|~`0t~w~10|', [N1]),
    nb_setval(Atom, Arg),
    nb_setval('atom#', N1).
mal_atom(Args, Atom) :-
    nb_setval('atom#', 0),
    mal_atom(Args, Atom).

mal_ns(NS) :-
    list_to_assoc(
        [
            '*'-mal_times,
            '+'-mal_plus,
            '-'-mal_minus,
            '/'-mal_div,
            '<'-mal_gt,
            '<='-mal_gte,
            '='-mal_eq,
            '>'-mal_lt,
            '>='-mal_lte,
            'count'-mal_count,
            'deref'-mal_deref,
            'empty?'-mal_emptyq,
            'list'-mal_list,
            'list?'-mal_listq,
            'pr-str'-mal_pr_str,
            'println'-mal_println,
            'prn'-mal_prn,
            'str'-mal_str
        ],
        NS
    ).
