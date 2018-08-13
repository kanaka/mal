:- set_prolog_flag(double_quotes, chars).

:- use_module(library(assoc)).
:- use_module(parser, [mal_forms/3]).

mal_arithmetic(_, [V], V).
mal_arithmetic(F, [integer(V)|Vals], integer(Sum)) :-
    mal_arithmetic(F, Vals, integer(S1)),
    Calc =.. [F, V, S1],
    Sum is Calc.

mal_plus(Vals, Sum) :- mal_arithmetic('+', Vals, Sum).
mal_minus(Vals, Sum) :- mal_arithmetic('-', Vals, Sum).
mal_times(Vals, Sum) :- mal_arithmetic('*', Vals, Sum).
mal_div(Vals, Sum) :- mal_arithmetic('div', Vals, Sum).

read_chars(end_of_file, _) :-
    throw(end_of_file).
read_chars(Chars, AST) :-
    phrase(mal_forms(AST), Chars), !.

mal_read(Stream, AST) :-
    read_line_to_string(Stream, String),
    string_chars(String, Chars),
    read_chars(Chars, AST).

env(Env), [Env, Val] --> [Env, Val].

result(Val), [Env, Val] --> { var(Val) }, [Env, Val].
result(Val), [Env, Val] --> [Env, _].
result(Val0, Val), [Env, Val] --> [Env, Val0].

values([Expr]) -->
    mal_eval(Expr),
    result(Val, [Val]).
values([Expr | Exprs]) -->
    mal_eval(Expr),
    result(Val),
    values(Exprs),
    result(Vals, [Val | Vals]).

mal_eval(list([])) --> result(list([])).
mal_eval(integer(N)) --> result(integer(N)).
mal_eval(keyword(K)) --> result(keyword(K)).
mal_eval(string(S)) --> result(string(S)).
mal_eval(symbol(Sym)) -->
    env(Env),
    { get_assoc(Sym, Env, Result) },
    result(Result).
mal_eval([Expr]) --> mal_eval(Expr).
mal_eval([Expr | Exprs]) -->
    mal_eval(Expr),
    mal_eval(Exprs).
mal_eval(list([symbol(Sym) | Exprs])) -->
    values(Exprs),
    result(Vals, Result),
    env(Env),
    { get_assoc(Sym, Env, Functor),
      call(Functor, Vals, Result) }.
mal_eval(vector(Exprs)) -->
    values(Exprs),
    result(Vals, vector(Vals)).
mal_eval(hash_map(Exprs)) -->
    values(Exprs),
    result(Vals, hash_map(Vals)).

mal_print(Stream, Expr) :-
    phrase(mal_forms(Expr), Codes), !,
    format(Stream, '~s~N', [Codes]).

mal_rep(IStream, Env, OStream) :-
    mal_read(IStream, AST),
    phrase(mal_eval(AST), [Env, nil], [_, Result]),
    mal_print(OStream, [Result]).

handle_error(syntax_error(Message)) :-
    format('~s~N', [Message]).
handle_error(end_of_file) :-
    halt.

main(Env) :-
    prompt(_, 'user> '),
    ignore(catch(mal_rep(current_input, Env, current_output),
		 Error,
		 handle_error(Error))),
    main(Env).

main :-
    list_to_assoc(
        [
            '+'-mal_plus,
 	    '-'-mal_minus,
 	    '*'-mal_times,
 	    '/'-mal_div
        ],
        Env
    ),
    main(Env).
