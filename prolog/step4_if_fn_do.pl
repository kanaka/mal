:- use_module(library(assoc)).
:- use_module(parser, [mal_forms/3]).
:- use_module(core).

:- set_prolog_flag(double_quotes, chars).

chars_ast(end_of_file, _) :-
    throw(end_of_file).
chars_ast(Chars, AST) :-
    phrase(mal_forms(AST), Chars), !.

mal_read(Stream, AST) :-
    read_line_to_string(Stream, String),
    string_chars(String, Chars),
    chars_ast(Chars, AST).

env(Env), [Env, Val] --> { var(Env) }, [Env, Val].
env(Env), [Env, Val] --> [_, Val].
env(Env0, Env), [Env, Val] --> [Env0, Val].

result(Val), [Env, Val] --> { var(Val) }, [Env, Val].
result(Val), [Env, Val] --> [Env, _].
result(Val0, Val), [Env, Val] --> [Env, Val0].

values([]) --> result([]).
values([Expr | Exprs]) -->
    mal_eval(Expr),
    result(Val),
    values(Exprs),
    result(Vals, [Val | Vals]).

mal_let([], Body) -->
    mal_eval(Body).
mal_let([symbol(Sym), Expr | Vars], Body) -->
    mal_eval(Expr),
    env(Env0, Env),
    result(Val),
    { put_assoc(Sym, Env0, Val, Env) },
    mal_let(Vars, Body).

mal_truthy('false', _, Else) -->
    mal_eval(Else).
mal_truthy('nil', _, Else) -->
    mal_eval(Else).
mal_truthy(_, Then, _) -->
    mal_eval(Then).

binds([]) --> result('nil').
binds([symbol('&'), symbol(More)]) -->
    result(Vals, 'nil'),
    env(Env0, Env),
    { put_assoc(More, Env0, list(Vals), Env )}.
binds([symbol(Arg) | Args]) -->
    result([Val | Vals], Vals),
    env(Env0, Env),
    { put_assoc(Arg, Env0, Val, Env) },
    binds(Args).

extend_(K-V, A0, A) :-
    put_assoc(K, A0, V, A).

extend(Env) -->
    env(Env0, Env1),
    { assoc_to_list(Env, List),
      foldl(extend_, List, Env0, Env1) }.

mal_apply(fn(env=Env, args=Args, body=Body)) -->
    extend(Env),
    binds(Args),
    mal_eval(Body).
mal_apply(Fn) -->
    result(Vals, Result),
    { call(Fn, Vals, Result) }.

mal_eval('false') --> result('false').
mal_eval('true') --> result('true').
mal_eval('nil') --> result('nil').
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
mal_eval(list([])) --> result(list([])).
mal_eval(list([symbol('fn*'), Arguments, Body])) -->
    { list(Args) = Arguments ;
      vector(Args) = Arguments },
    env(Env),
    result(fn(env=Env, args=Args, body=Body)).
mal_eval(list([symbol('if'), Test, Then])) -->
    mal_eval(list([symbol('if'), Test, Then, nil])).
mal_eval(list([symbol('if'), Test, Then, Else])) -->
    mal_eval(Test),
    result(T),
    mal_truthy(T, Then, Else).
mal_eval(list([symbol('do') | Exprs])) -->
    mal_eval(Exprs).
mal_eval(list([symbol('def!'), symbol(Sym), Expr])) -->
    mal_eval(Expr),
    result(Val),
    env(Env0, Env),
    { put_assoc(Sym, Env0, Val, Env) }.
mal_eval(list([symbol('let*'), Vars | Body])) -->
    env(Env0),
    { list(Variables) = Vars ;
      vector(Variables) = Vars },
    mal_let(Variables, Body),
    env(_, Env0).
mal_eval(list([Expr | Exprs])) -->
    mal_eval(Expr),
    result(Fn),
    values(Exprs),
    env(Env0),
    mal_apply(Fn),
    env(_, Env0).
mal_eval(vector(Exprs)) -->
    values(Exprs),
    result(Vals, vector(Vals)).
mal_eval(hash_map(Exprs)) -->
    values(Exprs),
    result(Vals, hash_map(Vals)).

mal_print(Stream, Expr) :-
    phrase(mal_forms(Expr), Chars), !,
    format(Stream, '~s~N', [Chars]).

mal_rep(IStream, Env0, Env, OStream) :-
    mal_read(IStream, AST),
    phrase(mal_eval(AST), [Env0, nil], [Env, Result]),
    mal_print(OStream, [Result]).

handle_error(syntax_error(Message)) :-
    format('~s~N', [Message]).
handle_error(end_of_file) :-
    halt.

main(Env0) :-
    prompt(_, 'user> '),
    ignore(
        catch(
            mal_rep(current_input, Env0, Env, current_output),
	    Error,
	    handle_error(Error)
        ) ;
        Env = Env0
    ),
    main(Env).

main :-
    mal_ns(Env0),
    chars_ast("(def! not (fn* (v) (if v false true)))", AST),
    phrase(mal_eval(AST), [Env0, nil], [Env, _]),
    main(Env).
