:- use_module(library(assoc)).
:- use_module(parser, [mal_forms/3]).
:- use_module(core).

:- set_prolog_flag(double_quotes, chars).

chars_ast(end_of_file, _) :- throw(end_of_file).
chars_ast(Chars, AST) :- phrase(mal_forms(AST), Chars), !.

mal_read(Stream, AST) :-
    read_line_to_string(Stream, String),
    string_chars(String, Chars),
    chars_ast(Chars, AST).

global(Global0, Global) -->
    envs([Env | Envs], Envs),
    global(Global0, Global),
    envs(Envs_, [Env | Envs_]).
global(Global0, Global) -->
    envs([Global0], [Global]).

env(Env), [Val, Env | Envs] --> { var(Env) }, [Val, Env | Envs].
env(Env), [Val, Env | Envs] --> [Val, _ | Envs].
env(Env0, Env), [Val, Env | Envs] --> [Val, Env0 | Envs].

envs(Envs), [Val | Envs] --> [Val | Envs].
envs(Envs0, Envs), [Val | Envs] --> [Val | Envs0].

result(Val), [Val, Env | Envs] --> { var(Val) }, [Val, Env | Envs].
result(Val), [Val, Env | Envs] --> [_, Env | Envs].
result(Val0, Val), [Val, Env | Envs] --> [Val0, Env | Envs].

values([]) --> result([]).
values([Expr | Exprs]) -->
    mal_eval(Expr),
    result(Val),
    values(Exprs),
    result(Vals, [Val | Vals]).

value_of(symbol(Sym)) -->
    envs([Env | _]),
    { get_assoc(Sym, Env, Value) },
    result(Value).
value_of(symbol(Sym)) -->
    envs([Env | Envs], Envs),
    value_of(symbol(Sym)),
    envs(_, [Env | Envs]).
value_of(symbol(Sym)) -->
    { format(atom(Error), 'No such symbol in env ~w', Sym),
      throw(symbol_error(Error)) }.

mal_let([], Body) --> mal_eval(Body).
mal_let([symbol(Sym), Expr | Vars], Body) -->
    mal_eval(Expr),
    env(Env0, Env),
    result(Val),
    { put_assoc(Sym, Env0, Val, Env) },
    mal_let(Vars, Body).

mal_truthy('false', _, Else) --> mal_eval(Else).
mal_truthy('nil', _, Else) --> mal_eval(Else).
mal_truthy(_, Then, _) --> mal_eval(Then).

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

mal_apply(fn(env=Env, args=Args, body=Body)) -->
    envs(Envs, [Env | Envs]),
    binds(Args),
    mal_eval(Body),
    envs(_, Envs).
mal_apply(Fn) -->
    result(Vals, Result),
    { call(Fn, Vals, Result) }.

quasiquote(list([symbol('unquote'), Value])) -->
    mal_eval(Value),
    result(Result, [Result]).
quasiquote(list([symbol('splice-unquote'), Value])) -->
    mal_eval(Value),
    result(Result),
    { list(Values) = Result;
      throw(error("splice-unquote does not return list")) },
    result(Values).
quasiquote(vector(List)) -->
    quasiquote(list(List)).
quasiquote(list(List)) -->
    quasiquote(List),
    result(Result, [list(Result)]).
quasiquote([]) --> result([]).
quasiquote([Head | Tail]) -->
    quasiquote(Head),
    result(First),
    quasiquote(Tail),
    result(Rest),
    { append(First, Rest, List) },
    result(List).
quasiquote(Value) --> result([Value]).

mal_eval('false') --> result('false').
mal_eval('true') --> result('true').
mal_eval('nil') --> result('nil').
mal_eval(integer(N)) --> result(integer(N)).
mal_eval(keyword(K)) --> result(keyword(K)).
mal_eval(string(S)) --> result(string(S)).
mal_eval(symbol(Sym)) --> value_of(symbol(Sym)).
mal_eval([]) --> result(Result, Result).
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
    global(Env0, Env),
    { put_assoc(Sym, Env0, Val, Env) }.
mal_eval(list([symbol('let*'), Vars | Body])) -->
    { empty_assoc(Env),
      list(Variables) = Vars ;
      vector(Variables) = Vars },
    envs(Envs, [Env | Envs]),
    mal_let(Variables, Body),
    envs(_, Envs).
mal_eval(list([symbol('eval'), Expr])) -->
    mal_eval(Expr),
    result(Val),
    mal_eval(Val).
mal_eval(list([symbol('swap!'), Sym, Fn | Exprs])) -->
    values(Exprs),
    result(Values),
    mal_eval(Sym),
    result(Atom),
    { mal_deref([Atom], Value) },
    mal_eval(list([Fn, Value | Values])),
    result(Result),
    { mal_reset([Atom, Result], _)}.
mal_eval(list([symbol('quote'), Value])) -->
    result(Value).
mal_eval(list([symbol('quasiquote'), Value])) -->
    quasiquote(Value),
    result([Result], Result).
mal_eval(list([Expr | Exprs])) -->
    mal_eval(Expr),
    result(Fn),
    values(Exprs),
    mal_apply(Fn).
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
    phrase(mal_eval(AST), [nil, Env0], [Result, Env]),
    mal_print(OStream, [Result]).

handle_error(syntax_error(Message)) :-
    format('~s~N', [Message]).
handle_error(end_of_file) :- halt.
handle_error(E) :-
    format('~w~N', [E]).

namespace(NS) :-
    mal_ns(Env0),
    put_assoc('eval', Env0, mal_eval, Env1),
    open_null_stream(Ignore),
    open_string("(def! not (fn* (v) (if v false true)))", Not),
    mal_rep(Not, Env1, Env2, Ignore),
    open_string("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))", Load),
    mal_rep(Load, Env2, Env3, Ignore),
    put_assoc('*ARGV*', Env3, list([]), NS).

repl(Env0) :-
    ignore(catch(
        mal_rep(current_input, Env0, Env, current_output),
	Error,
	handle_error(Error)
    ) ; Env = Env0),
    repl(Env).

repl :-
    namespace(NS),
    prompt(_, 'user> '),
    repl(NS).

argv([], []).
argv([Arg | Args], [string(A) | Rest]) :-
    atom_string(Arg, A),
    argv(Args, Rest).

run_file :-
    current_prolog_flag(argv, ARGV),
    [File | Arguments] = ARGV,
    atom_string(File, F),
    argv(Arguments, MalArgs),
    namespace(Env0),
    put_assoc('*ARGV*', Env0, list(MalArgs), Env),
    phrase(mal_eval(list([symbol('load-file'), string(F)])), [nil, Env], _).

main:-
    run_file ; repl.
