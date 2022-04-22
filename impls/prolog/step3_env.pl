% -*- mode: prolog; -*- select prolog mode in the emacs text editor

:- initialization(main, main).

:- consult([env, printer, reader, types, utils]).

% Read

mal_read(Ast) :-
    write("user> "),
    read_line_to_string(current_input, Line),
    (Line = end_of_file -> throw(exit_repl) ; true),
    (rl_add_history(Line) -> true ; true), % fails for duplicate lines
    read_str(Line, Ast).

% Eval non-empty list depending on their first element.

eval_list(Env, 'def!', Args, Res) :- !,
    check(Args = [Key, Form], "def!: expects 2 arguments, got: ~L", [Args]),
    check(atom(Key), "def!: ~F is not a symbol", [Key]),
    eval(Env, Form, Res),
    env_set(Env, Key, Res).

eval_list(Env, 'let*', Args, Res) :- !,
    check(Args = [Binds, Form], "let*: expects 2 arguments, got: ~L", [Args]),
    check(unbox_seq(Binds, Xs), "let*: ~F is not a sequence", [Binds]),
    env(Env, Let_Env),
    check(map_keyvals(let_loop(Let_Env), Xs), "let*: odd length: ~L", [Binds]),
    eval(Let_Env, Form, Res).

let_loop(Env, Key, Form) :- !,
    check(atom(Key), "let*: ~F is not a key", [Key]),
    eval(Env, Form, Value),
    env_set(Env, Key, Value).

% apply phase

eval_list(Env, First, Rest, Res) :-
    eval(Env, First, Fn),
    check(mal_fn(Goal, Fn), "cannot apply, ~F is not a function", [Fn]),
    maplist(eval(Env), Rest, Args),
    call(Goal, Args, Res).

% The eval function itself.

% Uncomment this to get a trace with environments.
%% eval(Env, Ast, _) :-
%%     format("EVAL: ~F    in ~V\n", [Ast, Env]),
%%     fail.                       % Proceed with normal alternatives.

eval(Env, List, Res) :-
    list([First | Args], List), !,
    eval_list(Env, First, Args, Res).

eval(_,   nil,    nil).
eval(_,   true,   true).
eval(_,   false,  false).
eval(Env, Symbol, Res) :-
    atom(Symbol), !,
    check(env_get(Env, Symbol, Res), "'~F' not found", [Symbol]).

eval(Env, Vector, Res) :-
    vector(Xs, Vector), !,
    maplist(eval(Env), Xs, Ys),
    vector(Ys, Res).

eval(Env, Map, Res) :- map_map(eval(Env), Map, Res).

eval(_, Anything_Else, Anything_Else).

% Print

print(Ast) :- format("~F\n", [Ast]).

% REP

rep(Env) :-
    mal_read(Ast),
    eval(Env, Ast, Evaluated),
    print(Evaluated).

% Main program

repl(Env) :-
    catch(rep(Env), mal_error(Message), writeln(Message)),
    repl(Env).

add([X, Y], Res) :- integer(X), integer(Y),          Res is X + Y.
sub([X, Y], Res) :- integer(X), integer(Y),          Res is X - Y.
mul([X, Y], Res) :- integer(X), integer(Y),          Res is X * Y.
div([X, Y], Res) :- integer(X), integer(Y), Y \== 0, Res is X / Y.

define_core_function(Env, Symbol, Core_Function) :-
    mal_fn(Core_Function, Form),
    env_set(Env, Symbol, Form).

main(_Argv) :-
    getenv("HOME", Home),
    string_concat(Home, "/.mal-history", History),
    (exists_file(History) -> rl_read_history(History) ; true),

    env(Env),
    map_keyvals(define_core_function(Env), ['+', add, '-', sub, '*', mul, '/', div]),

    catch(repl(Env), exit_repl, nl),

    (rl_write_history(History) -> true ; true).
