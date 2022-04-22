% -*- mode: prolog; -*- select prolog mode in the emacs text editor

:- initialization(main, main).

:- consult([printer, reader, types, utils]).

% Read

mal_read(Ast) :-
    write("user> "),
    read_line_to_string(current_input, Line),
    (Line = end_of_file -> throw(exit_repl) ; true),
    (rl_add_history(Line) -> true ; true), % fails for duplicate lines
    read_str(Line, Ast).

% apply phase

eval_list(Env, First, Rest, Res) :-
    eval(Env, First, Fn),
    check(mal_fn(Goal, Fn), "cannot apply, ~F is not a function", [Fn]),
    maplist(eval(Env), Rest, Args),
    call(Goal, Args, Res).

% The eval function itself.

% Uncomment this to get a trace.
%% eval(_, Ast, _) :-
%%     format("EVAL: ~F\n", [Ast]),
%%     fail.                       % Proceed with normal alternatives.

eval(Env, List, Res) :-
    list([First | Args], List), !,
    eval_list(Env, First, Args, Res).

eval(_,   nil,    nil).
eval(_,   true,   true).
eval(_,   false,  false).
eval(Env, Symbol, Res) :-
    atom(Symbol), !,
    check(get_assoc(Symbol, Env, Res), "'~F' not found", [Symbol]).

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

main(_Argv) :-
    getenv("HOME", Home),
    string_concat(Home, "/.mal-history", History),
    (exists_file(History) -> rl_read_history(History) ; true),

    mal_fn(add, Add),
    mal_fn(sub, Sub),
    mal_fn(mul, Mul),
    mal_fn(div, Div),
    list_to_assoc(['+' - Add, '-' - Sub, '*' - Mul, '/' - Div], Env),

    catch(repl(Env), exit_repl, nl),

    (rl_write_history(History) -> true ; true).
