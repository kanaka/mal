% -*- mode: prolog; -*- select prolog mode in the emacs text editor

:- initialization(main, main).

:- consult([core, env, printer, reader, types, utils]).

% Read

mal_read(Ast) :-
    write("user> "),
    read_line_to_string(current_input, Line),
    (Line = end_of_file -> throw(exit_repl) ; true),
    (rl_add_history(Line) -> true ; true), % fails for duplicate lines
    read_str(Line, Ast).

% Eval non-empty list depending on their first element.
:- discontiguous eval_list/4.

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

eval_list(Env, if, Args, Res) :- !,
    check(if_assign_args(Args, Form, Then, Else),
          "if: expects 2 or 3 arguments, got: ~L", [Args]),
    eval(Env, Form, Test),
    if_select(Test, Then, Else, Selected),
    eval(Env, Selected, Res).

if_assign_args([Form, Then, Else], Form, Then, Else).
if_assign_args([Form, Then],       Form, Then, nil).

if_select(false, _,    Else, Else) :- !.
if_select(nil,   _,    Else, Else) :- !.
if_select(_,     Then, _,    Then).

eval_list(Env, 'fn*', Args, Res) :- !,
    check(Args = [Params, Form], "fn*: expects 2 arguments, got: ~L", [Args]),
    check(unbox_seq(Params, Keys), "fn*: ~F is not a sequence", [Params]),
    check(maplist(atom, Keys), "fn*: ~F should contains symbols", [Params]),
    mal_fn(apply_fn(Keys, Form, Env), Res).

apply_fn(Keys, Form, Env, Args, Res) :-
    env(Env, Apply_Env),
    check(env_bind(Apply_Env, Keys, Args),
          "cannot apply fn*[~L] to [~L]", [Keys, Args]),
    eval(Apply_Env, Form, Res).

eval_list(Env, do, Args, Res) :- !,
    foldl(do_loop(Env), Args, nil, Res).

do_loop(Env, Elt, _Old_Acc, New_Acc) :- eval(Env, Elt, New_Acc).

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

re(Env, String) :-
    read_str(String, Ast),
    eval(Env, Ast, _).

define_core_function(Env, Symbol, Core_Function) :-
    mal_fn(wrap_failure(Core_Function), Form),
    env_set(Env, Symbol, Form).

main(_Argv) :-
    getenv("HOME", Home),
    string_concat(Home, "/.mal-history", History),
    (exists_file(History) -> rl_read_history(History) ; true),

    env(Env),
    core_ns(Core_Ns),
    map_keyvals(define_core_function(Env), Core_Ns),
    define_core_function(Env, eval, core_eval(Env)),

    re(Env, "(def! not (fn* [a] (if a false true)))"),

    catch(repl(Env), exit_repl, nl),

    (rl_write_history(History) -> true ; true).
