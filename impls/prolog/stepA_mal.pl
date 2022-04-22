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

eval_list(_, quote, Args, Res) :- !,
    check(Args = [Res], "quote: expects 1 argument, got ~L", [Args]).

eval_list(_, quasiquoteexpand, Args, Res) :- !,
    check(Args = [X], "quasiquoteexpand: expects 1 argument, got: ~L", [Args]),
    quasiquote(X, Res).

eval_list(Env, quasiquote, Args, Res) :- !,
    check(Args = [X], "quasiquote: expects 1 argument, got: ~L", [Args]),
    quasiquote(X, Y),
    eval(Env, Y, Res).

quasiquote(List, Res) :-
    list(Xs, List), !,
    (   Xs = [unquote | Args]
    ->  check(Args = [Res], "unquote: expects 1 argument, got: ", [Args])
    ;   list([], Empty),
        foldr(qq_loop, Empty, Xs, Res)).
quasiquote(Vector, Res) :-
    vector(Xs, Vector), !,
    list([], Empty),
    foldr(qq_loop, Empty, Xs, Y),
    list([vec, Y], Res).
quasiquote(nil,   nil).
quasiquote(true,  true).
quasiquote(false, false).
quasiquote(Symbol_Or_Map, Res) :-
    (atom(Symbol_Or_Map) -> true ; is_map(Symbol_Or_Map)), !,
    list([quote, Symbol_Or_Map], Res).
quasiquote(Anything_Else, Anything_Else).

qq_loop(Elt, Acc, Res) :-
    list(['splice-unquote' | Args], Elt), !,
    check(Args = [X], "splice-unquote: expects 1 argument, got:", [Args]),
    list([concat, X, Acc], Res).
qq_loop(Elt, Acc, Res) :-
    quasiquote(Elt, Quasiquoted),
    list([cons, Quasiquoted, Acc], Res).

eval_list(Env, 'try*', Args, Res) :- !,
    (   Args = [Test]
    ->  eval(Env, Test, Res)
    ;   check(Args = [Test, Catch],
              "try*: expects 1 or 2 arguments, got: ~L", [Args]),
        check(list(['catch*', Key, Form], Catch),
              "try*: ~F is not a catch* list", [Catch]),
        check(atom(Key), "catch*: ~F is not a key", [Key]),
        catch(eval(Env, Test, Res), mal_error(Error),
              (env(Env, Try_Env),
               env_set(Try_Env, Key, Error),
               eval(Try_Env, Form, Res)))).

eval_list(Env,  'defmacro!', Args, Res) :- !,
    check(Args = [Key, Form],
          "defmacro!: expects 2 arguments, got: ~L", [Args]),
    check(atom(Key), "defmacro!: ~F is not a key", [Key]),
    eval(Env, Form, Fn),
    check(mal_fn(_Goal, Fn), "defmacro!: ~F is not a function", [Fn]),
    mal_macro(Fn, Res),
    env_set(Env, Key, Res).

eval_list(Env, macroexpand, Args, Res) :- !,
    check(Args = [X], "macroexpand: expects 1 argument, got: ~L", [Args]),
    macroexpand(Env, X, Res).

macroexpand(Env, Ast, Res) :-
    list([Key | Args], Ast),
    env_get(Env, Key, Macro),
    mal_macro(Fn, Macro), !,
    mal_fn(Goal, Fn),
    call(Goal, Args, New_Ast),
    macroexpand(Env, New_Ast, Res).
macroexpand(_, Ast, Ast).

% apply phase

eval_list(Env, First, Rest, Res) :-
    eval(Env, First, Fn),
    (   mal_macro(F, Fn)
    ->  % If the Fn macro refers to F, apply F then evaluate,
        mal_fn(Goal, F),
        call(Goal, Rest, New_Ast),
        eval(Env, New_Ast, Res)
    ;   % else evaluate arguments, apply Fn.
        check(mal_fn(Goal, Fn), "cannot apply, ~F is not a function", [Fn]),
        maplist(eval(Env), Rest, Args),
        call(Goal, Args, Res)).

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

eval(Env, Map, Res) :-
    map_map(eval(Env), Map, Res).

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
    catch(rep(Env), mal_error(X), format("Exception: ~F\n", [X])),
    repl(Env).

re(Env, String) :-
    read_str(String, Ast),
    eval(Env, Ast, _).

define_core_function(Env, Symbol, Core_Function) :-
    mal_fn(wrap_failure(Core_Function), Form),
    env_set(Env, Symbol, Form).

core_eval(Env, [Ast], Res) :- eval(Env, Ast, Res).

main(Argv) :-
    getenv("HOME", Home),
    string_concat(Home, "/.mal-history", History),
    (exists_file(History) -> rl_read_history(History) ; true),

    env(Env),
    core_ns(Core_Ns),
    map_keyvals(define_core_function(Env), Core_Ns),
    define_core_function(Env, eval, core_eval(Env)),

    env_set(Env, '*host-language*', "prolog"),

    re(Env, "(def! not (fn* [a] (if a false true)))"),
    re(Env, "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"),
    re(Env, "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"),

    (   maplist(atom_string, Argv, [Script | Args])

    ->  % If Argv starts with a script, set arguments and load it.
        list(Args, Mal_Argv),
        env_set(Env, '*ARGV*', Mal_Argv),

        format(string(Load_Script), "(load-file \"~s\")", [Script]),
        re(Env, Load_Script)

    ;   % else read from standard input.
        list([], Mal_Argv),
        env_set(Env, '*ARGV*', Mal_Argv),

        re(Env, "(println (str \"Mal [\" *host-language* \"]\"))"),
        catch(repl(Env), exit_repl, nl)
    ),

    (rl_write_history(History) -> true ; true).
