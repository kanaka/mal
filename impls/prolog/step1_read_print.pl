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

% Eval

eval(Ast, Ast).

% Print

print(Ast) :- format("~F\n", [Ast]).

% REP

rep :-
    mal_read(Ast),
    eval(Ast, Evaluated),
    print(Evaluated).

% Main program

repl :-
    catch(rep, mal_error(Message), writeln(Message)),
    repl.

main(_Argv) :-
    getenv("HOME", Home),
    string_concat(Home, "/.mal-history", History),
    (exists_file(History) -> rl_read_history(History) ; true),

    catch(repl, exit_repl, nl),

    (rl_write_history(History) -> true ; true).
