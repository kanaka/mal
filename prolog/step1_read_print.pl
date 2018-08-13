:- use_module(parser, [mal_forms/3]).

:- set_prolog_flag(double_quotes, chars).

read_chars(end_of_file, _) :-
    throw(end_of_file).
read_chars(Chars, AST) :-
    phrase(mal_forms(AST), Chars), !.

mal_read(Stream, AST) :-
    read_line_to_string(Stream, String),
    string_chars(String, Chars),
    read_chars(Chars, AST).

mal_eval(AST, _, AST).

mal_print(Stream, Expr) :-
    phrase(mal_forms(Expr), Chars), !,
    format(Stream, '~s~N', [Chars]).

mal_rep(IStream, OStream) :-
    mal_read(IStream, AST),
    mal_eval(AST, _, Expr),
    mal_print(OStream, Expr).

handle_error(syntax_error(Message)) :-
    format('~s~N', [Message]).
handle_error(end_of_file) :-
    halt.

main :-
    prompt(_, 'user> '),
    ignore(catch(mal_rep(current_input, current_output),
		 Error,
		 handle_error(Error))),
    main.
