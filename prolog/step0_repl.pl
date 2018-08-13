:- set_prolog_flag(double_quotes, chars).

mal_read(Stream, Codes) :-
    read_line_to_string(Stream, Codes),
    Codes \= end_of_file.

mal_eval(AST, _, AST).

mal_print(Stream, Expr) :-
    format(Stream, '~s~N', [Expr]).

mal_rep(IStream, OStream) :-
    mal_read(IStream, AST),
    mal_eval(AST, _, Expr),
    mal_print(OStream, Expr).

main :-
    prompt(_, 'user> '),
    mal_rep(current_input, current_output),
    main.
