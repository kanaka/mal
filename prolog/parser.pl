:- module(parser, [mal_forms/3, print_readably/1]).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, chars).

symbol_char(C) -->
    [C], { \+ member(C, "\";[]{}()'`~^@, ") }.

symbol_chars([C| Cs]) -->
    symbol_char(C), symbol_chars(Cs), !.
symbol_chars([C]) -->
    symbol_char(C).

mal_symbol(symbol(Token)) -->
    { var(Token) },
    symbol_chars(Chars), { atom_chars(Token, Chars) }.
mal_symbol(symbol(Token)) -->
    { nonvar(Token), atom_chars(Token, Chars) },
    symbol_chars(Chars).

number_chars_(Number, Chars) :-
    catch(number_chars(Number, Chars), _, fail).

mal_integer(integer(Number)) -->
    { nonvar(Number), number_chars(Number, Chars) },
    Chars.
mal_integer(integer(Number)) -->
    { var(Number) },
    "-", digits(Chars),
    { number_chars_(Number, ['-' | Chars]) }.
mal_integer(integer(Number)) -->
    { var(Number) },
    "+", digits(Chars),
    { number_chars_(Number, Chars) }.
mal_integer(integer(Number)) -->
    { var(Number) },
    digits(Chars),
    { number_chars_(Number, Chars) }.

mal_atomic(Atom) -->
    (mal_integer(Atom), !);
    mal_symbol(Atom).

mal_blanks --> blanks.
mal_blanks --> ",", mal_blanks.
mal_blanks --> ",".

paren_seq(')', Forms, list(Forms)).
paren_seq(']', Forms, vector(Forms)).
paren_seq('}', Forms, hash_map(Forms)).

mal_seq(Seq) -->
    [Open],
    { char_type(Open, paren(Close)),
      paren_seq(Close, Forms, Seq) },
    mal_blanks,
    mal_forms(Forms),
    mal_blanks,
    [Close].
mal_seq(V) -->
    { var(V) },
    [O],
    { char_type(O, paren(C)) },
    mal_blanks, mal_forms(_), string(_), eos,
    { format(string(S), "expected '~a', got EOF", C),
      throw(syntax_error(S)) }.

mal_special(list([symbol('quote'), Form])) -->
    { var(Form) },
    "'", mal_form(Form).
mal_special(list([symbol('quasiquote'), Form])) -->
    { var(Form) },
    "`", mal_form(Form).
mal_special(list([symbol('splice-unquote'), Form])) -->
    { var(Form) },
    "~@", mal_form(Form).
mal_special(list([symbol('unquote'), Form])) -->
    { var(Form) },
    "~", mal_form(Form).
mal_special(list([symbol('deref'), Form])) -->
    { var(Form) },
    "@", mal_form(Form).
mal_special(list([symbol('with-meta'), F2, F1])) -->
    { var(F1), var(F2) },
    "^", mal_form(F1), mal_blanks, mal_form(F2).

str_char('\n') -->
    "\\n", !.
str_char('"') -->
    "\\\"", !.
str_char('\\') -->
    "\\\\", !.
str_char(C) -->
    [C], { \+ member(C, "\"") }.

str_chars([C| Cs]) -->
    str_char(C), str_chars(Cs).
str_chars([C]) -->
    str_char(C).

mal_string(string("")) -->
   "\"\"".
mal_string(string(Str)) -->
    { nonvar(Str), format(chars(Chars), "~q", [Str]) },
    Chars, !.
mal_string(string(Str)) -->
    { var(Str) },
    "\"" , str_chars(Chars), "\"",
    { string_chars(Str, Chars) }, !.
mal_string(string(Str)) -->
    { var(Str) },
    "\"", str_chars(_), eos,
    { throw(syntax_error("expected '\"', got EOF")) }.

mal_form(Form) -->
    mal_string(Form)
    ; mal_special(Form)
    ; mal_atomic(Form)
    ; mal_seq(Form).

mal_comment --> ";", string_without("\n", _), "\n".
mal_comment --> ";", string_without("\n", _), eos.

mal_forms([]) --> [].
mal_forms([Form | Forms]) -->
    { var(Form) },
    mal_blanks,
    mal_form(Form),
    mal_blanks,
    mal_forms(Forms),
    mal_blanks.
mal_forms([Form]) -->
    { var(Form) },
    mal_blanks,
    mal_form(Form),
    mal_blanks.
mal_forms([Form | Forms]) -->
    { var(Form) },
    mal_comment,
    mal_forms([Form | Forms]).
mal_forms([]) -->
    mal_comment.
mal_forms([Form]) -->
    { nonvar(Form) },
    mal_form(Form).
mal_forms([Form | Forms]) -->
    { nonvar(Form) },
    mal_form(Form), " ", mal_forms(Forms).
