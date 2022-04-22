% -*- mode: prolog; -*- select prolog mode in the emacs text editor

:- use_module(library(dcg/basics)).

read_str(String, Form) :-
    string_codes(String, Codes),
    check(phrase(read_form(Form), Codes, _Rest),
          "unbalanced expression: '~s'", [String]).

read_form(Res) --> zero_or_more_separators, (
      `(`,  !, read_list(`)`, Forms),            { list(Forms, Res) }
    | `[`,  !, read_list(`]`, Forms),            { vector(Forms, Res) }
    | `{`,  !, read_list(`}`, Forms),            { 'hash-map'(Forms, Res) }
    | `\``, !, read_form(Form),                  { list([quasiquote, Form], Res) }
    | `\'`, !, read_form(Form),                  { list([quote, Form], Res) }
    | `^`,  !, read_form(Meta), read_form(Data), { list(['with-meta', Data, Meta], Res) }
    | `:`,  !, at_least_one_symcode(Codes),      { string_codes(String, Codes),
                                                   Res = mal_kwd(String) }
    | `\"`, !, until_quotes(Codes),              { string_codes(Res, Codes) }
    | `@`,  !, read_form(Form),                  { list([deref, Form], Res) }
    | `~@`, !, read_form(Form),                  { list(['splice-unquote', Form], Res) }
    | `~`,  !, read_form(Form),                  { list([unquote, Form], Res)  }
    | integer(Res)
    | at_least_one_symcode(Cs),                  { atom_codes(Res, Cs) }).

read_list(Closing, [Form | Forms]) --> read_form(Form), !, read_list(Closing, Forms).
read_list(Closing, [])             --> zero_or_more_separators, Closing.

zero_or_more_separators --> separator, !, zero_or_more_separators
                            | [].

separator --> [C], { sepcode(C) }, !.
separator --> `;`, string_without(`\n`, _Comment).

at_least_one_symcode([C | Cs]) --> [C], { symcode(C) }, zero_or_more_symcodes(Cs).

until_quotes([])          --> [0'"].
until_quotes([0'\n | Cs]) --> `\\n`,  !, until_quotes(Cs).
until_quotes([0'"  | Cs]) --> `\\\"`, !, until_quotes(Cs).
until_quotes([0'\\ | Cs]) --> `\\\\`, !, until_quotes(Cs).
until_quotes([C    | Cs]) --> [C],       until_quotes(Cs).

zero_or_more_symcodes(Cs) --> at_least_one_symcode(Cs), !.
zero_or_more_symcodes([]) --> [].

sepcode(0',).
sepcode(0' ).
sepcode(0'\n).

symcode(C) :- code_type(C, alnum).
symcode(0'!).
symcode(0'#).
symcode(0'$).
symcode(0'%).
symcode(0'&).
symcode(0'*).
symcode(0'+).
symcode(0'-).
symcode(0'/).
symcode(0'<).
symcode(0'=).
symcode(0'>).
symcode(0'?).
symcode(0'_).
symcode(0'|).
