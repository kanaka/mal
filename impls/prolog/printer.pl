% -*- mode: prolog; -*- select prolog mode in the emacs text editor

:- format_predicate('F', format_mal_form(_Arg,_Form)).
:- format_predicate('L', format_mal_list(_Arg,_Forms)).
format_mal_list(_Arg, Forms) :- print_list(t, " ", Forms).
format_mal_form(_Arg, Form) :- pr_str(t, Form).

pr_str(t, String) :- string(String), !,
    write("\""),
    string_codes(String, Codes),
    maplist(pr_str_escape, Codes),
    write("\"").

pr_str(_, Atomic) :- atomic(Atomic), !,
    % number, symbol, nil, true, false, unreadable string.
    write(Atomic).

pr_str(_, mal_kwd(Keyword)) :- !,
    put_char(:),
    write(Keyword).

pr_str(Readably, Vector) :- vector(Elements, Vector), !,
    write("["),
    print_list(Readably, " ", Elements),
    write("]").

pr_str(Readably, List) :- list(Elements, List), !,
    write("("),
    print_list(Readably, " ", Elements),
    write(")").

pr_str(Readably, Map) :- map_to_key_value_list(Map, Key_Value_List), !,
    write("{"),
    print_list(Readably, " ", Key_Value_List),
    write("}").

pr_str(_, Fn) :- mal_fn(_Goal, Fn), !, write("<fn>").

pr_str(_, Macro) :- mal_macro(_Fn, Macro), !,
    write("<macro>").

pr_str(_, Atom) :- mal_atom(Value, Atom), !,
    format("(atom ~F)", [Value]).

pr_str(_, Invalid) :-
    format(string(Msg), "pr_str detected an invalid form: ~w\n", [Invalid]),
    print_message(warning, Msg),
    abort.

pr_str_escape(0'\n) :- write("\\n").
pr_str_escape(0'")  :- write("\\\"").
pr_str_escape(0'\\) :- write("\\\\").
pr_str_escape(C)    :- put_code(C).

print_list(_, _, []).
print_list(Readably, Separator, [X | Xs]) :-
    pr_str(Readably, X),
    maplist(print_list_append(Readably, Separator), Xs).

print_list_append(Readably, Separator, Element) :-
    write(Separator),
    pr_str(Readably, Element).
