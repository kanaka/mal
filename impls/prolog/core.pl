% -*- mode: prolog; -*- select prolog mode in the emacs text editor

wrap_failure(Goal, Args, Res) :-
    check(call(Goal,Args, Res),
          "~a: wrong arguments: ~L", [Goal, Args]).

bool(Goal, true) :- call(Goal), !.
bool(_,    false).

'nil?'([X], R) :- bool(=(nil,X), R).

'false?'([X], R) :- bool(=(false, X), R).

'true?'([X], R) :- bool(=(true, X), R).

% Numbers

'number?'([X], R) :- bool(integer(X), R).

add([X, Y], R) :- integer(X), integer(Y),         R is X + Y.

sub([X, Y], R) :- integer(X), integer(Y),         R is X - Y.

mul([X, Y], R) :- integer(X), integer(Y),         R is X * Y.

div([X, Y], R) :- integer(X), integer(Y), Y \= 0, R is X / Y.

'<='([X, Y], R) :- integer(X), integer(Y), bool(=<(X, Y), R).

ge(  [X, Y], R) :- integer(X), integer(Y), bool(>=(X, Y), R).

lt(  [X, Y], R) :- integer(X), integer(Y), bool(<(X, Y), R).

gt(  [X, Y], R) :- integer(X), integer(Y), bool(>(X, Y), R).

% Symbols

'symbol?'([false], false).
'symbol?'([nil],   false).
'symbol?'([true],  false).
'symbol?'([X], R) :- bool(atom(X), R).

symbol([X], R) :- string(X), atom_string(R, X).

% Keywords

'keyword?'([X], R) :- bool(=(X, mal_kwd(_)), R).

keyword([X], mal_kwd(X)) :- string(X).
keyword([R], R)          :- R = mal_kwd(_).

% Sequences

'list?'([X], R) :- bool(list(_, X), R).

'vector?'([X], R) :- bool(vector(_, X), R).

'sequential?'([X], R) :- bool(unbox_seq(X, _), R).

'empty?'([X], R) :- bool(unbox_seq(X, []), R).

count([X], R) :- unbox_seq(X, S), !, length(S, R).
count([nil], 0).

vec([X], R) :- unbox_seq(X, S), vector(S, R).

cons([X, Y], R) :- unbox_seq(Y, Ys), list([X | Ys], R).

concat(Xs, Z) :- maplist(unbox_seq, Xs, Ys), append(Ys, Zs), list(Zs, Z).

nth([Sequence, Index], Element) :-
    unbox_seq(Sequence, Xs),
    check(nth0(Index, Xs, Element),
          "nth: index ~d out of bounds of ~F", [Index, Sequence]).

first([X], Y) :- unbox_seq(X, Xs), !,
    (Xs = [Y | _] -> true ; Y = nil).
first([nil], nil).

rest([X],   R) :- unbox_seq(X, Xs), !,
    (Xs = [_ | Rs] -> true ; Rs = []),
    list(Rs, R).
rest([nil], R) :- list([], R).

map([Fn, Seq], R) :-
    unbox_seq(Seq, Xs),
    mal_fn(Goal, Fn),
    maplist(enlist_apply(Goal), Xs, Rs), list(Rs, R).

enlist_apply(Goal, X, R) :- call(Goal, [X], R).

conj([Vector | Ys], R) :- vector(Xs, Vector), !,
    append(Xs, Ys, Zs),
    vector(Zs, R).
conj([List   | Ys], R) :- list(Xs, List),
    foldl(cons, Ys, Xs, Zs), list(Zs, R).

cons(X, Xs, [X | Xs]).

seq([X],  nil) :- unbox_seq(X, []).
seq([X],  X)   :- list(_, X).
seq([X],  R)   :- vector(Xs, X), !, list(Xs, R).
seq([""], nil).
seq([S],  R)   :- string(S), !,
    string_chars(S, Chars),
    maplist(atom_string, Chars, Strings),
    list(Strings, R).
seq([nil], nil).

% Maps (there is little not much we can do out of types).

'map?'([X], R) :- bool(is_map(X), R).

get([Map, Key], R) :- get(Map, Key, R).
get([_,   _], nil).

'contains?'([Map, Key], R) :- bool(get(Map, Key, _), R).

dissoc([Map | Keys], Res) :- foldl(dissoc, Keys, Map, Res).

% Atoms

'atom?'([X], R) :- bool(mal_atom(_, X), R).

atom([A], R) :- mal_atom(A, R).

deref([A], R) :- mal_atom(R, A).

'reset!'([A, R], R) :- mal_atom(_, A), set_mal_atom_value(A, R).

'swap!'([Atom, Function | Args], R) :-
    mal_atom(Old, Atom),
    mal_fn(Goal, Function),
    call(Goal, [Old | Args], R),
    set_mal_atom_value(Atom, R).

apply([Fn | Xs], R) :-
    flatten_last(Xs, Args),
    mal_fn(Goal, Fn),
    call(Goal, Args, R).

flatten_last([X],      Xs)       :- unbox_seq(X, Xs).
flatten_last([X | Xs], [X | Ys]) :- flatten_last(Xs, Ys).

% Strings

'string?'([X], R) :- bool(string(X), R).

'pr-str'(Args, R)   :- with_output_to(string(R), print_list(t, " ", Args)).

str(     Args, R)   :- with_output_to(string(R), print_list(f, "",  Args)).

prn(     Args, nil) :-                           print_list(t, " ", Args), nl.

println( Args, nil) :-                           print_list(f, " ", Args), nl.

'read-string'([S], R) :- string(S), read_str(S, R).

slurp([Path], R) :-
    string(Path),
    (read_file_to_string(Path, R, []) -> true ; R = nil).

readline([Prompt], R) :-
    string(Prompt),
    write(Prompt),
    read_line_to_string(current_input, R),
    (R = end_of_file -> R = nil ; true).

throw([X], nil) :- throw(mal_error(X)).

'time-ms'([], Ms) :- get_time(S), Ms is round(1_000*S).

eq([X, Y], R) :- bool(mal_equal(X, Y), R).

'fn?'([X], R) :- bool(mal_fn(_, X), R).

'macro?'([X], R) :- bool(mal_macro(_, X), R).

'prolog-asserta'([String], nil) :-
    string(String),
    catch((read_term_from_atom(String, Term, []),
           asserta(Term)),
          Error,
          throwf("prolog-asserta: ~w", [Error])).

'prolog-call'([String], Res) :-
    string(String),
    catch((read_term_from_atom(String, Term, []),
           call(Term, Res)),
          Error,
          throwf("prolog-call: ~w", [Error])),
    check(valid_mal(Res), "prolog-call: invalid result: ~w", [Res]).

core_ns([
    % naming exceptions
    '+', add,
    '-', sub,
    '*', mul,
    '/', div,
    '=', eq,
    '<', lt,
    '>=', ge,
    '>', gt,
    % step 4
    '<=', '<=',
    prn, prn,
    list, list,
    'list?', 'list?',
    'empty?', 'empty?',
    count, count,
    'pr-str', 'pr-str',
    str, str,
    println, println,
    % step 6
    'read-string', 'read-string',
    slurp, slurp,
    atom, atom,
    'atom?', 'atom?',
    deref, deref,
    'reset!', 'reset!',
    'swap!', 'swap!',
    % step 7
    cons, cons,
    concat, concat,
    vec, vec,
    % step 8
    nth, nth,
    first, first,
    rest, rest,
    % step 9
    throw, throw,
    apply, apply,
    map, map,
    'nil?', 'nil?',
    'true?', 'true?',
    'false?', 'false?',
    'symbol?', 'symbol?',
    symbol, symbol,
    keyword, keyword,
    'keyword?', 'keyword?',
    vector, vector,
    'vector?', 'vector?',
    'sequential?', 'sequential?',
    'hash-map', 'hash-map',
    'map?', 'map?',
    assoc, assoc,
    dissoc, dissoc,
    get, get,
    'contains?', 'contains?',
    keys, keys,
    vals, vals,
    % step A
    readline, readline,
    meta, meta,
    'with-meta', 'with-meta',
    'time-ms', 'time-ms',
    conj, conj,
    'string?', 'string?',
    'number?', 'number?',
    'fn?', 'fn?',
    'macro?', 'macro?',
    seq, seq,
    'prolog-asserta', 'prolog-asserta',
    'prolog-call', 'prolog-call']).
