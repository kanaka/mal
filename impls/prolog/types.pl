% -*- mode: prolog; -*- select prolog mode in the emacs text editor

:- discontiguous mal_equal/2.
:- discontiguous 'with-meta'/2.
:- discontiguous meta/2.
:- discontiguous valid_mal/1.

% A MAL number is represented by a Prolog integer.

% A MAL symbol is represented by a Prolog atom,
% including `false`, `nil` and `true`.

% A MAL string is represented by a Prolog string.

% A MAL keyword is represented as mal_kwd(String), and there is no
% reason to encapsulate this information.

% The remaining representations are encapsulated because they may have
% to evolve, and interfer directly with metadata.

mal_equal(X,  X) :- atomic(X), !.
mal_equal(mal_kwd(S), mal_kwd(S)) :- !.

valid_mal(X) :- integer(X), !.
valid_mal(X) :- atom(X), !.
valid_mal(X) :- string(X), !.
valid_mal(mal_kwd(S)) :- !, string(S).

% Sequences

% list(?Forms, ?List)
%   Bi-directional conversion between a list of MAL forms and a MAL list.
%   At least one of the two arguments must be instantiated.
%   Fails if the second argument is instantiated but not a MAL list.
% vector(?Forms, ?Vector)
%   Similar for MAL vectors.

list(Forms, mal_list(Forms)) :- !.
list(Forms, mal_list(Forms, _Meta)) :- !.

vector(Forms, mal_vector(Forms)) :- !.
vector(Forms, mal_vector(Forms, _Meta)) :- !.

mal_equal(S1, S2) :-
    unbox_seq(S1, L1), !,
    unbox_seq(S2, L2),
    maplist(mal_equal, L1, L2).

'with-meta'([X, Meta], mal_list(  Forms, Meta)) :- list(  Forms, X), !.
'with-meta'([X, Meta], mal_vector(Forms, Meta)) :- vector(Forms, X), !.

meta([mal_list(_,   Meta)], Meta) :- !.
meta([mal_vector(_, Meta)], Meta) :- !.

valid_mal(mal_list(F))      :- !, maplist(valid_mal, F).
valid_mal(mal_list(F, M))   :- !, maplist(valid_mal, F), valid_mal(M).
valid_mal(mal_vector(F))    :- !, maplist(valid_mal, F).
valid_mal(mal_vector(F, M)) :- !, maplist(valid_mal, F), valid_mal(M).

% Maps

% Other files should not directly depend on Assoc, as there may be
% good reasons to change the map representation.

'hash-map'(Key_Value_List, mal_map(Res)) :-
    empty_assoc(Assoc),
    check(foldl_keyvals(assoc, Assoc, Key_Value_List, Res),
          "hash-map: odd count of key and values in ~L", [Key_Value_List]).

is_map(mal_map(_Assoc)) :- !.
is_map(mal_map(_Assoc, _Meta)) :- !.

is_key(Key) :- string(Key), !.
is_key(mal_kwd(_)) :- !.

unbox_map(mal_map(Assoc), Assoc) :- !.
unbox_map(mal_map(Assoc, _Meta), Assoc) :- !.

get(Map, Key, Res) :-
    unbox_map(Map, Assoc),
    is_key(Key),
    get_assoc(Key, Assoc, Res).

assoc([Map | Key_Value_List], mal_map(Res)) :-
    unbox_map(Map, Assoc),
    check(foldl_keyvals(assoc, Assoc, Key_Value_List, Res),
          "assoc: odd count of key and values in [~L]", [Key_Value_List]).

assoc(Assoc, Key, Value, Res) :-
    check(is_key(Key), "map keys must be strings or symbol, not ~F", [Key]),
    put_assoc(Key, Assoc, Value, Res).

% This order of parameter is convenient with foldl.
dissoc(Key, Map, mal_map(Res)) :-
    unbox_map(Map, Assoc),
    is_key(Key),
    % del_assoc fails if the key did previously exist,
    % and we do not want to search twice.
    (del_assoc(Key, Assoc, _Value, Res) -> true ; Res = Assoc).

map_map(Goal, Map, mal_map(Res)) :-
    unbox_map(Map, Assoc),
    map_assoc(Goal, Assoc, Res).

keys([Map], Res) :-
    unbox_map(Map, Assoc),
    assoc_to_keys(Assoc, Keys),
    list(Keys, Res).

vals([Map], Res) :-
    unbox_map(Map, Assoc),
    assoc_to_values(Assoc, Vals),
    list(Vals, Res).

% MAL map -> key/value Prolog list
% Fail if the form is not a map.
map_to_key_value_list(Map, Forms) :-
    unbox_map(Map, Assoc),
    assoc_to_list(Assoc, Pairs),
    foldr(convert_pair, [], Pairs, Forms).

convert_pair(Key - Value, Acc, [Key, Value | Acc]).

mal_equal(Map1, Map2) :-
    unbox_map(Map1, Assoc1), !,
    unbox_map(Map2, Assoc2),
    % map_assoc(mal_equal) does not work here because its result
    % depends on the internal structure.
    assoc_to_list(Assoc1, Pairs1),
    assoc_to_list(Assoc2, Pairs2),
    maplist(map_pair_equal, Pairs1, Pairs2).

map_pair_equal(K1 - V1, K2 - V2) :- K1 = K2, mal_equal(V1, V2).

'with-meta'([X, Meta], mal_map(Assoc, Meta)) :- unbox_map(X, Assoc), !.

meta([mal_map(_, Meta)], Meta) :- !.

valid_mal(mal_map(Assoc)) :- !,
    is_assoc(Assoc),
    assoc_to_list(Assoc, Pairs),
    maplist(valid_mal_pair, Pairs).
valid_mal(mal_map(Assoc, Meta)) :- !,
    is_assoc(Assoc),
    assoc_to_list(Assoc, Pairs),
    maplist(valid_mal_pair, Pairs),
    valid_mal(Meta).

valid_mal_pair(K - V) :- is_key(K), valid_mal(V).

% Functions

% Goal is called with call(Goal, [Arg1, Arg2..], Res).
% It should never fail, and use mal_error/1 to report problems.

mal_fn(Goal, mal_fn(Goal)) :- !.
mal_fn(Goal, mal_fn(Goal, _Meta)) :- !.

'with-meta'([mal_fn(Goal),        Meta], mal_fn(Goal, Meta)) :- !.
'with-meta'([mal_fn(Goal, _Meta), Meta], mal_fn(Goal, Meta)) :- !.

meta([mal_fn(_,Meta)], Meta) :- !.

valid_mal(mal_fn(_)) :- !.
valid_mal(mal_fn(_, Meta)) :- !, valid_mal(Meta).

% Macros

mal_macro(Fn, mal_macro(Fn)).

% Atoms

mal_atom(Value, mal_atom(Value)).

set_mal_atom_value(Atom, Value) :- setarg(1, Atom, Value).

valid_mal(mal_atom(Value)) :- !, valid_mal(Value).

% Catch-all clause for objects without metadata.

meta([_], nil) :- !.
