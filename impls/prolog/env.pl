% -*- mode: prolog; -*- select prolog mode in the emacs text editor

:- format_predicate('V', env_format(_Arg,_Env)).

env(mal_env(Assoc, t)) :- empty_assoc(Assoc).

env(Outer, mal_env(Assoc, Outer)) :- empty_assoc(Assoc).

env_get(mal_env(Assoc, _), Key, Value) :- get_assoc(Key, Assoc, Value).
env_get(mal_env(_, Outer), Key, Value) :- env_get(Outer, Key, Value).

env_set(Env, Key, Value) :-
    Env = mal_env(Old, _),
    put_assoc(Key, Old, Value, New),
    setarg(1, Env, New).

env_format(_Arg, mal_env(Assoc, _Outer)) :-
    assoc_to_list(Assoc, Pairs),
    maplist(env_format_pair, Pairs).

env_format_pair(K - V) :- format(" ~a:~F", [K, V]).

% Does *not* check that the keys are symbols. This is done once when
% the fn* structure is created.
env_bind(_Env, [], []).
env_bind(Env, ['&', K], Vs) :- !,
    list(Vs, List),
    env_set(Env, K, List).
env_bind(Env, [K | Ks], [V | Vs]) :-
    env_set(Env, K, V),
    env_bind(Env, Ks, Vs).
