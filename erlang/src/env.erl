%%%
%%% Environement
%%%

-module(env).

-export([env_new/1, env_bind/3, env_find/2, env_root/1, env_set/3, env_get/2]).

env_new(_Outer) ->
	ok.

env_bind(_Env, _Bindings, _Exprs) ->
	ok.

env_find(_Env, _Key) ->
	ok.

env_root(_Env) ->
	ok.

env_set(_Env, _Key, _Value) ->
	ok.

env_get(_Env, _Key) ->
	ok.
