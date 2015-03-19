%%%
%%% Environement
%%%

-module(env).

-export([new/1, set/3, get/2]).

-record(env, {outer, data}).

%%
%% Public API
%%

-spec new(Outer) -> Env
    when Outer :: #env{},
         Env   :: #env{}.
% Construct a new environment; use 'undefined' for Outer is this is the
% root environment.
new(Outer) ->
	#env{outer=Outer, data=#{}}.

-spec set(Env1, Key, Value) -> Env2
    when Env1  :: #env{},
         Key   :: {symbol, term()},
         Value :: term(),
         Env2  :: #env{}.
set(Env, Key, Value) ->
    case Key of
        {symbol, Name} ->
            Map = maps:put(Name, Value, Env#env.data),
            #env{outer=Env#env.outer, data=Map};
        _ -> throw("env:set/3 called with non-symbol key")
    end.

-spec get(Env, Key) -> Value
    when Env   :: #env{},
         Key   :: {symbol, term()},
         Value :: term().
get(Env, Key) ->
    case Key of
        {symbol, Name} ->
        	case find(Env, Name) of
                nil -> throw(io_lib:format("'~s' not found", [Name]));
                E   -> maps:get(Name, E#env.data)
            end;
        _ -> throw("env:get/2 called with non-symbol key")
    end.

%%
%% Internal functions
%%

find(Env, Name) ->
    case maps:is_key(Name, Env#env.data) of
        true  -> Env;
        false ->
            case Env#env.outer of
                undefined -> nil;
                Outer     -> find(Outer, Name)
            end
    end.
