%%%
%%% Environement
%%%

-module(env).

-export([new/0, new/1, bind/3, set/3, get/2, fallback/2]).

-record(env, {outer, data, fallback=undefined}).

%%
%% Public API
%%

-spec new() -> Env
    when Env :: #env{}.
new() ->
    new(undefined).

-spec new(Outer) -> Env
    when Outer :: #env{},
         Env   :: #env{}.
new(Outer) ->
	#env{outer=Outer, data=#{}}.

-spec bind(Env1, Names, Values) -> Env2
    when Env1   :: #env{},
         Names  :: [term()],
         Values :: [term()],
         Env2   :: #env{}.
bind(Env, [], []) ->
    Env;
bind(Env, [{symbol, "&"},Name], Values) ->
    set(Env, Name, {list, Values});
bind(Env, [Name|Ntail], [Value|Vtail]) ->
    bind(set(Env, Name, Value), Ntail, Vtail).

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

-spec fallback(Env1, Fallback) -> Env2
    when Env1     :: #env{},
         Fallback :: #env{},
         Env2     :: #env{}.
fallback(Env, Fallback) ->
    #env{outer=Env#env.outer, data=Env#env.data, fallback=Fallback}.

%%
%% Internal functions
%%

find(Env, Name) ->
    case maps:is_key(Name, Env#env.data) of
        true  -> Env;
        false ->
            case Env#env.outer of
                undefined ->
                    case Env#env.fallback of
                        undefined -> nil;
                        Fallback  -> find(Fallback, Name)
                    end;
                Outer     -> find(Outer, Name)
            end
    end.
