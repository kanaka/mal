%%%
%%% Environment
%%%
%%% We need an "object" to represent the environment: something whose state can
%%% change over time, while keeping a single, unchanging reference to that
%%% object. This is done in Erlang using lightweight processes. Fortunately, OTP
%%% makes this easy.
%%%

-module(env).
-behavior(gen_server).

-export([new/1, bind/3, find/2, get/2, set/3, root/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {outer, data}).

%%
%% Public API
%%

-spec new(Outer) -> Pid
    when Outer :: #state{},
         Pid   :: pid().
% @doc Pass 'undefined' for Outer if no parent environment.
new(Outer) ->
    case gen_server:start(?MODULE, [Outer], []) of
        {ok, Pid} -> Pid;
        {error, Reason} -> error(Reason)
    end.

-spec bind(Pid, Names, Values) -> ok
    when Pid    :: pid(),
         Names  :: [term()],
         Values :: [term()].
bind(Pid, Names, Values) ->
    gen_server:call(Pid, {bind, Names, Values}).

-spec find(Pid1, Key) -> Pid2
    when Pid1 :: pid(),
         Key  :: {symbol, string()},
         Pid2 :: pid() | nil.
find(Pid, {symbol, Name}) ->
    gen_server:call(Pid, {find_pid, Name}).

-spec get(Pid, Key) -> Value
    when Pid   :: pid(),
         Key   :: {symbol, string()},
         Value :: term().
get(Pid, {symbol, Name}) ->
	case gen_server:call(Pid, {get, Name}) of
        {ok, Value} -> Value;
        {error, Reason} -> error(Reason)
    end;
get(_Pid, _Key) ->
    error("env:get/2 called with non-symbol key").

-spec set(Pid, Key, Value) -> ok
    when Pid   :: pid(),
         Key   :: {symbol, string()},
         Value :: term().
set(Pid, {symbol, Name}, Value) ->
    gen_server:call(Pid, {set, Name, Value});
set(_Env, _Key, _Value) ->
    error("env:set/3 called with non-symbol key").

-spec root(Pid1) -> Pid2
    when Pid1 :: pid(),
         Pid2 :: pid().
root(Pid) ->
    gen_server:call(Pid, root).

%%
%% gen_server callbacks
%%

init([]) ->
    init([undefined]);
init([Outer]) ->
    {ok, #state{outer=Outer, data=#{}}}.

handle_call({bind, Names, Values}, _From, State) ->
    NewEnv = env_bind(State, Names, Values),
    {reply, ok, NewEnv};
handle_call({find_env, Name}, _From, State) ->
    {reply, env_find(State, Name), State};
handle_call({find_pid, Name}, _From, State) ->
    {reply, pid_find(State, Name), State};
handle_call({get, Name}, _From, State) ->
    {reply, env_get(State, Name), State};
handle_call({set, Name, Value}, _From, State) ->
    {reply, ok, env_set(State, Name, Value)};
handle_call(root, _From, State) ->
    {reply, env_root(State), State};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    error_logger:info_msg("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal functions
%%

pid_find(Env, Name) ->
    case maps:is_key(Name, Env#state.data) of
        true  -> self();
        false ->
            case Env#state.outer of
                undefined -> nil;
                Outer     -> gen_server:call(Outer, {find_pid, Name})
            end
    end.

env_find(Env, Name) ->
    case maps:is_key(Name, Env#state.data) of
        true  -> Env;
        false ->
            case Env#state.outer of
                undefined -> nil;
                Outer     -> gen_server:call(Outer, {find_env, Name})
            end
    end.

-spec env_bind(Env1, Names, Values) -> Env2
    when Env1   :: #state{},
         Names  :: [term()],
         Values :: [term()],
         Env2   :: #state{}.
env_bind(Env, [], []) ->
    Env;
env_bind(Env, [{symbol, "&"}, {symbol, Name}], Values) ->
    env_set(Env, Name, {list, Values, nil});
env_bind(Env, [{symbol, Name}|Ntail], [Value|Vtail]) ->
    env_bind(env_set(Env, Name, Value), Ntail, Vtail).

-spec env_get(Env, Key) -> {ok, Value} | {error, string()}
    when Env   :: #state{},
         Key   :: {symbol, string()},
         Value :: term().
env_get(Env, Name) ->
    case env_find(Env, Name) of
        nil -> {error, io_lib:format("'~s' not found", [Name])};
        E   -> {ok, maps:get(Name, E#state.data)}
    end.

-spec env_set(Env1, Key, Value) -> Env2
    when Env1  :: #state{},
         Key   :: {symbol, string()},
         Value :: term(),
         Env2  :: #state{}.
env_set(Env, Name, Value) ->
    Map = maps:put(Name, Value, Env#state.data),
    #state{outer=Env#state.outer, data=Map}.

-spec env_root(Env1) -> Env2
    when Env1 :: #state{},
         Env2 :: #state{}.
env_root(Env) ->
    case Env#state.outer of
        undefined -> self();
        Outer     -> gen_server:call(Outer, root)
    end.
