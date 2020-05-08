%%%
%%% Atom
%%%
%%% Atoms in MAL represent mutable data, which is not native to Erlang. The
%%% lightweight technique for representing mutable data in Erlang is with a
%%% lightweight process.
%%%

-module(atom).
-behavior(gen_server).

-export([new/1, deref/1, reset/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {atom}).

%%
%% Public API
%%

-spec new(Atom) -> Pid
    when Atom :: term(),
         Pid  :: pid().
new(Atom) ->
    case gen_server:start(?MODULE, [Atom], []) of
        {ok, Pid} -> Pid;
        {error, Reason} -> error(Reason)
    end.

-spec deref(Pid) -> Value
    when Pid   :: pid(),
         Value :: term().
deref(Pid) ->
	gen_server:call(Pid, deref).

-spec reset(Pid, Value) -> ok
    when Pid   :: pid(),
         Value :: term().
reset(Pid, Value) ->
    gen_server:call(Pid, {reset, Value}).

%%
%% gen_server callbacks
%%

init([]) ->
    init([nil]);
init([Value]) ->
    {ok, #state{atom=Value}}.

handle_call(deref, _From, State) ->
    {reply, State#state.atom, State};
handle_call({reset, Value}, _From, _State) ->
    {reply, Value, #state{atom=Value}};
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
