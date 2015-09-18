-module(lngs_event_queue).
-behaviour(gen_server).

-record(state, {
    ets, next_id = 1,
    subs = []
}).

-record(sub, {
    id, pull_id, func
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([pull/3, push/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ServerId) ->
    gen_server:start_link(ServerId, ?MODULE, [], []).

pull(ServerId, ClientId, Fun) ->
    gen_server:cast(ServerId, {pull, ClientId, Fun}, infinity).

push(ServerId, Event) ->
    gen_server:cast(ServerId, {push, Event}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    Ets = ets:new(?MODULE, [public, ordered_set]),
    {ok, #state{ets = Ets}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({pull, ClientId, Fun}, State) ->
    {Sub, Subs} = case lists:keytake(ClientId, 2, State#state.subs) of
        false ->
            {#sub{id = ClientId, pull_id = 1, func = Fun}, State#state.subs};
        {value, OldSub, NewSubs} when OldSub#sub.func =:= undefined ->
            {OldSub#sub{func = Fun}, NewSubs};
        {value, OldSub, NewSubs} ->
            OldFun = OldSub#sub.func,
            OldFun(error, resubbed),
            {OldSub#sub{func = Fun}, NewSubs}
    end,
    Sub2 = maybe_push(Sub, State),
    Subs2 = lists:keystore(ClientId, 2, Subs, Sub2),
    {noreply, State#state{subs = Subs2}};

handle_cast({push, Event}, State) ->
    State2 = update_id(State),
    Entry = {State#state.next_id, Event},
    Subs = lists:map(fun
        (Sub = #sub{func = undefined}) ->
            Sub;
        (Sub) ->
            Fun = Sub#sub.func,
            Fun(ok, Entry),
            Sub#sub{func = undefined, pull_id = State2#state.next_id}
    end, State#state.subs),
    {noreply, State2#state{subs = Subs}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    lists:foreach(fun
        (#sub{func = undefined}) ->
            ok;
        (#sub{func = Fun}) ->
            Fun(error, {'EXIT', Reason})
    end, State#state.subs).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

maybe_push(Sub = #sub{pull_id = PullId}, _State = #state{next_id = PullId}) ->
    Sub;
maybe_push(Sub = #sub{pull_id = PullId}, State) ->
    Ets = State#state.ets,
    case ets:lookup(Ets, PullId) of
        [{PullId, Event}] ->
            Fun = Sub#sub.func,
            Fun(ok, Event),
            Sub#sub{pull_id = PullId + 1, func = undefined};
        [] ->
            Sub
    end.

update_id(State) ->
    Id = State#state.next_id,
    State#state{next_id = Id + 1}.
