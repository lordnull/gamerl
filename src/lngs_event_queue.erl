%% @doc An event queue serves up one event per pull. If more than one process
%% pulls, they will get different events. A process that pulls is blocked until
%% an event is put. The queue doesn't care about how an event is defined, it
%% simply provides an place to put and pull them.
-module(lngs_event_queue).
-behaviour(gen_fsm).
-define(SERVER(ID), {via, lngs_pid_lookup, {?MODULE, ID}}).

-type event() :: any().

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([pull/1, append/2, prepend/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([
	init/1, terminate/3,
	handle_event/3, handle_sync_event/4,
	handle_info/3,
	code_change/4
]).
-export([
	all_empty/2, all_empty/3,
	no_pullers/2, no_pullers/3,
	no_actions/2, no_actions/3
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Start a new server using the given id, registered usin the
%% lngs_pid_lookup.
-spec start_link(Id :: any()) -> {'ok', pid()}.
start_link(Id) ->
    gen_fsm:start_link(?SERVER(Id), ?MODULE, Id, []).

%% @doc Return the oldest event in the queue, blocking if the queue is empty.
-spec pull(Id :: any()) -> any().
pull(Id) ->
	gen_fsm:sync_send_event(?SERVER(Id), pull, infinity).

%% @doc Append the given events to the queue.
-spec append(Id :: any(), event() | [event()]) -> 'ok'.
append(_Id, []) ->
	ok;

append(Id, Events) when is_list(Events) ->
	gen_fsm:sync_send_event(?SERVER(Id), {append, Events}, infinity);

append(Id, Event) ->
	append(Id, [Event]).

%% @doc Prepend the given events to the queue.
-spec prepend(Id :: any(), event() | [event()]) -> 'ok'.
prepend(_Id, []) ->
	ok;

prepend(Id, Actions) when is_list(Actions) ->
	gen_fsm:sync_send_event(?SERVER(Id), {prepend, Actions}, infinity);

prepend(Id, Action) ->
	prepend(Id, [Action]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(ID) ->
	_ = put(server_id, ID),
    {ok, all_empty, undefined}.

all_empty(pull, From, _State) ->
	{next_state, no_actions, [From]};

all_empty({SomePend, Actions}, _From, _State) when SomePend =:= prepend; SomePend =:= append ->
	{reply, ok, no_pullers, Actions}.

all_empty(_Event, State) ->
	{next_state, all_empty, State}.

no_pullers(pull, From, Actions) ->
	[Action | Rest] = Actions,
	gen_fsm:reply(From, Action),
	case Rest of
		[] ->
			{next_state, all_empty, []};
		_ ->
			{next_state, no_pullers, Rest}
	end;

no_pullers({SomePend, NewActions}, _From, Actions) when SomePend =:= prepend; SomePend =:= append ->
	NewState = case SomePend of
		prepend ->
			NewActions ++ Actions;
		append ->
			Actions ++ NewActions
	end,
	{reply, ok, no_pullers, NewState}.

no_pullers(_Event, State) ->
	{next_state, no_pullers, State}.

no_actions(pull, From, Pullers) ->
	{next_state, no_actions, Pullers ++ [From]};

no_actions({SomePend, NewActions}, From, Pullers) when SomePend =:= prepend; SomePend =:= append ->
	gen_fsm:reply(From, ok),
	case consume(Pullers, NewActions) of
		{pullers, NewPullers} ->
			{next_state, no_actions, NewPullers};
		{actions, RemainingActions} ->
			{next_state, no_pullers, RemainingActions};
		undefined ->
			{next_state, all_empty, undefined}
	end.

no_actions(_Event, State) ->
	{next_state, no_actions, State}.

handle_sync_event(_Event, _From, StateName, State) ->
	{reply, {error, invalid}, StateName, State}.

handle_event(_Event, StateName, State) ->
	{reply, {error, invalid}, StateName, State}.

handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

consume([], []) ->
	undefined;

consume([], Actions) ->
	{actions, Actions};

consume(Pullers, []) ->
	{pullers, Pullers};

consume([Puller | RestPullers], [Action | RestActions]) ->
	gen_fsm:reply(Puller, Action),
	consume(RestPullers, RestActions).
