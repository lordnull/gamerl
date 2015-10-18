%% @doc An event, in its simpliest form, is a function that mutates the game
%% state. Some actors in the game may want to react to the event, creating thier
%% own events, while some game states may require that an event's mutation be
%% modified.
%%
%% An example of the first is if a fighter can counter attack. The inital attack
%% event would happen (possibly including other events like takeing damage), then
%% the counter attack would happen.
%%
%% An example of the second is damage prevention. A damage event may mutate an
%% actors health down by 3, but if the actor has damage prevention of 1, it
%% should only mutate it down by 2.
%%
%% So an event will go through several stages: queued, modified, pre-fired,
%% and resolved.
%%
%% A queued event is waiting to be acted upon. For example, after an attack hits,
%% a damage event would be queued up. An lngs_event record is crated, and put
%% into the lngs_event_queue. There it waits until this pulls it.
%%
%% Once pulled, the event is compared against any mutators. A mutator can
%% determine the event cannot happen (such as damaging an already dead actor) or
%% the event has a different mutator (such as in the case of damage reduction or
%% inversion).
%%
%% Once the mutators have had thier chance, the event is now 'mutated', and
%% immediately goes into the next phase, 'pre-fired'. This allows actors that
%% have registered to the event listener to create new events to be fired either
%% before or after the event has resolved. The event itself cannot be altered.
%% For example, an actor may gain a bonus to attack after taking damage, or
%% speed. Or perhaps at the end of a turn, before the turn actually changes,
%% something else should change. After all listeners have received the event,
%% the event runner puts the event back into the queue, tagging it as having
%% prefired. It also puts any new events the listeners have generated into the
%% queue. Finally, it does a pull again.
%%
%% Eventually, the event runner will get an event that is tagged as 'pre-fired'.
%% The event runner then executes the event, thus mutating the game state. Then,
%% it does the same step for the event as the pre-fire, only this time, it is
%% post-fire, and the event that was just evaluated is not put back into queue.
%% At this point the event is 'resolved'.
-module(lngs_event_runner).
-behaviour(gen_server).
-define(SERVER(Id), {via, lngs_pid_lookup, {?MODULE, Id}}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Id) ->
    gen_server:start_link(?SERVER(Id), ?MODULE, Id, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Id) ->
    {ok, Id, 0}.

handle_call(_Request, _From, State) ->
    {reply, {error, invalid}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, Id) ->
	Action = lngs_event_queue:pull(Id),
    lager:debug("about to maybe fire an action: ~p", [Action]),
	ok = maybe_fire_action(Id, Action),
	{noreply, Id, 0};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

maybe_fire_action(Id, Action) ->
    lager:debug("Checking rules for action ~p", [Action]),
	case lngs_event_mutator_registrar:is_allowed(Id, Action) of
		{deny, Why} ->
			lager:debug("Didn't do action ~p due to ~p", [Action, Why]);
		{ok, MaybeNewAction} ->
            lager:debug("New action to fire: ~p", [MaybeNewAction]),
			fire_action(Id, MaybeNewAction)
	end.

fire_action(Id, Action) ->
	fire_action(Id, Action, lngs_event:pre_fired(Action)).

fire_action(Id, Action, false) ->
	{NewPrepended, Appended} = lngs_event_listener:pre_send(Id, Action),
	ok = lngs_event_queue:append(Id, Appended),
	Action2 = lngs_event:pre_fired(true, Action),
	Prepended = [Action2 | NewPrepended],
	lngs_event_queue:prepend(Id, Prepended);

fire_action(Id, Action, true) ->
	NewAction = lngs_event:evaluate(Id, Action),
    lager:debug("Action post evaluate: ~p", [NewAction]),
	{Prepended, Appended} = lngs_event_listener:post_send(Id, NewAction),
	ok = lngs_event_queue:prepend(Id, Prepended),
	lngs_event_queue:append(Id, Appended).
