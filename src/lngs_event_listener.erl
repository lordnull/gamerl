%% @doc Registrar for actors to respond to events. Also the behavior definer for
%% said actors. The actors are for the most part like a gen_event module, just
%% with an additional handle_game_event function.
%%
%% While evaluating the handle_game_event, the callback module can use the
%% {@link reply/3}, {@link reply_append/2}, {@link reply_prepend} functions to
%% indicate new events in response to the one that is being evaluated. Not that
%% listeners here cannot modify the event. If that is desired, a mutator needs
%% to be registered instead.
-module(lngs_event_listener).
-behavior(gen_event).

-callback handle_game_event(From :: any(), PreOrPost :: 'pre' | 'post', Event :: any(), Args :: any()) -> any().
-callback handle_call(Call :: any(), State :: any()) -> any().
-callback handle_info(Info :: any(), State :: any()) -> any().
-callback terminate(Why :: any(), State :: any()) -> any().
-callback init(any()) -> {'ok', any()}.

-define(SERVER(Id), {via, lngs_pid_lookup, {?MODULE, Id}}).

-export([start_link/1]).
-export([add_listener/3, send/3, pre_send/2, post_send/2]).
-export([reply_prepend/2, reply_append/2, reply/3]).
-export([call/3]).
-export([
	init/1, terminate/2, code_change/3,
	handle_call/2, handle_event/2, handle_info/2
]).

%% @doc Start a event listener with the given id, registered using the
%% lngs_pid_lookup.
start_link(Id) ->
	gen_event:start_link(?SERVER(Id)).

%% @doc Add an actor. Note that the handler can be either module() or {module(), any()},
%% allowing the same module to be used multiple times (just like gen_event).
add_listener(Id, Handler, Args) ->
	gen_event:add_handler(?SERVER(Id), {?MODULE, Handler}, {Handler, Args, Id}).

%% @doc Alter the listeners that a new event is coming through, and whether it's
%% come through before (post) or if it's new (pre). Returns to the caller the
%% events to prepend and the events to append.
-spec send(Id :: any(), PreOrPost :: 'pre' | 'post', Event :: any()) -> {[any()], [any()]}.
send(Id, PreOrPost, Event) ->
	Self = self(),
	Ref = make_ref(),
	lager:debug("notifying the world that we're doing a thing: ~p", [Event]),
	ok = gen_event:sync_notify(?SERVER(Id), {action_event, {Self, Ref}, PreOrPost, Event}),
	PrependingEvents = get_prependers(Ref),
	AppendingEvents = get_appenders(Ref),
	SortFun = fun by_priority/2,
	{lists:sort(SortFun, PrependingEvents), lists:sort(SortFun, AppendingEvents)}.

%% @doc For use by listeners in thier 'handle_game_event' callback function.
reply(FromRef, PreOrPost, ActionList) when is_list(ActionList) ->
	{Pid, Ref} = FromRef,
	Pid ! {PreOrPost, Ref, ActionList},
	ok;

reply(FromRef, PreOrPost, Action) ->
	reply(FromRef, PreOrPost, [Action]).

%% @doc For use by listeners in thier 'handle_game_event' callback function.
reply_append(FromRef, ActionList) ->
	reply(FromRef, append, ActionList).

%% @doc For use by listeners in thier 'handle_game_event' callback function.
reply_prepend(FromRef, ActionList) ->
	reply(FromRef, prepend, ActionList).

pre_send(Id, Action) ->
	send(Id, pre, Action).

post_send(Id, Action) ->
	send(Id, post, Action).

get_prependers(Ref) ->
	receive_replies(Ref, prepend).

get_appenders(Ref) ->
	receive_replies(Ref, append).

call(Id, HandlerId, Call) ->
	gen_event:call(?SERVER(Id), {?MODULE, HandlerId}, Call, infinity).

init({SwapType, ok}) ->
	init(SwapType);

init({Handler, Args, Id}) ->
	TrueCallback = case Handler of
		{Atom, _HandlerId} when is_atom(Atom) ->
			Atom;
		Handler when is_atom(Handler) ->
			Handler
	end,
	{ok, CbState} = TrueCallback:init(Args),
	{ok, {TrueCallback, Id, CbState}}.

handle_event({action_event, From, PreOrPost, Action}, {Callback, _Id, Args} = State) ->
	RawResponse = Callback:handle_game_event(From, PreOrPost, Action, Args),
	fixup_response(RawResponse, State).

handle_call(Call, {Callback, _Id, Args} = State) ->
	RawResponse = Callback:handle_call(Call, Args),
	fixup_response(RawResponse, State).

handle_info(Info, {Callback, _Id, Args} = State) ->
	RawResponse = Callback:handle_info(Info, Args),
	fixup_response(RawResponse, State).

terminate(Why, {Callback, _Id, Args}) ->
	_ = Callback:terminate(Why, Args),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

receive_replies(Ref, ReplyType) ->
	receive_replies(Ref, ReplyType, []).

receive_replies(Ref, ReplyType, Acc) ->
	receive
		{ReplyType, Ref, Actions} ->
			receive_replies(Ref, ReplyType, Acc ++ Actions)
	after
		0 ->
			Acc
	end.

fixup_response(remove_handler, _State) ->
	remove_handler;
fixup_response({remove_handler, _} = Reply, _State) ->
	Reply;
fixup_response({swap_handler, Why, NewState, NewHandler, NewArgs}, {_Callback, Id, _OldCbState}) ->
	NewCallback = case NewHandler of
		{Atom, _} -> Atom;
		_ -> NewHandler
	end,
	{swap_handler, Why, {NewCallback, Id, NewState}, {?MODULE, NewHandler}, {NewHandler, Id, NewArgs}};
fixup_response({swap_handler, Reply, Why, NewState, NewHandler, NewArgs}, {_Callback, Id, _OldCbState}) ->
	NewCallback = case NewHandler of
		{Atom, _} -> Atom;
		_ -> NewHandler
	end,
	{swap_handler, Reply, Why, {NewCallback, Id, NewState}, {?MODULE, NewHandler}, {NewHandler, Id, NewArgs}};
fixup_response({ok, NewState}, {Callback, Id, _Args}) ->
	{ok, {Callback, Id, NewState}};
fixup_response({ok, NewState, hibernate}, {Callback, Id, _}) ->
	{ok, {Callback, Id, NewState}, hibernate};
fixup_response({ok, Reply, NewState}, {Callback, Id, _}) ->
	{ok, Reply, {Callback, Id, NewState}};
fixup_response({ok, Reply, NewState, hibernate}, {Callback, Id, _}) ->
	{ok, Reply, {Callback, Id, NewState}, hibernate}.

by_priority(ActionA, ActionB) ->
	lngs_event:priority(ActionA) =< lngs_event:priority(ActionB).
