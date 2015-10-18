%% @doc Provides a simple structure for manageing an events state.
-module(lngs_event).
% this is just for creating the accessors and setters.
-compile([{parse_transform, rec2json}]).

-record(lngs_event, {
	priority = 0,
	pre_fired = false :: boolean(),
	name,
	details,
	eval = fun() -> ok end :: fun((Id :: any(), Details :: any()) -> any()) | {atom(), atom(), [any()]}
}).

-export([create/3, create/4, evaluate/2]).

%% @doc like {create/4}, only the priority is the default 0.
create(Name, Details, Eval) ->
	create(Name, Details, 0, Eval).

%% @doc Create a new event record. Generally, it is a good idea to make the
%% details have as much information about what will happen as possible, as
%% those details are passed in (along with a game session id) to the evaulation
%% function. Thus:
%%
%% DO:
%%     create(<<"deal_damage">>, {<<"player1">>, 27, cold}, 0, fun damage:do_it/2).
%%
%% DON'T:
%%     create(<<"deal_damage">>, undefined, 0, fun(Game, _) -> damage:do_it(Game, <<"player1">>, 27, cold) end).
create(Name, Details, Priority, Eval) ->
	#lngs_event{
		priority = Priority,
		name = Name,
		details = Details,
		eval = Eval
	}.

evaluate(Id, Event) when is_record(Event, lngs_event) ->
	Fun = Event#lngs_event.eval,
	Details = Event#lngs_event.details,
	NewDetails = case catch Fun(Id, Details) of
		Out ->
			Out
	end,
	Event:details(NewDetails).
