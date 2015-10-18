%% @doc Structure that defines a mutator, and how they interact with each other.
%% A mutaotr is a way to define exception based gameplay. For example, a game
%% rule may be that a a foobar attack deals 5 damage. However, to implement
%% a buff that says foobar attack deals 3 damage instead, you could modify the
%% code for the foobar attack, or you could define the foobar damage as an event,
%% and create a mutator that checks for the buff and overrides the event with
%% a new event that only deals 3 damage.
%%
%% Of course, exception based gameplay means there can be exceptions to
%% exceptions, so each mutator has an identifier, and each mutator can list
%% which mutators it overrides.
-module(lngs_event_mutator).
% This is here for for creating accessors and getters.
-compile([{parse_transform, rec2json}]).

-callback text_description() -> binary() | string().
-callback identifier() -> any().
-callback overrides() -> [any()].
-callback is_allowed(any()) -> 'ok' | {'ok', any()} | 'deny' | 'undefined'.

-record(lngs_event_mutator, {
	id :: pos_integer() | string(),
	overrides = [] :: [any()],
	text_description = <<"some rule">> :: string(),
	callback :: module() | fun((any()) -> 'ok' | 'deny' | 'undefined' | {'ok', any()})
}).

-export([create/1, create/4]).
-export([is_allowed/3]).

create(Identifier, Overrides, Description, CallbackFun) ->
	#lngs_event_mutator{
		id = Identifier,
		overrides = Overrides,
		text_description = Description,
		callback = CallbackFun
	}.

create(Module) when is_atom(Module) ->
	create(Module:identifier(), Module:overrides(), Module:text_description(), Module).

is_allowed(GameId, Action, #lngs_event_mutator{callback = Callback}) ->
	if
		is_atom(Callback) ->
			Callback:is_allowed(GameId, Action);
		is_function(Callback, 2) ->
			Callback(GameId, Action)
	end.
