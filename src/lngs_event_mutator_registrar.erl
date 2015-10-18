%% @doc The intent is to start up a new event mutator evaluator for each game
%% session without needing to keep hitting a long term storage solution. Load up
%% the rules once, and then away we go!
%%
%% This stores, for each game session, the mutators to events. It assumes the
%% mutators are stored as lngs_event_mutator records in lngs_data.
-module(lngs_event_mutator_registrar).
-behaviour(gen_server).
-define(SERVER(Id), {via, lngs_pid_lookup, {?MODULE, Id}}).

-record(rule, {
	game_id,
	base,
	overrides = []
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([is_allowed/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(GameId) ->
    gen_server:start_link(?SERVER(GameId), ?MODULE, GameId, []).

is_allowed(GameId, Action) ->
	gen_server:call(?SERVER(GameId), {is_allowed, Action}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(GameId) ->
	{ok, BaseRules} = lngs_data:t_search(lngs_event_mutator, [{overrides, []}]),
	{ok, AllRules} = lngs_data:t_search(lngs_event_mutator, []),
	TreedRules = load_rules(BaseRules, AllRules),
    {ok, {TreedRules, GameId}}.

handle_call({is_allowed, Action}, _From, {Rules, GameId} = State) ->
	Evaled = check_rules(Action, Rules, GameId),
	Reply = case Evaled of
		undefined ->
			{ok, Action};
		_ ->
			Evaled
	end,
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

load_rules(BaseRules, AllRules) ->
	lists:map(fun(BaseRule) ->
		load_rule(BaseRule, AllRules)
	end, BaseRules).

load_rule(BaseRule, AllRules) ->
	Overrides = lists:filter(fun(Rule) ->
		lngs_event_mutator:is_override_of(Rule, BaseRule)
	end, AllRules),
	LoadedOverrides = lists:map(fun(Rule) ->
		load_rule(Rule, AllRules)
	end, Overrides),
	#rule{base = BaseRule, overrides = LoadedOverrides}.

check_rules(Action, Rule, GameId) when is_record(Rule, rule) ->
	Rules = Rule#rule.overrides,
	case check_rules(Action, Rules, GameId) of
		undefined ->
			lngs_event_mutator:is_allowed(GameId, Action, Rule#rule.base);
		Else ->
			Else
	end;

check_rules(Action, Rules, GameId) when is_list(Rules) ->
	check_rules(Action, Rules, GameId, false).

check_rules(_Action, [], _GameId, false) ->
	undefined;

check_rules(Action, [Rule | Tail], GameId, AnyoneCared) ->
	case check_rules(Action, Rule, GameId) of
		undefined ->
			check_rules(Action, Tail, GameId, AnyoneCared);
		{deny, _Why} = Out ->
			Out;
		Else ->
			NewAction = maybe_new_action(Else, Action),
			check_rules(NewAction, Tail, GameId, true)
	end.

maybe_new_action(ok, Action) ->
	Action;

maybe_new_action({ok, Action}, _OldAction) ->
	Action.

