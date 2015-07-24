-module(lngs_pid_lookup).
-behavior(gen_server).

-define(COUNTER_ETS, lngs_pid_lookup_counter).

-export([
	register_name/2,
	unregister_name/1,
	whereis_name/1,
	send/2
]).
-export([start_link/0, next_id/1]).
-export([
	init/1,
	handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3
]).

-record(state, {types = []}).

-type lookup_name() :: any().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link() ->
	?MODULE = lngs_util:ensure_ets(?MODULE, [named_table, public]),
	?COUNTER_ETS = lngs_util:ensure_ets(?COUNTER_ETS, [named_table, public]),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Get an unregistered id for use with lookup registration.
%% As long as no registrations of the given type have been stored without first
%% using next_id/0, the id is guarenteed to be unique and unused.
-spec next_id(Type :: any()) -> non_neg_integer().
next_id(Type) ->
	ets:update_counter(?COUNTER_ETS, Type, {2, 1, 1000000, 0}, {Type, 0}).

-spec register_name(lookup_name(), pid()) -> 'yes' | 'no'.
register_name(Name, Pid) ->
	gen_server:call(?MODULE, {register, Name, Pid}, infinity).

unregister_name(Name) ->
	case ets:lookup(?MODULE, Name) of
		[] ->
			ok;
		[{Name, Pid, Mon}] ->
			gen_server:call(?MODULE, {unregister, Name, Pid, Mon})
	end.

whereis_name(Name) ->
	case ets:lookup(?MODULE, Name) of
		[] ->
			undefined;
		[{Name, Pid, _Mon}] ->
			Pid
	end.

send(Ref,Msg) ->
	maybe_send(Ref, Msg, whereis_name(Ref)).

maybe_send(Ref, Msg, undefined) ->
	error({badarg, {Ref, Msg}});

maybe_send(_Ref, Msg, Pid) ->
	Pid ! Msg,
	Pid.

init(_) ->
	{ok, #state{}}.

handle_call({register, Name, Pid}, _From, State) ->
	case ets:lookup(?MODULE, Name) of
		[] ->
			Mon = erlang:monitor(process, Pid),
			ets:insert(?MODULE, {Name, Pid, Mon}),
			{reply, yes, State};
		[{Name, Pid, _Mon}] ->
			{reply, yes, State};
		_ ->
			{reply, no, State}
	end;

handle_call({unregister, Name, _Pid, Mon}, _From, State) ->
	_ = demonitor(Mon, [flush]),
	_ = ets:delete(?MODULE, Name),
	{reply, ok, State};

handle_call(_, _From, State) ->
	{reply, {error, invalid}, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info({'DOWN', Mon, process, Pid, _Why}, State) ->
	_ = ets:match_delete(?MODULE, {'_', Pid, Mon}),
	{noreply, State}.

terminate(_, _) ->
	ok.

code_change(_, State, _) ->
	{ok, State}.

-ifdef(TEST).

use_test_() ->
	{spawn, {setup, fun() ->
		{ok, Pid} = lngs_pid_lookup:start_link(),
		P1 = spawn(fun() -> receive _ -> ok end end),
		P2 = spawn(fun() -> receive _ -> ok end end),
		{Pid, P1, P2}
	end,
	fun({Pid, P1, P2}) ->
		P1 ! ok,
		P2 ! ok,
		unlink(Pid),
		exit(Pid, shutdown)
	end,
	fun({_Lookup, P1, P2}) -> [

		{"Able to get an id", fun() ->
			Got = lngs_pid_lookup:next_id(pants),
			?assert(is_integer(Got))
		end},

		{"able to register", fun() ->
			Got = lngs_pid_lookup:register_name(pants, P1),
			?assertEqual(yes, Got)
		end},

		{"able to reregister", fun() ->
			Got = lngs_pid_lookup:register_name(pants, P1),
			?assertEqual(yes, Got)
		end},

		{"cannot duplicate register", fun() ->
			Got = lngs_pid_lookup:register_name(pants, P2),
			?assertEqual(no, Got)
		end},

		{"Can register after exit", fun() ->
			P1 ! ok,
			mon_wait(P1),
			timer:sleep(timer:seconds(1)),
			Got = lngs_pid_lookup:register_name(pants, P2),
			?assertEqual(yes, Got)
		end},

		{"Can do lookup", fun() ->
			Got = lngs_pid_lookup:whereis_name(pants),
			?assertEqual(P2, Got)
		end},

		{"Can unregister", fun() ->
			ok = lngs_pid_lookup:unregister_name(pants),
			Got = lngs_pid_lookup:whereis_name(pants),
			?assertEqual(undefined, Got)
		end},

		{"Can do a send", fun() ->
			yes = lngs_pid_lookup:register_name(pants, P2),
			Got = lngs_pid_lookup:send(pants, hi),
			?assertEqual(P2, Got)
		end},

		{"error badarg on send to unregistered", fun() ->
			ok = lngs_pid_lookup:unregister_name(pants),
			?assertError({badarg, {pants, hi}}, lngs_pid_lookup:send(pants, hi))
		end}
	] end}}.

mon_wait(P) when is_pid(P) ->
	_ = erlang:monitor(process, P),
	receive
		{'DOWN', _, process, P, _} ->
			ok
	end.

-endif.

