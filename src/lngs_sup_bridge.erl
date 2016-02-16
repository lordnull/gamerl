%% @doc A supervisor bridge that takes in any mfa that returns a pid and uses
%% it as what to supervise. This means very simple processes that don't need the
%% overhead of a gen_server can be spawned and supervised (cough
%% lngs_rec_user_auth:start_expire_server/0 cough).

-module(lngs_sup_bridge).
-behavior(supervisor_bridge).

-export([
	start_link/1,
	start_link/2,
	start_link/3,
	start_link/4
]).

-export([
	init/1,
	terminate/2
]).

start_link(Fun) when is_function(Fun) ->
	supervisor_bridge:start_link(?MODULE, Fun).

start_link(Name, Fun) ->
	supervisor_bridge:start_link(Name, ?MODULE, Fun).

start_link(M, F, A) ->
	supervisor_bridge:start_link(?MODULE, {M, F, A}).

start_link(Name, M, F, A) ->
	supervisor_bridge:start_link(?MODULE, {M, F, A}).

init(Starter) ->
	StartRes = case Starter of
		{M, F, A} ->
			erlang:apply(M, F, A);
		Fun when is_function(Fun) ->
			Fun()
	end,
	if
		is_pid(StartRes) ->
			{ok, StartRes, StartRes};
		true ->
			{error, not_spawn_function}
	end.

terminate(Why, Pid) ->
	RealReason = case Why of
		shutdown ->
			normal;
		_ ->
			Why
	end,
	exit(Pid, Why).
