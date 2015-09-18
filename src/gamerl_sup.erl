-module(gamerl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	case application:get_env(gamerl, start_reloader, false) of
		false ->
			ok;
		true ->
			application:ensure_all_started(reloader)
	end,

	UrlPort = application:get_env(gamerl, url_port, 3434),
	ListenPort = application:get_env(gamerl, listen_port, UrlPort),
	Listeners = application:get_env(gamerl, listeners, 10),
	Keyfile = application:get_env(gamerl, keyfile, code:priv_dir(gamerl) ++ "/key"),
	Certfile = application:get_env(gamerl, certfile, code:priv_dir(gamerl) ++ "/lngs.crt"),

	CompiledDispatch = gamerl:get_compiled_routes(),

	UrlProto = application:get_env(gamerl, url_protocol, https),
	ListenProto = application:get_env(gamerl, listen_proto, UrlProto),
	{ok, _} = case ListenProto of
		https ->
			cowboy:start_https(gamerl_listener, Listeners,
				[ {port, ListenPort}, {keyfile, Keyfile}, {certfile, Certfile}],
				[{env, [{dispatch, CompiledDispatch}]}] 
			);
		http ->
			cowboy:start_http(gamerl_listener, Listeners,
				[ {port, ListenPort} ], 
				[ {env, [{dispatch, CompiledDispatch}]} ]
			)
	end,
	Kids = [
	
		{lngs_pid_lookup, {lngs_pid_lookup, start_link, []}, permanent, 5000, worker, [lngs_pid_lookup]},

		{lngs_dets, {lngs_dets, start_link, []}, permanent, 5000, worker, [lngs_dets]},

		{lngs_data, {lngs_data, start_link, [lngs_data, lngs_dets]}, permanent, 5000, worker, [lngs_data]},

		{lngs_session, {lngs_session, start_link, []}, permanent, 5000, worker, [lngs_sesion]}
		
	],
    {ok, { {one_for_one, 5, 10}, Kids} }.
