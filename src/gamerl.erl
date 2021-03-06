%% @doc Gamerl is an application to help build web based games with an erlang
%% server. It provides tools for rest style resources, as well as action and
%% event queuing.
%%
%% This module is primarily for managing a running gamerl system, and the
%% associated web resources; eg, ensuring the running cowboy instance behanves.
-module(gamerl).

-type host_name() :: binary().
-type path() :: binary().
-type route_rule() :: {path(), module(), [any()]}.

-export([set_index/1, set_index/2, set_default/1, set_default/2]).
-export([set_routes/1, set_route/2, set_route/3]).
-export([get_routes/0, get_compiled_routes/0]).

%% @doc Return the routes option as it should be used by cowboy.
-spec get_routes() -> [{host_name(), [route_rule()]}].
get_routes() ->
	CustomRoutes = application:get_env(gamerl, routes, []),

	RoutableMods = [lngs_http_auth, lngs_http_websocket, lngs_rest_default],
	BuildinRoutes = lists:foldl(fun(Module, Acc) ->
		RawModRoutes = Module:routes(),
		lists:foldl(fun(RawModuleRoute, InnerAcc) ->
			Expanded = expand_route(RawModuleRoute, Module),
			InnerAcc ++ [Expanded]
		end, Acc, RawModRoutes)
	end, [], RoutableMods),

	MimeTypes = {mimetypes, cow_mimetypes, all},

	IndexRouteFile = application:get_env(gamerl, index_file, {priv_file, gamerl, <<"www/index.html">>}),
	IndexRouteArgs = [ MimeTypes ],
	IndexRoute = {<<"/">>, cowboy_static, erlang:append_element(IndexRouteFile, IndexRouteArgs)},

	DefaultRouteDir = application:get_env(gamerl, default_directory, {priv_dir, gamerl, <<"www">>}),
	DefaultRouteArgs = [ MimeTypes ],
	DefaultRoute = {<<"/[...]">>, cowboy_static, erlang:append_element(DefaultRouteDir, DefaultRouteArgs)},

	AllRoutes = CustomRoutes ++ BuildinRoutes ++ [IndexRoute, DefaultRoute],
	Host = application:get_env(gamerl, listen_host, '_'),
	[{Host, AllRoutes}].

expand_route(Binary, Module) when is_binary(Binary) ->
	{Binary, Module, undefined};

expand_route({Binary, Args}, Module) ->
	{Binary, Module, Args}.

%% @doc This is the same as calling `cowboy_router:compile(get_routes())'.
get_compiled_routes() ->
	Routes = get_routes(),
	cowboy_router:compile(Routes).

%% @doc Sets the file to serve when the requested page is '/'.
set_index(File) when is_binary(File) ->
	Directory = filename:dirname(File),
	Filename = filename:basename(File),
	ok = application:set_env(gamerl, index_file, Filename),
	ok = application:set_env(gamerl, index_directory, Directory),
	update_cowboy_routes().

%% @doc Sets the file to serve when the requested page is '/'. This sets the
%% index page to a file in the priv dir of the given erlang application.
set_index(AppName, IndexFile) ->
	ok = application:set_env(gamerl, index_file, {priv_file, AppName, IndexFile}),
	update_cowboy_routes().

%% @doc Sets the directory to look for files that do not match the registered
%% resources.
set_default(Directory) ->
	ok = application:set_env(gamerl, default_directory, Directory),
	update_cowboy_routes().

%% @doc Sets the directory to look for files that do not match the regsitered
%% resrouces as a subdirectory of an erlang appliction's priv directory.
set_default(AppName, Directory) ->
	ok = application:set_env(gamerl, default_directory, {priv_dir, AppName, Directory}),
	update_cowboy_routes().

%% @doc Set the resources based on module or list of modules. If a list, then
%% the routes are added. In short, providing a list `Modules' is equivalent
%% to `lists:foreach(fun set_routes/1, Modules)'.
%%
%% Each module passed in must export a 'routes/0' function, which returns a list
%% of routes. The return routes are expended as such:
%% * Path :: binary() -> {Path, [], Module, []}
%% * {Path :: binary(), Args :: term()} -> {Path, [], Module, Args}
%% * {Path :: binary(), Constraints :: [term()], Args :: term()} -> {Path, Constraints, Module, Args}
%% * {Path :: binary(), AltModule :: atom(), Args :: term()} -> {Path, [], AltModule, Args}
%% * {_Path, _Constratints, _AltModule, _Args} = Route -> Route
set_routes(Modules) when is_list(Modules) ->
	lists:foreach(fun set_routes/1, Modules);

set_routes(Module) when is_atom(Module) ->
	Routes = Module:routes(),
	lists:foreach(fun
		(ModuleRoute) when is_binary(ModuleRoute) ->
			set_route(ModuleRoute, Module);
		({RouteString, Args}) ->
			set_route(RouteString, Module, Args);
		({RouteString, Constraints, Args}) when is_list(Constraints) ->
			set_route(RouteString, Constraints, Module, Args);
		({RouteString, AltModule, Args}) when is_atom(AltModule) ->
			set_route(RouteString, [], AltModule, Args);
		({RouteString, Constraints, AltModule, Args}) ->
			set_route(RouteString, Constraints, AltModule, Args)
	end, Routes).

%% @doc Set the given `RouteString' to use `Module' with `undefined' as the
%% argument.
set_route(RouteString, Module) ->
	set_route(RouteString, [], Module, undefined).

%% @doc Set the given `RotueString' to use `Module' with the given `Args'.
set_route(RouteString, Module, Args) ->
	set_route(RouteString, [], Module, Args).

%% @doc Set the given `RotueString' to use `Module' under given `Constraints'
%% and behing passed in the `Args'.
set_route(RouteString, Constraints, Module, Args) ->
	CurrentRoutes = application:get_env(gamerl, routes, []),
	Cleaned = case lists:keytake(RouteString, 1, CurrentRoutes) of
		false ->
			CurrentRoutes;
		{value, _, New} ->
			New
	end,
	RouteTuple = case Constraints of
		[] ->
			{RouteString, Module, Args};
		_ ->
			{RouteString, Constraints, Module, Args}
	end,
	NewRoutes = [RouteTuple | Cleaned],
	ok = application:set_env(gamerl, routes, NewRoutes),
	update_cowboy_routes().

update_cowboy_routes() ->
	Compiled = get_compiled_routes(),
	cowboy:set_env(gamerl_listener, dispatch, Compiled).
