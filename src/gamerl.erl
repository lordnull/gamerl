-module(gamerl).

-export([set_index/1, set_default/1]).
-export([set_routes/1, set_route/2, set_route/3]).
-export([get_routes/0, get_compiled_routes/0]).

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

	IndexRouteDir = application:get_env(gamerl, index_directory, {priv_dir, gamerl, <<"www">>}),
	IndexRouteFile = application:get_env(gamerl, index_file, <<"index.html">>),
	IndexRouteArgs = [
		{directory, IndexRouteDir},
		{file, IndexRouteFile},
		{mimetypes, {{lngs_mime, from_path}, undefined}}
	],
	IndexRoute = {<<"/">>, cowboy_static, IndexRouteArgs},

	DefaultRouteDir = application:get_env(gamerl, default_directory, {priv_dir, gamerl, <<"www">>}),
	DefaultRouteArgs = [
		{directory, DefaultRouteDir},
		{mimetypes, {{ings_mime, from_path}, undefined}}
	],
	DefaultRoute = {<<"/[...]">>, cowboy_static, DefaultRouteArgs},

	AllRoutes = CustomRoutes ++ BuildinRoutes ++ [IndexRoute, DefaultRoute],
	Host = application:get_env(gamerl, listen_host, '_'),
	[{Host, AllRoutes}].

expand_route(Binary, Module) when is_binary(Binary) ->
	{Binary, Module, undefined};

expand_route({Binary, Args}, Module) ->
	{Binary, Module, Args}.

get_compiled_routes() ->
	Routes = get_routes(),
	cowboy_router:compile(Routes).

set_index(File) when is_binary(File) ->
	Directory = filename:dirname(File),
	Filename = filename:basename(File),
	ok = application:set_env(gamerl, index_file, Filename),
	ok = application:set_env(gamerl, index_directory, Directory),
	update_cowboy_routes().

set_default(Directory) ->
	ok = application:set_env(gamerl, default_directory, Directory),
	update_cowboy_routes().

set_routes(Module) when is_atom(Module) ->
	Routes = Module:routes(),
	lists:foreach(fun
		(ModuleRoute) when is_binary(ModuleRoute) ->
			set_route(ModuleRoute, Module);
		({RouteString, Args}) ->
			set_route(RouteString, Module, Args)
	end, Routes).

set_route(RouteString, Module) ->
	set_route(RouteString, Module, undefined).

set_route(RouteString, Module, Args) ->
	CurrentRoutes = application:get_env(gamerl, routes, []),
	Cleaned = case lists:keytake(RouteString, CurrentRoutes) of
		false ->
			CurrentRoutes;
		{value, _, New} ->
			New
	end,
	NewRoutes = [{RouteString, Module, Args} | Cleaned],
	ok = application:set_env(gamerl, routes, NewRoutes),
	update_cowboy_routes().

update_cowboy_routes() ->
	Compiled = get_compiled_routes(),
	cowboy:set_env(gamerl_listener, dispatch, Compiled).
