-module(lngs_util).
-export([hexstr_to_bin/1, bin_to_hexstr/1]).
-export([make_url/0, make_url/1]).
-export([bind/2, all/2]).
-export([ensure_ets/2]).

% functions to help set up testing
%-export([start_testnode/0, start_testnode/1, start_testnode/2, add_paths/0,
%	cover_mods/1]).

%% @doc Converts binaries to hexidecimal strings.
-spec(bin_to_hexstr/1 :: (Bin :: binary()) -> [48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 97 | 98 | 99 | 100 | 101 | 102]).
bin_to_hexstr(Bin) ->
	string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)])).

%% @doc Converts a hexidecimal string in any case to a binary.
-spec(hexstr_to_bin/1 :: (S :: string()) -> binary() | 'error').
hexstr_to_bin(S) ->
	hexstr_to_bin(string:to_lower(S), []).

-define(HEX, [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f]).

%% @private
-spec(hexstr_to_bin/2 :: (string(), Acc :: string()) -> binary() | 'error').
hexstr_to_bin([], Acc) ->
	list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X, Y | T], Acc) ->
	case {lists:member(X, ?HEX), lists:member(Y, ?HEX)} of
		{true, true} ->
			{ok, [V], []} = io_lib:fread("~16u", [X, Y]),
			hexstr_to_bin(T, [V | Acc]);
		_Else ->
			error
	end.

make_url() ->
	make_url([]).

make_url(Path) ->
	Proto = application:get_env(shipshoot, protocol, https),
	Host = application:get_env(shipshoot, hostname, "localhost"),
	Port = application:get_env(shipshoot, url_port, 3434),
	make_url(Proto, Host, Port, Path).

make_url(Proto, Host, Port, Path) when is_list(Path), length(Path) > 0, is_list(hd(Path)) ->
	Path1 = filename:join(Path),
	make_url(Proto, Host, Port, Path1);
make_url(Proto, Host, Port, [$/ | Path]) ->
	make_url(Proto, Host, Port, Path);
make_url(Proto, Host, Port, <<$/, Path/binary>>) ->
	make_url(Proto, Host, Port, Path);
make_url(Proto, Host, Port, Path) ->
	iolist_to_binary(io_lib:format("~s://~s:~p/~s", [Proto, Host, Port, Path])).


bind(Arg, []) ->
	{ok, Arg};

bind(Arg, [Fun | Tail]) ->
	case Fun(Arg) of
		{ok, Arg2} ->
			bind(Arg2, Tail);
		Else ->
			Else
	end.

all(_Arg, []) ->
	true;

all(Arg, [Fun | Tail]) ->
	all(Arg, Fun(Arg), Tail).

all(_Arg, false, _Tail) ->
	false;

all(Arg, Res, []) ->
	Res;

all(Arg, true, [Fun | Tail]) ->
	all(Arg, Fun(Arg), Tail).

ensure_ets(Name, Options) ->
	case ets:info(Name) of
		undefined ->
			ets:new(Name, Options);
		_ ->
			Name
	end.

%
%
%
%
%
%
%
%
%
%
%
%
%
%
%
%start_testnode() ->
%	add_paths(),
%	case node() of
%		nonode@nohost ->
%			[] = os:cmd("epmd -daemon"),
%			case net_kernel:start([rpg_battlemap_test, shortnames]) of
%				{ok, _} ->
%					node();
%				{error, {{already_started, _}, _}}  ->
%					node();
%				_ ->
%					erlang:error(node_fail)
%			end;
%		Else ->
%			Else
%	end.
%
%start_testnode(Name) ->
%	start_testnode(Name, net_adm:localhost()).
%
%start_testnode(Name, Host) ->
%	start_testnode(),
%	case slave:start_link(Host, Name) of
%		{ok, N} -> 
%			rpc:call(N, ?MODULE, add_paths, []),
%			Covered = [Mod || {Mod, cover_compiled} <- code:all_loaded()],
%			rpc:call(N, ?MODULE, cover_mods, [Covered]),
%			N;
%		{error, {already_running, N}} -> N
%	end.
%
%add_paths() ->
%	{Pre, Deps} = case {file:list_dir("deps"), file:list_dir("../deps")} of
%		{{ok, Files}, _} -> {"deps/", Files};
%		{_, {ok, Files}} -> {"../deps/", Files};
%		_ -> {[], []}
%	end,
%	Paths = [Pre ++ X ++ "/ebin" || X <- Deps],
%	code:add_paths(Paths).
%
%cover_mods([]) ->
%	cover:start();
%cover_mods([Mod | Tail]) ->
%	{ok, Mod} = cover:compile_beam(Mod),
%	cover_mods(Tail).
%
%
%
%
%-module(rpgb).
%
%-ifdef(TEST).
%-include_lib("eunit/include/eunit.hrl").
%-endif.
%
%-export([res_init/1, get_env/1, get_env/2, get_dropbox_api_key/0,
%	make_url/0, make_url/1, sluggify/1, get_routes/2]).
%%-export([is_printable/1, is_not_printable/1, is_string/1]).
%%-export([to_json/1, to_json/2]).
%-export([set_proplist/3, set_proplist/2, scrub_disallowed/2]).
%-export([now_to_timestamp/1]).
%-export([bind/2]).
%-export([splice/3,splice/4]).
%-export([snip/2]).
%-export([start_app/1]).
%
%scrub_disallowed(Json, []) ->
%	Json;
%
%scrub_disallowed(Json, [Nope | Tail] = Nopes) ->
%	case proplists:delete(Nope, Json) of
%		Json ->
%			scrub_disallowed(Json, Tail);
%		Json2 ->
%			scrub_disallowed(Json2, Nopes)
%	end.
%
%start_app(AppName) ->
%	case application:start(AppName) of
%		{error, {not_started, Dep}} ->
%			start_app(Dep),
%			start_app(AppName);
%		Else ->
%			Else
%	end.
%
%res_init(Term) ->
%	case get_env(trace) of
%		undefined ->
%			{ok, Term};
%		{ok, TraceDir} ->
%			{{trace, TraceDir}, Term}
%	end.
%
%get_env(Key) ->
%	application:get_env(rpg_battlemap, Key).
%
%get_env(Key, Default) ->
%	case application:get_env(rpg_battlemap, Key) of
%		undefined -> {ok, Default};
%		E -> E
%	end.
%
%get_dropbox_api_key() ->
%	{ok, Key} = get_env(dropbox_api_key, ""),
%	Key.
%
%make_url() ->
%	make_url([]).
%
%make_url(Path) ->
%	{ok, Proto} = get_env(protocol, https),
%	{ok, Host} = get_env(hostname, "localhost"),
%	{ok, Port} = get_env(port, 9090),
%	make_url(Proto, Host, Port, Path).
%
%make_url(Proto, Host, Port, Path) when is_list(Path), length(Path) > 0, is_list(hd(Path)) ->
%	Path1 = filename:join(Path),
%	make_url(Proto, Host, Port, Path1);
%make_url(Proto, Host, Port, [$/ | Path]) ->
%	make_url(Proto, Host, Port, Path);
%make_url(Proto, Host, Port, <<$/, Path/binary>>) ->
%	make_url(Proto, Host, Port, Path);
%make_url(Proto, Host, Port, Path) ->
%	iolist_to_binary(io_lib:format("~s://~s:~p/~s", [Proto, Host, Port, Path])).
%
%sluggify(Binary) when is_binary(Binary) ->
%	list_to_binary(sluggify(binary_to_list(Binary)));
%sluggify(String) ->
%	{ok, CleanInvalid} = re:compile("[^-a-zA-Z0-9,&\s]+", [caseless]),
%	{ok, DashToUnder} = re:compile("-"),
%	{ok, SpaceToDash} = re:compile("\\s"),
%	Regs = [{CleanInvalid,""},{DashToUnder,"_"},{SpaceToDash,"-"}],
%	lists:foldl(fun({Req,Rep},S) -> re:replace(S,Req,Rep) end,String,Regs).
%
%set_proplist(Key, Val, Proplist) ->
%	set_proplist([{Key, Val}], Proplist).
%
%set_proplist(New, Old) ->
%	New0 = [pl_expand(X) || X <- New],
%	Old0 = [pl_expand(X) || X <- Old],
%	New1 = lists:keysort(1, New0),
%	Old1 = lists:keysort(1, Old0),
%	Merged = lists:ukeymerge(1, New1, Old1),
%	Merged0 = [M || {_K, V} = M <- Merged, V /= undefined],
%	proplists:compact(Merged0).
%
%pl_expand(Proplist) ->
%	pl_expand(Proplist, []).
%
%pl_expand([], Acc) ->
%	lists:reverse(Acc);
%
%pl_expand([{_K,_V} = H | Tail], Acc) ->
%	pl_expand(Tail, [H | Acc]);
%
%pl_expand([Atom | Tail], Acc) ->
%	pl_expand(Tail, [{Atom, true} | Acc]).
%
%now_to_timestamp({Mega, Sec, Micro}) ->
%	% might not be fast, but it's easy.
%	[Mega0, Sec0, Micro0] = [integer_to_list(X) || X <- [Mega, Sec, Micro]],
%	Sec1 = lists:append(Mega0, Sec0, ".", Micro0),
%	Sec2 = list_to_float(Sec1),
%	Sec2 * 1000.
%
%get_routes(HP, Mods) ->
%	get_routes(HP, Mods, []).
%
%get_routes(_HP, [], Acc) ->
%	lists:reverse(Acc);
%
%get_routes(HP, [Mod | Tail], Acc) ->
%	Routes = Mod:get_routes(),
%	Acc2 = make_route_tuple(HP, Mod, Routes, Acc),
%	get_routes(HP, Tail, Acc2).
%
%make_route_tuple(_HP, _Mod, [], Acc) ->
%	Acc;
%
%make_route_tuple(HP, Mod, [{Route, Opts} | Tail], Acc) when is_binary(Route), is_list(Opts) ->
%	Tuple = {Route, Mod, [HP | Opts]},
%	make_route_tuple(HP, Mod, Tail, [Tuple | Acc]);
%make_route_tuple(HP, Mod, [Route | Tail], Acc) when is_binary(Route) ->
%	Tuple = {Route, Mod, [HP]},
%	make_route_tuple(HP, Mod, Tail, [Tuple | Acc]);
%make_route_tuple(HP, Mod, [{_Path, _OtherMod, _OtherArgs} = Tuple| Tail], Acc) ->
%	make_route_tuple(HP, Mod, Tail, [Tuple | Acc]).
%
%bind(Arg, []) ->
%	{ok, Arg};
%
%bind(Arg, [Fun | Tail]) ->
%	case Fun(Arg) of
%		{ok, Arg2} ->
%			bind(Arg2, Tail);
%		Else ->
%			Else
%	end.
%
%splice(List, Start, Delete) ->
%	splice(List, Start, Delete, []).
%
%splice(List, Start, Delete, Inserts) when is_integer(Start), is_integer(Delete), 
%		is_list(List), is_list(Inserts), Delete >= 0, Delete + Start - 1 =< length(List),
%		Start >= 1, Delete >= 0 ->
%	{Head, Tail} = lists:split(Start - 1, List),
%	Nommed = delete_n(Tail, Delete),
%	Head ++ Inserts ++ Nommed.
%
%delete_n(List, 0) ->
%	List;
%delete_n([_ | List], N) when is_integer(N), N > 0 ->
%	delete_n(List, N - 1).
%
%snip(_Nth, []) ->
%	erlang:error(badarg);
%
%snip(Nth, List) when is_integer(Nth), is_list(List), 1 =< Nth, Nth =< length(List) ->
%	{Head, [_ | Tail]} = lists:split(Nth - 1, List),
%	Head ++ Tail.
%
%-ifdef(TEST).
%
%bind_test_() -> [
%	{"three successes", fun() ->
%		F1 = fun(a) ->
%			{ok, b}
%		end,
%		F2 = fun(b) ->
%			{ok, c}
%		end,
%		F3 = fun(c) ->
%			{ok, d}
%		end,
%		?assertEqual({ok, d}, bind(a, [F1, F2, F3]))
%	end},
%
%	{"boom on second", fun() ->
%		F1 = fun(a) ->
%			{ok, b}
%		end,
%		F2 = fun(b) ->
%			{error, b}
%		end,
%		F3 = fun(c) ->
%			{ok, d}
%		end,
%		?assertEqual({error, b}, bind(a, [F1, F2, F3]))
%	end}].
%
%splice_test_() -> [
%	?_assertEqual([1], splice([], 1, 0, [1])),
%	?_assertEqual([], splice([1], 1, 1, [])),
%	?_assertEqual([1,2,3], splice([1,3], 2, 0, [2])),
%	?_assertEqual([1,3], splice([1,2,3], 2, 1, [])),
%	?_assertEqual([1,a,3], splice([1,2,3], 2, 1, [a])),
%	?_assertEqual([1,2,3,4,5,6,7,8,9], splice([1,2,3,7,8,9], 4, 0, [4,5,6]))
%].
%
%snip_test_() -> [
%	?_assertEqual([], snip(1, "a")),
%	?_assertEqual("ac", snip(2, "abc")),
%	?_assertEqual("ab", snip(3, "abc"))
%].
%
%
%-endif.
