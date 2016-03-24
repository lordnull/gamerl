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
	Proto = application:get_env(shipshoot, protocol, http),
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
