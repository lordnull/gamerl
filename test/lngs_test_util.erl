-module(lngs_test_util).

-export([do_exits/1, do_exit/1]).
-export([maybe_start_lager/0, maybe_stop_lager/0, maybe_stop_lager/1]).

maybe_start_lager() ->
	maybe_start_lager(os:getenv("LAGER")).

maybe_start_lager(false) ->
	[];

maybe_start_lager(_) ->
	Out = application:ensure_all_started(lager),
	_ = lager:set_loglevel(lager_console_backend, debug),
	Out.

maybe_stop_lager() ->
	maybe_stop_lager(os:getenv("LAGER")).

maybe_stop_lager(false) ->
	ok;

maybe_stop_lager([]) ->
	ok;

maybe_stop_lager([Atom | _]) when is_atom(Atom) ->
	lists:foreach(fun(App) ->
		application:stop(App)
	end, lists:reverse(Atom));

maybe_stop_lager(_) ->
	application:stop(lager).

do_exits(Pids) ->
	lists:foreach(fun do_exit/1, Pids).

do_exit(undefined) ->
	ok;

do_exit(Atom) when is_atom(Atom) ->
	do_exit(whereis(Atom));

do_exit(Pid) when is_pid(Pid) ->
	lager:info("Exiting ~p", [Pid]),
	unlink(Pid),
	_ = monitor(process, Pid),
	exit(Pid, shutdown),
	receive
		{'DOWN', _, process, Pid, _} -> ok
	end;

do_exit({via, Module, Name}) ->
	do_exit(Module:whereis_name(Name)).

