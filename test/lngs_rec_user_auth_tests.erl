-module(lngs_rec_user_auth_tests).

-include_lib("eunit/include/eunit.hrl").
-define(DATA_FILE, "data").

use_test_() ->
	{setup, fun() ->
		{ok, Pid} = lngs_ets:start_link(lngs_data, ?MODULE, [public]),
		Pid
	end,
	fun(Pid) ->
		_ = unlink(Pid),
		_ = exit(Pid, kill),
		Mon = erlang:monitor(process, Pid),
		receive
			{'DOWN', Mon, process, Pid, _} ->
				ok
		end
	end,
	fun(_) -> [

		{"able to set a password for a user", fun() ->
			{ok, User} = lngs_rec_user:get_maybe_created(<<"user@exmaple.com">>),
			{Ok, Result} = lngs_rec_user_auth:set_password(User, <<"goliath">>),
			?assertEqual(ok, Ok),
			?assertEqual(User:id(), Result:id())
		end},

		{"able to verify a correct password", fun() ->
			{Ok, Result} = lngs_rec_user_auth:verify(<<"user@exmaple.com">>, <<"goliath">>),
			?assertEqual(ok, Ok),
			?assertEqual(<<"user@exmaple.com">>, Result:email())
		end},

		{"able to verify an incorrect password", fun() ->
			{Error, Wut} = lngs_rec_user_auth:verify(<<"user@exmaple.com">>, <<"pants">>),
			?assertEqual(error, Error)
		end},

		{"able to deny access to a non-existant user", fun() ->
			{Error, Wut} = lngs_rec_user_auth:verify(<<"not_a_user@example.com">>, <<"does-not-matter">>),
			?assertEqual(error, Error)
		end}

	] end}.
