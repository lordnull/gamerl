-module(lngs_rec_user_auth).

-compile([{parse_transform, rec2json}]).
-define(DIGEST_FUNC, sha256).
-define(STORAGE_INTERATIONS, 1000).
-define(RESET_TOKEN_LIFESPAN_SEC, 1200).
-define(CLEAR_EXPIRED_TOKENS_INTERVAL_SEC, 300000).

-record(?MODULE, {
	id, % this should be a 'foreign' key to the lngs_rec_user that this info is for
	username,
	hashed_password,
	salt,
	interations,
	reset_token,
	reset_expiration
}).

-export([
	verify/2,
	set_password/2,
	create_reset_token/1,
	expire_reset_token/1,
	start_expire_server/0
]).

verify(Username, Password) when is_binary(Username) ->
	verify(lngs_data:t_search(?MODULE, [{username, Username}]), Password);

verify({ok, [UserRec]}, Password) ->
	#lngs_rec_user_auth{hashed_password = Hash, salt = Salt, interations = Interations} = UserRec,
	case pbkdf2:pbkdf2(?DIGEST_FUNC, Password, Salt, Interations) of
		Hash when Interations < ?STORAGE_INTERATIONS ->
			_ = set_password(UserRec, Password),
			lngs_data:t_get_by_id(lngs_rec_user, UserRec:id());
		Hash ->
			lngs_data:t_get_by_id(lngs_rec_user, UserRec:id());
		_ ->
			{error, verification_failed}
	end;

verify(_, _) ->
	{error, verification_failed}.

set_password(Username, Password) when is_binary(Username) ->
	case lngs_data:t_search(lngs_rec_user, [{username, Username}]) of
		{ok, []} ->
			{error, not_found};
		{ok, [UserRec]} ->
			set_password(UserRec, Password)
	end;

set_password(UserRec, Password) ->
	Salt = crypto:rand_bytes(8),
	Hash = pbkdf2:pbkdf2(?DIGEST_FUNC, Password, Salt, ?STORAGE_INTERATIONS),
	Record = #lngs_rec_user_auth{
		id = UserRec:id(),
		username = UserRec:email(),
		hashed_password = Hash,
		salt = Salt,
		interations = ?STORAGE_INTERATIONS
	},
	lngs_data:t_save(Record).

create_reset_token(UserRec) ->
	{ok, AuthRec} = case lngs_data:t_get_by_id(?MODULE, UserRec:id()) of
		{error, not_found} ->
			RandomPw = uuid:uuid4(),
			set_password(UserRec, list_to_binary(uuid:to_string(RandomPw)));
		Else ->
			Else
	end,
	Token = list_to_binary(uuid:to_string(uuid:uuid4())),
	ExpirationTime = erlang:system_time(seconds) + ?RESET_TOKEN_LIFESPAN_SEC,
	AuthRec2 = AuthRec#lngs_rec_user_auth{reset_token = Token, reset_expiration = ExpirationTime},
	lngs_data:t_save(AuthRec2).

expire_reset_token(AuthRec) ->
	AuthRec2 = AuthRec#lngs_rec_user_auth{reset_token = undefined, reset_expiration = undefined},
	lngs_data:t_save(AuthRec2).

start_expire_server() ->
	spawn_link(fun expire_loop/0).

expire_loop() ->
	_ = erlang:send_after(?CLEAR_EXPIRED_TOKENS_INTERVAL_SEC, self(), clear_dead_tokens),
	receive
		clear_dead_tokens ->
			Now = erlang:system_time(seconds),
			{ok, Expired} = lngs_data:t_search(?MODULE, [{reset_expiration, '=<', Now}]),
			ok = lists:foreach(fun expire_reset_token/1, Expired),
			expire_loop()
	end.

	