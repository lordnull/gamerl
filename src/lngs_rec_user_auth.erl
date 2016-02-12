-module(lngs_rec_user_auth).

-compile([{parse_transform, rec2json}]).
-define(DIGEST_FUNC, sha256).
-define(STORAGE_INTERATIONS, 1000).

-record(?MODULE, {
	id, % this should be a 'foreign' key to the lngs_rec_user that this info is for
	username,
	hashed_password,
	salt,
	interations
}).

-export([
	verify/2,
	set_password/2
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
