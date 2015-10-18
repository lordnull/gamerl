-module(lngs_rec_user).

-compile([{parse_transform, rec2json}]).

-record(lngs_rec_user, {
	id,
	email :: string()
}).

-export([new/1, get_maybe_created/1]).

new(Props) ->
	?MODULE:from_json(Props).

get_maybe_created(Email) ->
	{ok, Re} = re:compile(".+@.+"),
	case re:run(Email, Re) of
		nomatch ->
			{error, not_email};
		_ ->
			case lngs_data:search(?MODULE, [{email, Email}]) of
				{ok, []} ->
					User = #?MODULE{email = Email},
					lngs_data:save(User);
				{ok, [User | _]} ->
					{ok, User}
			end
	end.
