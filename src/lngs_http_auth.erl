-module(lngs_http_auth).

-export([routes/0]).
-export([init/2]).
-export([
	allowed_methods/2, is_authorized/2,
	content_types_provided/2, content_types_accepted/2,
	resource_exists/2, allow_missing_post/2,
	from_json/2, to_js/2
]).

routes() ->
	[ <<"/js/conf.js">>
	, <<"/auth/login">>
	, <<"/auth/logout">>
	, <<"/auth/pwr/:reset_token">>
	, <<"/auth/">>
	, <<"/auth/signup">>
	].

-record(ctx, {
	session,
	action,
	auth_rec
}).

init(Req, _Opts) ->
	{ok, Session, Req1} = lngs_session:get_or_create(Req),
	Path = cowboy_req:path(Req1),
	lager:debug("path: ~p", [Path]),
	Action = case Path of
		<<"/auth/login">> ->
			login;
		<<"/auth/logout">> ->
			logout;
		<<"/auth/pwr/:reset_token">> ->
			reset;
		<<"/js/conf.js">> ->
			conf;
		<<"/auth/signup">> ->
			signup;
		<<"/auth">> ->
			set_password;
		_ ->
			undefined
	end,
	Ctx = #ctx{session = Session, action = Action},
	{cowboy_rest, Req1, Ctx}.

allowed_methods(Req, #ctx{action = conf} = Ctx) ->
	{[<<"GET">>, <<"HEAD">>], Req, Ctx};
allowed_methods(Req, Ctx) ->
	{[<<"GET">>, <<"POST">>, <<"HEAD">>], Req, Ctx}.

is_authorized(Req, #ctx{action = conf} = Ctx) ->
	{true, Req, Ctx};
is_authorized(Req, #ctx{action = Action} = Ctx) when Action =:= login; Action =:= logout ->
	{true, Req, Ctx};
is_authorized(Req, #ctx{action = reset} = Ctx) ->
	{true, Req, Ctx};
is_authorized(Req, #ctx{action = set_password} = Ctx) ->
	Session = Ctx#ctx.session,
	case lngs_session:get_user(Session) of
		undefined ->
			{{false, <<"post">>}, Req, Ctx};
		_User ->
			{true, Req, Ctx}
	end;
is_authorized(Req, #ctx{action = undefined} = Ctx) ->
	case cowboy_req:method(Req) of
		{<<"POST">>, Req1} ->
			lager:debug("authorized"),
			{true, Req1, Ctx};
		{_, Req1} ->
			lager:debug("not authorized, post to me"),
			{{false, <<"post">>}, Req1, Ctx}
	end.

resource_exists(Req, #ctx{action = reset} = Ctx) ->
	{Token, Req1} = cowboy_req:binding(reset_token, Req),
	case Token of
		undefined ->
			{false, Req1, Ctx};
		_ ->
			case lngs_data:t_search(lngs_rec_user_auth, [{reset_token, Token}]) of
				{ok, [AuthRec]} ->
					{true, Req1, Ctx#ctx{auth_rec = AuthRec}};
				_ ->
					{false, Req1, Ctx}
			end
	end;

resource_exists(Req, Ctx) ->
	{true, Req, Ctx}.

allow_missing_post(Req, Ctx) ->
	{false, Req, Ctx}.

content_types_provided(Req, #ctx{action = conf} = Ctx) ->
	Types = [{{<<"application">>, <<"x-javascript">>, '*'}, to_js}],
	{Types, Req, Ctx};
content_types_provided(Req, Ctx) ->
	lager:debug("content types provided, method: ~p", [element(1, cowboy_req:method(Req))]),
	Types = [
		{{<<"text">>, <<"html">>, '*'}, to_html},
		{{<<"text">>, <<"json">>, '*'}, to_json}
	],
	{Types, Req, Ctx}.

content_types_accepted(Req, Ctx) ->
	lager:debug("content types accepted; got ~p", [element(1, cowboy_req:header(<<"content-type">>, Req))]),
	Types = [
		{{<<"application">>, <<"json">>, '*'}, from_json}
	],
	{Types, Req, Ctx}.

to_js(Req, #ctx{action = conf} = Ctx) ->
	{ok, Session, Req1} = lngs_session:get_or_create(Req),
	UserName = case lngs_session:get_user(Session) of
		undefined -> <<"null">>;
		UserRec -> <<"'", (UserRec:email())/binary, "'">>
	end,
	LoginUrl = lngs_util:make_url(["auth", "login"]),
	LogoutUrl = lngs_util:make_url(["auth", "logout"]),
	JsBase =
		"define({~n"
		"    'currentUser': ~s,~n"
		"    'loginUrl': '~s',~n"
		"    'logoutUrl': '~s'~n"
		"});~n",
	Js = io_lib:format(JsBase, [UserName, LoginUrl, LogoutUrl]),
	{Js, Req1, Ctx}.

from_json(Req, #ctx{action = logout} = Ctx) ->
	lager:debug("processing logout"),
	Req1 = lngs_session:destroy(Req),
	{true, Req1, Ctx};

from_json(Req, #ctx{session = Session, action = login} = Ctx) ->
	lager:debug("processing login"),
	{ok, Post, Req1} = cowboy_req:body(Req),
	Json = jsx:decode(Post),
	lager:debug("Json term:  ~p", [Json]),
	Username = proplists:get_value(<<"username">>, Json),
	Password = proplists:get_value(<<"password">>, Json),
	case lngs_rec_user_auth:verify(Username, Password) of
		{error, _Wut} ->
			{false, Req1, Ctx};
		{ok, UserRec} ->
			{ok, Session1} = lngs_session:set_user(UserRec, Session),
			Req2 = cowboy_req:set_resp_body(Username, Req1),
			{true, Req2, Ctx#ctx{session = Session1}}
	end;

from_json(Req, #ctx{action = reset, auth_rec = AuthRec} = Ctx) ->
	{ok, Post, Req1} = cowboy_req:body(Req),
	Json = jsx:decode(Post),
	Password = proplists:get_value(<<"password">>, Json),
	{ok, UserRec} = lngs_data:t_get_by_id(lngs_rec_user, AuthRec:id()),
	{ok, _} = lngs_rec_user_auth:set_password(UserRec, Password),
	{true, Req1, Ctx};

from_json(Req, #ctx{action = set_password} = Ctx) ->
	{ok, Post, Req1} = cowboy_req:body(Req),
	Json = jsx:decode(Post),
	Session = Ctx#ctx.session,
	User = lngs_session:get_user(Session),
	Username = User:email(),
	case proplists:get_value(<<"password">>) of
		undefined ->
			{false, Req1, Ctx};
		Password ->
			_ = lngs_rec_user_auth:set_password(User, Password),
			{true, Req1, Ctx}
	end;

from_json(Req, #ctx{action = signup} = Ctx) ->
	{ok, Post, Req1} = cowboy_req:body(Req),
	Json = jsx:decode(Post),
	case proplists:get_value(<<"username">>, Json) of
		undefined ->
			lager:info("Could not do signup since no username was given"),
			{false, Req1, Ctx};
		Username ->
			case proplists:get_value(<<"password">>) of
				undefined ->
					{false, Req1, Ctx};
				Password ->
					case lngs_data:t_search(lngs_rec_user, [{email, Username}]) of
						{ok, []} ->
							case lngs_rec_user:get_maybe_created(Username) of
								{ok, UserRec} ->
									_ = lngs_rec_user_auth:set_password(UserRec, Password),
									{true, Req1, Ctx};
								CreatedErr ->
									lager:info("could not create user: ~p", [CreatedErr]),
									{false, Req1, Ctx}
							end;
						_UserExistsErr ->
							lager:info("could not do signup since user exists"),
							{false, Req1, Ctx}
					end
			end
	end.
