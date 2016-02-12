-module(lngs_http_auth).

-export([routes/0]).
-export([init/2]).
-export([
	allowed_methods/2, is_authorized/2,
	content_types_provided/2, content_types_accepted/2,
	from_json/2, to_js/2
]).

routes() -> [<<"/js/conf.js">>, <<"/auth/login">>, <<"/auth/logout">>].

-record(ctx, {
	session,
	action
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
		<<"/js/conf.js">> ->
			conf;
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
is_authorized(Req, #ctx{action = undefined} = Ctx) ->
	case cowboy_req:method(Req) of
		{<<"POST">>, Req1} ->
			lager:debug("authorized"),
			{true, Req1, Ctx};
		{_, Req1} ->
			lager:debug("not authorized, post to me"),
			{{false, <<"post">>}, Req1, Ctx}
	end.

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
	BaseURL = lngs_util:make_url(),
	{ok, Post, Req1} = cowboy_req:body(Req),
	Json = jsx:decode(Post),
	lager:debug("Json term:  ~p", [Json]),
	Username = proplists:get_value(<<"username">>, Json),
	Password = proplists:get_value(<<"password">>, Json),
	case lngs_rec_user_auth:verify(Username, Password) of
		{error, Wut} ->
			{false, Req1, Ctx};
		{ok, UserRec} ->
			{ok, Session1} = lngs_session:set_user(UserRec, Session),
			Req2 = cowboy_req:set_resp_body(Username, Req1),
			{true, Req2, Ctx#ctx{session = Session1}}
	end.
