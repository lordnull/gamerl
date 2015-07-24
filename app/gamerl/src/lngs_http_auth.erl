-module(lngs_http_auth).

-export([routes/0]).
-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	content_types_provided/2, content_types_accepted/2, from_json/2, to_js/2]).

routes() -> [<<"/js/conf.js">>, <<"/auth/login">>, <<"/auth.logout">>].

-record(ctx, {
	session,
	action
}).

init(_Protos, _Req, _HostPort) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
	{ok, Session, Req1} = ssg_session:get_or_create(Req),
	{Path, Req2} = cowboy_req:path(Req1),
	lager:debug("path:  ~p", [Path]),
	Action= case Path of
		<<"/auth/login">> ->
			login;
		<<"/auth/logout">> ->
			logout;
		<<"/js/conf.js">> ->
			conf;
		_ ->
			undefined
	end,
	{ok, Req2, #ctx{session = Session, action = Action}}.

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
			{{false, <<"persona">>}, Req1, Ctx}
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
	{ok, Session, Req1} = ssg_session:get_or_create(Req),
	UserName = case ssg_session:get_user(Session) of
		undefined -> <<"null">>;
		UserRec -> <<"'", (UserRec:email())/binary, "'">>
	end,
	LoginUrl = ssg_util:make_url(["auth", "login"]),
	LogoutUrl = ssg_util:make_url(["auth", "logout"]),
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
	Req1 = ssg_session:destroy(Req),
	{true, Req1, Ctx};

from_json(Req, #ctx{session = Session, action = login} = Ctx) ->
	lager:debug("processing login"),
	BaseURL = ssg_util:make_url(),
	{ok, Post, Req1} = cowboy_req:body(Req),
	Json = jsx:to_term(Post),
	lager:debug("Json term:  ~p", [Json]),
	Assertion = proplists:get_value(<<"assertion">>, Json),
	AssertBody = jsx:to_json([{<<"assertion">>, Assertion},
		{<<"audience">>, BaseURL}]),
	Asserted = ibrowse:send_req("https://verifier.login.persona.org/verify",
		[{"Content-Type", "application/json"}], post, AssertBody),
	case Asserted of
		{ok, "200", _Heads, AssertedBody} ->
			AssertedJson = jsx:to_term(list_to_binary(AssertedBody)),
			Email = proplists:get_value(<<"email">>, AssertedJson),
			case proplists:get_value(<<"status">>, AssertedJson) of
				<<"okay">> ->
					{ok, Session1} = case ssg_data:t_search(ssg_rec_user, [{email, Email}]) of
						{ok, []} ->
							lager:info("Creating new user ~p", [Email]),
							{ok, Userrec} = ssg_rec_user:new([{email, Email}]),
							{ok, Userrec1} = ssg_data:t_save(Userrec),
							ssg_session:set_user(Userrec1, Session);
						{ok, Userrecs} ->
							[Userrec | Destroy] = lists:keysort(2, Userrecs),
							spawn(fun() ->
								[ssg_data:t_delete(R) || R <- Destroy]
							end),
							lager:debug("existant user ~p", [Userrec]),
							ssg_session:set_user(Userrec, Session)
					end,
					Req2 = cowboy_req:set_resp_body(Email, Req1),
					{true, Req2, Ctx#ctx{session = Session1}};
				NotOkay ->
					lager:notice("data returned invalid: ~p", [NotOkay]),
					{false, Req1, Ctx}
			end;
		_ ->
			lager:info("Verifier failed:  ~p", [Asserted]),
			{false, Req1, Ctx}
	end.

