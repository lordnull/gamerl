-module(lngs_http_websocket).
-behavior(cowboy_websocket_handler).

-export([routes/0]).
-export([rest_execute/6]).
-export([rest_send/3, rest_send/4]).
-export([send/2]).
-export([onresponse/5]).
-export([init/3, websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3]).

-record(state, {routes, user, session, battle_id}).

routes() ->
	[{<<"/ws">>, ?MODULE}].

rest_send(Pid, Resource, Method) ->
	rest_send(Pid, Resource, Method, null).

rest_send(Pid, Resource, MethodAtom, Data) ->
	Method = list_to_binary(atom_to_list(MethodAtom)),
	Pid ! {rest_send, Resource, Method, Data},
	ok.

% stubbed out for cowboy's sake
send(_,_) ->
	ok.

onresponse(Status, Headers, <<>>, WSPid, From) ->
	onresponse(Status, Headers, <<"null">>, WSPid, From);
onresponse(Status, _Headers, Data, WsPid, From) when Status >= 200, Status < 300 ->
	lager:debug("The data before conversion: ~p", [Data]),
	Term = jsx:to_term(Data),
	WsPid ! {reply, From, true, Term};
onresponse(303, _Headers, Data, WsPid, From) ->
	lager:debug("The data before conversion: ~p", [Data]),
	Term = jsx:to_term(Data),
	WsPid ! {reply, From, true, Term};
onresponse(Status, _Headers, Data, WsPid, From) ->
	lager:debug("no accepted: ~p, ~p", [Status, Data]),
	WsPid ! {reply, From, false, Data}.

init(_, Req, _Routes) ->
	case ssg_session:get(Req) of
		{ok, _Session} ->
			{upgrade, protocol, cowboy_websocket};
		{error, notfound} ->
			{ok, Req2} = cowboy_req:reply(401, [
				{<<"content-type">>, <<"text/plain">>}
			], "Not Logged in", Req),
			{shutdown, Req2, undefined}
	end.

websocket_init(_, Req, Routes) ->
	BindRes = ssg_util:bind({Req, #state{}}, [
		fun get_session/1,
		fun join_battle/1
	]),
	case BindRes of
		{ok, {Req1, State}} ->
			{ok, Req1, State#state{routes = Routes}};
		{error, Req2} ->
			lager:warning("Could not set up websocket"),
			{shutdown, Req2}
	end.

websocket_handle({text, Msg}, Req, State) ->
	case jsx:to_term(Msg) of
		[{}] ->
			{ok, Req, State};
		BadJson when not is_list(BadJson) ->
			{ok, Req, State};
		Json ->
			Action = proplists:get_value(<<"action">>, Json),
			Resource = proplists:get_value(<<"resource">>, Json),
			Data = proplists:get_value(<<"data">>, Json),
			From = proplists:get_value(<<"reply_with">>, Json),
			dispatch(Req, From, Action, <<"/", Resource/binary>>, Data, State, self())
	end;
websocket_handle(_Msg, Req, State) ->
	{ok, Req, State}.

websocket_info({reply, From, Accepted, Data}, Req, State) ->
	Resource = iolist_to_binary(io_lib:format("reply/~p", [From])),
	lager:debug("doing a reply with data ~p", [Data]),
	Term = [
		{<<"resource">>, Resource},
		{<<"action">>, <<"REPLY">>},
		{<<"accepted">>, Accepted},
		{<<"data">>, deatomize(Data)}
	],
	{reply, {text, jsx:to_json(Term)}, Req, State};

websocket_info({rest_send, Resource, Method, Data}, Req, State) ->
	Term = [
		{<<"resource">>, iolist_to_binary(Resource)},
		{<<"action">>, deatomize(Method)},
		{<<"data">>, deatomize(Data)}
	],
	{reply, {text, jsx:to_json(Term)}, Req, State};

websocket_info(Info, Req, State) ->
	lager:debug("Got info: ~p", [Info]),
	{ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
	lager:debug("going down due to ~p", [Reason]),
	ok.

deatomize(Fine) when Fine; not Fine; Fine =:= null ->
	Fine;

deatomize(Atom) when is_atom(Atom) ->
	atom_to_binary(Atom, latin1);

deatomize({Key, Value}) ->
	{deatomize(Key), deatomize(Value)};

deatomize(List) when is_list(List) ->
	lists:map(fun(E) ->
		deatomize(E)
	end, List);

deatomize(E) ->
	E.

get_session({Req, State}) ->
	case ssg_session:get(Req) of
		{ok, Session} ->
			case ssg_session:get_user(Session) of
				undefined ->
					{ok, Req2} = cowboy_req:reply(401, [{<<"content-type">>, <<"text/plain">>}], "Not Logged in", Req),
					{error, Req2};
				User ->
					{ok, {Req, State#state{user = User, session = Session}}}
			end;
		_ ->
			{ok, Req2} = cowboy_req:reply(401, [{<<"content-type">>, <<"text/plain">>}], "Not Logged in", Req),
			{error, Req2}
	end.

join_battle({Req, State}) ->
	{Id, Req2} = cowboy_req:binding(battle_id, Req),
	User = State#state.user,
	case ssg_btl_player:join(Id, User:email()) of
		ok ->
			{ok, {Req2, State#state{battle_id = Id}}};
		Else ->
			lager:info("~p could not join battle ~p due to ~p", [User:email(), Id, Else]),
			Req3 = cowboy_req:reply(400, [{<<"content-type">>, <<"text/plain">>}], io_lib:format("~p", [Else]), Req2),
			{error, Req3}
	end.

dispatch(Req, From, Action, Resource, Data, State, Self) ->
	Routes = State#state.routes,
	FakeReq = fake_cowboy_req(Req, Action, From, Data, Self, Resource),
	case cowboy_router:execute(FakeReq, [{dispatch, Routes}]) of
		{ok, Req2, [{handler, Handler}, {handler_opts, HandlerOpts}|Env]} ->
			spawn(?MODULE, rest_execute, [Req2, From, Self, Handler, HandlerOpts, Env]),
			{ok, Req, State};
		{error, Code, ReplyReq} ->
			maybe_reply(From, Code, ReplyReq, Req, State)
	end.

fake_cowboy_req(OriginalReq, Method, From, Data, WsPid, Resource) ->
	{OriginalHeaders, _OReq1} = cowboy_req:headers(OriginalReq),
	lager:debug("Data: ~p", [Data]),
	Encoded = case Data of
		undefined ->
			<<>>;
		_ ->
			jsx:to_json(Data)
	end,
	Size = byte_size(Encoded),
	Headers = [
		{<<"content-length">>, list_to_binary(integer_to_list(Size))},
		{<<"content-type">>, <<"application/json">>}
		| OriginalHeaders],
	ReqArgs = [
		{?MODULE, WsPid}, % socket
		?MODULE, % transport
		undefined, % peer
		Method, % method
		Resource, % path
		undefined, % query
		'HTTP/1.1', % version
		Headers, % headers
		undefined, % host
		undefined, % port
		Encoded, % buffer
		false, % can keep alive
		undefined, % compress
		make_reply_fun(WsPid, From) % on response
	],
	erlang:apply(cowboy_req, new, ReqArgs).

make_reply_fun(WsPid, From) ->
	lager:debug("created reply fun"),
	fun(Status, Headers, Data, Req) ->
		lager:debug("onrepsonse is happening!"),
		onresponse(Status, Headers, Data, WsPid, From),
		Req
	end.

maybe_reply(From, Code, ReplyReq, Req, State) ->
	lager:debug("maybe reply: ~p, ~p, ~p", [From, Code, ReplyReq]),
	Reply = [
		{<<"action">>, <<"reply">>},
		{<<"resource">>, iolist_to_binary(io_lib:format("reply/~p", [From]))},
		{<<"accepted">>, false}
	],
	Io = jsx:to_json(Reply),
	{reply, {text, Io}, Req, State}.

rest_execute(Req, _From, _WSPid, Handler, HandlerOpts, Env) ->
	_Got = cowboy_rest:upgrade(Req, Env, Handler, HandlerOpts).

