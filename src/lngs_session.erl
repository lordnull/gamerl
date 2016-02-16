-module(lngs_session).

-behavior(gen_server).

-define(COOKIE_NAME, <<"lngsid">>).

-include_lib("stdlib/include/qlc.hrl").
-type request_data() :: any().
-type session_id() :: binary().
-type user_data() :: [{binary(),binary()}].
-type timestamp() :: {pos_integer(),pos_integer(),pos_integer()}.
-type session() ::
	{session_id(), undefined | user_data(), dict:dict(), timestamp()}.
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).
% api
-export([start_link/0, start_link/1, get_or_create/1, get/1, create/0,
	destroy/1, get_id/1, get_user/1, get_value/2, get_value/3, set_user/2,
	to_dict/1, make_ets/0]).

%% =================================================================
%% Api
%% =================================================================

%% @doc Start the session server.
-spec start_link() -> {'ok', pid()}.
start_link() -> start_link([]).

%% @doc There are currently no actual options, so just starts the session
%% server.  The session is ets backed.
-spec start_link(Opts :: [any()]) -> {'ok', pid()}.
start_link(Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Based on the id or webmachine request data, return a session
%% always.  There is no indication given if the session is new or old.
%% If just the session id is passed in, `{ok, Session}' is returned,
%% otherwise `{ok, Sessin, ReqData}' is returned.  The new ReqData will
%% have appropriate headers set if needed.
-spec get_or_create (SessionRef :: string() | binary() | request_data()) ->
	{'ok', session()} | {'ok', session(), request_data()}.
get_or_create(Id) when is_list(Id) ->
	get_or_create(list_to_binary(Id));

get_or_create(Id) when is_binary(Id) ->
	case ?MODULE:get(Id) of
		{error, notfound} -> create();
		E -> E
	end;

get_or_create(ReqData) ->
	lager:debug("req data version of get_or_create", []),
	{Newness, {ok, Session}} = case ?MODULE:get(ReqData) of
		{error, notfound} -> {new, create()};
		E -> {old, E}
	end,
	case Newness of
		old ->
			lager:debug("session already exists:  ~p", [get_id(Session)]),
			lager:debug("session:  ~p", [Session]),
			{ok, Session, ReqData};
		new ->
			SessionId1 = get_id(Session),
			lager:debug("session was created:  ~p", [SessionId1]),
			ReqData1 = set_cookie(SessionId1, ReqData),
			{ok, Session, ReqData1}
	end.

set_cookie(SessionId, Req) ->
	cowboy_req:set_resp_cookie(?COOKIE_NAME, SessionId, [
		{max_age, 60 * 60 *24 * 7},
		{path, <<"/">>}
	], Req).

%% @doc Get session data if available based on id or `request_data()'.
-spec get/1 :: (SessionReference :: string() | binary() | request_data()) ->
	{'error','notfound'} | {'ok', session()}.
get(undefined) ->
	{error, notfound};

get(Id) when is_list(Id) ->
	?MODULE:get(list_to_binary(Id));

get(Id) when is_binary(Id) ->
	QH = qlc:q([X || {SessId, _, _, _} = X <- ets:table(?MODULE),
		SessId =:= Id]),
	case qlc:e(QH) of
		[] -> {error, notfound};
		[Session] ->
			ets:update_element(?MODULE, Id, {4, calendar:local_time()}),
			{ok, Session}
	end;

get(Req) ->
	Cookies = cowboy_req:parse_cookies(Req),
	MaybeValue = proplists:get_value(?COOKIE_NAME, Cookies),
	?MODULE:get(MaybeValue).

%% @doc Creates a new session in the ets table and returns it.  While the
%% uuid's for the session are randomly generated, there is still a check
%% to ensure they are unique.
-spec create() -> {'ok', session()}.
create() ->
	Uuid = make_uuid(),
	Session = {Uuid, undefined, dict:new(), calendar:local_time()},
	case ets:insert_new(?MODULE, Session) of
		false ->
			create();
		true ->
			{ok, Session}
	end.

%% @doc Remove a session from the ets.
-spec destroy (Id :: any()) -> 'ok'.
destroy(Id) when is_binary(Id) ->
	ets:delete(?MODULE, Id);
destroy(Id) when is_list(Id) ->
	ets:delete(list_to_binary(Id));
destroy(Req) ->
	Cookies = cowboy_req:parse_cookies(Req),
	case proplists:get_value(?COOKIE_NAME, Cookies) of
		undefined ->
			ok;
		Id ->
			?MODULE:destroy(Id)
	end,
	set_cookie(<<>>, Req).

%% @doc Extracts the id from a `session()'.
-spec get_id(Session :: session()) -> binary().
get_id({Id, _, _, _}) -> Id.

%% @doc Extracts the user proplist from a `session()'.
-spec get_user(Session :: session()) -> user_data().
get_user({_, User, _, _}) -> User.

%% @doc Sets the user of a session, overwriting an existing one.
-spec set_user(User :: user_data(), Session :: session()) ->
	{'ok', session()}.
set_user(User, {Id, undefined, Values, _}) ->
	Session = {Id, User, Values, calendar:local_time()},
	ets:insert(?MODULE, Session),
	{ok, Session};

set_user(User, {_Id, User, _, _} = S) ->
	{ok, S};

set_user(User, {Id, _OldUser, _OldValues, _TimeStarted}) ->
	Session = {Id, User, dict:new(), calendar:local_time()},
	ets:insert(?MODULE, Session),
	{ok, Session}.

%% @doc Extracts a value from the dictionary associated with the session.
-spec get_value(Key :: any(), Session :: session()) ->
	'error' | {'ok', any()}.
get_value(Key, {_, _, Dict, _}) ->
	dict:find(Key, Dict).

%% @doc Extracts a value from the dictionary associated with the session.
%% If the given key has no value, the default is returned.
-spec get_value (Key :: any(), Session :: session(), Default :: any()) ->
	{'ok', any()}.
get_value(Key, {_, _, Dict, _}, Default) ->
	case dict:find(Key, Dict) of
		error -> {ok, Default};
		E -> E
	end.

%% @doc Turns the session tuple into a dictionary.  Also accepts undefined,
%% meaning a new dictionry is returned.
-spec to_dict/1 :: (Session :: 'undefined' | session()) -> dict:dict().
to_dict(undefined) ->
	dict:new();

to_dict({_Id, User, Values, TimeStarted}) ->
	dict:from_list([
		{user, User},
		{timestarted, TimeStarted},
		{values, Values}
	]).

make_ets() ->
	case ets:info(?MODULE) of
		undefined ->
			ets:new(?MODULE, [named_table, public]);
		_ ->
			ets:delete_all_objects(?MODULE)
	end.

%% =================================================================
%% Init
%% =================================================================

%% @hidden
init(_Opts) ->
	Ets = make_ets(),
	Self = self(),
	Timer = erlang:send_after(1000 * 60 * 60, Self, clear_dead_sessions),
	{ok, {Timer, Ets}}.

%% =================================================================
%% handle_call
%% =================================================================

%% @hidden
handle_call(_Msg, _From, State) ->
	{reply, {error, unhandled}, State}.

%% =================================================================
%% handle_cast
%% =================================================================

%% @hidden
handle_cast(_Msg, State) ->
	{noreply, State}.

%% =================================================================
%% handle_info
%% =================================================================

%% @hidden
handle_info(clear_dead_sessions, {_, Ets}) ->
	Now = calendar:local_time(),
	NowSecs = calendar:datetime_to_gregorian_seconds(Now),
	QH = qlc:q([Id || {Id, _, _, LastTouch} <- ets:table(Ets),
		too_old(LastTouch, NowSecs)]),
	Ids = qlc:e(QH),
	[ets:delete(Ets, I) || I <- Ids],
	Self = self(),
	Timer = erlang:send_after(1000 * 60 * 60, Self, clear_dead_sessions),
	{noreply, {Timer, Ets}};
	
handle_info(_Msg, State) ->
	{noreply, State}.

too_old(TestDate, Now) ->
	TestSecs = calendar:datetime_to_gregorian_seconds(TestDate),
	ThreeHours = 1000 * 60 * 60 * 24 * 8,
	TimeDiff = Now - TestSecs,
	if
		TimeDiff > ThreeHours -> true;
		true -> false
	end.
	
%% =================================================================
%% terminate
%% =================================================================

%% @hidden
terminate(_Why, _State) -> ok.

%% =================================================================
%% code_change
%% =================================================================

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% =================================================================
%% Internal Functions
%% =================================================================

make_uuid() ->
	make_uuid(crypto:rand_bytes(16)).

make_uuid(Bin) when is_binary(Bin) ->
	Hexstr = lngs_util:bin_to_hexstr(Bin),
	Hexbin = list_to_binary(Hexstr),
	<<Eight:8/binary, Four1:4/binary, _:1/binary, Four2:3/binary, _:1/binary, Four3:3/binary, End/binary>> = Hexbin,
	<<Eight/binary, $-, Four1/binary, $-, $4, Four2/binary, $-, $a, Four3/binary, $-, End/binary>>.
	
%% =================================================================
%% Tests
%% =================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

make_uuid_test() ->
	Bin = <<33,190,233,81,19,2,56,20,150,182,80,133,173,234,131,6>>,
	Expected = <<"21bee951-1302-4814-a6b6-5085adea8306">>,
	?assertEqual(Expected, make_uuid(Bin)).

-endif.
