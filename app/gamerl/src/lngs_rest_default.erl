-module(lngs_rest_default).
% so much badness due to now permission checking. So bad, so very very
% bad.

-export([routes/0]).
-export([init/3, rest_init/2]).
-export([allowed_methods/2, is_authorized/2, forbidden/2, resource_exists/2,
	delete_resource/2, content_types_provided/2, content_types_accepted/2,
	to_html/2, to_json/2, from_json/2]).

-record(state, {
	type,
	record,
	session,
	user
}).

routes() ->
	Records = [ssg_rec_base_ship, ssg_rec_fleet,
		ssg_rec_fleet_ship, ssg_rec_scenario, ssg_rec_weapon],
	lists:map(fun(RecName) ->
		"ssg_rec_" ++ ShortRecName = atom_to_list(RecName),
		BinRecName = list_to_binary(ShortRecName),
		Path = <<"/", BinRecName/binary, "[/:id]">>,
		{Path, RecName}
	end, Records).

init(_Conn, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, RecName) ->
	lager:debug("rest action for ~p", [RecName]),
	{ok, Session, Req1} = ssg_session:get_or_create(Req),
	{Id, Req2} = cowboy_req:binding(id, Req1),
	Rec = case Id of
		undefined ->
			undefined;
		_ ->
			LookupId = try list_to_integer(binary_to_list(Id)) of
				Int -> Int
			catch
				error:badarg -> undefined
			end,
			case ssg_data:t_get_by_id(RecName, LookupId) of
				{ok, GotRec} -> GotRec;
				Else -> Else
			end
	end,
	State = #state{type = RecName, record = Rec, session = Session, user = ssg_session:get_user(Session)},
	{ok, Req2, State}.

allowed_methods(Req, #state{record = undefined} = State) ->
	{[<<"GET">>, <<"HEAD">>, <<"POST">>], Req, State};

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"PUT">>, <<"HEAD">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State) ->
	User = State#state.user,
	{Method, Req1} = cowboy_req:method(Req),
	case User of
		undefined when Method =:= <<"GET">> ->
			{true, Req1, State};
		undefined ->
			{{false, <<"persona">>}, Req1, State};
		_ ->
			{true, Req1, State}
	end.

forbidden(Req, State) ->
	User = State#state.user,
	UserId = case User of
		undefined ->
			undefined;
		_ ->
			User:id()
	end,
	{Method, Req1} = cowboy_req:method(Req),
	OwnerId = case State#state.record of
		undefined ->
			undefined;
		Record ->
			try Record:owner_id() of
				Owner -> Owner
			catch
				_:_ -> noowner
			end
	end,
	case {UserId, OwnerId, Method} of
		{_, _, <<"GET">>} ->
			lager:debug("Allowed due to get"),
			{false, Req1, State};
		{undefined, undefined, <<"POST">>} ->
			lager:info("allwed due to post despite no user"),
			{false, Req1, State};
		{_, undefined, <<"POST">>} ->
			lager:info("alloweed due to no owner, and is post"),
			{false, Req1, State};
		{undefined, _, _} ->
			lager:info("denied due to no user"),
			{true, Req1, State};
		{Owned, Owned, _} ->
			lager:info("allwed due to owner"),
			{false, Req1, State};
		{_Owned, noowner, _} ->
			lager:info("allwed due to never owned"),
			{false, Req1, State};
		{_, _, _} ->
			lager:info("forbiddend due to not not owner"),
			{true, Req1, State}
	end.

resource_exists(Req, State) ->
	{Method, Req1} = cowboy_req:method(Req),
	case {Method, State#state.record} of
		{<<"POST">>, undefined} ->
			{false, Req1, State};
		{_, {error, notfound}} ->
			{false, Req1, State};
		{_, _} ->
			{true, Req1, State}
	end.

delete_resource(Req, #state{record = undefined} = State) ->
	{false, Req, State};

delete_resource(Req, State) ->
	case ssg_data:t_delete(State#state.record) of
		{ok, _} ->
			{true, Req, State};
		{error, Err} ->
			lager:info("Delete failed: ~p", [Err]),
			{false, Req, State}
	end.

content_types_provided(Req, State) ->
	Types = [
		{{<<"application">>, <<"json">>, '*'}, to_json},
		{{<<"text">>, <<"html">>, '*'}, to_html}
	],
	{Types, Req, State}.

content_types_accepted(Req, State) ->
	Types = [
		{{<<"application">>, <<"json">>, '*'}, from_json}
	],
	{Types, Req, State}.

to_html(Req, State) ->
	Priv = code:priv_dir(shipshoot),
	Index = Priv ++ "/www/index.html",
	{ok, Bin} = file:read_file(Index),
	{Bin, Req, State}.

to_json(Req, #state{record = undefined} = State) ->
	{ok, Recs} = ssg_data:t_search(State#state.type, []),
	Json = lists:map(fun(Record) ->
		fix_record_json(Record)
	end, Recs),
	lager:info("Before going through jsx: ~p", [Json]),
	{jsx:to_json(Json), Req, State};

to_json(Req, State) ->
	Json = fix_record_json(State#state.record),
	{jsx:to_json(Json), Req, State}.

from_json(Req, State) ->
	User = State#state.user,
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	MaybeUpdatedRec = case State#state.record of
		undefined ->
			(State#state.type):new([
				{owner_id, User:id()},
				{created, os:timestamp()},
				{updated, os:timestamp()} | Term
			]);
		InitialRec ->
			InitialRec:from_json(Term)
	end,
	case MaybeUpdatedRec of
		{ok, UpdatedRec} ->
			{ok, FinalRec} = ssg_data:t_save(UpdatedRec),
			OutJson = jsx:to_json(fix_record_json(FinalRec)),
			Req2 = cowboy_req:set_resp_body(OutJson, Req1),
			Return = case State#state.record of
				undefined ->
					Location = make_location(FinalRec),
						{true, Location};
					_	->
						true
			end,
			{Return, Req2, State};
		Wut ->
			lager:info("wat: ~p", [Wut]),
			{halt, Req, State}
	end.

make_location(Record) ->
	Type = element(1, Record),
	TypeList = atom_to_list(Type),
	"ssg_rec_" ++ UrlList = TypeList,
	IdPart = try Record:id() of
		N when is_integer(N) ->
			"/" ++ integer_to_list(N);
		N ->
			N
	catch
		_ -> "/"
	end,
	list_to_binary("/" ++ UrlList ++ IdPart).

fix_record_json(Record) ->
	Location = make_location(Record),
	Record:to_json([{url, Location}, owner, callback, callback_args, fun maybe_set_owner/2]).

maybe_set_owner(Json, Record) ->
	Type = element(1, Record),
	case erlang:function_exported(Type, owner_id, 1) of
		false ->
			Json;
		true ->
			OwnerId = Record:owner_id(),
			{ok, Owner} = ssg_data:t_get_by_id(ssg_rec_user, OwnerId),
			[{owner, Owner:email()} | Json]
	end.

