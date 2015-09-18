%% @doc To allow for independant query-able ets tables per process, for
%% great glory/fun.
-module(lngs_ets).

-include_lib("stdlib/include/qlc.hrl").

-record(?MODULE, { ets }).

% api
-export([start_link/1, start_link/2, start_link/3, ets/1]).
% rpgb_gen_data
-export([
	get_by_id/3,
	save/2,
	delete/3,
	search/3,
	transaction/2
]).

%% ====================================================================
%% External api
%% ====================================================================

start_link(TableName) ->
	start_link(TableName, [public]).

start_link(TableName, Options) ->
	Tid = ets:new(TableName, Options),
	Rec = #?MODULE{ets = Tid},
	lngs_data:start_link(Rec).

start_link(ProcName, TableName, Options) ->
	Tid = ets:new(TableName, Options),
	Rec = #?MODULE{ets = Tid},
	lngs_data:start_link(ProcName, Rec).
	
ets(Rec) ->
	Rec#?MODULE.ets.

get_by_id(Type, Id, Rec) ->
	%lager:debug("lookup ~p of type ~p", [Id, Type]),
	case ets:lookup(Rec:ets(), {Type, Id}) of
		[] -> {error, notfound};
		[{_Key, O} | _] -> {ok, O}
	end.

save(Rec, Ets) ->
	%lager:debug("Save ~p", [Rec]),
	Type = element(1, Rec),
	Fields = Type:field_names(),
	Rec1 = case lists:member(id, Fields) of
		true ->
			case Rec:id() of
				undefined ->
					Id = update_counter(Type, Ets),
					Rec:id(Id);
				_ ->
					Rec
			end;
		_ ->
			Rec
	end,
	%lager:debug("Counter updated"),
	case ets:insert(Ets:ets(), {{Type, Rec1:id()}, Rec1}) of
		true ->
			%lager:debug("insert successful"),
			{ok, Rec1};
		Err ->
			%lager:error("insertion did not go as planned: ~p", [Err]),
		Err
	end.

delete(Type, Id, Ets) ->
	%lager:debug("delete ~p for type ~p", [Id, Type]),
	case ets:delete(Ets:ets(), {Type, Id}) of
		ok ->
			{ok, 1};
		Err ->
			Err
	end.

search(Type, Params, Ets) ->
	%lager:debug("Seek type ~p with params ~p", [Type, Params]),
	FieldNames = get_field_names(Type),
	BlankTuple1 = ['_' || _ <- FieldNames],
	BlankTuple2 = [Type | BlankTuple1],
	BlankRec = list_to_tuple(BlankTuple2),
	FieldIndexs = indexize(FieldNames, 2),
	Match = build_match(Params, FieldIndexs, BlankRec),
	case ets:match_object(Ets:ets(), {{Type, '_'}, Match}) of
		Objs when is_list(Objs) ->
			Objs1 = [O || {_, O} <- Objs],
			{ok, Objs1};
		E ->
			E
	end.

transaction(Fun, _Ets) ->
	%lager:debug("Proceeding with transaction"),
	Fun().

%% ====================================================================
%% internal
%% ====================================================================

get_field_names(RecName) ->
	RecName:field_names().

indexize(Items, StartIndex) ->
	indexize(Items, StartIndex, []).

indexize([], _Index, Acc) ->
	lists:reverse(Acc);

indexize([Head | Tail], Index, Acc) ->
	indexize(Tail, Index + 1, [{Head, Index} | Acc]).

build_match([], _Indexs, Rec) ->
	Rec;
build_match([{Key, Value} | Tail], Indexes, Rec) ->
	case proplists:get_value(Key, Indexes) of
		undefined ->
			build_match(Tail, Indexes, Rec);
		N ->
			Rec1 = setelement(N, Rec, Value),
			build_match(Tail, Indexes, Rec1)
	end.

%create_default(RecName) ->
%	RecName:new([]).

update_counter(Type, Rec) ->
	Ets = Rec:ets(),
	%lager:debug("updating counter ~p in table ~p", [Type, Ets]),
	try ets:update_counter(Ets, Type, 1) of
		N -> N
	catch
		error:badarg ->
			ets:insert(Ets, {Type, 1}),
			1
	end.

