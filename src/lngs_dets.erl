-module(lngs_dets).

-behavior(gen_server).

-include_lib("stdlib/include/qlc.hrl").

-define(dets_table, lngs_dets).

% api
-export([start_link/0, start_link/1, stop/0]).
-export([update_records/0]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).
-export([
  get_by_id/2,
  save/1,
  delete/2,
  search/2,
	transaction/1
]).

%% ====================================================================
%% External api
%% ====================================================================

start_link() ->
  start_link([]).

start_link(Options) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

stop() ->
   gen_server:cast(?MODULE, stop).

get_by_id(Type, Id) ->
  case dets:lookup(?dets_table, {Type, Id}) of
    [] -> {error, notfound};
    [{_Key, O} | _] -> {ok, O}
  end.

save(Rec) ->
  Type = element(1, Rec),
	Fields = Type:field_names(),
	Rec1 = case lists:member(id, Fields) of
		true ->
			case Rec:id() of
				undefined ->
					Id = update_counter(Type),
					Rec:id(Id);
				_ ->
					Rec
			end;
		_ ->
			Rec
	end,
  case dets:insert(?dets_table, {{Type, Rec1:id()}, Rec1}) of
    ok ->
      {ok, Rec1};
    Err ->
      Err
  end.

delete(Type, Id) ->
  case dets:delete(?dets_table, {Type, Id}) of
    ok ->
      {ok, 1};
    Err ->
      Err
  end.

search(Type, Params) ->
  FieldNames = get_field_names(Type),
  FieldIndexs = indexize(FieldNames, 2),
  BindAtoms = lists:map(fun({_, N}) ->
    list_to_atom("$" ++ integer_to_list(N))
  end, FieldIndexs),
  BlankTuple1 = [A || A <- BindAtoms],
  BlankTuple2 = [Type | BlankTuple1],
  BlankRec = list_to_tuple(BlankTuple2),
  BoundAtoms = lists:map(fun({_, N}) ->
    {N, list_to_atom("$" ++ integer_to_list(N))}
  end, FieldIndexs),
  Match = build_match(Params, FieldIndexs, BoundAtoms),
  case dets:select(?dets_table, [{{{Type, '_'}, BlankRec}, Match, ['$_']}]) of
    Objs when is_list(Objs) ->
      Objs1 = [O || {_, O} <- Objs],
      {ok, Objs1};
    E ->
      E
  end.

transaction(Fun) ->
	Fun().

%% @doc run after a record schema change. If the record schema schanges,
%% this will do a generic transform in an attmept to get it back to good.
%% this makes a few assumptions:
%% 1: all entries in the dets are a record.
%% 2: all records have an id as element 2, created as element last -1,
%%    and updated as element last
%% 3: all records have reasonable defaults.
%% 4: updates to records always happen by adding a feild just before
%%    created.
%% 5: records never get smaller
update_records() ->
  gen_server:call(?MODULE, update_records, infinity).

%% ====================================================================
%% gen_server
%% ====================================================================

%% --------------------------------------------------------------------
%% Init
%% --------------------------------------------------------------------

init(Options) ->
  DataDir = case proplists:get_value(data_dir, Options) of
    undefined ->
      Dir1 = filename:join(code:priv_dir(gamerl), "data"),
      ok = filelib:ensure_dir(Dir1),
      Dir1;
    Dir ->
      filename:join(Dir, "data")
  end,
  {ok, _} = dets:open_file(?dets_table, [{file, DataDir}]),
  {ok, undefined}.

%% --------------------------------------------------------------------
%% handle_call
%% --------------------------------------------------------------------

handle_call(update_records, _From, State) ->
  % I'm not worryied about blocking this server since it doesn't do
  % anything else.
  TraverseFun = fun is_current_record/1,
  Updated = dets:traverse(?dets_table, TraverseFun),
  [dets:insert(?dets_table, Update) || Update <- Updated],
  {reply, ok, State};

handle_call(_Msg, _From, State) ->
	{reply, {error, invalid}, State}.

%% --------------------------------------------------------------------
%% handle_cast
%% --------------------------------------------------------------------

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% handle_info
%% --------------------------------------------------------------------

handle_info(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% termiante
%% --------------------------------------------------------------------

terminate(_Meh, _State) ->
	dets:close(?dets_table).

%% --------------------------------------------------------------------
%% code_change
%% --------------------------------------------------------------------

code_change(_Meh, State, _Xtra) ->
	{ok, State}.

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

build_match(Comparisons, Indexes, BoundAtoms) when is_list(Comparisons) ->
  lists:foldl(fun(Comparison, MatchAcc) ->
    case build_match(Comparison, Indexes, BoundAtoms) of
      {error, _} ->
        MatchAcc;
      {ok, Match} ->
        MatchAcc ++ [Match]
    end
  end, [], Comparisons);

build_match({Key, Value}, Indexes, BoundAtoms) ->
  build_match({Key, '=:=', Value}, Indexes, BoundAtoms);

build_match({Key, Compare, Value}, Indexes, BoundAtoms) ->
  case proplists:get_value(Key, Indexes) of
    undefined ->
      {error, no_key};
    N ->
      Atom = proplists:get_value(N, BoundAtoms),
      {ok, {Compare, Atom, Value}}
  end.

is_current_record(Object) when is_atom(element(1, Object)) ->
  % it's a counter, don't muck with it.
  continue;
is_current_record(Object) ->
  {{Type, _Id} = Key, Value} = Object,
  Fields = get_field_names(Type),
  if
    length(Fields) == tuple_size(Value) - 1 ->
      continue;
    length(Fields) >= tuple_size(Value) ->
      Default = create_default(Type),
      Updated = update_old_record(Value, Default),
      dets:insert(?dets_table, {Key, Updated}),
      continue;
    true ->
      lager:notice("could not update ~p as it's already larger or same size as upgraded record", [Key]),
      lager:debug("unupdateable recored: ~p", [Value]),
      continue
  end.

create_default(RecName) ->
	RecName:new([]).

update_old_record(Old, Default) when is_tuple(Old), is_tuple(Default) ->
  OldList = tuple_to_list(Old),
  DefaultList = tuple_to_list(Default),
  {OldHead, OldTimes} = lists:split(length(OldList) - 2, OldList),
  {DefHead, _DefTimes} = lists:split(length(DefaultList) - 2, DefaultList),
  {_AlreadySet, NewFields} = lists:split(length(OldHead), DefHead),
  NewList = OldHead ++ NewFields ++ OldTimes,
  list_to_tuple(NewList).

update_counter(Type) ->
	try dets:update_counter(?dets_table, Type, 1) of
		N -> N
	catch
		error:badarg ->
			dets:insert(?dets_table, {Type, 1}),
			1
	end.

