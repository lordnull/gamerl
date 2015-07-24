-module(lngs_data).

-behavior(gen_server).

% api
-export([
	start_link/1,
	start_link/2,

	get_by_id/2,
	search/2,
	save/1,
	delete/1,
	delete/2,

	t_get_by_id/2,
	t_get_by_id/3,
	t_search/2,
	t_search/3,
	t_save/1,
	t_save/2,
	t_delete/1,
	t_delete/2,
	t_delete/3,

	transaction/1,
	transaction/2

]).

-define(t(FuncName, StoreRef, Args), transaction(fix_up_ref(StoreRef), fun() -> erlang:apply(?MODULE, FuncName, Args) end)).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

%% ====================================================================
%% External api
%% ====================================================================

start_link(Callback) ->
	gen_server:start_link(?MODULE, Callback, []).

start_link(Name, Callback) when is_atom(Name) ->
	gen_server:start_link({local, Name}, ?MODULE, Callback, []);

start_link(Name, Callback) ->
	gen_server:start_link(Name, ?MODULE, Callback, []).

get_by_id(Type, Id) ->
	maybe_in_transaction(get_by_id, [Type, Id]).

t_get_by_id(Type, Id) ->
	t_get_by_id(?MODULE, Type, Id).

t_get_by_id(StoreRef, Type, Id) ->
	?t(get_by_id, StoreRef, [Type, Id]).

search(Type, SearchParams) ->
	maybe_in_transaction(search, [Type, SearchParams]).

t_search(Type, Params) ->
	t_search(?MODULE, Type, Params).

t_search(StoreRef, Type, Params) ->
	?t(search, StoreRef, [Type, Params]).

save(Record) ->
	maybe_in_transaction(save, [Record]).

t_save(Record) ->
	t_save(?MODULE, Record).

t_save(StoreRef, Record) ->
	?t(save, StoreRef, [Record]).

delete(Record) ->
	Type = element(1, Record),
	Id = Record:id(),
	delete(Type, Id).

t_delete(Record) ->
	t_delete({local, ?MODULE}, Record).

delete(Type, Id) ->
	maybe_in_transaction(delete, [Type, Id]).

t_delete(StoreRef, Record) when is_tuple(StoreRef), is_pid(StoreRef) ->
	?t(delete, StoreRef, [Record]);

t_delete(Type, Id) ->
	t_delete({local, ?MODULE}, Type, Id).

t_delete(StoreRef, Type, Id) ->
	?t(delete, StoreRef, [Type, Id]).

transaction(Fun) ->
	transaction(?MODULE, Fun).

transaction(StoreRef, Fun) ->
	gen_server:call(StoreRef, {api, transaction, [Fun]}).

%% ====================================================================
%% Gen server callbacks
%% ====================================================================

%% --------------------------------------------------------------------
%% Init
%% --------------------------------------------------------------------

init(Callback) ->
	{ok, Callback}.

%% --------------------------------------------------------------------
%% handle_call
%% --------------------------------------------------------------------

handle_call({api, Function, Args}, From, Callback) ->
	{Pid, _Ref} = erlang:spawn_monitor(fun() ->
		%lager:debug("Transaction pid ~p start. Callback: ~p; From: ~p; Args: ~p", [self(), Callback, From, Args]),
		put(ssg_transaction_module, Callback),
		Res = apply(Callback, Function, Args),
		%lager:debug("Replying to ~p with ~p", [From, Res]),
		gen_server:reply(From, Res)
	end),
	put(Pid, From),
	{noreply, Callback};

handle_call(_Msg, _From, Callback) ->
	{reply, {error, invalid}, Callback}.

%% --------------------------------------------------------------------
%% handle_cast
%% --------------------------------------------------------------------

handle_cast(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% handle_info
%% --------------------------------------------------------------------

handle_info({'DOWN', _Ref, process, Pid, Why}, State) when Why =/= normal andalso Why =/= shutdown ->
	lager:notice("Transaction pid ~p exited abnormally due to ~p", [Pid, Why]),
	case get(Pid) of
		undefined ->
			ok;
		From ->
			gen_server:reply(From, {error, Why}),
			erase(Pid)
	end,
	{noreply, State};
handle_info(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% terminate
%% --------------------------------------------------------------------

terminate(_Why, _State) -> ok.

%% --------------------------------------------------------------------
%% code_change
%% --------------------------------------------------------------------

code_change(_Huh, State, _Xtra) ->
	{ok, State}.

%% ====================================================================
%% Internal Functions
%% ====================================================================

maybe_in_transaction(Function, Args) ->
	Module = extract_module(),
	apply(Module, Function, Args).

extract_module() ->
	case get(ssg_transaction_module) of
		undefined ->
			error(no_transaction);
		Else ->
			Else
	end.

fix_up_ref({local, Atom}) ->
	Atom;

fix_up_ref(Ref) ->
	Ref.

