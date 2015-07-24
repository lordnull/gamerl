-module(lngs_increment_list).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([length/1, compare/2, compare/3, fit/3, which/2]).

%% @doc Return the total length of the increment list.
length(List) ->
	lists:foldl(fun length_fold/2, 0, List).

length_fold({L, _}, Acc) ->
	L + Acc.

%% @doc Same as compare(Length, 0, List).
compare(Length, List) when Length >= 0 ->
	compare(Length, 0, List).

%% @doc Given the `Length' and `Strain', is the given length possible?
compare(Length, Strain, List) when Length >= 0 ->
	case which(Length, List) of
		{error, too_far} = Toofar ->
			Toofar;
		{ok, {_, Cost}, _Idx} when Cost * Strain > 0 ->
			{error, already_strained};
		{ok, {_, Atom}, _Idx} when is_atom(Atom) ->
			{error, Atom};
		{ok, {_, Cost}, _Idx} ->
			{ok, Cost}
	end.

%% @doc Which item in the list will allow the length, if any? If the length
%% does not hit any entry, {error, too_far} is returned. Also returns the
%% index position of the entry.
which(Length, List) when Length >= 0 ->
	which(Length, 1, List).

which(_Length, _Idx, []) ->
	{error, too_far};

which(Length, Idx, [{L, _Cost} = Entry | _Tail]) when Length < L ->
	{ok, Entry, Idx};

which(Length, Idx, [{L, _} | Tail]) ->
	which(Length - L, Idx + 1, Tail).

%% @doc Given the length, adjust it up or down until it fits the list.
fit(Length, Strains, List) ->
	case compare(Length, Strains, List) of
		{error, too_far} ->
			shorten_to_fit(Length, Strains, List);
		{error, too_short} ->
			lengthen_to_fit(Length, Strains, List);
		{error, already_strained} ->
			strained_to_fit(Length, Strains, List);
		{ok, Cost} ->
			{Length, Cost}
	end.

shorten_to_fit(Length, Strains, List) ->
	MaxLen = ?MODULE:length(List),
	RevList = lists:reverse(List),
	shorten_to_fit_(MaxLen, Strains, RevList).

shorten_to_fit_(MaxLen, Strains, [{L, Cost} | Tail]) when is_atom(Cost) ->
	shorten_to_fit_(MaxLen - L, Strains, Tail);

shorten_to_fit_(MaxLen, Strains, [{L, Cost} | Tail]) when Strains * Cost > 0 ->
	shorten_to_fit_(MaxLen - L, Strains, Tail);

shorten_to_fit_(MaxLen, Strains, [{_L, Cost} | _Tail]) ->
	{MaxLen, Cost}.

lengthen_to_fit(_Length, Strains, [{NewLength, _} | Tail]) ->
	lengthen_to_fit_(NewLength, Strains, Tail).

lengthen_to_fit_(Acc, Strains, [{L, Cost} | Tail]) when is_atom(Cost) ->
	lengthen_to_fit_(Acc + L, Strains, Tail);

lengthen_to_fit_(Acc, Strains, [{L, Cost} | Tail]) when Strains * Cost > 0 ->
	lengthen_to_fit_(Acc + L, Strains, Tail);

lengthen_to_fit_(Acc, _Strains, [{_L, Cost} | _Tail]) ->
	{Acc, Cost}.

strained_to_fit(Length, Strains, List) ->
	{ok, Entry, Idx} = which(Length, List),
	ListLen = erlang:length(List),
	HeadList = lists:sublist(List, Idx - 1),
	[Entry | TailList] = FullTail = lists:sublist(List, Idx, ListLen),
	HeadIsStrain = lists:all(fun({_L, Cost}) ->
		is_atom(Cost) orelse Cost > 0
	end, HeadList),
	case HeadIsStrain of
		true ->
			HeadLen = ?MODULE:length(HeadList),
			%Diff = Length - HeadLen,
			{WorkingLen, Cost} = lengthen_to_fit(0, Strains, FullTail),
			{HeadLen + WorkingLen, Cost};
		false ->
			shorten_to_fit(Length, Strains, HeadList)
	end.

-ifdef(TEST).

compare_test_() ->
	[
		{"rest entry at end", fun() ->
			?assertEqual({ok, 1}, compare(5, [{1, 0}, {rest, 1}]))
		end},

		{"no rest entry", fun() ->
			?assertEqual({error, too_far}, compare(5, [{1, 0}]))
		end},

		{"landing somewhere in the middle", fun() ->
			?assertEqual({ok, 0}, compare(3, [{2, too_short}, {2, 0}, {2, 1}]))
		end}
	].

fit_test_() ->
	List = [{2, too_short}, {2, 1}, {2, -1}, {2, 0}, {2, 1}],
	[
		{"too short, no strain", fun() ->
			?assertEqual({2, 1}, fit(1, 0, List))
		end},

		{"too short, strain", fun() ->
			?assertEqual({4, -1}, fit(3, 1, List))
		end},

		{"too long, no strain", fun() ->
			?assertEqual({10, 1}, fit(14, 0, List))
		end},

		{"too long, strained", fun() ->
			?assertEqual({8, 0}, fit(14, 1, List))
		end},

		{"is okay, no strain", fun() ->
			?assertEqual({7, 0}, fit(7, 0, List))
		end},

		{"is okay, strain", fun() ->
			?assertEqual({7, 0}, fit(7, 1, List))
		end}
	].

-endif.
