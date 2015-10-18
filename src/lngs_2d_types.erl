%% @doc Type holder for the 2d structures, allowing rec2json to convert
%% between them nicely.

-module(lngs_2d_types).

-opaque vector() :: lngs_2d_vector:vector().
-opaque arc() :: lngs_2d_arc:arc().

-export_type([vector/0, arc/0]).

-export([vector/1, arc/1]).

vector({X, Y}) ->
	{ok, [X, Y]};

vector([X, Y]) ->
	{ok, {X, Y}};

vector(_) ->
	error.

arc({CenterVector, Radius, Start, Diff} = Arc) ->
	try begin {ok, [
		{<<"center">>, element(2, vector(CenterVector))},
		{<<"radius">>, Radius},
		{<<"start">>, Start},
		{<<"diff">>, Diff},
		{<<"startv">>, element(2, vector(lngs_2d_arc:startv(Arc)))},
		{<<"endv">>, element(2, vector(lngs_2d_arc:endv(Arc)))},
		{<<"end_from_start">>, element(2, vector(lngs_2d_arc:end_from_start(Arc)))}
	]} end catch
		_:_ ->
			error
	end;

arc(Json) ->
	try begin
		Center = vector(proplists:get_value(<<"center">>, Json)),
		Radius = proplists:get_value(<<"radius">>, Json),
		Start = proplists:get_value(<<"start">>, Json),
		End = proplists:get_value(<<"end">>, Json),
		Direction = case proplists:get_value(<<"direction">>, Json) of
			<<"counter_clockwise">> ->
				counter_clockwise;
			<<"clockwise">> ->
				clockwise
		end,
		Arc = lngs_2d_arc:new(Center, Radius, Start, End, Direction),
		{ok, Arc}
	end catch
		_What:_Why ->
			error
	end.

