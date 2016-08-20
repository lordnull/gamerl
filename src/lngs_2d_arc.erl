%% @doc Representation and opertionas for an arc of a circle on a 2d plan.
-module(lngs_2d_arc).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type arc() :: {Center :: lngs_2d_vector:vector(), Radius :: number(), Start :: number(), Diff :: number()}.

-export_type([arc/0]).

-export([new/4, center/1, radius/1, start/1, diff/1]).
-export([unit/1, length/1, circle/1]).
-export([startv/1, startv_origin/1, endv/1, endv_origin/1]).
-export([end_from_start/1]).
-export([length/2]).

%% @doc Generate a new arc. An arc can be described by the radius, start angle
%% and end angle on the circle the arc uses. The center is usful for finding the
%% start point and end point of the arc.
-spec new(Center :: lngs_2d_vector:vector(), Radius :: number(), Start :: number(), Diff :: number()) -> arc().
new(Center, Radius, Start, Diff) ->
	{Center, Radius, Start, Diff}.

center({Center, _, _, _}) ->
	Center.

radius({_, Radius, _, _}) ->
	Radius.

start({_, _, Start, _}) ->
	Start.

diff({_, _, _, Diff}) ->
	Diff.

%% @doc Return the circle the arc is on.
circle({Center, Radius, _, _}) ->
	lngs_2d_circle:new(Center, Radius).

%% @doc return the vector at the start position of the arc. This is
%% relative to the arc's center.
startv({_Center, Radius, Start, _Diff}) ->
	lngs_2d_vector:mult(lngs_2d_vector:from_angle(Start), Radius).

%% @doc Return the vector of the start position of the arc relative to (0,0).
startv_origin({Center, _, _, _} = Arc) ->
	StartV = startv(Arc),
	lngs_2d_vector:add(Center, StartV).

%% @doc Return the vector at the end position of the arc. This is relative
%% to the arc's center.
endv({_Center, Radius, Start, Diff}) ->
	Angle = Start + Diff,
	lngs_2d_vector:mult(lngs_2d_vector:from_angle(Angle), Radius).

%% @doc Return the vector of the end position of the arc relative to (0,0).
endv_origin({Center, _, _, _} = Arc) ->
	EndV = endv(Arc),
	lngs_2d_vector:add(Center, EndV).

%% @doc Given the arc, what is the vector to the end position if the start
%% position is the origin.
end_from_start(Arc) ->
	StartVO = startv_origin(Arc),
	EndVO = endv_origin(Arc),
	lngs_2d_vector:diff(EndVO, StartVO).

%% @doc Like a vector's unit, an arc's unit will have a length of 1 when 
%% measured from the start. This means the only number will actuall change
%% is the diff. The only exception is if the original diff is 0, in which
%% case it stays that way.
unit({Center, Radius, Start, Diff} = Arc) ->
	Circle = circle(Arc),
	Circumference = lngs_2d_circle:circumference(Circle),
	UnitDiff = (math:pi() * 2) / Circumference,
	NewDiff = if
		Diff > 0 ->
			UnitDiff;
		Diff < 0 ->
			UnitDiff * -1;
		true ->
			0
	end,
	{Center, Radius, Start, NewDiff}.

%% @doc Find the length of the given arc.
length({_, _, _, RawDiff}) when RawDiff == 0 ->
	0;

length({_Center, _Radius, _Start, RawDiff} = Arc) ->
	Circle = circle(Arc),
	Circumference = lngs_2d_circle:circumference(Circle),
	Diff = abs(RawDiff),
	(Circumference * Diff) / (math:pi() * 2).

%% @doc set the length of the given arc. Too long and you get a circle
%% back.
length({Center, Radius, Start, _Diff}, NewLength) when NewLength == 0 ->
	{Center, Radius, Start, 0};

length({Center, Radius, Start, Diff} = Arc, NewLength) when NewLength > 0 ->
	OriginalLength = ?MODULE:length(Arc),
	Ratio = NewLength / OriginalLength,
	NewDiff = Diff * Ratio,
	{Center, Radius, Start, NewDiff}.

-ifdef(TEST).

length2_test_() ->
	SizeChanges = [0.0, 0.1, 0.3, 0.5, 0.9, 1, 1.5, 1.75, 2, 3, 4],

	lists:map(fun(SizeMod) ->
		Arc = lngs_2d_arc:new({0, 0}, 5, math:pi() / 7, math:pi() * 0.3),
		OriginalLen = lngs_2d_arc:length(Arc),
		NewLen = OriginalLen * SizeMod,
		Arc2 = lngs_2d_arc:length(Arc, NewLen),
		
		Got = lngs_2d_arc:length(Arc2),
		?_assertEqual(NewLen, Got * 1.0)
	end, SizeChanges).

values_test_() ->
	Centers = [{X,Y} || X <- [0, 5], Y <- [0, 5]],
	ArgsAndExpecteds = [
		{0, math:pi() / 2, {{5,0}, {0,5}, {-5, 5}}},
		{math:pi() / 2, math:pi() / 2, {{0,5}, {-5, 0}, {-5, -5}}},
		{math:pi(), math:pi() / 2, {{-5, 0}, {0, -5}, {5, -5}}},

		{0, math:pi() / -2, {{5, 0}, {0, -5}, {-5, -5}}},
		{math:pi() / -2, math:pi() / -2, {{0, -5}, {-5, 0}, {-5, 5}}},
		{math:pi() * -1, math:pi() / -2, {{-5, 0}, {0, 5}, {5, 5}}},

		{0, math:pi() / -2, {{5,0}, {0,-5}, {-5, -5}}},
		{math:pi() / 2, math:pi() / -2, {{0,5}, {5, 0}, {5, -5}}},
		{math:pi(), math:pi() / -2, {{-5, 0}, {0, 5}, {5, 5}}},

		{math:pi() / -2, math:pi() / 2, {{0, -5}, {5, 0}, {5, 5}}},
		{math:pi() * -1, math:pi() / 2, {{-5, 0}, {0, -5}, {5, -5}}}
	],

	FullTestList = [
		{lngs_2d_arc:new(Center, 5, Start, Diff), Expecteds}
		|| Center <- Centers, {Start, Diff, Expecteds} <- ArgsAndExpecteds
	],

	lists:map(fun({Arc, {EStartV, EEndV, EEndStartV}}) ->
		{iolist_to_binary(io_lib:format("~p", [Arc])), fun() ->
			?assertEqual(EStartV, tround(lngs_2d_arc:startv(Arc))),
			?assertEqual(EEndV, tround(lngs_2d_arc:endv(Arc))),
			?assertEqual(EEndStartV, tround(lngs_2d_arc:end_from_start(Arc)))
		end}
	end, FullTestList).

tround(Tuple) ->
	IntList = [round(F) || F <- tuple_to_list(Tuple)],
	list_to_tuple(IntList).

-endif.
