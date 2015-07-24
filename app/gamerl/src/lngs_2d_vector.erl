-module(lngs_2d_vector).

-opaque vector() :: {number(), number()}.

-export_type([vector/0]).

-export([new/2, from_angle/1]).
-export([x/1, y/1]).
-export([add/2, sum/1]).
-export([substract/2, diff/2]).
-export([multiply/2, mult/2]).
-export([divide/2, 'div'/2]).
-export([dot/2, cross/2, length/1, 'length^2'/1, unit/1]).
-export([rotate_90/1, rotate_180/1, rotate_270/1, rotate/2, angle/1, angle_to/2]).
-export([dist/2, 'dist^2'/2]).
-export([project/2]).

new(X, Y) ->
	{X, Y}.

from_angle(Radians) ->
	{math:cos(Radians), math:sin(Radians)}.

x({X, _}) ->
	X.

y({_, Y}) ->
	Y.

add({X1, Y1}, {X2, Y2}) ->
	{X1 + X2, Y1 + Y2}.

sum(ListOfVs) ->
	lists:foldl(fun(V, Acc) ->
		add(V, Acc)
	end, {0, 0}, ListOfVs).

substract(V1, V2) ->
	diff(V1,V2).

diff({X1, Y1}, {X2, Y2}) ->
	{X1 - X2, Y1 - Y2}.

multiply(V1, V2) ->
	mult(V1, V2).

mult({X1, Y1}, {X2, Y2}) ->
	{X1 * X2, Y1 * Y2};

mult({X1, Y1}, Number) when is_number(Number) ->
	{X1 * Number, Y1 * Number}.

divide(V1, V2) ->
	'div'(V1, V2).

'div'({X1, Y1}, {X2, Y2}) ->
	{X1 / X2, Y1 / Y2};

'div'({X1, Y1}, Number) when is_number(Number) ->
	{X1 / Number, Y1 / Number}.

dot({X1, Y1}, {X2, Y2}) ->
	X1 * X2 + Y1 * Y2.

cross({X1, Y1}, {X2, Y2}) ->
	X1 * Y2 - Y1 * X2.

length(V1) ->
	math:pow('length^2'(V1), 0.5).

'length^2'({X, Y}) ->
	X * X + Y * Y.

unit(V) ->
	'div'(V, ?MODULE:length(V)).

rotate_90({X, Y}) ->
	% according to wikipedia:
	% r(90) = [[0 -1] [1 0]]
	{Y * -1, X}.

rotate_180({X, Y}) ->
	% wikpedia says this is:
	% r(180) = [[-1 0] [0 -1]]
	{X * -1, Y * -1}.

rotate_270({X, Y}) ->
	% wikipedia says:
	% r(270) = [[0 1] [ -1 0 ]]
	{Y * -1, X * -1}.

rotate({X, Y}, Rads) ->
	{X * math:cos(Rads) - Y * math:sin(Rads), X * math:sin(Rads) + Y * math:cos(Rads)}.

angle({X, Y}) ->
	math:atan2(Y, X).

angle_to(FromV, ToV) ->
	angle(diff(ToV, FromV)).

dist(V1, V2) ->
	?MODULE:length(diff(V1, V2)).

'dist^2'(V1, V2) ->
	'length^2'(diff(V1, V2)).

%% @doc Project V1 onto V2
project(V1, V2) ->
	V2Unit = unit(V2),
	dot(V1, V2Unit).

