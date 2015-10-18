%% @doc Opertions for 2d vectors.
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

-spec new(number(), number()) -> vector().
new(X, Y) ->
	{X, Y}.

-spec from_angle(number()) -> vector().
from_angle(Radians) ->
	{math:cos(Radians), math:sin(Radians)}.

-spec x(vector()) -> number().
x({X, _}) ->
	X.

-spec y(vector()) -> number().
y({_, Y}) ->
	Y.

-spec add(vector(), vector()) -> vector().
add({X1, Y1}, {X2, Y2}) ->
	{X1 + X2, Y1 + Y2}.

-spec sum([vector()]) -> vector().
sum(ListOfVs) ->
	lists:foldl(fun(V, Acc) ->
		add(V, Acc)
	end, {0, 0}, ListOfVs).

-spec substract(vector(), vector()) -> vector().
substract(V1, V2) ->
	diff(V1,V2).

-spec diff(vector(), vector()) -> vector().
diff({X1, Y1}, {X2, Y2}) ->
	{X1 - X2, Y1 - Y2}.

-spec multiply(vector(), vector()) -> vector().
multiply(V1, V2) ->
	mult(V1, V2).

-spec mult(vector(), vector() | number()) -> vector().
mult({X1, Y1}, {X2, Y2}) ->
	{X1 * X2, Y1 * Y2};

mult({X1, Y1}, Number) when is_number(Number) ->
	{X1 * Number, Y1 * Number}.

-spec divide(vector(), vector() | number()) -> vector().
divide(V1, V2) ->
	'div'(V1, V2).

-spec 'div'(vector(), vector() | number()) -> vector().
'div'({X1, Y1}, {X2, Y2}) ->
	{X1 / X2, Y1 / Y2};

'div'({X1, Y1}, Number) when is_number(Number) ->
	{X1 / Number, Y1 / Number}.

%% @doc Return the dot product of two vectors. This is defined as
%% X1 * X2 + Y1 * Y2.
-spec dot(vector(), vector()) -> number().
dot({X1, Y1}, {X2, Y2}) ->
	X1 * X2 + Y1 * Y2.

%% @doc Return the corss product of two vectors. This is defined as
%% X1 * Y2 - Y1 * X2.
-spec cross(vector(), vector()) -> number().
cross({X1, Y1}, {X2, Y2}) ->
	X1 * Y2 - Y1 * X2.

-spec length(vector()) -> number().
length(V1) ->
	math:pow('length^2'(V1), 0.5).

%% @doc Returns the length of the vector squared. If all you need to do is
%% see if a vector is longer than another, it is more efficient to get the
%% squared length of each and compare those, thus skipping the expensive
%% square root calculation that `lenght/' incures.
-spec 'length^2'(vector()) -> number().
'length^2'({X, Y}) ->
	X * X + Y * Y.

%% @doc Return a vector that has the same 'direction' as the passed in one, but
%% has a length of 1.
-spec unit(vector()) -> vector().
unit(V) ->
	'div'(V, ?MODULE:length(V)).

%% @doc This is faster than `rotate(90)' because it avoids expensive trig
%% operations.
-spec rotate_90(vector()) -> vector().
rotate_90({X, Y}) ->
	% according to wikipedia:
	% r(90) = [[0 -1] [1 0]]
	{Y * -1, X}.

%% @doc This is faster than `rotate(180)' because it avoids expensive trig
%% operations.
-spec rotate_180(vector()) -> vector().
rotate_180({X, Y}) ->
	% wikpedia says this is:
	% r(180) = [[-1 0] [0 -1]]
	{X * -1, Y * -1}.

%% @doc This is faster than `rotate(270)' because it avoids expensive trig
%% operations.
-spec rotate_270(vector()) -> vector().
rotate_270({X, Y}) ->
	% wikipedia says:
	% r(270) = [[0 1] [ -1 0 ]]
	{Y * -1, X * -1}.

%% @doc Return the vector that results from rotation the given vector the given
%% number of radians.
-spec rotate(vector(), number()) -> vector().
rotate({X, Y}, Rads) ->
	{X * math:cos(Rads) - Y * math:sin(Rads), X * math:sin(Rads) + Y * math:cos(Rads)}.

%% @doc Return the angle to the vector in radians.
-spec angle(vector()) -> number().
angle({X, Y}) ->
	math:atan2(Y, X).

%% @doc Translate the `ToV' by the `FromV', and find the angle there. This is
%% liternatlly just `angle(diff(ToV, FromV))'.
-spec angle_to(FromV :: vector(), ToV :: vector()) -> number().
angle_to(FromV, ToV) ->
	angle(diff(ToV, FromV)).

%% @doc Find the distance from V1 to V2.
-spec dist(vector(), vector()) -> number().
dist(V1, V2) ->
	?MODULE:length(diff(V1, V2)).

%% @doc Find the squared distance between V1 and V2. If you need to compare this
%% to, say, the range of a weapon, it is less expensive computationally to square
%% the range of the weapon and compare it against this, than it is to find the
%% square root of a number, as is needed in `dist/2'.
-spec 'dist^2'(vector(), vector()) -> number().
'dist^2'(V1, V2) ->
	'length^2'(diff(V1, V2)).

%% @doc Project V1 onto V2
-spec project(vector(), vector()) -> number().
project(V1, V2) ->
	V2Unit = unit(V2),
	dot(V1, V2Unit).

