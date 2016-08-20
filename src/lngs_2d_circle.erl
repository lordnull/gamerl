%% @doc Operatins for cirlces on a 2d plane.
-module(lngs_2d_circle).

-type radius() :: number().
-type center() :: lngs_2d_vector:vector().
-type circle() :: {center(), radius()}.

-export_type([radius/0, center/0, circle/0]).

-export([new/2, radius/1, center/1, x/1, y/1]).
-export([diameter/1, circumference/1, area/1]).

-spec new(Center :: center(), Radius :: radius()) -> circle().
new(Center, Radius) ->
	{Center, Radius}.

-spec radius(Cirlce :: circle()) -> radius().
radius({_Center, Radius}) ->
	Radius.

-spec center(Circle :: circle()) -> center().
center({Center, _Radius}) ->
	Center.

-spec x(Circle :: circle()) -> number().
x({{X, _Y}, _Radius}) ->
	X.

-spec y(Circle :: circle()) -> number().
y({{_X, Y}, _Radius}) ->
	Y.

diameter({_Center, Radius}) ->
	2 * Radius.

circumference(Circle) ->
	math:pi() * diameter(Circle).

area({_Center, Radius}) ->
	math:pi() * (math:pow(Radius, 2)).

