-module(lngs_grid_util).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([dist/2, dist/4, 'dist^2'/2, 'dist^2'/4]).
-export([angle_to/2, deg_angle_to/2]).
-export([deg_to_rad/1, rad_to_deg/1]).

dist({X1, Y1}, {X2, Y2}) ->
	dist(X1, Y1, X2, Y2);

dist(Ship1, Ship2) when is_tuple(Ship1), is_tuple(Ship2) ->
	Middles = dist(Ship1:x(), Ship1:y(), Ship2:x(), Ship2:y()),
	DiameterSum = (Ship1:diameter() / 2) + (Ship2:diameter() / 2),
	LessDiameters = Middles - DiameterSum,
	if
		LessDiameters < 0 ->
			0;
		true ->
			LessDiameters
	end.

dist(X1, Y1, X2, Y2) ->
	math:pow('dist^2'(X1, Y1, X2, Y2), 0.5).

'dist^2'({X1, Y1}, {X2, Y2}) ->
	'dist^2'(X1, Y1, X2, Y2);

'dist^2'(Ship1, Ship2) when is_tuple(Ship1), is_tuple(Ship2) ->
	'dist^2'(Ship1:x(), Ship1:y(), Ship2:x(), Ship2:y()).

'dist^2'(X1, Y1, X2, Y2) ->
	math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2).

deg_angle_to(FromXY, ToXY) ->
	Angle = angle_to(FromXY, ToXY),
	rad_to_deg(Angle).

angle_to({FromX, FromY}, {ToX, ToY}) ->
	angle_to(FromX, ToX, FromY, ToY);

angle_to(FromShip, ToShip) when is_tuple(FromShip), is_tuple(ToShip) ->
	angle_to(FromShip:x(), ToShip:x(), FromShip:y(), ToShip:y()).

%angle_to(FromX, ToX, Y, Y) when ToX >= FromX ->
%	0.0;
%
%angle_to(FromX, ToX, Y, Y) when ToX < FromX ->
%	math:pi();
%
angle_to(X, X, FromY, ToY) when FromY =< ToY ->
	math:pi() / 2;

angle_to(X, X, FromY, ToY) when FromY > ToY ->
	math:pi() / -2;

angle_to(FromX, ToX, FromY, ToY) ->
	DeltaX = ToX - FromX,
	DeltaY = ToY - FromY,
	Theta = math:atan(DeltaY / DeltaX),
	angle_to_maybe_behind(FromX, ToX, Theta).

angle_to_maybe_behind(X, X, Theta) ->
	Theta;

angle_to_maybe_behind(FromX, ToX, Theta) when ToX < FromX, Theta > 0 ->
	-1 * (math:pi() - Theta);

angle_to_maybe_behind(FromX, ToX, Theta) when ToX < FromX, Theta < 0 ->
	math:pi() + Theta;

angle_to_maybe_behind(FromX, ToX, _Theta) when ToX < FromX ->
	math:pi();

angle_to_maybe_behind(_FromX, _ToX, Theta) ->
	Theta.

deg_to_rad(Deg) ->
	Deg * (math:pi() / 180).

rad_to_deg(Rad) ->
	Rad * (180 / math:pi()).

-ifdef(TEST).

dist_test_() -> [

	{"0,0 to 5,0", fun() ->
		Got = dist({0,0}, {5,0}),
		?assertEqual(5.0, Got)
	end},

	{"-5,0 to 5,0", fun() ->
		Got = dist({-5, 0}, {5, 0}),
		?assertEqual(10.0, Got)
	end},

	{"0,0 to 5,5", fun() ->
		Got = dist({0,0}, {5,5}),
		?assertEqual(math:pow(50, 0.5), Got)
	end},

	{"-7,3 to -2,-2", fun() ->
		Got = dist({-7, 3}, {-2, -2}),
		?assertEqual(math:pow(50, 0.5), Got)
	end},

	{"5,0 to 0,5", fun() ->
		Got = dist({5,0}, {0,5}),
		?assertEqual(math:pow(50, 0.5), Got)
	end}
	
	].

angle_to_test_() -> [

		{"0,0 to 5,0", fun() ->
			Got = deg_angle_to({0,0}, {5,0}),
			?assertEqual(0.0, Got)
		end},

		{"0,0 to -5,0", fun() ->
			Got = deg_angle_to({0,0}, {-5, 0}),
			?assertEqual(180.0, Got)
		end},

		{"0,0 to 0,-5", fun() ->
			Got = deg_angle_to({0,0}, {0,-5}),
			?assertEqual(-90.0, Got)
		end},

		{"0,0 to 0,5", fun() ->
			Got = deg_angle_to({0,0}, {0,5}),
			?assertEqual(90.0, Got)
		end},

		{"0,0 to 5,5", fun() ->
			Got = deg_angle_to({0,0}, {5,5}),
			?assertEqual(45.0, Got)
		end},

		{"0,0 to 5,-5", fun() ->
			Got = deg_angle_to({0,0}, {5,-5}),
			?assertEqual(-45.0, Got)
		end},

		{"0,0 to -5,5", fun() ->
			Got = deg_angle_to({0,0}, {-5, 5}),
			?assertEqual(135.0, Got)
		end},

		{"0,0 to -5,-5", fun() ->
			Got = deg_angle_to({0,0}, {-5, -5}),
			?assertEqual(-135.0, Got)
		end}

	].

-endif.
