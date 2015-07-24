-module(lngs_2d_intersects_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

collide_test_() ->
	[
		{"circle circle intersects", fun() ->

			C1 = lngs_2d_circle:new(lngs_2d_vector:new(3, 4), 2),
			C2 = lngs_2d_circle:new(lngs_2d_vector:new(4, 4), 2),

			Got = lngs_2d_intersects:intersects(C1, C2),

			?assert(Got)
		end},

		{"circle does not intersect", fun() ->
			
			C1 = lngs_2d_circle:new(lngs_2d_vector:new(-3, 4), 2),
			C2 = lngs_2d_circle:new(lngs_2d_vector:new(4, 4), 2),

			Got = lngs_2d_intersects:intersects(C1, C2),

			?assertNot(Got)
		end},

		{"circles and lines", [

			{"line is nowhere close", fun() ->

				Line = {lngs_2d_vector:new(3, 5), lngs_2d_vector:new(4, 3)},
				Circle = lngs_2d_circle:new(lngs_2d_vector:new(-2, -3), 2),

				Got = lngs_2d_intersects:intersects(Line, Circle),
				Got2 = lngs_2d_intersects:intersects(Circle, Line),

				?assertNot(Got),
				?assertNot(Got2)
			end},

			{"line passes through, vert right", fun() ->

				Line = {lngs_2d_vector:new(128, 64), lngs_2d_vector:new(96, 320)},
				Circle = lngs_2d_circle:new(lngs_2d_vector:new(92, 160), 64),

				Got = lngs_2d_intersects:intersects(Line, Circle),
				Got2 = lngs_2d_intersects:intersects(Circle, Line),

				?assert(Got),
				?assert(Got2)
			end},

			{"line passes through, horizon top", fun() ->

				Line = {lngs_2d_vector:new(-1, 2), lngs_2d_vector:new(9, 3)},
				Circle = lngs_2d_circle:new(lngs_2d_vector:new(4, 5), 3),

				Got = lngs_2d_intersects:intersects(Line, Circle),
				Got2 = lngs_2d_intersects:intersects(Circle, Line),

				?assert(Got),
				?assert(Got2)
			end},

			{"line starts in circle", fun() ->

				Circle = lngs_2d_circle:new(lngs_2d_vector:new(5, 6), 3),
				Line = {lngs_2d_vector:new(4, 7), lngs_2d_vector:new(1, 23)},

				Got = lngs_2d_intersects:intersects(Line, Circle),
				Got2 = lngs_2d_intersects:intersects(Circle, Line),

				?assert(Got),
				?assert(Got2)
			end},

			{"line ends in circle", fun() ->

				Circle = lngs_2d_circle:new(lngs_2d_vector:new(5, 6), 3),
 				Line = {lngs_2d_vector:new(1, 23), lngs_2d_vector:new(4, 7)},

				Got = lngs_2d_intersects:intersects(Line, Circle),
				Got2 = lngs_2d_intersects:intersects(Circle, Line),

				?assert(Got),
				?assert(Got2)

			end}

		]}

	].

-endif.
