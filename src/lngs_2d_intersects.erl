%% @doc Functions the determine if one arbitrary gamerl 2d shape intersects
%% another.
-module(lngs_2d_intersects).

-export([intersects/2]).

intersects({{_, _} = C1, Radius1}, {{_, _} = C2, Radius2}) when is_number(Radius1), is_number(Radius2) ->
	RadiusSumSqr = math:pow(Radius1 + Radius2, 2),
	CenterDistSqr = lngs_2d_vector:'dist^2'(C1, C2),
	CenterDistSqr =< RadiusSumSqr;

intersects({{_, _}, _} = Circle, {{_, _}, {_, _}} = Line) ->
	intersects(Line, Circle);

intersects({{Lx1, Ly1} = SegA, {Lx2, Ly2} = SegB}, {{Cx, Cy} = CircPos, R}) when is_number(R) ->
	% the argorthm taken from:
	% http://doswa.com/2009/07/13/circle-segment-intersectioncollision.html
	% boils down to
	%		Find closest point on the segment to the center of the circle
	%		If that point is inside the circle, the segment intersects.
	SegV = lngs_2d_vector:diff(SegB, SegA),
	PtV = lngs_2d_vector:diff(CircPos, SegA),
	ProjVLen = lngs_2d_vector:project(PtV, SegV),
	%ProjVLen = lngs_2d_vector:length(ProjV),
	SegVLen = lngs_2d_vector:length(SegV),
	Closest = if
		ProjVLen < 0 ->
			SegA;
		ProjVLen > SegVLen ->
			SegB;
		true ->
			SegVUnit = lngs_2d_vector:unit(SegV),
			ProjV = lngs_2d_vector:mult(SegVUnit, ProjVLen),
			lngs_2d_vector:add(SegA, ProjV)
	end,
	DistV = lngs_2d_vector:diff(CircPos, Closest),
	DistVLen = lngs_2d_vector:length(DistV),
	if
		DistVLen < R ->
			true;
		%DistVLen =< 0 ->
		%	throw(circle_center_on_segment);
		true ->
			false
	end;

intersects({{_, _}, _} = Circle, {{_, _}, _, _, _, _} = Arc) ->
	intersects(Arc, Circle);

intersects({Center, Radius, Radians1, Radians2, Direction}, {CircCenter, CircRadius} = Circle) ->
	case intersects({Center, Radius}, Circle) of
		false ->
			false;
		true ->
			Moved = lngs_2d_vector:diff(CircCenter, Center),
			AngleToMoved = lngs_2d_vector:angle(Moved),
			case {Radians1, Radians2, Direction} of
				{R1, R2, clockwise} when R1 < R2 ->
					% +--> R2
					% |
					% |
					% +-- R1
					AngleToMoved =< R1 orelse R2 =< AngleToMoved;
				{R1, R2, counter_clockwise} when R1 < R2 ->
					% R2 --+
					%      |
					%      |
					% R1 <-+
					R1 =< AngleToMoved andalso AngleToMoved =< R2;
				{R1, R2, clockwise} when R2 < R1 ->
					% R1 --+
					%      |
					%      |
					% R2 <-+
					R2 =< AngleToMoved andalso AngleToMoved =< R1;
				{R1, R2, counter_clockwise} when R2 < R1 ->
					% +--> R1
					% |
					% |
					% +-- R2
					AngleToMoved =< R2 orelse R1 =< AngleToMoved
			end
	end;

intersects(_Wut, _Huh) ->
	throw(unknown_shapes).

