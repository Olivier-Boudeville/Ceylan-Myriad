% Copyright (C) 2010-2021 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% @doc Unit tests for the <b>linear 2D facilities</b>.
%
% See the linear_2D tested module.
%
-module(linear_2D_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% For a possible silencing thereof:
-export([ test_normal/0, test_pivot/0, test_angle/0, test_mec/0 ]).



test_normal() ->

	Ve = [9,1],

	NL = vector2:normal_left( Ve ),
	NR = vector2:normal_right( Ve ),

	[ PVe, PNL, PNR ] = [ point2:from_vector( V ) || V <- [ Ve, NL, NR ] ],

	POrigin = point2:null(),

	true  = linear_2D:is_strictly_on_the_right( PNR, POrigin, PVe ),
	false = linear_2D:is_strictly_on_the_right( PNL, POrigin, PVe ),
	false = linear_2D:is_strictly_on_the_right( PVe,  POrigin, PVe ),

	true  = linear_2D:is_strictly_on_the_right( PVe, POrigin, PNL ),
	false = linear_2D:is_strictly_on_the_right( PVe, POrigin, PNR ),

	NonVe = vector2:scale( Ve, -1 ),
	PNonVe = point2:from_vector( NonVe ),

	true  = linear_2D:is_strictly_on_the_right( PNL, POrigin, PNonVe ),
	false = linear_2D:is_strictly_on_the_right( PNR, POrigin, PNonVe ).



test_pivot() ->
	Pa    = {469,243},
	Pivot = {348,268},
	Pb    = {421,193},
	false = linear_2D:is_strictly_on_the_right( Pa, Pivot, Pb ).



test_angle() ->

	A = point2:null(),

	B1 = {1,0},
	B2 = {3,3},
	B3 = {-5,3},

	C1 = {0,1},
	C2 = {-2,-1},
	C3 = {1,-4},

	[ test_facilities:display( "Unoriented angle between point ~ts and "
		"~ts / ~ts is ~f degrees, oriented angle is ~f degrees.~n",
		[ point2:to_string( P1 ), point2:to_string( P2 ),
		  point2:to_string( P3 ), linear_2D:abs_angle_deg( P1, P2, P3 ),
		  linear_2D:angle_deg( P1, P2, P3 ) ] )
				|| P1 <- [A], P2 <- [B1,B2,B3], P3 <- [C1,C2,C3] ],

	true  = point2:are_close( B1, point2:translate(B1,[0.000001,0]) ),
	false = point2:are_close( B1, A ),

	true  = point2:is_within( A, C1, 1 ),
	true  = point2:is_within( A, B1, 1-0.0000001 ),
	false = point2:is_within( A, B2, 2 ).



% Number of random tests to do for each given number of points:
-define( test_count, 1000 ).
%-define( test_count, 100000 ).

test_mec() ->

	PointCountMin = 3,

	%PointCountMax = 5,
	PointCountMax = 15,
	%PointCountMax = 50,
	%PointCountMax = 500,

	test_facilities:display( "Testing the Minimal Enclosing Circles, "
		"from ~B to ~B points (each with ~B random tests).",
		[ PointCountMin, PointCountMax, ?test_count ] ),

	test_mec( PointCountMin, PointCountMax, _TestCount=0 ).



-define( min_coord, 0 ).

% To boost special cases:
%-define( max_coord, 50 ).
-define( max_coord, 1000 ).


% Will progressively slow down as the number of points increases:
test_mec( PointCountMax, PointCountMax, _TestCount=?test_count ) ->
	ok;

test_mec( PointCount, PointCountMax, _TestCount=?test_count ) ->

	NewPointCount = PointCount+1,

	test_facilities:display( " - testing for ~B random points",
							 [ NewPointCount ] ),

	test_mec( NewPointCount, PointCountMax, _TCount=0 );


test_mec( PointCount, PointCountMax, TestCount ) ->

	%test_facilities:display( "   * test #~B for ~B random points",
	%						 [ TestCount, PointCount ] ),

	Points = point2:draw_integer_random( ?min_coord, ?max_coord, PointCount ),

	MECircle = bounding_box2:get_lazy_circle_box( Points ),

	% Was so quick that wanted to check:
	%test_facilities:display( "For points ~p, got MEC=~p.",
	%						 [ Points, MECircle ] ),

	[ true = bounding_box2:is_within( P, MECircle ) || P <- Points ],

	test_mec( PointCount, PointCountMax, TestCount+1 ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_normal(),
	test_pivot(),
	test_angle(),
	test_mec(),

	test_facilities:stop().
