% Copyright (C) 2003-2013 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Unit tests for the GUI toolbox.
% See the gui.erl tested module.
-module(gui_test).


-define(Tested_modules,[gui]).


% For test_finished/0 and al:
-include("test_facilities.hrl").


get_main_window_width() ->
	800.

get_main_window_height() ->
	600.


get_canvas_width() ->
	640.

get_canvas_height() ->
	480.



init_test_gui() ->

	WindowSize = [ {width,get_main_window_width()},
				   {height,get_main_window_height()} ],

	GsId = gs:start(),

	MainWin = gs:window( GsId, WindowSize ++ [
								{title,"GUI Test"},
								{bg,gui:get_color(red)} ]),

	gs:config( MainWin, WindowSize ),



	gs:create( button, add_point_button, MainWin, [{width,75},{y,60},{x,10},
							 {width,100}, {label,{text,"Add point"}}]),

	InitialPointCount = 3,


	% Sets the GUI to visible:
	gs:config( MainWin, {map,true} ),

	gui_main_loop( MainWin, InitialPointCount, undefined ),

	gs:stop().


create_basic_test_gui( Canvas ) ->

	P1 = {20,10},
	P2 = {100,200},

	gui:draw_line( P1, P2, Canvas ),

	P3 = {300,50},
	Purple = gui:get_color(purple),

	gui:draw_line( P2, P3, Purple, Canvas ),
	P4 = {400,250},

	gui:draw_lines( [P1,P3,P4], gui:get_color(blue), Canvas ),

	gui:draw_cross( {36,26}, _FirstEdgeLength=6, gui:get_color(red), Canvas ),

	gui:draw_labelled_cross( {36,86}, _SecondEdgeLength=4, "Cross label",
							 Canvas ),

	% Taken from polygon_test.erl:
	MyTriangle = polygon:update_bounding_box( lazy_circle,
	  polygon:set_edge_color( yellow,
			  polygon:get_triangle( {110,110}, {250,155}, {120,335} ) ) ),

	MyUprightSquare = polygon:update_bounding_box( lazy_circle,
	  polygon:set_fill_color( red,
			  polygon:get_upright_square( _Center = {250,250},
										  _EdgeLength = 50 ) ) ),
	polygon:render( MyTriangle, Canvas ),
	polygon:render( MyUprightSquare, Canvas ).



create_test_gui( PointCount, MainWin ) ->

	Canvas = gs:create( canvas, MainWin, [
		{hscroll,bottom},
		{vscroll,left},
		{width,get_canvas_width()},
		{height,get_canvas_height()},
		% Centers canvas:
		{x,(get_main_window_width()-get_canvas_width()) / 2},
		{y,(get_main_window_height()-get_canvas_height()) / 2},
		{bg,gui:get_color(lime)}
		%{scrollregion, {100,200,30,200}}
										 ] ),

	RandomPoints = [ {random:uniform(200)+300,random:uniform(300)+100}
					 || _Count <- lists:seq(1,PointCount) ],

	%io:format( "Random points: ~w.~n", [RandomPoints] ),

	{Pivot,RemainingPoints} = linear_2D:find_pivot( RandomPoints ),

	%io:format( "Pivot: ~w, remaining: ~w.~n", [Pivot,RemainingPoints] ),

	gui:draw_labelled_cross( Pivot, _OtherEdgeLength=10, blue, "Pivot",
							 Canvas ),

	SortedPoints = linear_2D:sort_by_angle( Pivot, RemainingPoints ),

	%io:format( "Sorted points: ~w.~n", [SortedPoints] ),

	gui:draw_numbered_points( SortedPoints, Canvas ),
	gui:draw_lines( [Pivot|SortedPoints] ++ [Pivot], blue, Canvas ),


	HullPoints = linear_2D:compute_convex_hull( RandomPoints ),

	%io:format( "Hull points: ~w.~n", [HullPoints] ),
	io:format( "Number of hull/set points: ~B/~B.~n",
			   [length(HullPoints),PointCount] ),

	{Center,SquareRadius} = bounding_box:get_minimal_enclosing_circle_box(
							  HullPoints ),

	Radius = math:sqrt(SquareRadius),
	io:format( "Bounding Minimal Enclosing Circle: "
			   "center = ~p, radius = ~f.~n~n",
			   [Center,Radius] ),

	gui:draw_labelled_cross( Center, 5, purple, "MEC center", Canvas ),

	gui:draw_circle( Center, Radius, Canvas ),

	gui:draw_lines( [Pivot|HullPoints], red, Canvas ),
	Canvas.


% Main loop:
gui_main_loop( MainWin, PointCount, Canvas ) ->

	%io:format( "~nEntering main loop, point count is ~B.~n", [PointCount-1] ),

	receive

		{gs,_Pair,destroy,[],[]} ->
			io:format( "Quitting GUI test.~n" ),
			test_finished();

		to_do ->
			create_basic_test_gui( Canvas ),
			gui_main_loop( MainWin, PointCount, Canvas );

		{gs,add_point_button,click,[],[_Label]} ->
			gs:destroy( Canvas ),
			NewCanvas = create_test_gui( PointCount, MainWin ),
			gui_main_loop( MainWin, PointCount+1, NewCanvas );

		X ->
			io:format("GUI test got event '~w' (ignored).~n",[X]),
			gs:destroy( Canvas ),
			NewCanvas = create_test_gui( PointCount, MainWin ),
			gui_main_loop( MainWin, PointCount+1, NewCanvas )

	end.



% The actual test:
run() ->

	io:format( "--> Testing modules ~p.~n", [ ?Tested_modules ] ),

	case init:get_argument('-batch') of

		{ok,_} ->
			io:format( "(not running the GUI test, being in batch mode)~n" );

		_ ->
			init_test_gui()

	end,

	test_finished().
