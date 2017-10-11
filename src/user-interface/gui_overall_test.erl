% Copyright (C) 2003-2017 Olivier Boudeville
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



% More global test for the MyriadGUI toolbox.
%
% See the gui.erl tested module.
%
-module(gui_overall_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% For gui-related defines:
%-include("gui.hrl").


% FIXME:
-export([ render_mec/2 ]).

% State of the application, kept and updated by its main loop.
%
-record( my_test_state, {

		   main_frame = undefined :: gui:frame(),

		   render_shape_button = undefined :: gui:button(),
		   render_mec_button   = undefined :: gui:button(),
		   clear_canvas_button = undefined :: gui:button(),
		   add_point_button    = undefined :: gui:button(),
		   load_image_button   = undefined :: gui:button(),
		   quit_button         = undefined :: gui:button(),

		   canvas = undefined :: gui:canvas(),
		   point_count = 0 :: basic_utils:count(),
		   render_mode = test_shape_rendering :: 'test_shape_rendering'
											   | 'test_dynamic_mec'

}).

-type my_test_state() :: #my_test_state{}.



-spec get_main_window_width() -> linear:coordinate().
get_main_window_width() ->
	800.


-spec get_main_window_height() -> linear:coordinate().
get_main_window_height() ->
	600.



% Canvas dimensions automatically determined based on parent panel.


-spec run_test_gui() -> basic_utils:void().
run_test_gui() ->

	test_facilities:display( "~nStarting the actual overall MyriadGUI test, "
							 "from ~w.", [ self() ] ),

	gui:start(),

	MainFrameSize = { get_main_window_width(), get_main_window_height() },

	MainFrame = gui:create_frame( _Title="GUI Test", MainFrameSize ),

	% This process will subscribe to following event:
	MainFrameEvents = { onWindowClosed, MainFrame },

	% To check surfaces:
	%gui:set_background_color( MainFrame, red ),
	%gui:set_background_color( LeftPanel, blue ),
	%gui:set_background_color( RightPanel, green ),

	StatusBar = gui:create_status_bar( MainFrame ),

	gui:push_status_text( "Waiting for points to be added.", StatusBar ),


	LeftPanel = gui:create_panel( MainFrame ),

	RightPanel = gui:create_panel( MainFrame ),

	MainSizer = gui:create_sizer( _Orientation=horizontal ),

	% Constant width:
	gui:add_to_sizer( MainSizer, LeftPanel,
					  [ { proportion, 0 }, { flag, [ expand_fully ] } ] ),

	% Grows with the window:
	gui:add_to_sizer( MainSizer, RightPanel,
					  [ { proportion, 2 }, { flag, [ expand_fully ] } ] ),


	ControlBoxSizer = gui:create_sizer_with_labelled_box( vertical, LeftPanel,
														  "Controls" ),

	% Adding the buttons to the control panel:

	ButtonLabels = [ "Render a few random shapes", "Render MEC", "Add point",
					 "Load image", "Clear canvas", "Quit" ],

	ControlButtons = [ RenderShapeButton, RenderMECButton, AddPointButton,
					   LoadImageButton, ClearCanvasButton, QuitButton ] =
		gui:create_buttons( ButtonLabels, _Parent=LeftPanel ),

	ButtonEvents = { onButtonClicked, ControlButtons },

	gui:set_tooltip( LeftPanel, "Controls for the GUI test" ),

	gui:set_tooltip( RenderShapeButton, "Render shape" ),
	gui:set_tooltip( RenderMECButton, "Render Minimal Enclosing Circle" ),
	gui:set_tooltip( AddPointButton, "Add a point to the\ncurrent polygon" ),
	gui:set_tooltip( LoadImageButton, "Load image" ),
	gui:set_tooltip( ClearCanvasButton, "Clear canvas" ),
	gui:set_tooltip( QuitButton, "Quit" ),

	ButtonOpt = [ { flag, [ expand_fully ] } ],

	gui:add_to_sizer( ControlBoxSizer, ControlButtons, ButtonOpt ),

	gui:set_sizer( LeftPanel, ControlBoxSizer ),

	PolyBoxSizer = gui:create_sizer_with_labelled_box( vertical, RightPanel,
													   "Polygon View" ),

	Canvas = gui:create_canvas( RightPanel ),

	gui:set_background_color( Canvas, pink ),

	CanvasEvents = { [ onRepaintNeeded, onResized ], Canvas },

	gui:add_to_sizer( PolyBoxSizer, Canvas,
					  [ { proportion, 1 }, { flag, [ expand_fully ] } ] ),

	gui:set_tooltip( Canvas, "Random polygons and their MEC\n"
							 "(Minimum Enclosing Circle Box) are drawn here." ),

	gui:set_sizer( RightPanel, PolyBoxSizer ),

	gui:set_sizer( MainFrame, MainSizer ),

	% Renders the GUI:
	gui:show( MainFrame ),

	InitialPointCount = 3,

	EventsOfInterest = [ MainFrameEvents, ButtonEvents, CanvasEvents ],

	InitialTestState = #my_test_state{ main_frame=MainFrame,
									   render_shape_button=RenderShapeButton,
									   render_mec_button=RenderMECButton,
									   clear_canvas_button=ClearCanvasButton,
									   add_point_button=AddPointButton,
									   load_image_button=LoadImageButton,
									   quit_button=QuitButton,
									   canvas=Canvas,
									   point_count=InitialPointCount,
									   render_mode=test_shape_rendering },


	gui:receive_events( EventsOfInterest ),

	test_main_loop( InitialTestState ).



% The main loop of this test.
%
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( TestState=#my_test_state{ main_frame=MainFrame,
										  render_shape_button=RenderShapeButton,
										  render_mec_button=RenderMECButton,
										  add_point_button=AddButton,
										  load_image_button=LoadImageButton,
										  clear_canvas_button=ClearCanvasButton,
										  quit_button=QuitButton,
										  canvas=Canvas,
										  point_count=PointCount,
										  render_mode=RenderMode } ) ->

	trace_utils:trace_fmt( "Test main loop running, render mode is ~p, "
						   "point count is ~B.", [ RenderMode, PointCount ] ),

	receive

		{ onWindowClosed, [ MainFrame, Context ] } ->

			trace_utils:trace_fmt( "Main frame ~s has been closed "
								   "(~s), test success.",
								   [ gui:object_to_string( MainFrame ),
									 gui:context_to_string( Context ) ] ),

			gui:destruct_window( MainFrame ),

			gui:stop();


		{ onButtonClicked, [ RenderShapeButton, Context ] } ->

			trace_utils:trace_fmt( "Render shape button ~s has been clicked "
								   "(~s).",
								   [ gui:object_to_string( QuitButton ),
									 gui:context_to_string( Context ) ] ),

			test_main_loop( TestState );


		{ onButtonClicked, [ RenderMECButton, Context ] } ->

			trace_utils:trace_fmt( "Render MEC button ~s has been clicked "
								   "(~s).",
								   [ gui:object_to_string( QuitButton ),
									 gui:context_to_string( Context ) ] ),

			test_main_loop( TestState );


		{ onButtonClicked, [ AddButton, Context ] } ->

			trace_utils:trace_fmt( "Add point button ~s has been clicked "
								   "(~s).",
								   [ gui:object_to_string( QuitButton ),
									 gui:context_to_string( Context ) ] ),

			test_main_loop( TestState );


		{ onButtonClicked, [ LoadImageButton, Context ] } ->

			trace_utils:trace_fmt( "Load image button ~s has been clicked "
								   "(~s).",
								   [ gui:object_to_string( QuitButton ),
									 gui:context_to_string( Context ) ] ),

			test_main_loop( TestState );


		{ onButtonClicked, [ ClearCanvasButton, Context ] } ->

			trace_utils:trace_fmt( "Clear canvas button ~s has been clicked "
								   "(~s).",
								   [ gui:object_to_string( QuitButton ),
									 gui:context_to_string( Context ) ] ),

			test_main_loop( TestState );


		{ onButtonClicked, [ QuitButton, Context ] } ->

			trace_utils:trace_fmt( "Quit button ~s has been clicked "
								   "(~s), test success.",
								   [ gui:object_to_string( QuitButton ),
									 gui:context_to_string( Context ) ] ),

			gui:destruct_window( MainFrame ),

			gui:stop();


		{ onRepaintNeeded, [ Canvas, Context ] } ->

			trace_utils:trace_fmt( "Canvas ~s needing repaint (~s).",
								   [ gui:object_to_string( Canvas ),
									 gui:context_to_string( Context ) ] ),

			case RenderMode of

				test_shape_rendering ->
					render_shapes( Canvas );

				test_dynamic_mec ->
					render_mec( Canvas, PointCount )

			end,

			test_main_loop( TestState );


		{ onResized, [ Canvas, NewSize, Context ] } ->

			trace_utils:trace_fmt( "Canvas ~s resized to ~w (~s).",
								   [ gui:object_to_string( Canvas ), NewSize,
									 gui:context_to_string( Context ) ] ),

			case RenderMode of

				test_shape_rendering ->

					% Needed, as the canvas must adapt to its resized panel:
					%
					gui_canvas:update( Canvas ),

					render_shapes( Canvas );

				test_dynamic_mec ->
					render_mec( Canvas, PointCount )

			end,

			test_main_loop( TestState );


		Other ->
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message: ~p.", [ Other ] )

	end.





% Renders the shape examples onto the specified canvas.
%
-spec render_shapes( gui_canvas:canvas() ) -> gui_canvas:canvas().
render_shapes( Canvas ) ->

	trace_utils:trace_fmt( "Rendering shapes, redrawing canvas ~w, "
						   "of size ~w.",
						   [ Canvas, gui_canvas:get_size( Canvas ) ] ),

	gui_canvas:set_background_color( Canvas, yellow ),

	gui_canvas:clear( Canvas ),

	P1 = { 20,10 },
	P2 = { 100, 200 },

	gui_canvas:draw_line( Canvas, P1, P2 ),

	P3 = {300,50},
	Purple = gui_color:get_color( blue ),

	gui_canvas:draw_line( Canvas, P2, P3, Purple ),
	P4 = {400,250},

	gui_canvas:set_draw_color( Canvas, red ),
	gui_canvas:draw_lines( Canvas, [ P1, P3, P4 ] ),


	gui_canvas:set_draw_color( Canvas, black ),
	gui_canvas:draw_cross( Canvas, {36,26}, _FirstEdgeLength=6 ),

	LabelPosition = {72,300},

	LabelText = "A simple label, the cross indicating its specified location",

	gui_canvas:draw_label( Canvas, LabelPosition, LabelText ),
	gui_canvas:draw_cross( Canvas, LabelPosition ),

	gui_canvas:draw_labelled_cross( Canvas, {36,86}, _SecondEdgeLength=4,
									"Cross label" ),

	gui_canvas:set_draw_color( Canvas, firebrick ),
	gui_canvas:set_fill_color( Canvas, chartreuse ),
	gui_canvas:draw_circle( Canvas, _CircleCenter={80,80}, _Radius=80 ),

	gui_canvas:set_fill_color( Canvas, none ),
	gui_canvas:draw_circle( Canvas, _OtherCircleCenter={180,180},
							_OtherRadius=180 ),

	% Taken from polygon_test.erl:
	MyTriangle = polygon:update_bounding_box( lazy_circle,
	   polygon:set_edge_color( fuchsia,
			  polygon:get_triangle( {110,110}, {550,155}, {420,335} ) ) ),

	MyUprightSquare = polygon:update_bounding_box( lazy_circle,
	   polygon:set_edge_color( steelblue,
			  polygon:get_upright_square( _Center={250,250},
										  _EdgeLength=50 ) ) ),

	polygon:render( MyTriangle, Canvas ),
	polygon:render( MyUprightSquare, Canvas ),

	gui_canvas:blit( Canvas ).



% Renders the MEC (Minimal Enclosing Circle) view, for a polygon of specified
% number of vertices, whose coordinates are randomly determined.
%
-spec render_mec( gui_canvas:canvas(), basic_utils:count() ) ->
						gui_canvas:canvas().
render_mec( Canvas, PointCount ) ->

	trace_utils:trace_fmt( "Rendering MEC with ~B points.", [ PointCount ] ),

	gui_canvas:set_background_color( Canvas, blue ),

	gui_canvas:clear( Canvas ),

	gui_canvas:set_draw_color( Canvas, white ),

	RandomPoints = [ { random_utils:get_random_value( 200 ) + 300,
					   random_utils:get_random_value( 300 ) + 100 }
					|| _Count <- lists:seq( 1, PointCount ) ],

	%trace_utils:debug_fmt( "Random points: ~w.", [ RandomPoints ] ),

	{ Pivot, RemainingPoints } = linear_2D:find_pivot( RandomPoints ),

	%trace_utils:debug_fmt( "Pivot: ~w, remaining: ~w.",
	% [ Pivot, RemainingPoints ] ),

	SortedPoints = linear_2D:sort_by_angle( Pivot, RemainingPoints ),

	%trace_utils:debug_fmt( "Sorted points: ~w.", [ SortedPoints ] ),

	gui_canvas:draw_lines( Canvas, [ Pivot | SortedPoints ] ++ [ Pivot ],
						   green ),

	HullPoints = linear_2D:compute_convex_hull( RandomPoints ),

	%trace_utils:debug_fmt( "Hull points: ~w.", [ HullPoints ] ),

	%trace_utils:debug_fmt( "Number of hull/set points: ~B/~B.",
	%		   [ length( HullPoints ), PointCount ] ),

	{ ExactCenter, SquareRadius } =
		bounding_box:get_minimal_enclosing_circle_box( HullPoints ),

	Center = linear_2D:roundify( ExactCenter ),

	Radius = math:sqrt( SquareRadius ),

	%trace_utils:debug_fmt( "Bounding Minimal Enclosing Circle: "
	%		   "center = ~p, radius = ~f.~n", [ Center, Radius ] ),

	gui_canvas:draw_labelled_cross( Canvas, Center, 5, purple, "MEC center" ),

	gui_canvas:draw_circle( Canvas, Center, round( Radius ) ),

	gui_canvas:draw_lines( Canvas, [ Pivot | HullPoints ], red ),

	% Draws the crosses last, to have them on top:
	gui_canvas:draw_labelled_cross( Canvas, Pivot, _OtherEdgeLength=10, black,
									"Pivot" ),

	gui_canvas:set_draw_color( Canvas, white ),

	gui_canvas:draw_numbered_points( Canvas, SortedPoints ),

	gui_canvas:blit( Canvas ).




% Runs the test.
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display( "(not running the MyriadGUI test, "
									 "being in batch mode)" );

		false ->
			run_test_gui()

	end,

	test_facilities:stop().
