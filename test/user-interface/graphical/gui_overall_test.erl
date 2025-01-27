% Copyvright (C) 2013-2023 Olivier Boudeville
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
% Creation date: 2013.

-module(gui_overall_test).

-moduledoc """
More **global testing** of the MyriadGUI toolbox.

See the gui.erl tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").

% For the circle record; the sole include that MyriadGUI user code shall
% reference:
%
-include_lib("myriad/include/myriad_gui.hrl").


% For reuse by other tests:
-export([ get_main_window_width/0, get_main_window_height/0,
		  get_main_window_size/0 ]).

% For unused silencing:
-export([ test_state_to_string/1 ]).


-record( my_test_state, {

	main_frame :: gui_frame:frame(),

	render_shape_button :: button(),
	render_mec_button   :: button(),
	clear_canvas_button :: button(),
	add_point_button    :: button(),

	% Convenient to detect canvas repaints (as disappears then):
	paste_image_button :: button(),

	quit_button :: button(),

	canvas :: canvas(),

	% Allows to keep track of how many renderings were done:
	render_count = 0 :: count(),

	point_count = 0 :: count(),

	render_mode = test_shape_rendering :: render_mode() } ).


-doc """
The state of the test application, kept and updated by its main loop.
""".
-type my_test_state() :: #my_test_state{}.


% Local type:
-type render_mode() :: 'test_shape_rendering' | 'test_dynamic_mec'.


% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type size() :: gui:size().
-type width() :: gui:width().
-type height() :: gui:height().

-type button() :: gui_button:button().
-type canvas() :: gui_canvas:canvas().



-spec test_state_to_string( my_test_state() ) -> ustring().
test_state_to_string( #my_test_state{ main_frame=MainFrame,
									  render_shape_button=RenderShapeButton,
									  render_mec_button=RenderMECButton,
									  clear_canvas_button=ClearCanvasButton,
									  add_point_button=AddPointButton,
									  paste_image_button=PasteImageButton,
									  quit_button=QuitButton,
									  canvas=Canvas,
									  render_count=RenderCount,
									  point_count=PointCount,
									  render_mode=RenderMode } ) ->
	text_utils:format( "test state: ~ts", [
		text_utils:strings_to_string( [
			text_utils:format( "MainFrame: ~p", [ MainFrame ] ),
			text_utils:format( "RenderShapeButton: ~p", [ RenderShapeButton ] ),
			text_utils:format( "RenderMECButton: ~p", [ RenderMECButton ] ),
			text_utils:format( "ClearCanvasButton: ~p", [ ClearCanvasButton ] ),
			text_utils:format( "AddPointButton: ~p", [ AddPointButton ] ),
			text_utils:format( "PasteImageButton: ~p", [ PasteImageButton ] ),
			text_utils:format( "QuitButton: ~p", [ QuitButton ] ),
			text_utils:format( "Canvas: ~p", [ Canvas ] ),
			text_utils:format( "RenderCount: ~p", [ RenderCount ] ),
			text_utils:format( "PointCount: ~p", [ PointCount ] ),
			text_utils:format( "RenderMode: ~p", [ RenderMode ] ) ] ) ] ).



-spec get_main_window_width() -> width().
get_main_window_width() ->
	800.


-spec get_main_window_height() -> height().
get_main_window_height() ->
	600.



-doc "Returns the size of the main test window.".
-spec get_main_window_size() -> size().
get_main_window_size() ->
	{ get_main_window_width(), get_main_window_height() }.



% Canvas size automatically determined based on its parent panel.


-doc "Executes the actual test.".
-spec run_test_gui() -> void().
run_test_gui() ->

	test_facilities:display( "Starting the actual overall MyriadGUI test, "
							 "from user process ~w.", [ self() ] ),

	trace_utils:notice( "A frame with a stack of buttons shall appear; "
		"one may increase the number of random points and repeatedly "
		"request the corresponding MEC (Minimal Enclosing Circle) "
		"to be displayed (in blue), together with the convex hull "
		"(in green; often coinciding with "
		"the MEC for lower numbers of points). "
		"The test will end as soon as the frame is closed "
		"or the Quit button is clicked." ),

	gui:start(),

	MainFrame = gui_frame:create( _Title="MyriadGUI Overall Test",
								  get_main_window_size() ),

	% This process will subscribe to following event:
	MainFrameEvents = { onWindowClosed, MainFrame },


	StatusBar = gui_statusbar:create( MainFrame ),

	gui_statusbar:push_text( StatusBar, "Waiting for points to be added." ),

	LeftPanel = gui_panel:create( MainFrame ),

	RightPanel = gui_panel:create( MainFrame ),

	% To check surfaces:
	%gui_widget:set_background_color( MainFrame, red ),
	%gui_widget:set_background_color( LeftPanel, blue ),
	%gui_widget:set_background_color( RightPanel, green ),

	MainSizer = gui_sizer:create( _Orientation=horizontal ),

	% Constant width:
	gui_sizer:add_element( MainSizer, LeftPanel,
						   [ { proportion, 0 }, expand_fully ] ),

	% Grows with the window:
	gui_sizer:add_element( MainSizer, RightPanel,
						   [ { proportion, 2 }, expand_fully ] ),


	ControlBoxSizer = gui_sizer:create_with_labelled_box( vertical, "Controls",
														  LeftPanel ),

	% Adding the buttons to the control panel:

	ButtonLabels = [ "Render a few random shapes", "Render MEC", "Add point",
					 "Paste image", "Clear canvas", "Quit" ],

	ControlButtons = [ RenderShapeButton, RenderMECButton, AddPointButton,
					   PasteImageButton, ClearCanvasButton, QuitButton ] =
		gui_button:create_multiple( ButtonLabels, _Parent=LeftPanel ),

	ButtonEvents = { onButtonClicked, ControlButtons },

	gui_widget:set_tooltip( LeftPanel, "Controls for the GUI test" ),

	gui_widget:set_tooltip( RenderShapeButton, "Render shape" ),

	gui_widget:set_tooltip( RenderMECButton,
							"Render Minimal Enclosing Circle" ),

	gui_widget:set_tooltip( AddPointButton,
							"Add a point to the\ncurrent polygon" ),

	gui_widget:set_tooltip( PasteImageButton, "Paste image" ),
	gui_widget:set_tooltip( ClearCanvasButton, "Clear canvas" ),
	gui_widget:set_tooltip( QuitButton, "Quit" ),

	gui_sizer:add_elements( ControlBoxSizer, ControlButtons, expand_fully ),

	gui_widget:set_sizer( LeftPanel, ControlBoxSizer ),

	PolyBoxSizer = gui_sizer:create_with_labelled_box( vertical, "Polygon View",
													   RightPanel ),

	Canvas = gui_canvas:create( RightPanel ),

	gui_widget:set_background_color( Canvas, pink ),

	% Generally back-buffered canvases need to subscribe to onRepaintNeeded but
	% also to onResized, if they just *blit* their back-buffer (not re-render
	% it) when receiving an onRepaintNeeded event; otherwise the resized canvas
	% will blit a different-sized back-buffer and show random parts of memory.
	%
	% An alternative (less efficient) option is not to subscribe to onResized
	% events and to perform a full rendering at each onRepaintNeeded.
	%
	%CanvasEvents = { onRepaintNeeded, Canvas },
	CanvasEvents = { [ onRepaintNeeded, onResized ], Canvas },

	gui_sizer:add_element( PolyBoxSizer, Canvas,
						   [ { proportion, 1 }, expand_fully ] ),

	gui_widget:set_tooltip( Canvas, "Random polygons and their MEC\n"
		"(Minimum Enclosing Circle Box) are drawn here." ),

	gui_widget:set_sizer( RightPanel, PolyBoxSizer ),

	gui_widget:set_sizer( MainFrame, MainSizer ),

	EventsOfInterest = [ MainFrameEvents, ButtonEvents, CanvasEvents ],

	% To be done before rendering the GUI (with gui:show/1), as it may result in
	% events to be emitted (e.g. onRepaintNeeded) that would not be received, if
	% not already subscribed to:
	%
	gui:subscribe_to_events( EventsOfInterest ),

	InitialPointCount = 3,

	InitialRenderMode = test_shape_rendering,

	% First rendering not relevant here (still in default, initial size of
	% 20x20, and a onRepaintNeeded event will happen first):
	%
	%render( InitialRenderMode, InitialPointCount, Canvas ),

	InitialTestState = #my_test_state{ main_frame=MainFrame,
									   render_shape_button=RenderShapeButton,
									   render_mec_button=RenderMECButton,
									   clear_canvas_button=ClearCanvasButton,
									   add_point_button=AddPointButton,
									   paste_image_button=PasteImageButton,
									   quit_button=QuitButton,
									   canvas=Canvas,
									   point_count=InitialPointCount,
									   render_mode=InitialRenderMode },

	cond_utils:if_defined( myriad_gui_test_verbose,
		trace_utils:debug_fmt( "Initial ~ts",
							   [ test_state_to_string( InitialTestState ) ] ) ),

	% Renders the GUI:
	gui_frame:show( MainFrame ),

	test_main_loop( InitialTestState ).



-doc "The main loop of this test.".
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( TestState=#my_test_state{ main_frame=MainFrame,
										  render_shape_button=RenderShapeButton,
										  render_mec_button=RenderMECButton,
										  add_point_button=AddButton,
										  paste_image_button=PasteImageButton,
										  clear_canvas_button=ClearCanvasButton,
										  quit_button=QuitButton,
										  canvas=Canvas,
										  render_count=RenderCount,
										  render_mode=RenderMode } ) ->

	cond_utils:if_defined( myriad_gui_test_verbose,
		trace_utils:info_fmt( "Test main loop running, render mode is ~p, "
			"render count is ~B, point count is ~B.",
			[ RenderMode, RenderCount,
			  TestState#my_test_state.point_count ] ) ),

	% We use trace_utils:notice* to discriminate more easily the traces
	% originating from this test from any MyriadGUI ones:

	receive

		{ onButtonClicked, [ RenderShapeButton, _RenderShapeButtonId,
							 EventContext ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Render shape test button ~ts has been clicked (~ts).",
					[ gui:object_to_string( RenderShapeButton ),
					  gui_event:context_to_string( EventContext ) ] ),
				basic_utils:ignore_unused( EventContext ) ),

			NewTestState = TestState#my_test_state{
				render_mode=test_shape_rendering },

			render_shapes( Canvas ),

			test_main_loop( NewTestState );


		{ onButtonClicked, [ RenderMECButton, _RenderMECButtonId,
							 EventContext ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Render MEC test button ~ts has been clicked (~ts).",
					[ gui:object_to_string( RenderMECButton ),
					  gui_event:context_to_string( EventContext ) ] ),
				basic_utils:ignore_unused( EventContext ) ),

			NewTestState = TestState#my_test_state{
				render_mode=test_dynamic_mec },

			render_mec( Canvas, TestState#my_test_state.point_count ),

			test_main_loop( NewTestState );


		{ onButtonClicked, [ AddButton, _AddButtonId, EventContext ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Add point test button ~ts has been clicked (~ts).",
					[ gui:object_to_string( AddButton ),
					  gui_event:context_to_string( EventContext ) ] ),
				basic_utils:ignore_unused( EventContext ) ),

			NewPointCount = TestState#my_test_state.point_count + 1,

			NewTestState = TestState#my_test_state{ point_count=NewPointCount },

			test_main_loop( NewTestState );


		{ onButtonClicked, [ PasteImageButton, _PasteImageButtonId,
							 EventContext ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Paste image button ~ts has been clicked (~ts).",
					[ gui:object_to_string( PasteImageButton ),
					  gui_event:context_to_string( EventContext ) ] ),
				basic_utils:ignore_unused( EventContext ) ),

			ImagePath = "../../../doc/myriad-small.png",

			gui_canvas:load_image( Canvas, _Pos={150,50}, ImagePath ),
			gui_canvas:blit( Canvas ),

			test_main_loop( TestState );


		{ onButtonClicked, [ ClearCanvasButton, _ClearCanvasButtonId,
							 EventContext ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Clear canvas button ~ts has been clicked (~ts).",
					[ gui:object_to_string( ClearCanvasButton ),
					  gui_event:context_to_string( EventContext ) ] ),
				basic_utils:ignore_unused( EventContext ) ),

			gui_canvas:clear( Canvas ),
			gui_canvas:blit( Canvas ),

			test_main_loop( TestState );


		{ onButtonClicked, [ QuitButton, _QuitButtonId, EventContext ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt( "Quit test button ~ts has been clicked "
					"(~ts), test success.",
					[ gui:object_to_string( QuitButton ),
					  gui_event:context_to_string( EventContext ) ] ),
				basic_utils:ignore_unused( EventContext ) ),

			gui_frame:destruct( MainFrame ),

			gui:stop();


		{ onRepaintNeeded, [ Canvas, _CanvasId, EventContext ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Test canvas '~ts' needing repaint (~ts).",
					[ gui:object_to_string( Canvas ),
					  gui_event:context_to_string( EventContext ) ] ),
				basic_utils:ignore_unused( EventContext ) ),

			gui_canvas:blit( Canvas ),

			test_main_loop( TestState#my_test_state{
				render_count=RenderCount+1 } );


		{ onResized, [ Canvas, _CanvasId, NewSize, EventContext ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Test canvas '~ts' resized to ~p (~ts).",
					[ gui:object_to_string( Canvas ), NewSize,
					  gui_event:context_to_string( EventContext ) ] ),
				basic_utils:ignore_unused( [ NewSize, EventContext ] ) ),

			render( RenderMode, TestState#my_test_state.point_count, Canvas ),

			test_main_loop( TestState#my_test_state{
				render_count=RenderCount+1 } );


		{ onWindowClosed, [ MainFrame, _MainFrameId, EventContext ] } ->

			trace_utils:notice_fmt( "Test main frame ~ts has been closed "
				"(~ts), test success.",
				[ gui:object_to_string( MainFrame ),
				  gui_event:context_to_string( EventContext ) ] ),

			gui_frame:destruct( MainFrame ),

			gui:stop();


		Other ->
			% Extra newline for better separation:
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message:~n ~p.~n", [ Other ] ),
			test_main_loop( TestState )

	end.



-doc "Renders the specified canvas.".
-spec render( render_mode(), count(), canvas() ) -> void().
render( _RenderMode=test_shape_rendering, _PointCount, Canvas ) ->
	render_shapes( Canvas );

render( _RenderMode=test_dynamic_mec, PointCount, Canvas ) ->
	render_mec( Canvas, PointCount ).



-doc "Renders the shape examples onto the specified canvas.".
-spec render_shapes( canvas() ) -> void().
render_shapes( Canvas ) ->

	%trace_utils:info_fmt(
	%   "Rendering example shapes, redrawing canvas ~w, of size ~w.",
	%   [ Canvas, gui:get_size( Canvas ) ] ),

	%gui_widget:set_background_color( Canvas, yellow ),

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

	gui_canvas:set_fill_color( Canvas, undefined ),
	gui_canvas:draw_circle( Canvas, _OtherCircleCenter={180,180},
							_OtherRadius=180 ),

	% Taken from polygon_test.erl:
	MyTriangle = polygon:update_bounding_surface( lazy_circle,
		polygon:set_edge_color( fuchsia,
			polygon:get_triangle( {110,110}, {550,155}, {420,335} ) ) ),

	MyUprightSquare = polygon:update_bounding_surface( lazy_circle,
		polygon:set_edge_color( steelblue,
			polygon:get_upright_square( _Center={250,250}, _EdgeLength=50 ) ) ),

	polygon:render( MyTriangle, Canvas ),
	polygon:render( MyUprightSquare, Canvas ),


	UnitRoots = linear_2D:get_roots_of_unit( _N=7, _StartingAngle=math:pi()/4 ),

	ScaledRoundRoots = [ point2:roundify( point2:scale( P, _Factor=50 ) )
									|| P <- UnitRoots ],

	TransVec = [ 80, 400 ],
	TransRoots = [ point2:translate( P, TransVec ) || P <- ScaledRoundRoots ],

	RootPoly = polygon:get_polygon( TransRoots ),

	polygon:render( RootPoly, Canvas ),

	gui_canvas:blit( Canvas ).



-doc """
Renders the MEC (Minimal Enclosing Circle) view, for a polygon of specified
number of vertices, whose coordinates are randomly determined at each
invocation.
""".
-spec render_mec( canvas(), count() ) -> void().
render_mec( Canvas, PointCount ) ->

	%trace_utils:info_fmt( "Rendering MEC for ~B random points.",
	%                      [ PointCount ] ),

	%gui_widget:set_background_color( Canvas, blue ),

	gui_canvas:clear( Canvas ),

	gui_canvas:set_draw_color( Canvas, white ),

	RandomPoints = [ { random_utils:get_uniform_value( 200 ) + 300,
					   random_utils:get_uniform_value( 300 ) + 100 }
							|| _Count <- lists:seq( 1, PointCount ) ],

	%trace_utils:debug_fmt( "Random points: ~w.", [ RandomPoints ] ),

	{ Pivot, RemainingPoints } = linear_2D:find_pivot( RandomPoints ),

	%trace_utils:debug_fmt( "Pivot: ~w, remaining: ~w.",
	%   [ Pivot, RemainingPoints ] ),

	SortedPoints = linear_2D:sort_by_angle( Pivot, RemainingPoints ),

	%trace_utils:debug_fmt( "Sorted points: ~w.", [ SortedPoints ] ),

	gui_canvas:draw_lines( Canvas, [ Pivot | SortedPoints ] ++ [ Pivot ],
						   green ),

	HullPoints = linear_2D:compute_convex_hull( RandomPoints ),

	%trace_utils:debug_fmt( "Hull points: ~w.", [ HullPoints ] ),

	%trace_utils:debug_fmt( "Number of hull/set points: ~B/~B.",
	%                       [ length( HullPoints ), PointCount ] ),

	#circle{ center=ExactCenter, square_radius=SquareRadius } =
		bounding_surface:get_minimal_enclosing_circle( HullPoints ),

	Center = point2:roundify( ExactCenter ),

	Radius = math:sqrt( SquareRadius ),

	%trace_utils:debug_fmt( "Bounding Minimal Enclosing Circle: "
	%                       "center = ~p, radius = ~f.", [ Center, Radius ] ),

	gui_canvas:draw_labelled_cross( Canvas, Center, 5, purple, "MEC center" ),

	gui_canvas:draw_circle( Canvas, Center, round( Radius ) ),

	gui_canvas:draw_lines( Canvas, [ Pivot | HullPoints ], blue ),

	% Draws the crosses last, to have them on top:
	gui_canvas:draw_labelled_cross( Canvas, Pivot, _OtherEdgeLength=10, black,
							 "Pivot" ),

	gui_canvas:set_draw_color( Canvas, blue ),

	gui_canvas:draw_numbered_points( Canvas, SortedPoints ),

	gui_canvas:blit( Canvas ).



-doc "Runs the test.".
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the MyriadGUI test, being in batch mode)" );

		false ->
			run_test_gui()

	end,

	test_facilities:stop().
