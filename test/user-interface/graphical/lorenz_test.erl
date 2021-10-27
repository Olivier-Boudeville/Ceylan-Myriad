% Copyright (C) 2014-2021 Olivier Boudeville
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


% Unit test mostly for the <b>canvas facility</b>, based on the Lorenz equations
% to show its strange attractor.
%
-module(lorenz_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% For gui-related defines:
-include("gui.hrl").


-export([ get_name/1, draw_lines/3 ]).



% Rendering section.

-type zoom_factor() :: maths_utils:factor().


% Description of a simple, local, screen coordinate system:
-record( screen, {

   center :: integer_point2(),

   zoom_x :: zoom_factor(),
   zoom_y :: zoom_factor() } ).

-type screen() :: #screen{}.



% Shorthands:

-type coordinate() :: linear:coordinate().

-type integer_point2() :: point2:integer_point2().

-type point3() :: point3:point3().

-type time() :: rk4_solver:time().
-type f3p() :: rk4_solver:f3p().



% @doc Resolves the specified equations based on the specified initial
% conditions and derivate function, notifying the specified listener of the new
% computations.
%
% In we are in 3D here:
-spec solver_main_loop( f3p(), point3(), time(), time(), screen(), pid() ) ->
															 no_return().
solver_main_loop( F, CurrentPoint, CurrentTime, Timestep, Screen,
				  ListenerPid ) ->

	receive

		{ set_time_step, NewTimestep } ->

			% Whatever the units may be:
			trace_utils:debug_fmt( "Changing time step from ~p to ~p.",
								   [ Timestep, NewTimestep ] ),

			solver_main_loop( F, CurrentPoint, CurrentTime, NewTimestep, Screen,
							  ListenerPid );


		stop ->
			ok


	after 0 ->

			% Basic version, one message per point, of course a lot too verbose:
			% New point is yn+1, current point is yn, timestep is h:
			%
			%NewPoint = compute_next_estimate( F, CurrentPoint, CurrentTime,
			%                                  Timestep ),

			%trace_utils:debug_fmt( "- new point computed: ~p", [ NewPoint ] ),

			%ListenerPid ! { draw_point, NewPoint, self() },

			% New version: sending a list of PointCount points at once, moreover
			% having already projected them on screen coordinates:

			PointCount = 50,

			{ NewProjectedPoints, LastPoint, NewTime } =
				compute_next_estimates( F, CurrentPoint, CurrentTime,
										Timestep, Screen, PointCount ),

			%trace_utils:debug_fmt( "Computed following points: ~w.~n",
			%                       [ NewProjectedPoints ] ),

			ListenerPid ! { draw_points, NewProjectedPoints, self() },

			% Explicit yielding, otherwise you may experience problems:
			timer:sleep( 1 ),

			solver_main_loop( F, LastPoint, NewTime, Timestep, Screen,
							  ListenerPid )

	end.



% @doc Returns a list of the next PointCount projected points, the last point
% computed and the corresponding next current time.
%
compute_next_estimates( F, Point, Time, Timestep, Screen, PointCount ) ->
	% Clearer than a fold:
	compute_next_estimates( F, Point, Time, Timestep, Screen, PointCount,
							_Acc=[] ).


% (helper)
compute_next_estimates( _F, Point, NextTime, _Timestep, _Screen, _PointCount=0,
						Acc ) ->
	{ lists:reverse( Acc ), Point, NextTime } ;

compute_next_estimates( F, Point, Time, Timestep, Screen, PointCount, Acc ) ->

	NewPoint = rk4_solver:compute_next_estimate3p( F, Point, Time, Timestep ),

	NewProjectedPoint = project_2D( NewPoint, Screen ),

	compute_next_estimates( F, NewPoint, Time + Timestep, Timestep, Screen,
							PointCount-1, [ NewProjectedPoint | Acc ] ).




% Function f( t, v ) corresponding to the equations of the Lorenz system.
%
% See http://en.wikipedia.org/wiki/Lorenz_system
%
lorenz_function( _Time, _Vector=[ X0, Y0, Z0 ] ) ->

	% These equations here happen not to depend on time.

	Sigma = 10.0,
	Rho   = 28.0,
	Beta  = 8.0 / 3.0,

	X1 = Sigma * ( Y0 - X0 ),
	Y1 = X0 * ( Rho - Z0 ) - Y0,
	Z1 = X0 * Y0 - Beta * Z0,

	[ X1, Y1, Z1 ].




% GUI section.


% State of the program, passed between event handlers.
-record( gui_state, { main_frame,
					  start_button,
					  stop_button,
					  quit_button,
					  canvas,
					  screen :: screen(),

		  % The solver table is an associative table whose keys are the PID of
		  % each solver, and whose values are {Color, LastPoint} pairs:
		  %
		  solver_table :: table:table() } ).



% The left part of the windows gathers the buttons, while the right one shows
% the canvas.


-spec get_main_window_width() -> coordinate().
get_main_window_width() ->
	1920.


-spec get_main_window_height() -> coordinate().
get_main_window_height() ->
	1080.



%-spec get_canvas_width() -> coordinate().
%get_canvas_width() ->
%	640.


%-spec get_canvas_height() -> coordinate().
%get_canvas_height() ->
%	480.





% @doc Lists all the declared names of widget identifiers.
get_all_id_names() ->
	[ 'MainFrame', 'StartButton', 'StopButton', 'QuitButton' ].



% @doc Returns the numerical ID corresponding to the specified name.
%
% (a good target for a parse transform)
%
-spec get_id( atom() ) -> gui:id().
get_id( Name ) ->
	list_utils:get_index_of( Name, get_all_id_names() ).



% @doc Returns the name (as an atom) of the specified widget (expected to be
% named).
%
-spec get_name( gui:id() ) -> atom().
get_name( Id ) ->

	Names = get_all_id_names(),

	Len = length( Names ),

	case Id of

		Id when Id < 1 orelse Id > Len ->
			unknown;

		_ ->
			lists:nth( Id, Names )

	end.



% @doc Initialises the GUI and associated parts (solver).
-spec start() -> no_return().
start() ->

	gui:start(),

	observer:start(),

	FrameSize = { get_main_window_width(), get_main_window_height() },

	MainFrame = gui:create_frame( _Title="Lorenz Test", _FramePos=auto,
		FrameSize, _FrameStyle=default, _Id=get_id( 'MainFrame' ),
		_MaybeParent=undefined ),

	%gui:connect( MainFrame, close_window ),
	gui:subscribe_to_events( { onWindowClosed, MainFrame } ),

	StatusBar = gui:create_status_bar( MainFrame ),

	SolverCount = system_utils:get_core_count(),
	%SolverCount = 2,
	%SolverCount = 0,

	gui:push_status_text( text_utils:format( "Initialisation of ~B solvers.",
											 [ SolverCount ] ), StatusBar ),

	LeftPanel = gui:create_panel( MainFrame ),

	RightPanel = gui:create_panel( MainFrame ),

	%gui:set_background_color( MainFrame, red ),
	%gui:set_background_color( LeftPanel, blue ),
	%gui:set_background_color( RightPanel, green ),

	MainSizer = gui:create_sizer( horizontal ),

	% Constant width:
	gui:add_to_sizer( MainSizer, LeftPanel,
					  [ { proportion, 0 }, { flag, [ expand_fully ] } ] ),

	% Grows with the window:
	gui:add_to_sizer( MainSizer, RightPanel,
					  [ { proportion, 2 }, { flag, [ expand_fully ] } ] ),

	ControlBoxSizer = gui:create_sizer_with_labelled_box( vertical, LeftPanel,
														  "Controls" ),

	% Adding the buttons to the control panel:

	% Common settings:

	Position = auto,
	ButtonSize = auto,
	ButtonStyle = default,
	ParentButton = LeftPanel,

	StartButton = gui:create_button( "Start resolution", Position, ButtonSize,
		ButtonStyle, get_id( 'StartButton' ), ParentButton ),

	%gui:connect( StartButton, command_button_clicked ),
	gui:subscribe_to_events( { onButtonClicked, StartButton } ),


	StopButton = gui:create_button( "Stop resolution", Position, ButtonSize,
		ButtonStyle, get_id( 'StopButton' ), ParentButton ),

	%gui:connect( StopButton, command_button_clicked ),
	gui:subscribe_to_events( { onButtonClicked, StopButton } ),


	QuitButton = gui:create_button( "Quit", Position, ButtonSize, ButtonStyle,
									get_id( 'QuitButton' ), ParentButton ),

	%gui:connect( QuitButton, command_button_clicked ),
	gui:subscribe_to_events( { onButtonClicked, QuitButton } ),


	gui:set_tooltip( LeftPanel, "Controls for the Lorenz test" ),

	ButtonOpt = [ { flag, [ expand_fully ] } ],

	gui:add_to_sizer( ControlBoxSizer, StartButton, ButtonOpt ),

	gui:add_to_sizer( ControlBoxSizer, StopButton, ButtonOpt ),

	gui:add_to_sizer( ControlBoxSizer, QuitButton, ButtonOpt ),

	gui:set_sizer( LeftPanel, ControlBoxSizer ),

	PolyBoxSizer = gui:create_sizer_with_labelled_box( vertical, RightPanel,
													   "Phase Space" ),

	Canvas = gui:create_canvas( RightPanel ),

	gui:set_background_color( Canvas, red ),

	gui:clear( Canvas ),

	%gui:connect( Canvas, paint ),
	gui:subscribe_to_events( { onRepaintNeeded, Canvas } ),

	%gui:connect( Canvas, size ),
	gui:subscribe_to_events( { onResized, Canvas } ),

	gui:add_to_sizer( PolyBoxSizer, Canvas,
					  [ { proportion, 1 }, { flag, [ expand_fully ] } ] ),

	gui:set_tooltip( Canvas, "Lorenz Attractor." ),

	gui:set_sizer( RightPanel, PolyBoxSizer ),

	gui:set_sizer( MainFrame, MainSizer ),

	% Sets the GUI to visible:
	gui:show( MainFrame ),

	Screen = #screen{ center={ get_main_window_width() / 3 - 550,
							   get_main_window_height() / 2 },
					  zoom_x=24.0,
					  zoom_y=24.0 },

	Colors = gui_color:get_random_colors( SolverCount ),

	% The function corresponding to the equation system to solve:
	Derivative = fun lorenz_function/2,

	% Initial conditions:
	InitialPoint = { 0.1, 0.0, 0.0 },

	InitialTime = 0.0,

	InitialTimestep = 0.005,

	SolverTable = create_solver_table( Derivative, Colors, InitialPoint,
									   InitialTime, InitialTimestep, Screen ),

	InitialState = #gui_state{ main_frame=MainFrame,
							   start_button=StartButton,
							   stop_button=StopButton,
							   quit_button=QuitButton,
							   canvas=Canvas,
							   screen=Screen,
							   solver_table=SolverTable },

	erlang:process_flag( priority, _Level=high ),

	gui_main_loop( InitialState ),

	gui:stop().



% This table helps the rendering process keeping track of the solvers feeding it
% with new points to plot.
%
create_solver_table( Derivative, Colors, InitialPoint, InitialTime,
					 InitialTimestep, Screen ) ->
	create_solver_table( Derivative, Colors, InitialPoint, InitialTime,
						 InitialTimestep, Screen, _Acc=[] ).


% (helper)
create_solver_table( _Derivative, _Colors=[], _InitialPoint, _InitialTime,
					 _InitialTimestep, _Screen, Acc ) ->
	table:new( Acc );

create_solver_table( Derivative, _Colors=[ C | T ],
					 _PreviousInitialPoint={ X, Y, Z }, InitialTime,
					 InitialTimestep, Screen, Acc ) ->

	NewInitialPoint = { X+5.0, Y+5.0, Z+5.0 },

	% For the closure:
	TestPid = self(),

	NewSolver = spawn_link( fun() -> solver_main_loop( Derivative,
		NewInitialPoint, InitialTime, InitialTimestep, Screen, TestPid ) end ),

	GUIInitialPoint = project_2D( NewInitialPoint, Screen ),

	NewAcc = [ { NewSolver, { C, GUIInitialPoint } } | Acc ],

	create_solver_table( Derivative, T, NewInitialPoint, InitialTime,
						 InitialTimestep, Screen, NewAcc ).



% The main loop of this test, driven by the receiving of MyriadGUI messages.
gui_main_loop( State=#gui_state{ main_frame=MainFrame,
								 start_button=StartButton,
								 stop_button=StopButton,
								 quit_button=QuitButton,
								 canvas=Canvas,
								 screen=Screen } ) ->

	gui:draw_line( Canvas, { 1, 1 }, { 40, 30 }, red ),

	%test_facilities:display( "~nEntering main loop." ),

	% 'undefined' if having to quit:
	MaybeNewState = receive

		% Routine messages sent by solvers shall be listed last, otherwise they
		% will eclipse other messages (ex: GUI ones):

		%#wx{ obj=MainFrame, event={ wxClose, close_window } } ->
		{ onWindowClosed, [ MainFrame, _Context ] } ->
			test_facilities:display( "Quitting Lorenz test." ),
			undefined;


		%#wx{ obj=StartButton,
		%	 event=#wxCommand{ type=command_button_clicked } } ->
		{ onButtonClicked, [ StartButton, _Context ] } ->
			test_facilities:display( "Start button clicked." ),
			%NewCanvas = render_test( Canvas ),
			gui:clear( Canvas ),
			gui:draw_line( Canvas, { 1, 40 }, { 40, 1 }, blue ),
			State#gui_state{ canvas=Canvas };


		%#wx{ obj=StopButton,
		%	 event=#wxCommand{ type=command_button_clicked } } ->
		{ onButtonClicked, [ StopButton, _Context ] } ->
			test_facilities:display( "Stop button clicked." ),
			State#gui_state{ canvas=Canvas };


		%#wx{ obj=QuitButton,
		%	 event=#wxCommand{ type=command_button_clicked } } ->
		{ onButtonClicked, [ QuitButton, _Context ] } ->
			test_facilities:display( "Quit button clicked." ),

			SolverTable = State#gui_state.solver_table,

			[ SolverPid ! stop || SolverPid <- table:keys( SolverTable ) ],

			undefined;


		%#wx{ obj=Any, event=#wxCommand{ type=command_button_clicked } } ->
		{ onButtonClicked, [ AnyOtherButton, _Context ] } ->
			test_facilities:display( "Following button clicked: ~w.",
									 [ AnyOtherButton ] ),
			State;


		% Received for example when another window overlapped:
		%#wx{ event=#wxPaint{} } ->
		{ onRepaintNeeded, [ _GUIObject, _Context ] } ->
			test_facilities:display( "Repainting." ),
			gui:clear( Canvas ),
			gui:blit( Canvas ),
			State;


		%#wx{ event=#wxSize{ size=NewSize } } ->
		{ onResized, [ _GUIObject, NewSize, _Context ] } ->
			test_facilities:display( "Resizing to ~w.", [ NewSize ] ),
			gui:resize( Canvas, NewSize ),
			%gui:clear( NewCanvas ),
			%State#gui_state{ canvas=NewCanvas };
			State;				

		{ draw_points, NewPoints, SendingSolverPid } ->

			%trace_utils:debug_fmt( "Drawing ~B points from ~w.~n",
		   %    [ length( NewPoints ), SendingSolverPid ] ),

			SolverTable = State#gui_state.solver_table,

			{ Color, LastPoint } =
						table:get_value( SendingSolverPid, SolverTable ),

			NewLastPoint =
						draw_lines( Canvas, [ LastPoint | NewPoints ], Color ),

			gui:blit( Canvas ),

			SolverTable = State#gui_state.solver_table,

			NewSolverTable = table:add_entry( _K=SendingSolverPid,
								_V={ Color, NewLastPoint }, SolverTable ),

			State#gui_state{ solver_table=NewSolverTable };


		{ draw_point, NewPoint, SendingSolverPid } ->

			trace_utils:debug_fmt( " - drawing ~p (from ~p)~n",
								   [ NewPoint, SendingSolverPid ] ),

			SolverTable = State#gui_state.solver_table,

			{ Color, LastPoint } =
						table:get_value( SendingSolverPid, SolverTable ),

			SourceDrawPoint = project_2D( LastPoint, Screen ),

			DestinationDrawPoint = project_2D( NewPoint, Screen ),

			gui:draw_line( Canvas, SourceDrawPoint, DestinationDrawPoint,
						   Color ),

			gui:blit( Canvas ),

			NewSolverTable = table:add_entry( _K=SendingSolverPid,
									_V={ Color, NewPoint }, SolverTable ),

			State#gui_state{ solver_table=NewSolverTable };


		Any ->
			trace_utils:warning_fmt( "Lorenz test got event '~w' (ignored).",
									 [ Any ] ),
			State

	end,

	case MaybeNewState of

		undefined ->
			% Simply stop recursing:
			ok;

		NewState ->
			gui_main_loop( NewState )

	end.



% @doc Projects the specified 3D point onto 2D screen system.
-spec project_2D( point3(), screen() ) -> integer_point2().
project_2D( _Point={ X, Y, Z }, #screen{ center={ Xc, Yc },
										 zoom_x=ZoomX,
										 zoom_y=ZoomY } ) ->

	F = 1 / math:sqrt( 2 ),

	{ round( Xc + ZoomX * ( Z - F*X ) ), round( Yc + ZoomY * ( Y - F*X ) ) }.



% @doc Draws lines between all specified (already projected) points, and returns
% the last of these points.
%
draw_lines( _Canvas, _Points=[ LastPoint ], _Color ) ->
	LastPoint;

draw_lines( Canvas, _Points=[ P1, P2 | T ], Color ) ->

	gui:draw_line( Canvas, P1, P2, Color ),

	draw_lines( Canvas, [ P2 | T ], Color ).



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the GUI test, being in batch mode)" );

		false ->
			start()

	end,
test_facilities:stop().
