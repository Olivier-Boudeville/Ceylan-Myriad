% Copyright (C) 2023-2023 Olivier Boudeville
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
% Creation date: Wednesday, October 4, 2023.


% @doc Testing the <b>support for the management of splash screens</b>.
-module(gui_splash_test).


% Implementation notes:
%
% We favor mostly the PNG and JPEG formats.
%
% Here, rather than using our canvas, we directly paint of the panel defined
% within the main frame.


% For run/0 export and al:
-include("test_facilities.hrl").


% For myriad_spawn_link:
-include_lib("myriad/include/spawn_utils.hrl").


% Shorthands:

-type frame() :: gui:frame().
-type panel() :: gui:panel().
-type bitmap() :: gui:bitmap().



% State of the test application, kept and updated by its main loop.
-record( my_test_state, {

	main_frame :: frame(),

	splash_frame :: maybe( frame() ),

	% The splash panel (in the splash frame), used here as a canvas:
	splash_panel :: panel(),

	% The off-screen bitmaps where all renderings take place:
	backbuffer :: bitmap(),

	% The ready-to-use in-memory data corresponding to an image to be displayed:
	image_bitmap :: bitmap() } ).

-type my_test_state() :: #my_test_state{}.



% Silencing now that not subscribing to onRepaintNeeded:
-export([ update_panel/2 ]).



% @doc Runs the actual test.
-spec run_splash_screen_test() -> void().
run_splash_screen_test() ->

	ImagePath = gui_image_test:get_test_main_image_path(),

	test_facilities:display( "Starting the splash test, "
		"by creating an empty main frame followed by said splash screen, "
		"showing the '~ts' image.", [ ImagePath ] ),

	WaitingDurationMs = 2000,

	trace_utils:notice_fmt( "A splash screen displaying the Myriad logo shall "
		"appear, and vanish when the test requests it, after ~ts. "
		"The test will end as soon as the main frame is closed.",
		[ time_utils:duration_to_string( WaitingDurationMs ) ] ),

	gui:start(),

	Pos = auto,
	NoId = undefined,

	MainFrame = gui_frame:create( _MTitle="MyriadGUI Splash Screen Test",
		Pos, _MSize={ 800, 600 }, _MStyles=[ default ], NoId,
		_MaybeParent=undefined ),


	% Rather minimal:
	SplashStyles = [ no_border, no_taskbar, float_on_parent ],

	SplashFrame = gui_frame:create( _STitle="Myriad Splash Screen", Pos,
		_SSize=auto, SplashStyles, NoId, _Parent=MainFrame ),

	SplashPanel = gui_panel:create( SplashFrame ),


	% The backbuffer on which panel content will be drawn:
	BackbufferBitmap = gui_bitmap:create_empty_for( SplashPanel ),

	% The image bitmap, kept to regenerate the backbuffer as needed:
	ImgBitmap = gui_bitmap:create_from( ImagePath ),

	% Initialisation:
	render_scene( SplashPanel, BackbufferBitmap, ImgBitmap ),
	StatusBar = gui_statusbar:create( MainFrame ),

	gui_statusbar:push_text( StatusBar, "Displaying splash screen." ),

	% No need to subscribe to 'onRepaintNeeded' for the panel:
	gui:subscribe_to_events( [ { onWindowClosed, MainFrame },
							   { onResized, SplashPanel } ] ),

	% Renders the GUI:
	gui_frame:show( MainFrame ),

	gui_frame:show( SplashFrame ),

	% Closure:
	MainTestPid = self(),

	DurationMs = 1500,

	trace_utils:debug_fmt( "Will remove splash screen in ~ts.",
						   [ time_utils:duration_to_string( DurationMs ) ] ),

	?myriad_spawn_link( fun() ->
							timer:sleep( DurationMs ),
							MainTestPid ! removeSplash
						end ),

	test_main_loop( #my_test_state{ main_frame=MainFrame,
									splash_frame=SplashFrame,
									splash_panel=SplashPanel,
									backbuffer=BackbufferBitmap,
									image_bitmap=ImgBitmap } ),

	gui:stop().



% The main loop of this test.
-spec test_main_loop( my_test_state() ) -> void().
test_main_loop( TestState=#my_test_state{ main_frame=MainFrame,
										  splash_frame=SplashFrame,
										  %splash_panel=SplashPanel,
										  backbuffer=BackbufferBitmap,
										  image_bitmap=ImgBitmap } ) ->

	receive

		removeSplash ->
			trace_utils:debug( "Removing splash." ),
			gui_frame:destruct( SplashFrame ),
			test_main_loop( TestState#my_test_state{ splash_frame=undefined } );


		% Not subscribed to onRepaintNeeded, so never activated:
		%{ onRepaintNeeded, [ Panel, _Context ] } ->
		%   trace_utils:debug( "Repainting test panel." ),
		%
		%   % No size change, backbuffer still legit:
		%   update_panel( Panel, BackbufferBitmap ),
		%
		%   %trace_utils:debug( "Test panel repainted (blit)." ),
		%
		%   test_main_loop( TestState );


		{ onResized, [ Panel, _PanelId, NewSize, Context ] } ->

			%trace_utils:debug( "Resizing test panel." ),

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Splash panel '~ts' resized to ~p (~ts).",
					[ gui:object_to_string( Panel ), NewSize,
					  gui_event:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( [ NewSize, Context ] ) ),

			% We have to resize the framebuffer first:
			NewBackbufferBitmap = gui_bitmap:create_empty( NewSize ),

			render_scene( Panel, NewBackbufferBitmap, ImgBitmap ),

			gui_bitmap:destruct( BackbufferBitmap ),

			%trace_utils:debug( "Test panel resized (render)." ),

			test_main_loop( TestState#my_test_state{
				backbuffer=NewBackbufferBitmap } );


		{ onWindowClosed, [ MainFrame, _MainFrameId, Context ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt( "Test main frame ~ts has been closed "
					"(~ts), test success.",
					[ gui:object_to_string( MainFrame ),
					  gui_event:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( Context ) ),

			gui_frame:destruct( MainFrame );


		Other ->
			% Extra newline for better separation:
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message:~n ~p.~n", [ Other ] ),
			test_main_loop( TestState )

	end.



% @doc Renders the scene: updates the (bitmap) backbuffer accordingly, and blits
% it to the specified panel.
%
render_scene( TargetPanel, BackbufferBitmap, ImageBitmap ) ->

	% Updates the backbuffer with the stored image:

	% Locks the target surface (device context):
	BackbufferDC = gui_bitmap:lock( BackbufferBitmap ),

	gui_render:clear_device_context( BackbufferDC ),

	gui_bitmap:draw( _Source=ImageBitmap, BackbufferDC, _PosInTarget={15,130} ),

	% Then blits this updated backbuffer to the panel:
	TopLeftPos = {0,0},

	TargetPanelDC = gui_widget:lock( TargetPanel ),

	gui_render:blit( _From=BackbufferDC, _FromPos=TopLeftPos,
		_BlitArea=gui_bitmap:get_size( BackbufferBitmap ),
		_To=TargetPanelDC, _ToPos=TopLeftPos ),

	gui_widget:unlock( TargetPanelDC ),

	gui_bitmap:unlock( BackbufferDC ).



% @doc Blits the current backbuffer bitmap to the specified panel once cleared.
update_panel( TargetPanel, BackbufferBitmap ) ->

	% No need to update the update the framebuffer.

	% Locks the target surface (device context):
	BackbufferDC = gui_bitmap:lock( BackbufferBitmap ),

	% Then blits this updated backbuffer to the panel:
	TopLeftPos = {0,0},

	TargetPanelDC = gui_widget:lock( TargetPanel ),

	gui_render:blit( BackbufferDC, TopLeftPos,
		gui_bitmap:get_size( BackbufferBitmap ), TargetPanelDC, TopLeftPos ),

	gui_widget:unlock( TargetPanelDC ),
	gui_bitmap:unlock( BackbufferDC ).



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the image test, being in batch mode)" );

		false ->
			run_splash_screen_test()

	end,

	test_facilities:stop().
