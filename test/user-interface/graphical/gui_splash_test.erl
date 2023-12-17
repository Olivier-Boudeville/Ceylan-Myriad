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



% For run/0 export and al:
-include("test_facilities.hrl").


% For myriad_spawn_link:
-include_lib("myriad/include/spawn_utils.hrl").


% Shorthands:

-type frame() :: gui:frame().

-type splash_info() :: gui_splash:splash_info().
-type splash_panel() :: gui_splash:splash_panel().


% State of the test application, kept and updated by its main loop.
-record( my_test_state, {

	main_frame :: frame(),

	% Information for any current splash screen:
	splash_info :: maybe( splash_info() ),

	% Allows easy pattern-matching of events:
	splash_panel :: maybe( splash_panel() ) } ).

-type my_test_state() :: #my_test_state{}.




% @doc Runs the actual test.
-spec run_splash_screen_test() -> void().
run_splash_screen_test() ->

	% This test just waits for a fixed duration:
	WaitingDurationMs = 200,

	trace_utils:notice_fmt( "A basic splash screen displaying the Myriad logo "
		"shall appear, and vanish when the test requests it, "
		"after ~ts. Then a dynamic, more complex splash screen shall appear "
		"(with icon, texts, etc.) and disappear in turn. "
		"The test will end as soon as the main frame is closed.",
		[ time_utils:duration_to_string( WaitingDurationMs ) ] ),

	gui:start(),

	Pos = auto,
	NoId = undefined,

	MainFrame = gui_frame:create( _MTitle="MyriadGUI Splash Screen Test",
		Pos, _MSize={ 800, 600 }, _MStyles=[ default ], NoId,
		_MaybeParent=undefined ),

	% To create a contrast between the frame background and the splash screen:
	MainPanel = gui_panel:create( MainFrame ),

	gui_widget:set_background_color( MainPanel, _Color=bisque ),

	ImgPath = file_utils:join( "..", test_facilities:get_myriad_logo_path() ),

	BasicSplashInfo = gui_splash:create_basic( ImgPath, _ScaleFactor=0.5,
											   _SplashParent=MainFrame ),

	StatusBar = gui_statusbar:create( MainFrame ),

	gui_statusbar:push_text( StatusBar, "Displaying splash screen." ),

	% Splash already subscribed by itself:
	gui:subscribe_to_events( [ { onWindowClosed, MainFrame } ] ),

	% Renders the GUI:
	gui_frame:show( MainFrame ),

	% Must be shown after the main frame is shown, otherwise will not be
	% centered in it, but on the whole screen, which is not desirable:
	%
	gui_splash:show( BasicSplashInfo ),

	% Closure:
	MainTestPid = self(),

	trace_utils:debug_fmt( "Will decide to remove splash screen in ~ts.",
		[ time_utils:duration_to_string( WaitingDurationMs ) ] ),

	?myriad_spawn_link( fun() ->
							timer:sleep( WaitingDurationMs ),
							MainTestPid ! removeBasicSplash,

							timer:sleep( WaitingDurationMs ),
							MainTestPid ! createDynamicSplash,

							%timer:sleep( WaitingDurationMs ),
							timer:sleep( 25000 ),
							MainTestPid ! removeDynamicSplash,

							MainTestPid ! quit

						end ),

	test_main_loop( #my_test_state{
		main_frame=MainFrame,
		splash_info=BasicSplashInfo,

		% Needed for properly pattern-matching events afterwards:
		splash_panel=gui_splash:get_panel( BasicSplashInfo ) } ).




% The main loop of this test.
-spec test_main_loop( my_test_state() ) -> void().
test_main_loop( TestState=#my_test_state{ main_frame=MainFrame,
										  splash_info=SplashInfo,
										  splash_panel=SplashPanel } ) ->

	receive

		% First the application-specific events of interest:

		removeBasicSplash ->
			trace_utils:debug( "Removing basic splash screen." ),

			gui_splash:destruct( SplashInfo ),

			test_main_loop( TestState#my_test_state{ splash_info=undefined,
													 splash_panel=undefined } );


		createDynamicSplash ->

			trace_utils:debug( "Adding dynamic splash screen." ),

			IconImgPath =
				file_utils:join( "..", test_facilities:get_myriad_icon_path() ),

			TitleStr = "Foobar",

			VersionStr = "v1.0.17",

			DescStr = "Foobar is a Frobnicator with twin acceleration beams",

			URLStr = "www.foobar.org",

			BackgroundColor = red, %yellow,%lightblue,

			MainImgPath =
				file_utils:join( "..", test_facilities:get_myriad_logo_path() ),

			GeneralInfoStr = "Foobar comes with absolutely no warranty, "
				"but is completely free for any kind of use "
				"(including commercial).",

			CopyrightStr = "Copyright (C) 2022-2023 John Doe, "
				"James Bond and Others",

			DynamicSplashInfo = gui_splash:create_dynamic( IconImgPath,
				TitleStr, VersionStr, DescStr, URLStr, BackgroundColor,
				MainImgPath, GeneralInfoStr, CopyrightStr,
				_SplashParent=MainFrame ),

			gui_splash:show( DynamicSplashInfo ),

			test_main_loop( TestState#my_test_state{
				splash_info=DynamicSplashInfo,
				splash_panel=gui_splash:get_panel( DynamicSplashInfo ) } );


		removeDynamicSplash ->
			trace_utils:debug( "Removing dynamic splash screen." ),

			gui_splash:destruct( SplashInfo ),

			test_main_loop( TestState#my_test_state{ splash_info=undefined,
													 splash_panel=undefined } );


		quit ->
			trace_utils:debug( "Quitting test." ),
			ok;


		% Then the events this test is subscribed to:

		{ onWindowClosed, [ MainFrame, _MainFrameId, Context ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt( "Test main frame ~ts has been closed "
					"(~ts), test success.",
					[ gui:object_to_string( MainFrame ),
					  gui_event:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( Context ) ),

			gui_frame:destruct( MainFrame ),

			gui:stop();


		% The splash-related events to manage:

		{ onRepaintNeeded, [ SplashPanel, _SplashPanelId, _Context ] } ->

			% Too verbose:
			%trace_utils:debug_fmt( "Repainting splash panel ~w.",
			%                       [ SplashPanel ] ),

			% Implies no state change:
			gui_splash:on_repaint_needed( SplashPanel, SplashInfo ),

			test_main_loop( TestState );


		{ onResized, [ SplashPanel, _SplashPanelId, NewSize, Context ] } ->

			% May actually be resized to the exact same size:
			trace_utils:debug_fmt( "Resizing splash panel ~w from ~w to ~w "
				"(~ts).",
				[ SplashPanel, gui_panel:get_size( SplashPanel ), NewSize,
				  gui_event:context_to_string( Context ) ] ),

			NewSplashInfo =
				gui_splash:on_resized( SplashPanel, NewSize, SplashInfo ),

			NewTestState = TestState#my_test_state{ splash_info=NewSplashInfo },

			test_main_loop( NewTestState );


		Other ->
			% Extra newline for better separation:
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message:~n ~p.~n", [ Other ] ),
			test_main_loop( TestState )

	end.



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the splash test, being in batch mode)" );

		false ->
			run_splash_screen_test()

	end,

	test_facilities:stop().
