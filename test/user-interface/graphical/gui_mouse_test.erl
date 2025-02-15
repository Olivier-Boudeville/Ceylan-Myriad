% Copyright (C) 2022-2025 Olivier Boudeville
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
% Creation date: Saturday, May 28, 2022.

-module(gui_mouse_test).

-moduledoc """
Testing of the **MyriadGUI mouse support**.

See the gui_mouse.erl tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").


-doc """
Here the main loop just has to remember the frame whose closing is awaited for.
""".
-type my_test_state() :: gui:frame().



-doc "Actual execution of the test.".
-spec run_test_gui() -> void().
run_test_gui() ->

	test_facilities:display(
		"~nStarting the actual mouse test of MyriadGUI, from ~w. ",
		[ self() ] ),

	trace_utils:notice( "An empty, resizable test frame shall appear; "
						"the test will end as soon as it is closed." ),

	gui:start(),

	TestFrame = gui_frame:create( "This is the single and only test frame, "
								  "for mouse testing" ),

	MouseEventTypes = [
		onMouseMoved,

		onMouseLeftButtonPressed, onMouseLeftButtonReleased,
		onMouseLeftButtonDoubleClicked,

		onMouseMiddleButtonPressed, onMouseMiddleButtonReleased,
		onMouseMiddleButtonDoubleClicked,

		onMouseRightButtonPressed, onMouseRightButtonReleased,
		onMouseRightButtonDoubleClicked,

		onMouseFourthButtonPressed, onMouseFourthButtonReleased,
		onMouseFourthButtonDoubleClicked,

		onMouseFifthButtonPressed, onMouseFifthButtonReleased,
		onMouseFifthButtonDoubleClicked,

		onMouseWheelScrolled,

		onMouseEnteredWidget, onMouseLeftWidget ],

	gui:subscribe_to_events(
		{ [ onWindowClosed | MouseEventTypes ], TestFrame } ),

	trace_utils:notice( "Please close the frame to end this test." ),

	gui_frame:show( TestFrame ),

	test_main_loop( TestFrame ).



-doc """
A very simple main loop, whose actual state is simply the GUI object
corresponding to the frame that shall be closed to stop the test.
""".
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( TestFrame ) ->

	receive

		% Would be far more too numerous for the console:
		{ onMouseMoved, [ TestFrame, _TestFrameId, _EventContext ] } ->
			test_main_loop( TestFrame );

		{ onWindowClosed, [ TestFrame, _TestFrameId, EventContext ] } ->
			trace_utils:info_fmt( "Test frame '~ts' closed (~ts).",
				[ gui:object_to_string( TestFrame ),
				  gui_event:context_to_string( EventContext ) ] ),

			gui_frame:destruct( TestFrame ),

			trace_utils:info( "Test frame closed, test success." ),

			gui:stop();

		% We can see button events, entered/left window ones, etc.:
		Other ->
			trace_utils:notice_fmt( "Test main loop received "
				"(and did not specifically managed) the following "
				"message:~n  ~p.", [ Other ] ),
			test_main_loop( TestFrame )

	end.



-doc "Runs the test.".
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display( "(not running the MyriadGUI mouse "
									 "test, being in batch mode)" );

		false ->
			run_test_gui()

	end,

	test_facilities:stop().
