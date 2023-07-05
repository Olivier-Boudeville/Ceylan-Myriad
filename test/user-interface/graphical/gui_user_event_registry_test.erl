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
% Creation date: Wednesday, July 5, 2023.


% @doc Test showcasing the use of the <b>user event registry</b>, to provide the
% user a generic main event loop.
%
-module(gui_user_event_registry_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% For the key define:
-include_lib("myriad/include/myriad_gui.hrl").


% @doc Actual execution of the test.
-spec run_test_gui() -> void().
run_test_gui() ->

	test_facilities:display(
		"~nStarting the actual user-event registery test of MyriadGUI, "
		"from ~w. ", [ self() ] ),

	QuitButtonId = quit_button_id,



	trace_utils:notice( "An empty, resizable test frame shall appear; "
						"the test will end as soon as it is closed." ),

	gui:start(),

	TestFrame = gui:create_frame( "This is the single and only test frame" ),

	% A frame cannot handle key events, so we create a panel within it:
	TestPanel = gui:create_panel( _PanelParent=TestFrame ),

	QuitButton = gui:create_button( _Label="Quit by button click!",
		_Id=QuitButtonId, _ButtonParent=TestPanel ),

	EventsOfInterest = [ { onButtonClicked, QuitButton },
						 { onKeyPressed, TestPanel },
						 { onWindowClosed, TestFrame } ],
TODO
	gui:subscribe_to_events( EventsOfInterest ),

	% Different ways of quitting:
	QuitEvents = [ { button_clicked, QuitButtonId },
				   { keycode_pressed, ?MYR_K_q },
				   window_closed ],

	UserEventSpecs = [ { quit_requested, QuitEvents } ],

	UserEventRegistry = gui_event:create_user_event_registry( UserEventSpecs ),

	gui:show( TestFrame ),

	trace_utils:notice( "Please click the quit button, hit 'q' or "
						"close the frame to end this test." ),

	% Not even a main loop here, just using the MyriadGUI one based on
	% user-events, and waiting for a single of them:
	%
	case gui_event:get_application_event( UserEventRegistry ) of

		{ quit_requested, BaseEvent } ->
			trace_utils:notice_fmt( "Quitting, based on the following base "
				"user event:~n ~ts",
				[ gui_event:gui_event_to_string( BaseEvent ) ] )

	end.



% @doc Runs the test.
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
