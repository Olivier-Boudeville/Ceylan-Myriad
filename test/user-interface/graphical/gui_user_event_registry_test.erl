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
		"~nStarting the actual user-event registry test of MyriadGUI, "
		"from ~w. ", [ self() ] ),

	QuitButtonId = quit_button_id,

	trace_utils:notice( "An empty, resizable test frame shall appear, "
		"with two panels; the left one listens for keypresses whereas "
		"the second has a button; the test will end as soon as the "
		"frame is closed, the button is clicked, "
		"or 'q' is hit whereas the left panel has the focus "
		"(which is the case initially)." ),

	gui:start(),

	TestFrame = gui:create_frame( "This is the single and only test frame" ),

	% A frame cannot handle key events, so we create a panel within it; moreover
	% as soon as a panel is the parent of a button, it will not receive key
	% press events (even if having the focus), so we create a panel for key
	% presses and one for buttons:

	KeyPanel = gui:create_panel( _KPanelParent=TestFrame ),
	gui:set_background_color( KeyPanel, green ),
	gui:set_tooltip( KeyPanel, "Panel collecting key presses." ),

	ButtonPanel = gui:create_panel( _BPanelParent=TestFrame ),
	gui:set_background_color( ButtonPanel, red ),
	gui:set_tooltip( ButtonPanel, "Panel for buttons." ),

	MainSizer = gui:create_sizer( _Orientation=horizontal ),

	% Grows with the window:
	gui:add_to_sizer( MainSizer, KeyPanel,
					  [ { proportion, 2 }, expand_fully ] ),

	% Constant width:
	gui:add_to_sizer( MainSizer, ButtonPanel,
					  [ { proportion, 1 }, expand_fully ] ),

	gui:set_sizer( TestFrame, MainSizer ),

	QuitButton = gui:create_button( _Label="Quit by button click!",
		_Id=QuitButtonId, _ButtonParent=ButtonPanel ),

	EventsOfInterest = [ { onButtonClicked, QuitButton },
						 { onKeyPressed, KeyPanel },
						 { onKeyPressed, ButtonPanel },
						 { onWindowClosed, TestFrame } ],

	gui:subscribe_to_events( EventsOfInterest ),


	% The different ways of quitting:
	QuitEvents = [ { button_clicked, QuitButtonId },
				   { keycode_pressed, ?MYR_K_q },
				   window_closed ],

	UserEventSpecs = [ { quit_requested, QuitEvents } ],

	UserEventRegistry = gui_event:create_user_event_registry( UserEventSpecs ),

	% Focus needed to receive events:
	gui:set_focus( KeyPanel ),

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
				[ gui_event:gui_event_to_string( BaseEvent ) ] ),

			% A frame is a window:
			gui:destruct_window( TestFrame ),

			trace_utils:info( "Test frame closed, test success." ),

			gui:stop();


		Other ->
			throw( { unexpected_application_event_pair, Other } )

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
