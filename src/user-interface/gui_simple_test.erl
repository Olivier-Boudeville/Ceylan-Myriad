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


% Simple unit tests for the GUI toolbox: creates a few frames, enter a main
% loops, and exits when the fourth frame is closed by the user.
%
% See the gui.erl tested module.
%
-module(gui_simple_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Here the main loop just has to remember the frame whose closing is awaited
% for:
%
-type my_test_state() :: gui:frame().


run_test_gui() ->

	test_facilities:display( "~nStarting the actual simple test, from ~w.",
							 [ self() ] ),


	InitialGUIState = gui:start(),

	%gui:set_debug_level( [ calls, life_cycle ] ),

	FirstFrame = gui:create_frame( "This is the first frame" ),

	SecondFrame = gui:create_frame( "This is the second frame" ),

	ThirdFrame = gui:create_frame( "This is the third frame",
					   _Position={ 50, 10 }, _Size={ 150, 200 },
					   _Style=[ default ] ),

	FourthFrame = gui:create_frame( "This is the fourth frame" ),

	trace_utils:info( "Please close the fourth frame to end this test." ),

	Frames = [ FirstFrame, SecondFrame, ThirdFrame, FourthFrame ],

	gui:show( Frames ),

	% As a result, closing the third frame will not be known from here:
	SubscribedFrames = [ FirstFrame, SecondFrame, FourthFrame ],

	SubscribedEvents = [ { onWindowClosed, SubscribedFrames } ],

	ReadyGUIState = gui:handle_events( InitialGUIState, SubscribedEvents ),

	test_main_loop( FourthFrame, ReadyGUIState ).




% A very simple main loop, whose actual state is simply the GUI object
% corresponding to the frame that shall be closed to stop the test
% (i.e. CloseFrame).
%
-spec test_main_loop( my_test_state(), gui:gui_state() ) -> no_return().
test_main_loop( CloseFrame, GUIState ) ->

	trace_utils:trace( "Test main loop running..." ),

	receive

		{ onWindowClosed, [ CloseFrame, Context ] } ->

			trace_utils:trace_fmt( "Closing frame ~w has been, well, closed "
								   "(context: ~p), test success.",
								   [ CloseFrame, Context ] ),

			gui:destruct_window( CloseFrame ),

			gui:stop( GUIState );


		{ onWindowClosed, [ AnyFrame, Context ] } ->
			trace_utils:trace_fmt( "Frame ~w closed (context: ~p).",
								   [ AnyFrame, Context ] ),
			gui:destruct_window( AnyFrame ),
			test_main_loop( CloseFrame, GUIState )

	end.



% Runs the test.
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display( "(not running the GUI test, "
									 "being in batch mode)" );

		false ->
			run_test_gui()

	end,

	test_facilities:stop().
