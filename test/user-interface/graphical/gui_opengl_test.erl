% Copyright (C) 2003-2021 Olivier Boudeville
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


% @doc Testing the <b>OpenGL support</b>.
%
% See the gui_opengl.erl tested module.
%
-module(gui_opengl_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-record( my_test_state, {

	% Test-specific state:
	test_main_frame :: gui:frame() }).

-type my_test_state() :: #my_test_state{}.



-spec run_test_gui() -> void().
run_test_gui() ->

	test_facilities:display( "~nStarting the test of OpenGL support." ),

	test_facilities:display( "Checking whether OpenGL hardware acceleration is "
		"available: ~ts", [ gui_opengl:is_hardware_accelerated() ] ),

	gui:start(),

	%gui:set_debug_level( [ calls, life_cycle ] ),

	MainFrame = gui:create_frame( "GUI OpenGL Test" ),

	StatusBar = gui:create_status_bar( MainFrame ),

	gui:push_status_text( "Testing OpenGL now.", StatusBar ),

	gui:show( MainFrame ),

	gui:subscribe_to_events( { onWindowClosed, MainFrame } ),

	InitialState = #my_test_state{ test_main_frame=MainFrame },

	gui_main_loop( InitialState ),

	gui:stop().



% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages.
%
-spec gui_main_loop( my_test_state() ) -> void().
gui_main_loop( State=#my_test_state{ test_main_frame=MainFrame } ) ->

	receive

		{ onWindowClosed, [ MainFrame, _Context ] } ->
			trace_utils:info( "Main frame closed, test success." ),
			gui:destruct_window( MainFrame );

		OtherEvent ->
			trace_utils:warning_fmt( "Test ignored event '~p'.",
									 [ OtherEvent ] ),

			gui_main_loop( State )

	end.





% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display( "(not running the OpenGL test, "
									 "being in batch mode)" );

		false ->
			run_test_gui()

	end,

	test_facilities:stop().
