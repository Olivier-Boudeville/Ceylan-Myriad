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


% Testing the OpenGL support.
%
% See the gui_opengl.erl tested module.
%
-module(gui_opengl_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% For the #wx record:
-include_lib("wx/include/wx.hrl").


run_test_gui() ->

	test_facilities:display( "~nStarting the test of OpenGL support." ),

	InitialState = gui:start(),

	%gui:set_debug_level( [ calls, life_cycle ] ),

	MainFrame = gui:create_frame( "GUI OpenGL Test" ),

	_StatusBar = gui:create_status_bar( MainFrame ),

	gui:show( MainFrame ),

	SubscribedEvents = [
		  { MainFrame, onWindowClosed } ],

	LoopState = gui:enter_main_loop( InitialState,
									 SubscribedEvents ),

	receive

		{ onWindowClosed, [ MainFrame, _Id, _UserData, _WxEvent ] } ->
			gui:destruct_window( MainFrame ),
			trace_utils:trace( "Main frame closed, test success." ),
			gui:stop( LoopState )

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
