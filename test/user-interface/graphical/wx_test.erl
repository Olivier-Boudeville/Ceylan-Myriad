% Copyright (C) 2023-2025 Olivier Boudeville
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
% Creation date: Friday, August 25, 2023.

-module(wx_test).

-moduledoc """
A minimalistic test to investigate wx.
""".


% For run/0 export and al:
-include("test_facilities.hrl").


% As unrelated to MyriadGUI:
-include_lib("wx/include/wx.hrl").



% Silencing:
-export([ determine_constants/0, test_frame/0, test_window_from_frame/0 ]).



determine_constants() ->

	wxe_util:init_nif( _Silent=false ),

	wxe_util:setup_consts(),

	trace_utils:notice_fmt( "Current value for wxFD_MULTIPLE is ~w.",
							[ ?wxFD_MULTIPLE ] ).



test_frame() ->
	_WxServer = wx:new(),
	%process_flag(trap_exit, true),

	TestWindowOpts = [ { style, ?wxBORDER_NONE } ],

	TestWindow = wxWindow:new( _Parent=wx:null(), _Id=?wxID_ANY,
							   TestWindowOpts ),

	wxWindow:show( TestWindow ),

	%timer:sleep( 3000 ),

	TestFrameOpts = [ { style, ?wxBORDER_NONE } ],

	TestFrame = wxFrame:new( wx:null(), ?wxID_ANY,
							 "Test frame", TestFrameOpts ),

	wxFrame:show( TestFrame ),

	timer:sleep( 1000 ),

	%% receive

	%%	Any ->
	%%		trace_utils:debug_fmt( "Test received ~w.", [ Any ] )
	%%		% Terminates.

	%% end.

	terminates.


test_window_from_frame() ->
	_WxServer = wx:new(),
	process_flag(trap_exit, true),

	%TestWindowOpts = [ { style, ?wxBORDER_NONE } ],
	TestWindowOpts = [],

	% Shows that a mere window may not even appear onscreen; use a frame
	% instead.
	%
	TestWindow = wxWindow:new( _Parent=wx:null(), _Id=?wxID_ANY,
							   TestWindowOpts ),

	wxWindow:show( TestWindow ),

	receive

		Any ->
			trace_utils:debug_fmt( "Test received ~w.", [ Any ] )
			% Terminates.

	end.



-doc "Executes the actual test.".
-spec run_gui_test() -> void().
run_gui_test() ->

	test_facilities:display( "~nStarting the wx test." ),

	% Either of the next two functions shall be commented-out, as otherwise the
	% first would load the wx NIF, and the second as well (hence: "NIF library
	% already loaded):

	% Loads the wx NIF:
	%determine_constants(),

	test_frame(),

	%test_window_from_frame(),

	test_facilities:display( "End of the wx test." ).



-doc "Runs the test.".
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the MyriadGUI test, being in batch mode)" );

		false ->
			run_gui_test()

	end,

	test_facilities:stop().
