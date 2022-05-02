% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Friday, April 22, 2022.


% @doc Unit tests for the management of <b>menus</b>, in menu bars and popup
% menus.
%
-module(gui_menu_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Shorthands:

-type frame() :: gui:frame().
-type menu() :: gui:menu().


-type my_test_state() :: { frame(), menu() }.
% Here the main loop just has to remember the popup menu to activate in case of
% right click on the main frame, and this frame whose closing is awaited for.


-spec create_test_menu_bar( frame() ) -> menu().
create_test_menu_bar( Frame ) ->

	MenuBar = gui:create_menu_bar(),

	% First drop-down:
	FirstMenu = gui:create_menu(),

	[ gui:add_item( FirstMenu, N,
					_Label=text_utils:format( "I am &~ts", [ N ] ) )
								|| N <- gui:get_standard_item_names() ],

	gui:add_menu( MenuBar, FirstMenu, "&First menu" ),

	gui:set_menu_bar( Frame, MenuBar ),

	FirstMenu.



% @doc Executes the actual test.
-spec run_gui_test() -> void().
run_gui_test() ->

	test_facilities:display( "~nStarting the menu test; use the menu bar "
		"and/or right-click on the frame to obtain a popup menu." ),

	gui:start(),

	Frame = gui:create_frame( "This is the overall frame for menu testing" ),

	MainMenu = gui:create_menu(),

	_A = gui:add_item( MainMenu, _Label="Item A" ),
	_B = gui:add_item( MainMenu, _Id=undefined, "Item B" ),

	FirstSubMenu = gui:create_menu(),

	% Defining here named identifiers rathen than auto-set numerical ones:
	_C = gui:append_submenu( MainMenu, item_c, "Item C", FirstSubMenu ),

	SecondSubMenu = gui:create_menu(),

	_D = gui:append_submenu( MainMenu, item_d, "Item D", SecondSubMenu,
							 "I am D's help" ),


	_E = gui:add_checkable_item( FirstSubMenu, _EId=item_e, "Item E" ),
	_F = gui:add_checkable_item( FirstSubMenu, _FId=item_f, "Item F",
								"I am F's help" ),
	_G = gui:add_checkable_item( FirstSubMenu, _GId=item_g, "Item G" ),

	% E let as it is.
	gui:set_checkable_menu_item( FirstSubMenu, item_f, _SetAsChecked=true ),
	gui:set_checkable_menu_item( FirstSubMenu, item_g, false ),

	_H = gui:add_radio_item( MainMenu, undefined, "Item H" ),
	_I = gui:add_radio_item( MainMenu, undefined, "Item I", "I am I's help" ),

	_J = gui:add_separator( MainMenu ),

	gui:set_menu_item_status( MainMenu, item_c, enabled ),
	gui:set_menu_item_status( MainMenu, item_d, disabled ),

	FileMenu = create_test_menu_bar( Frame ),

	AllMenus = [ MainMenu, FirstSubMenu, SecondSubMenu, FileMenu ],

	gui:show( Frame ),

	gui:subscribe_to_events( [
		{ [ onMouseRightButtonReleased, onWindowClosed ], Frame },
		{ onMenuItemSelected, [ Frame | AllMenus ] } ] ),

	test_main_loop( _InitialState={ Frame, MainMenu } ).




% @doc A very simple main loop, whose actual state is simply the GUI object
% corresponding to the frame that shall be closed to stop the test
% (i.e. CloseFrame).
%
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( State={ Frame, MainMenu } ) ->

	trace_utils:info( "Test main loop running..." ),

	receive

		{ onMouseRightButtonReleased, [ Frame, _FrameId, _Context ] } ->
			gui:activate_popup_menu( Frame, MainMenu ),
			test_main_loop( State );

		{ onMenuItemSelected, [ _Menu, _ItemId=exit_menu_item, _Context ] } ->
			trace_utils:info( "Exit menu item selected, stopping test." );

		{ onMenuItemSelected, [ _Menu, ItemId, _Context ] } ->
			%trace_utils:debug_fmt( "Received for menu ~w: ~w",
			%                       [  Menu, Context ] ),
			trace_utils:info_fmt( "Menu item '~w' selected.", [ ItemId ] ),
			test_main_loop( State );

		{ onWindowClosed, [ Frame, _FrameId, _Context ] } ->
			trace_utils:info( "Main frame has been closed; test success." ),
			gui:destruct_window( Frame ),
			gui:stop();

		Other ->
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message: ~p.", [ Other ] ),
			test_main_loop( State )

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
			run_gui_test()

	end,

	test_facilities:stop().
