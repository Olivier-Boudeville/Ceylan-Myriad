% Copyright (C) 2016-2018 Olivier Boudeville
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


% Unit tests for the ui toolbox.
%
% See the ui.erl tested module.
%
-module(ui_test).


% For run/0 export and al:
-include("test_facilities.hrl").



% The actual test:
run_test_ui() ->

	UIState = ui:start(),

	ui:display( "My text to display!", UIState ),

	ui:display_error( "My error to display!", UIState ),

	ui:display_numbered_list( "My label for following items:",
							  [ "Foo", "Bar", "Baz" ], UIState ),

	FirstChoice = ui:choose_designated_item(
					[ { choice_1, "Choice 1" },
					  { choice_2, "Choice 2" },
					  { choice_3, "Choice 3" },
					  { choice_4, "Choice 4" } ], UIState ),

	ui:display( "First choice has been ~p", [ FirstChoice ], UIState ),

	ui:add_separation( UIState ),

	SecondChoice = ui:choose_numbered_item_with_default(
					 "This is my label; just choose below:",
					 [ "Choice 1", "Choice 2", "Choice 3", "Choice 4" ],
					 2, UIState ),


	ui:display( "Second choice has been ~p", [ SecondChoice ], UIState ),


	ui:trace( "My UI trace", UIState ),

	ui:stop( UIState ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display( "(not running the MyriadGUI test, "
									 "being in batch mode)" );

		false ->
			run_test_ui()

	end,


	test_facilities:stop().
