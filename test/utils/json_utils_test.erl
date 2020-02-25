% Copyright (C) 2015-2020 Olivier Boudeville
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


% Unit tests for the JSON services.
%
% See the json_utils.erl tested module.
%
-module(json_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case json_utils:is_parser_available() of

		true ->
			test_facilities:display( "JSON parser is available." );

		false ->
			test_facilities:display( "Warning: no JSON parser available, "
									 "test stops here." ),
			test_facilities:stop()

	end,

	json_utils:start_parser(),

	TestFile = "example.json",

	test_facilities:display( "Reading test file '~s'.", [ TestFile ] ),

	TestTerm = json_utils:from_json_file( TestFile ),

	json_utils:stop_parser(),

	Type = type_utils:get_type_of( TestTerm ),

	test_facilities:display(
	  "Test file read, type of corresponding term is: '~s'.", [ Type ] ),

	test_facilities:display( "The read term is:~n ~p", [ TestTerm ] ),

	test_facilities:display( "Interpreted type: '~s'.",
					 [ type_utils:interpret_type_of( TestTerm, _Level=infinite ) ] ),

	test_facilities:stop().
