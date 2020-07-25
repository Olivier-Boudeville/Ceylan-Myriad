% Copyright (C) 2020-2020 Olivier Boudeville
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
% Creation date: Wednesday, May 20, 2020.


% Unit tests for the shell_utils toolbox.
%
% See the shell_utils.erl tested module.
%
-module(shell_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Note:
%
% One can test the support of command-line arguments with, for example:
%
% make shell_utils_run CMD_LINE_OPT="aa -my-first-opt u v bb -my-other-opt cc"



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	ArgTable = shell_utils:get_argument_table(),

	test_facilities:display( "Obtained following argument table: ~s",
		[ shell_utils:argument_table_to_string( ArgTable ) ] ),

	TargetOption = 'pz',

	{ Values, RemainingArguments } =
		shell_utils:extract_command_argument( TargetOption ),

	test_facilities:display( "Knowing the actual command-line arguments were:~n"
		"~p~nfor option '~s', we extracted following value(s):~n~p~nand "
		"got the rest of the arguments:~n~p",
		[ init:get_arguments(), TargetOption, Values, RemainingArguments ] ),

	test_facilities:stop().
