% Copyright (C) 2024-2024 Olivier Boudeville
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
% Creation date: Wednesday, August 28, 2024.

-module(shell_utils_test).

-moduledoc """
Unit tests for the **shell-related** facilities.

See the shell_utils.erl tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	ShellPid = shell_utils:start_link_shell(),

	FirstRes = shell_utils:execute_command( "A=1.", ShellPid ),

	test_facilities:display( "First assignment result: ~p.", [ FirstRes ] ),
	FirstRes = 1,


	SecondRes = shell_utils:execute_command( "B=2.", ShellPid ),

	test_facilities:display( "Second assignment result: ~p.", [ SecondRes ] ),
	SecondRes = 2,


	ThirdRes = shell_utils:execute_command( "A+B.", ShellPid ),

	test_facilities:display( "Addition result: ~p.", [ ThirdRes ] ),
	ThirdRes = 3,


	LRes = shell_utils:execute_command( "L = [3, 2, 1].", ShellPid ),
	test_facilities:display( "List assignment result: ~p.", [ LRes ] ),
	LRes = [ 3, 2, 1 ],

	SortRes = shell_utils:execute_command( "lists:sort(L).", ShellPid ),
	test_facilities:display( "Sorting result: ~p.", [ SortRes ] ),
	SortRes = [ 1, 2, 3 ],

	ShellPid ! { terminateSynch, self() },

	receive

		onShellTerminated ->
			ok

	end,

	test_facilities:stop().
