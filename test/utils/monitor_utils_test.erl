% Copyright (C) 2026-2026 Olivier Boudeville
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
% Creation date: Sunday, September 6, 2026.

-module(monitor_utils_test).

-moduledoc """
Unit tests for the management of **monitors**, typically of processes.

See the `monitor_utils` tested module.
""".


% For run/0 export and al:
-include("test_facilities.hrl").


-define( wait_delay, 100 ).

emulate_target_process( _Action=terminate_normally ) ->
    trace_utils:debug_fmt( "Test target process ~w terminating normally.",
                           [ self() ] );

emulate_target_process( _Action=crash ) ->
    basic_utils:crash();

emulate_target_process( _Action=throw ) ->
    throw( target_process_throwing ).



test_process_monitor() ->

    test_facilities:display( "Monitoring the current (test) process, ~w.",
                             [ self() ] ),

    SelfMonPid = monitor_utils:monitor_self(),


    % To avoid too much interleaving in the outputs:
    timer:sleep( ?wait_delay ),

    test_facilities:display(
        "~nMonitoring a process that will terminate normally." ),

    NormalPid = spawn(
        fun() -> emulate_target_process( _Action=terminate_normally ) end ),

    _NormalMonPid = monitor_utils:monitor_process( NormalPid ),


    % This should result in a warning trace, a console trace notification of
    % 'DOWN' (normal), and an error report:

    timer:sleep( ?wait_delay ),

    test_facilities:display( "~nMonitoring a process that will crash." ),

    CrashPid = spawn(
        fun() -> emulate_target_process( _Action=crash ) end ),

    _CrashMonPid = monitor_utils:monitor_process( CrashPid ),


    % This results in an error report nocatch) and a console trace notification
    % of 'DOWN' (noproc):

    timer:sleep( ?wait_delay ),
    test_facilities:display( "~nMonitoring a process that will throw." ),

    ThrowPid = spawn(
        fun() -> emulate_target_process( _Action=throw ) end ),

    _ThrowMonPid = monitor_utils:monitor_process( ThrowPid ),


    % Same consequences as for throw, but with function_clause instead:
    timer:sleep( ?wait_delay ),
    test_facilities:display(
        "~nMonitoring a process that will fail to match any clause." ),

    NonMatchingPid = spawn(
        fun() -> emulate_target_process( _Action=undefined ) end ),

    _NonMatchingMonPid = monitor_utils:monitor_process( NonMatchingPid ),


    timer:sleep( ?wait_delay ),

    % Otherwise would come too late for the test as well:
    SelfMonPid ! terminate.



-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    test_facilities:display( "Note that this test *is* expected to display "
        "various warnings, errors and error reports." ),

    test_process_monitor(),

    test_facilities:stop().
