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
% Creation date: Sunday, December 24, 2017.

-module(process_utils_test).

-moduledoc """
Unit tests of facilities regarding **(Erlang) processes**.

See the `process_utils` tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    PeriodMs = 100,

    TestedPid = spawn_link(

        fun() ->
            MyLabel = text_utils:format( "Tested process ~w", [ self() ] ),
            process_utils:set_label( MyLabel ),

            % Check:
            MyLabel = process_utils:get_label( self() ),

            % No need for a Y-combinator with (recursive) named funs:
            fun Ticker() ->
                io:format( "Hello, I am ~ts.~n",
                    [ process_utils:get_label( self() ) ] ),

                % Fill its own mailbox:
                self() ! hello,

                timer:sleep( PeriodMs ),

                Ticker()

            end()

        end),

    test_facilities:display( "Setting intentionally very low limits in terms "
        "of length of message queue and maximum reduction increase." ),

    TestedDesc = "Tested process",

    % Low as well:
    SamplingPeriodMs = 250,
    MsgThreshold = 5,
    ReducThreshold = 10,

    MsgMonitPid = process_utils:spawn_message_queue_monitor( TestedPid,
        TestedDesc, MsgThreshold, SamplingPeriodMs ),

    ReducMonitPid = process_utils:spawn_reduction_monitor( TestedPid,
        TestedDesc, ReducThreshold, SamplingPeriodMs ),

    % Monitoring processes flagged here as abnormal:
    OverallMonit = process_utils:spawn_overall_monitor( MsgThreshold,
        ReducThreshold, SamplingPeriodMs ),

    % Wait a bit:
    timer:sleep( 4000 ),

    [ MonitPid ! terminate
      || MonitPid <- [ OverallMonit, ReducMonitPid, MsgMonitPid ] ],

    test_facilities:stop().
