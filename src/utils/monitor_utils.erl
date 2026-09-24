% Copyright (C) 2007-2026 Olivier Boudeville
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
% Creation date: July 1, 2007.

-module(monitor_utils).

-moduledoc """
Gathering of various facilities related to the **monitoring of processes, ports,
time changes or nodes**, based on **Erlang monitors**.

See `monitor_utils_test.erl` for the corresponding test.

See also the `process_utils` module for more process-level monotoring (not based
on Erlang monitors), for example the `spawn_message_queue_monitor/*` functions.
""".



-doc "Not allowed to be shortened into a local `reference/0` type.".
-type monitor_ref() :: reference().


-doc "The types of language elements that can be monitored.".
-type monitored_element_type() :: 'process' | 'port' | 'clock'.



% (not exported yet by the 'erlang' module)
% That is: pid() | registered_process_identifier().
% -type monitored_process() :: erlang:monitor_process_identifier().
-doc "Designates an Erlang process being monitored.".
-type monitored_process() :: pid() | erlang:registered_process_identifier().


-doc "The PID of an ad hoc process in charge of monitoring a target process.".
-type proc_monitor_pid() :: pid().


-doc """
Information returned with monitoring a target process: the monitor itself, and
the PID of the ad hoc process relying on it.
""".
% A bit like a swapped version of the result of spawn_monitor/*:
-type proc_monitor_info() :: { monitor_ref(), proc_monitor_pid() }.




% (not exported yet by the 'erlang' module)
%-type monitored_port() :: erlang:monitor_port_identifier().
-doc "Designates an Erlang port being monitored.".
-type monitored_port() :: port() | registered_name().



-doc "To monitor time offsets.".
-type monitored_clock() :: 'clock_service'.


-doc "An actual element being monitored.".
-type monitored_element() :: monitored_process() | monitored_port()
                           | monitored_clock().



-doc """
This information may be:

- the exit reason of the process

- or `noproc` (process or port did not exist at the time of monitor creation)

- or `noconnection` (no connection to the node where the monitored process
resides)
""".
-type monitor_info() :: basic_utils:exit_reason() | 'noproc' | 'noconnection'.



-doc "See `net_kernel:monitor_nodes/2` for more information.".
-type monitor_node_info() :: list_table:list_table().



-doc "Options to monitor a node.".
-type monitor_node_option() :: { 'node_type', net_utils:node_type() }
                             | 'nodedown_reason'.


-export_type([ monitor_ref/0, monitored_element_type/0, monitored_process/0,
               proc_monitor_pid/0, proc_monitor_info/0,
               monitored_port/0, monitored_clock/0,
               monitored_element/0, monitor_info/0, monitor_node_info/0,
               monitor_node_option/0 ]).


% For nodes:
-export([ monitor_nodes/1, monitor_nodes/2 ]).

% For processes:
-export([ monitor_self/0, monitor_process/1 ]).


% For myriad_spawn:
-include("spawn_utils.hrl").


% Type shorthands:

-type registered_name() :: naming_utils:registration_name().



% Node monitoring section.


-doc """
Subscribes or unsubscribes the calling process to node status change messages.

See `net_kernel:monitor_nodes/2` for more information.
""".
-spec monitor_nodes( boolean() ) -> void().
monitor_nodes( DoStartNewSubscription ) ->
    monitor_nodes( DoStartNewSubscription, _Options=[] ).



-doc """
Subscribes or unsubscribes the calling process to node status change messages.

See `net_kernel:monitor_nodes/2` for more information.
""".
-spec monitor_nodes( boolean(), [ monitor_node_option() ] ) -> void().
monitor_nodes( DoStartNewSubscription, Options ) ->

    case net_kernel:monitor_nodes( DoStartNewSubscription, Options ) of

        ok ->
            ok;

        Error ->
            throw( { node_monitoring_failed, Error, DoStartNewSubscription,
                     Options } )

    end.




% Process monitoring section.
%
% The goal here is notably to track the life-cycle of processes, for debugging
% purposes
%
% Using a monitor is better than relying on links and trapping EXITs.
%
% Sending a monitor request will in turn result in monitor messages to be
% received.


-doc """
Monitors the current process with a dedicated one reporting with traces any
monitoring event, whose PID is returned.

This monitor (including the corresponding process) can be terminated by sending
the `terminate` atom to the returned PID.
""".
-spec monitor_self() -> proc_monitor_pid().
monitor_self() ->
    monitor_process( self() ).


-doc """
Monitors the specified process with a dedicated one reporting with traces any
monitoring event, whose PID is returned.

This monitor (including the corresponding process) can be terminated by sending
the `terminate` atom to the returned PID.
""".
-spec monitor_process( monitored_process() ) -> proc_monitor_pid().
monitor_process( TargetProcId ) ->
    % Not wanting to kill, even with 'normal', the caller when the monitoring
    % process terminates, so no link:
    %
    ?myriad_spawn( fun() -> monitor_process_init( TargetProcId ) end ).



% Run by a dedicated process:
monitor_process_init( TargetProcId ) ->

    MonRef = erlang:monitor( _Type=process, TargetProcId ),

    trace_bridge:debug_fmt_echoed( "The process ~w is monitoring "
        "the target process ~p now, based on monitor ~p.",
        [ self(), TargetProcId, MonRef ] ),

    monitor_process_loop( TargetProcId, MonRef ).


% (helper)
monitor_process_loop( TargetProcId, MonRef ) ->

    receive

        { ReasonTag, _MonitorRef=MonRef, _Type=process, _Object=TargetProcId,
          TriggerReason } ->
            % Note that we expect that a monitor is fired at most once (only),
            % so we terminate here:
            %
            trace_bridge:notice_fmt_echoed( "~w monitor (~w) event triggered "
                "(through ~w) for the target process ~p; reason:~n ~p",
                [ ReasonTag, MonRef, self(), TargetProcId, TriggerReason ] );

        terminate ->
            trace_bridge:info_fmt_echoed( "Requested to terminate "
                "the process ~w in charge of monitoring the target process ~p "
                "(based on monitor ~w).", [ self(), TargetProcId, MonRef ] );

        Other ->
            trace_bridge:error_fmt_echoed( "Monitoring process ~w "
                "for process ~p received an unexpected message (~p), "
                "ignoring it.", [ self(), TargetProcId, Other] ),

            monitor_process_loop( TargetProcId, MonRef )

    end.
