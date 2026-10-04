% Copyright (C) 2023-2026 Olivier Boudeville
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
% Creation date: Thursday, May 18, 2023.

-module(process_utils).

-moduledoc """
Gathering of various convenient facilities regarding **(Erlang) processes**.

See `process_utils_test.erl` for the corresponding test.

See also the `monitor_utils` module, which relies on Erlang monitors.
""".



-export([ spawn_message_queue_monitor/1, spawn_message_queue_monitor/2,
          spawn_message_queue_monitor/4 ]).

-export([ spawn_reduction_monitor/1, spawn_reduction_monitor/2,
          spawn_reduction_monitor/4 ]).

-export([ get_overall_monitor_default_registration_name/0,
          spawn_overall_monitor/0, spawn_overall_monitor/3,
          spawn_overall_monitor/4 ]).




-export([ set_label/1, get_label/0, get_label/1,
          get_label_string/0, get_label_string/1,
          describe/0, describe/1, label_to_string/1 ]).



-doc """
The PID of a (Myriad) monitoring process.

Not to be mixed up with an Erlang monitor.
""".
-type monitor_pid() :: pid().


-doc """
The PID of a process monitoring the message queue of processes (i.e. mailboxes).
""".
-type mailbox_monitor_pid() :: monitor_pid().

-doc "A number of (Erlang) messages.".
-type message_count() :: count().


-doc "The PID of a process monitoring reductions of processes.".
-type reduction_monitor_pid() :: monitor_pid().

-doc "A number of reductions done by a process.".
-type reduction_count() :: count().


-doc "The PID of a process monitoring multiple metrics of processes.".
-type overall_monitor_pid() :: monitor_pid().




% Mostly defined here to remember it:
%
% (stored with the '$process_label' key in the process dictionary)
%
-doc """
A label set by the user on a given process.

Helps the debugging of unregistered processes, notably when they are not able to
process messages anymore.

Many tools (observer, logger, crash reporter, etc., including ours) will use
this information afterwards.
""".
-type process_label() :: term(). % Generally a text_utils:bin_string().



-export_type([ monitor_pid/0,
               mailbox_monitor_pid/0, message_count/0,
               reduction_monitor_pid/0, reduction_count/0,
               overall_monitor_pid/0,
               process_label/0 ]).


-doc "Monitoring information regarding a process.".
-type proc_info() ::
    { pid(), option( message_count() ), option( reduction_count() ) }.


-doc "A table holding monitoring information regarding processes.".
-type proc_table() :: table( pid(), reduction_count() ).



% Implementation notes:
%
% The proc_lib module is of interest here.



% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type any_string() :: text_utils:any_string().
-type bin_string() :: text_utils:bin_string().

-type milliseconds() :: time_utils:milliseconds().

-type registration_name() :: naming_utils:registration_name().



% For myriad_spawn_link/1:
-include("spawn_utils.hrl").





% Section for the monitoring of message queues, to detect whether processes
% accumulate messages in their mailbox.


-doc """
Spawns a process monitoring the length of the message queue of the specified
process: returns the PID of an helper process that displays a warning message
if, in the course of a periodic sampling each two seconds, this length is above
1000.

The `terminate` atom shall be sent to the returned PID in order to terminate the
corresponding monitoring process.
""".
-spec spawn_message_queue_monitor( pid() ) -> mailbox_monitor_pid().
spawn_message_queue_monitor( MonitoredPid ) ->
    spawn_message_queue_monitor( MonitoredPid,
                                 _MaybeMonitoredProcessDescStr=undefined ).



-doc """
Spawns a process monitoring the length of the message queue of the specified
process: returns the PID of an helper process that displays a warning message
(with any description thereof supplied) if, in the course of a periodic sampling
each two seconds, this length is above 1000.

The `terminate` atom shall be sent to the returned PID in order to terminate the
corresponding monitoring process.
""".
-spec spawn_message_queue_monitor( pid(), option( any_string() ) ) ->
                                            mailbox_monitor_pid().
spawn_message_queue_monitor( MonitoredPid, MaybeMonitoredProcessDesc ) ->
    spawn_message_queue_monitor( MonitoredPid, MaybeMonitoredProcessDesc,
        _MsgThreshold=1000, _SamplingPeriodMs=2000 ).



-doc """
Spawns a process monitoring the length of the message queue of the specified
process: returns the PID of an helper process that displays a warning message
(with any description thereof supplied) if, in the course of the specified
periodic sampling, this length is above the specified threshold.

The `terminate` atom shall be sent to the returned PID in order to terminate the
corresponding monitoring process.
""".
-spec spawn_message_queue_monitor( pid(), option( any_string() ),
            message_count(), milliseconds() ) -> mailbox_monitor_pid().
spawn_message_queue_monitor( MonitoredPid, MaybeMonitoredProcessDesc,
                             MsgThreshold, SamplingPeriodMs ) ->

    BinProcDesc = case MaybeMonitoredProcessDesc of

        undefined ->
            text_utils:bin_format( "process ~w", [ MonitoredPid ] );

        AnyDesc ->
            text_utils:bin_format( "the '~ts' process (~w)",
                                   [ AnyDesc, MonitoredPid ] )

    end,

    MonitorPid = ?myriad_spawn_link(
        fun() ->
            message_queue_monitor_main_loop( MonitoredPid, BinProcDesc,
                                             MsgThreshold, SamplingPeriodMs )
        end ),

    trace_utils:debug_fmt( "Spawned ~w as message-queue monitor of "
        "~ts; threshold for message-queue length: ~B; sampling period: ~ts.",
        [ MonitorPid, BinProcDesc, MsgThreshold,
          time_utils:duration_to_string( SamplingPeriodMs ) ] ),

    MonitorPid.



% (helper)
-spec message_queue_monitor_main_loop ( pid(), bin_string(), message_count(),
                                        milliseconds() ) -> no_return().
message_queue_monitor_main_loop( MonitoredPid, BinProcDesc, MsgThreshold,
                                 SamplingPeriodMs ) ->

    receive

        terminate ->
            % (monitored PID included)
            trace_utils:debug_fmt(
                "(message-queue monitor ~w for ~ts terminated)",
                [ self(), BinProcDesc ] ),

            terminated;


        UnexpectedMsg ->
            trace_utils:warning_fmt( "Unexpected message received by message "
                "queue monitor, thus ignored: ~p", [ UnexpectedMsg ] ),

            % Resets delay...
            message_queue_monitor_main_loop( MonitoredPid, BinProcDesc,
                                             MsgThreshold, SamplingPeriodMs )

    after SamplingPeriodMs ->

            { message_queue_len, QueueLen } =
                erlang:process_info( MonitoredPid, message_queue_len ),

            QueueLen > MsgThreshold andalso
                trace_utils:warning_fmt( "The length of the message queue "
                    "of ~ts is ~B (thus exceeding the ~w threshold)~ts",
                    [ BinProcDesc, QueueLen, MsgThreshold,
                      describe_latest_messages( MonitoredPid, _LastMsgCount=12 )
                    ] ),

            message_queue_monitor_main_loop( MonitoredPid, BinProcDesc,
                                             MsgThreshold, SamplingPeriodMs )

    end.




% Section for the monitoring of reduction counts, to detect whether processes
% are becoming abnormally busy, typically if having entered uncontrolled
% infinite recursion.


-doc """
Spawns a process monitoring the number of reductions done by the specified
process: returns the PID of an helper process that displays a warning message
if, after 1 second of a periodic sampling, the reduction count increased of at
least 5000.

The `terminate` atom shall be sent to the returned PID in order to terminate the
corresponding monitoring process.
""".
-spec spawn_reduction_monitor( pid() ) -> mailbox_monitor_pid().
spawn_reduction_monitor( MonitoredPid ) ->
    spawn_reduction_monitor( MonitoredPid,
                             _MaybeMonitoredProcessDescStr=undefined ).



-doc """
Spawns a process monitoring the number of reductions done by the specified
process: returns the PID of an helper process that displays a warning message
(with any description thereof supplied) if, after 1 second of a periodic
sampling, the reduction count increased of at least 5000.

The `terminate` atom shall be sent to the returned PID in order to terminate the
corresponding monitoring process.
""".
-spec spawn_reduction_monitor( pid(), option( any_string() ) ) ->
                                            mailbox_monitor_pid().
spawn_reduction_monitor( MonitoredPid, MaybeMonitoredProcessDesc ) ->
    spawn_reduction_monitor( MonitoredPid, MaybeMonitoredProcessDesc,
        _ReducThreshold=5000, _SamplingPeriodMs=1000 ).



-doc """
Spawns a process monitoring the number of reductions done by the specified
process: returns the PID of an helper process that displays a warning message
(with any description thereof supplied) if, after the specified sampling period,
the reduction count increased of at least the specified threshold.

The `terminate` atom shall be sent to the returned PID in order to terminate the
corresponding monitoring process.
""".
-spec spawn_reduction_monitor( pid(), option( any_string() ),
        reduction_count(), milliseconds() ) -> mailbox_monitor_pid().
spawn_reduction_monitor( MonitoredPid, MaybeMonitoredProcessDesc,
                         ReducThreshold, SamplingPeriodMs ) ->

    % So always includes the monitored PID:
    BinProcDesc = case MaybeMonitoredProcessDesc of

        undefined ->
            text_utils:bin_format( "process ~w", [ MonitoredPid ] );

        AnyDesc ->
            text_utils:bin_format( "the '~ts' process (~w)",
                                   [ AnyDesc, MonitoredPid ] )

    end,

    MonitorPid = ?myriad_spawn_link(
        fun() ->

            { reductions, InitReducs } =
                    erlang:process_info( MonitoredPid, reductions ),

            reduction_monitor_main_loop( MonitoredPid, BinProcDesc, InitReducs,
                _MaxDeltaReducs=ReducThreshold, SamplingPeriodMs )

        end ),

    trace_utils:debug_fmt( "Spawned ~w as reduction monitor of ~ts;
        maximum delta threshold for reductions: ~B; sampling period: ~ts.",
        [ MonitorPid, BinProcDesc, ReducThreshold,
          time_utils:duration_to_string( SamplingPeriodMs ) ] ),

    MonitorPid.



% (helper)
-spec reduction_monitor_main_loop ( pid(), bin_string(), reduction_count(),
    reduction_count(), milliseconds() ) -> no_return().
reduction_monitor_main_loop( MonitoredPid, BinProcDesc, CurrentReducs,
                             MaxDeltaReducs, SamplingPeriodMs ) ->

    receive

        terminate ->
            % (monitored PID included)
            trace_utils:debug_fmt( "(reduction monitor ~w for ~ts terminated)",
                                   [ self(), BinProcDesc ] ),

            terminated;


        UnexpectedMsg ->

            % Resets delay...
            trace_utils:warning_fmt( "Unexpected message received by reduction "
                "monitor, thus ignored: ~p", [ UnexpectedMsg ] ),

            reduction_monitor_main_loop( MonitoredPid, BinProcDesc,
                CurrentReducs, MaxDeltaReducs, SamplingPeriodMs )

    after SamplingPeriodMs ->

            { reductions, NewReducs } =
                erlang:process_info( MonitoredPid, reductions ),

            DeltaReducs = NewReducs - CurrentReducs,
            DeltaReducs > MaxDeltaReducs andalso
                trace_utils:warning_fmt( "The number of reductions of ~ts "
                    "increased of ~B (reaching ~B), exceeding the delta "
                    "threshold of ~B reductions per ~ts.",
                    [ BinProcDesc, DeltaReducs, NewReducs, MaxDeltaReducs,
                      time_utils:duration_to_string( SamplingPeriodMs ) ] ),

            reduction_monitor_main_loop( MonitoredPid, BinProcDesc, NewReducs,
                MaxDeltaReducs, SamplingPeriodMs )

    end.



% Section for the general (multi-topic: message queue, reductions), untargeted
% (all processes) monitoring.


-doc "Returns the default (local) registration name of the overall monitor.".
-spec get_overall_monitor_default_registration_name() -> registration_name().
get_overall_monitor_default_registration_name() ->
    myriad_overall_process_monitor.



-doc """
Spawns a process monitoring with reasonable defaults the length of the message
queues and the instantaneous consumption of reductions of all processes running
on the current node: returns the PID of an helper process that displays a
warning message (with any description thereof supplied) if, in the course of the
specified periodic sampling, the metrics of some processes exceed limits:
- message queue exceeding specified threshold
- reduction count increased of at least the specified threshold

The `terminate` atom shall be sent to the returned PID in order to terminate the
corresponding monitoring process.
""".
-spec spawn_overall_monitor() -> no_return().
spawn_overall_monitor() ->
    spawn_overall_monitor( _MsgThreshold=200, _ReducThreshold=250_000,
        _SamplingPeriodMs=2_000,
        _LocalRegName=get_overall_monitor_default_registration_name() ).



-doc """
Spawns a process monitoring the length of the message queues and the
instantaneous consumption of reductions of all processes running on the current
node: returns the PID of an helper process that displays a warning message (with
any description thereof supplied) if, in the course of the specified periodic
sampling, the metrics of some processes exceed limits:
- message queue exceeding specified threshold
- reduction count increased of at least the specified threshold

The `terminate` atom shall be sent to the returned PID in order to terminate the
corresponding monitoring process.
""".
-spec spawn_overall_monitor( message_count(), reduction_count(),
                             milliseconds() ) -> no_return().
spawn_overall_monitor( MsgThreshold, ReducThreshold, SamplingPeriodMs ) ->
    spawn_overall_monitor( MsgThreshold, ReducThreshold, SamplingPeriodMs,
                           _MaybeLocalRegName=undefined ).



-doc """
Spawns a process monitoring the length of the message queues and the
instantaneous consumption of reductions of all processes running on the current
node: returns the PID of an helper process that displays a warning message (with
any description thereof supplied) if, in the course of the specified periodic
sampling, the metrics of some processes exceed limits:
- message queue exceeding specified threshold
- reduction count increased of at least the specified threshold

If a non-undefined registration name is specified, this monitor will be locally
registered according to this name.

The `terminate` atom shall be sent to the returned PID in order to terminate the
corresponding monitoring process.
""".
-spec spawn_overall_monitor( message_count(), reduction_count(),
    milliseconds(), option( registration_name() ) ) -> no_return().
spawn_overall_monitor( MsgThreshold, ReducThreshold, SamplingPeriodMs,
                       MaybeLocalRegName ) ->

    MonitorPid = ?myriad_spawn_link(
        fun() ->

            % Not registered globally, as each node may want its own monitor:
            MaybeLocalRegName =:= undefined orelse
                naming_utils:register_as( MaybeLocalRegName,
                                          _RegScope=local_only ),

            overall_monitor_main_loop( MsgThreshold, ReducThreshold,
                SamplingPeriodMs, _ProcTable=table:new() )
        end ),

    trace_utils:debug_fmt( "Spawned overall process monitor ~w, whose "
        "threshold for message-queue length is ~B, delta-reduction threshold "
        "is ~B, and sampling period is ~ts.",
        [ MonitorPid, MsgThreshold, ReducThreshold,
          time_utils:duration_to_string( SamplingPeriodMs ) ] ),

    MonitorPid.



% (helper)
-spec overall_monitor_main_loop( message_count(), reduction_count(),
                                 milliseconds(), proc_table() ) -> no_return().
overall_monitor_main_loop( MsgThreshold, ReducThreshold, SamplingPeriodMs,
                           ProcTable ) ->

    receive

        terminate ->
            % (monitored PID included)
            trace_utils:debug_fmt( "(overall reduction monitor ~w terminated)",
                                   [ self() ] ),

            terminated;


        UnexpectedMsg ->
            trace_utils:warning_fmt( "Unexpected message received by overall "
                "monitor, thus ignored: ~p", [ UnexpectedMsg ] ),

            % Resets delay...
            overall_monitor_main_loop( MsgThreshold, ReducThreshold,
                                       SamplingPeriodMs, ProcTable )

    after SamplingPeriodMs ->

            { NewProcTable, ProcInfos } =
                scan_all_processes( ProcTable, MsgThreshold, ReducThreshold ),

            case ProcInfos of

                [] ->
                    ok;

                [ ProcInfo ] ->
                    trace_utils:warning_fmt( "Abnormal metrics: ~ts "
                        "(thresholds being up to ~B messages per mailbox, "
                        "and up to ~B new reductions over ~ts)",
                        [ interpret_proc_info( ProcInfo ), MsgThreshold,
                          ReducThreshold, time_utils:duration_to_string(
                                            SamplingPeriodMs ) ] );


                 ProcInfos ->
                    Strs = [ interpret_proc_info( PI ) || PI <- ProcInfos ],
                    trace_utils:warning_fmt( "~B processes have abnormal "
                        "metrics (thresholds being up to ~B messages per "
                        "mailbox, and up to ~B new reductions over ~ts): ~ts",
                        [ length( Strs ), MsgThreshold, ReducThreshold,
                          time_utils:duration_to_string( SamplingPeriodMs ),
                          text_utils:strings_to_string( Strs ) ] )

                end,

           overall_monitor_main_loop( MsgThreshold, ReducThreshold,
                                      SamplingPeriodMs, NewProcTable )

    end.




-doc """
Scans all processes, updating the specified table and returning information
about the processes that may be problematic.
""".
-spec scan_all_processes( proc_table(), message_count(), reduction_count() ) ->
          { proc_table(), [ proc_info() ] }.
scan_all_processes( ProcTable, MsgThreshold, ReducThreshold ) ->
    scan_all_processes( _ProcIter=erlang:processes_iterator(), ProcTable,
                        MsgThreshold, ReducThreshold, _AccProcInfos=[] ).


% (helper)
scan_all_processes( ProcIter, ProcTable, MsgThreshold, ReducThreshold,
                    AccProcInfos ) ->

    Self = self(),

    case erlang:processes_next( ProcIter ) of

        % Not scanning ourself:
        { Self, NewProcIter } ->
            scan_all_processes( NewProcIter, ProcTable, MsgThreshold,
                                ReducThreshold, AccProcInfos );

        { Pid, NewProcIter } ->

            case erlang:process_info( Pid,
                                      [ message_queue_len, reductions ] ) of

                % Process is not alive anymore, i.e. died concurrently, thus to
                % be ignored:
                %
                undefined ->
                    NewProcTable = table:remove_entry( Pid, ProcTable ),
                    scan_all_processes( NewProcIter, NewProcTable, MsgThreshold,
                                        ReducThreshold, AccProcInfos );

                [ { message_queue_len, QueueLen },
                  { reductions, NewReducs } ] ->
                    { NewProcTable, NewAccProcInfos } = integrate_process( Pid,
                        ProcTable, QueueLen, MsgThreshold, NewReducs,
                        ReducThreshold, AccProcInfos ),

                    scan_all_processes( NewProcIter, NewProcTable, MsgThreshold,
                                        ReducThreshold, NewAccProcInfos )

            end;

        % End of iteration:
        none ->
            { ProcTable, AccProcInfos }

    end.


-doc "Integrates the metrics of the corresponding process.".
-spec integrate_process( pid(), proc_table(), message_count(), message_count(),
    reduction_count(), reduction_count(), [ proc_info() ] ) ->
                                { proc_table(), [ proc_info() ] }.
integrate_process( Pid, ProcTable, QueueLen, MsgThreshold,
                   NewReducs, ReducThreshold, AccProcInfos ) ->

    MaybeMsgCount = case QueueLen >= MsgThreshold of

        true ->
            QueueLen;

        _False ->
            undefined

    end,

    MaybeExtraReducCount = case table:lookup_entry( _K=Pid, ProcTable ) of

        % Process not known yet:
        key_not_found ->
            undefined;

        { value, PrevReducs } ->
            ExtraReducCount = NewReducs - PrevReducs,
            case ExtraReducCount >= ReducThreshold of

                true ->
                    ExtraReducCount;

                _OtherFalse ->
                    undefined

            end

    end,

    NewProcTable = table:add_entry( Pid, _V=NewReducs, ProcTable ),

    NewAccProcInfos = case { MaybeMsgCount, MaybeExtraReducCount } of

        { undefined, undefined } ->
            AccProcInfos;

        % At least one problematic metrics, so:
        _ ->
            [ { Pid, MaybeMsgCount, MaybeExtraReducCount } | AccProcInfos ]

    end,

    { NewProcTable, NewAccProcInfos }.



-spec interpret_proc_info( proc_info() ) -> ustring().
interpret_proc_info( _ProcInfo={ _Pid, _MaybeMsgCount=undefined,
                                 _MaybeExtraReducCount=undefined } ) ->
    "(no relevant process information - abnormal)";

interpret_proc_info(
        _ProcInfo={ Pid, MsgCount, _MaybeExtraReducCount=undefined } ) ->
    text_utils:format( "~ts has ~B messages in its mailbox~ts",
        [ describe( Pid ), MsgCount,
          describe_latest_messages( Pid, _LastCount=4 ) ] );

interpret_proc_info( _ProcInfo={ Pid, _MaybeMsgCount=undefined,
                                 ExtraReducCount } ) ->
    text_utils:format( "~ts used ~B extra reductions",
                       [ describe( Pid ), ExtraReducCount ] );

interpret_proc_info( _ProcInfo={ Pid, MsgCount, ExtraReducCount } ) ->
    text_utils:format( "~ts used ~B extra reductions and "
        "has ~B messages in its mailbox~ts",
        [ describe( Pid ), ExtraReducCount, MsgCount,
          describe_latest_messages( Pid, _LastCount=4 ) ] ).



-doc """
Describes the latest messages sitting in the mailbox of the specified process.
""".
-spec describe_latest_messages( pid(), count() ) -> ustring().
describe_latest_messages( TargetPid, LastMsgCount ) ->

    { messages, Msgs } = erlang:process_info( TargetPid, messages ),

    LastMessages = case length( Msgs ) of

        L when L > LastMsgCount ->
            { Lasts, _Prevs } =
                list_utils:extract_last_elements( Msgs, LastMsgCount ),
            Lasts;

        % Return everything possible:
        _ ->
            Msgs

    end,

    text_utils:format( ", last ones being: ~ts",
        [ text_utils:strings_to_string( [ text_utils:term_to_bounded_string(
            M, _MaxLen=80 ) || M <- LastMessages ], _IndentationLevel=1 ) ] ).






% Section for the monitoring of reduction counts, to detect whether processes
% are becoming abnormally busy, typically if having entered uncontrolled
% infinite recursion.



% Section for the management of process-level labels.

-doc "Sets the label of the current process.".
-spec set_label( process_label() ) -> void().
set_label( ProcessLabel ) ->
    proc_lib:set_label( ProcessLabel ).



-doc "Gets the label (if any) of the current process.".
-spec get_label() -> option( process_label() ).
get_label() ->
    get_label( self() ).


-doc "Gets the label (if any) of the specified process.".
-spec get_label( pid() ) -> option( process_label() ).
get_label( Pid ) ->
    proc_lib:get_label( Pid ).



-doc "Gets a (possibly empty) label string for the current process.".
-spec get_label_string() -> ustring().
get_label_string() ->
    get_label_string( self() ).


-doc "Gets a (possibly empty) label string for the specified process.".
-spec get_label_string( pid() ) -> ustring().
get_label_string( Pid ) ->
    case get_label( Pid ) of

        undefined ->
            "";

        Label ->
            text_utils:format( " (labelled '~ts')",
                               [ label_to_string( Label ) ] )

    end.



-doc """
Returns a description of the current process, taking into account any associated
label.
""".
-spec describe() -> ustring().
describe() ->
    describe( self() ).


-doc """
Returns a description of the specified process, taking into account any
associated label.
""".
-spec describe( pid() ) -> ustring().
describe( Pid ) ->
    case get_label( Pid ) of

        undefined ->
            text_utils:format( "~w", [ Pid ] );

        Label ->
            text_utils:format( "process '~ts' (~w)" ,
                               [ label_to_string( Label ), Pid ] )

    end.


-doc "Returns the most suitable/readable string for the specified label.".
-spec label_to_string( process_label() ) -> ustring().
label_to_string( Label ) ->
    case text_utils:is_string_like( Label ) of

        true ->
            text_utils:format( "~ts", [ Label ] );

        _False ->
            text_utils:format( "~p", [ Label ] )

    end.
