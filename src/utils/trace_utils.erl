% Copyright (C) 2013-2026 Olivier Boudeville
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
% Creation date: Friday, November 1, 2013.

-module(trace_utils).

-moduledoc """
Gathering of various very **low-level trace-related facilities** on the
console. These are (runtime) logs; they are by default not linked to the Erlang
logging subsystem; see the `set_handler/0` function in the current module so
that they are integrated to the logger facilities.

The API functions provided by the current module are mostly useful so that their
call can be replaced by calls to the far more advanced facilities of the
`Ceylan-Traces` layer with no change in their parameters.

For example, `trace_utils:debug_fmt("I am the ~B.", [1])` may be replaced by
`?debug_fmt("I am the ~B.", [1])` to switch, in a trace emitter instance, from
basic traces output on the console to traces sent through the Traces subsystem.

As a result, for a trace severity S in `[debug, info, notice, warning, error,
critical, alert, emergency, void]`, `trace_utils:S` may be replaced as a whole
by `?S` to promote a very debug-oriented trace into a potentially more durable
one.

Note that a given trace emission can be fully disabled (with no remaining
resource consumption at all) thanks to the `cond_utils:if_defined*` primitives.

This module is also a logger one, see
<https://erlang.org/doc/apps/kernel/logger_chapter.html>.

See `trace_utils_test.erl` for testing.

Not to be mixed up with the `traces_utils` module of Ceylan-Traces (note that
their names differ).
""".



% To resolve name clash:
-compile( { no_auto_import, [ error/1 ] } ).


-doc "An actual trace message.".
-type trace_message() :: ustring().


-doc "An actual (binary) trace message.".
-type trace_bin_message() :: bin_string().


-doc "An actual trace message, of any string type.".
-type trace_any_message() :: any_string().



-doc """
Defining, according to the Erlang newer logger API and thus in accordance with
the Syslog protocol (RFC 5424), 8 levels of severity (plus a `void` one), from
least important to most: debug, info, notice, warning, error, critical, alert
and emergency (void being always muted):

See also `standard_logger_level_to_severity/1` for a proper conversion.
""".
-type trace_severity() ::

    % For debug-level messages:
    'debug'

    % For informational, lower-level messages:
  | 'info'

    % For normal yet significant conditions:
  | 'notice'

    % For warning conditions:
  | 'warning'

    % For error conditions:
  | 'error'

    % For critical conditions:
  | 'critical'

    % For actions that must be taken immediately:
  | 'alert'

    % Highest criticity, when system became unusable:
  | 'emergency'

    % For messages that shall be fully muted (disabled):
  | 'void'.



-doc "A format with quantifiers (such as `~p`).".
-type trace_format() :: text_utils:format_string().



-doc "Values corresponding to format quantifiers.".
-type trace_values() :: text_utils:format_values().



-doc """
Categorization of a trace message.

A message may or may not (which is the default and general case - resulting in
the use of the `uncategorized` atom) be categorized.

Atoms are supported as well, as a limited number of message categorization
generally applies.
""".
-type trace_message_categorization() :: ustring() | atom().



-doc """
A message may or may not (which is the default and general case - resulting in
the use of the `uncategorized` atom) be categorized.

Note that the `bin_` prefix may be a bit misleading here, as an atom can still
be used.

Atoms are supported as well, as a limited number of message categorization
generally applies.
""".
-type trace_bin_message_categorization() :: bin_string() | atom().



-doc """
An applicative timestamp for a trace; it can be anything (e.g.` integer() |
'none'`), no constraint applies on purpose, so that any kind of
application-specific timestamps can be elected.

Textual timestamps shall better be binaries or atoms rather than plain strings.
""".
-type trace_timestamp() :: any().



-doc "Not including the `void` severity here.".
-type trace_priority() :: 0..7.


-export_type([ trace_message/0, trace_bin_message/0, trace_severity/0,
               trace_format/0, trace_values/0,
               trace_message_categorization/0,
               trace_bin_message_categorization/0,
               trace_timestamp/0, trace_priority/0 ]).



-export([ debug/1, debug_fmt/2, debug_categorized/2, debug_categorized_timed/3,

          info/1, info_fmt/2, info_categorized/2, info_categorized_timed/3,

          notice/1, notice_fmt/2, notice_categorized/2,
          notice_categorized_timed/3,

          warning/1, warning_fmt/2, warning_categorized/2,
          warning_categorized_timed/3,

          error/1, error_fmt/2, error_categorized/2, error_categorized_timed/3,

          critical/1, critical_fmt/2, critical_categorized/2,
          critical_categorized_timed/3,

          alert/1, alert_fmt/2, alert_categorized/2, alert_categorized_timed/3,

          emergency/1, emergency_fmt/2, emergency_categorized/2,
          emergency_categorized_timed/3,

          void/1, void_fmt/2, void_categorized/2, void_categorized_timed/3,

          safer_display/1, safer_display/2,

          echo/2, echo/3, echo/4,

          get_priority_for/1, get_severity_for/1, is_error_like/1 ]).


% Logger-related API (see
% https://erlang.org/doc/apps/kernel/logger_chapter.html):
%
-export([ set_handler/0, add_handler/0, log/2, set_logger_format_max_depth/1 ]).


% Handler id:
-define( myriad_logger_id, ceylan_myriad_logger_handler_id ).


% At least for error cases, ellipsing traces is not a good idea; as we are
% relying here on mere console outputs, the ellipsing of other traces may be
% relevant (this is the default here):
%
-ifdef(myriad_unellipsed_traces).

 % Disables the ellipsing of traces:
 -define( ellipse_length, unlimited ).

-else. % myriad_unellipsed_traces

 % Default:
 -define( ellipse_length, 2000 ).

-endif. % myriad_unellipsed_traces



% Implementation notes:
%
% Compared to mere io:format/{1,2} calls, these trace primitives add
% automatically the trace type (e.g. "[debug] ") at the beginning of the
% message, finish it with a carriage-return/line-feed, and for the most
% important trace types, try to ensure that they are synchronous (blocking).

% No space is kept between the brackets of the severity (e.g. "[debug]") and any
% message starting with "[".


% Traces of lesser importance are ellipsed, as the console output does not allow
% to browse them conveniently.
%
% Care has been taken so that all traces emitted thanks to trace_utils
% (e.g. trace_utils:warning/1) return the 'ok' atom (instead of the previous
% void()), so that callers (like trace_bridge) can in turn return different
% atoms based on the actual outputs done.


% Type shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().


% As consoles use fixed-size fonts, all severities are justified to fit in the
% space of the longest, which is "emergency", so that their messages are then
% properly aligned.

%% -define( debug_prefix,     "[  debug  ]" ).
%% -define( info_prefix,      "[  info   ]" ).
%% -define( notice_prefix,    "[  notice ]" ).
%% -define( warning_prefix,   "[ warning ]" ).
%% -define( error_prefix,     "[  error  ]" ).
%% -define( critical_prefix,  "[ critical]" ).
%% -define( alert_prefix,     "[  alert  ]" ).
%% -define( emergency_prefix, "[emergency]" ).

% Finally we prefer this more compact version:
-define( debug_prefix,     "[debug]" ).
% Note that not 'info', to avoid a space:
-define( info_prefix,      "[infor]" ).
-define( notice_prefix,    "[notic]" ).
-define( warning_prefix,   "[warni]" ).
-define( error_prefix,     "[error]" ).
-define( critical_prefix,  "[criti]" ).
-define( alert_prefix,     "[alert]" ).
-define( emergency_prefix, "[emerg]" ).



-doc "Outputs the specified debug message.".
-spec debug( trace_any_message() ) -> 'ok'.
debug( Message ) ->
    actual_display( ?debug_prefix ++ offset( Message ) ).


-doc "Outputs the specified formatted debug message.".
-spec debug_fmt( trace_format(), trace_values() ) -> 'ok'.
debug_fmt( Format, Values ) ->
    debug( text_utils:format( Format, Values ) ).



-doc """
Outputs the specified debug message, with the specified message categorization.
""".
-spec debug_categorized( trace_any_message(),
                         trace_message_categorization() ) -> 'ok'.
debug_categorized( Message, _MessageCategorization=uncategorized ) ->
    actual_display( ?debug_prefix ++ offset( Message ) );

debug_categorized( Message, MessageCategorization ) ->
    actual_display( ?debug_prefix ++ text_utils:format( "[~ts]~ts",
                    [ MessageCategorization, offset( Message ) ] ) ).



-doc """
Outputs the specified debug message, with the specified message categorization
and time information.
""".
-spec debug_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
debug_categorized_timed( Message, _MessageCategorization=uncategorized,
                         Timestamp ) ->
    actual_display( ?debug_prefix ++ text_utils:format( "[at ~ts]~ts",
                    [ Timestamp, offset( Message ) ] ) );

debug_categorized_timed( Message, MessageCategorization, Timestamp ) ->
    actual_display( ?debug_prefix ++ text_utils:format( "[~ts][at ~ts]~ts",
        [ MessageCategorization, Timestamp, offset( Message ) ] ) ).




-doc "Outputs the specified info message.".
-spec info( trace_any_message() ) -> 'ok'.
info( Message ) ->
    actual_display( ?info_prefix ++ offset( Message ) ).



-doc "Outputs the specified formatted info message.".
-spec info_fmt( trace_format(), trace_values() ) -> 'ok'.
info_fmt( Format, Values ) ->
    info( text_utils:format( Format, Values ) ).



-doc """
Outputs the specified info message, with the specified message categorization.
""".
-spec info_categorized( trace_any_message(), trace_message_categorization() ) ->
                                            'ok'.
info_categorized( Message, _MessageCategorization=uncategorized ) ->
    actual_display( ?info_prefix ++ offset( Message ) );

info_categorized( Message, MessageCategorization ) ->
    actual_display( ?info_prefix ++ text_utils:format( "[~ts]~ts",
                    [ MessageCategorization, offset( Message ) ] ) ).



-doc """
Outputs the specified info message, with the specified message categorization
and time information.
""".
-spec info_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
info_categorized_timed( Message, _MessageCategorization=uncategorized,
                        Timestamp ) ->
    actual_display( ?info_prefix ++ text_utils:format( "[at ~ts]~ts",
                    [ Timestamp, offset( Message ) ] ) );

info_categorized_timed( Message, MessageCategorization, Timestamp ) ->
    actual_display( ?info_prefix ++ text_utils:format( "[~ts][at ~ts]~ts",
        [ MessageCategorization, Timestamp, offset( Message ) ] ) ).



-doc "Outputs the specified notice message.".
-spec notice( trace_any_message() ) -> 'ok'.
notice( Message ) ->
    actual_display( ?notice_prefix ++ offset( Message ) ).


-doc "Outputs the specified formatted notice message.".
-spec notice_fmt( trace_format(), trace_values() ) -> 'ok'.
notice_fmt( Format, Values ) ->
    notice( text_utils:format( Format, Values ) ).



-doc """
Outputs the specified notice message, with the specified message categorization.
""".
-spec notice_categorized( trace_any_message(),
                         trace_message_categorization() ) -> 'ok'.
notice_categorized( Message, _MessageCategorization=uncategorized ) ->
    actual_display( ?notice_prefix ++ offset( Message ) );

notice_categorized( Message, MessageCategorization ) ->
    actual_display( ?notice_prefix ++ text_utils:format( "[~ts]~ts",
                    [ MessageCategorization, offset( Message ) ] ) ).


-doc """
Outputs the specified notice message, with the specified message categorization
and time information.
""".
-spec notice_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
notice_categorized_timed( Message, _MessageCategorization=uncategorized,
                          Timestamp ) ->
    actual_display( ?notice_prefix ++ text_utils:format( "[at ~ts]~ts",
                    [ Timestamp, offset( Message ) ] ) );

notice_categorized_timed( Message, MessageCategorization, Timestamp ) ->
    actual_display( ?notice_prefix ++ text_utils:format( "[~ts][at ~ts]~ts",
        [ MessageCategorization, Timestamp, offset( Message ) ] ) ).



-doc "Outputs the specified warning message.".
-spec warning( trace_any_message() ) -> 'ok'.
warning( Message ) ->
    severe_display( ?warning_prefix ++ offset( Message ) ).



-doc "Outputs the specified formatted warning message.".
-spec warning_fmt( trace_format(), trace_values() ) -> 'ok'.
warning_fmt( Format, Values ) ->
    warning( text_utils:format( Format, Values ) ).



-doc """
Outputs the specified warning message, with the specified message
categorization.
""".
-spec warning_categorized( trace_any_message(),
                           trace_message_categorization() ) -> 'ok'.
warning_categorized( Message, _MessageCategorization=uncategorized ) ->
    severe_display( ?warning_prefix ++ offset( Message ) );

warning_categorized( Message, MessageCategorization ) ->
    severe_display( ?warning_prefix ++ text_utils:format( "[~ts]~ts",
                    [ MessageCategorization, offset( Message ) ] ) ).



-doc """
Outputs the specified warning message, with the specified message categorization
and time information.
""".
-spec warning_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
-spec debug_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
warning_categorized_timed( Message, _MessageCategorization=uncategorized,
                           Timestamp ) ->
    severe_display( ?warning_prefix ++ text_utils:format( "[at ~ts]~ts",
                    [ Timestamp, offset( Message ) ] ) );

warning_categorized_timed( Message, MessageCategorization, Timestamp ) ->
    severe_display( ?warning_prefix ++ text_utils:format( "[~ts][at ~ts]~ts",
        [ MessageCategorization, Timestamp, offset( Message ) ] ) ).




-doc "Outputs the specified error message.".
-spec error( trace_any_message() ) -> 'ok'.
error( Message ) ->
    severe_display( ?error_prefix ++ offset( Message ) ).



-doc "Outputs the specified formatted error message.".
-spec error_fmt( trace_format(), trace_values() ) -> 'ok'.
error_fmt( Format, Values ) ->
    error( text_utils:format( Format, Values ) ).



-doc """
Outputs the specified error message, with the specified message
categorization.
""".
-spec error_categorized( trace_any_message(),
                           trace_message_categorization() ) -> 'ok'.
error_categorized( Message, _MessageCategorization=uncategorized ) ->
    severe_display( ?error_prefix ++ offset( Message ) );

error_categorized( Message, MessageCategorization ) ->
    severe_display( ?error_prefix ++ text_utils:format( "[~ts]~ts",
                    [ MessageCategorization, offset( Message ) ] ) ).



-doc """
Outputs the specified error message, with the specified message categorization
and time information.
""".
-spec error_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
-spec debug_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
error_categorized_timed( Message, _MessageCategorization=uncategorized,
                           Timestamp ) ->
    severe_display( ?error_prefix ++ text_utils:format( "[at ~ts]~ts",
                    [ Timestamp, offset( Message ) ] ) );

error_categorized_timed( Message, MessageCategorization, Timestamp ) ->
    severe_display( ?error_prefix ++ text_utils:format( "[~ts][at ~ts]~ts",
        [ MessageCategorization, Timestamp, offset( Message ) ] ) ).





-doc "Outputs the specified critical message.".
-spec critical( trace_any_message() ) -> 'ok'.
critical( Message ) ->
    severe_display( ?critical_prefix ++ offset( Message ) ).



-doc "Outputs the specified formatted critical message.".
-spec critical_fmt( trace_format(), trace_values() ) -> 'ok'.
critical_fmt( Format, Values ) ->
    critical( text_utils:format( Format, Values ) ).



-doc """
Outputs the specified critical message, with the specified message
categorization.
""".
-spec critical_categorized( trace_any_message(),
                           trace_message_categorization() ) -> 'ok'.
critical_categorized( Message, _MessageCategorization=uncategorized ) ->
    severe_display( ?critical_prefix ++ offset( Message ) );

critical_categorized( Message, MessageCategorization ) ->
    severe_display( ?critical_prefix ++ text_utils:format( "[~ts]~ts",
                    [ MessageCategorization, offset( Message ) ] ) ).



-doc """
Outputs the specified critical message, with the specified message
categorization and time information.
""".
-spec critical_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
-spec debug_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
critical_categorized_timed( Message, _MessageCategorization=uncategorized,
                           Timestamp ) ->
    severe_display( ?critical_prefix ++ text_utils:format( "[at ~ts]~ts",
                    [ Timestamp, offset( Message ) ] ) );

critical_categorized_timed( Message, MessageCategorization, Timestamp ) ->
    severe_display( ?critical_prefix ++ text_utils:format( "[~ts][at ~ts]~ts",
        [ MessageCategorization, Timestamp, offset( Message ) ] ) ).



-doc "Outputs the specified alert message.".
-spec alert( trace_any_message() ) -> 'ok'.
alert( Message ) ->
    severe_display( ?alert_prefix ++ offset( Message ) ).



-doc "Outputs the specified formatted alert message.".
-spec alert_fmt( trace_format(), trace_values() ) -> 'ok'.
alert_fmt( Format, Values ) ->
    alert( text_utils:format( Format, Values ) ).



-doc """
Outputs the specified alert message, with the specified message
categorization.
""".
-spec alert_categorized( trace_any_message(),
                           trace_message_categorization() ) -> 'ok'.
alert_categorized( Message, _MessageCategorization=uncategorized ) ->
    severe_display( ?alert_prefix ++ offset( Message ) );

alert_categorized( Message, MessageCategorization ) ->
    severe_display( ?alert_prefix ++ text_utils:format( "[~ts]~ts",
                    [ MessageCategorization, offset( Message ) ] ) ).



-doc """
Outputs the specified alert message, with the specified message categorization
and time information.
""".
-spec alert_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
-spec debug_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
alert_categorized_timed( Message, _MessageCategorization=uncategorized,
                           Timestamp ) ->
    severe_display( ?alert_prefix ++ text_utils:format( "[at ~ts]~ts",
                    [ Timestamp, offset( Message ) ] ) );

alert_categorized_timed( Message, MessageCategorization, Timestamp ) ->
    severe_display( ?alert_prefix ++ text_utils:format( "[~ts][at ~ts]~ts",
        [ MessageCategorization, Timestamp, offset( Message ) ] ) ).





-doc "Outputs the specified emergency message.".
-spec emergency( trace_any_message() ) -> 'ok'.
emergency( Message ) ->
    severe_display( ?emergency_prefix ++ offset( Message ) ).



-doc "Outputs the specified formatted emergency message.".
-spec emergency_fmt( trace_format(), trace_values() ) -> 'ok'.
emergency_fmt( Format, Values ) ->
    emergency( text_utils:format( Format, Values ) ).



-doc """
Outputs the specified emergency message, with the specified message
categorization.
""".
-spec emergency_categorized( trace_any_message(),
                           trace_message_categorization() ) -> 'ok'.
emergency_categorized( Message, _MessageCategorization=uncategorized ) ->
    severe_display( ?emergency_prefix ++ offset( Message ) );

emergency_categorized( Message, MessageCategorization ) ->
    severe_display( ?emergency_prefix ++ text_utils:format( "[~ts]~ts",
                    [ MessageCategorization, offset( Message ) ] ) ).



-doc """
Outputs the specified emergency message, with the specified message
categorization and time information.
""".
-spec emergency_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
-spec debug_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
emergency_categorized_timed( Message, _MessageCategorization=uncategorized,
                           Timestamp ) ->
    severe_display( ?emergency_prefix ++ text_utils:format( "[at ~ts]~ts",
                    [ Timestamp, offset( Message ) ] ) );

emergency_categorized_timed( Message, MessageCategorization, Timestamp ) ->
    severe_display( ?emergency_prefix ++ text_utils:format( "[~ts][at ~ts]~ts",
        [ MessageCategorization, Timestamp, offset( Message ) ] ) ).



-doc """
"Outputs" the specified void message.
""".
-spec void( trace_any_message() ) -> 'ok'.
void( _Message ) ->
    ok.



-doc """
"Outputs" the specified formatted void message.
""".
-spec void_fmt( trace_format(), trace_values() ) -> 'ok'.
void_fmt( _Format, _Values ) ->
    ok.



-doc """
"Outputs" the specified void message, with the specified message categorization.
""".
-spec void_categorized( trace_any_message(),
                        trace_message_categorization() ) -> 'ok'.
void_categorized( _Message, _MessageCategorization ) ->
    ok.



-doc """
"Outputs" the specified void message, with the specified message categorization
and time information.
""".
-spec void_categorized_timed( trace_any_message(),
        trace_message_categorization(), trace_timestamp() ) -> 'ok'.
void_categorized_timed( _Message, _MessageCategorization, _Timestamp ) ->
    ok.



-define( echo_prefix, "[echoed] " ++ ).


-doc """
Echoes the specified trace in the specified trace channel.

Defined notably to perform integrated operations (a trace being sent through
both a basic system and a more advanced one), in order that the trace macros of
upper layers (e.g. `send_alert_fmt/3`, in the Traces layer) do not need to bind
variables in their body (which may trigger bad matches as soon as more than once
trace is sent in the same scope).
""".
-spec echo( trace_message(), trace_severity() ) -> 'ok'.
echo( TraceMessage, _TraceSeverity=debug ) ->
    debug( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=info ) ->
    info( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=notice ) ->
    notice( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=warning ) ->
    warning( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=error ) ->
    error( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=critical ) ->
    critical( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=alert ) ->
    alert( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=emergency ) ->
    emergency( ?echo_prefix TraceMessage );

echo( _TraceMessage, _TraceSeverity=void ) ->
    ok.



-doc """
Echoes the specified trace in the specified trace severity channel, for
the specified message categorization.

Defined notably to perform integrated operations (a trace being sent through
both a basic system and a more advanced one), in order that the trace macros of
upper layers (e.g. `send_alert_fmt/3`, in the Traces layer) do not need to bind
variables in their body (which may trigger bad matches as soon as more than once
trace is sent in the same scope).
""".
-spec echo( trace_any_message(), trace_severity(),
            trace_message_categorization() ) -> 'ok'.
echo( TraceMessage, _TraceSeverity=debug, MessageCategorization ) ->
    debug_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=info, MessageCategorization ) ->
    info_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=notice, MessageCategorization ) ->
    notice_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=warning, MessageCategorization ) ->
    warning_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=error, MessageCategorization ) ->
    error_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=critical, MessageCategorization ) ->
    critical_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=alert, MessageCategorization ) ->
    alert_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=emergency, MessageCategorization ) ->
    emergency_categorized( TraceMessage, MessageCategorization );

echo( _TraceMessage, _TraceSeverity=void, _MessageCategorization ) ->
    ok.



-doc """
Echoes the specified trace in the specified trace channel, for the specified
message categorization and timestamp.

Defined notably to perform integrated operations (a trace being sent through
both a basic system and a more advanced one), in order that the trace macros of
upper layers (e.g. `send_alert_fmt/3`, in the Traces layer) do not need to bind
variables in their body (which may trigger bad matches as soon as more than once
trace is sent in the same scope).
""".
-spec echo( trace_any_message(), trace_severity(),
            trace_message_categorization(), trace_timestamp() ) -> void().
echo( TraceMessage, _TraceSeverity=debug, MessageCategorization, Timestamp ) ->
    debug_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=info, MessageCategorization, Timestamp ) ->
    info_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=notice, MessageCategorization, Timestamp ) ->
    notice_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=warning, MessageCategorization,
      Timestamp ) ->
    warning_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=error, MessageCategorization, Timestamp ) ->
    error_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=critical, MessageCategorization,
      Timestamp ) ->
    critical_categorized_timed( TraceMessage, MessageCategorization,
                                Timestamp );

echo( TraceMessage, _TraceSeverity=alert, MessageCategorization, Timestamp ) ->
    alert_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=emergency, MessageCategorization,
      Timestamp ) ->
    emergency_categorized_timed( TraceMessage, MessageCategorization,
                                 Timestamp );

echo( _TraceMessage, _TraceSeverity=void, _MessageCategorization,
      _Timestamp ) ->
    ok.



-doc """
Returns the (numerical) priority associated to the specified trace severity
(that is emergency, alert, etc.).

See also its reciprocal `get_severity_for/1`.
""".
-spec get_priority_for( trace_severity() ) -> trace_priority().
% From most common to least:
get_priority_for( debug ) ->
    7;

get_priority_for( info ) ->
    6;

get_priority_for( notice ) ->
    5;

get_priority_for( warning ) ->
    4;

get_priority_for( error ) ->
    3;

get_priority_for( critical ) ->
    2;

get_priority_for( alert ) ->
    1;

get_priority_for( emergency ) ->
    0;

get_priority_for( Other ) ->
    throw( { unexpected_trace_severity, Other } ).

% 'void' not expected here.



-doc """
Returns the trace severity (that is emergency, error, etc.) associated to the
specified (numerical) severity (which corresponds also to a log level, in terms
of the newer standard logger).

See also its reciprocal `get_priority_for/1`.
""".
-spec get_severity_for( trace_priority() ) -> trace_severity().
% From most common to least:
get_severity_for( 7 ) ->
    debug;

get_severity_for( 6 ) ->
    info;

get_severity_for( 5 ) ->
    notice;

get_severity_for( 4 ) ->
    warning;

get_severity_for( 3 ) ->
    error;

get_severity_for( 2 ) ->
    critical;

get_severity_for( 1 ) ->
    alert;

get_severity_for( 0 ) ->
    emergency;

get_severity_for( Other ) ->
    throw( { unexpected_trace_severity, Other } ).

% 'void' never returned here.



-doc """
Tells whether the specified severity belongs to the error-like ones (typically
the ones that must never be missed by the user, hence are echoed on the console
as well).
""".
-spec is_error_like( trace_severity() ) -> boolean().
is_error_like( Severity ) ->
    lists:member( Severity, [ warning, error, critical, alert, emergency ] ).




% Handler section for the integration of Erlang (newer) logger.
%
% Refer to https://erlang.org/doc/man/logger.html.


-doc """
Replaces the current (probably default) logger handler with this Myriad one
(registered as the `default` handler).

Note that Myriad logging defaults will then apply (see `get_handler_config/0`),
which is bound to imply a finer level of reported logs. As a result, after this
function is called (directly or not), new error-like log messages may seem to
appear.
""".
-spec set_handler() -> void().
set_handler() ->

    %debug( "Setting trace_utils logger handler." ),

    TargetHandler = default,

    case logger:remove_handler( TargetHandler ) of

        ok ->
            ok;

        { error, RemoveErrReason } ->
            throw( { unable_to_remove_log_handler, RemoveErrReason,
                     TargetHandler } )

    end,

    case logger:add_handler( _HandlerId=default, _Module=?MODULE,
                             get_handler_config() ) of

        ok ->
            ok;

        { error, AddErrReason } ->
            throw( { unable_to_set_myriad_log_handler, AddErrReason,
                     TargetHandler } )

    end.



-doc """
Registers this Myriad logger handler as an additional one (not replacing the
default one).
""".
-spec add_handler() -> void().
add_handler() ->

    case logger:add_handler( _HandlerId=?myriad_logger_id, _Module=?MODULE,
                             get_handler_config() ) of

        ok ->
            ok;

        { error, Reason } ->
            throw( { unable_to_add_myriad_log_handler, Reason } )

    end.



-doc """
Returns the (initial) configuration of the Myriad logger handler.

Note that Myriad opted for the finest log level, yet due to the primary log
level of logger (which is `notice`), by default `debug` and `info` messages will
still be filtered out. See `logger:set_primary_config/2` or refer to
`trace_utils_test.erl` for extra information.
""".
-spec get_handler_config() -> logger:handler_config().
get_handler_config() ->

    #{ % No configuration needed here by our handler:
       config => undefined

       % Finest default level:
       % level => all,

       % filter_default => log | stop,
       % filters => [],
       % formatter => {logger_formatter, DefaultFormatterConfig}

       % Set by logger:
       %id => HandlerId
       %module => Module
     }.



-doc """
Mandatory callback for log handlers.

See <https://erlang.org/doc/man/logger.html#HModule:log-2>.
""".
-spec log( logger:log_event(), logger:handler_config() ) -> void().
log( _LogEvent=#{ level := Level,
                  %meta => #{error_logger => #{emulator => [...]
                  msg := Msg }, _Config ) ->

    %io:format( "### Logging following event:~n ~p~n(with config: ~p).~n",
    %           [ LogEvent, Config ] ),

     TraceMsg = case Msg of

        { report, Report } ->
            { FmtStr, FmtValues } = logger:format_report( Report ),
            text_utils:format( FmtStr, FmtValues );

        { string, S } ->
            S;

        { FmtStr, FmtValues } ->
            text_utils:format( FmtStr, FmtValues );

        Other ->
            throw( { unexpected_log_message, Other } )

     end,

    % As too often this information is lacking:
    FullTraceMsg = case is_error_like( Level ) of

        true ->
            text_utils:format( "~ts, stacktrace being ~ts",
                [ TraceMsg, code_utils:interpret_stacktrace() ] );

        false ->
            TraceMsg

    end,

    %io:format( "### Logging following event:~n ~p~n(with config: ~p)~n "
    %   "resulting in: '~ts' (severity: ~p).",
    %   [ LogEvent, Config, FullTraceMsg, Severity ] ),

    % Now the standard level corresponds directly to our severity:
    echo( FullTraceMsg, _Severity=Level, 'erlang_logger' );

log( LogEvent, _Config ) ->
    throw( { unexpected_log_event, LogEvent } ).



-doc """
Sets the logger maximum depth when formatting messages.

This allows limiting the length of the error logger output in crashes.
""".
-spec set_logger_format_max_depth( text_utils:depth() ) -> void().
set_logger_format_max_depth( Depth ) ->

    application:set_env( kernel, error_logger_format_depth, Depth ),

    % Force a reset, so that the previous depth applies:
    error_logger:tty( false ),
    error_logger:tty( true ).




% Helper section.



-doc """
Displays the specified message.

Note: adds a carriage-return/line-feed at the end of the message.

(helper, to provide a level of indirection)
""".
-spec severe_display( trace_message() ) -> 'ok'.
severe_display( Message ) ->

    Bar = "----------------",

    % Sometimes legit messages may be already terminated by a newline (e.g. if
    % using text_utils:strings_to_string/1); avoiding an extraneous blank line
    % between the message and the final bar:
    %
    % (initial newline finally kept, otherwise may be displayed at the end of an
    % unrelated line)
    %
    Str = "\n<" ++ Bar ++ "\n"
        ++ text_utils:ensure_newline_terminated( Message ) ++ Bar ++ ">",

    % Could be also error_logger:info_msg/1 for example:
    actual_display( Str ),
    system_utils:await_output_completion().



-doc """
Displays the specified message.

Note:
- adds a carriage-return/line-feed at the end of the message
- some callers may rely on the fact that this function actually returns 'ok'
  (e.g. to contrast it with other calls, like in the trace_bridge module

(helper, to provide a level of indirection)
""".
-spec actual_display( trace_message() ) -> 'ok'.
actual_display( Message ) ->

    %io:format( "Current error output setting: ~ts.~n",
    %           [ basic_utils:get_error_report_output() ] ),

    % For a lower-level trace management like this one, based on console
    % printouts, we used to ellipse longer messages as they are mostly
    % unreadable anyway:
    %
    %RetainedMsg = text_utils:ellipse( Message, _DefaultMaxLen=2500 ),

    % Now we apply the overall error report output settings, which is better
    % (more flexible):
    %
    RetainedMsg = case code_utils:get_error_report_output_ellipsings() of

         { _MaybeStdOutputEllipseLen=undefined, _ForFile } ->
             Message;

         { StdOutputEllipseLen, _ForFile } ->
             text_utils:ellipse( Message, StdOutputEllipseLen )

    end,


    % Not wanting a space before any opening bracket, so that "[TIMESTAMP][SEV]
    % [EMITTER]" becomes "[TIMESTAMP][SEV][EMITTER]":
    %
    FinalMsg = case RetainedMsg of

        [ $ , $[ | T ] ->
            [ $[ | T ];

        _ ->
            RetainedMsg

    end,

    % This default timeout (30 seconds, in milliseconds) may not be sufficient
    % in all cases:
    %
    %basic_utils:display_timed( RetainedMsg, _MsTimeOut=30000 ).

    % If wanting a faster, less safe version (ending newline must be kept):
    %
    % (now timestamped, as more useful for example in erlang.log.* files)
    %
    io:format( "[~ts]~ts~n",
               [ time_utils:get_textual_timestamp(), FinalMsg ] ).



-doc """
Displays the specified message.

Note: adds a carriage-return/line-feed at the end of the message.
""".
-spec safer_display( trace_message() ) -> 'ok'.
safer_display( Message ) ->

    % This default timeout (30 seconds, in milliseconds) may not be sufficient
    % in all cases:
    %
    basic_utils:display_timed( Message, _MsTimeOut=30000 ).



-doc """
Displays the specified format-based message, in a safer way.

Useful when debugging.

Note: adds a carriage-return/line-feed at the end of the message.
""".
-spec safer_display( trace_format(), trace_values() ) -> 'ok'.
safer_display( Format, Values ) ->
    safer_display( text_utils:format( Format, Values ) ).


-doc """
Offsets the specified message: adds a leading space iff its first character is
not an opening bracket, so that `[TIMESTAMP][SEV] [EMITTER]` becomes
`[TIMESTAMP][SEV][EMITTER]`.
""".
-spec offset( ustring() ) -> ustring().
% Do nothing if starting with an opening bracket:
offset( S=[ $[ | _T ] ) ->
    S;

offset( S ) ->
    % Otherwise add a leading space:
    [ $ | S ].
