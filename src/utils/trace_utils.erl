% Copyright (C) 2017-2017 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Friday, November 1, 2013.


% Gathering of various very low-level trace-related facilities on the console.
%
% They are mostly useful so that their call can be replaced by calls to the far
% more advanced facilities of the 'Traces' layer with no change in their
% parameters.
%
% For example, 'trace_utils:debug_fmt( "I am the ~B.", [ 1 ] )' may be replaced
% by '?debug_fmt( "I am the ~B.", [ 1 ] )' to switch from basic traces on the
% console to traces on the Traces subsystem.
%
% As a result, for a trace severity S in [ debug, trace, info, warning, error,
% fatal ], 'trace_utils:S' may be replaced as a whole by '?S' to promote a very
% debug-oriented trace into a more durable one.
%
% See trace_utils_test.erl for testing.
%
-module(trace_utils).


% To resolve name clash:
-compile( { no_auto_import, [ error/1 ] } ).


% An actual trace message:
-type trace_message() :: string().


% 7 levels of severity, from least important to most: debug, trace, info,
% warning, error and fatal (void being always muted):
%
-type trace_severity() ::  'debug'| 'trace' | 'info'
						 | 'warning' | 'error' | 'fatal' | 'void'.


% A format with quantifiers (such as ~p):
-type trace_format() :: string().

% Values corresponding to format quantifiers:
-type trace_values() :: [ any() ] .

% Categorization of a trace message:
-type trace_message_categorization() :: string().

% An applicative timestamp for a trace:
-type trace_timestamp() :: string().


-export_type([ trace_message/0, trace_severity/0,
			   trace_format/0, trace_values/0, trace_message_categorization/0,
			   trace_timestamp/0 ]).



-export([ debug/1, debug_fmt/2, debug_categorized/2, debug_categorized_timed/3,

		  trace/1, trace_fmt/2, trace_categorized/2, trace_categorized_timed/3,

		  info/1, info_fmt/2, info_categorized/2, info_categorized_timed/3,

		  warning/1, warning_fmt/2, warning_categorized/2,
		  warning_categorized_timed/3,

		  error/1, error_fmt/2, error_categorized/2, error_categorized_timed/3,

		  fatal/1, fatal_fmt/2, fatal_categorized/2, fatal_categorized_timed/3,

		  echo/2, echo/3, echo/4

		]).



% Implementation notes:
%
% Compared to mere io:format/{1,2} calls, these trace primitives add
% automatically the trace type (ex: "[debug] ") at the beginning of the message,
% finishes it with a carriage-return/line-feed, and for the most important trace
% types, try to ensure they are synchronous (blocking).



% Outputs specified debug message.
%
-spec debug( trace_message() ) -> basic_utils:void().
debug( Message ) ->
	actual_display( "[debug] " ++ Message ).


% Outputs specified formatted debug message.
%
-spec debug_fmt( trace_format(), trace_values() ) -> basic_utils:void().
debug_fmt( Format, Values ) ->
	actual_display( "[debug] " ++ Format, Values ).


% Outputs specified debug message, with specified message categorization.
%
-spec debug_categorized( trace_message(), trace_message_categorization() ) ->
							   basic_utils:void().
debug_categorized( Message, _MessageCategorization=uncategorized ) ->
	actual_display( "[debug] ~s", [ Message ] );

debug_categorized( Message, MessageCategorization ) ->
	actual_display( "[debug][~s] ~s", [ MessageCategorization, Message ] ).


% Outputs specified debug message, with specified message categorization and
% time information.
%
-spec debug_categorized_timed( trace_message(), trace_message_categorization(),
							   trace_timestamp() ) -> basic_utils:void().
debug_categorized_timed( Message, _MessageCategorization=uncategorized,
						 Timestamp ) ->
	actual_display( "[debug][at ~s] ~s", [ Timestamp, Message ] );

debug_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	actual_display( "[debug][~s][at ~s] ~s",
					[ MessageCategorization, Timestamp, Message ] ).



% Outputs specified trace message.
%
-spec trace( trace_message() ) -> basic_utils:void().
trace( Message ) ->
	actual_display( "[trace] " ++ Message ).


% Outputs specified formatted trace message.
%
-spec trace_fmt( trace_format(), trace_values() ) -> basic_utils:void().
trace_fmt( Format, Values ) ->
	actual_display( "[trace] " ++ Format, Values ).


% Outputs specified trace message, with specified message categorization.
%
-spec trace_categorized( trace_message(), trace_message_categorization() ) ->
							   basic_utils:void().
trace_categorized( Message, _MessageCategorization=uncategorized ) ->
	actual_display( "[trace] ~s", [ Message ] );

trace_categorized( Message, MessageCategorization ) ->
	actual_display( "[trace][~s] ~s", [ MessageCategorization, Message ] ).


% Outputs specified trace message, with specified message categorization and
% time information.
%
-spec trace_categorized_timed( trace_message(), trace_message_categorization(),
							   trace_timestamp() ) -> basic_utils:void().
trace_categorized_timed( Message, _MessageCategorization=uncategorized,
						 Timestamp ) ->
	actual_display( "[trace][at ~s] ~s", [ Timestamp, Message ] );

trace_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	actual_display( "[trace][~s][at ~s] ~s",
					[ MessageCategorization, Timestamp, Message ] ).



% Outputs specified info message.
%
-spec info( trace_message() ) -> basic_utils:void().
info( Message ) ->
	actual_display( "[info] " ++ Message ).


% Outputs specified formatted info message.
%
-spec info_fmt( trace_format(), trace_values() ) -> basic_utils:void().
info_fmt( Format, Values ) ->
	actual_display( "[info] " ++ Format, Values ).


% Outputs specified info message, with specified message categorization.
%
-spec info_categorized( trace_message(), trace_message_categorization() ) ->
							  basic_utils:void().
info_categorized( Message, _MessageCategorization=uncategorized ) ->
	actual_display( "[info] ~s", [ Message ] );

info_categorized( Message, MessageCategorization ) ->
	actual_display( "[info][~s] ~s", [ MessageCategorization, Message ] ).


% Outputs specified info message, with specified message categorization and time
% information.
%
-spec info_categorized_timed( trace_message(), trace_message_categorization(),
							  trace_timestamp() ) -> basic_utils:void().
info_categorized_timed( Message, _MessageCategorization=uncategorized,
						Timestamp ) ->
	actual_display( "[info][at ~s] ~s", [ Timestamp, Message ] );

info_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	actual_display( "[info][~s][at ~s] ~s",
					[ MessageCategorization, Timestamp, Message ] ).



% Outputs specified warning message.
%
-spec warning( trace_message() ) -> basic_utils:void().
warning( Message ) ->
	severe_display( "[warning] " ++ Message ),
	system_utils:await_output_completion().


% Outputs specified formatted warning message.
%
-spec warning_fmt( trace_format(), trace_values() ) ->
						 basic_utils:void().
warning_fmt( Format, Values ) ->
	severe_display( "[warning] " ++ Format, Values ),
	system_utils:await_output_completion().


% Outputs specified warning message, with specified message categorization.
%
-spec warning_categorized( trace_message(), trace_message_categorization() ) ->
								 basic_utils:void().
warning_categorized( Message, _MessageCategorization=uncategorized ) ->
	severe_display( "[warning] ~s", [ Message ] );

warning_categorized( Message, MessageCategorization ) ->
	severe_display( "[warning][~s] ~s", [ MessageCategorization, Message ] ).


% Outputs specified warning message, with specified message categorization and
% time information.
%
-spec warning_categorized_timed( trace_message(),
		trace_message_categorization(), trace_timestamp() ) ->
									   basic_utils:void().
warning_categorized_timed( Message, _MessageCategorization=uncategorized,
						   Timestamp ) ->
	severe_display( "[warning][at ~s] ~s",
					[ Timestamp, Message ] );

warning_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	severe_display( "[warning][~s][at ~s] ~s",
					[ MessageCategorization, Timestamp, Message ] ).



% Outputs specified error message.
%
-spec error( trace_message() ) -> basic_utils:void().
error( Message ) ->
	severe_display( "[error] " ++ Message ),
	system_utils:await_output_completion().


% Outputs specified formatted error message.
%
-spec error_fmt( trace_format(), trace_values() ) -> basic_utils:void().
error_fmt( Format, Values ) ->
	severe_display( "[error] " ++ Format, Values ),
	system_utils:await_output_completion().


% Outputs specified error message, with specified message categorization.
%
-spec error_categorized( trace_message(), trace_message_categorization() ) ->
							   basic_utils:void().
error_categorized( Message, _MessageCategorization=uncategorized ) ->
	severe_display( "[error] ~s", [ Message ] );

error_categorized( Message, MessageCategorization ) ->
	severe_display( "[error][~s] ~s", [ MessageCategorization, Message ] ).


% Outputs specified error message, with specified message categorization and
% time information.
%
-spec error_categorized_timed( trace_message(), trace_message_categorization(),
							  trace_timestamp() ) -> basic_utils:void().
error_categorized_timed( Message, _MessageCategorization=uncategorized,
						 Timestamp ) ->
	severe_display( "[error][at ~s] ~s", [ Timestamp, Message ] );

error_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	severe_display( "[error][~s][at ~s] ~s",
					[ MessageCategorization, Timestamp, Message ] ).



% Outputs specified fatal message.
%
-spec fatal( trace_message() ) -> basic_utils:void().
fatal( Message ) ->
	severe_display( "[fatal] " ++ Message ),
	system_utils:await_output_completion().


% Outputs specified formatted fatal message.
%
-spec fatal_fmt( trace_format(), trace_values() ) -> basic_utils:void().
fatal_fmt( Format, Values ) ->
	severe_display( "[fatal] " ++ Format, Values ),
	system_utils:await_output_completion().


% Outputs specified fatal message, with specified message categorization.
%
-spec fatal_categorized( trace_message(), trace_message_categorization() ) ->
							   basic_utils:void().
fatal_categorized( Message, _MessageCategorization=uncategorized ) ->
	severe_display( "[fatal] ~s", [ Message ] );

fatal_categorized( Message, MessageCategorization ) ->
	severe_display( "[fatal][~s] ~s", [ MessageCategorization, Message ] ).


% Outputs specified fatal message, with specified message categorization and
% time information.
%
-spec fatal_categorized_timed( trace_message(), trace_message_categorization(),
							  trace_timestamp() ) -> basic_utils:void().
fatal_categorized_timed( Message, _MessageCategorization=uncategorized,
						 Timestamp ) ->
	severe_display( "[fatal][at ~s] ~s", [ Timestamp, Message ] );

fatal_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	severe_display( "[fatal][~s][at ~s] ~s",
					[ MessageCategorization, Timestamp, Message ] ).



% Echoes specified trace in specified trace channel.
%
% Defined notably to perform integrated operations (a trace being sent through
% both a basic system and a more advanced one), in order that the trace macros
% of upper layers (ex: send_fatal_fmt/3, in the Traces layer) do not need to
% bind variables in their body (which may trigger bad matches as soon as more
% than once trace is sent in the same scope).
%
-spec echo( trace_message(), trace_severity() ) -> basic_utils:void().
echo( TraceMessage, _TraceSeverity=debug ) ->
	debug( TraceMessage );

echo( TraceMessage, _TraceSeverity=trace ) ->
	trace( TraceMessage );

echo( TraceMessage, _TraceSeverity=info ) ->
	info( TraceMessage );

echo( TraceMessage, _TraceSeverity=warning ) ->
	warning( TraceMessage );

echo( TraceMessage, _TraceSeverity=error ) ->
	error( TraceMessage );

echo( TraceMessage, _TraceSeverity=fatal ) ->
	fatal( TraceMessage );

echo( _TraceMessage, _TraceSeverity=void ) ->
	ok.



% Echoes specified trace in specified trace channel, for specified message
% categorization.
%
% Defined notably to perform integrated operations (a trace being sent through
% both a basic system and a more advanced one), in order that the trace macros
% of upper layers (ex: send_fatal_fmt/3, in the Traces layer) do not need to
% bind variables in their body (which may trigger bad matches as soon as more
% than once trace is sent in the same scope).
%
-spec echo( trace_message(), trace_severity(),
			trace_message_categorization() ) -> basic_utils:void().
echo( TraceMessage, _TraceSeverity=debug, MessageCategorization ) ->
	debug_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=trace, MessageCategorization ) ->
	trace_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=info, MessageCategorization ) ->
	info_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=warning, MessageCategorization ) ->
	warning_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=error, MessageCategorization ) ->
	error_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=fatal, MessageCategorization ) ->
	fatal_categorized( TraceMessage, MessageCategorization );

echo( _TraceMessage, _TraceSeverity=void, _MessageCategorization ) ->
	ok.



% Echoes specified trace in specified trace channel, for specified message
% categorization and timestamp.
%
% Defined notably to perform integrated operations (a trace being sent through
% both a basic system and a more advanced one), in order that the trace macros
% of upper layers (ex: send_fatal_fmt/3, in the Traces layer) do not need to
% bind variables in their body (which may trigger bad matches as soon as more
% than once trace is sent in the same scope).
%
-spec echo( trace_message(), trace_severity(), trace_message_categorization(),
			trace_timestamp() ) -> basic_utils:void().
echo( TraceMessage, _TraceSeverity=debug, MessageCategorization, Timestamp ) ->
	debug_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=trace, MessageCategorization, Timestamp ) ->
	trace_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=info, MessageCategorization, Timestamp ) ->
	info_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=warning, MessageCategorization,
	  Timestamp ) ->
	warning_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=error, MessageCategorization, Timestamp ) ->
	error_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=fatal, MessageCategorization, Timestamp ) ->
	fatal_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( _TraceMessage, _TraceSeverity=void, _MessageCategorization,
	  _Timestamp ) ->
	ok.




% Helper section.



% Displays specified message.
%
% Note: adds a carriage-return/line-feed at the end of the message.
%
% (helper, to provide a level of indirection)
%
-spec severe_display( trace_message() ) -> basic_utils:void().
severe_display( Message ) ->

	Bar = "----------------",

	% Could be also error_logger:info_msg/1 for example:
	actual_display( "\n<" ++ Bar ++ "\n" ++ Message ++ "\n" ++ Bar ++ ">\n" ).



% Displays specified format-based message.
%
% Note: adds a carriage-return/line-feed at the end of the message.
%
% (helper, to provide a level of indirection)
%
-spec severe_display( trace_format(), trace_values() ) ->
							basic_utils:void().
severe_display( Format, Values ) ->
	Message = text_utils:format( Format, Values ),
	severe_display( Message ).



% Displays specified message.
%
% Note: adds a carriage-return/line-feed at the end of the message.
%
% (helper, to provide a level of indirection)
%
-spec actual_display( trace_message() ) -> basic_utils:void().
actual_display( Message ) ->
	% Default-timeout may not be sufficient (30 seconds, in milliseconds)
	basic_utils:display_timed( Message, _TimeOut=30000 ).



% Displays specified format-based message.
%
% Note: adds a carriage-return/line-feed at the end of the message.
%
% (helper, to provide a level of indirection)
%
-spec actual_display( trace_format(), trace_values() ) ->
							basic_utils:void().
actual_display( Format, Values ) ->
	basic_utils:display( Format, Values ).
