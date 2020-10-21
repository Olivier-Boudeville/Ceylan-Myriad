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
% Creation date: Sunday, October 18, 2020.



% The trace bridge allows a software to depend only on the Ceylan-Myriad layer,
% yet to be able (optionally) to be using another piece of software (possibly
% the Ceylan-Traces layer, refer to http://traces.esperide.org/) at runtime for
% its logging, so that in all cases exactly one (and the most appropriate)
% logging system is used, even when lower-level libraries are involved.
%
% It is useful to provide native, integrated, higher-level logging to basic
% libraries (ex: LEEC, see
% https://github.com/Olivier-Boudeville/letsencrypt-erlang), should their user
% require it - while being able to remain lean and mean if wanted.
%
-module(trace_bridge).


% Suspiciously akin to Ceylan-Traces conventions:

%-type trace_emitter_name() :: bin_string().
%-type trace_categorization() :: bin_string().

% Possibly a class_TraceAggregator:aggregator_pid():
-type bridge_pid() :: pid().

% An actual trace message:
-type trace_message() :: ustring().

-export_type([ bridge_pid/0 ]).


-export([ register/3, set_application_timestamp/1, unregister/0,
		  debug/1, debug_fmt/2, trace/1, trace_fmt/2,
		  info/1, info_fmt/2, warning/1, warning_fmt/2,
		  error/1, error_fmt/2, fatal/1, fatal_fmt/2,
		  void/1, void_fmt/2 ]).


% Keys defined in the process dictionary:
-define( myriad_trace_bridge_key, "_myriad_trace_bridge" ).



% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type trace_severity() :: trace_utils:trace_severity().
-type trace_timestamp() :: trace_utils:trace_timestamp().



% Implementation notes:
%
% The process dictionary is used in order to avoid carrying along too many
% parameters.
%
% Note special-casing the 'void' severity, as not used frequently enough.


-type bridge_info() :: { TraceEmitterName :: bin_string(),
						 TraceCategory :: bin_string(),
						 Location :: bin_string(),
						 ApplicationTimestamp :: trace_timestamp() }.


% To silence warning:
-export_type([ bridge_info/0 ]).



% Registers the current process as a trace bridge.
-spec register( ustring(), ustring(), bridge_pid() ) -> void().
register( TraceEmitterName, TraceCategory, BridgePid ) ->

	BridgeKey = ?myriad_trace_bridge_key,

	BinName = text_utils:string_to_binary( TraceEmitterName ),
	BinCateg = text_utils:string_to_binary( TraceCategory ),
	Location = net_utils:localnode_as_binary(),
	DefaultApplicationTimestamp = undefined,

	BridgeInfo = { BinName, BinCateg, Location, BridgePid,
				   DefaultApplicationTimestamp },

	case process_dictionary:get( BridgeKey ) of

		% Normal case:
		undefined ->
			process_dictionary:put( BridgeKey, BridgeInfo );

		UnexpectedInfo ->
			throw( { myriad_trace_bridge_already_registered, UnexpectedInfo,
					 BridgeInfo } )

	end.



% Sets the current application timestamp.
%
% Note: if no trace bridge is registered, does nothing.
%
-spec set_application_timestamp( trace_timestamp() ) -> void().
set_application_timestamp( NewAppTimestamp ) ->

	BridgeKey = ?myriad_trace_bridge_key,

	case process_dictionary:get( BridgeKey ) of

		undefined ->
			ok;

		BridgeInfo ->
			NewBridgeInfo = setelement( _AppTmspIndex=5, BridgeInfo,
										NewAppTimestamp ),

			process_dictionary:put( BridgeKey, NewBridgeInfo )

	end.



% Unregisters the current process, which acted as a trace bridge; never fails.
-spec unregister() -> void().
unregister() ->
	% No-op if not set:
	process_dictionary:remove( _K=?myriad_trace_bridge_key ).



% Primitives for trace emission.


% Outputs specified debug message.
-spec debug( trace_message() ) -> void().
debug( Message ) ->
	send( debug, Message ).


% Outputs specified debug message to format.
-spec debug_fmt( format_string(), format_values() ) -> void().
debug_fmt( MessageFormat, MessageValues ) ->
	send( debug, MessageFormat, MessageValues ).



% Outputs specified trace message.
-spec trace( trace_message() ) -> void().
trace( Message ) ->
	send( trace, Message ).


% Outputs specified trace message to format.
-spec trace_fmt( format_string(), format_values() ) -> void().
trace_fmt( MessageFormat, MessageValues ) ->
	send( trace, MessageFormat, MessageValues ).



% Outputs specified info message.
-spec info( trace_message() ) -> void().
info( Message ) ->
	send( info, Message ).


% Outputs specified info message to format.
-spec info_fmt( format_string(), format_values() ) -> void().
info_fmt( MessageFormat, MessageValues ) ->
	send( info, MessageFormat, MessageValues ).



% Outputs specified warning message.
-spec warning( trace_message() ) -> void().
warning( Message ) ->
	send( warning, Message ).


% Outputs specified warning message to format.
-spec warning_fmt( format_string(), format_values() ) -> void().
warning_fmt( MessageFormat, MessageValues ) ->
	send( warning, MessageFormat, MessageValues ).



% Outputs specified error message.
-spec error( trace_message() ) -> void().
error( Message ) ->
	send( error, Message ).


% Outputs specified error message to format.
-spec error_fmt( format_string(), format_values() ) -> void().
error_fmt( MessageFormat, MessageValues ) ->
	send( error, MessageFormat, MessageValues ).



% Outputs specified fatal message.
-spec fatal( trace_message() ) -> void().
fatal( Message ) ->
	send( fatal, Message ).


% Outputs specified fatal message to format.
-spec fatal_fmt( format_string(), format_values() ) -> void().
fatal_fmt( MessageFormat, MessageValues ) ->
	send( fatal, MessageFormat, MessageValues ).



% "Outputs" specified void message.
-spec void( trace_message() ) -> void().
void( _Message ) ->
	ok.


% "Outputs" specified void message to format.
-spec void_fmt( format_string(), format_values() ) -> void().
void_fmt( _MessageFormat, _MessageValues ) ->
	ok.



% (helper)
-spec send( trace_severity(), trace_message() ) -> void().
send( SeverityType, Message ) ->

	case process_dictionary:get( ?myriad_trace_bridge_key ) of

		% No bridge set, using the direct, basic Myriad traces:
		undefined ->
			trace_utils:SeverityType( Message );

		% A bridge is available; mimicking the Ceylan-Traces protocol:
		BridgeInfo ->
			send_bridge( SeverityType, Message, BridgeInfo )

	end.



% (helper)
-spec send( trace_severity(), format_string(), format_values() ) -> void().
send( SeverityType, MessageFormat, MessageValues ) ->

	Message = text_utils:format( MessageFormat, MessageValues ),

	case process_dictionary:get( ?myriad_trace_bridge_key ) of

		% No bridge set, using the direct, basic Myriad traces:
		undefined ->
			trace_utils:SeverityType( Message );

		% A bridge is available:
		BridgeInfo ->
			send_bridge( SeverityType, Message, BridgeInfo )

	end.



% Mimicking the Ceylan-Traces protocol.
%
% (helper)
send_bridge( SeverityType, Message,
			 _BridgeInfo={ TraceEmitterName, TraceEmitterCategorization,
						   BinLocation, BridgePid, AppTimestamp } ) ->

	AppTimestampString = text_utils:term_to_binary( AppTimestamp ),

	TimestampText = text_utils:string_to_binary(
					  time_utils:get_textual_timestamp() ),

	BridgePid ! { send,
		[ _TraceEmitterPid=self(),
		  TraceEmitterName,
		  TraceEmitterCategorization,
		  AppTimestampString,
		  _Time=TimestampText,
		  _Location=BinLocation,
		  _MessageCategorization='Trace Bridge',
		  %_MessageCategorization=uncategorized,
		  _Priority=trace_utils:get_priority_for( SeverityType ),
		  _Message=text_utils:string_to_binary( Message ) ] }.
