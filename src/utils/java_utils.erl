% Copyright (C) 2007-2017 Olivier Boudeville
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
% Adapted from code contributed by EDF R&D (original author: Robin Huart).


% Gathering of some convenient facilities for the binding to the Java language
%
% See java_utils_test.erl for the corresponding tests.
%
% See also: python_utils.erl for a similar binding.
%
-module(java_utils).



% Helper exports:
-export([ send_oneway/3, wait_for_request_result/2,
		  java_class_to_file_name/1 ]).



% PID associated to a Java mailbox:
-type java_mbox_pid() :: pid().


% The title of a request sent to Java.
-type title() ::  atom().


% The parameters of a request sent to Java.
-type body() :: [ any() ].


% The result from a request that was sent to Java.
-type result() :: any().


% The name of a Java class:
-type java_class_name() :: atom().


% The name of a Java file (*.java):
-type java_file() :: atom().


-export_type([ java_mbox_pid/0, title/0, body/0, result/0,
			   java_class_name/0, java_file/0 ]).



% Implementation notes:

% The actual Erlang-Java binding is obtained thanks to the native Jinterface
% package.
%
% See http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html
% for more information.
%
% On the Java side, the equivalent instance of an Erlang node is an OtpNode
% (approximately a JVM) and messages can be sent and/or received through
% instances of mailboxes (OtpMbox) that are associated to a PID.
%
% From the point of view of an Erlang process, such a PID can be treated as if
% it was also an Erlang process.



% Requests specified interpreter to execute specified oneway.
%
-spec send_oneway( java_mbox_pid(), title(), body() ) -> basic_utils:void().
send_oneway( MailboxPid, MessageTitle, MessageBody )
  when is_atom( MessageTitle ) ->

	MailboxPid ! { self(), MessageTitle, MessageBody }.



% Receives a message from the Java world, usually in answer to a send_oneway/3
% call having used the same MessageTitle argument, and tries to match it with
% the different accepted types of messages.
%
-spec wait_for_request_result( java_mbox_pid(), title() ) -> result().
wait_for_request_result( MailboxPid, MessageTitle )
  when is_atom( MessageTitle ) ->

	% Waits for the response:
	Message = receive

		_Msg={ Headers, Body } when is_tuple( Headers ) andalso
							erlang:element( 1, Headers ) == java_message ->

			erlang:append_element( erlang:delete_element( 1, Headers ), Body )

	end,

	case Message of

		% Return of a successful request:
		{ request_completed, _ReceivedData } ->

			Message;

		% Trace emitted from Java:
		TraceMessage = { trace_emitted, TraceType, _TraceFormattedMessage }
		  when is_atom( TraceType ) ->

			TraceMessage;


		% Exception raised from Java:
		ExceptionMessage = { exception_raised, ExceptionType,
							 _ExceptionFormattedMessage }
		  when is_atom( ExceptionType ) ->

			ExceptionMessage;


		% Catch-all clause for message receiving:
		OtherMessage ->
			trace_utils:error_fmt( "A message received from a Java (Jinterface)"
								   " OtpMbox driven by ~w, in answer to '~p', "
								   "does not respect the expected format: ~p~n",
								   [ MailboxPid, MessageTitle, OtherMessage ] ),
			throw( { invalid_java_message_received, OtherMessage } )

	end.



% Deduces the (root) name of a Java file (source code for a class) from the name
% of the class it implements, according to the naming conventions used by the
% language.
%
% With Java, both names are identical (hence we just check if the name looks
% CamelCased, i.e. if at least its first letter is in upper case).
%
-spec java_class_to_file_name( java_class_name() | string() ) -> java_file().
java_class_to_file_name( ClassName ) when is_atom( ClassName ) ->
	java_class_to_file_name( text_utils:atom_to_string( ClassName ) );

java_class_to_file_name( ClassNameString ) ->

	case text_utils:is_uppercase( ClassNameString ) of

		true ->
			text_utils:string_to_atom( ClassNameString );

		false ->
			throw( { java_classname_not_camelcased, ClassNameString } )

	end.
