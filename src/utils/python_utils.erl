% Copyright (C) 2016-2017 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Robin Huart (robin-externe.huart@edf.fr)



% Gathering of some convenient facilities for the binding to the Python language
%
% See python_utils_test.erl for the corresponding tests.
%
-module(python_utils).



% Helper exports:
-export([ get_beam_directories_for_binding/0,
		  send_oneway/3, wait_for_request_result/2,
		  pep8_class_to_pep8_module/1 ]).



% PID of a Python interpreter:
-type interpreter_pid() :: pid().


% The title of a request sent to Python.
-type title() ::  atom().


% The parameters of a request sent to Python.
-type body() :: [ any() ].


% The result from a request that was sent to Python.
-type result() :: any().


% The name of a Python class, according to the PEP8:
-type pep8_class_name() :: atom().


% The name of a Python module, according to the PEP8:
-type pep8_class_module() :: atom().


-export_type([ interpreter_pid/0, title/0, body/0, result/0,
			   pep8_class_name/0, pep8_class_module/0 ]).



% Implementation notes:
%
% The actual Erlang-Python binding is obtained thanks to the ErlPort library
% (http://erlport.org/; source in https://github.com/hdima/erlport), to be
% installed in ~/Software/ErlPort/ErlPort-current-install.
%
% Please refer to the 'Prerequisite section' of system_utils.erl for more
% guidance about installation paths.



% Finds the BEAM locations of all the dependencies required for binding with
% Python.
%
-spec get_beam_directories_for_binding() -> [ file_utils:directory_name() ].
get_beam_directories_for_binding() ->

	% A single directory is apparently necessary and sufficient so that the
	% computing nodes have access both to ErlPort.
	%
	% Previously we thought that system_utils:get_dependency_base_directory(
	% "ErlPort" ) was needed as well, so that initialization of the interpreters
	% would not fail with {not_found,"erlport/priv"}, short of being able to
	% find erlport.beam, yet it was due to the base directory bearing a
	% different name than 'erlport' in our installation settings.

	% Needed so that python:start/1 and all are found:
	[ system_utils:get_dependency_code_directory( "ErlPort" ) ].



% Requests specified interpreter to execute specified oneway.
%
-spec send_oneway( interpreter_pid(), title(), body() ) -> basic_utils:void().
send_oneway( InterpreterPid, MessageTitle, MessageBody )
  when is_atom( MessageTitle ) ->

	% Simple wrapper around ErlPort's cast method, sending the title and the
	% body of a message separately.
	%
	EncodedMessageTitle = text_utils:atom_to_binary( MessageTitle ),

	python:cast( InterpreterPid, { self(), EncodedMessageTitle, MessageBody } ).



% Receives a message from the Python world, usually in answer to a send_oneway/3
% call having used the same MessageTitle argument, and tries to match it with
% the different accepted types of messages.
%
-spec wait_for_request_result( interpreter_pid(), title() ) -> result().
wait_for_request_result( InterpreterPid, MessageTitle )
  when is_atom( MessageTitle ) ->

	% Waits for the answer:
	receive

		% Normal, successful case:
		_SuccessMessage={ <<"request_completed">>, ReceivedData } ->
			{ request_completed, ReceivedData };


		% Trace emitted from Python:
		_TraceMessage={ <<"trace_emitted">>, TraceType,
						  TraceFormattedMessage }
		  when is_binary( TraceType ) andalso
			   is_binary( TraceFormattedMessage ) ->

			{ trace_emitted, text_utils:binary_to_atom( TraceType ),
			  text_utils:binary_to_string( TraceFormattedMessage ) };


		% Exception raised from Python:
		_ExceptionMessage={ <<"exception_raised">>, ExceptionType,
							  ExceptionFormattedMessage } when
			  is_binary( ExceptionType ) andalso
			  is_binary( ExceptionFormattedMessage )  ->

			{ exception_raised, text_utils:binary_to_atom( ExceptionType ),
			  text_utils:binary_to_string( ExceptionFormattedMessage ) };


		% Catch-all clause for message receiving:
		OtherMessage ->
			io:format( "A message received from the Python interpreter "
					   "driven by ~w, in answer to '~p', does not respect "
					   "the expected format: ~p~n",
					   [ InterpreterPid, MessageTitle, OtherMessage ] ),
			throw( { invalid_python_message_received, OtherMessage } )

	end.



% Deduces the name of a Python file (module) from the name of the Python class
% that it implements, according the naming conventions adopted in PEP 8.
%
% Ex: 'MyFoobarExample' resulting in 'my_foobar_example'.
%
-spec pep8_class_to_pep8_module( pep8_class_name() | string() ) ->
									   pep8_class_module().
pep8_class_to_pep8_module( ClassName ) when is_atom( ClassName ) ->
	pep8_class_to_pep8_module( text_utils:atom_to_string( ClassName ) );

pep8_class_to_pep8_module( ClassNameString ) ->

	CapitalizedWords = text_utils:split_camel_case( ClassNameString ),

	LowercaseWords = [ string:to_lower( Word ) || Word <- CapitalizedWords ],

	text_utils:string_to_atom( string:join( LowercaseWords, "_" ) ).
