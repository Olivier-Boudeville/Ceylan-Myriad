% Copyright (C) 2007-2016 Olivier Boudeville
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
% Creation date: July 1, 2007.


% Gathering of various convenient facilities.
%
% See basic_utils_test.erl for the corresponding test.
%
-module(basic_utils).


% For list_impl:
-include("data_types.hrl").



% Registration functions.
%
-export([ register_as/2, register_as/3, register_or_return_registered/2,
		  unregister/2,

		  get_registered_pid_for/1, get_registered_pid_for/2,
		  get_locally_registered_pid_for/2,

		  get_registered_names/1,

		  is_registered/1, is_registered/2,
		  wait_for_global_registration_of/1, wait_for_global_registration_of/2,
		  wait_for_local_registration_of/1,
		  wait_for_remote_local_registrations_of/2,
		  display_registered/0 ]).



% Notification-related functions.
%
-export([ speak/1, notify_user/1, notify_user/2 ]).



% Message-related functions.
%
-export([ flush_pending_messages/0, flush_pending_messages/1,
		  wait_for/2, wait_for/4, wait_for_acks/4, wait_for_acks/5,
		  wait_for_summable_acks/5,
		  wait_for_many_acks/4, wait_for_many_acks/5,
		  send_to_pid_list_impl/2 ]).



% Miscellaneous functions.
%
-export([ size/1, display_process_info/1,
		  checkpoint/1, display/1, display/2, display_error/1, display_error/2,
		  debug/1, debug/2,
		  parse_version/1, compare_versions/2,
		  get_process_specific_value/0, get_process_specific_value/2,
		  get_execution_target/0,
		  is_alive/1, is_alive/2,
		  is_debug_mode_enabled/0,
		  generate_uuid/0, create_uniform_tuple/2,
		  stop/0, stop/1, stop_on_success/0, stop_on_failure/0,
		  stop_on_failure/1,
		  crash/0, enter_infinite_loop/0, trigger_oom/0 ]).



% Hints about retrieving the name of the function being currently evaluated by a
% process (as a ?FUNCTION macro could do):
%
% - either:
%
% current_function() ->
%    catch throw( x ), [_, {_, F, _, _} | _] = erlang:get_stacktrace(),
%    F.
%
% - or, maybe better:
%
% erlang:element( 2, erlang:element( 2, erlang:process_info( self(),
%   current_function ) ) ) ).



% To tell that a returned value is not of interest to the caller:
% (could/should be: "-type void() :: 'VoiD'" for example)
%
%-opaque void() :: any().
-type void() :: any().


% Allows to count elements (positive integer, possibly zero):
%
-type count() :: non_neg_integer().


% Describes a mask of bits:
%
-type bit_mask() :: integer().


% A string UUID (ex: "ed64ffd4-74ee-43dc-adba-be37ed8735aa"):
-type uuid() :: string().


% The reason may be any term:
%
-type exit_reason() :: any().


% Quite often, variables (ex: record fields) are set to 'undefined'
% (i.e. "Nothing") before being set later:
%
-type maybe( T ) :: T | 'undefined'.


% Designates user-specified data (opaque, unspecified type):
%
-type user_data() :: any().


% Designates an accumulator (of any type), to document typically fold-like
% operations:
%
-type accumulator() :: any().



% Corresponds to smart (sortable, insertion-friendly) identifiers.
%
% Sometimes identifiers that can be sorted and that allow introducing any number
% of new identifiers between any two successive ones are needed.
%
% We use list of integers for that, whose default ordering corresponds to this
% need.
%
% For example, if having defined two identifiers [7,2] and [7,3], we can
% introduce two identifiers between them, typically [7,2,1] and [7,2,2], since
% the Erlang term ordering tells us that [7,2] < [7,2,1] < [7,2,2] < [7,3].
%
% As a result, no need to define specific comparison operators, '=:=', '<' and
% '>', hence 'lists:sort/1' are already adequate for that.
%
% Example: lists:sort( [ [7,3], [7,2,1], [7,2,2], [7,2] ] ) =
%   [ [7,2], [7,2,1], [7,2,2], [7,3] ].
%
-type sortable_id() :: [ integer() ].



-type registration_name() :: atom().


-type registration_scope() :: 'global_only' | 'local_only'
							| 'local_and_global' | 'none'.


-type look_up_scope() :: 'global' | 'local'
					   | 'local_and_global' | 'local_otherwise_global'.


-type version_number() :: integer().

% By default we consider a version is a triplet of numbers:
-type version() :: { version_number(), version_number(), version_number() }.


-type two_digit_version() :: { version_number(), version_number() }.

-type any_version() :: version() | two_digit_version().


% For all non-null index (i.e. the ones that start at 1).
-type positive_index() :: pos_integer().


% To distinguish with the built-in type, which can be a parameterised module:
-type module_name() :: atom().

-type function_name() :: atom().

-type argument() :: any().


% A mfa (module-function-arguments) command:
-type command_spec() :: { module_name(), function_name(), [ argument() ] }.



% To store (UNIX-like) user names:
-type user_name() :: nonempty_string().
-type atom_user_name() :: atom().


% Possible outcome of a partial-order comparison of two elements:
-type comparison_result() :: 'lower' | 'equal' | 'higher'.


% The exception classes that can be raised:
-type exception_class() :: 'throw' | 'exit' | 'error'.

% The status code returned by a shell command:
-type status_code() :: 0..255. % i.e. byte()


-export_type([

			  void/0, count/0, bit_mask/0, uuid/0, exit_reason/0, maybe/1,
			  user_data/0, accumulator/0, sortable_id/0,
			  registration_name/0, registration_scope/0, look_up_scope/0,
			  version_number/0, version/0, two_digit_version/0, any_version/0,
			  positive_index/0,
			  module_name/0, function_name/0, argument/0, command_spec/0,
			  user_name/0, atom_user_name/0,
			  comparison_result/0, exception_class/0, status_code/0

			  ]).



% Registration functions.
%
% Note that:
% - only local processes can be registered locally
% - a given PID cannot be registered globally under more than one name




% Registers the current process under specified name, which must be an atom.
%
% Declaration is register_as( Name, RegistrationType ) with
% RegistrationType in 'local_only', 'global_only', 'local_and_global', 'none'
% depending on what kind of registration is requested.
%
% Throws an exception on failure (ex: if that name is already registered).
%
-spec register_as( registration_name(), registration_scope() ) -> void().
register_as( Name, RegistrationType ) ->
	register_as( self(), Name, RegistrationType ).



% Registers specified (local) PID under specified name, which must be an atom.
%
% Declaration is: register_as( Pid, Name, RegistrationType ) with
% RegistrationType in 'local_only', 'global_only', 'local_and_global',
% 'none', depending on what kind of registration is requested.
%
% Throws an exception on failure.
%
-spec register_as( pid(), registration_name(), registration_scope() ) -> void().
register_as( Pid, Name, local_only ) when is_atom( Name ) ->

	%io:format( "register_as: local_only, with PID=~w and Name='~p'.~n",
	%		  [ Pid, Name ] ),

	try erlang:register( Name, Pid ) of

		true ->
			ok

	catch

		ExceptionType:Exception ->
			throw( { local_registration_failed, Name,
					 { ExceptionType, Exception } } )

	end;

register_as( Pid, Name, global_only ) when is_atom( Name ) ->
	case global:register_name( Name, Pid ) of

		yes ->
			ok;

		no ->
			throw( { global_registration_failed, Name } )

	end;

register_as( Pid, Name, local_and_global ) when is_atom( Name ) ->
	register_as( Pid, Name, local_only ),
	register_as( Pid, Name, global_only );

register_as( _Pid, _Name, none ) ->
	ok.




% Registers specified PID under specified name (which must be an atom) and scope
% (only local_only and global_only registration scopes permitted), and returns
% 'registered', or returns the PID of any process already registered.
%
% This is an atomic operation, which is not meant to fail.
%
% Allows for example a series of non-synchronised processes to all attempt to
% register: the first will succeed, all the others will get its PID, none will
% fail.
%
-spec register_or_return_registered( registration_name(),
	'global_only' | 'local_only' ) -> 'registered' | pid().
register_or_return_registered( Name, Scope ) when is_atom( Name ) ->

	% Minor annoyance: we ensured that looking up a process relied generally on
	% a different atom than registering it (ex: 'global' vs 'global_only').
	%
	% Here, we expect the user to specify a registration atom; we need to
	% convert it for look-up purposes:
	%
	LookUpScope = registration_to_look_up_scope( Scope ),

	case is_registered( Name, LookUpScope ) of

		not_registered ->

			try

				register_as( Name, Scope ),
				registered

			catch

				throw:_ ->
					% Another process must have registered in-between, let's
					% restart:
					%
					% (a small random waiting could be added here)
					%
					register_or_return_registered( Name, Scope )

			end;


		Pid ->
			Pid

	end.



% Unregisters specified name from specified registry.
%
% Throws an exception in case of failure.
%
-spec unregister( registration_name(), registration_scope() ) -> void().
unregister( Name, local_only ) ->

	try erlang:unregister( Name ) of

		true ->
			ok

	catch

		ExceptionType:Exception ->
			throw( { local_unregistration_failed, Name,
					 { ExceptionType, Exception } } )

	end;

unregister( Name, global_only ) ->
	% Documentation says it returns "void" (actually 'ok'):
	try

		global:unregister_name( Name )

	catch

		ExceptionType:Exception ->
			throw( { global_unregistration_failed, Name,
					 { ExceptionType, Exception } } )

	end;

unregister( Name, local_and_global ) ->
	unregister( Name, local_only ),
	unregister( Name, global_only );

unregister( _Name, none ) ->
	ok.



% Returns the PID that should be already registered, as specified name.
%
% Local registering will be requested first, if not found global one will be
% tried.
%
% No specific waiting for registration will be performed, see
% wait_for_*_registration_of instead.
%
-spec get_registered_pid_for( registration_name() ) -> pid().
get_registered_pid_for( Name ) ->
	get_registered_pid_for( Name, _RegistrationType=local_otherwise_global ).



-spec get_registered_pid_for( registration_name(), look_up_scope() ) ->  pid().
get_registered_pid_for( Name, _RegistrationType=local_otherwise_global ) ->

	try

		get_registered_pid_for( Name, local )

	catch

		{ not_registered_locally, _Name } ->

			try

				get_registered_pid_for( Name, global )

			catch

				{ not_registered_globally, Name } ->
					throw( { neither_registered_locally_nor_globally, Name } )

			end

	end;

get_registered_pid_for( Name, _RegistrationType=local ) ->
	case erlang:whereis( Name ) of

		undefined ->
			throw( { not_registered_locally, Name } );

		Pid ->
			Pid

	end;

get_registered_pid_for( Name, _RegistrationType=global ) ->
	case global:whereis_name( Name ) of

		undefined ->
			throw( { not_registered_globally, Name } );

		Pid ->
			Pid

	end;

% So that the atom used for registration can be used for look-up as well,
% notably in static methods (see the registration_type defines).
get_registered_pid_for( Name, _RegistrationType=local_and_global ) ->
	get_registered_pid_for( Name, local_otherwise_global ).



% Returns the PID of the process corresponding to the specified local name on
% specified node: that process is expected to be locally registered on that
% specified node.
%
% Throws an exception on failure.
%
-spec get_locally_registered_pid_for( registration_name(),
									 net_utils:atom_node_name() ) -> pid().
get_locally_registered_pid_for( Name, TargetNode ) ->

	case rpc:call( TargetNode, _Mod=erlang, _Fun=whereis, _Args=[ Name ] ) of

		{ badrpc, Reason } ->
			throw( { not_registered_locally, Name, TargetNode, Reason } );

		Res ->
			Res

	end.



% Returns a list of the names of the registered processes, for specified look-up
% scope.
%
-spec get_registered_names( look_up_scope() ) -> [ registration_name() ].
get_registered_names( _LookUpScope=global ) ->
	global:registered_names();

get_registered_names( _LookUpScope=local ) ->
	erlang:registered().



% Tells whether specified name is registered in the specified local/global
% context: if no, returns the 'not_registered' atom, otherwise returns the
% corresponding PID.
%
% Local registering will be requested first, if not found global one will be
% tried.
%
% No specific waiting for registration will be performed, see
% wait_for_*_registration_of instead.
%
-spec is_registered( registration_name() ) -> pid() | 'not_registered'.
is_registered( Name ) ->
	is_registered( Name, _RegistrationType=local_otherwise_global ).



-spec is_registered( registration_name(), look_up_scope() ) ->
						   pid() | 'not_registered'.
is_registered( Name, _LookUpScope=global ) ->

	case global:whereis_name( Name ) of

		undefined ->
			not_registered ;

		Pid ->
			Pid

	end;


is_registered( Name, _LookUpScope=local ) ->

	case erlang:whereis( Name ) of

		undefined ->
			not_registered;

		Pid ->
			Pid

	end;



% Returns a PID iff both local and global look-ups returns a PID, and the same
% one.
%
is_registered( Name, _LookUpScope=local_and_global ) ->

	case is_registered( Name, local ) of

		not_registered ->
			not_registered;

		Pid ->

			case is_registered( Name, global ) of

				% Already bound!
				Pid ->
					Pid;

				not_registered ->
					not_registered

			end

	end;


is_registered( Name, _LookUpScope=local_otherwise_global ) ->

	case is_registered( Name, local ) of

		not_registered ->
			is_registered( Name, global );

		Pid ->
			Pid

	end;

% Normally, 'local_only', 'global_only' and 'none' should only be specified for
% registration (not for looking-up); nevertheless the following clauses allow to
% use the same parameter for reading as for registration, even if we do not know
% which.
%
% So that the atom used for registration can be used for look-up as well,
% notably in static methods (see the registration_type defines).
%
is_registered( Name, _LookUpScope=local_only ) ->
	is_registered( Name, local );

is_registered( Name, _LookUpScope=global_only ) ->
	is_registered( Name, global ).





% Waits (up to 10 seconds) until specified name is globally registered.
%
% Returns the resolved PID, or throws
% { global_registration_waiting_timeout, Name }.
%
-spec wait_for_global_registration_of( registration_name() ) -> pid().
wait_for_global_registration_of( Name ) ->
	wait_for_global_registration_of( Name, _Seconds=10 ).


wait_for_global_registration_of( Name, _Seconds=0 ) ->
	throw( { global_registration_waiting_timeout, Name } );

wait_for_global_registration_of( Name, SecondsToWait ) ->
	case global:whereis_name( Name ) of

		undefined ->
			timer:sleep( 1000 ),
			wait_for_global_registration_of( Name, SecondsToWait-1 );

		Pid ->
			Pid

	end.




% Waits (up to 5 seconds) until specified name is locally registered.
%
% Returns the resolved PID, or throws {local_registration_waiting_timeout,Name}.
%
-spec wait_for_local_registration_of( registration_name() ) -> pid() | port().
wait_for_local_registration_of( Name ) ->
	wait_for_local_registration_of( Name , 5 ).


wait_for_local_registration_of( Name, _Seconds=0 ) ->
	throw( { local_registration_waiting_timeout, Name } );

wait_for_local_registration_of( Name, SecondsToWait ) ->

	case erlang:whereis( Name ) of

		undefined ->
			timer:sleep( 1000 ),
			wait_for_local_registration_of( Name, SecondsToWait-1 );

		Pid ->
			Pid

	end.



% Waits for specified name RegisteredName (an atom) to be locally registered on
% all specified nodes before returning.
%
% A time-out is triggered if the waited duration exceeds 10 seconds.
%
-spec wait_for_remote_local_registrations_of( registration_name(),
				 [ net_utils:atom_node_name() ] ) -> void().
wait_for_remote_local_registrations_of( RegisteredName, Nodes ) ->

	% Up to 10 seconds, 0.5 seconds of waiting between two, thus 20 attempts:
	RemainingAttempts = round( 10 / 0.5 ),

	wait_for_remote_local_registrations_of( RegisteredName, Nodes,
											RemainingAttempts ).


% Helper function.
wait_for_remote_local_registrations_of( RegisteredName, Nodes,
										_RemainingAttempts=0 ) ->
	throw( { time_out_while_waiting_remote_local_registration, RegisteredName,
			 Nodes } );

wait_for_remote_local_registrations_of( RegisteredName, Nodes,
										RemainingAttempts ) ->

	{ ResList, BadNodes } = rpc:multicall( Nodes, erlang, whereis,
										   [ RegisteredName  ], _Timeout=2000 ),

	case BadNodes of

		[] ->
			ok;

		_ ->
			throw( { bad_nodes_while_waiting_remote_local_registration,
					 RegisteredName, BadNodes } )

	end,

	case lists:member( undefined, ResList ) of

		true ->

			% Happens regularly on some settings:
			%io:format( "~nwait_for_remote_local_registrations_of: for ~p, "
			%		  "retry needed ~n~n", [ Nodes ] ),

			% At least one node not ready (we do not know which), waiting a bit
			% for it:
			timer:sleep( 500 ),
			wait_for_remote_local_registrations_of( RegisteredName, Nodes,
													RemainingAttempts - 1 );

		false ->
			ok

	end.



% Displays registered processes.
%
-spec display_registered() -> void().
display_registered() ->

	io:format( "On a total of ~B existing processes on node '~s':~n",
			   [ length( processes() ), node() ] ),

	case global:registered_names() of

		[] ->
			io:format( " - no process is globally-registered~n" );

		Globals ->
			io:format( " - ~B processes are globally-registered:~n~p~n",
					   [ length( Globals ), Globals ] )

	end,

	case registered() of

		[] ->
			io:format( " - no process is locally-registered~n" );

		Locals ->
			io:format( " - ~B processes are locally-registered:~n~p~n",
					   [ length( Locals ), Locals ] )

	end.





% Returns a string containing a new universally unique identifier (UUID), based
% on the system clock plus the system's ethernet hardware address, if present.
%
-spec generate_uuid() -> uuid().
generate_uuid() ->

	case executable_utils:lookup_executable( "uuidgen" ) of

		false ->
			display( "~nWarning: no 'uuidgen' found on system, "
					 "defaulting to our failsafe implementation.~n" ),
			uuidgen_internal();

		Exec ->

			% Random-based, rather than time-based (otherwise we end up
			% collecting a rather constant suffix):
			%
			case system_utils:run_executable( Exec ++ " -r" ) of

				{ _ExitCode=0, Res } ->
					Res;

				{ ExitCode, ErrorOutput } ->
					throw( { uuid_generation_failed, ExitCode, ErrorOutput } )

			end

	end.



% Quick and dirty replacement:
%
uuidgen_internal() ->

	% Using /dev/random instead would incur waiting of a few seconds that were
	% deemed too long for this use:
	%
	case system_utils:run_executable(
		   "/bin/dd if=/dev/urandom bs=1 count=32 2>/dev/null" ) of

		{ _ReturnCode=0, Output } ->
			% We translate these bytes into hexadecimal values:
			V = [ string:to_lower( hd(
				  io_lib:format( "~.16B", [ B rem 16 ] ) ) )  || B <- Output ],

			lists:flatten( io_lib:format(
							 "~s~s~s~s~s~s~s~s-~s~s~s~s-~s~s~s~s-~s~s~s~s-"
							 "~s~s~s~s~s~s~s~s~s~s~s~s", V ) );

		{ ErrorCode, ErrorOutput } ->
			throw( { uuidgen_internal_failed, ErrorCode, ErrorOutput } )

	end.



% Creates a tuple of specified size, all elements having the same, specified,
% value.
%
-spec create_uniform_tuple( Size::count(), Value::any() ) -> tuple().
create_uniform_tuple( Size, Value ) ->

	List = lists:duplicate( Size, Value ),

	list_to_tuple( List ).



% Stops smoothly the underlying VM, with a normal, success status code (0).
%
% Also also to potentially override Erlang standard teardown procedure.
%
-spec stop() -> no_return().
stop() ->
	stop( _Success=0 ).



% Stops smoothly the underlying VM, with a normal, success error code (0).
%
% Also also to potentially override Erlang standard teardown procedure.
%
-spec stop( status_code() ) -> no_return().
stop( StatusCode ) ->
	% Far less brutal than erlang:halt/{0,1}:
	init:stop( StatusCode ).



% Stops smoothly the underlying VM, with a normal, success status code (0).
%
-spec stop_on_success() -> no_return().
stop_on_success() ->
	stop( _Success=0 ).



% Stops smoothly the underlying VM, with a default error status code (1).
%
-spec stop_on_failure() -> no_return().
stop_on_failure() ->
	stop_on_failure( _OurDefaultErrorCode=5 ).


% Stops smoothly the underlying VM, with a default error status code (1).
%
-spec stop_on_failure( status_code() ) -> no_return().
stop_on_failure( StatusCode ) ->
	stop( StatusCode ).



% Crashes the current process immediately.
%
% Useful for testing reliability, for example.
%
-spec crash() -> any().
crash() ->

	io:format( "*** Crashing on purpose process ~w ***~n", [ self() ] ),

	% Must outsmart the compiler; there should be simpler solutions:
	A = system_utils:get_core_count(),
	B = system_utils:get_core_count(),

	% Dividing thus by zero:
	1 / ( A - B ).



% Makes the current process enter in an infinite, mostly idle loop.
%
% Useful for testing reliability, for example.
%
enter_infinite_loop() ->

	io:format( "~p in infinite loop...", [ self() ] ),

	% Loops every minute:
	timer:sleep( 60000 ),

	enter_infinite_loop().



% Triggers a OOM crash, i.e. Out of Memory.
%
% Useful for testing reliability, for example.
%
trigger_oom() ->

	io:format( "~p triggering OOM (out of memory) crash...", [ self() ] ),

	% Expected: Crash dump was written to: erl_crash.dump
	%  binary_alloc: Cannot allocate 1000000000031 bytes of memory (of type
	% "binary").

	<<1:8000000000000>>.






% Notification-related functions.


% Speaks the specified message, using espeak.
%
-spec speak( string() ) -> void().
speak( Message ) ->
	system_utils:run_background_executable(
	  "espeak -s 140 \"" ++ Message ++ "\"" ).



% Notifies the user of the specified message, with log output and synthetic
% voice.
%
-spec notify_user( string() ) -> void().
notify_user( Message ) ->
	io:format( Message ),
	speak( Message ).



% Notifies the user of the specified message, with log output and synthetic
% voice.
%
% Example: 'basic_utils:notify_user( "Hello ~w", [ Name ]).'
%
-spec notify_user( string(), list() ) -> void().
notify_user( Message, FormatList ) ->

	ActualMessage = io_lib:format( Message, FormatList ),

	io:format( ActualMessage ),
	speak( ActualMessage ).



% Message-related section.


% Flushes all the messages still in the mailbox of this process.
%
-spec flush_pending_messages() -> void().
flush_pending_messages() ->

	receive

		_ ->
			flush_pending_messages()

	after 0 ->
		ok

	end.



% Flushes all the messages still in the mailbox of this process that match the
% specified one.
%
-spec flush_pending_messages( any() ) -> void().
flush_pending_messages( Message ) ->

	receive

		Message ->
			flush_pending_messages( Message )

	after 0 ->
		ok

	end.





% Waits (indefinitively) for the specified count of the specified message to be
% received.
%
-spec wait_for( any(), count() ) -> void().
wait_for( _Message, _Count=0 ) ->
	ok;

wait_for( Message, Count ) ->

	%io:format( "Waiting for ~B messages '~p'.~n", [ Count, Message ] ),
	receive

		Message ->
			wait_for( Message, Count-1 )

	end.



% Waits (indefinitively) for the specified count of the specified message to be
% received, displaying repeatedly on the console a notification should the
% duration between two receivings exceed the specified time-out.
%
% Typical usage: basic_utils:wait_for( { foobar_result, done }, _Count=5,
% _Duration=2000, "Still waiting for ~B task(s) to complete" ).
%
-spec wait_for( any(), count(), unit_utils:milliseconds(),
				text_utils:format_string() ) -> void().
wait_for( _Message, _Count=0, _TimeOutDuration, _TimeOutFormatString ) ->
	ok;

wait_for( Message, Count, TimeOutDuration, TimeOutFormatString ) ->

	%io:format( "Waiting for ~B messages '~p'.~n", [ Count, Message ] ),

	receive

		Message ->
			%io:format( "Received message '~p'.~n", [ Message ] ),
			wait_for( Message, Count-1 )

	after TimeOutDuration ->

		io:format( TimeOutFormatString ++ " after ~s",
				   [ Count, text_utils:duration_to_string( TimeOutDuration ) ] )

	end.





% Wait patterns, safer and better defined once for all.




% Waits until receiving from all expected senders the specified acknowledgement
% message, expected to be in the form of { AckReceiveAtom, WaitedSenderPid }.
%
% Throws a { ThrowAtom, StillWaitedSenders } exception on time-out (if any, as
% the time-out can be disabled if set to 'infinity').
%
% See wait_for_many_acks/{4,5} if having a large number of senders waited for.
%
-spec wait_for_acks( [ pid() ], time_utils:time_out(), atom(), atom() ) ->
						   void().
wait_for_acks( WaitedSenders, MaxDurationInSeconds, AckReceiveAtom,
			   ThrowAtom ) ->

	wait_for_acks( WaitedSenders, MaxDurationInSeconds, _DefaultPeriod=1000,
				   AckReceiveAtom, ThrowAtom ).



% Waits until receiving from all expected senders the specified acknowledgement
% message, expected to be in the form of { AckReceiveAtom, WaitedSenderPid },
% ensuring a check is performed at least at specified period.
%
% Throws a { ThrowAtom, StillWaitedSenders } exception on time-out.
%
% See wait_for_many_acks/{4,5} if having a large number of senders waited for.
%
-spec wait_for_acks( [ pid() ], time_utils:time_out(),
					 unit_utils:milliseconds(), atom(), atom() ) -> void().
wait_for_acks( WaitedSenders, MaxDurationInSeconds, Period,
			   AckReceiveAtom, ThrowAtom ) ->

	InitialTimestamp = time_utils:get_timestamp(),

	wait_for_acks_helper( WaitedSenders, InitialTimestamp,
		MaxDurationInSeconds, Period, AckReceiveAtom, ThrowAtom ).



% (helper)
%
wait_for_acks_helper( _WaitedSenders=[], _InitialTimestamp,
			  _MaxDurationInSeconds, _Period, _AckReceiveAtom, _ThrowAtom ) ->
	ok;

wait_for_acks_helper( WaitedSenders, InitialTimestamp, MaxDurationInSeconds,
					  Period, AckReceiveAtom, ThrowAtom ) ->

	receive

		{ AckReceiveAtom, WaitedPid } ->

			NewWaited = list_utils:delete_existing( WaitedPid, WaitedSenders ),

			%io:format( "(received ~p, still waiting for instances ~p)~n",
			%		   [ WaitedPid, NewWaited ] ),

			wait_for_acks_helper( NewWaited, InitialTimestamp,
				  MaxDurationInSeconds, Period, AckReceiveAtom, ThrowAtom )

	after Period ->

			NewDuration = time_utils:get_duration_since( InitialTimestamp ),

			case ( MaxDurationInSeconds =/= infinity ) andalso
					  ( NewDuration > MaxDurationInSeconds ) of

				true ->
					throw( { ThrowAtom, WaitedSenders } );

				false ->
					% Still waiting then:

					%io:format( "(still waiting for instances ~p)~n",
					%   [ WaitedSenders ] ),

					wait_for_acks_helper( WaitedSenders, InitialTimestamp,
						MaxDurationInSeconds, Period, AckReceiveAtom,
						ThrowAtom )

			end

	end.



% Waits until receiving from all expected senders the specified acknowledgement
% message, expected to be in the form of:
% { AckReceiveAtom, ToAdd, WaitedSenderPid }.
%
% Returns the sum of the specified initial value with all the ToAdd received
% values.
%
% Throws a { ThrowAtom, StillWaitedSenders } exception on time-out (if any, as
% the time-out can be disabled if set to 'infinity').
%
-spec wait_for_summable_acks( [ pid() ], number(), time_utils:time_out(),
							  atom(), atom() ) -> number().
wait_for_summable_acks( WaitedSenders, InitialValue, MaxDurationInSeconds,
						AckReceiveAtom, ThrowAtom ) ->

	wait_for_summable_acks( WaitedSenders, InitialValue, MaxDurationInSeconds,
							_DefaultPeriod=1000, AckReceiveAtom, ThrowAtom ).



% Waits until receiving from all expected senders the specified acknowledgement
% message, expected to be in the form of:
% { AckReceiveAtom, ToAdd, WaitedSenderPid }
%
% ensuring a check is performed at least at specified period and summing all
% ToAdd values with the specified initial one
%
% Throws a { ThrowAtom, StillWaitedSenders } exception on time-out.
%
%
-spec wait_for_summable_acks( [ pid() ], number(), time_utils:time_out(),
		   unit_utils:milliseconds(), atom(), atom() ) -> number().
wait_for_summable_acks( WaitedSenders, CurrentValue, MaxDurationInSeconds,
						Period, AckReceiveAtom, ThrowAtom ) ->

	InitialTimestamp = time_utils:get_timestamp(),

	wait_for_summable_acks_helper( WaitedSenders, CurrentValue,
								   InitialTimestamp, MaxDurationInSeconds,
								   Period, AckReceiveAtom, ThrowAtom ).



% (helper)
%
wait_for_summable_acks_helper( _WaitedSenders=[], CurrentValue,
							   _InitialTimestamp, _MaxDurationInSeconds,
							   _Period, _AckReceiveAtom, _ThrowAtom ) ->
	CurrentValue;

wait_for_summable_acks_helper( WaitedSenders, CurrentValue, InitialTimestamp,
			MaxDurationInSeconds, Period, AckReceiveAtom, ThrowAtom ) ->

	receive

		{ AckReceiveAtom, ToAdd, WaitedPid } ->

			NewWaited = list_utils:delete_existing( WaitedPid, WaitedSenders ),

			%io:format( "(received ~p, still waiting for instances ~p)~n",
			%		   [ WaitedPid, NewWaited ] ),

			wait_for_summable_acks_helper( NewWaited, CurrentValue + ToAdd,
				  InitialTimestamp, MaxDurationInSeconds, Period,
				  AckReceiveAtom, ThrowAtom )

	after Period ->

			NewDuration = time_utils:get_duration_since( InitialTimestamp ),

			case ( MaxDurationInSeconds =/= infinity ) andalso
					  ( NewDuration > MaxDurationInSeconds ) of

				true ->
					throw( { ThrowAtom, WaitedSenders } );

				false ->
					% Still waiting then:

					%io:format( "(still waiting for instances ~p)~n",
					%   [ WaitedSenders ] ),

					wait_for_summable_acks_helper( WaitedSenders, CurrentValue,
						InitialTimestamp, MaxDurationInSeconds, Period,
						AckReceiveAtom,	ThrowAtom )

			end

	end.





% Waits until receiving from all expected (numerous) senders the specified
% acknowledgement message.
%
% Throws specified exception on time-out.
%
-spec wait_for_many_acks( ?list_impl_type, unit_utils:milliseconds(), atom(),
						  atom() ) -> void().
wait_for_many_acks( WaitedSenders, MaxDurationInSeconds, AckReceiveAtom,
					ThrowAtom ) ->

	wait_for_many_acks( WaitedSenders, MaxDurationInSeconds,
						_DefaultPeriod=1000, AckReceiveAtom, ThrowAtom ).



% Waits until receiving from all expected (numerous) senders the specified
% acknowledgement message.
%
% Throws specified exception on time-out, checking at the specified period.
%
-spec wait_for_many_acks( ?list_impl_type, unit_utils:milliseconds(),
						  unit_utils:milliseconds(), atom(), atom() ) -> void().
wait_for_many_acks( WaitedSenders, MaxDurationInSeconds, Period,
					AckReceiveAtom, ThrowAtom ) ->

	InitialTimestamp = time_utils:get_timestamp(),

	wait_for_many_acks_helper( WaitedSenders, InitialTimestamp,
		MaxDurationInSeconds, Period, AckReceiveAtom, ThrowAtom ).



% For this version we prefer a look-up optimised list to a plain one.
%
% (helper)
%
wait_for_many_acks_helper( WaitedSenders, InitialTimestamp,
						   MaxDurationInSeconds, Period, AckReceiveAtom,
						   ThrowAtom ) ->

	case ?list_impl:is_empty( WaitedSenders ) of

		true ->
			ok;

		false ->

			receive

				{ AckReceiveAtom, WaitedPid } ->

					NewWaited = ?list_impl:delete( WaitedPid, WaitedSenders ),

					wait_for_many_acks_helper( NewWaited, InitialTimestamp,
					   MaxDurationInSeconds, Period, AckReceiveAtom, ThrowAtom )

			after Period ->

					NewDuration = time_utils:get_duration_since(
									InitialTimestamp ),

					case NewDuration > MaxDurationInSeconds of

						true ->
							throw( { ThrowAtom, WaitedSenders } );

						false ->
							% Still waiting then:
							wait_for_many_acks_helper( WaitedSenders,
								InitialTimestamp, MaxDurationInSeconds, Period,
								AckReceiveAtom, ThrowAtom )

					end

			end

	end.



% Sends the specified message to all elements (supposed to be PID) reachable
% from the specified list_impl list, and returns the number of sent messages.
%
% (helper)
%
-spec send_to_pid_list_impl( term(), ?list_impl_type ) -> count().
send_to_pid_list_impl( Message, PidListImpl ) ->

	% Conceptually (not a basic list, though):
	 % [ Pid ! Message || Pid <- PidListImpl ]

	% Supposedly, it is done faster with iterators than first using
	% ?list_impl:to_list/1 then iterating on the resulting plain list:

	Iterator = ?list_impl:iterator( PidListImpl ),

	% Returns the count:
	send_to_pid_list_impl( Message, ?list_impl:next( Iterator ), _Count=0 ).



% (helper)
%
send_to_pid_list_impl( _Message, none, Count ) ->
	Count;

send_to_pid_list_impl( Message, { Pid, NewIterator }, Count ) ->
	Pid ! Message,
	send_to_pid_list_impl( Message, ?list_impl:next( NewIterator ), Count + 1 ).





% Miscellaneous functions.


% Returns the number of bytes used by specified term.
%
-spec size( term() ) -> system_utils:byte_size().
size( Term ) ->
	system_utils:get_size( Term ).


% Displays information about the process(es) identified by specified PID.
%
-spec display_process_info( pid() | [ pid() ] ) -> void().
display_process_info( PidList ) when is_list( PidList ) ->
	[ display_process_info( Pid ) || Pid <- PidList ];

display_process_info( Pid ) when is_pid( Pid ) ->

	LocalNode = node(),

	% erlang:process_info/1 throws badarg if the process is not local:
	case node( Pid ) of


		LocalNode ->
			case erlang:process_info( Pid ) of

				undefined ->
					io:format( "PID ~w refers to a (local) dead process~n",
							   [ Pid ] );

				PropList ->
					Strings = [ io_lib:format( "~s: ~p", [ K, V ] )
							   || { K, V } <- PropList ],
					io:format( "PID ~w refers to a local live process, "
							   "whose information are:~s",
							   [ Pid,
								  text_utils:strings_to_string( Strings ) ] )

			end;


		OtherNode ->

			% The current module may not be on this node:
			case rpc:call( OtherNode, _M=erlang, _F=process_info, _A=[ Pid ] )
			   of

				{ badrpc, Reason } ->
					io:format( "No information found for process ~w "
							   "running on remote node ~p; reason: ~p.~n",
							   [ Pid, OtherNode, Reason ] );

				undefined ->
					io:format( "PID ~w refers to a dead process on "
							   "remote node ~s.~n", [ Pid, OtherNode ] );

				PropList ->

					Strings = [ io_lib:format( "~s: ~p", [ K, V ] )
								|| { K, V } <- PropList ],

					io:format( "PID ~w refers to a live process on "
							   "remote node ~s, whose information are:~s",
							   [ Pid, OtherNode,
								 text_utils:strings_to_string( Strings ) ] )

			end

	end.



% Displays a numbered checkpoint.
%
% Useful for debugging purposes.
%
-spec checkpoint( integer() ) -> void().
checkpoint( Number ) ->
	display( "----- CHECKPOINT #~B -----", [ Number ] ).



% Displays specified string on the standard output of the console, ensuring as
% much as possible this message is output synchronously, so that it can be
% output on the console even if the virtual machine is to crash just after.
%
-spec display( string() ) -> void().
display( Message ) ->

	% Finally io:format has been preferred to erlang:display, as the latter one
	% displays quotes around the strings.

	io:format( "~s~n", [ Message ] ),
	system_utils:await_output_completion().

	% May not go through group leader (like io:format), thus less likely to
	% crash without displaying the message:
	%
	%erlang:display( lists:flatten( [ Message, ".~n" ] ) ).
	%erlang:display( Message ).



% Displays specified format string filled according to specified values on the
% standard output of the console, ensuring as much as possible this message is
% output synchronously, so that it can be output on the console even if the
% virtual machine is to crash just after.
%
-spec display( text_utils:format_string(), [ any() ] ) -> void().
display( Format, Values ) ->
	display( io_lib:format( Format, Values ) ).



% Displays specified string on the standard error output of the console,
% ensuring as much as possible this message is output synchronously, so that it
% can be output on the console even if the virtual machine is to crash just
% after.
%
-spec display_error( string() ) -> void().
display_error( Message ) ->

	% At least once, following call resulted in no output at all (standard_error
	% not functional):
	%
	%io:format( standard_error, "~s~n", [ Message ] ),

	% So:
	io:format( "~s~n", [ Message ] ),

	system_utils:await_output_completion().



% Displays specified format string filled according to specified values on the
% standard error output of the console, ensuring as much as possible this
% message is output synchronously, so that it can be output on the console even
% if the virtual machine is to crash just after.
%
-spec display_error( text_utils:format_string(), [ any() ] ) -> void().
display_error( Format, Values ) ->
	%io:format( standard_error, Format ++ "~n", Values ),
	io:format( Format ++ "~n", Values ),
	system_utils:await_output_completion().




% Displays, for debugging purposes, specified string, ensuring as much as
% possible this message is output synchronously, so that it can be output on the
% console even if the virtual machine is to crash just after.
%
-spec debug( string() ) -> void().
debug( Message ) ->
	%io:format( "## Debug: ~s.~n", [ Message ] ),
	%system_utils:await_output_completion().
	erlang:display( "## Debug: " ++ Message ).



% Displays, for debugging purposes, specified format string filled according to
% specified values, ensuring as much as possible this message is output
% synchronously, so that it can be output on the console even if the virtual
% machine is to crash just after.
%
-spec debug( text_utils:format_string(), [ any() ] ) -> void().
debug( Format, Values ) ->
	debug( io_lib:format( Format, Values ) ).





% Parses specified textual version.
%
% Ex: "4.2.1" should become {4,2,1}, and "2.3" should become {2,3}.
%
-spec parse_version( string() ) -> any_version().
parse_version( VersionString ) ->

	% First transform "4.22.1" into ["4","22","1"]:
	Elems = string:tokens( VersionString, "." ),

	% Then simply switch to {4,22,1}:
	list_to_tuple( [ text_utils:string_to_integer(E) || E <- Elems ] ).



% Compares the two pairs or triplets, which describe two version numbers (ex:
% {0,1,0} or {4,2}) and returns either first_bigger, second_bigger, or equal.
%
% The two compared versions must have the same number of digits.
%
% Note: the default term order is already what we needed.
%
-spec compare_versions( any_version(), any_version() ) ->
							  'equal' | 'first_bigger' | 'second_bigger'.
compare_versions( {A1,A2,A3}, {B1,B2,B3} ) ->

	case {A1,A2,A3} > {B1,B2,B3} of

		true ->
			first_bigger;

		false ->

			case {A1,A2,A3} =:= {B1,B2,B3} of

				true ->
					equal;

				false ->
					second_bigger

			end

	end;

compare_versions( {A1,A2}, {B1,B2} ) ->

	case {A1,A2} > {B1,B2} of

		true ->
			first_bigger;

		false ->

			case {A1,A2} =:= {B1,B2} of

				true ->
					equal;

				false ->
					second_bigger

			end

	end.




% Returns a value (a strictly positive integer) expected to be as much as
% possible specific to the current process.
%
% Mostly based on its PID.
%
% Useful for example when a large number of similar processes try to access to
% the same resource (ex: a set of file descriptors) over time: they can rely on
% some random waiting based on that process-specific value in order to smooth
% the accesses over time.
%
% We could imagine taking into account as well the current time, the process
% reductions, etc. or generating a reference.
%
-spec get_process_specific_value() -> pos_integer().
get_process_specific_value() ->

	% PID are akin to <X.Y.Z>.

	PidAsText = lists:flatten( io_lib:format( "~w", [ self() ] ) ),

	%io:format( "PID: ~w.~n", [ self() ] ) ,
	% Ex: ["<0","33","0>"]:
	[ [ $< | First ], Second, Third ] = string:tokens( PidAsText, "." ),

	% We add 1 to x and z as they might be null:
	{ F, [] } = string:to_integer( First ),
	{ S, [] } = string:to_integer( Second ),

	[ $> | ExtractedThird ] = lists:reverse( Third ),

	{ T, [] } = string:to_integer( ExtractedThird ),

	X = F+1,
	Y = S,
	Z = T+1,
	%io:format( "Res = ~w.~n", [X*Y*Z] ),
	X*Y*Z.



% Returns a process-specific value in [Min,Max[.
%
-spec get_process_specific_value( integer(), integer() ) -> integer().
get_process_specific_value( Min, Max ) ->

	Value = get_process_specific_value(),

	{ H, M, S } = erlang:time(),

	( ( ( H + M + S + 1 ) * Value ) rem ( Max - Min ) ) + Min.



% Converts a registration scope into a look-up one.
%
% Note: only legit for a subset of the registration scopes, otherwise a case
% clause is triggered..
%
% (helper)
%
-spec registration_to_look_up_scope( registration_scope() ) ->
										   look_up_scope().
registration_to_look_up_scope( _Scope=global_only ) ->
	global;

registration_to_look_up_scope( _Scope=local_only ) ->
	local;

registration_to_look_up_scope( _Scope=local_and_global ) ->
	local_and_global.



% Returns the execution target this module was compiled with, i.e. either the
% atom 'development' or 'production'.


% Dispatched in actual clauses, otherwise Dialyzer will detect an
% underspecification:
%
% -spec get_execution_target() -> 'production' | 'development'.

-ifdef(exec_target_is_production).

-spec get_execution_target() -> 'production'.
get_execution_target() ->
	production.

-else. % exec_target_is_production

-spec get_execution_target() -> 'development'.
get_execution_target() ->
	development.

-endif. % exec_target_is_production



% Tells whether the specified process, designated by its PID, by a textual
% representation of it (like "<9092.61.0>") or by a registred name (local
% otherwise global) like 'foobar_service') was still existing at the moment of
% this call.
%
% Note: generally not to be used when relying on a good design.
%
-spec is_alive( pid() | string() | registration_name() ) -> boolean().
is_alive( TargetPid ) when is_pid( TargetPid ) ->
	is_alive( TargetPid, node( TargetPid ) );

is_alive( TargetPidString ) when is_list( TargetPidString ) ->
	TargetPid = list_to_pid( TargetPidString ),
	is_alive( TargetPid, node( TargetPid ) );

is_alive( TargetPidName ) when is_atom( TargetPidName ) ->
	TargetPid = get_registered_pid_for( TargetPidName,
						_RegistrationType=local_otherwise_global ),
	is_alive( TargetPid, node( TargetPid ) ).



% Tells whether the specified process (designated by its PID) supposed to run on
% specified node (specified as an atom) was still existing at the moment of this
% call.
%
% Note: generally not to be used when relying on a good design; and is_alive/1
% should be preferred.
%
-spec is_alive( pid(), net_utils:atom_node_name() ) -> boolean().
is_alive( TargetPid, Node ) when is_pid( TargetPid ) ->

	% erlang:is_process_alive/1 is more intended for debugging purposes...

	case node() of

		Node ->
			erlang:is_process_alive( TargetPid );

		_OtherNode ->
			%io:format( "Testing liveliness of process ~p on node ~p.~n",
			%		  [ TargetPid, Node ] ),
			rpc:call( Node, _Mod=erlang, _Fun=is_process_alive,
					  _Args=[ TargetPid ] )

	end.



% Returns whether the debug mode is activated for the compilation of this
% module.

% Dispatched in actual clauses, otherwise Dializer will detect an
% underspecification:
%
%-spec is_debug_mode_enabled() -> boolean().

-ifdef(debug_mode_is_enabled).

-spec is_debug_mode_enabled() -> true.
is_debug_mode_enabled() ->
	true.

-else. % debug_mode_is_enabled

-spec is_debug_mode_enabled() -> false.
is_debug_mode_enabled() ->
	false.

-endif. % debug_mode_is_enabled
