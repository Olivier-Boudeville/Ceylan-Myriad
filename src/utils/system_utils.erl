% Copyright (C) 2010-2017 Olivier Boudeville
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
% Creation date: Thursday, February 11, 2010.


% Gathering of various system convenient facilities.
%
% See system_utils_test.erl for the corresponding test.
%
-module(system_utils).




% User-related functions.
-export([ get_user_name/0, get_user_name_string/0,
		  get_user_home_directory/0, get_user_home_directory_string/0 ]).


% Lower-level services.
-export([ await_output_completion/0, await_output_completion/1 ]).


% System-related functions.
-export([

		  run_executable/1, run_executable/2, run_executable/3,
		  get_standard_environment/0,
		  monitor_port/2,
		  evaluate_shell_expression/1, evaluate_shell_expression/2,

		  run_background_executable/1, run_background_executable/2,
		  evaluate_background_shell_expression/1,
		  evaluate_background_shell_expression/2,

		  get_environment_prefix/1, get_actual_expression/2,
		  get_environment_variable/1, set_environment_variable/2,
		  get_environment/0, environment_to_string/0, environment_to_string/1,

		  get_interpreter_version/0, get_application_version/1,

		  get_size_of_vm_word/0, get_size_of_vm_word_string/0,
		  get_size/1,

		  interpret_byte_size/1, interpret_byte_size_with_unit/1,
		  convert_byte_size_with_unit/1,

		  display_memory_summary/0,
		  get_total_physical_memory/0, get_total_physical_memory_string/0,
		  get_total_physical_memory_on/1, get_memory_used_by_vm/0,
		  get_total_memory_used/0,

		  get_swap_status/0, get_swap_status_string/0,
		  get_core_count/0, get_core_count_string/0,
		  get_process_count/0, get_process_count_string/0,
		  compute_cpu_usage_between/2, compute_cpu_usage_for/1,
		  compute_detailed_cpu_usage/2, get_cpu_usage_counters/0,

		  get_disk_usage/0, get_disk_usage_string/0,
		  get_mount_points/0,
		  get_known_pseudo_filesystems/0, get_filesystem_info/1,
		  filesystem_info_to_string/1,

		  get_current_directory_string/0,

		  get_operating_system_description/0,
		  get_operating_system_description_string/0,
		  get_system_description/0 ]).


% Prerequisite-related section.

% Name of a (third-party) prerequisite package (ex: "ErlPort", "jsx", etc.).
%
-type package_name() :: string().

-export_type([ package_name/0 ]).


-export([ get_dependency_base_directory/1, get_dependency_code_directory/1,
		  is_json_support_available/0, get_json_unavailability_hint/0,
		  is_hdf5_support_available/0, get_hdf5_unavailability_hint/0  ]).



% Size, in number of bytes:
-type byte_size() :: integer().


-opaque cpu_usage_info() :: { integer(), integer(), integer(), integer(),
							  integer() }.


-type cpu_usage_percentages() :: { math_utils:percent(), math_utils:percent(),
								   math_utils:percent(), math_utils:percent(),
								   math_utils:percent() }.


% For record declarations:
-include("system_utils.hrl").


% Describes the static information about a computing host:
-type host_static_info() :: #host_static_info{}.


% Describes the dynamic information about a computing host:
-type host_dynamic_info() :: #host_dynamic_info{}.



% Known real, actual types of filesystems:
-type actual_filesystem_type() :: 'ext2' | 'ext3' | 'ext4' | 'vfat'.


% Known pseudo filesystems:
-type pseudo_filesystem_type() :: 'devtmpfs' | 'tmpfs'.


% All the known types of filesystems (atom, to capture even lacking ones):
-type filesystem_type() :: actual_filesystem_type()
						 | pseudo_filesystem_type()
						 | 'unknown' | atom().


% Stores information about a filesystem:
%
-record( fs_info, {

		   % Device name (ex: /dev/sda5):
		   filesystem :: file_utils:path(),

		   % Mount point (ex: /boot):
		   mount_point :: file_utils:path(),

		   % Filesystem type (ex: 'ext4'):
		   type :: filesystem_type(),

		   % Used size, in bytes:
		   used_size :: byte_size(),

		   % Available size, in bytes:
		   available_size :: byte_size(),

		   % Number of used inodes:
		   used_inodes :: basic_utils:count(),

		   % Number of available inodes:
		   available_inodes :: basic_utils:count()

} ).


-type fs_info() :: #fs_info{}.



% Describes a command to be run (i.e. path to an executable, with possibly
% parameters):
%
-type command() :: text_utils:ustring().


% Return the (positive integer) return code of an executable being run
% (a.k.a. exit status):
%
% (0 means success, while a strictly positive value means error)
%
-type return_code() :: basic_utils:count().


% Output of the run of an executable:
%
-type command_output() :: text_utils:ustring().


% All information returned by a shell command:
%
-type command_outcome() :: { return_code(), command_output() }.



% Describes a shell expression:
%
-type shell_expression() :: text_utils:ustring().


% Output of the evaluation of a shell expression (at least currently, only its
% standard output; no exit status):
%
-type expression_outcome() :: text_utils:ustring().



% Name of a shell environment variable:
-type env_variable_name() :: string().


% Value of a shell environment variable ('false' meaning that the corresponding
% variable is not set)
%
-type env_variable_value() :: string() | 'false'.


% Represents a shell environment (a set of variables):
-type environment() :: [ { env_variable_name(), env_variable_value() } ].


% Working directory of an executed command:
-type working_dir() :: file_utils:directory_name()
					 | file_utils:bin_directory_name()
					 | 'undefined'.


% Basic authentication information:

-type user_name() :: string().
-type password() :: string().

-type basic_credential() :: { user_name(), password() }.



-export_type([ byte_size/0, cpu_usage_info/0, cpu_usage_percentages/0,
			   host_static_info/0, host_dynamic_info/0,

			   actual_filesystem_type/0, pseudo_filesystem_type/0,
			   filesystem_type/0, fs_info/0,

			   command/0, return_code/0, command_output/0, command_outcome/0,

			   shell_expression/0, expression_outcome/0,

			   env_variable_name/0, env_variable_value/0, environment/0,
			   working_dir/0,

			   user_name/0, password/0, basic_credential/0

			 ]).




% User-related functions.


% Returns the name of the current user, as a plain string.
%
-spec get_user_name() -> string().
get_user_name() ->

	case os:getenv( "USER" ) of

		false ->

			trace_utils:error( "The name of the user could not be "
							   "obtained from the shell environment "
							   "(no USER variable defined)." ),

			throw( user_name_not_found_in_environment );

		UserName ->
			UserName

	end.



% Returns a textual description of the name of the current user.
%
% Cannot crash.
%
-spec get_user_name_string() -> text_utils:ustring().
get_user_name_string() ->

	try

		io_lib:format( "user name: ~ts", [ get_user_name() ] )

	catch _AnyClass:Exception ->

		io_lib:format( "no user name information could be obtained (~p)",
					   [ Exception ] )

	end.



% Returns the home directory of the current user, as a plain string.
%
-spec get_user_home_directory() -> string().
get_user_home_directory() ->

	% Was: os:getenv( "HOME" )
	case init:get_argument( home ) of

		{ ok, [ [ Home ] ] } ->
			Home;

		Error ->
			throw( { home_directory_not_found, Error } )

	end.



% Returns a textual description of the home directory of the current user.
%
% Cannot crash.
%
-spec get_user_home_directory_string() -> text_utils:ustring().
get_user_home_directory_string() ->

	try

		io_lib:format( "user home directory: ~ts",
					   [ get_user_home_directory() ] )

	catch _AnyClass:Exception ->

		io_lib:format( "no home directory information could be "
					   "obtained (~p)", [ Exception ] )

	end.



% Lower-level services.


% Awaits the completion of an output operation (ex: io:format/2).
%
% Especially useful when displaying an error message on the standard output and
% then immediately halting the VM, in order to avoid a race condition between
% the displaying and the halting.
%
% We use a relatively short waiting here, just out of safety. It may be in some
% cases insufficient (ex: for error traces to be sent, received and stored
% *before* the VM is halted after a throw/1 that may be executed just after).
%
% In this case, await_output_completion/1 should be used, with a larger delay.
%
-spec await_output_completion() -> basic_utils:void().

-ifdef(debug_mode_is_enabled).

% Default time-out duration (one second):
await_output_completion() ->
	await_output_completion( _TimeOut=100 ).

-else. % debug_mode_is_enabled


% Extended time-out (one minute), if for example being in production, on a
% possibly heavily loaded system:
%
% (warning: this may impact adversely the timing if intensive logging is used)
%
await_output_completion() ->
	await_output_completion( _TimeOut=2000 ).

-endif. % debug_mode_is_enabled



% Awaits the completion of a io:format request, with a specified time-out, in
% milliseconds.
%
% Especially useful when displaying an error message on the standard output and
% then immediately halting the VM, in order to avoid a race condition between
% the displaying and the halting.
%
-spec await_output_completion( unit_utils:millisecond() ) -> basic_utils:void().
await_output_completion( TimeOut ) ->

	% Not sure it is really the proper way of waiting, however should be still
	% better than timer:sleep( 500 ):
	%
	% (we suppose that the time-out here is in milliseconds)

	%io:format( "(awaiting output completion)~n", [] ),

	% Almost just a yield:
	timer:sleep( 10 ),

	%io:format( "(output completed)~n", [] ),

	% Does not seem always sufficient:
	sys:get_status( error_logger, TimeOut ).





% Functions relative to the local Erlang system.


% Section to run executables and evaluation shell expressions.
%
% The former will return both the exit code and the command output, while the
% latter will be able only to return the command output (command being either an
% executable with arguments or a shell expression).


% We wish we could specify a command as a single, standalone one, or as a list
% of command elements, but the lack of a string type prevents it (as the
% parameter of the called functions would be a list in both cases).


% Runs (synchronously) specified executable with arguments, specified as a
% single, standalone string, with a standard environment, from the current
% working directory, and returns its return code (exit status) and its outputs
% (both the standard and the error ones).
%
% This function will run a specific executable, not evaluate a shell expression
% (that would possibly run executables).
%
% So one should not try to abuse this function by adding '&' at the end to
% trigger a background launch - this would just be interpreted as a last
% argument. Use run_background_executable/{1,2} in this module instead.
%
%-spec run_executable( command() | [ command() ] ) -> command_outcome().
%run_executable( Commands ) when is_list( Commands ) ->
-spec run_executable( command() ) -> command_outcome().
%run_executable( Commands ) ->

%	ActualCommand = text_utils:join( _Sep=" ", Commands ),

%	run_executable( ActualCommand );

run_executable( Command ) ->
	run_executable( Command, get_standard_environment() ).



% Executes (synchronously) specified shell command (specified as a single,
% standalone string) in specified shell environment and current directory, and
% returns its return code (exit status) and its outputs (both the standard and
% the error ones).
%
-spec run_executable( command(), environment() ) -> command_outcome().
run_executable( Command, Environment ) ->
	run_executable( Command, Environment, _WorkingDir=undefined ).



% Executes (synchronously) specified shell command (specified as a single,
% standalone string) in specified shell environment and directory, and returns
% its return code (exit status) and its outputs (both the standard and the error
% ones).
%
-spec run_executable( command(), environment(), working_dir() ) ->
							command_outcome().
run_executable( Command, Environment, WorkingDir ) ->

	%io:format( "Running executable: '~s' with environment '~s' "
	%		   "from working directory '~p'.~n",
	%		   [ Command, environment_to_string( Environment ), WorkingDir ] ),

	PortOpts = [ stream, exit_status, use_stdio, stderr_to_stdout, in, eof,
				 { env, Environment } ],

	PortOptsWithPath = case WorkingDir of

		undefined ->
			PortOpts;

		_ ->
			[ { cd, WorkingDir } | PortOpts ]

	end,

	Port = open_port( { spawn, Command }, PortOptsWithPath ),

	read_port( Port, _Data=[] ).



% Helper to read command data from a port.
%
read_port( Port, Data ) ->

	%io:format( "Reading port ~p (data: '~p').~n", [ Port, Data ] ),

	receive

		{ Port, { data, NewData } } ->
			%io:format( "Received data: '~p'.~n", [ NewData ] ),
			read_port( Port, [ NewData | Data ] );

		% As mentioned in the documentation, "the eof message and the
		% exit_status message appear in an unspecified order":

		{ Port, eof } ->
			%io:format( "Received eof (first).~n" ),
			port_close( Port ),

			receive

				{ Port, { exit_status, ExitStatus } } ->

					%io:format( "Received exit_status (second).~n" ),

					% Otherwise we have an enclosing list and last character is
					% always "\n":
					%
					Output = text_utils:remove_ending_carriage_return(
							   lists:flatten( lists:reverse( Data ) ) ),

					{ ExitStatus, Output }

			end;

		{ Port, { exit_status, ExitStatus } } ->

			%io:format( "Received exit_status (first).~n" ),

			receive

				{ Port, eof } ->
					%io:format( "Received eof (second).~n" ),

					port_close( Port ),

					Output = text_utils:remove_ending_carriage_return(
							   lists:flatten( lists:reverse( Data ) ) ),

					{ ExitStatus, Output }

			end;

		% Added by ourselves so that we can avoid process leakage:
		terminate_port ->
			% Anyway no PID to send information to:
			port_terminated

	 end.



% Returns a default, standard environment for "porcelain"-like executions,
% i.e. executions that are, as much as possible, reproducible in various runtime
% contexts (typically: with locale-independent outputs).
%
% To be used with run_executable/{2,3}.
%
-spec get_standard_environment() -> environment().
get_standard_environment() ->
	[ { "LANG", "C" } ].



% Monitors a port: reads command data and signals from a port, and report it.
%
monitor_port( Port, Data ) ->

	io:format( "Process ~p starting the monitoring of port ~p (data: '~p').~n",
			   [ self(), Port, Data ] ),

	receive

		{ Port, { data, NewData } } ->
			io:format( "Port monitor ~p received data: '~p'.~n",
					   [ self(), NewData ] ),
			monitor_port( Port, [ NewData | Data ] );

		% As mentioned in the documentation, "the eof message and the
		% exit_status message appear in an unspecified order":

		{ Port, eof } ->
			io:format( "Port monitor ~p received eof (first).~n", [ self() ] ),
			port_close( Port ),

			receive

				{ Port, { exit_status, ExitStatus } } ->

					io:format( "Port monitor ~p received exit_status "
							   "(second).~n", [ self() ] ),

					% Otherwise we have an enclosing list and last character is
					% always "\n":
					%
					Output = text_utils:remove_ending_carriage_return(
							   lists:flatten( lists:reverse( Data ) ) ),

					{ ExitStatus, Output }

			end;

		{ Port, { exit_status, ExitStatus } } ->

			io:format( "Port monitor ~p received exit_status (first).~n",
					   [ self() ] ),

			receive

				{ Port, eof } ->
					io:format( "Port monitor ~p received eof (second).~n",
							   [ self() ] ),

					port_close( Port ),

					Output = text_utils:remove_ending_carriage_return(
							   lists:flatten( lists:reverse( Data ) ) ),

					{ ExitStatus, Output }

			end;

		% Added by ourselves so that we can avoid process leakage:
		terminate_port ->

			io:format( "Port monitor ~p terminating.~n", [ self() ] ),

			% Anyway no PID to send information to:
			port_terminated

	 end.



% Evaluates specified shell (ex: sh, bash, etc. - not Erlang) expression, in
% a standard environment.
%
% No return code is available with this approach, only the output of the
% expression.
%
-spec evaluate_shell_expression( shell_expression() ) -> expression_outcome().
evaluate_shell_expression( Expression ) ->
	evaluate_shell_expression( Expression, get_standard_environment() ).



% Evaluates specified shell (ex: sh, bash, etc. - not Erlang) expression, in
% specified environment.
%
% No return code is available with this approach, only the output of the
% expression.
%
-spec evaluate_shell_expression( shell_expression(), environment() ) ->
									   expression_outcome().
evaluate_shell_expression( Expression, Environment ) ->

	FullExpression = get_actual_expression( Expression, Environment ),

	%io:format( "Evaluation shell expression '~s' in environment ~s.~n",
	%		   [ FullExpression, environment_to_string( Environment ) ] ),

	% No return code available, success supposed:
	text_utils:remove_ending_carriage_return( os:cmd( FullExpression ) ).


% No evaluate_shell_expression/3 defined, as one may change the current working
% directory directly from the shell expression.



% Executes asynchronously, in the background, specified executable with
% parameters, in a standard environment.
%
% As a consequence it returns no return code (exit status) nor output.
%
% For that, as it is a process-blocking operation in Erlang, a dedicated process
% is spawned (and most probably lost).
%
% If this function is expected to be called many times, to avoid the process
% leak, one should consider using evaluate_background_shell_expression/1
% instead.
%
-spec run_background_executable( command() ) -> basic_utils:void().
run_background_executable( Command ) ->
	run_background_executable( Command, get_standard_environment() ).



% Executes asynchronously, in the background, specified shell command, in
% specified environment.
%
% As a consequence it returns no return code (exit status) nor output.
%
% For that, as it is a process-blocking operation in Erlang, a dedicated process
% is spawned (and most probably lost).
%
% If this function is expected to be called many times, to avoid the process
% leak, one should consider using evaluate_background_shell_expression/2
% instead.
%
-spec run_background_executable( command(), environment() ) ->
									   basic_utils:void().
run_background_executable( Command, Environment ) ->
	run_background_executable( Command, Environment, _WorkingDir=undefined ).



% Executes asynchronously, in the background, specified shell command, in
% specified environment.
%
% As a consequence it returns no return code (exit status) nor output.
%
% For that, as it is a process-blocking operation in Erlang, a dedicated process
% is spawned (and most probably lost).
%
% If this function is expected to be called many times, to avoid the process
% leak, one should consider using evaluate_background_shell_expression/2
% instead.
%
-spec run_background_executable( command(), environment(), working_dir() ) ->
									   basic_utils:void().
run_background_executable( Command, Environment, WorkingDir ) ->

	% Apparently using a port-based launch and a background execution will block
	% the current process, so we sacrifice a process here - yet we monitor it:
	%
	spawn_link( fun() ->
					_Port = run_executable( Command, Environment, WorkingDir )
					% Not very useful, as nothing to monitor normally:
					%monitor_port( Port, _Data=[] )
				end ).



% Executes asynchronously, in the background, specified shell expression with
% specified environment, in current directory.
%
% As a consequence it returns no return code (exit status) nor output.
%
-spec evaluate_background_shell_expression( shell_expression() ) ->
												  basic_utils:void().
evaluate_background_shell_expression( Expression ) ->
	evaluate_background_shell_expression( Expression,
										  get_standard_environment() ).



% Executes asynchronously, in the background, specified shell command with
% specified environment, in current directory.
%
% As a consequence it returns no return code (exit status) nor output.
%
-spec evaluate_background_shell_expression( shell_expression(),
				environment() ) -> basic_utils:void().
evaluate_background_shell_expression( Expression, Environment ) ->

	FullExpression = get_actual_expression( Expression, Environment ),

	os:cmd( FullExpression ++ " &" ).



% Returns a string that can be used as a shell prefix for commands, based on
% specified environment.
%
-spec get_environment_prefix( environment() ) -> string().
get_environment_prefix( Environment ) ->

	% We do not specifically *unset* a variable whose value is false, we set it
	% to an empty string:
	%
	VariableStrings = [ begin

							ActualValue = case Value of

								false ->
									"";

								_ ->
									Value

							end,

							io_lib:format( "~s=~s", [ Name, ActualValue ] )

						end || { Name, Value } <- Environment ],

	text_utils:join( _Separator=" ", VariableStrings ).



% Returns the full, actual shell expression corresponding to specified
% expression and environment.
%
-spec get_actual_expression( shell_expression(), environment() ) ->
								   expression_outcome().
get_actual_expression( Expression, _Environment=[] ) ->
	% Allows to avoid starting the command with a space:
	Expression;

get_actual_expression( Expression, Environment ) ->
	get_environment_prefix( Environment ) ++ " " ++ Expression.



% Returns the value associated to the specified environment variable (if any),
% otherwise 'false'.
%
-spec get_environment_variable( env_variable_name() ) -> env_variable_value().
get_environment_variable( VarName ) ->
	os:getenv( VarName ).



% Sets the specified environment variable to the specified value, possibly
% overwriting a past value.
%
-spec set_environment_variable( env_variable_name(), env_variable_value() ) ->
									  basic_utils:void().
set_environment_variable( VarName, VarValue ) ->
	os:putenv( VarName, VarValue ).



% Returns the current shell environment, sorted by variable names.
%
-spec get_environment() -> environment().
get_environment() ->

	StringEnv = lists:sort( os:getenv() ),

	[ text_utils:split_at_first( $=, VarEqValue ) || VarEqValue <-StringEnv ].



% Returns a textual description of the current shell environment.
%
-spec environment_to_string() -> string().
environment_to_string() ->
	environment_to_string( get_environment() ).



% Returns a textual description of the specified shell environment.
%
-spec environment_to_string( environment() ) -> string().
environment_to_string( Environment ) ->

	{ SetVars, UnsetVars } = lists:partition(
							   fun( { _Name, _Value=false } ) ->
									   false;

								  ( _ ) ->
									   true

							   end,
							   Environment ),

	VariableStrings = [ io_lib:format( "~s = ~s", [ Name, Value ] )
						|| { Name, Value } <- SetVars ],

	FinalVariableStrings = case UnsetVars of

		[] ->
			VariableStrings;

		_ ->
			UnsetNames = [ Name || { Name, _False } <- UnsetVars ],

			UnsetString = "unset variables: "
								++ text_utils:join( _Sep=", ", UnsetNames ),

			list_utils:append_at_end( UnsetString, VariableStrings )

	end,

	text_utils:strings_to_string( FinalVariableStrings ).



% Returns the version informations of the current Erlang interpreter (actually
% the environment one, including the VM) being used.
%
% Returns a full version name (ex: "R13B04") or, if not available, a shorter one
% (ex: "R11B").
%
-spec get_interpreter_version() -> string().
get_interpreter_version() ->

	% Older versions (pre-R13A?) did not support the otp_release tag:
	try erlang:system_info( otp_release ) of

		StringVersion ->

			try list_to_integer( StringVersion ) of

				V ->
					% Ex: V=17 Newer release (ex: 17.0-rc1) do not comply to the
					% traditional scheme, applying it for uniformity and maybe a
					% bit of nostalgia:
					%
					lists:flatten( io_lib:format( "R~BB", [ V ] ) )

			catch

				_:_ ->
					% Ex: StringVersion="R13B04":
					StringVersion

			end

	catch

		_:_ ->
			% Here we revert to another (older) solution:
			{ _OTPInfos, StringVersion } = init:script_id(),
			% Ex: StringVersion="R11B"
			StringVersion

	end.



% Returns the version information (as a 2 or 3-part tuple) corresponding to the
% specified Erlang application (ex: for 'kernel', could return {3,0} or
% {2,16,3}).
%
% Throws an exception if the information could not be retrieved.
%
-spec get_application_version( atom() ) -> basic_utils:any_version().
get_application_version( Application ) ->

	case application:get_key( Application, vsn ) of

		% Ex: "3.0" or "2.16.3":
		{ ok, VsnString } ->
			basic_utils:parse_version( VsnString );

		undefined ->
			throw( { application_version_not_found, Application } )

	end.



% Returns the size, in bytes, of a word of this Virtual Machine.
%
-spec get_size_of_vm_word() -> basic_utils:count().
get_size_of_vm_word() ->
	erlang:system_info( wordsize ).



% Returns a textual description of the size of a VM word.
%
% Cannot crash.
%
-spec get_size_of_vm_word_string() -> text_utils:ustring().
get_size_of_vm_word_string() ->

	try

		io_lib:format( "size of a VM word: ~B bytes",
					   [ get_size_of_vm_word() ] )

	catch _AnyClass:Exception ->

		io_lib:format( "size of a VM word could not be obtained (~p)",
					   [ Exception ] )

	end.



% Returns the size of specified term, in bytes, in the heap.
%
% Note that off-heap data (such as binaries larger than 64 bytes) is not counted
% here. The (flat) size is incremented to account for the top term word (which
% is kept in a register or on the stack).
%
-spec get_size( term() ) -> byte_size().
get_size( Term ) ->

	% With sharing taken into account:
	% use ( erts_debug:size( Term ) + 1 ) * get_size_of_vm_word()
	%
	( erts_debug:flat_size( Term ) + 1 ) * get_size_of_vm_word().



% Returns a string containing a user-friendly description of the specified size
% expressed in bytes, using GiB (Gibibytes, not Gigabytes), MiB (Mebibytes, not
% Megabytes), KiB (Kibibytes, not Kilobytes) and bytes.
%
% See http://en.wikipedia.org/wiki/Kibibyte
%
-spec interpret_byte_size( byte_size() ) -> string().
interpret_byte_size( SizeInBytes ) ->

	Kilo = 1024,
	Mega = Kilo*Kilo,
	Giga = Kilo*Mega,

	ListWithGiga = case SizeInBytes div Giga of

		0 ->
			[];

		GigaNonNull->
			[ io_lib:format( "~B GiB", [ GigaNonNull ] ) ]

	end,

	SizeAfterGiga = SizeInBytes rem Giga,
	%io:format( "SizeAfterGiga = ~B.~n", [ SizeAfterGiga ] ),

	ListWithMega = case SizeAfterGiga div Mega of

		0 ->
			ListWithGiga;

		MegaNonNull->
			[ io_lib:format( "~B MiB", [ MegaNonNull ] ) | ListWithGiga ]

	end,

	SizeAfterMega = SizeAfterGiga rem Mega,
	%io:format( "SizeAfterMega = ~B.~n", [ SizeAfterMega ] ),

	ListWithKilo = case SizeAfterMega div Kilo of

		0 ->
			ListWithMega;

		KiloNonNull->
			[ io_lib:format( "~B KiB", [ KiloNonNull ] ) | ListWithMega ]

	end,

	SizeAfterKilo = SizeAfterMega rem Kilo,
	%io:format( "SizeAfterKilo = ~B.~n", [ SizeAfterKilo ] ),

	ListWithByte = case SizeAfterKilo rem Kilo of

		0 ->
			ListWithKilo ;

		1->
			[ "1 byte" | ListWithKilo ];

		AtLeastTwoBytes ->
			 [ io_lib:format( "~B bytes", [ AtLeastTwoBytes ] ) | ListWithKilo ]

	end,

	%io:format( "Unit list is: ~w.~n", [ ListWithByte ] ),

	case ListWithByte of

		[] ->
			"0 byte";

		[ OneElement ] ->
			OneElement;

		[ Smaller | Bigger ] ->
			text_utils:join( ", ", lists:reverse( Bigger ) )
				++ " and " ++ Smaller

	end.



% Returns a string containing a user-friendly description of the specified size
% expressed in bytes, using the most appropriate unit among GiB (Gibibytes, not
% Gigabytes), MiB (Mebibytes, not Megabytes), KiB (Kibibytes, not Kilobytes) and
% bytes, rounding that value to 1 figure after the comma (this is thus an
% approximate value).
%
% See http://en.wikipedia.org/wiki/Kibibyte
%
-spec interpret_byte_size_with_unit( byte_size() ) -> string().
interpret_byte_size_with_unit( Size ) ->

	{ Unit, Value } = convert_byte_size_with_unit( Size ),

	case Unit of

		byte ->

			case Value of

				0 ->
					"0 byte";

				1 ->
					"1 byte";

				Other ->
					io_lib:format( "~B bytes", [ Other ] )

			end;

		kib ->
			io_lib:format( "~.1f KiB", [ Value ] );

		mib ->
			io_lib:format( "~.1f MiB", [ Value ] );

		gib ->
			io_lib:format( "~.1f GiB", [ Value ] )

	end.



% Converts the specified size, in bytes, as a value expressed in an appropriate
% size unit.
%
% Returns a { Unit, Value } pair, in which:
%
% - Unit is the largest size unit that can be selected so that the specified
% size if worth at least 1 unit of it (ex: we do not want a value 0.9, at least
% 1.0 is wanted); Unit can be 'gib', for GiB (Gibibytes), 'mib', for MiB
% (Mebibytes), 'kib' for KiB (Kibibytes), or 'byte', for Byte
%
% - Value is the converted byte size, in the specified returned unit, expressed
% either as an integer (for bytes) or as a float
%
% Ex: 1023 (bytes) translates to { byte, 1023 }, 1025 translates to
% { kib, 1.0009765625 }.
%
% Note that the returned value cannot be expected to be exact (rounded),
% therefore this function is mostly useful for user output.
%
-spec convert_byte_size_with_unit( byte_size() ) ->
	{ 'byte', integer() } | { 'kib', float() } | { 'mib', float() }
											 | { 'gib', float() }.
convert_byte_size_with_unit( SizeInBytes ) ->

	Kilo = 1024,
	Mega = Kilo*Kilo,
	Giga = Kilo*Mega,

	case SizeInBytes div Giga of

		0 ->

			case SizeInBytes div Mega of

				0 ->

					case SizeInBytes div Kilo of

						0 ->
							%{ byte, float( SizeInBytes ) };
							{ byte, SizeInBytes };

						_ ->
							{ kib, SizeInBytes / Kilo }

					end;

				_ ->
					{ mib, SizeInBytes/Mega }

			end;

		_ ->
			{ gib, SizeInBytes / Giga }

	end.



% Returns a summary of the dynamically allocated memory currently being used by
% the Erlang emulator.
%
-spec display_memory_summary() -> basic_utils:void().
display_memory_summary() ->

	SysSize  = erlang:memory( system ),
	ProcSize = erlang:memory( processes ),

	Sum = SysSize + ProcSize,

	io:format( "  - system size: ~s (~s)~n",
			  [ interpret_byte_size_with_unit( SysSize ),
				text_utils:percent_to_string( SysSize / Sum ) ] ),

	io:format( "  - process size: ~s (~s)~n",
			  [ interpret_byte_size_with_unit( ProcSize ),
				text_utils:percent_to_string( ProcSize / Sum ) ] ).



% Returns the total installed physical volatile memory (RAM) of the local
% computer, expressed in bytes.
%
-spec get_total_physical_memory() -> byte_size().
get_total_physical_memory() ->

	% Note: '\\awk' is '\awk' once escaped; a backslash allows to unalias the
	% corresponding command, to avoid not-so-compliant alternatives to be used.

	% First check the expected unit is returned, by pattern-matching:
	UnitCommand = "/bin/cat /proc/meminfo | /bin/grep 'MemTotal:' "
				  "| \\awk '{print $3}'",

	case run_executable( UnitCommand ) of

		 { _ExitCode=0, _Output="kB" } ->

			% Ok, using kB indeed.

			ValueCommand =
				"/bin/cat /proc/meminfo | /bin/grep 'MemTotal:' "
				"| \\awk '{print $2}'",

			% The returned value of following command is like "12345\n", in
			% bytes:
			%
			case run_executable( ValueCommand ) of

				{ _ExitCode=0, MemSizeString } ->

					% They were probably kiB:
					list_to_integer( MemSizeString ) * 1024;

				{ ExitCode, ErrorOutput } ->
					throw( { total_physical_memory_inquiry_failed, ExitCode,
							 ErrorOutput } )

			end;

		{ ExitCode, ErrorOutput } ->
			throw( { total_physical_memory_inquiry_failed, ExitCode,
					 ErrorOutput } )

	end.



% Returns a textual description of the total installed physical memory.
%
% Cannot crash.
%
-spec get_total_physical_memory_string() -> text_utils:ustring().
get_total_physical_memory_string() ->

	try

		io_lib:format( "total physical memory: ~ts",
					  [ interpret_byte_size( get_total_physical_memory() ) ] )

	catch _AnyClass:Exception ->
			io_lib:format( "no total physical RAM information could be "
						   "obtained (~p)", [ Exception ] )

	end.




% Returns the total installed physical volatile memory (RAM) of the computer on
% which specified node (specified as an atom) is running, expressed in bytes.
%
-spec get_total_physical_memory_on( net_utils:atom_node_name() ) -> byte_size().
get_total_physical_memory_on( Node ) ->

	% No standard environment enforced here.

	% First check the expected unit is returned, by pattern-matching:
	UnitCommand = "/bin/cat /proc/meminfo | /bin/grep 'MemTotal:' "
				  "| \\awk '{print $3}'",
	"kB\n" = rpc:call( Node, os, cmd, [ UnitCommand ] ),

	ValueCommand = "/bin/cat /proc/meminfo | /bin/grep 'MemTotal:' "
				   "| \\awk '{print $2}'",
	ValueCommandOutput = rpc:call( Node, os, cmd, [ ValueCommand ] ),

	% The returned value of following command is like "12345\n", in bytes:
	MemorySizeString = text_utils:remove_ending_carriage_return(
								ValueCommandOutput ),

	% They were probably kiB:
	list_to_integer( MemorySizeString ) * 1024.



% Returns the total memory used, in bytes, by this instance of the Erlang VM,
% i.e. the total amount of memory currently allocated by the Erlang processes
% and by this emulator.
%
-spec get_memory_used_by_vm() -> byte_size().
get_memory_used_by_vm() ->
	erlang:memory( total ).



% Returns { UsedRAM, TotalRAM } where UsedRAM is the actual total memory used on
% the current host by all applications, in bytes, and TotalRAM is the total
% installed RAM, in bytes.
%
% The cached memory and the buffers used by the kernel are not taken into
% account into the returned count.
%
-spec get_total_memory_used() -> { byte_size(), byte_size() }.
get_total_memory_used() ->

	% Example of memory information as returned by the 'free' command:
	% (slightly edited with variable names)
	%
	% """
	%             total       used       free     shared    buffers    cached
	% Mem:       A=8202424  B=5588920  C=2613504  D=0       E=567480   F=3212392
	% -/+ buffers/cache:    G=1809048  H=6393376
	% """
	%
	% We have: H = C + D + E + F, and G = A - H. D is never used (obsolete).
	% We return here { G, A }, thus { G, G+H }.

	% Avoid locale and greps 'buffers/cache:' (ex: on Debian) as well as
	% 'buff/cache' (ex: on Arch)
	%MemoryInfo = os:cmd( "LANG= free -b | grep '/cache' "
	%					 "| awk '{print $3,$4}'" ),

	% Converts MemoryInfo from "a b\n" to ["a","b\n"]
	%[ AppliUsedString, TotalFreeTermString ] =
	%  string:tokens( MemoryInfo, " " ),

	% Unfortunately on Arch we have quite different outputs, like:
	%          total        used        free      shared  buff/cache   available
	% Mem:   8047428     2476488     1124396      362228     4446544     4893712
	% Swap:        0           0           0


	% This is G:
	%AppliUsedSize = text_utils:string_to_integer( AppliUsedString ),

	%TotalFreeString = text_utils:remove_ending_carriage_return(
	%													TotalFreeTermString ),

	% This is H:
	%TotalFreeSize = text_utils:string_to_integer( TotalFreeString ),

	% { G, G+H }:
	%{ AppliUsedSize, AppliUsedSize + TotalFreeSize }.


	% So finally we prefered /proc/meminfo, used first to get MemTotal:
	%
	TotalString = case run_executable( "/bin/cat /proc/meminfo | "
				"/bin/grep '^MemTotal:' | \\awk '{print $2,$3}'" ) of

		{ _TotalExitCode=0, TotalOutput } ->
			%io:format( "TotalOutput: '~p'~n", [ TotalOutput ] ),
			TotalOutput;

		{ TotalExitCode, TotalErrorOutput } ->
			throw( { total_memory_used_inquiry_failed, TotalExitCode,
					 TotalErrorOutput } )

	end,

	[ Total, "kB" ] = string:tokens( TotalString, " " ),

	TotalByte = text_utils:string_to_integer( Total ) * 1024,

	% MemAvailable does not seem always available:
	%
	FreeString = case run_executable(
			"/bin/cat /proc/meminfo | /bin/grep '^MemAvailable:' "
			"| \\awk '{print $2,$3}'" )  of

		{ _AvailExitCode=0, MemAvailOutput } ->
			%io:format( "## using MemAvailable~n" ),
			MemAvailOutput;

		{ _AvailExitCode, _AvailErrorOutput } ->

			% In some cases (ex: Debian 6.0), no 'MemAvailable' is defined, we
			% use 'MemFree' instead (we consider they are synonymous):

			%io:format( "## using MemFree~n" ),

			case run_executable(
				   "/bin/cat /proc/meminfo | /bin/grep '^MemFree:' | "
				   "\\awk '{print $2,$3}'" ) of

				{ _FreeExitCode=0, MemFreeOutput } ->
					MemFreeOutput;

				{ FreeExitCode, FreeErrorOutput } ->
					throw( { total_memory_used_inquiry_failed, FreeExitCode,
							 FreeErrorOutput } )

			end

	end,

	% The problem is that even if MemAvailable is not found, we have a zero exit
	% code (and an empty string):
	%
	FreeByte = case FreeString of

		[] ->

			% As a last resort we do as before, i.e. we use free:

			%io:format( "## using free~n" ),

			case run_executable(
				"/bin/free -b | /bin/grep '/cache' | \\awk '{print $3}'" ) of

				{ _ExitCode=0, FreeOutput } ->
					% Already in bytes:
					text_utils:string_to_integer( FreeOutput );

				{ ExitCode, ErrorOutput } ->
					throw( { total_memory_used_inquiry_failed, ExitCode,
							 ErrorOutput } )

			end;

		_ ->

			[ Free, "kB" ] = string:tokens( FreeString, " " ),

			text_utils:string_to_integer( Free ) * 1024

	end,

	UsedByte = TotalByte - FreeByte,

	{ UsedByte, TotalByte }.



% Returns a textual description of the current RAM status.
%
% Cannot crash.
%
-spec get_ram_status_string() -> text_utils:ustring().
get_ram_status_string() ->

	try

		case get_total_memory_used() of

			{ _UsedRAM, _TotalRAM=0 } ->
				"total RAM size could not be obtained (found null)";

			{ UsedRAM, TotalRAM } ->
				io_lib:format( "RAM memory used: ~s, over a total of ~s (~s)",
					[ interpret_byte_size( UsedRAM ),
					  interpret_byte_size( TotalRAM ),
					  text_utils:percent_to_string( UsedRAM / TotalRAM ) ] )

		end

	catch _AnyClass:Exception ->
			io_lib:format( "no RAM information could be obtained (~p)",
						   [ Exception ] )

	end.




% Returns { UsedSwap, TotalSwap } where UsedSwap is the size of the used swap
% and TotalSwap is the total amount of swap space on the local host, both
% expressed in bytes.
%
% Will crash if the information cannot be retrieved properly.
%
-spec get_swap_status() -> { byte_size(), byte_size() }.
get_swap_status() ->

	% Same reason as for get_total_memory_used/0:
	%SwapInfos = os:cmd( "free -b | grep 'Swap:' | awk '{print $2, $3}'" ),
	SwapTotalString = case run_executable(
		  "/bin/cat /proc/meminfo | /bin/grep '^SwapTotal:' | "
		  "\\awk '{print $2,$3}'" ) of

		{ _TotalExitCode=0, TotalOutput } ->
			TotalOutput;

		{ TotalExitCode, TotalErrorOutput } ->
			throw( { swap_inquiry_failed, TotalExitCode, TotalErrorOutput } )

	end,

	[ TotalString, "kB" ] = string:tokens( SwapTotalString, " " ),

	TotalByte = text_utils:string_to_integer( TotalString ) * 1024,


	SwapFreeString = case run_executable(
		"cat /proc/meminfo | /bin/grep '^SwapFree:' | "
		"\\awk '{print $2,$3}'" ) of

		{ _FreeExitCode=0, FreeOutput } ->
			FreeOutput;

		{ FreeExitCode, FreeErrorOutput } ->
			throw( { swap_inquiry_failed, FreeExitCode, FreeErrorOutput } )

	end,


	[ FreeString, "kB" ] = string:tokens( SwapFreeString, " " ),

	FreeByte = text_utils:string_to_integer( FreeString ) * 1024,

	UsedByte = TotalByte - FreeByte,

	{ UsedByte, TotalByte }.



% Returns a textual description of the current swap status.
%
% Cannot crash.
%
-spec get_swap_status_string() -> text_utils:ustring().
get_swap_status_string() ->

	try

		case get_swap_status() of

			{ _UsedSwap, _TotalSwap=0 } ->
					   "no swap found";

			{ UsedSwap, TotalSwap } ->
					   io_lib:format( "swap used: ~s over a total of ~s (~s)",
									  [ interpret_byte_size( UsedSwap ),
										interpret_byte_size( TotalSwap ),
										text_utils:percent_to_string(
										  UsedSwap / TotalSwap ) ] )

		end


	catch _AnyClass:Exception ->
			io_lib:format( "no swap information could be obtained (~p)",
						   [ Exception ] )

	end.




% Returns the number of cores available on the local host.
%
% Throws an exception on failure.
%
-spec get_core_count() -> integer().
get_core_count() ->

	CoreString = case run_executable(
						"/bin/cat /proc/cpuinfo | /bin/grep -c processor" ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { core_count_inquiry_failed, ExitCode, ErrorOutput } )

	end,

	try

		text_utils:string_to_integer( CoreString )

	catch

		{ integer_conversion_failed, CoreString } ->
			throw( { could_not_determine_core_count, CoreString } )

	end.



% Returns a textual description of the number of the local processing cores.
%
% Cannot crash.
%
-spec get_core_count_string() -> text_utils:ustring().
get_core_count_string() ->

	try

		io_lib:format( "number of cores: ~B", [ get_core_count() ] )

	catch _AnyClass:Exception ->

		io_lib:format( "no core information could be obtained (~p)",
					   [ Exception ] )

	end.




% Returns the number of live Erlang processes on the current node.
%
-spec get_process_count() -> basic_utils:count().
get_process_count() ->
	erlang:system_info( process_count ).



% Returns a textual description of the number of live Erlang processes on the
% current node.
%
% Cannot crash.
%
-spec get_process_count_string() -> text_utils:ustring().
get_process_count_string() ->

	try

		io_lib:format( "number of existing Erlang processes: ~B ",
					  [ get_process_count() ] )

	catch _AnyClass:Exception ->

		io_lib:format( "no information about the number of live Erlang "
					   "processes could be obtained (~p)", [ Exception ] )

	end.



% Returns an aggregated view of the CPU usage (a float in [0;100]) based on the
% two specified sets of CPU counters, i.e. the average (on all cores of all
% processors of the local host) percentage of CPU utilization (all kinds of
% usage except idle) during the period which elapsed between the start and end
% measures (in that order).
%
% Typical usage:
%
% FirstMeasure = system_utils:get_cpu_usage_counters(),
% (do something)
% SecondMeasure = system_utils:get_cpu_usage_counters(),
%
% UsageInPercent = system_utils:compute_cpu_usage_between( FirstMeasure,
%   SecondMeasure )
%
-spec compute_cpu_usage_between( cpu_usage_info(), cpu_usage_info() ) ->
									   math_utils:percent().
compute_cpu_usage_between( StartCounters, EndCounters ) ->

	Percentages = compute_detailed_cpu_usage( StartCounters, EndCounters ),

	compute_cpu_usage_for( Percentages ).



% Returns an aggregated view of the CPU usage (a float in [0;100]) based on the
% specified detailed CPU percentages, i.e. the average (on all cores of all
% processors of the local host) percentage of CPU utilization (all kinds of
% usage except idle) during the period the input percentages correspond to.
%
% Returns 'undefined' iff the specified usage is itself undefined.
%
-spec compute_cpu_usage_for( basic_utils:maybe( cpu_usage_percentages() ) ) ->
								   basic_utils:maybe( math_utils:percent() ).
compute_cpu_usage_for( undefined ) ->
	undefined;

compute_cpu_usage_for( { UserPercent, NicePercent, SystemPercent, _IdlePercent,
						OtherPercent } ) ->

	% Every usage matters here, except idle:
	UserPercent + NicePercent + SystemPercent + OtherPercent.



% Returns a detailed view of the CPU usage, i.e. the average (on all cores of
% all processors of the local host) percentage of the various kinds of CPU
% utilization: { UserPercent, NicePercent, SystemPercent, IdlePercent,
% OtherPercent }, respectively for user mode, user mode with low priority
% (nice), system mode, idle task and all other usages (if any), between the two
% sets of measures.
%
% If the two sets of specified counters are equal, returns 'undefined', as no
% usage can be quantified then.
%
-spec compute_detailed_cpu_usage( cpu_usage_info(), cpu_usage_info() ) ->
	basic_utils:maybe( cpu_usage_percentages() ).
compute_detailed_cpu_usage( _StartCounters={ U1, N1, S1, I1, O1 },
							_EndCounters = { U2, N2, S2, I2, O2 } ) ->

	User = U2 - U1,

	Nice = N2 - N1,

	System = S2 - S1,

	Idle = I2 - I1,

	Other = O2 - O1,

	% This would be great if we could avoid a division by zero:
	case User + Nice + System + Idle + Other of

		% Yes, this happens:
		0.0 ->
			undefined;

		Sum ->

			RoundDigits = 1,

			UserPercent = math_utils:round_after( 100 * User / Sum,
												  RoundDigits ),

			NicePercent = math_utils:round_after( 100 * Nice / Sum,
												  RoundDigits ),

			SystemPercent = math_utils:round_after( 100 * System / Sum,
													RoundDigits ),

			IdlePercent = math_utils:round_after( 100 * Idle / Sum,
												  RoundDigits ),

			AllButOtherPercent = UserPercent + NicePercent + SystemPercent
				+ IdlePercent,

			% Avoids rounding errors:
			OtherPercent = math_utils:round_after( 100 - AllButOtherPercent,
										  RoundDigits ),

			{ UserPercent, NicePercent, SystemPercent, IdlePercent,
			  OtherPercent }

	end.



% Returns the instantaneous CPU counters, as maintained from boot.
%
% Note: mostly useful in terms of differences over time.
%
-spec get_cpu_usage_counters() -> cpu_usage_info().
get_cpu_usage_counters() ->

	% grep more versatile than: '| head -n 1':
	StatString = case run_executable(
						"/bin/cat /proc/stat | /bin/grep 'cpu '" ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { cpu_counters_inquiry_failed, ExitCode, ErrorOutput } )

	end,

	% Ex: cpu  1331302 11435 364777 150663306 82509 249 3645 0 0

	% Tells the time spent in user mode, user mode with low priority (nice),
	% system mode, and the idle task.

	[ "cpu", UserString, NiceString, SystemString, IdleString | T ] =
						string:tokens( StatString, " " ),

	User   = text_utils:string_to_integer( UserString ),
	Nice   = text_utils:string_to_integer( NiceString ),
	System = text_utils:string_to_integer( SystemString ),
	Idle   = text_utils:string_to_integer( IdleString ),

	% Adapts to any architecture and update (iowait, irq, softirq, steal, guest,
	% etc.):
	Other = lists:sum( [ text_utils:string_to_integer( E ) || E <- T ] ),

	%io:format( "user = ~f, nice = ~f, system = ~f, idle = ~f, other = ~f, "
	%			"T = ~p~n", [ User, Nice, System, Idle, Other, T ] ),

	{ User, Nice, System, Idle, Other }.



% Returns the current usage of disks, as a human-readable string.
%
-spec get_disk_usage() -> text_utils:ustring().
get_disk_usage() ->

	case run_executable( "/bin/df -h" ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { disk_usage_inquiry_failed, ExitCode,
					 ErrorOutput } )

	end.



% Returns a textual description of the current disk usage.
%
% Cannot crash.
%
-spec get_disk_usage_string() -> text_utils:ustring().
get_disk_usage_string() ->

	try

		io_lib:format( "current disk usage:~n~ts", [ get_disk_usage() ] )

	catch _AnyClass:Exception ->
			io_lib:format( "no disk usage information could be obtained (~p)",
						   [ Exception ] )

	end.





% Returns a list of the known types of pseudo-filesystems.
%
-spec get_known_pseudo_filesystems() -> [ pseudo_filesystem_type() ].
get_known_pseudo_filesystems() ->

	% A list of all current filesystems can be obtained thanks to: 'df -T'.
	[ tmpfs, devtmpfs ].



% Returns a list of the current, local mount points (excluding the
% pseudo-filesystems).
%
-spec get_mount_points() -> [ file_utils:path() ].
get_mount_points() ->

	FirstCmd = "/bin/df -h --local --output=target"
		++ get_exclude_pseudo_fs_opt() ++ " | /bin/grep -v 'Mounted on'",

	case run_executable( FirstCmd ) of

		{ _FirstExitCode=0, ResAsOneString } ->
			%io:format( "## using direct df~n" ),
			text_utils:split( ResAsOneString, "\n" );

		{ _FirstExitCode, _FirstErrorOutput } ->

			% Older versions of df may not know the --output option:
			SecondCmd = "/bin/df -h --local "
				++ get_exclude_pseudo_fs_opt()
				++ "| /bin/grep -v 'Mounted on' | \\awk '{print $6}'",

			case run_executable( SecondCmd ) of

				{ _SecondExitCode=0, ResAsOneString } ->
					%io:format( "## using legacy df~n" ),
					text_utils:split( ResAsOneString, "\n" );

				{ SecondExitCode, SecondErrorOutput } ->
					throw( { mount_point_inquiry_failed, SecondExitCode,
							 SecondErrorOutput } )

			end

	end.



% (helper for df)
%
get_exclude_pseudo_fs_opt() ->

	Excludes = [ " --exclude-type=" ++ text_utils:atom_to_string( P )
				 || P <- get_known_pseudo_filesystems() ],

	text_utils:join( _Sep=" ", Excludes ).



% Returns information about the specified filesystem.
%
-spec get_filesystem_info( file_utils:bin_path() | file_utils:path() ) ->
								 fs_info().
get_filesystem_info( BinFilesystemPath ) when is_binary( BinFilesystemPath ) ->
	get_filesystem_info( text_utils:binary_to_string( BinFilesystemPath ) );

get_filesystem_info( FilesystemPath ) ->

	Cmd = "/bin/df --block-size=1K --local "
		++ get_exclude_pseudo_fs_opt()
		++ " --output=source,target,fstype,used,avail,iused,iavail '"
		++ FilesystemPath ++ "' | /bin/grep -v 'Mounted on'",

	case run_executable( Cmd ) of

		{ _ExitCode=0, ResAsOneString } ->
			% Order of the columns: 'Filesystem / Mounted on / Type / Used /
			% Avail / IUsed / IFree':
			%
			case text_utils:split( ResAsOneString, " " ) of

				[ Fs, Mount, Type, USize, ASize, Uinodes, Ainodes ] ->

					%io:format( "## using direct df~n" ),

					% df outputs kiB, not kB:
					#fs_info{
					  filesystem=Fs,
					  mount_point=Mount,
					  type=get_filesystem_type( Type ),
					  used_size = 1024 * text_utils:string_to_integer( USize ),
					  available_size = 1024 *
						  text_utils:string_to_integer( ASize ),
					  used_inodes = text_utils:string_to_integer( Uinodes ),
					  available_inodes =
						  text_utils:string_to_integer( Ainodes )
				 };

				_ ->
					get_filesystem_info_alternate( FilesystemPath )

			end;

		{ _ExitCode, _ErrorOutput } ->
			get_filesystem_info_alternate( FilesystemPath )

	end.




% Alternate version, if the base version failed.
%
get_filesystem_info_alternate( FilesystemPath ) ->

	% df must have failed, probably outdated and not understanding --output,
	% defaulting to a less precise syntax:

	%io:format( "## using alternate df~n" ),

	Cmd = "/bin/df --block-size=1K --local "
		++ get_exclude_pseudo_fs_opt() ++ " "
		++ FilesystemPath ++ "| /bin/grep -v 'Mounted on'",

	case run_executable( Cmd ) of

		{ _ExitCode=0, ResAsOneString } ->

			case text_utils:split( ResAsOneString, " " ) of

				[ Fs, _1KBlocks,  USize, ASize, _UsedPercent, Mount ] ->

					% df outputs kiB, not kB:
					#fs_info{
					  filesystem=Fs,
					  mount_point=Mount,
					  type=unknown,
					  used_size = 1024 * text_utils:string_to_integer( USize ),
					  available_size = 1024 *
						  text_utils:string_to_integer( ASize ),
					  used_inodes = 0,
					  available_inodes = 0
					};

				_ ->
					throw( { filesystem_inquiry_failed, FilesystemPath,
							 ResAsOneString } )

			end;

		{ _ExitCode, ErrorOutput } ->
			throw( { filesystem_inquiry_failed, FilesystemPath,
					 ErrorOutput } )

	end.



% Returns a textual description of the specified filesystem information.
%
-spec filesystem_info_to_string( fs_info() ) -> text_utils:ustring().
filesystem_info_to_string( #fs_info{ filesystem=Fs, mount_point=Mount,
									 type=Type,
									 used_size=USize, available_size=ASize,
									 used_inodes=Uinodes,
									 available_inodes=Ainodes } ) ->

	% For example vfat does not have inodes:
	InodeString = case Uinodes + Ainodes of

		0 ->
			"";

		S ->
			Percent = 100 * Uinodes / S,
			text_utils:format( ", hence used at ~.1f%", [ Percent ] )

	end,

	text_utils:format( "filesystem ~s mounted on ~s (type: ~s). "
					   "Used size: ~B bytes (i.e. ~s), available size: "
					   "~B bytes (i.e. ~s) hence used at ~.1f% "
					   "(total size: ~s), "
					   "using ~B inodes and having ~B of them available~s",
					   [ Fs, Mount, Type, USize,
						 interpret_byte_size_with_unit( USize ), ASize,
						 interpret_byte_size_with_unit( ASize ),
						 100 * USize / ( USize + ASize ),
						 interpret_byte_size_with_unit( USize + ASize ),
						 Uinodes, Ainodes, InodeString ] ).



-spec get_filesystem_type( text_utils:ustring() ) -> filesystem_type().
get_filesystem_type( TypeString ) ->
	% Better for now than relying on an uncomplete list:
	text_utils:string_to_atom( TypeString ).



% Returns a textual description of the current working directory.
%
% Cannot crash.
%
-spec get_current_directory_string() -> text_utils:ustring().
get_current_directory_string() ->

	try

		io_lib:format( "current directory: ~ts",
					   [ file_utils:get_current_directory() ] )

	catch _AnyClass:Exception ->
			io_lib:format( "no information about the current directory "
						   "could be obtained (~p)", [ Exception ] )

	end.



% Returns a string describing the current operating system.
%
-spec get_operating_system_description() -> text_utils:ustring().
get_operating_system_description() ->

	OSfile = "/etc/os-release",

	case file_utils:is_existing_file_or_link( OSfile ) of

		true ->

			case run_executable( "/bin/cat " ++ OSfile ++
					" | /bin/grep PRETTY_NAME | "
					"/bin/sed 's|^PRETTY_NAME=\"||1' | "
					"/bin/sed 's|\"$||1' 2>/dev/null" ) of

				{ _ExitCode=0, Output } ->
					Output;

				{ _ExitCode, _ErrorOutput } ->
					get_operating_system_description_alternate()

			end;

		false ->
			get_operating_system_description_alternate()

	end.


get_operating_system_description_alternate() ->

	IdentifierPath = "/etc/issue.net",

	case file_utils:is_existing_file( IdentifierPath ) of

		true ->
			BinString = file_utils:read_whole( IdentifierPath ),
			text_utils:trim_whitespaces(
			  text_utils:binary_to_string( BinString ) );

		false ->
			"(unknown operating system)"

	end.



% Returns a textual description of the operating system being used.
%
% Cannot crash.
%
-spec get_operating_system_description_string() -> text_utils:ustring().
get_operating_system_description_string() ->

	try

		io_lib:format( "operating system: ~ts",
					   [ get_operating_system_description() ] )

	catch _AnyClass:Exception ->
			io_lib:format( "no information about the operating system "
						   "could be obtained (~p)", [ Exception ] )

	end.




% Returns a string describing the current state of the local system.
%
% Will not crash, even if some information could not be retrieved.
%
-spec get_system_description() -> string().
get_system_description() ->

	% We use ~ts instead of ~s as in some cases, Unicode strings might be
	% returned:
	%
	Subjects = [ get_core_count_string(),
				 get_size_of_vm_word_string(),
				 get_operating_system_description_string(),
				 get_process_count_string(),
				 get_total_physical_memory_string(),
				 get_ram_status_string(),
				 get_swap_status_string(),
				 get_user_name_string(),
				 get_user_home_directory_string(),
				 get_current_directory_string(),
				 get_disk_usage_string() ],

	text_utils:strings_to_string( Subjects ).



% Prerequisite section.

% We suppose that by default all third-party dependencies (example taken here:
% the Foobar software) are conventionally installed under a common base
% directory, which is in turn conventionally named and located just under the
% user directory.
%
% More precisely, on Unix systems, our convention requests the base directory of
% all (third-party) dependencies to be '~/Software/'.
%
% We expect to have then the name of each prerequisite specified in CamelCase;
% ex: '~/Software/Foobar/'.
%
% Finally, each version thereof shall be installed in that directory (ex: a
% clone of the Foobar repository that could be named
% '~/Software/Foobar/foobar-20170601'), and be designated by a symbolic link
% named 'Foobar-current-install', still defined in '~/Software/Foobar'.
%
% As a result, one then can always access the current version of Foobar through
% the '~/Software/Foobar/Foobar-current-install' path (possibly pointing to
% successive versions thereof over time).


% Returns the (expected, conventional) base installation directory of the
% specified third-party, prerequisite package (ex: "Foobar").
%
-spec get_dependency_base_directory( package_name() ) ->
										   file_utils:directory_name().
get_dependency_base_directory( PackageName="ErlPort" ) ->

	% ErlPort must be special-cased, as its actual base installation directory
	% *must* be named "erlport" (otherwise the interpreter initialization may
	% fail on new nodes with the {not_found,"erlport/priv"} error).
	%
	% So:
	%
	% - if the 'ERLPORT_BASE_DIR' environment variable is defined, and set to an
	% existing directory, then this directory will be retained
	%
	% - otherwise a default will be used, corresponding to the
	% '~/Software/ErlPort/erlport' directory

	case get_environment_variable( "ERLPORT_BASE_DIR" ) of


		false ->

			% Then trying default path:

			PathComponents = [ get_user_home_directory(), "Software", PackageName,
							   "erlport" ],

			DefaultDir = file_utils:normalise_path(
						   file_utils:join( PathComponents ) ),

			case file_utils:is_existing_directory( DefaultDir ) of

				true ->
					trace_utils:debug_fmt( "Using default Erlport directory '~s'.",
										   [ DefaultDir ] ),
					DefaultDir;

				false ->
					trace_utils:error_fmt( "No Erlport installation found: the "
						"ERLPORT_BASE_DIR environment variable is not defined, "
						"and the default directory ('~s') does not exist.",
						[ DefaultDir ] ),

					throw( { erlport_default_directory_not_found, DefaultDir } )

			end;


		EnvDir ->
			case filename:basename( EnvDir ) of

				"erlport" ->
					case file_utils:is_existing_directory( EnvDir ) of

						true ->
							trace_utils:debug_fmt( "Using the Erlport directory "
								"specified in the ERLPORT_BASE_DIR environment "
								"variable: '~s'.", [ EnvDir ] ),
							EnvDir;

						false ->
							trace_utils:error_fmt( "The Erlport directory "
								"specified in the ERLPORT_BASE_DIR environment "
								"variable ('~s') does not exist.", [ EnvDir ] ),

							throw( { erlport_specified_directory_not_found,
									 EnvDir } )

					end;

				_ ->
					trace_utils:error_fmt( "The Erlport directory "
								"specified in the ERLPORT_BASE_DIR environment "
								"variable ('~s') does not end with 'erlport'.",
								[ EnvDir ] ),
					throw( { invalid_erlport_specified_directory, EnvDir } )

			end

	end;


get_dependency_base_directory( PackageName ) ->

	% Expected to return a fully resolved version of the
	% "$HOME/Software/Foobar/Foobar-current-install" path, such as
	% "/home/stallone/Software/Foobar/Foobar-current-install":
	%
	PathComponents = [ get_user_home_directory(), "Software", PackageName,
					   PackageName ++ "-current-install" ],

	file_utils:normalise_path( file_utils:join( PathComponents ) ).



% Returns the (expected, conventional) code installation directory of the
% specified third-party, prerequisite, Erlang package (ex: "Foobar").
%
-spec get_dependency_code_directory( package_name() ) ->
										   file_utils:directory_name().
get_dependency_code_directory( PackageName ) ->

	% We would expect here
	% /home/stallone/Software/Foobar/Foobar-current-install/ebin:
	%
	file_utils:join( get_dependency_base_directory( PackageName ), "ebin" ).



% Tells whether a JSON support is available.
%
-spec is_json_support_available() -> boolean().
is_json_support_available() ->
	% This module can be built in all cases:
	rest_utils:is_json_parser_available().


% Returns a string explaining what to do in order to have the JSON support
% available.
%
-spec get_json_unavailability_hint() -> string().
get_json_unavailability_hint() ->
	"Hint: inspect, in common/GNUmakevars.inc, the USE_REST and "
	"JSX_BASE variables, knowing that the current code path is: "
		++ code_utils:get_code_path_as_string().


% Tells whether an HDF5 support is available.
%
-spec is_hdf5_support_available() -> boolean().
is_hdf5_support_available() ->

	% Unlike dependencies like jsx whose compilation (in rest_utils.erl) do not
	% need any specific *.hrl header (therefore rest_utils:start/0 is available
	% in all cases), hdf5_support needs one (erlhdf5.hrl), hence the
	% hdf5_support module may not be built at all, and thus will not be
	% available even in order to provide a means of telling whether HDF can be
	% supported.
	%
	% So:
	%
	case code_utils:is_beam_in_path( hdf5_support ) of

		not_found ->
			false;

		_Paths ->
			true

	end.



% Returns a string explaining what to do in order to have the HDF5 support
% available.
%
-spec get_hdf5_unavailability_hint() -> string().
get_hdf5_unavailability_hint() ->
	"Hint: inspect, in common/GNUmakevars.inc, the USE_HDF5 and "
	"ERLHDF5_BASE variables.".
