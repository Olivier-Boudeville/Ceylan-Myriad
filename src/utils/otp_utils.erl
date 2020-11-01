% Copyright (C) 2019-2020 Olivier Boudeville
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
% Creation date: Monday, July 15, 2019.


% Various helpers for OTP applications, releases, etc.
%
% Useful notably to test an OTP application from within it, from a non-OTP
% context, i.e. from a simple test without needing to create a separate,
% dedicated OTP release for that.
%
% See myriad_otp_application_test.erl as an example.
%
% Following convention is supposed to apply for testing: all applications
% (i.e. the tested one and its prerequisites) are expected to have their build
% trees (typically GIT clones) located in the same parent directory (as
% siblings), each named as its application (ex: "myriad" root directory for the
% myriad application, not for example "Ceylan-Myriad"), so that, from the build
% tree of a tested application, the build trees of its prerequisites can be
% found (ex: as "../myriad").
%
-module(otp_utils).


% Name of an OTP application (ex: 'myriad'):
-type application_name() :: atom().

% Name of an OTP application as a string (ex: "myriad"):
-type string_application_name() :: atom().

% Name of an OTP application as any legit type.
-type any_application_name() :: application_name() | string_application_name().

-type restart_type() :: application:restart_type().

% The PID of an OTP supervisor:
-type supervisor_pid() :: pid().


% Designates how an (OTP) application is run:
-type application_run_context() ::
		'as_native' % if using Ceylan native build/run system
	  | 'as_otp_release'. % if using OTP release


-export_type([ application_name/0, string_application_name/0,
			   any_application_name/0, restart_type/0, supervisor_pid/0,
			   application_run_context/0 ]).


-export([ get_string_application_name/1,

		  get_local_ebin_path_for/2, get_ebin_path_for/2,

		  prepare_for_execution/2, list_otp_standard_applications/0,

		  start_application/1, start_application/2,
		  start_applications/1, start_applications/2,

		  stop_application/1, stop_applications/1,

		  get_supervisor_settings/2,

		  check_application_run_context/1, application_run_context_to_string/1,

		  get_priv_root/1, get_priv_root/2 ]).


% Shorthands:

-type file_path() :: file_utils:file_path().
-type directory_path() :: file_utils:directory_path().

-type ustring() :: text_utils:ustring().



% Returns a string version of specified application name.
-spec get_string_application_name( application_name() ) ->
										string_application_name().
get_string_application_name( AppName ) ->
	text_utils:atom_to_string( AppName ).



% Returns the path, local to the current build tree, that should correspond to
% the ebin directory of the specified prerequisite OTP application.
%
% Ex: otp_utils:get_local_ebin_path_for( myriad, ".." ) if run from a "test"
% direct subdirectory of the current build tree.
%
% Note: the returned path is not checked for existence.
%
-spec get_local_ebin_path_for( application_name(), directory_path() ) ->
									directory_path().
get_local_ebin_path_for( AppName, BuildDir ) ->
	file_utils:join( [ BuildDir, "_build", "default", "lib",
					   get_string_application_name( AppName ), "ebin" ] ).



% Returns the path (if any) to the ebin directory of the specified prerequisite
% (user, i.e. not standard) OTP application of this test, namely the tested
% application itself or one of its own prerequisites (supposing the 'default'
% rebar3 profile being used), based on the specified root of the current build
% tree.
%
% Following locations will be searched for that ebin directory, from the root of
% its build directory, and in that order:
%  1. any local _build/default/lib/APP_NAME/ebin (for the tested application
%     itself)
%  2. any local _checkouts/APP_NAME/_build/default/lib/APP_NAME/ebin
%  3. any sibling ../APP_NAME/_build/default/lib/APP_NAME/ebin
%
% Ex: MaybeEbinPath = otp_utils:get_ebin_path_for( foobar, ".." ).
%
-spec get_ebin_path_for( application_name(), directory_path() ) ->
							maybe( directory_path() ).
get_ebin_path_for( AppName, BuildDir ) ->

	% If this application corresponds to the directly tested one (1):
	LocalEBinPath = get_local_ebin_path_for( AppName, BuildDir ),

	case file_utils:is_existing_directory_or_link( LocalEBinPath ) of

		true ->
			trace_utils:trace_fmt( "Using, for OTP application '~s', "
				"the '~s' local ebin path.", [ AppName, LocalEBinPath ] ),

			LocalEBinPath;

		% Then maybe a checkout (2):
		false ->

			%trace_utils:debug_fmt( "No local '~s' found.", [ LocalEBinPath ] ),

			AppNameStr = get_string_application_name( AppName ),

			% Not easily derived from LocalEBinPath:
			CheckoutEbinPath = file_utils:join( [ BuildDir, "_checkouts",
				AppNameStr, "_build", "default", "lib", AppNameStr, "ebin" ] ),

			case file_utils:is_existing_directory_or_link( CheckoutEbinPath ) of

				true ->
					trace_utils:trace_fmt( "Using, for the prerequisite "
						"application '~s', the '~s' checkout ebin path.",
						[ AppName, CheckoutEbinPath ] ),
					CheckoutEbinPath;

				false ->
					%trace_utils:debug_fmt( "No checkout '~s' found.",
					%					   [ CheckoutEbinPath ] ),

					% Then (3): maybe this application is a prerequisite of the
					% tested one, available in a sibling directory thereof (see
					% module notes above):
					%
					SiblingBuildDir =
						file_utils:join( [ BuildDir, "..", AppNameStr ] ),

					SiblingEBinPath =
						get_local_ebin_path_for( AppName, SiblingBuildDir ),

					case file_utils:is_existing_directory_or_link(
						   SiblingEBinPath ) of

						true ->
							trace_utils:trace_fmt( "Using, for the prerequisite"
							  " application '~s', the '~s' sibling ebin path.",
							  [ AppName, SiblingEBinPath ] ),
							SiblingEBinPath;

						false ->
							%trace_utils:debug_fmt( "No sibling '~s' found.",
							%					   [ SiblingEBinPath ] ),
							undefined

					end

			end

	end.



% Prepares the VM environment so that the specified application(s) to be
% involved in a non-OTP execution (ex: a test), and also their own
% prerequisites, can be run, based on the specified current build tree: ensures
% that their .app can be found (supposing thus that they are available and
% built), including afterwards by OTP, when starting them (thanks to updates to
% the current code path).
%
% An ordered application list is somehow semantically similar to the 'deps'
% entry of a rebar.config file.
%
% Returns whether a corresponding execution can be triggered: either ready now,
% or lacking (at least) one application.
%
-spec prepare_for_execution( application_name() | [ application_name() ],
							 directory_path() ) -> void().
prepare_for_execution( AppName, BuildDir ) when is_atom( AppName ) ->

	case lists:member( AppName, list_otp_standard_applications() ) of

		true ->
			trace_utils:trace_fmt( "Using defaults for the standard OTP "
								   "application '~s'.", [ AppName ] ),
			ready;

		false ->
			prepare_user_application_for_exec( AppName, BuildDir )

	end;

prepare_for_execution( AppNames, BuildDir ) ->
	[ prepare_for_execution( A, BuildDir ) || A <- AppNames ].



% Manages specified application (not expected to be a standard one) and its
% prerequisites, if any: checks that their .app specification can be found, that
% they are built, and updates the code path accordingly.
%
% (helper)
%
prepare_user_application_for_exec( AppName, BuildDir ) ->

	% Specific checking, just for the sake of a (non-OTP) test, that the
	% specified OTP application is already available, in the usual _build
	% directory, through the default rebar3 profile:
	%
	case get_ebin_path_for( AppName, BuildDir ) of

		undefined ->
			AbsBuildDir = file_utils:ensure_path_is_absolute( BuildDir ),
			trace_utils:error_fmt( "No build directory found for the '~s' "
				"OTP application (not belonging to the known standard Erlang "
				"applications, so searched through any local rebar _build "
				"directory, in any _checkouts one, or through its sibling "
				"applications); this execution thus cannot be performed "
				"(run beforehand 'make rebar3-compile' at the root of the "
				"~s source tree for a more relevant testing).",
				[ AppName, AbsBuildDir ] ),
			throw( { lacking_app, no_ebin_path, AppName, AbsBuildDir } );


		EBinPath ->
			% Extra checking:
			AppFilename = text_utils:format( "~s.app", [ AppName ] ),
			AppFilePath = file_utils:join( EBinPath, AppFilename ),
			case file_utils:is_existing_file_or_link( AppFilePath ) of

				true ->
					% So that this .app file can be found by OTP when starting
					% said application:
					%
					% At least an extra step is taken: checking the application
					% has a chance to run (ex: has it been compiled?) by
					% searching for its main module, as defined in its .app
					% file:

					case parse_app_spec( AppFilePath, EBinPath, BuildDir ) of

						true ->
							% Adds this ebin path to the VM code paths so that
							% the corresponding BEAM files can be found also:
							%
							code_utils:declare_beam_directory( EBinPath,
															   last_position );

						{ false, LackingModPath } ->
							trace_utils:error_fmt( "The application '~s' does "
							  "not seem to be built: its app file ('~s') "
							  "mentions at least a module whose BEAM file "
							  "('~s') could not be found.",
							  [ AppName, AppFilePath, LackingModPath ] ),

							throw( { application_not_built, AppName,
									 LackingModPath } )

					end;


				false ->
					trace_utils:error_fmt( "No '~s' file found for the '~s' "
						"OTP application (searched in '~s'), this execution "
						"cannot be performed "
						"(run beforehand 'make rebar3-compile' at the root of "
						"the ~s source tree for a more relevant testing).",
						[ AppFilename, AppName, EBinPath, AppName ] ),
					throw( { lacking_app, no_app_file, AppName, AppFilePath } )

			end

	end.



% Manages the specified .app specification: checks that the specified
% application seems indeed built, and prepares also the execution of its
% dependencies.
%
% Returns either true or false, then accompanied by the path of any lacking BEAM
% file.
%
% (helper)
%
-spec parse_app_spec( file_path(), directory_path(), directory_path() ) ->
							 'true' | { 'false', file_path() }.
parse_app_spec( AppFilePath, EBinPath, BuildDir ) ->

	trace_utils:debug_fmt( "Examining .app specification '~s'.",
						   [ AppFilePath ] ),

	case file_utils:read_terms( AppFilePath ) of

		[ { application, AppName, Entries } ] ->
			% We cannot rely on the 'mod' entry, which is defined only for
			% active applications, so:
			%
			Compiled = case list_table:lookup_entry( modules, Entries ) of

				% Abnormal, as mandatory:
				key_not_found ->
					throw( { no_modules_entry, AppName, AppFilePath } );

				{ value, [] } ->
					% No module declared, supposing that alles gut:
					true;

				% Testing just the first module found:
				{ value, [ FirstModName | _ ] } ->
					ExpectedModPath = file_utils:join( EBinPath,
							code_utils:get_beam_filename( FirstModName ) ),
					case file_utils:is_existing_file( ExpectedModPath ) of

						true ->
							true;

						false ->
							{ false, ExpectedModPath }

					end

			end,

			case Compiled of

				true ->

					% Compile check performed, now let's take care of the
					% dependencies as well:
					%
					AppDeps = list_table:get_value( applications, Entries ),

					[ prepare_for_execution( A, BuildDir ) || A <- AppDeps ],
					true;

				% i.e. { false, ExpectedModPath }:
				FalsePair ->
					FalsePair

			end;

		_ ->
			throw( { unexpected_app_spec, AppFilePath } )

	end.



% Returns a list of the applications known to belong to the OTP standard
% distribution (and thus whose availability does not have to be checked).
%
% (simply obtained by listing the directories in the lib directory at the root
% of an Erlang source tree)
%
-spec list_otp_standard_applications() -> [ application_name() ].
list_otp_standard_applications() ->
	[ asn1, common_test, compiler, crypto, debugger, dialyzer, diameter, edoc,
	  eldap, erl_docgen, erl_interface, et, eunit, ftp, hipe, inets, jinterface,
	  kernel, megaco, mnesia, observer, odbc, os_mon, parsetools, public_key,
	  reltool, runtime_tools, sasl, snmp, ssh, ssl, stdlib, syntax_tools, tftp,
	  tools, wx, xmerl ].




% Starts the specified OTP application, with the 'temporary' restart type.
%
% Note: all prerequisite applications shall have been started beforehand
% (not relying on OTP here, hence no automatic start of dependencies).
%
-spec start_application( application_name() ) -> void().
start_application( AppName ) ->
	start_application( AppName, _RestartType=temporary ).


% Starts the specified OTP application, with the specified restart type.
%
% Note: all prerequisite applications shall have been started beforehand
% (not relying on OTP here, hence no automatic start of dependencies).
%
-spec start_application( application_name(), restart_type() ) -> void().
start_application( AppName, RestartType ) ->

	trace_utils:trace_fmt( "Starting the '~s' application, with restart "
						   "type '~s'.", [ AppName, RestartType ] ),

	case application:start( AppName, RestartType ) of

		ok ->
			trace_utils:trace_fmt( "Application '~s' successfully started.",
								   [ AppName ] );

		{ error, Reason } ->
			trace_utils:error_fmt( "Application '~s' failed to start: ~p",
								   [ AppName, Reason ] ),
			throw( { app_start_failed, AppName, RestartType, Reason } )

	end.



% Starts the specified OTP applications, in their specified order, all with the
% 'temporary' restart type.
%
% Note: any prerequisite application shall have been started beforehand (not
% relying on OTP here, hence no automatic start of dependencies).
%
-spec start_applications( [ application_name() ] ) -> void().
start_applications( AppNames ) ->
	start_applications( AppNames, _RestartType=temporary ).


% Starts the specified OTP applications, sequentially and in their specified
% order, all with the specified restart type.
%
% Note: any prerequisite application shall have been started beforehand (not
% relying on OTP here, hence no automatic start of dependencies).
%
-spec start_applications( [ application_name() ], restart_type() ) -> void().
start_applications( AppNames, RestartType ) ->
	[ start_application( App, RestartType ) || App <- AppNames ].



% Stops the specified OTP application.
%
% Note: not relying on OTP here, hence dependencies shall be explicitly stopped,
% in the reverse order of the starting of these applications.
%
-spec stop_application( application_name() ) -> void().
stop_application( AppName ) ->

	trace_utils:trace_fmt( "Stopping the '~s' application.", [ AppName ] ),

	case application:stop( AppName ) of

		ok ->
			trace_utils:trace_fmt( "Application '~s' successfully stopped.",
								   [ AppName ] );

		{ error, Reason } ->
			trace_utils:error_fmt( "Application '~s' failed to stop: ~p",
								   [ AppName, Reason ] ),
			throw( { app_stop_failed, AppName, Reason } )

	end.


% Stops the specified OTP applications, sequentially and in their *reversed*
% specified order (so that the same list of prerequisite applications can be
% used both for start and stop).
%
% Note: not relying on OTP here, hence dependencies shall be explicitly stopped,
% in the reverse order of the starting of these applications.
%
-spec stop_applications( [ application_name() ] ) -> void().
stop_applications( AppNames ) ->
	[ stop_application( App ) || App <- lists:reverse( AppNames ) ].



% Returns the supervisor-level settings corresponding to the specified restart
% strategy and execution context.
%
% Note that the execution context must be explicitly specified (typically by
% calling a get_execution_target/0 function defined in a key module of that
% layer, based on Myriad's basic_utils.hrl), otherwise the one that would apply
% is the one of Myriad, not the one of the calling layer.
%
% See https://erlang.org/doc/design_principles/sup_princ.html#supervisor-flags
% for further information.
%
-spec get_supervisor_settings( supervisor:strategy(),
				   basic_utils:execution_target() ) -> supervisor:sup_flags().
get_supervisor_settings( RestartStrategy, _ExecutionTarget=development ) ->

	% No restart wanted in development mode; we do not want the supervisor to
	% hide crashes, and when an error occurs we want to see it logged once, not
	% as a longer series of that same error:
	%
	#{ strategy  => RestartStrategy,
	   intensity => _MaxRestarts=0,
	   period    => _WithinSeconds=3600 };

get_supervisor_settings( RestartStrategy, _ExecutionTarget=production ) ->

	% In production mode, we apply here basic defaults (actually the same as
	% used by the 'kernel' standard module in safe mode):
	%
	#{ strategy  => RestartStrategy,
	   intensity => _MaxRestarts=4,
	   period    => _WithinSeconds=3600 }.



% Checks that the specified application run context is legit.
-spec check_application_run_context( application_run_context() ) -> void().
check_application_run_context( _AppRunContext=as_native ) ->
	ok;

check_application_run_context( _AppRunContext=as_otp_release ) ->
	ok;

check_application_run_context( OtherAppRunContext ) ->
	throw( { invalid_application_run_context, OtherAppRunContext } ).


% Returns a textual representation of specified application run context.
-spec application_run_context_to_string( application_run_context() ) ->
											   ustring().
application_run_context_to_string( _AppRunContext=as_native ) ->
	"based on the Ceylan native native build/run system";

application_run_context_to_string( _AppRunContext=as_otp_release ) ->
	"as an OTP release".



% Returns the root directory of the application-private tree ('priv'), based on
% specified module (expected to belong to that application).
%
% It may be useful to fetch data or NIF code for example.
%
% One may specify ?MODULE as argument, provided that this module belongs to the
% application of interest.
%
-spec get_priv_root( basic_utils:module_name() ) -> directory_path().
get_priv_root( ModuleName ) ->
	get_priv_root( ModuleName, _BeSilent=false ).



% Returns the root directory of the application-private tree ('priv'), based on
% specified module (expected to belong to that application), being silent (no
% trace) if requested.
%
% It may be useful to fetch data or NIF code for example.
%
% One may specify ?MODULE as argument, provided that this module belongs to the
% application of interest.
%
-spec get_priv_root( basic_utils:module_name(), boolean() ) ->
						   directory_path().
get_priv_root( ModuleName, BeSilent ) ->

   case code:priv_dir( ModuleName ) of

	   % May happen even if being listed in the 'modules' entry of the relevant
	   % .app/.app.src files.
	   %
	   { error, PError } ->

		   % PError=bad_name, not that useful:
		   case BeSilent of

			   true ->
				   ok;

			   false ->
					trace_utils:warning_fmt( "Unable to determine 'priv' "
						"directory from module '~s': ~w.",
						[ ModuleName, PError ] )

		   end,

			case code:which( ModuleName ) of

				WError when is_atom( WError ) ->
					throw( { priv_lookup_failed, WError } );

				ObjectCodePath -> % when is_list( ObjectCodePath ) ->
					EbinDir = file_utils:get_base_path( ObjectCodePath ),
					AppPath = file_utils:get_base_path( EbinDir ),
					file_utils:join( AppPath, "priv" )

			end;

		Path ->
			Path

	end.
