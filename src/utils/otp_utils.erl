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

		  get_local_ebin_path_for/2, get_otp_paths_of/2,

		  prepare_for_execution/2,

		  start_application/1, start_application/2,
		  start_applications/1, start_applications/2,

		  stop_application/1, stop_applications/1,

		  get_supervisor_settings/2,

		  check_application_run_context/1, application_run_context_to_string/1,

		  get_priv_root/1, get_priv_root/2 ]).


% Information about how an OTP application shall be started (the function to
% call being implicitly 'start'):
%
-type app_start_info() :: { module_name(), start_args() }.

-type start_args() :: basic_utils:arguments().


% Shorthands:

-type module_name() :: basic_utils:module_name().

-type file_path() :: file_utils:file_path().
-type directory_path() :: file_utils:directory_path().
-type abs_directory_path() :: file_utils:abs_directory_path().

-type ustring() :: text_utils:ustring().




% Prepares the VM environment so that the specified top-level prerequisite
% application(s) to be involved in a non-OTP execution, and also all their own
% prerequisites in turn, can be run, based on the specified current build tree:
% ensures that their .app can be found (supposing thus that they are available
% and built), including afterwards by OTP, when starting them (thanks to updates
% to the current code path).
%
% The specified, ordered, application list is somehow semantically similar to
% the 'deps' entry of a rebar.config file.
%
% Returns an ordered list of app_start_info that shall be applied in
% turn, or a {cannot_execute, Reason} pair.
%
% Note that it is not strictly necessary to discriminate between applications
% that are active or not and to collect their start module/arguments, as
% application:start/1 seems able to support non-active applications and will
% trigger by itself the right start call.

-spec prepare_for_execution( application_name() | [ application_name() ],
			 directory_path() ) -> [ app_start_info() ].
prepare_for_execution( AppName, BuildDir ) when is_atom( AppName ) ->
	prepare_for_execution( [ AppName ], BuildDir );

prepare_for_execution( AppNames, BuildDir ) when is_list( AppNames ) ->

	% From this entry point, we prefer to deal with absolute, normalised paths:
	AbsBuildDir = file_utils:ensure_path_is_absolute( BuildDir ),

	trace_utils:debug_fmt( "Preparing for the execution of applications ~p, "
						   "from '~s'.", [ AppNames, AbsBuildDir ] ),

	% After a depth-first traversal resulting in listing paths from leaves to
	% roots, we reserve to be top to bottom (still with duplicates):
	%
	PreparedStartInfoLists = lists:reverse(
			prepare_for_exec( AppNames, AbsBuildDir, _Acc=[] ) ),

	%trace_utils:debug_fmt( "Pre-deduplication start lists:~n  ~p",
	%					   [ PreparedStartInfoLists ] ),

	FinalStartInfos = list_utils:uniquify_ordered( PreparedStartInfoLists ),

	trace_utils:debug_fmt( "Final start information list:~n  ~p",
						   [ FinalStartInfos ] ),

	FinalStartInfos.



% Manages specified application (not expected to be a standard one) and,
% recursively, all its prerequisites (direct or not), if any: checks that their
% .app specification can be found, that they are built, and updates the code
% path accordingly.
%
% Called recursively (through parse_app_spec/3).
%
% (helper)
%
-spec prepare_for_exec( application_name(), abs_directory_path(),
						[ app_start_info() ] ) -> [ app_start_info() ].
prepare_for_exec( _AppNames=[], _AbsBuildDir, Acc ) ->
	% Merges all prerequisites, in their (bottom-up) order (duplicates taken
	% care of later):
	%
	list_utils:flatten_once( Acc );

prepare_for_exec( [ AppName | T ], AbsBuildDir, Acc ) ->

	case get_otp_paths_of( AppName, AbsBuildDir ) of

		undefined ->
			trace_utils:error_fmt( "No ebin/build directories found "
				"for the '~s' OTP application (searched in turn in any "
				"rebar3 _checkouts or local _build directory, "
				"through any sibling applications; not belonging either "
				"to the known standard Erlang applications); "
				"this execution thus cannot be performed "
				"(one may run beforehand, if available, 'make rebar3-compile' "
				"at the root of the ~s source tree for a more relevant "
				" testing).", [ AppName, AbsBuildDir ] ),
			throw( { lacking_app, no_relevant_directory_found, AppName,
					 AbsBuildDir } );


		{ AbsEBinPath, AbsAppBuildDir } ->
			% Preparing the recursive lookup of prerequisites:
			AppFilename = text_utils:format( "~s.app", [ AppName ] ),
			AppFilePath = file_utils:join( AbsEBinPath, AppFilename ),

			case file_utils:is_existing_file_or_link( AppFilePath ) of

				true ->
					% We need to include this ebin path in the VM code path, so
					% the corresponding .app file and also the BEAM files of
					% that application can be found by OTP when starting it.
					%
					% We need also, in addition to recursing in prerequisite
					% applications, to handle the fact that the current
					% application might be an active one, and as such will
					% require to be specifically started, thanks to the function
					% of a module that it declared entry in its .app file (as
					% '{mod, {ModName, StartArgs}}'.
					%
					% At least an extra step is taken: checking that the
					% application has a chance to run (ex: has it been
					% compiled?) by searching for one of its modules (even if
					% not being an active application), as obtained from its
					% .app file as well.
					%
					case parse_app_spec( AppFilePath, AbsEBinPath,
										 AbsAppBuildDir ) of

						{ false, LackingModPath } ->
							trace_utils:error_fmt( "The application '~s' does "
							  "not seem to be built: its app file ('~s') "
							  "mentions at least a module whose BEAM file "
							  "('~s') could not be found.",
							  [ AppName, AppFilePath, LackingModPath ] ),

							throw( { application_not_built, AppName,
									 LackingModPath } );

						% Possibly obtained after having recursed through
						% dependencies as much as needed:
						%
						AppStartInfos ->
							% Adds this ebin path to the VM code paths so that
							% the corresponding BEAM files can be found also:
							%
							code_utils:declare_beam_directory( AbsEBinPath,
															   last_position ),
							prepare_for_exec( T, AbsBuildDir,
											  [ AppStartInfos | Acc ] )

					end;


				false ->
					trace_utils:error_fmt( "No '~s' file found for the '~s' "
						"OTP application (searched in '~s'), this execution "
						"cannot be performed "
						"(run beforehand 'make rebar3-compile' at the root of "
						"the ~s source tree for a more relevant testing).",
						[ AppFilename, AppName, AbsEBinPath, AppName ] ),
					throw( { lacking_app, no_app_file, AppName, AppFilePath } )

			end

	end.



% Returns the {AbsEbinPath, AbsBuildPath} paths (if any) to the ebin directory
% and build directory of the specified prerequisite OTP application, namely the
% executed application itself or one of its own prerequisites (supposing the
% 'default' rebar3 profile being used), based on the specified root of the
% current build tree.
%
% Following locations will be searched for the relevant directories, from the
% root of the specified build directory, and in that order:
%  1. any local _checkouts/APP_NAME/_build/default/lib/APP_NAME/ebin (a priori
%  more relevant than any _checkouts/APP_NAME/ebin)
%  2. any local _build/default/lib/APP_NAME/ebin (for the executed application
%     itself or for its dependencies)
%  3. any sibling ../APP_NAME/_build/default/lib/APP_NAME/ebin
%  4. the OTP system tree itself, where standard applications are available
%  (i.e. ${ERLANG_ROOT}/lib/erlang/lib)
%
% Returning the application build directory of this application allows to locate
% in turn any prerequisite it would have (typically through checkouts and
% siblings).
%
-spec get_otp_paths_of( application_name(), abs_directory_path() ) ->
			maybe( { abs_directory_path(), abs_directory_path() } ).
get_otp_paths_of( AppName, AbsBuildDir ) ->

	AppNameStr = get_string_application_name( AppName ),

	% Trying location #1:
	AbsCheckoutEbinPath = file_utils:join( [ AbsBuildDir, "_checkouts",
		AppNameStr, "_build", "default", "lib", AppNameStr, "ebin" ] ),

	case file_utils:is_existing_directory_or_link( AbsCheckoutEbinPath ) of

		true ->
			trace_utils:trace_fmt( "Using, for the prerequisite application "
				"'~s', the '~s' checkout ebin path.",
				[ AppName, AbsCheckoutEbinPath ] ),

			{ AbsCheckoutEbinPath, _AppBuildDir=AbsBuildDir };

		false ->
			%trace_utils:debug_fmt( "Application '~s' not found in _checkouts "
			%	"ebin path '~s', trying in _build.",
			%	[ AppName, AbsCheckoutEbinPath ] ),

			% Trying location #2, if this application corresponds to the
			% directly tested one:
			%
			AbsLocalEBinPath = get_local_ebin_path_for( AppName, AbsBuildDir ),

			case file_utils:is_existing_directory_or_link( AbsLocalEBinPath ) of

				true ->
					% Most useful trace for execution preparation:
					trace_utils:trace_fmt( "Using, for the prerequisite "
						"application '~s', the '~s' build-local ebin path.",
						[ AppName, AbsLocalEBinPath ] ),

					{ AbsLocalEBinPath, _AppBuildDir=AbsBuildDir };

				% Then trying location #3, maybe this application is a
				% prerequisite of the executed one, available in a sibling
				% directory thereof:
				%
				false ->
					%trace_utils:debug_fmt( "Application '~s' not found in "
					%	"build-local ebin path '~s', trying as sibling.",
					%	[ AppName, AbsLocalEBinPath ] ),

					% Still absolute (../APP_NAME), and normalised:
					AbsSiblingBuildDir = file_utils:normalise_path(
					  file_utils:join( file_utils:get_base_path( AbsBuildDir ),
									   AppNameStr ) ),

					AbsSiblingEBinPath =
						get_local_ebin_path_for( AppName, AbsSiblingBuildDir ),

					case file_utils:is_existing_directory_or_link(
						   AbsSiblingEBinPath ) of

						true ->
							trace_utils:trace_fmt( "Using, for the prerequisite"
							  " application '~s', the '~s' sibling ebin path.",
							  [ AppName, AbsSiblingEBinPath ] ),
							{ AbsSiblingEBinPath,
							  _AppBuildDir=AbsSiblingBuildDir };

						false ->
							%trace_utils:debug_fmt( "Application '~s' not found"
							%  " in sibling path '~s', trying as standard OTP.",
							%  [ AppName, AbsSiblingEBinPath ] ),
							case code:lib_dir( AppName ) of

								{ error, bad_name } ->
									%trace_utils:debug( "Not found either "
									%		   "as an OTP application." ),
									undefined;

								AbsStdPath ->
									% We reuse the original build dir, only
									% sensible option remaining for next
									% lookups:
									%
									{ file_utils:join( AbsStdPath, "ebin" ),
									  AbsBuildDir }

							end

					end

			end

	end.



% Returns the (absolute) path, located in the specified (absolute) build tree,
% that should correspond to the ebin directory of the specified prerequisite OTP
% application.
%
% Ex: get_local_ebin_path_for(myriad, "..") if run from a "test" direct
% subdirectory of the current build tree.
%
% Note: the returned path is not checked for existence.
%
-spec get_local_ebin_path_for( application_name(), abs_directory_path() ) ->
									abs_directory_path().
get_local_ebin_path_for( AppName, AbsBuildDir ) ->
	file_utils:join( [ AbsBuildDir, "_build", "default", "lib",
					   get_string_application_name( AppName ), "ebin" ] ).


% Returns a string version of specified application name.
-spec get_string_application_name( application_name() ) ->
										string_application_name().
get_string_application_name( AppName ) ->
	text_utils:atom_to_string( AppName ).



% Manages the specified .app specification: checks that the specified
% application seems indeed built, and prepares also the execution of its
% dependencies.
%
% Returns either {parsed, AppStartInfos} for this application and all its
% dependencies (if any; direct or not), or {failed, ModNotFoundPath} where
% ModNotFoundPath is the path of an application-defined module found lacking.

% (helper)
%
-spec parse_app_spec( file_path(), abs_directory_path(),
  abs_directory_path() ) -> [ app_start_info() ] | { 'failed', file_path() }.
parse_app_spec( AppFilePath, AbsEBinPath, AbsBuildDir ) ->

	%trace_utils:debug_fmt( "Examining .app specification in '~s'.",
	%					   [ AppFilePath ] ),

	case file_utils:read_terms( AppFilePath ) of

		[ { application, AppName, Entries } ] ->
			% To check whether this application is compiled, we cannot rely on
			% the 'mod' entry, which is defined only for active applications,
			% so:
			%
			CompileInfo = case list_table:lookup_entry( modules, Entries ) of

				% Abnormal, as mandatory:
				key_not_found ->
					throw( { no_modules_entry, AppName, AppFilePath } );

				{ value, [] } ->
					% No module declared, supposing that alles gut:
					ok;

				% Testing just the first module found:
				{ value, [ FirstModName | _ ] } ->
					ExpectedModPath = file_utils:join( AbsEBinPath,
							code_utils:get_beam_filename( FirstModName ) ),
					case file_utils:is_existing_file( ExpectedModPath ) of

						true ->
							ok;

						false ->
							{ failed, ExpectedModPath }

					end

			end,

			case CompileInfo of

				ok ->

					% Compile check performed, now let's take care of the
					% dependencies as well:
					%
					AppDeps = list_table:get_value_with_defaults( applications,
									_DefNoDep=[], Entries ),

					trace_utils:debug_fmt( "Dependencies found for '~s': ~p; "
						"using build directory '~s'.",
						[ AppName, AppDeps, AbsBuildDir ] ),

					DepStartInfos = prepare_for_exec( AppDeps, AbsBuildDir,
													  _DepAcc=[] ),

					% Add our own info if needed, i.e. if being an active
					% application:
					%
					case list_table:get_value_with_defaults( mod,
									_DefNotActive=undefined, Entries ) of

						undefined ->
							% Not an active application then:
							DepStartInfos;

						StartPair={ _StartModule, _StartArgs } ->
							[ StartPair | DepStartInfos ]

					end;

				% i.e. {false, ExpectedModPath}:
				FalsePair ->
					FalsePair

			end;

		_ ->
			throw( { invalid_app_spec, AppFilePath } )

	end.



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
