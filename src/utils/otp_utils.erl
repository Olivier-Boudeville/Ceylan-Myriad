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
		'as_native'       % if using Ceylan native build/run system
	  | 'as_otp_release'. % if using OTP release


-export_type([ application_name/0, string_application_name/0,
			   any_application_name/0, restart_type/0, supervisor_pid/0,
			   application_run_context/0 ]).


-export([ get_string_application_name/1,

		  prepare_for_execution/2,

		  start_application/1, start_application/2,
		  start_applications/1, start_applications/2,

		  stop_application/1, stop_applications/1,

		  get_supervisor_settings/2,

		  check_application_run_context/1, application_run_context_to_string/1,

		  get_priv_root/1, get_priv_root/2 ]).



% Shorthands:

-type module_name() :: basic_utils:module_name().

-type file_path() :: file_utils:file_path().
-type directory_path() :: file_utils:directory_path().
-type bin_directory_path() :: file_utils:bin_directory_path().
-type abs_directory_path() :: file_utils:abs_directory_path().

-type ustring() :: text_utils:ustring().

% Entries corresponding to the application specifications (see
% https://erlang.org/doc/man/app.html) read from a .app file:
%
-type app_spec() :: list_table:list_table().


% Information regarding a (generally prerequisite, OTP) application:
-record( app_info, {

		   % Stored here also only for convenience:
		   app_name :: application_name(),

		   % The (absolute) base root of that application:
		   root_dir :: bin_directory_path(),

		   % The (absolute) ebin directory root of that application:
		   ebin_dir :: bin_directory_path(),

		   % If set, means that it is an active application:
		   start_mod_args :: maybe( { module_name(),
									  basic_utils:arguments() } ),

		   % As contained in its .app file:
		   spec :: app_spec() }).

-type app_info() :: #app_info{}.



% A table allowing to look-up dependencies only once (and not many times, like
% for the kernel application):
%
-type app_table() :: table( application_name(), app_info() ).




% Prepares the VM environment so that the specified top-level prerequisite
% application(s) to be involved in a non-OTP execution - and also all their own
% prerequisites recursively in turn - can be run, based on the specified current
% base tree: ensures that their .app can be found (supposing thus that they are
% available and built, checks being performed), including afterwards by OTP,
% when starting them (thanks to updates to the current code path).
%
% Returns an ordered, complete list (with no duplicates) of applications names
% that shall be started in turn (otherwise throws an exception), so that each
% application is started once if active (or not at all if non-active), and after
% all its prerequisites.
%
% The specified, ordered, application list is somehow semantically similar to
% the 'deps' entry of a rebar.config file.
%
% Prerequisites that are common to multiple dependencies are supposed to be of a
% single version.
%
% Note that our previous implementation (refer to commit 5fffa10) used to
% discriminate between applications that are active or not, and to collect their
% start module/arguments; this is not necessary though, as application:start/1
% seems able to support non-active applications, and for active ones to trigger
% by itself the right start call.
%
% Application information is now cached, so that base applications (ex: kernel)
% are not repeatedly searched, but just once.
%
-spec prepare_for_execution( application_name() | [ application_name() ],
							 directory_path() ) -> [ application_name() ].
prepare_for_execution( AppName, BaseDir ) when is_atom( AppName ) ->
	prepare_for_execution( [ AppName ], BaseDir );

prepare_for_execution( AppNames, BaseDir ) when is_list( AppNames ) ->

	% From this entry point, we prefer to deal with absolute, normalised paths:
	AbsBaseDir = file_utils:ensure_path_is_absolute( BaseDir ),

	trace_utils:debug_fmt( "Preparing for the execution from '~s' "
		"of following top-level applications:~n  ~p",
		[ AbsBaseDir, AppNames ] ),

	{ FullDeps, _FinalAppTable } = prepare_for_exec( AppNames, AbsBaseDir,
										 _AccDeps=[], _AppTable=table:new() ),

	% After a depth-first traversal resulting in listing paths from leaves to
	% roots, we reverse the results in order to enforce a top to bottom order
	% (still with duplicates):
	%
	PreparedApps = lists:reverse( FullDeps ),

	%trace_utils:debug_fmt( "Pre-deduplication start lists:~n  ~p",
	%					   [ PreparedStartInfoLists ] ),

	% Now each prerequisite shall be started once, the first time it is useful:
	FinalApps = list_utils:uniquify_ordered( PreparedApps ),

	trace_utils:debug_fmt( "Final application list to be started in turn:~n ~p",
						   [ FinalApps ] ),

	FinalApps.



% Manages specified application and, recursively, all its prerequisites (direct
% or not), if any: checks that their .app specification can be found, that they
% are built, updates the code path accordingly and lists the active ones.
%
% Called recursively (through parse_app_spec/3).
%
% (helper)
%
-spec prepare_for_exec( application_name(), abs_directory_path(),
			[ application_name() ], app_table() ) ->
							  { [ application_name() ], app_table() }.
prepare_for_exec( _AppNames=[], _AbsBaseDir, AccDeps, AppTable ) ->
	% Merges all prerequisites, in their (currently bottom-up) order (duplicates
	% taken care of later):
	%
	%list_utils:flatten_once( AccDeps );
	{ AccDeps, AppTable };


prepare_for_exec( [ AppName | T ], AbsBaseDir, AccDeps, AppTable ) ->

	case get_app_info( AppName, AbsBaseDir, AppTable ) of

		undefined ->
			trace_utils:error_fmt( "No application information found "
				"for the '~s' OTP application (searched in turn in local ebin, "
				"rebar3 _checkouts or _build directory, through any sibling "
				"applications or as a standard application; "
				"this execution thus cannot be performed "
				"(one may run beforehand, if relevant, 'make rebar3-compile' "
				"at the root of the ~s source tree for a more relevant "
				"testing).", [ AppName, AbsBaseDir ] ),
			throw( { lacking_app, no_relevant_directory_found, AppName,
					 AbsBaseDir } );

		{ #app_info{ root_dir=BinAppBaseDir, spec=AppEntries },
		  DirectAppTable } ->

			% All checks already performed, ebin directory already added in the
			% code path when generating application information (we need to
			% include this ebin path in the VM code path so that the
			% corresponding .app file and also the BEAM files of that
			% application can be found by OTP when starting it).
			%
			DepAppNames = list_table:get_value_with_defaults( applications,
												  _DefNoDep=[], AppEntries ),

			trace_utils:debug_fmt( "Preparing for the execution of application "
				"'~s', whose direct dependencies are: ~w.",
				[ AppName, DepAppNames ] ),

			{ CompleteDepApps, DepAppTable } = prepare_for_exec( DepAppNames,
				BinAppBaseDir, _NestedAppDeps=[], DirectAppTable ),

			prepare_for_exec( T, AbsBaseDir,
				[ AppName | CompleteDepApps ] ++ AccDeps, DepAppTable )

	end.



% Returns the information about the specified OTP application, namely the
% executed application itself or one of its own (direct or indirect)
% prerequisites (supposing the 'default' rebar3 profile being used, if
% relevant), based on the specified root of the current base tree.
%
% Following locations will be searched for the relevant directories, from the
% root of the specified base directory, and in that order:
%
%  1. local ebin (for the executed application itself)
%  2. any local _checkouts/APP_NAME/ebin (a priori more relevant than any
%  _checkouts/APP_NAME/_build/default/lib/APP_NAME/ebin )
%  3. any local _build/default/lib/APP_NAME/ebin (for the dependencies of the
%  executed application)
%  4. any sibling ../APP_NAME/ebin (rather than
%  ../APP_NAME/_build/default/lib/APP_NAME/ebin)
%  5. the installed OTP system tree itself, where standard applications are
%  available (i.e. in ${ERLANG_ROOT}/lib/erlang/lib)
%
% Returning also the application base directory of this application allows to
% locate in turn any prerequisite it would have (typically through checkouts,
% local _build or siblings).
%
-spec get_app_info( application_name(), abs_directory_path(), app_table() ) ->
						  { app_info(), app_table() }.
get_app_info( AppName, AbsBaseDir, AppTable ) ->

	case table:lookup_entry( AppName, AppTable ) of

		% Already cached:
		{ value, AppInfo } ->
			{ AppInfo, AppTable };

		key_not_found ->
			generate_app_info( AppName, AbsBaseDir, AppTable )

	end.



% Generates and stores the information regarding the specified application.
% Modifies the code path.
%
-spec generate_app_info( application_name(), abs_directory_path(),
						 app_table() ) -> { app_info(), app_table() }.
generate_app_info( AppName, AbsBaseDir, AppTable ) ->

	trace_utils:trace_fmt( "Generating information for application '~s' "
		"from '~s'.", [ AppName, AbsBaseDir ] ),

	% We used to search only for an 'ebin' path, yet, for example in the case of
	% sibling directories, a wrong ebin directory could be selected. Now, to
	% avoid any mistake, we look up the full path of the target .app file.

	AppNameStr = get_string_application_name( AppName ),

	AppFilename = AppNameStr ++ ".app",

	% Trying location #1 (see get_app_info/3 comment):
	ThisEBinDir = file_utils:join( AbsBaseDir, "ebin" ),

	ThisAppFilePath = file_utils:join( ThisEBinDir, AppFilename ),

	trace_utils:debug_fmt( "[1] Application '~s' looked up locally based "
		"on '~s'.", [ AppName, ThisAppFilePath ] ),

	{ AppFilePath, EBinDir, NewBaseDir } =
		case file_utils:is_existing_file_or_link( ThisAppFilePath ) of

		true ->
			trace_utils:trace_fmt( "Using, for the application '~s', "
				"the directly local '~s' file.", [ AppName, ThisAppFilePath ] ),
			{ ThisAppFilePath, ThisEBinDir, AbsBaseDir };

		% Trying location #2, if this application is in a local checkout:
		false ->
			CheckBaseDir = file_utils:join(
							 [ AbsBaseDir, "_checkouts", AppNameStr ] ),

			CheckEBinDir = file_utils:join( CheckBaseDir, "ebin" ),

			CheckoutAppPath = file_utils:join( CheckEBinDir, AppFilename ),

			trace_utils:debug_fmt( "[2] Application '~s' not found directly "
				"in local ebin, trying in local checkout, based on '~s'.",
				[ AppName, CheckoutAppPath ] ),

			case file_utils:is_existing_file_or_link( CheckoutAppPath ) of

				true ->
					trace_utils:trace_fmt( "Using, for the application '~s', "
						"the local checkout '~s' file.",
						[ AppName, CheckoutAppPath ] ),
					{ CheckoutAppPath, CheckEBinDir, CheckBaseDir };

				% Then trying #3, i.e. as a local build dependency:
				false ->
					DepEBinDir = file_utils:join( [ AbsBaseDir, "_build",
						"default", "lib", AppNameStr, "ebin" ] ),

					DepAppPath = file_utils:join( DepEBinDir, AppFilename ),

					trace_utils:debug_fmt( "[3] Application '~s' not found in "
						"local checkout, trying as a local build dependency, "
						"based on '~s'.", [ AppName, DepAppPath ] ),

					% To avoid too much nesting:
					try_next_locations( AppName, AppNameStr, AppFilename,
										DepEBinDir, DepAppPath, AbsBaseDir )

			end

	end,

	AppInfo = interpret_app_file( AppFilePath, AppName, EBinDir, NewBaseDir ),

	trace_utils:debug( app_info_to_string( AppInfo ) ),

	NewAppTable = table:add_entry( AppName, AppInfo, AppTable ),

	% We take advantage that, for each application needed, this code is executed
	% exactly once to update the VM code path for it (so that, on start-up, the
	% OTP procedure finds its .app file and andalso its BEAM files):
	%
	code_utils:declare_beam_directory( EBinDir, last_position ),

	{ AppInfo, NewAppTable }.



% (helper)
try_next_locations( AppName, AppNameStr, AppFilename, DepEBinDir, DepAppPath,
					AbsBaseDir ) ->

	case file_utils:is_existing_file_or_link( DepAppPath ) of

		true ->
			trace_utils:trace_fmt( "Using, for the application '~s', the local "
				"build dependency '~s' file.", [ AppName, DepAppPath ] ),
			{ DepAppPath, DepEBinDir, AbsBaseDir };

		% Trying #4, i.e. as a sibling application:
		false ->
			% Still absolute (../APP_NAME), and normalised:
			SibBaseDir = file_utils:join(
				file_utils:get_base_path( AbsBaseDir ), AppNameStr ),

			SibEbinDir = file_utils:join( SibBaseDir, "ebin" ),

			SibAppPath= file_utils:join( SibEbinDir, AppFilename ),

			trace_utils:debug_fmt( "[4] Application '~s' not found as a local "
				"build dependency, trying as a sibling based on '~s'.",
				[ AppName, SibAppPath ] ),

			case file_utils:is_existing_file_or_link( SibAppPath ) of

				true ->
					trace_utils:trace_fmt( "Using, for the application '~s', "
						"the sibling '~s' file.", [ AppName, SibAppPath ] ),
					{ SibAppPath, SibEbinDir, SibBaseDir };

				% Trying #5, i.e. as a standard OTP application:
				false ->
					trace_utils:debug_fmt( "[5] Application '~s' not found as "
						"a sibling, trying as a standard OTP application.",
						[ AppName ] ),

					case code:lib_dir( AppName ) of

						{ error, bad_name } ->
							trace_utils:error_fmt( "Application '~s' not found "
								"either as an OTP application.", [ AppName ] ),
							throw( { application_not_found, AppName,
									 AbsBaseDir } );

						AbsStdPath ->
							StdEbinDir = file_utils:join( AbsStdPath, "ebin" ),
							StdAppPath= file_utils:join( StdEbinDir,
														 AppFilename ),
							case file_utils:is_existing_file( StdAppPath ) of

								true ->
									trace_utils:trace_fmt( "Using, for the "
									  "application '~s', the OTP '~s' file.",
									  [ AppName, StdAppPath ] ),

									{ StdAppPath, StdEbinDir, AbsStdPath };

								% Abnormal:
								false ->
									throw( { otp_app_file_not_found,
											 StdAppPath } )

							end

					end

			end

	end.



% Returns a string version of specified application name.
-spec get_string_application_name( application_name() ) ->
										string_application_name().
get_string_application_name( AppName ) ->
	text_utils:atom_to_string( AppName ).


% Searches for the BEAM file corresponding to the specified module in the
% specified ebin directory, otherwise through current code path.
%
-spec look_up_beam( module_name(), abs_directory_path(), file_path(),
					application_name() ) -> void().
look_up_beam( ModuleName, EBinPath, AppFilePath, AppName ) ->

	TestedBeamFilename = code_utils:get_beam_filename( ModuleName ),

	ExpectedModPath = file_utils:join( EBinPath, TestedBeamFilename ),

	case file_utils:is_existing_file( ExpectedModPath ) of

		true ->
			ok;

		false ->
			% This might be normal in the case of a Ceylan application, as they
			% do not gather their BEAM files in a single (ebin) directory.
			% However then their build system shall have ensured that the right
			% directories are already in the code path. Checking that:
			%
			case code_utils:is_beam_in_path( ModuleName ) of

				not_found ->
					trace_utils:error_fmt( "The application '~s' whose "
						"information is in '~s' does not seem compiled "
						"(neither found as '~s' nor through the code path).",
						[ AppName, AppFilePath, ExpectedModPath ] ),
					throw( { not_compiled, AppName, TestedBeamFilename } );

				_DirPaths ->
					ok

			end

	end.


% Interprets the specification of the application whose .app file is specified.
%
% Returns the specification of the specified application, as read from its
% specified (supposedly already checked for existence) .app file.
%
% (helper)
%
-spec interpret_app_file( file_path(), application_name(),
	abs_directory_path(), abs_directory_path() ) -> app_spec().
interpret_app_file( AppFilePath, AppName, EBinPath, BaseDir ) ->

	trace_utils:debug_fmt( "Examining application specification in '~s'.",
						   [ AppFilePath ] ),

	case file_utils:read_terms( AppFilePath ) of

		[ { application, AppName, Entries } ] ->

			ActiveInfo = list_table:get_value_with_defaults( mod,
												 _Def=undefined, Entries ),

			% To check whether this application is compiled, we cannot rely on
			% the 'mod' entry, which is defined only for *active* applications,
			% so:
			%
			case list_table:lookup_entry( modules, Entries ) of

				% Abnormal, as mandatory:
				key_not_found ->
					throw( { no_modules_entry, AppName, AppFilePath } );

				{ value, [] } ->
					% No module declared (weird); supposing that alles gut:
					trace_utils:warning_fmt( "Application '~s' declared no "
						"module; supposing that it is built.", [ AppName ] );

				% Testing just the first module found:
				{ value, [ FirstModName | _ ] } ->
					look_up_beam( FirstModName, EBinPath, AppFilePath, AppName )

			end,

			#app_info{ app_name=AppName,
					   root_dir=text_utils:string_to_binary( BaseDir ),
					   ebin_dir=text_utils:string_to_binary( EBinPath ),
					   start_mod_args=ActiveInfo,
					   spec=Entries };

		_ ->
			throw( { invalid_app_spec, AppFilePath, AppName } )

	end.


% Returns a textual representation of specified application information.
-spec app_info_to_string( app_info() ) -> ustring().
app_info_to_string( #app_info{ app_name=AppName,
							   root_dir=RootDir,
							   ebin_dir=EBinDir,
							   start_mod_args=ActiveInfo,
							   spec=Entries } ) ->

	ActStr = case ActiveInfo of

		undefined ->
			"not an active application";

		{ ModName, Args } ->
			text_utils:format( "active application (to be run as "
				"~s:start(~p))", [ ModName, Args ] )

	end,

	%Longer = true,
	Longer = false,

	case Longer of

		true ->
			text_utils:format( "Application information for '~s': root "
				"directory is '~s', ebin one is '~s', ~s, and spec is:~n  ~p",
				[ AppName, RootDir, EBinDir, ActStr, Entries ] );

		false ->
			text_utils:format( "Application information for '~s': root "
				"directory is '~s', ebin one is '~s', ~s, spec having "
				"~B entries",
				[ AppName, RootDir, EBinDir, ActStr, length( Entries ) ] )

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



% Starts the specified OTP applications (if not started yet), sequentially and
% in the specified order, all with the 'temporary' restart type.
%
% Note: any non-included prerequisite application shall have been started
% beforehand (not relying on OTP here, hence no automatic start of
% dependencies).
%
-spec start_applications( [ application_name() ] ) -> void().
start_applications( AppNames ) ->
	start_applications( AppNames, _RestartType=temporary ).


% Starts the specified OTP applications (if not started yet), sequentially and
% in the specified order, all with the specified restart type.
%
% Note: any non-included prerequisite application shall have been started
% beforehand (not relying on OTP here, hence no automatic start of
% dependencies).
%
-spec start_applications( [ application_name() ], restart_type() ) -> void().
start_applications( _AppNames=[], _RestartType ) ->
	ok;

start_applications( [ AppName | T ], RestartType ) ->

	trace_utils:debug_fmt( "Starting application '~s' with restart type '~s'.",
						   [ AppName, RestartType ] ),

	% Not needing to use our knowledge about this application being active or
	% not:
	%
	case application:ensure_started( AppName, RestartType ) of

		ok ->
			start_applications( T, RestartType );

		{ error, Reason } ->
			throw( { start_failed, AppName, Reason } )

	end.



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
-spec get_priv_root( module_name() ) -> directory_path().
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
-spec get_priv_root( module_name(), boolean() ) -> directory_path().
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
