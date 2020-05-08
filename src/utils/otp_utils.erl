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


-export_type([ application_name/0, string_application_name/0,
			   any_application_name/0, restart_type/0, supervisor_pid/0 ]).


-export([ get_string_application_name/1,

		  get_local_ebin_path_for/2, get_ebin_path_for/2,

		  prepare_for_test/2,

		  start_application/1, start_application/2,
		  start_applications/1, start_applications/2,

		  stop_application/1, stop_applications/1,

		  get_priv_root/1, get_priv_root/2 ]).


% Shorthands:

-type directory_path() :: file_utils:directory_path().



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
% OTP application of this test, namely the tested application itself or one of
% its own prerequisites (supposing the 'default' rebar3 profile being used),
% based on the specified root of the current build tree.
%
% Ex: MaybeEbinPath = otp_utils:get_ebin_path_for( foobar, ".." ).
%
-spec get_ebin_path_for( application_name(), directory_path() ) ->
							maybe( directory_path() ).
get_ebin_path_for( AppName, BuildDir ) ->

	% If this application corresponds to the directly tested one:
	LocalEBinPath = get_local_ebin_path_for( AppName, BuildDir ),

	case file_utils:is_existing_directory_or_link( LocalEBinPath ) of

		true ->
			trace_utils:trace_fmt( "Using, for the tested application '~s', "
				"the '~s' local ebin path.", [ AppName, LocalEBinPath ] ),
			LocalEBinPath;

		% Then maybe this application is a prerequisite of the tested one, available
		% in a sibling directory thereof (see module notes above):
		%
		false ->
			PrereqBuildDir = file_utils:join(
				[ BuildDir, "..", get_string_application_name( AppName ) ] ),

			PrereqEBinPath = get_local_ebin_path_for( AppName, PrereqBuildDir ),

			case file_utils:is_existing_directory_or_link( PrereqEBinPath ) of

				true ->
					trace_utils:trace_fmt( "Using, for the prerequisite "
						"application '~s', the '~s' sibling ebin path.",
						[ AppName, PrereqEBinPath ] ),

					PrereqEBinPath;

				false ->
					undefined

			end

	end.



% Prepares the specified application(s) to be involved in a test, based on the
% specified current build tree: ensures that their .app can be found (supposing
% thus that they are available and built), including afterwards by OTP when
% starting them (thanks to updates to the current code path).
%
% Returns whether a corresponding test can be run: either ready or lacking (at
% least) one application.
%
-spec prepare_for_test( application_name() | [ application_name() ],
		directory_path() ) -> 'ready' | { 'lacking_app', application_name() }.
prepare_for_test( AppName, BuildDir ) when is_atom( AppName ) ->

	% Specific checking, just for the sake of a (non-OTP) test, that the
	% specified OTP application is already available, in the usual _build
	% directory, through the default rebar3 profile:
	%
	case get_ebin_path_for( AppName, BuildDir ) of

		undefined ->
			trace_utils:warning_fmt( "No build directory found for the '~s' "
				"OTP application (searched from '~s', locally or through "
				"siblings), this test cannot be performed "
				"(run beforehand 'make rebar3-compile' at the root of the "
				"~s source tree for a more relevant testing).",
				[ AppName, BuildDir, AppName ] ),
			{ lacking_app, AppName };


		EBinPath ->
			% Extra checking:
			AppFilename = text_utils:format( "~s.app", [ AppName ] ),
			AppFilePath = file_utils:join( EBinPath, AppFilename ),
			case file_utils:is_existing_file_or_link( AppFilePath ) of

				true ->
					% So that this .app file can be found by OTP when starting
					% said application:
					%
					code_utils:declare_beam_directory( EBinPath ),
					ready;

				false ->
					trace_utils:warning_fmt( "No '~s' file found for the '~s' "
						"OTP application (searched in '~s'), this test cannot "
						"be performed"
						"(run beforehand 'make rebar3-compile' at the root of "
						"the ~s source tree for a more relevant testing).",
						[ AppFilename, AppName, EBinPath, AppName ] ),
					{ lacking_app, AppName }

			end

	end;

prepare_for_test( _AppNames=[], _BuildDir ) ->
	ready;

prepare_for_test( _AppNames=[ AppName | T ], BuildDir ) ->
	case prepare_for_test( AppName, BuildDir ) of

		ready ->
			prepare_for_test( T, BuildDir );

		LackPair ->
			LackPair

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



% Returns the root directory of the application-private tree ('priv'), based on
% specified module (expected to belong to that application).
%
% It may be useful to fetch data or NIF code for example.
%
% One may specified ?MODULE as argument, provided this module belongs to the
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
% One may specified ?MODULE as argument, provided this module belongs to the
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
					EbinDir = filename:dirname( ObjectCodePath ),
					AppPath = filename:dirname( EbinDir ),
					file_utils:join( AppPath, "priv" )

			end;

		Path ->
			Path

	end.
