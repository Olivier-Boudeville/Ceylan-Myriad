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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: July 1, 2007.


% Gathering of various facilities regarding the management of Erlang code
% (typically BEAM files).
%
% See code_utils_test.erl for the corresponding test.
%
-module(code_utils).


-export([ get_code_for/1, get_md5_for_loaded_module/1,
		  get_md5_for_stored_module/1, is_loaded_module_same_on_filesystem/1,
		  deploy_modules/2, deploy_modules/3,
		  declare_beam_directory/1, declare_beam_directory/2,
		  declare_beam_directories/1, declare_beam_directories/2,
		  get_code_path/0, get_code_path_as_string/0,
		  list_beams_in_path/0, get_beam_filename/1, is_beam_in_path/1,
		  interpret_stacktrace/0, interpret_stacktrace/1,
		  interpret_stacktrace/2,
		  interpret_stack_item/2 ]).


%-type stack_location() :: [ { file, file_utils:path() },
%							 { line, meta_utils:line() } ].

-type stack_location() :: [ { atom(), any() } ].


-type stack_item() :: { basic_utils:module_name(),
						basic_utils:function_name(),
						arity(),
						stack_location() }.


-type stack_trace() :: [ stack_item() ].


% The file extension of a BEAM file:
-define( beam_extension, ".beam" ).


% Code-related functions.


% Returns, by searching the code path, the in-file object code for specified
% module, i.e. a { ModuleBinary, ModuleFilename } pair for the module specified
% as an atom, or throws an exception.
%
-spec get_code_for( basic_utils:module_name() ) ->
						  { binary(), file:filename() }.
get_code_for( ModuleName ) ->

	case code:get_object_code( ModuleName ) of

		{ ModuleName, ModuleBinary, ModuleFilename } ->
			{ ModuleBinary, ModuleFilename };

		error ->
			throw( { module_code_lookup_failed, ModuleName } )

	end.



% Returns the MD5 for the specified loaded (in-memory, used by the VM) module.
%
% Otherwise returns a undefined function exception (ModuleName:module_info/1).
%
-spec get_md5_for_loaded_module( basic_utils:module_name() ) ->
									   executable_utils:md5_sum().
get_md5_for_loaded_module( ModuleName ) ->
	ModuleName:module_info( md5 ).



% Returns the MD5 for the specified stored (on filesystem, found through the
% code path) module.
%
-spec get_md5_for_stored_module( basic_utils:module_name() ) ->
									   executable_utils:md5_sum().
get_md5_for_stored_module( ModuleName ) ->
	{ BinCode, _ModuleFilename } = get_code_for( ModuleName ),
	{ ok, { ModuleName, MD5SumBin } } = beam_lib:md5( BinCode ),
	binary_to_integer( MD5SumBin, _Base=16 ).



% Tells whether the specified (supposedly loaded) module is the same as the one
% found through the code path.
%
-spec is_loaded_module_same_on_filesystem( basic_utils:module_name() ) ->
												 boolean().
is_loaded_module_same_on_filesystem( ModuleName ) ->
	LoadedMD5 = get_md5_for_loaded_module( ModuleName ),
	StoredMD5 = get_md5_for_stored_module( ModuleName ),
	%io:format( "Loaded MD5: ~p~nStored MD5: ~p~n", [ LoadedMD5, StoredMD5 ] ),
	LoadedMD5 == StoredMD5.



% RPC default time-out, in milliseconds:
% (30s, could be infinity)
-define( rpc_timeout, 30*1000 ).



% Deploys the specified list of modules on the specified list of nodes (atoms):
% sends them these modules (as a binary), and loads them so that they are ready
% for future use.
%
% If an exception is thrown with 'badfile' being reported as the error, this may
% be caused by a version mistmatch between the Erlang environments in the source
% and at least one of the remote target hosts (ex: ERTS 5.5.2 vs 5.8.2).
%
-spec deploy_modules( [ module() ], [ net_utils:atom_node_name() ] ) ->
							basic_utils:void().
deploy_modules( Modules, Nodes ) ->
	deploy_modules( Modules, Nodes, _Timeout=?rpc_timeout ).



% Deploys the specified list of modules on the specified list of nodes (atoms):
% sends them these modules (as a binary), and loads them so that they are ready
% for future use.
%
% Timeout is the time-out duration, either an integer number of milliseconds, or
% the infinity atom.
%
% If an exception is thrown with 'badfile' being reported as the error, this may
% be caused by a version mistmatch between the Erlang environments in the source
% and at least one of the remote target hosts (ex: ERTS 5.5.2 vs 5.8.2).
%
-spec deploy_modules( [ module() ], [ net_utils:atom_node_name() ],
					  time_utils:time_out() ) -> basic_utils:void().
deploy_modules( Modules, Nodes, Timeout ) ->

	% At least until the next version to come after R14B02, there was a possible
	% race condition here, as, on an a just-launched (local) node, the rpc
	% server could start to serve requests (ex: load_binary ones for file_utils)
	% whereas the code server was not registered yet (as code_server), resulting
	% in following type of error:
	%
	% {badrpc,{'EXIT',{badarg,[{code_server,call,2},
	% {rpc,'-handle_call_call/6-fun-0-',5}]}}}
	%
	% So here we should poll until the code_server can be found registered on
	% each of the remote nodes:
	naming_utils:wait_for_remote_local_registrations_of( code_server, Nodes ),

	% Then for each module in turn, contact each and every node in parallel:
	[ deploy_module( M, get_code_for( M ), Nodes, Timeout ) || M <- Modules ].



% (helper function)
-spec deploy_module( module(), { binary(), file_utils:file_name() },
		  [ net_utils:atom_node_name() ], time_utils:time_out() ) ->
						   basic_utils:void().
deploy_module( ModuleName, { ModuleBinary, ModuleFilename }, Nodes, Timeout ) ->

	%io:format( "Deploying module '~s' (filename '~s') on nodes ~p "
	%		  "with time-out ~p.~n",
	%		  [ ModuleName, ModuleFilename, Nodes, Timeout ] ),

	{ ResList, BadNodes } = rpc:multicall( Nodes, code, load_binary,
				[ ModuleName, ModuleFilename, ModuleBinary ], Timeout ),

	%io:format( "ResList = ~p, BadNodes = ~p~n", [ ResList, BadNodes ] ),

	ReportedErrors = [ E || E <- ResList, E =/= { module, ModuleName } ],
	%io:format( "Reported errors: ~p~n", [ ReportedErrors ] ),

	case BadNodes of

		[] ->
			case ReportedErrors of

				[] ->
					%io:format( "Module '~s' successfully deployed on ~p.~n",
					%		[ ModuleName, Nodes ] ),
					ok;

				_ ->
					% Preferring returning the full list, rather than
					% ReportedErrors:
					throw( { module_deployment_failed, ModuleName, ResList } )

			end;

		_ ->
			throw( { module_deployment_failed, ModuleName,
					 { ResList, BadNodes } } )

	end.

	% Optionally, do some checking:
	% Check = [ { N, rpc:call( N, code, is_loaded, [ ModuleName ] ) }
	%   || N <- Nodes ],

	% % Performs two tasks, error selection and badrpc removal:
	% RPCErrors = [ {N,Reason} || { N, {badrpc,Reason} } <- Check ],
	% LoadFailingNodes = [ N || { N, false } <- Check ],
	% case RPCErrors of

	%	[] ->

	%		case LoadFailingNodes of

	%			[] ->
	%				ok;

	%			_ ->
	%				throw( { deploy_module_checking_failed, LoadFailingNodes } )

	%		end;

	%	_ ->
	%		throw( { deploy_module_checking_error, RPCErrors, LoadFailingNodes }
	% )

	% end.



% Declares specified directory as an additional code path where BEAM files will
% be looked up by the VM, adding it at first position in the code path.
%
% Throws an exception if the directory does not exist.
%
-spec declare_beam_directory( file_utils:directory_name() ) ->
									basic_utils:void().
declare_beam_directory( Dir ) ->
	declare_beam_directory( Dir, first_position ).



% Declares specified directory as an additional code path where BEAM files will
% be looked up by the VM, adding it at first position in the code path.
%
% Throws an exception if the directory does not exist.
%
-spec declare_beam_directory( file_utils:directory_name(),
		 'first_position' | 'last_position' ) -> basic_utils:void().
declare_beam_directory( Dir, first_position ) ->

	case code:add_patha( Dir ) of

		true ->
			ok;

		{ error, bad_directory } ->
			throw( { non_existing_beam_directory, Dir } )

	end;

declare_beam_directory( Dir, last_position ) ->

	case code:add_pathz( Dir ) of

		true ->
			ok;

		{ error, bad_directory } ->
			throw( { non_existing_beam_directory, Dir } )

	end.



% Declares specified directories as additional code paths where BEAM files will
% be looked up by the VM, adding them at first position in the code path.
%
% Throws an exception if at least one of the directories does not exist.
%
-spec declare_beam_directories( [ file_utils:directory_name() ] ) ->
									  basic_utils:void().
declare_beam_directories( Dirs ) ->
	declare_beam_directories( Dirs, first_position ).



% Declares specified directories as additional code paths where BEAM files will
% be looked up by the VM, adding them either at first or last position in the
% code path.
%
% Throws an exception if at least one of the directories does not exist.
%
-spec declare_beam_directories( [ file_utils:directory_name() ],
			'first_position' | 'last_position' ) -> basic_utils:void().
declare_beam_directories( Dirs, first_position ) ->
	check_beam_dirs( Dirs ),
	code:add_pathsa( Dirs );

declare_beam_directories( Dirs, last_position ) ->
	check_beam_dirs( Dirs ),
	code:add_pathsz( Dirs ).



% Checks that specified directories exist.
%
% (helper)
%
check_beam_dirs( _Dirs=[] ) ->
	ok;

check_beam_dirs( _Dirs=[ D | T ] ) ->

	% We allow symlinks (ex: for ~/Software/X/X-current-install):
	case file_utils:is_existing_directory_or_link( D ) of

		true ->
			check_beam_dirs( T );

		false ->
			throw( { non_existing_beam_directory, D } )

	end.



% Returns a normalised, sorted list of directories in the current code path
% (without duplicates).
%
-spec get_code_path() -> [ file_utils:directory_name() ].
get_code_path() ->

	NormalisedPaths =
		[ file_utils:normalise_path( P ) || P <- code:get_path() ],

	lists:sort( list_utils:uniquify( NormalisedPaths ) ).



% Returns a textual representation of the current code path.
%
-spec get_code_path_as_string() -> string().
get_code_path_as_string() ->

	CodePath = get_code_path(),

	text_utils:format( "current code path is:~s",
					   [ text_utils:strings_to_string( CodePath ) ] ).



% Lists all modules that exist in the current code path, based on the BEAM files
% found.
%
-spec list_beams_in_path() -> [ basic_utils:module_name() ].
list_beams_in_path() ->

	% Directly inspired from:
	% http://alind.io/post/5664209650/all-erlang-modules-in-the-code-path

	[ list_to_atom( filename:basename( File, ?beam_extension ) )
		|| Path <- code:get_path(),
		   File <- filelib:wildcard( "*.beam", Path ) ].



% Returns the filename of the BEAM file corresponding to specified module.
%
-spec get_beam_filename( basic_utils:module_name() ) -> file_utils:file_name().
get_beam_filename( ModuleName ) when is_atom( ModuleName ) ->

	ModuleNameString = text_utils:atom_to_string( ModuleName ),

	ModuleNameString ++ ?beam_extension.




% Tells whether specified module has its BEAM file in the current code path.
%
% Returns either a list of its paths (if being available at least once), or
% 'not_found'.
%
% Note that a given module can be nevertheless found more than once, typically
% if reachable from the current directory and an absolute one in the code path.
%
-spec is_beam_in_path( basic_utils:module_name() ) ->
							 'not_found' | [ file_utils:path() ].
is_beam_in_path( ModuleName ) when is_atom( ModuleName ) ->

	ModuleNameString = text_utils:atom_to_string( ModuleName ),

	case list_utils:uniquify(
		   [ file_utils:normalise_path( file_utils:join( Path, File ) )
			 || Path <- code:get_path(),
				File <- filelib:wildcard( "*.beam", Path ),
				filename:basename( File, ?beam_extension ) =:=
					ModuleNameString ] ) of

		[] ->
			not_found;

		Paths ->
			Paths

	end.




% Returns a "smart" textual representation of the current stacktrace.
%
-spec interpret_stacktrace() -> string().
interpret_stacktrace() ->
	StackTrace = erlang:get_stacktrace(),
	interpret_stacktrace( StackTrace ).


% Returns a "smart" textual representation of specified stacktrace.
%
-spec interpret_stacktrace( stack_trace() ) -> string().
interpret_stacktrace( StackTrace ) ->
	interpret_stacktrace( StackTrace, _FullPathsWanted=false ).


% Returns a "smart" textual representation of specified stacktrace, listing
% either the full path of the corresponding source files, or just their
% filename.
%
-spec interpret_stacktrace( stack_trace(), boolean() ) -> string().
interpret_stacktrace( StackTrace, FullPathsWanted ) ->

	%io:format( "Interpreting stack trace:~n~p~n", [ StackTrace ] ),

	StringItems = [ interpret_stack_item( I, FullPathsWanted )
					|| I <- StackTrace ],

	text_utils:strings_to_enumerated_string( StringItems ).




% Helper:
interpret_stack_item( { Module, Function, Arity, [ { file, FilePath },
												   { line, Line } ] },
					  _FullPathsWanted=true ) when is_integer( Arity ) ->
	text_utils:format( "~s:~s/~B   [defined in ~s (line ~B)]",
					   [ Module, Function, Arity,
						 file_utils:normalise_path( FilePath ),
						 Line ] );

interpret_stack_item( { Module, Function, Arity, [ { file, FilePath },
												   { line, Line } ] },
					  _FullPathsWanted=false ) when is_integer( Arity ) ->
	text_utils:format( "~s:~s/~B   [defined in ~s (line ~B)]",
					   [ Module, Function, Arity,
						 filename:basename( FilePath ),
						 Line ] );

interpret_stack_item( { Module, Function, Args, [ { file, FilePath },
												  { line, Line } ] },
					  _FullPathsWanted=false ) when is_list( Args ) ->
	text_utils:format( "~s:~s/~B   [defined in ~s (line ~B)]",
					   [ Module, Function, length( Args ),
						 filename:basename( FilePath ),
						 Line ] );

interpret_stack_item( { Module, Function, Arity, Location },
					  _FullPathsWanted ) when is_integer( Arity ) ->
	text_utils:format( "~s:~s/~B located in ~p",
					   [ Module, Function, Arity, Location ] );

interpret_stack_item( { Module, Function, Arguments, _Location=[] },
					  _FullPathsWanted ) when is_list( Arguments ) ->
	text_utils:format( "~s:~s/~B", [ Module, Function, length( Arguments ) ] );

interpret_stack_item( { Module, Function, Arguments, Location },
					  _FullPathsWanted ) when is_list( Arguments ) ->
	text_utils:format( "~s:~s/~B located in ~p",
					   [ Module, Function, length( Arguments ), Location ] );

% Never fail:
interpret_stack_item( I, _FullPathsWanted ) ->
	text_utils:format( "~p", [ I ] ).
