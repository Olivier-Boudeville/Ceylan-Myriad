% Copyright (C) 2007-2025 Olivier Boudeville
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
% Creation date: July 1, 2007.

-module(code_utils).

-moduledoc """
Gathering of various facilities regarding the management of **Erlang code**
(typically BEAM files).

See `code_utils_test.erl` for the corresponding test.

See also the `meta_utils` module for more module-level features (e.g. to
determine whether a given function is exported).
""".



-export([ get_code_for/1, get_md5_for_loaded_module/1,
		  get_md5_for_stored_module/1, is_loaded_module_same_on_filesystem/1,
		  deploy_modules/2, deploy_modules/3,
		  resolve_code_path/1,
		  declare_beam_directory/1, declare_beam_directory/2,
		  declare_beam_directories/1, declare_beam_directories/2,
		  remove_beam_directory/1, remove_beam_directory_if_set/1,
		  get_beam_dirs_for/1, get_beam_dirs_for_myriad/0,
		  declare_beam_dirs_for/1, declare_beam_dirs_for_myriad/0,
		  get_code_path/0, get_code_path_as_string/0,
		  code_path_to_string/0, code_path_to_string/1,
		  list_beams_in_path/0, get_beam_filename/1, is_beam_in_path/1,
		  get_source_filename/1,
		  find_module_source/1,
		  ensure_compiled/1, ensure_compiled/2, ensure_compiled/3,
		  recompile/1, recompile/2, recompile/3, recompile/4,
		  get_erlang_root_path/0,
		  get_stacktrace/0, get_stacktrace/1,
		  interpret_stacktrace/0,
		  interpret_stacktrace/1,
		  interpret_stacktrace/2,
		  interpret_shortened_stacktrace/1,
		  display_stacktrace/0,
		  interpret_error/2, interpret_undef_exception/3,
		  stack_info_to_string/1,
		  study_function_availability/3 ]).



-doc """
The code path used by a language, i.e. a list of directories (as plain strings)
to scan for runtime elements (Erlang `-pa`/`-pz`, Python `sys.path` with
`PYTHONPATH`, Java classpath, etc.).
""".
-type code_path() :: [ directory_path() ].



-doc "A code path that may comprise plain or resolvable paths.".
-type resolvable_code_path() :: [ possibly_resolvable_path() ].



-doc "A position regarding a code path.".
-type code_path_position() :: 'first_position' | 'last_position'.



-doc """
The last element of a stack item.

For example `[{file,file_path()}, {line, meta_utils:line()}]`.
""".
-type stack_info() :: map_hashtable:map_hashtable( atom(), term() )
					| list_table:list_table( atom(), term() ).



-doc "An element of a stack trace.".
-type stack_item() ::
	{ module_name(), function_name(), arity(), stack_info() }.



-doc "A stack trace.".
-type stack_trace() :: [ stack_item() ].



-doc """
Argument-level error information in a stacktrace, typically associated to the
`error_info` key in an `error_info/0` term.

Defined as `#{pos_integer() => unicode:chardata()}` by the `erl_erts_errors`
module.
""".
-type error_map() :: map_hashtable:map_hashtable( count(), ustring() ).



-doc "Name of a preprocessor define.".
-type define_name() :: ustring().



-doc "Value of a preprocessor define.".
-type define_value() :: ustring().



-doc "Describes a preprocessor define.".
-type define() :: define_name() | { define_name(), define_value() }.


-export_type([ code_path/0, resolvable_code_path/0, code_path_position/0,
			   stack_info/0, stack_item/0, stack_trace/0, error_map/0,
			   define_name/0, define_value/0, define/0 ]).



% The dotted file extension of an Erlang source file:
-define( erl_extension, ".erl" ).

% The dotted file extension of a BEAM file:
-define( beam_extension, ".beam" ).


% For the file_info record:
-include_lib("kernel/include/file.hrl").


% Type shorthands:

-type module_name() :: basic_utils:module_name().
-type function_name() :: basic_utils:function_name().
-type count() :: basic_utils:count().
-type base_status() :: basic_utils:base_status().
-type error_reason() :: basic_utils:error_reason().
-type error_term() :: basic_utils:error_term().

-type ustring() :: text_utils:ustring().

-type directory_path() :: file_utils:directory_path().
-type any_directory_path() :: file_utils:any_directory_path().

-type file_name() :: file_utils:file_name().
-type file_path() :: file_utils:file_path().
-type possibly_resolvable_path() :: file_utils:possibly_resolvable_path().

-type atom_node_name() :: net_utils:atom_node_name().

-type env_variable_name() :: system_utils:env_variable_name().

-type time_out() :: time_utils:time_out().

-type md5_sum() :: hash_utils:md5_sum().




% Code-related functions.


-doc """
Returns, by searching the code path, the in-file object code for specified
module, i.e. a `{ModuleBinary, ModuleFilename}` pair for the module specified as
an atom, or throws an exception.
""".
-spec get_code_for( module_name() ) -> { binary(), file_path() }.
get_code_for( ModuleName ) ->

	%trace_utils:debug_fmt( "Getting code for module '~ts', "
	%   "from current working directory '~ts'.",
	%   [ ModuleName, file_utils:get_current_directory() ] ),

	case code:get_object_code( ModuleName ) of

		{ ModuleName, ModuleBinary, ModuleFilename } ->
			{ ModuleBinary, ModuleFilename };

		error ->

			FoundBeams = list_beams_in_path(),

			ModString= text_utils:atoms_to_string( FoundBeams ),

			trace_utils:error_fmt( "Unable to find object code for '~ts' "
				"on '~ts', knowing that the current directory is ~ts and "
				"the ~ts~n The corresponding found BEAM files are: ~ts",
				[ ModuleName, node(), file_utils:get_current_directory(),
				  get_code_path_as_string(), ModString ] ),

			throw( { module_code_lookup_failed, ModuleName } )

	end.



-doc """
Returns the MD5 for the specified loaded (in-memory, used by the VM) module.

Otherwise returns a undefined function exception (`ModuleName:module_info/1`).
""".
-spec get_md5_for_loaded_module( module_name() ) -> md5_sum().
get_md5_for_loaded_module( ModuleName ) ->
	ModuleName:module_info( md5 ).



-doc """
Returns the MD5 for the specified stored (on filesystem, found through the code
path) module.
""".
-spec get_md5_for_stored_module( module_name() ) -> md5_sum().
get_md5_for_stored_module( ModuleName ) ->
	{ BinCode, _ModuleFilename } = get_code_for( ModuleName ),
	{ ok, { ModuleName, MD5SumBin } } = beam_lib:md5( BinCode ),
	% Now using binaries: binary_to_integer( MD5SumBin, _Base=16 ).
	MD5SumBin.



-doc """
Tells whether the specified (supposedly loaded) module is the same as the one
found through the code path.
""".
-spec is_loaded_module_same_on_filesystem( module_name() ) -> boolean().
is_loaded_module_same_on_filesystem( ModuleName ) ->
	LoadedMD5 = get_md5_for_loaded_module( ModuleName ),
	StoredMD5 = get_md5_for_stored_module( ModuleName ),
	%io:format( "Loaded MD5: ~p~nStored MD5: ~p~n", [ LoadedMD5, StoredMD5 ] ),
	LoadedMD5 == StoredMD5.



-doc """
Resolves the specified resolvable code path: returns a plain, immediately usable
code path (of course preserving path order).
""".
-spec resolve_code_path( resolvable_code_path() ) -> code_path().
resolve_code_path( AnyCodePaths ) ->
	[ file_utils:resolve_any_path( P ) || P <- AnyCodePaths ].



% RPC default time-out, in milliseconds:
% (45s, could be infinity as well)
-define( rpc_timeout, 45*1000 ).



-doc """
Deploys the specified list of modules on the specified list of nodes (specified
as atoms): sends them these modules (as a binary), and loads them so that they
are ready for future use, using a default time-out.

If an exception is thrown with `badfile` being reported as the error, this may
be caused by a version mistmatch between the Erlang environments in the source
and at least one of the remote target hosts (e.g. ERTS 5.5.2 vs 5.8.2).
""".
-spec deploy_modules( [ module() ], [ atom_node_name() ] ) -> void().
deploy_modules( Modules, Nodes ) ->
	deploy_modules( Modules, Nodes, _Timeout=?rpc_timeout ).



-doc """
Deploys the specified list of modules on the specified list of nodes (specified
as atoms): sends them these modules (as a binary), and loads them so that they
are ready for future use.

Timeout is the time-out duration, either an integer number of milliseconds, or
the 'infinity' atom.

If an exception is thrown with `badfile` being reported as the error, this may
be caused by a version mistmatch between the Erlang environments in the source
and at least one of the remote target hosts (e.g. ERTS 5.5.2 vs 5.8.2).
""".
-spec deploy_modules( [ module() ], [ atom_node_name() ], time_out() ) ->
							void().
deploy_modules( Modules, Nodes, Timeout ) ->

	% At least until the next version to come after R14B02, there was a possible
	% race condition here, as, on an a just-launched (local) node, the rpc
	% server could start to serve requests (e.g. load_binary ones for
	% file_utils) whereas the code server was not registered yet (as
	% code_server), resulting in following type of error:
	%
	% {badrpc,{'EXIT',{badarg,[{code_server,call,2},
	% {rpc,'-handle_call_call/6-fun-0-',5}]}}}
	%
	% So here we should poll until the code_server can be found registered on
	% each of the remote nodes:
	%
	naming_utils:wait_for_remote_local_registrations_of( code_server, Nodes ),

	%trace_utils:debug_fmt( "Getting code for modules ~p, on ~ts, "
	%   "whereas code path (evaluated from ~ts) is:~n  ~p",
	%   [ Modules, node(), file_utils:get_current_directory(),
	%     code:get_path() ] ),

	% Then for each module in turn, contact each and every node, in parallel:
	[ deploy_module( M, get_code_for( M ), Nodes, Timeout ) || M <- Modules ].



% (helper function)
-spec deploy_module( module(), { binary(), file_path() }, [ atom_node_name() ],
					 time_out() ) -> void().
deploy_module( ModuleName, { ModuleBinary, ModuleFilename }, Nodes, Timeout ) ->

	%trace_utils:debug_fmt( "Deploying module '~ts' (filename '~ts') "
	%   "'on nodes ~p with time-out ~p.",
	%   [ ModuleName, ModuleFilename, Nodes, Timeout ] ),

	{ ResList, BadNodes } = rpc:multicall( Nodes, code, load_binary,
		[ ModuleName, ModuleFilename, ModuleBinary ], Timeout ),

	%trace_utils:debug_fmt( "ResList = ~p, BadNodes = ~p~n",
	%                       [ ResList, BadNodes ] ),

	ReportedErrors = [ E || E <- ResList, E =/= { module, ModuleName } ],
	%trace_utils:debug_fmt( "Reported errors: ~p~n", [ ReportedErrors ] ),

	case BadNodes of

		[] ->
			case ReportedErrors of

				[] ->
					%trace_utils:debug_fmt( "Module '~ts' successfully "
					%    "deployed on ~p.~n", [ ModuleName, Nodes ] ),
					ok;

				_ ->
					% Preferring returning the full list, rather than
					% ReportedErrors:
					%
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




-doc """
Declares the specified directory as an additional code path where BEAM files
will be looked up by the VM, adding it at first position in the code path.

If this directory is already present in the code path, it is removed from its
old position and put first.

Throws an exception if the directory does not exist.
""".
-spec declare_beam_directory( any_directory_path() ) -> void().
declare_beam_directory( Dir ) ->
	declare_beam_directory( Dir, first_position ).



-doc """
Declares the specified directory as an additional code path where BEAM files
will be looked up by the VM, adding it as specified, at either first or last
position in the code path.

These functions ensure not to offset any given directory that was already in the
code path further in the code path.

Indeed, if this directory is already present in the code path:
- if first_position is specified, it is removed from its old position and put
first
- if last_position is specified, code path is not changed

Throws an exception if the directory does not exist.
""".
-spec declare_beam_directory( any_directory_path(), code_path_position() ) ->
									void().
declare_beam_directory( Dir, first_position ) ->

	cond_utils:if_defined( myriad_debug_code_path,
		trace_utils:debug_fmt( "Declaring in first position BEAM directory "
			"'~ts' in VM code path.", [ Dir ] ) ),

	% Plain string needed:
	DirStr = text_utils:ensure_string( Dir ),

	% No need to check directory for existence, code:add_patha/1 will do it:
	code:add_patha( DirStr ) =:= true orelse
		%{ error, bad_directory } ->
		throw( { non_existing_beam_directory, DirStr } );

declare_beam_directory( Dir, last_position ) ->

	cond_utils:if_defined( myriad_debug_code_path,
		trace_utils:debug_fmt( "Declaring in last position BEAM directory "
			"'~ts' in VM code path.", [ Dir ] ) ),

	% Plain string needed:
	DirStr = text_utils:ensure_string( Dir ),

	% No need to check directory for existence, code:add_pathz/1 will do it:
	code:add_pathz( DirStr ) =:= true orelse
		%{ error, bad_directory } ->of
		throw( { non_existing_beam_directory, DirStr } ).



-doc """
Declares the specified directories as additional code paths where BEAM files
will be looked up by the VM, adding them at first position in the code path.

Throws an exception if at least one of the directories does not exist.
""".
-spec declare_beam_directories( code_path() ) -> void().
declare_beam_directories( Dirs ) ->
	declare_beam_directories( Dirs, _Pos=first_position ).



-doc """
Declares the specified directories as additional code paths where BEAM files
will be looked up by the VM, adding them either at first or last position in the
code path.

Throws an exception if at least one of the directories does not exist.
""".
-spec declare_beam_directories( code_path(), code_path_position() ) -> void().
declare_beam_directories( Dirs, _Pos=first_position ) ->

	cond_utils:if_defined( myriad_debug_code_path,
		trace_utils:debug_fmt( "Declaring in first position BEAM directories "
			"~ts in VM code path.",
			[ text_utils:strings_to_listed_string( Dirs ) ] ) ),

	% As code:add_pathsa/1 does not report non-existing directories:
	check_beam_dirs( Dirs ),

	code:add_pathsa( Dirs );


declare_beam_directories( Dirs, last_position ) ->

	cond_utils:if_defined( myriad_debug_code_path,
		trace_utils:debug_fmt( "Declaring in last position BEAM directories "
			"~ts in VM code path.",
			[ text_utils:strings_to_listed_string( Dirs ) ] ) ),

	% As code:add_pathsz/1 does not report non-existing directories:
	check_beam_dirs( Dirs ),

	code:add_pathsz( Dirs ).



-doc "Checks that the specified directories exists.".
check_beam_dirs( _Dirs=[] ) ->
	ok;

check_beam_dirs( _Dirs=[ D | T ] ) ->

	% We allow symlinks (e.g. for ~/Software/X/X-current-install):
	case file_utils:is_existing_directory_or_link( D ) of

		true ->
			check_beam_dirs( T );

		false ->
			throw( { non_existing_beam_directory, D } )

	end.



-doc """
Removes the specified directory (either specified verbatim or designated as the
ebin directory of a specified application) from the current code path.

Throws an exception if the operation failed, including if it was not already
set.
""".
-spec remove_beam_directory(
		directory_path() | otp_utils:application_name() ) -> void().
remove_beam_directory( NameOrDir ) ->

	cond_utils:if_defined( myriad_debug_code_path,
		trace_utils:debug_fmt( "Removing directory designated by '~ts' "
							   "from VM code path.", [ NameOrDir ] ) ),

	case code:del_path( NameOrDir ) of

		true ->
			ok;

		false ->
			throw( { directory_not_found_in_code_path, NameOrDir } );

		{ error, bad_name }  ->
			throw( { invalid_app_name_for_code_path_removal, NameOrDir } )

	end.



-doc """
Removes the specified directory (either specified verbatim or designated as the
ebin directory of a specified application) from the current code path, if it was
already set (otherwise does nothing).

Throws an exception if the operation failed otherwise.
""".
-spec remove_beam_directory_if_set(
			directory_path() | otp_utils:application_name() ) -> void().
remove_beam_directory_if_set( NameOrDir ) ->

	cond_utils:if_defined( myriad_debug_code_path,
		trace_utils:debug_fmt( "Removing directory designated by '~ts' "
							   "from VM code path (if set).", [ NameOrDir ] ) ),

	case code:del_path( NameOrDir ) of

		true ->
			ok;

		false ->
			ok;

		{ error, bad_name }  ->
			throw( { invalid_app_name_for_code_path_removal, NameOrDir } )

	end.



-doc """
Returns the (ordered) list of (absolute) runtime BEAM directories obtained from
the build system located in the directory designated as the value associated to
the specified environment variable name.

Allows to obtain the code path that shall be declared to the VM so that all the
corresponding BEAMs become available.

Note: all code run from that function shall rely on plain Erlang, so that Myriad
itself can be made available with that module. As a result, this module can be
copied or simply symlinked from any directory, and will be usable (regarding the
get_beam_dirs_for* functions) from there as such (i.e. with no specific extra
prerequisite to take into account).

For example `get_beam_dirs_for("CEYLAN_MYRIAD")`.
""".
-spec get_beam_dirs_for( env_variable_name() ) -> code_path().
get_beam_dirs_for( VariableName ) ->

	case os:getenv( VariableName ) of

		false ->
			throw( { env_variable_not_set, VariableName } );

		BaseDir ->
			case file:read_link_info( BaseDir ) of

				{ ok, #file_info{ type=directory } } ->
					ok;

				{ ok, #file_info{ type=symlink } } ->
					ok;

				{ ok , #file_info{ type=OtherType } } ->
					throw( { invalid_filesystem_entry, OtherType, BaseDir } );

				{ error, E } ->
					throw( { directory_lookup_error, E, BaseDir } )

			end,

			Command = io_lib:format(
				"cd ~ts && make list-beam-dirs 2>/dev/null", [ BaseDir ] ),

			Dirs = string:tokens( os:cmd( Command ), _Sep="\n" ),
			%io:format( "Dirs:~n~p", [ Dirs ] )
			Dirs

	end.



-doc """
Returns the (ordered) list of (absolute) runtime BEAM directories corresponding
to this layer (i.e. the Ceylan-Myriad one).

Allows to obtain the code path that shall be declared to the VM so that all the
corresponding BEAMs become available.

The CEYLAN_MYRIAD environment variable must be defined and must point to the
corresponding root directory.

The layer top-level `ebin` directory could be used for that now that OTP
conventions are used.

Note: all code run from that function shall rely on plain Erlang, so that Myriad
itself can be made available with that module.
""".
-spec get_beam_dirs_for_myriad() -> code_path().
get_beam_dirs_for_myriad() ->
	% Expected to be set by convention in the environment:
	get_beam_dirs_for( "CEYLAN_MYRIAD" ).



-doc """
Declares automatically the relevant BEAM directories in the code path so that
the layer whose base directory is designated as the value associated to the
specified environment variable name is fully usable from then on.

Note: the determined directories are not specifically checked for existence, and
are added at the end of the code path.
""".
-spec declare_beam_dirs_for( env_variable_name() ) -> void().
declare_beam_dirs_for( VariableName ) ->
	code:add_pathsz( get_beam_dirs_for( VariableName ) ).



-doc """
Declares automatically the relevant BEAM directories in the code path so that
Ceylan-Myriad can be fully usable from then on.

Note:

- the `CEYLAN_MYRIAD` environment variable must be defined and must point to the
corresponding root directory

- the determined directories are not specifically checked for existence, and are
added at the end of the code path
""".
-spec declare_beam_dirs_for_myriad() -> void().
declare_beam_dirs_for_myriad() ->
	code:add_pathsz( get_beam_dirs_for_myriad() ).



-doc """
Returns a normalised, representation of the current code path, sorted in
alphabetical order (without duplicates).

Note that the sorting is more convenient for inspection yet implies that the
actual lookup order through these directories is most probably different.
""".
-spec get_code_path() -> code_path().
get_code_path() ->

	NormalisedPaths =
		[ file_utils:normalise_path( P ) || P <- code:get_path() ],

	lists:sort( list_utils:uniquify( NormalisedPaths ) ).



-doc """
Returns a textual representation of the current code path, sorted in
alphabetical order.

Note that the sorting is more convenient for inspection, yet implies that the
actual lookup order used by the VM through this code path is most probably
different.
""".
-spec get_code_path_as_string() -> ustring().
get_code_path_as_string() ->
	text_utils:format( "current code path (in alphabetical order) is: ~ts",
		[ text_utils:strings_to_string( get_code_path() ) ] ).



-doc """
Returns a textual description of the current code path, sorted in alphabetical
order.

Note that the sorting is more convenient for inspection yet implies that the
actual lookup order through these directories is most probably different.
""".
-spec code_path_to_string() -> ustring().
code_path_to_string() ->
	code_path_to_string( get_code_path() ).



-doc "Returns a textual description of the specified code path.".
-spec code_path_to_string( code_path() ) -> ustring().
code_path_to_string( _CodePath=[] ) ->
	% Initial space intended for caller-side consistency:
	"empty code path";

code_path_to_string( CodePath ) ->
	%trace_utils:debug_fmt( "Raw code path:~n ~p", [ CodePath ] ),

	% Should an empty path possibly happen, would be unclear, so:
	%FilteredCodePath = [ P || P <- CodePath, P =/= "" ],
	%text_utils:strings_to_enumerated_string( FilteredCodePath ).

	text_utils:strings_to_enumerated_string( CodePath ).



-doc """
Lists (in alphabetical order) all modules that exist in the current code path,
based on the BEAM files found.

Note that the sorting is more convenient for inspection yet implies that, should
a BEAM file be listed more than once (then being available in multiple paths),
the actual version that would be selected by the VM cannot be determined. See
`is_beam_in_path/1` for that.
""".
-spec list_beams_in_path() -> [ module_name() ].
list_beams_in_path() ->

	% Directly inspired from:
	% http://alind.io/post/5664209650/all-erlang-modules-in-the-code-path

	Files = [ list_to_atom( filename:basename( File, ?beam_extension ) )
				|| Path <- code:get_path(),
				   File <- filelib:wildcard( "*.beam", Path ) ],

	lists:sort( Files ).



-doc """
Returns the filename of the BEAM file corresponding to the specified module.
""".
-spec get_beam_filename( module_name() ) -> file_name().
get_beam_filename( ModuleName ) when is_atom( ModuleName ) ->
	ModuleNameString = text_utils:atom_to_string( ModuleName ),
	ModuleNameString ++ ?beam_extension.



-doc """
Tells whether the specified module has its BEAM file in the current code path.

Returns either a list of its absolute, canonicalised, unordered paths that
include this BEAM file (if being available at least once), or `not_found`.

Note:
 - hence this function does not return a boolean
 - the returned list (if any) of paths respects the order in the code path; as a
 result, its first element corresponds to the path of the BEAM file that would
 be loaded for the specified module

See also `interpret_undef_exception/3` for a direct feedback about the
availability of a given MFA.
""".
-spec is_beam_in_path( module_name() ) -> 'not_found' | [ file_path() ].
is_beam_in_path( ModuleName ) when is_atom( ModuleName ) ->

	ModuleFilename = text_utils:atom_to_string( ModuleName ) ++ ?beam_extension,

	%trace_utils:info_fmt( "Paths for module filename '~ts':~n  ~p",
	%                      [ ModuleFilename, code:get_path() ] ),

	% We have to ensure that all paths are absolute and normalised, so that we
	% can eliminate any duplicates among them (otherwise some module files could
	% be erroneously reported as being themselves duplicated):

	CurDirPath = file_utils:get_current_directory(),

	% Includes normalisation:
	VetPaths = list_utils:uniquify( [ file_utils:ensure_path_is_absolute(
		file_utils:join( P, ModuleFilename ), _BasePath=CurDirPath )
										|| P <- code:get_path() ] ),

	ExistingFilePaths =
		[ P || P <- VetPaths, file_utils:is_existing_file_or_link( P ) ],

	case ExistingFilePaths of

		[] ->
			not_found;

		Paths ->
			Paths

	end;

is_beam_in_path( Other ) ->
	throw( { non_atom_module_name, Other } ).



-doc """
Returns the filename of the (Erlang) source file corresponding to the specified
module.
""".
-spec get_source_filename( module_name() ) -> file_name().
get_source_filename( ModuleName ) when is_atom( ModuleName ) ->
	ModuleNameString = text_utils:atom_to_string( ModuleName ),
	ModuleNameString ++ ?erl_extension.



-doc """
Tries to find the source file of the specified module in the current code path.

Useful to be able to compile it.
""".
-spec find_module_source( module_name() ) -> fallible( file_path() ).
find_module_source( ModuleName ) ->

	SrcFilename = get_source_filename( ModuleName ),

	MaybeSrcPaths = [
		begin
			SrcPath = file_utils:join( P, SrcFilename ),
			case file_utils:is_existing_file_or_link( SrcPath ) of

				true ->
					SrcPath;

				false ->
					undefined

			end

		end || P <- code:get_path() ],


	case [ P || P <- MaybeSrcPaths, P =/= undefined ] of

		[ SingleSrcPath ] ->
			{ ok, SingleSrcPath };

		[] ->
			{ error, { source_file_not_found, SrcFilename } };

		MultipleSrcPaths ->
			{ error, { multiple_source_files_found, MultipleSrcPaths } }

	end.



-doc """
Compiles the specified module, by searching its sources through the code path
and requiring Myriad's build system to ensure its BEAM exists.

If a corresponding BEAM file exists, it will be rebuilt iff the build system
considers it should.
""".
-spec ensure_compiled( module_name() ) -> base_status().
ensure_compiled( ModuleName ) ->

	% Works whether or not a corresponding BEAM file exists:
	{ ok, ModSrcPath } = find_module_source( ModuleName ),

	BaseDir = file_utils:get_base_path( ModSrcPath ),

	ensure_compiled( ModuleName, BaseDir ).



-doc """
Ensures that the BEAM file corresponding to the specified module exists in the
specified directory.

Will compile the specified module in that directory (with no specific defines)
iff the build system considers it should.
""".
-spec ensure_compiled( module_name(), directory_path() ) -> base_status().
ensure_compiled( ModuleName, BaseDir ) ->
	ensure_compiled( ModuleName, BaseDir, _Defines=[] ).



-doc """
Ensures that the BEAM file corresponding to the specified module exists in the
specified directory.

Will compile the specified module in that directory, with the specified defines,
iff the build system considers it should.
""".
-spec ensure_compiled( module_name(), directory_path(), [ define() ] ) ->
										base_status().
ensure_compiled( ModuleName, BaseDir, Defines ) ->

	BEAMFilename = get_beam_filename( ModuleName ),

	MakeExecPath = executable_utils:get_make_path(),

	% We could use the -C/--directory but, in order to better emulate the usual
	% context, we change directory by ourselves for this execution (only):
	%
	% (-s: silent)
	%
	Args = [ "-s", BEAMFilename ] ++ make_options_for( Defines ),

	case system_utils:run_executable( MakeExecPath, Args,
			system_utils:get_standard_environment(),
			_WorkingDir=BaseDir ) of

		{ _ReturnCode=0, _Output="" } ->
			%trace_utils:debug_fmt( "Module '~ts' successfully "
			%   "recompiled, with no specific output.",
			%    ModuleName ] ),
			ok;

		{ _ReturnCode=0, _Output } ->
			% Generally just the base build message like:
			% "   Compiling standard module foobar.beam".
			%
			%trace_utils:warning_fmt( "Module '~ts' successfully "
			%   "recompiled, with output: '~ts'.",
			%   [ ModuleName, Output ] ),
			ok;

		{ ErrorCode, Output } ->
			trace_utils:error_fmt( "Failed to recompile module '~ts': "
				"error code ~B, output: '~ts'.",
				[ ModuleName, ErrorCode, Output ] ),
			{ error,
			  { module_recompilation_failed, ModuleName, Output } }

	end.



-doc """
Recompiles forcibly the specified module (even if the build system does not
detect a source change; this is typically useful if changing compilation
settings), with no specific define.

A (single) corresponding (possibly obsolete) BEAM file is expected to already
exist.

Relies on Myriad's build system, rules and parametrisation, that we deem is the
most flexible and robust option.
""".
-spec recompile( module_name() ) -> base_status().
recompile( ModuleName ) ->
	recompile( ModuleName, _ForceRecompilation=true ).



-doc """
Recompiles forcibly the specified module (even if the build system does not
detect a source change; this is typically useful if changing compilation
settings).

A (single) corresponding (possibly obsolete) BEAM file is expected to already
exist.

Relies on Myriad's build system, rules and parametrisation, that we deem is the
most flexible and robust option.
""".
-spec recompile( module_name(), boolean() ) -> base_status().
recompile( ModuleName, ForceRecompilation ) ->
	recompile( ModuleName, ForceRecompilation, _Defines=[] ).



-doc """
Recompiles the specified module, forcibly or not, with the specified
preprocessor defines.

If ForceRecompilation is true, the specified module will be recompiled even if
the build system does not detect a source change; this is typically useful if
changing compilation settings.

A (single) corresponding (possibly obsolete) BEAM file is expected to already
exist.

Relies on Myriad's build system, rules and parametrisation, that we deem is the
most flexible and robust option.
""".
-spec recompile( module_name(), boolean(), [ define() ] ) -> base_status().
recompile( ModuleName, ForceRecompilation, Defines ) ->

	% compile:file/2, the 'make' module, 'erl -compile' or 'erlc' could be used
	% instead, but then plenty of options would have to be taken into account.

	%trace_utils:debug_fmt( "Compiling module name '~ts'.", [ ModuleName ] ),

	case is_beam_in_path( ModuleName ) of

		not_found ->
			{ error, { module_not_found, ModuleName } };

		[ SinglePath ] ->
			BaseDir = file_utils:get_base_path( SinglePath ),
			recompile( ModuleName, BaseDir, ForceRecompilation, Defines );

		MultiplePaths ->
			{ error, { multiple_modules_found, ModuleName, MultiplePaths } }

	end.



-doc """
Recompiles the specified module, forcibly or not, with the specified
preprocessor defines, in the specified directory.

If ForceRecompilation is true, the specified module will be recompiled even if
the build system does not detect a source change; this is typically useful if
changing compilation settings.

A (single) corresponding (possibly obsolete) BEAM file is expected to already
exist.

Relies on Myriad's build system, rules and parametrisation, that we deem is the
most flexible and robust option.
""".
-spec recompile( module_name(), directory_path(), boolean(), [ define() ] ) ->
									base_status().
recompile( ModuleName, BaseDir, ForceRecompilation, Defines ) ->

	BeamPath = file_utils:join( BaseDir, get_beam_filename( ModuleName ) ),

	ForceRecompilation andalso file_utils:remove_file_if_existing( BeamPath ),

	ensure_compiled( ModuleName, BaseDir, Defines ).



-doc """
Returns a list of arguments corresponding to any specified preprocessor defines;
if none is specified, the defaults will be applied (as opposed to resetting to
no option).

Other compilation options may be supported in the future.

Returning a list allows no argument to be returned, whereas an empty one (`""`)
would still be seen by make afterwards (e.g. `"make: *** empty string invalid as
file name.  Stop."`).

Returning for example `["ERLANG_COMPILER_EXTRA_OPTS='-Dfoo -Dbar=1'"]`.
""".
-spec make_options_for( [ define() ] ) -> [ ustring() ].
make_options_for( _Defines=[] ) ->
	% Let defaults apply, rather than discarding them with
	% ERLANG_COMPILER_EXTRA_OPTS="":
	%
	[];

make_options_for( Defines ) ->
	make_options_for( Defines, _Acc=[] ).


make_options_for( _Defines=[], Acc ) ->
	% Preferring to respect define order:
	[ "ERLANG_COMPILER_EXTRA_OPTS='"
		++ list_utils:flatten_once( lists:reverse( Acc ) ) ++ "'" ];

make_options_for( _Defines=[ { DefineNameStr, DefineValueStr } | T ], Acc ) ->

	DefStr = text_utils:format( "-D~ts=~ts",
								[ DefineNameStr, DefineValueStr ] ),

	make_options_for( T, [ DefStr | Acc ] );

make_options_for( _Defines=[ DefineNameStr | T ], Acc ) ->
	make_options_for( T, [ "-D" ++ DefineNameStr | Acc ] ).



-doc """
Returns the root directory of Erlang/OTP, where it is installed.

For example `"/home/joe/Software/Erlang/Erlang-23.1/lib/erlang"` or
`"/usr/local/otp/lib"`.
""".
-spec get_erlang_root_path() -> directory_path().
get_erlang_root_path() ->
   code:root_dir().



-doc """
Returns (without crashing the program) the current stack trace.

A replacement for the deprecated `erlang:get_stacktrace/0`.
""".
-spec get_stacktrace() -> stack_trace().
get_stacktrace() ->
	get_stacktrace( _SkipLastElemCount=0 ).



-doc """
Returns (without crashing the program) the current stack trace, whose
`SkipLastElemCount` first elements have been dropped (to output a cleaner, more
relevant stacktrace).
""".
-spec get_stacktrace( count() ) -> stack_trace().
get_stacktrace( SkipLastElemCount ) ->
	try

		throw( generate_stacktrace )

	catch throw:generate_stacktrace:Stacktrace ->

		%trace_utils:debug_fmt( "Got stacktrace: ~p", [ Stacktrace ] ),

		% To remove the initial call to code_utils:get_stacktrace/0, by design
		% at the top of the stack:
		%
		list_utils:remove_first_elements( Stacktrace, SkipLastElemCount+1 )

	end.



-doc """
Returns a "smart" textual representation of the current stacktrace.
""".
-spec interpret_stacktrace() -> ustring().
interpret_stacktrace() ->

	% We do not want to include interpret_stacktrace/0 in the stack:
	Stacktrace = get_stacktrace( _SkipLastElemCount=1 ),

	interpret_stacktrace( Stacktrace ).



-doc """
Returns a "smart" textual representation of the specified stacktrace.
""".
-spec interpret_stacktrace( stack_trace() ) -> ustring().
interpret_stacktrace( Stacktrace ) ->
	interpret_stacktrace( Stacktrace, _ErrorTerm=undefined ).



-doc """
Returns a "smart", complete textual description of the specified error
stacktrace, including any argument-level analysis of the failure, listing just
the filename of the corresponding source files (no full path wanted).
""".
-spec interpret_stacktrace( stack_trace(), option( error_term() ) ) ->
										ustring().
interpret_stacktrace( Stacktrace, MaybeErrorTerm ) ->
	interpret_stacktrace( Stacktrace, MaybeErrorTerm, _FullPathsWanted=false ).



-doc """
Returns a "smart", complete textual description of the specified error
stacktrace, including any argument-level analysis of the failure, listing either
the full path of the corresponding source files, or just their filename.
""".
-spec interpret_stacktrace( stack_trace(), option( error_term() ),
							boolean() ) -> ustring().
% At least one stack item expected:
interpret_stacktrace( Stacktrace=[ FirstStackItem | OtherStackItems ],
					  MaybeErrorTerm, FullPathsWanted ) ->

	% Use any error diagnosis regarding top-level stack item:
	ErrorStr = interpret_stack_item( FirstStackItem, FullPathsWanted )
		++ case MaybeErrorTerm of

			undefined ->
				"";

			ErrorTerm ->
				interpret_error( ErrorTerm, Stacktrace )

		   end,

	OtherItemStrs = [ interpret_stack_item( I, FullPathsWanted )
							|| I <- OtherStackItems ],

	StringItems = [ ErrorStr | OtherItemStrs ],

	text_utils:strings_to_enumerated_string( StringItems ).



-doc """
Returns a "smart" textual representation of the current stacktrace, once
specified extra depth has been skipped (not counting this call).

Removing the specified number of last calls allows to skip unwanted
error-reporting functions and to return only a relevant stacktrace.
""".
-spec interpret_shortened_stacktrace( basic_utils:count() ) -> ustring().
interpret_shortened_stacktrace( SkipLastElemCount ) ->
	interpret_stacktrace( get_stacktrace( SkipLastElemCount ) ).



% Helper:
interpret_stack_item( { Module, Function, Arity, StackInfo },
					  FullPathsWanted ) when is_integer( Arity ) ->
	text_utils:format( "~ts:~ts/~B~ts", [ Module, Function, Arity,
		get_location_from( StackInfo, FullPathsWanted ) ] );

% Here we have not a raw arity, but the list of actual arguments (thus a lot
% more informative):
%
interpret_stack_item( { Module, Function, Args, StackInfo }, FullPathsWanted )
									when is_list( Args ) ->

	ArgStr = text_utils:format( "~p", [ Args ] ),

	% Any '~' in arguments must be escaped, otherwise next format will fail:
	EscapedArgStr = string:replace( _In=ArgStr, _SearchPattern="~",
									_Replacement="\~", _Where=all ),

	% Based on actual characters:
	FullArgStr = case length( ArgStr ) > 50 of

		true ->
			"~n  ";

		false ->
			" "

				 end ++ EscapedArgStr,

	% As FullArgStr must be interpreted, not used verbatim:
	text_utils:format(
		"~ts:~ts/~B called with the following list of arguments:"
			++ FullArgStr ++ "~ts",
		[ Module, Function, length( Args ),
		  get_location_from( StackInfo, FullPathsWanted ) ] );

% Never fail:
interpret_stack_item( I, _FullPathsWanted ) ->
	text_utils:format( "~p (error: unexpected stack item)", [ I ] ).



-doc """
Returns a textual description of the location (if any) found from the specified
error information.
""".
-spec get_location_from( stack_info(), boolean() ) -> ustring().
get_location_from( StackInfo, FullPathsWanted )
									when is_map( StackInfo ) ->
	get_location_from( map_hashtable:enumerate( StackInfo ), FullPathsWanted );

get_location_from( StackInfo, FullPathsWanted ) ->

	%trace_utils:debug_fmt( "get_location_from: StackInfo is ~p",
	%   [ StackInfo ] ).

	% Not wanted here (succeeds even if key not found):
	NoErrInfo = list_table:remove_entry( error_info, StackInfo ),

	{ MaybeFilePath, FileLessInfo } =
		case list_table:extract_entry_with_default( file, undefined,
													NoErrInfo ) of

			P={ undefined, _SInfo } ->
				P;

			{ SetFilePath, SInfo } ->
				Path = case FullPathsWanted of

					true ->
						file_utils:normalise_path( SetFilePath );

					false ->
						filename:basename( SetFilePath )

				end,
				{ Path, SInfo }

		end,

	{ MaybeLine, LineLessInfo } =
		list_table:extract_entry_with_default( line, undefined, FileLessInfo ),

	ExtraStr = case LineLessInfo of

		[] ->
			"";

		_ ->
			text_utils:format( " (warning: following stack information was "
							   "ignored: ~p)", [ LineLessInfo ] )

	end,

	case { MaybeFilePath, MaybeLine } of

		{ undefined, undefined } ->
			text_utils:format( "~ts", [ ExtraStr ] );

		{ FilePath, undefined } ->
			text_utils:format( "   [defined in ~ts]", [ FilePath ] );

		{ undefined, Line } ->
			text_utils:format( "   [defined at line ~B]", [ Line ] );

		{ FilePath, Line } ->
			text_utils:format( "   [defined in ~ts (line ~B)]",
							   [ FilePath, Line ] )

	end.



-doc "Displays the current stacktrace (not stopping the execution).".
-spec display_stacktrace() -> void().
display_stacktrace() ->

	% We do not want to include display_stacktrace/0 in the stack:
	Stacktrace = get_stacktrace( _SkipLastElemCount=1 ),
	trace_utils:info_fmt( "Current stacktrace is (latest calls first): ~ts~n",
						  [ interpret_stacktrace( Stacktrace ) ] ).



-doc "Interprets the specified error.".
-spec interpret_error( error_term(), stack_trace() ) -> ustring().
interpret_error( ErrorTerm, Stacktrace=[
		_StackInfo={ _Module, _Function, _Arguments, InfoListTable } | _ ] ) ->

	%trace_utils:debug_fmt( "interpret_error: Reason=~p, Stacktrace=~n ~p",
	%                       [ ErrorTerm, Stacktrace ] ),

	case list_table:lookup_entry( error_info, InfoListTable ) of

		{ value, ErrorInfoMap } ->
			case map_hashtable:lookup_entry( module, ErrorInfoMap ) of

				% Typically erl_erts_errors for BIFs:
				{ value, ErrorInfoModule } ->
					% Possibly erl_erts_errors;
					%trace_utils:debug_fmt( "Error info module: '~ts'.",
					%                       [ ErrorInfoModule ] ),
					DiagnoseMap =
						ErrorInfoModule:format_error( ErrorTerm, Stacktrace ),
					error_map_to_string( DiagnoseMap, ErrorTerm );

				key_not_found ->
					"(no module set for error_info) "
						++ stack_info_to_string( InfoListTable )

			end;

		key_not_found ->
			% No extra information lies here:
			%"(no error_info set) " ++ stack_info_to_string( StackInfo )
			""

	end.



-doc "Returns a textual description of the specified stack information.".
-spec stack_info_to_string( stack_info() ) -> ustring().
stack_info_to_string( [ { file, Filename }, { line, Line } ] ) ->
	text_utils:format( "in file ~ts, at line ~B", [ Filename, Line ] );

% Catch-all:
stack_info_to_string( StackInfo ) ->

	% We could look up here also any error_info entry, yet any error module
	% found there would require to have its format_error/2 function be called
	% with the error reason and the full stacktrace, both of which are
	% unavailable in this context.

	text_utils:format( "~p", [ StackInfo ] ).



-doc "Returns a textual description of the specified error map.".
-spec error_map_to_string( error_map(), error_reason() ) -> ustring().
error_map_to_string( ErrorMap, Reason ) ->

	%trace_utils:debug_fmt( "ErrorMap=~p, Reason=~p.", [ ErrorMap, Reason ] ),

	case lists:sort( map_hashtable:enumerate( ErrorMap ) ) of

		[] ->
			% This may happen (e.g. if executing <<"hello">> ++ [world]):
			%text_utils:format( "~ts (whereas no error listed - abnormal)",
			%                   [ error_reason_to_string( Reason ) ] );
			error_reason_to_string( Reason );

		% Special case as with the next one two ':' in the same sentence would
		% be used:
		%
		[ { N, Diag } ] ->
			text_utils:format( "~ts due to invalid argument #~B: ~ts",
							   [ error_reason_to_string( Reason ), N, Diag ] );

		NumberedErrors ->
			ErrorStrs = [ text_utils:format( "invalid argument #~B: ~ts",
							[ N, Diag ] ) || { N, Diag } <- NumberedErrors ],

			text_utils:format( "~ts due to: ~ts",
				[ error_reason_to_string( Reason ),
				  text_utils:strings_to_string( ErrorStrs, _IndentLevel=1 ) ] )

	end.



-doc "Returns a textual description of the specified error reason.".
-spec error_reason_to_string( error_reason() ) -> ustring().
error_reason_to_string( Reason ) ->
	text_utils:format( " that failed with ~p", [ Reason ] ).



-doc "Interprets an `undef` exception, typically after it has been raised.".
-spec interpret_undef_exception( module_name(), function_name(), arity() ) ->
										ustring().
interpret_undef_exception( ModuleName, FunctionName, Arity ) ->

	%trace_utils:debug_fmt( "Interpreting undef for ~p:~p/~p.",
	%                       [ ModuleName, FunctionName, Arity ] ),

	case is_beam_in_path( ModuleName ) of

		not_found ->
			text_utils:format( "no module '~ts' found in code path, "
				"which explains why its ~ts/~B function is reported "
				"as being undefined; ~ts",
				[ ModuleName, FunctionName, Arity,
				  get_code_path_as_string() ] );


		ModulePath ->

			case meta_utils:get_arities_for( ModuleName,
											 FunctionName ) of

				[] ->
					text_utils:format( "module '~ts' found in code path "
						"(as '~ts'), yet it does not export a '~ts' function "
						"(for any arity)",
						[ ModuleName, ModulePath, FunctionName ] );

				Arities ->
					interpret_arities( ModuleName, FunctionName, Arity,
									   Arities, ModulePath )

			end

	end.


% (helper)
interpret_arities( ModuleName, FunctionName, Arity, Arities, ModulePath ) ->

	case lists:member( Arity, Arities ) of

		true ->
			% Should never happen?
			text_utils:format( "module '~ts' found in code path (as '~ts'), "
				"and it exports the ~ts/~B function indeed",
				[ ModuleName, ModulePath, FunctionName, Arity ] );

		false ->
			ArStr = case Arities of

				[ A ] ->
					case A - Arity of

						_SingleLacking=1 ->
							text_utils:format( "arity ~B (only), the call "
								"may lack one argument", [ A ] );

						MoreLacking when MoreLacking > 1 ->
							text_utils:format( "arity ~B (only), the call "
								"may lack ~B arguments", [ A, MoreLacking ] );

						_SingleExtra=-1 ->
							text_utils:format( "arity ~B (only), the call "
								"may have one extra argument", [ A ] );

						MoreExtra when MoreExtra < -1 ->
							text_utils:format( "arity ~B (only), the call "
								"may have ~B extra arguments",
								[ A, -MoreExtra ] )

					end;

				_ ->
					Ars = [ text_utils:integer_to_string( I )
								|| I <- lists:sort( Arities ) ],

					ArsStr = text_utils:strings_to_listed_string( Ars ),

					text_utils:format( "other arities (i.e. ~ts), "
						"maybe the call to that function was made "
						"with a wrong number of parameters", [ ArsStr ] )

			end,

			text_utils:format( "module '~ts' found in code path (as '~ts'), "
				"yet it does not export a ~ts/~B function; as it exports "
				"this function for ~ts.",
				[ ModuleName, ModulePath, FunctionName, Arity, ArStr ] )

	end.



-doc """
Reports whether the specified function is available, and if not returns details
in order to facilitate any diagnosis.
""".
-spec study_function_availability( module_name(), function_name(), arity() ) ->
										ustring().
study_function_availability( ModuleName, FunctionName, Arity ) ->

	case is_beam_in_path( ModuleName ) of

		not_found ->
			text_utils:format( "no module '~ts' found in code path "
				"(its ~ts/~B function therefore cannot be called); ~ts",
				[ ModuleName, FunctionName, Arity,
				  get_code_path_as_string() ] );


		ModulePath ->

			case meta_utils:get_arities_for( ModuleName,
											 FunctionName ) of

				[] ->
					case meta_utils:list_exported_functions( ModuleName ) of

						[] ->
							text_utils:format( "module '~ts' found in code path"
								" (as '~ts'), yet it does not export any "
								"function", [ ModuleName, ModulePath ] );

						FunPairs ->
							FunDescStr = text_utils:strings_to_string( [
								text_utils:format( "~ts/~B", [ FName, FArity ] )
									|| { FName, FArity } <- FunPairs ] ),

							text_utils:format( "module '~ts' found in code path"
								" (as '~ts'), yet it does not export a '~ts' "
								"function (for any arity); it exports only "
								"the following ~B functions: ~ts",
								[ ModuleName, ModulePath, FunctionName,
								  length( FunPairs ), FunDescStr ] )

					end;

				Arities ->
					interpret_arities( ModuleName, FunctionName, Arity,
									   Arities, ModulePath )

			end

	end.
