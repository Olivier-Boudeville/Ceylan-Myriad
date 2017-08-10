% Copyright (C) 2003-2017 Olivier Boudeville
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
% Creation date: Saturday, July 12, 2008.


% Gathering of various convenient facilities regarding files.
%
% See file_utils_test.erl for the corresponding test.
%
-module(file_utils).


% Related standard modules: file, filename.



% Filename-related operations.
%
-export([ join/1, join/2, convert_to_filename/1, replace_extension/3,

		  exists/1, get_type_of/1, is_file/1,
		  is_existing_file/1, is_existing_file_or_link/1,
		  is_executable/1, is_directory/1, is_existing_directory/1,
		  is_existing_directory_or_link/1,
		  list_dir_elements/1,

		  get_size/1, get_last_modification_time/1, touch/1,
		  create_empty_file/1,

		  get_current_directory/0, set_current_directory/1,

		  filter_by_extension/2, filter_by_extensions/2,
		  filter_by_included_suffixes/2, filter_by_excluded_suffixes/2,

		  find_files_from/1, find_files_with_extension_from/2,
		  find_files_with_excluded_dirs/2,
		  find_files_with_excluded_suffixes/2,
		  find_files_with_excluded_dirs_and_suffixes/3,
		  find_directories_from/1,

		  create_directory/1, create_directory/2,
		  create_directory_if_not_existing/1,
		  create_temporary_directory/0,

		  remove_file/1, remove_file_if_existing/1,
		  remove_files/1, remove_files_if_existing/1,

		  remove_directory/1,

		  copy_file/2, copy_file_if_existing/2, copy_file_in/2,

		  rename/2, move_file/2, change_permissions/2,

		  is_absolute_path/1,
		  ensure_path_is_absolute/1, ensure_path_is_absolute/2,
		  normalise_path/1,

		  is_leaf_among/2,

		  path_to_variable_name/1, path_to_variable_name/2,

		  remove_upper_levels_and_extension/1,

		  get_image_extensions/0, get_image_file_png/1, get_image_file_gif/1 ]).



% I/O section.
%
-export([ open/2, open/3, close/1, close/2, read/2, write/2, write/3,
		  read_whole/1, write_whole/2, read_terms/1 ]).


% Compression-related operations.
%
-export([ get_extension_for/1, compress/2, decompress/2,
		  file_to_zipped_term/1, zipped_term_to_unzipped_file/1,
		  zipped_term_to_unzipped_file/2,
		  files_to_zipped_term/1, files_to_zipped_term/2,
		  zipped_term_to_unzipped_files/1, zipped_term_to_unzipped_files/2 ]).



% For the file_info record:
-include_lib("kernel/include/file.hrl").


% Type declarations:

% A path may designate either a file or a directory (in both case with leading,
% root directories possibly specified).
%
-type path() :: string().
-type bin_path() :: binary().

-type file_name() :: path().

% Just a convenience alias:
-type filename() :: file_name().

-type bin_file_name() :: binary().

-type directory_name() :: path().
-type bin_directory_name() :: binary().

-type extension() :: string().


% A leaf name, i.e. the final part of a path (possibly a file or directory).
%
% Ex: in 'aaa/bbb/ccc', 'aaa' is the root, and 'ccc' is the leaf.
%
-type leaf_name() :: string().



% All known types of file entries:
-type entry_type() :: 'device' | 'directory' | 'other' | 'regular' | 'symlink'.


% Relevant flags when opening a file:
%
% (file:mode() not exported currently unfortunately, see
% lib/kernel/src/file.erl)
%
%-type file_open_mode() :: file:mode() | 'ram'.
-type file_open_mode() :: tuple() | atom() | 'ram'.


% The supported compression formats:
-type compression_format() :: 'zip' | 'bzip2' | 'xz'.


% Corresponds to the handle to an open file (typically a file descriptor
% counterpart):
%
-type file() :: file:io_device().


% The various permissions that can be combined for file-like elements:
%
-type permission() :: 'owner_read' | 'owner_write' | 'owner_execute'
					  | 'group_read' | 'group_write' | 'group_execute'
					  | 'other_read' | 'other_write' | 'other_execute'
					  | 'set_user_id' | 'set_group_id'.


-export_type([ path/0, bin_path/0,
			   file_name/0, filename/0, bin_file_name/0,
			   directory_name/0, bin_directory_name/0,
			   extension/0,
			   entry_type/0,
			   permission/0,
			   compression_format/0,
			   file/0
			 ]).



% Filename-related operations.


% Joins the specified list of path elements.
%
% Note: join/1 added back to file_utils, filename:join( Components ) can be used
% instead. However filename:join( [ "", "my_dir" ] ) results in "/my_dir",
% whereas often we would want "my_dir", which is returned by file_utils:join/1.
%
-spec join( [ path() ] ) -> path().
join( _ComponentList=[ "" | T ] ) ->
	filename:join( T );

join( ComponentList ) ->
	filename:join( ComponentList ).



% Joins the two specified path elements.
%
% Note: join/2 added back to file_utils, filename:join( Name1, Name2 ) can be
% used instead. However filename:join( "", "my_dir" ) results in "/my_dir",
% whereas often we would want "my_dir", which is returned by file_utils:join/2.
%
-spec join( path(), path() ) -> path().
join( _FirstPath="", SecondPath ) ->
	SecondPath ;

join( FirstPath, SecondPath ) ->
	filename:join( FirstPath, SecondPath ).





% Converts specified name to an acceptable filename, filesystem-wise.
%
-spec convert_to_filename( string() ) ->
		file_name(). % none() in case of erlang:error/2
convert_to_filename( Name ) ->

	% Currently we use exactly the same translation rules both for node names
	% and file names (see net_utils:generate_valid_node_name_from/1).

	% Note however that now we duplicate the code instead of calling the
	% net_utils module from here, as otherwise there would be one more module
	% to deploy under some circumstances.

	% Replaces each series of spaces (' '), lower than ('<'), greater than
	% ('>'), comma (','), left ('(') and right (')') parentheses, single (''')
	% and double ('"') quotes, forward ('/') and backward ('\') slashes,
	% ampersand ('&'), tilde ('~'), sharp ('#'), at sign ('@'), all other kinds
	% of brackets ('{', '}', '[', ']'), pipe ('|'), dollar ('$'), star ('*'),
	% marks ('?' and '!'), plus ('+'), other punctation signs (';' and ':') by
	% exactly one underscore:
	%
	% (see also: net_utils:generate_valid_node_name_from/1)
	%
	re:replace( lists:flatten(Name),
			   "( |<|>|,|\\(|\\)|'|\"|/|\\\\|\&|~|"
			   "#|@|{|}|\\[|\\]|\\||\\$|\\*|\\?|!|\\+|;|:)+", "_",
		 [ global, { return, list } ] ).



% Returns a new filename whose extension has been updated.
%
% Ex: replace_extension( "/home/jack/rosie.ttf", ".ttf", ".wav" ) should return
% "/home/jack/rosie.wav".
%
-spec replace_extension( file_name(), extension(), extension() ) -> file_name().
replace_extension( Filename, SourceExtension, TargetExtension ) ->

	case string:rstr( Filename, SourceExtension ) of

		0 ->
			throw( { extension_not_found, SourceExtension, Filename } );

		Index ->
			string:substr( Filename, 1, Index-1 ) ++ TargetExtension

	end.



% Tells whether specified file entry exists, regardless of its type.
%
-spec exists( file_name() ) -> boolean().
exists( EntryName ) ->

	case file:read_file_info( EntryName ) of

		{ ok, _FileInfo } ->
			true;

		{ error, _Reason } ->
			false

	end.



% Returns the type of the specified file entry.
%
-spec get_type_of( file_name() ) -> entry_type().
get_type_of( EntryName ) ->

	% We used to rely on file:read_file_info/1, but an existing symlink pointing
	% to a non-existing entry was triggering the enoent error, while we just
	% wanted to know that the specified entry is an existing (yet dead) symlink.

	% Some tools (e.g. emacs) used thus to get in the way, as apparently they
	% create dead symlinks on purpose, to store information.

	case file:read_link_info( EntryName ) of

		{ ok, FileInfo } ->
			#file_info{ type=FileType } = FileInfo,
			FileType;

		{ error, eloop } ->
			% Probably a recursive symlink:
			throw( { too_many_symlink_levels, EntryName } );

		{ error, enoent } ->
			throw( { non_existing_entry, EntryName } )

	end.



% Returns whether the specified entry, supposedly existing, is a regular file.
%
% If the specified entry happens not to exist, a
% '{ non_existing_entry, EntryName }' exception will be thrown.
%
-spec is_file( file_name() ) -> boolean().
is_file( EntryName ) ->

	case get_type_of( EntryName ) of

		regular ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is a regular file.
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_existing_file( file_name() ) -> boolean().
is_existing_file( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		regular ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is either a regular file or a
% symbolic link.
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_existing_file_or_link( file_name() ) -> boolean().
is_existing_file_or_link( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		regular ->
			true ;

		symlink ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is executable for its current
% owner (can be either a regular file or a symbolic link).
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_executable( file_name() ) -> boolean().
is_executable( ExecutableName ) ->

	case file:read_file_info( ExecutableName ) of

		{ ok, FileInfo } ->

			#file_info{ type=FileType, mode=Mode } = FileInfo,

			case FileType of

				regular ->

					OwnerExecMask = 8#00100,
					case Mode band OwnerExecMask of

						0 ->
							% Not executable:
							false;

						_ ->
							% One positive case:
							true

					end;

				_ ->

					false

			end;

		_ ->
			false

	end.



% Returns whether the specified entry, supposedly existing, is a directory.
%
% If the specified entry happens not to exist, a
% '{ non_existing_entry, EntryName }' exception will be thrown.
%
-spec is_directory( directory_name() ) -> boolean().
is_directory( EntryName ) ->

	case get_type_of( EntryName ) of

		directory ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is a directory.
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_existing_directory( directory_name() ) -> boolean().
is_existing_directory( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		directory ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is a directory or a symbolic
% link.
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_existing_directory_or_link( directory_name() ) -> boolean().
is_existing_directory_or_link( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		directory ->
			true ;

		symlink ->
			true ;

		_ ->
			false

	end.



% Returns a tuple made of a four lists describing the file elements found in
% specified directory: { Files, Directories, OtherFiles, Devices }.
%
% Note that Files include symbolic links (dead or not).
%
-spec list_dir_elements( directory_name() ) ->
	{ [ file_name() ], [ directory_name() ], [ file_name() ], [ file_name() ] }.
list_dir_elements( Dirname ) ->

	%io:format( "list_dir_elements for '~s'.~n", [ Dirname ] ),

	{ ok, LocalDirElements } = file:list_dir( Dirname ),

	classify_dir_elements( Dirname, LocalDirElements, _Devices=[],
			_Directories=[], _Files=[], _OtherFiles=[] ).



-spec get_size( file_name() ) -> system_utils:byte_size().
get_size( Filename ) ->

	case file:read_file_info( Filename ) of

		{ ok, #file_info{ size=Size } } ->
			Size;

		{ error, Reason } ->
			throw( { file_info_failure, Reason, Filename } )

	end.



% Returns the last time at which the content of specified file was modified (not
% counting attribute or permission changes), according to the filesystem.
%
% Said time will be expressed as an integer number of seconds since (or before)
% Unix time epoch, which is 1970-01-01 00:00 UTC.
%
-spec get_last_modification_time( file_name() ) -> time_utils:posix_seconds().
get_last_modification_time( Filename ) ->

	case file:read_file_info( Filename, [ { time, posix } ] ) of

		{ ok, #file_info{ mtime=Seconds } } ->
			Seconds;

		{ error, Reason } ->
			throw( { file_info_failure, Reason, Filename } )

	end.



% Updates the modification time (the last time at which its content was reported
% as modified according to the filesystem) of the specified file, which must
% already exist.
%
% Note: leaves last access time unchanged, updates both modification and change
% times.
%
% See also: create_empty_file/1
%
-spec touch( file_name() ) -> basic_utils:void().
touch( Filename ) ->

	case is_existing_file( Filename ) of

		true ->
			% -c: do not create any file
			% -m: change only the modification time
			%
			case system_utils:run_executable( "/bin/touch -c -m '"
												  ++ Filename ++ "'" ) of

				{ 0, _Output } ->
					ok;

				{ ErrorCode, Output } ->
					throw( { touch_failed, Output, ErrorCode, Filename } )

			end;

		false ->
			throw( { non_existing_file_to_touch, Filename } )

	end.



% Creates an empty file bearing the specified filename (other use of touch).
%
% Potentially useful as a last-resort debugging tool (when no console output or
% applicative trace can be relied upon, we can at least leave side-effects on
% the filesystem).
%
% Note: of course a simple 'os:cmd( "/bin/touch ~/my-message.debug" ).' may be
% of use as well.
%
% See also: touch/1.
%
-spec create_empty_file( file_name() ) -> basic_utils:void().
create_empty_file( Filename ) ->

	case system_utils:run_executable( "/bin/touch '" ++ Filename ++ "'" ) of

		{ 0, _Output } ->
			ok;

		{ ErrorCode, Output } ->
			throw( { empty_file_creation_failed, Output, ErrorCode, Filename } )

	end.



% Returns the current directory, as a plain string.
%
% Throws an exception on failure.
%
-spec get_current_directory() -> directory_name().
get_current_directory() ->

	case file:get_cwd() of

		{ ok, Dir } ->
			Dir;

		{ error, Reason } ->
			throw( { failed_to_determine_current_directory, Reason } )

	end.



% Sets the specified directory as current directory.
%
% Throws an exception on failure.
%
-spec set_current_directory( directory_name() ) -> basic_utils:void().
set_current_directory( DirName ) ->

	 % For more detail of { 'error', atom() }, refer to type specifications of
	 % erlang files: file.erl and file.hrl.

	case file:set_cwd( DirName ) of

		ok ->
			ok;

		{ error, Error } ->
			throw( { set_current_directory_failed, DirName, Error } )

	end.



% Helper function.
%
% Returns a tuple containing four lists corresponding to the sorting of all
% file elements: { Directories, Files, Devices, OtherFiles }.
%
% Note that Files include symbolic links (dead or not).
%
classify_dir_elements( _Dirname, _Elements=[], Devices, Directories, Files,
					  OtherFiles ) ->
	% Note the reordering:
	{ Files, Directories, OtherFiles, Devices };

classify_dir_elements( Dirname, _Elements=[ H | T ],
		Devices, Directories, Files, OtherFiles ) ->

	 case get_type_of( filename:join( Dirname, H ) ) of

		device ->
			classify_dir_elements( Dirname, T, [ H | Devices ], Directories,
								   Files, OtherFiles ) ;

		directory ->
			classify_dir_elements( Dirname, T, Devices, [ H | Directories ],
								   Files, OtherFiles ) ;

		regular ->
			classify_dir_elements( Dirname, T, Devices, Directories,
								  [ H | Files ], OtherFiles ) ;

		% Managed as regular files:
		symlink ->
			classify_dir_elements( Dirname, T, Devices, Directories,
								   [ H | Files ], OtherFiles ) ;

		other ->
			classify_dir_elements( Dirname, T, Devices, Directories,
								   Files, [ H | OtherFiles ] )

	end.




% Regarding extensions: we could canonicalise their case, so that ".png" and
% ".PNG" are treated the same.


% Returns a list containing all elements of Filenames list whose extension is
% the specified one (ex: ".dat").
%
-spec filter_by_extension( [ file_name() ], extension() ) -> [ file_name() ].
filter_by_extension( Filenames, Extension ) ->
	filter_by_extension( Filenames, Extension, _Acc=[] ).


filter_by_extension( _Filenames=[], _Extension, Acc ) ->
	Acc ;

filter_by_extension( _Filenames=[ H | T ], Extension, Acc ) ->

	case filename:extension( H ) of

		Extension ->
			filter_by_extension( T, Extension, [ H | Acc ] ) ;

		_Other ->
			filter_by_extension( T, Extension, Acc )

	end.



% Returns a list containing all elements of Filenames list whose extension
% corresponds to one of the specified extensions (ex: [ ".dat", ".png" ]).
%
-spec filter_by_extensions( [ file_name() ], [ extension() ] ) ->
								 [ file_name() ].
filter_by_extensions( Filenames, Extensions ) ->
	filter_by_extensions( Filenames, Extensions, _Acc=[] ).


filter_by_extensions( _Filenames=[], _Extensions, Acc ) ->
	Acc ;

filter_by_extensions( _Filenames=[ F | T ], Extensions, Acc ) ->

	case lists:member( filename:extension( F ), Extensions ) of

		true ->
			filter_by_extensions( T, Extensions, [ F | Acc ] ) ;

		false ->
			filter_by_extensions( T, Extensions, Acc )

	end.



% Returns a list containing all elements of the Filenames list which match any
% of the specified suffixes.
%
-spec filter_by_included_suffixes( [ file_name() ], [ string() ] ) ->
										 [ file_name() ].
filter_by_included_suffixes( Filenames, IncludedSuffixes ) ->
	[ F || F <- Filenames, has_matching_suffix( F, IncludedSuffixes ) ].


% Returns a list containing all elements of the Filenames list which do not
% match any of the specified suffixes.
%
-spec filter_by_excluded_suffixes( [ file_name() ], [ string() ] ) ->
										 [ file_name() ].
filter_by_excluded_suffixes( Filenames, ExcludedSuffixes ) ->
	[ F || F <- Filenames, not has_matching_suffix( F, ExcludedSuffixes ) ].




-spec has_matching_suffix( file_name(), [ string() ] ) -> boolean().
has_matching_suffix( _Filename, _ExcludedSuffixes=[] ) ->
	false;

has_matching_suffix( Filename, [ S | OtherS ] ) ->

	% We have to avoid feeding string:substr/2 with a start position that is not
	% strictly positive, otherwise we would trigger a function clause error:

	LenFile = length( Filename ),
	LenSuffix = length( S ),

	case LenFile - LenSuffix + 1 of

		StartPos when StartPos > 0 ->

			case string:substr( Filename, StartPos ) of

				S ->
					true;

				_ ->
					has_matching_suffix( Filename, OtherS )

			end;

		_ ->
			has_matching_suffix( Filename, OtherS )

	end.




% Section dedicated to the look-up of files, with various variations (with or
% without extensions, with or without excluded directories, etc.)



% Returns the list of all regular files found from the root, in the whole
% subtree (i.e. recursively).
%
% All extensions and suffixes accepted, no excluded directories.
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_files_from( directory_name() ) -> [ file_name() ].
find_files_from( RootDir ) ->
	find_files_from( RootDir, _CurrentRelativeDir="", _Acc=[] ).



% Helper.
find_files_from( RootDir, CurrentRelativeDir, Acc ) ->

	%io:format( "find_files_from with root = '~s', current = '~s'.~n",
	%	[ RootDir, CurrentRelativeDir ] ),

	{ RegularFiles, Directories, _OtherFiles, _Devices } = list_dir_elements(
		join( RootDir, CurrentRelativeDir ) ),

	Acc ++ list_files_in_subdirs( Directories, RootDir, CurrentRelativeDir, [] )
		++ prefix_files_with( CurrentRelativeDir, RegularFiles ).


% Specific helper for find_files_from/3 above:
list_files_in_subdirs( _Dirs=[], _RootDir, _CurrentRelativeDir, Acc ) ->
	Acc;

list_files_in_subdirs( _Dirs=[ H | T ], RootDir, CurrentRelativeDir, Acc ) ->

	%io:format( "list_files_in_subdirs with root = '~s', current = '~s' "
	%	"and H='~s'.~n", [ RootDir, CurrentRelativeDir, H ] ),

	list_files_in_subdirs( T, RootDir, CurrentRelativeDir,
		find_files_from( RootDir, join( CurrentRelativeDir, H ), [] ) ++ Acc ).



% Returns the list of all regular files found from the root with specified
% extension, in the whole subtree (i.e. recursively).
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_files_with_extension_from( directory_name(), extension() )
		-> [ file_name() ].
find_files_with_extension_from( RootDir, Extension ) ->
	find_files_with_extension_from( RootDir, _CurrentRelativeDir="",
									Extension, _Acc=[] ).



% Helper.
find_files_with_extension_from( RootDir, CurrentRelativeDir, Extension, Acc ) ->

	%io:format( "find_files_from in ~s.~n", [ CurrentRelativeDir ] ),

	{ RegularFiles, Directories, _OtherFiles, _Devices } = list_dir_elements(
		join( RootDir, CurrentRelativeDir ) ),

	Acc ++ list_files_in_subdirs_with_extension( Directories, Extension,
										RootDir, CurrentRelativeDir, [] )
		++ prefix_files_with( CurrentRelativeDir,
			filter_by_extension( RegularFiles, Extension ) ).


% Helper for find_files_with_extension_from/4:
list_files_in_subdirs_with_extension( _Dirs=[], _Extension, _RootDir,
									  _CurrentRelativeDir, Acc) ->
	Acc;

list_files_in_subdirs_with_extension( _Dirs=[ H | T ], Extension, RootDir,
									  CurrentRelativeDir, Acc ) ->
	list_files_in_subdirs_with_extension( T, Extension, RootDir,
		CurrentRelativeDir,
		find_files_with_extension_from( RootDir, join( CurrentRelativeDir, H ),
			Extension, [] ) ++ Acc ).




% Returns the list of all regular files found from the root, in the whole
% subtree (i.e. recursively), with specified directories excluded.
%
% Note that the excluded directories can be specified as a full path (ex:
% "foo/bar/not-wanted"), for just as a final directory name (ex:
% "my-excluded-name"). In the latter case, all directories bearing that name
% (ex: "foo/bar/my-excluded-name") will be excluded as well.
%
% Thus when a directory D is specified in the excluded list, each traversed
% directory T will be compared twice to D: T will be matched against D, and
% against filename:basename(T), i.e. its final name, as well. As soon as one
% matches, T will be excluded.
%
% All extensions accepted.
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_files_with_excluded_dirs( directory_name(), [ directory_name() ] )
		-> [ file_name() ].
find_files_with_excluded_dirs( RootDir, ExcludedDirList ) ->
	find_files_with_excluded_dirs( RootDir, _CurrentRelativeDir="",
								  ExcludedDirList, _Acc=[] ).


% Helper.
find_files_with_excluded_dirs( RootDir, CurrentRelativeDir, ExcludedDirList,
							Acc ) ->

	%io:format( "find_files_with_excluded_dirs in ~s.~n",
	% [ CurrentRelativeDir ] ),

	{ RegularFiles, Directories, _OtherFiles, _Devices } = list_dir_elements(
		join( RootDir, CurrentRelativeDir ) ),

	% If for example ExcludedDirList=[ ".svn" ], we want to eliminate not only
	% ".svn" but also all "foo/bar/.svn", i.e. all directories having the same
	% (last) name:
	%
	FilteredDirectories = [ D || D <- Directories,
		not ( lists:member( join( CurrentRelativeDir, D ), ExcludedDirList )
			 or lists:member( D, ExcludedDirList ) ) ],

	Acc ++ list_files_in_subdirs_excluded_dirs( FilteredDirectories, RootDir,
								CurrentRelativeDir, ExcludedDirList, _Acc=[] )
		++ prefix_files_with( CurrentRelativeDir, RegularFiles ).


% Specific helper for find_files_with_excluded_dirs/4 above:
list_files_in_subdirs_excluded_dirs( _Dirs=[], _RootDir,
		_CurrentRelativeDir, _ExcludedDirList, Acc ) ->
	Acc;

list_files_in_subdirs_excluded_dirs( _Dirs=[ H | T ], RootDir,
		CurrentRelativeDir, ExcludedDirList, Acc ) ->

	list_files_in_subdirs_excluded_dirs( T, RootDir, CurrentRelativeDir,
		ExcludedDirList,
		find_files_with_excluded_dirs( RootDir, join( CurrentRelativeDir, H ),
			ExcludedDirList, [] ) ++ Acc ).





% Returns the list of all regular files found from the root which do not match
% any of the specified suffixes, in the whole subtree (i.e. recursively).
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_files_with_excluded_suffixes( directory_name(), [ string() ])
		-> [ file_name() ].
find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes ) ->
	find_files_with_excluded_suffixes( RootDir, _CurrentRelativeDir="",
									  ExcludedSuffixes, _Acc=[] ).



% Helper:
find_files_with_excluded_suffixes( RootDir, CurrentRelativeDir,
										ExcludedSuffixes, Acc ) ->

	%io:format( "find_files_with_excluded_suffixes in ~s.~n",
	% [ CurrentRelativeDir ] ),

	{ RegularFiles, Directories, _OtherFiles, _Devices } = list_dir_elements(
		join( RootDir, CurrentRelativeDir ) ),

	Acc ++ list_files_in_subdirs_with_excluded_suffixes( Directories,
			ExcludedSuffixes, RootDir, CurrentRelativeDir, [] )
		++ prefix_files_with( CurrentRelativeDir,
			filter_by_excluded_suffixes( RegularFiles, ExcludedSuffixes ) ).




% Helper for find_files_with_excluded_suffixes/4:
-spec list_files_in_subdirs_with_excluded_suffixes( list(), [ string() ],
		directory_name(), directory_name(), [ file_name() ]) -> [ file_name() ].
list_files_in_subdirs_with_excluded_suffixes( [], _ExcludedSuffixes, _RootDir,
									_CurrentRelativeDir, Acc ) ->
	Acc;

list_files_in_subdirs_with_excluded_suffixes( [ H | T ], ExcludedSuffixes,
								  RootDir, CurrentRelativeDir, Acc ) ->
	list_files_in_subdirs_with_excluded_suffixes( T, ExcludedSuffixes, RootDir,
		CurrentRelativeDir,
		find_files_with_excluded_suffixes( RootDir,
			 join( CurrentRelativeDir, H ), ExcludedSuffixes, [] ) ++ Acc ).






% Returns the list of all regular files found from the root with specified
% suffix, in the whole subtree (i.e. recursively), with specified directories
% excluded.
%
% Note that the excluded directories can be specified as a full path (ex:
% "foo/bar/not-wanted"), for just as a final directory name (ex:
% "my-excluded-name"). In the latter case, all directories bearing that name
% (ex: "foo/bar/my-excluded-name") will be excluded as well.
%
% Thus when a directory D is specified in the excluded list, each traversed
% directory T will be compared twice to D: T will be matched against D, and
% against filename:basename(T), i.e. its final name, as well. As soon as one
% matches, T will be excluded.
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_files_with_excluded_dirs_and_suffixes( directory_name(),
		[ directory_name() ], [ string() ] ) -> [ file_name() ].
find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirList,
										   ExcludedSuffixes ) ->

	%{ ok, CurrentDir } = file:get_cwd(),
	%io:format( "find_files_with_excluded_dirs_and_suffixes: current is ~s, "
	%		  "root is ~s.~n", [ CurrentDir, RootDir ] ),

	find_files_with_excluded_dirs_and_suffixes( RootDir,
			_CurrentRelativeDir="", ExcludedDirList, ExcludedSuffixes, _Acc=[]
											   ).


% Helper:
find_files_with_excluded_dirs_and_suffixes( RootDir, CurrentRelativeDir,
		ExcludedDirList, ExcludedSuffixes, Acc ) ->

	%io:format( "find_files_with_excluded_dirs_and_suffixes in ~s / ~s.~n",
	% [ RootDir, CurrentRelativeDir ] ),

	{ RegularFiles, Directories, _OtherFiles, _Devices } = list_dir_elements(
		join( RootDir, CurrentRelativeDir ) ),

	% If for example ExcludedDirList=[ ".svn" ], we want to eliminate not only
	% ".svn" but also all "foo/bar/.svn", i.e. all directories having the same
	% (last) name:
	%
	FilteredDirectories = [ D || D <- Directories,
		not ( lists:member( join( CurrentRelativeDir, D ), ExcludedDirList )
			 or lists:member( D, ExcludedDirList ) ) ],

	Acc ++ list_files_in_subdirs_excluded_dirs_and_suffixes(
			FilteredDirectories, RootDir, CurrentRelativeDir,
			ExcludedDirList, ExcludedSuffixes, _Acc=[] )
		++ prefix_files_with( CurrentRelativeDir,
			filter_by_excluded_suffixes( RegularFiles, ExcludedSuffixes ) ).




% Specific helper for find_files_with_excluded_dirs_and_suffixes/5 above:
list_files_in_subdirs_excluded_dirs_and_suffixes( _Dirs=[], _RootDir,
		_CurrentRelativeDir, _ExcludedDirList, _ExcludedSuffixes, Acc ) ->
	Acc;

list_files_in_subdirs_excluded_dirs_and_suffixes( _Dirs=[ H | T ], RootDir,
		CurrentRelativeDir, ExcludedDirList, ExcludedSuffixes, Acc ) ->
	list_files_in_subdirs_excluded_dirs_and_suffixes( T, RootDir,
		CurrentRelativeDir, ExcludedDirList, ExcludedSuffixes,
		find_files_with_excluded_dirs_and_suffixes( RootDir,
			join(CurrentRelativeDir,H), ExcludedDirList, ExcludedSuffixes, [] )
		++ Acc ).




-spec prefix_files_with( directory_name(), [ file_name() ] ) -> [ file_name() ].
prefix_files_with( RootDir, Files ) ->
	%io:format( "Prefixing ~p with '~s'.~n", [ Files, RootDir ] ),
	prefix_files_with( RootDir, Files, _Acc=[] ).


% Helper:
prefix_files_with( _RootDir, _Files=[], Acc ) ->
	Acc;

prefix_files_with( RootDir, [ H| T ], Acc ) ->
	prefix_files_with( RootDir, T, [ join( RootDir, H ) | Acc ] ).




% Returns the list of all directories found from the root, in the whole subtree
% (i.e. recursively).
%
% All returned pathnames are relative to this root.
% Ex: [ "./my-dir", "./tmp/other-dir" ].
%
-spec find_directories_from( directory_name() ) -> [ directory_name() ].
find_directories_from( RootDir ) ->
	find_directories_from( RootDir, "", _Acc=[] ).


% Helper:
find_directories_from( RootDir, CurrentRelativeDir, Acc ) ->

	%io:format( "find_directories_from in ~s.~n", [ CurrentRelativeDir ] ),

	{ _RegularFiles, Directories, _OtherFiles, _Devices } = list_dir_elements(
		join( RootDir, CurrentRelativeDir ) ),

	Acc ++ list_directories_in_subdirs( Directories,
			RootDir, CurrentRelativeDir, _Acc=[] )
		++ prefix_files_with( CurrentRelativeDir, Directories ).



% Helper:
list_directories_in_subdirs( _Dirs=[], _RootDir, _CurrentRelativeDir, Acc ) ->
	Acc;

list_directories_in_subdirs( _Dirs=[ H | T ], RootDir, CurrentRelativeDir,
							 Acc ) ->
	list_directories_in_subdirs( T, RootDir, CurrentRelativeDir,
		find_directories_from( RootDir, join( CurrentRelativeDir, H ), _Acc=[] )
		++ Acc ).



% Creates specified directory, without creating any intermediate (parent)
% directory that would not exist.
%
% Throws an exception if the operation failed.
%
-spec create_directory( directory_name() ) -> basic_utils:void().
create_directory( Dirname ) ->
	create_directory( Dirname, create_no_parent ).



% Creates the specified directory.
%
% If 'create_no_parent' is specified, no intermediate (parent) directory will be
% created.
%
% If 'create_parents' is specified, any non-existing intermediate (parent)
% directory will be created.
%
% Throws an exception if the operation fails, for example if the directory is
% already existing ( { create_directory_failed, "foobar", eexist } ).
%
-spec create_directory( directory_name(),
	   'create_no_parent' | 'create_parents' ) -> basic_utils:void().
create_directory( Dirname, create_no_parent ) ->

	case file:make_dir( Dirname ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { create_directory_failed, Dirname, Reason } )

	end;

create_directory( Dirname, create_parents ) ->
	create_dir_elem( filename:split( Dirname ), "" ).



% Creates specified directory (but not any parents), if not already existing.
%
% Throws an exception if the operation fails.
%
-spec create_directory_if_not_existing( directory_name() ) ->
											  basic_utils:void().
create_directory_if_not_existing( Dirname ) ->

	case is_existing_directory( Dirname ) of

		true ->
			ok;

		false ->
			create_directory( Dirname )

	end.



% Helper:
create_dir_elem( _Elems=[], _Prefix ) ->
	ok;

create_dir_elem( _Elems=[ H | T ], Prefix ) ->

	NewPrefix = join( Prefix, H ),

	case exists( NewPrefix ) of

		true ->
			ok ;

		false ->
			create_directory( NewPrefix, create_no_parent )

	end,
	create_dir_elem( T, NewPrefix ).



% Creates a non previously existing temporary directory, and returs its full
% path.
%
-spec create_temporary_directory() -> directory_name().
create_temporary_directory() ->

	TmpDir = join( [ "/tmp", system_utils:get_user_name(),
					 basic_utils:generate_uuid() ] ),

	case exists( TmpDir ) of

		true ->
			% Very bad luck apparently, or same random root:
			create_temporary_directory();

		false ->
			create_directory( TmpDir, create_parents ),
			TmpDir

	end.



% Removes specified file, specified as a plain string.
%
% Throws an exception if any problem occurs.
%
-spec remove_file( file_name() ) -> basic_utils:void().
remove_file( Filename ) ->

	%io:format( "## Removing file '~s'.~n", [ Filename ] ),

	case file:delete( Filename ) of

		ok ->
			ok;

		Error ->
			throw( { remove_file_failed, Filename, Error } )

	end.



% Removes specified files, specified as a list of plain strings.
%
-spec remove_files( [ file_name() ] ) -> basic_utils:void().
remove_files( FilenameList ) ->
	[ remove_file( Filename ) || Filename <- FilenameList ].



% Removes specified file, specified as a plain string, iff it is already
% existing, otherwise does nothing.
%
-spec remove_file_if_existing( file_name() ) -> basic_utils:void().
remove_file_if_existing( Filename ) ->

	case is_existing_file( Filename ) of

		true ->
			remove_file( Filename );

		false ->
			ok

	end.



% Removes each specified file, in specified list of plain strings, iff it is
% already existing.
%
-spec remove_files_if_existing( [ file_name() ] ) -> basic_utils:void().
remove_files_if_existing( FilenameList ) ->
	[ remove_file_if_existing( Filename ) || Filename <- FilenameList ].



% Removes specified directory, which must be empty.
%
-spec remove_directory( directory_name() ) -> basic_utils:void().
remove_directory( DirectoryName ) ->

	%io:format( "## Removing directory '~s'.~n", [ DirectoryName ] ),

	case file:del_dir( DirectoryName ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { remove_directory_failed, Reason, DirectoryName } )

	end.



% Copies a specified file to a given destination filename (not a directory name,
% see copy_file_in/2 for that), overwriting any previous file.
%
% Note: content is copied and permissions are preserved (ex: the copy of an
% executable file will be itself executable, other permissions as well, unlike
% /bin/cp which relies on umask).
%
-spec copy_file( file_name(), file_name() ) -> basic_utils:void().
copy_file( SourceFilename, DestinationFilename ) ->

	% First, checks the source file exists and retrieves its meta-information:
	case file:read_file_info( SourceFilename ) of

		{ ok, #file_info{ mode=Mode } } ->

			case file:copy( SourceFilename, DestinationFilename ) of

				{ ok, _ByteCount } ->
					% Now sets the permissions of the copy:
					ok = file:change_mode( DestinationFilename, Mode );

				Error ->
					throw( { copy_file_failed, SourceFilename,
							 DestinationFilename, Error } )

			end;

		{ error, Reason } ->
			throw( { copy_file_failed, SourceFilename, Reason } )

	end.



% Copies a specified file in a given destination directory, overwriting any
% previous file, and returning the full path of the copied file.
%
% Note: content is copied and permissions are preserved (ex: the copy of an
% executable file will be itself executable, other permissions as well, unlike
% /bin/cp which relies on umask).
%
-spec copy_file_in( file_name(), directory_name() ) -> file_name().
copy_file_in( SourceFilename, DestinationDirectory ) ->

	Filename = filename:basename( SourceFilename ),

	TargetPath = join( DestinationDirectory, Filename ),

	copy_file( SourceFilename, TargetPath ),

	TargetPath.



% Copies a specified file to a given destination iff it is already existing.
%
% Note: content is copied and permissions are preserved (ex: the copy of an
% executable file will be itself executable).
%
-spec copy_file_if_existing( file_name(), file_name() ) -> basic_utils:void().
copy_file_if_existing( SourceFilename, DestinationFilename ) ->

	case is_existing_file( SourceFilename ) of

		true ->
			copy_file( SourceFilename, DestinationFilename );

		false ->
			ok

	end.



% Renames specified file.
%
-spec rename( file_name(), file_name() ) -> basic_utils:void().
rename( SourceFilename, DestinationFilename ) ->
	move_file( SourceFilename, DestinationFilename ).



% Moves specified file so that it is now designated by specified filename.
%
-spec move_file( file_name(), file_name() ) -> basic_utils:void().
move_file( SourceFilename, DestinationFilename ) ->

	%io:format( "## Moving file '~s' to '~s'.~n",
	%		   [ SourceFilename, DestinationFilename ] ),

	%copy_file( SourceFilename, DestinationFilename ),
	%remove_file( SourceFilename ).

	% Simpler, better:
	case file:rename( SourceFilename, DestinationFilename ) of

		ok ->
			ok;

		Error ->
			throw( { move_file_failed, Error,  SourceFilename,
					 DestinationFilename } )

	end.



% Returns the low-level permission associated to specified one.
%
-spec get_permission_for( permission() | [ permission() ] ) -> integer().
get_permission_for( owner_read ) ->
	8#00400;

get_permission_for( owner_write ) ->
	8#00200;

get_permission_for( owner_execute ) ->
	8#00100;

get_permission_for( group_read ) ->
	8#00040;

get_permission_for( group_write ) ->
	8#00020;

get_permission_for( group_execute ) ->
	8#00010;

get_permission_for( other_read ) ->
	8#00004;

get_permission_for( other_write ) ->
	8#00002;

get_permission_for( other_execute ) ->
	8#00001;

get_permission_for( set_user_id ) ->
	16#800;

get_permission_for( set_group_id ) ->
	16#400;

get_permission_for( PermissionList ) when is_list( PermissionList ) ->
	lists:foldl( fun( P, Acc ) ->
						 get_permission_for( P ) + Acc
				 end,
				 _Acc0=0,
				 PermissionList ).



% Changes the permissions of specified file.
%
-spec change_permissions( file_name(), permission() | [ permission() ] ) ->
								basic_utils:void().
change_permissions( Filename, NewPermissions ) ->

	ActualPerms = get_permission_for( NewPermissions ),

	case file:change_mode( Filename, ActualPerms ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { change_permission_failed, Reason, Filename,
					 NewPermissions } )

	end.



% Tells whether the specified path is an absolute one.
%
% A path is deemed absolute iff it starts with "/".
%
-spec is_absolute_path( path() ) -> boolean().
is_absolute_path( _Path=[ $/ | _Rest ] ) ->
	true;

is_absolute_path( _Path ) ->
	false.



% Returns an absolute, normalised path corresponding to specified path.
%
% If it is not already absolute, it will made so by using the current working
% directory.
%
-spec ensure_path_is_absolute( path() ) -> path().
ensure_path_is_absolute( Path ) ->

	AbsPath = case is_absolute_path( Path ) of

		true ->
			% Already absolute:
			Path;

		false ->
			% Relative, using current directory as base:
			join( get_current_directory(), Path )

	end,

	normalise_path( AbsPath ).



% Returns an absolute path corresponding to specified path, using base path as
% root directory (this must be an absolute path).
%
% Ex: ensure_path_is_absolute( "tmp/foo", "/home/dalton" ) will return
% "/home/dalton/tmp/foo".
%
-spec ensure_path_is_absolute( path(), path() ) -> path().
ensure_path_is_absolute( TargetPath, BasePath ) ->

	case is_absolute_path( TargetPath ) of

		true ->
			% Already absolute:
			TargetPath;

		false ->
			% Relative, using specified base directory:
			case is_absolute_path( BasePath ) of

				true ->
					join( BasePath, TargetPath );

				false ->
					throw( { base_path_not_absolute, BasePath } )
			end

	end.



% Normalises specified path (canonicalise it), by translating it so that no
% superfluous '.' or '..' is present afterwards.
%
% For example, "/home/garfield/../lisa/./src/.././tube" shall be normalised in
% "/home/lisa/tube".
%
-spec normalise_path( path() ) -> path().
normalise_path( _Path="." ) ->
	".";
	%get_current_directory();

normalise_path( Path ) ->

	%io:format( "Normalising path '~p'.~n", [ Path ] ),

	ElemList = filename:split( Path ),

	join( filter_elems( ElemList, _Acc=[] ) ).


filter_elems( _ElemList=[], Acc ) ->
	lists:reverse( Acc );

filter_elems( _ElemList=[ "." | T ], Acc ) ->
	filter_elems( T, Acc );

filter_elems( _ElemList=[ ".." | T ], Acc ) ->
	filter_elems( T, tl( Acc ) );

filter_elems( _ElemList=[ E | T ], Acc ) ->
	filter_elems( T, [ E | Acc ] ).



% The approach below would not work with, for example, "X/Y/Z/../../A":

%	RevElemList = lists:reverse( filename:split( Path ) ),

%	% Returns in the right order:
%	join( filter_elems( RevElemList, _Acc=[] ) ).


% filter_elems( _Elems=[], Acc ) ->
%	Acc;

% filter_elems( _Elems=[ "." | T ], Acc ) ->
%	filter_elems( T, Acc );

% filter_elems( _Elems=[ "..", _E | T ], Acc ) ->
%	filter_elems( T, Acc );

% filter_elems( _Elems=[ E | T ], Acc ) ->
%	filter_elems( T, [ E | Acc ] ).



% Tells whether specified basename (ex: a pathless filename) is among the
% specified list of full paths; returns either false or the first full path
% found corresponding to that leaf element.
%
% Ex:
%  false = file_utils:is_leaf_among( "xx", [ "a/b/c/yy", "d/e/zz" ] )
%  "a/b/c/xx"  = file_utils:is_leaf_among( "xx", [ "a/b/c/xx", "d/e/zz" ] )
%
-spec is_leaf_among( leaf_name(), [ path() ] ) ->
						   { 'false' | path() }.
is_leaf_among( _LeafName, _PathList=[] ) ->
	false;

is_leaf_among( LeafName, _PathList=[ Path | T ] ) ->

	case filename:basename( Path ) of

		LeafName ->
			Path;

		_  ->
			is_leaf_among( LeafName, T )

	end.



% Converts specified path (full filename, like '/home/jack/test.txt' or
% './media/test.txt') into a variable name licit in most programming languages
% (ex: C/C++).
%
% Rule here is:
%  - variable name starts with a prefix, user-supplied or the default one
%  - any leading './' is removed
%  - '-' becomes '_'
%  - '.' becomes '_'
%  - '/' becomes '_'
%
-spec path_to_variable_name( path() ) -> string().
path_to_variable_name( Filename ) ->
	path_to_variable_name( Filename, "File_" ).



% Removes any leading './'.
%
-spec path_to_variable_name( path(), string() ) -> string().
path_to_variable_name( [ $.,$/ | T ], Prefix ) ->
	convert( T, Prefix );

path_to_variable_name( Filename, Prefix ) ->
	convert( Filename, Prefix ).



% Helper function:
%
convert( Filename, Prefix ) ->

	NoDashName = re:replace( lists:flatten( Filename ), "-+", "_",
		[ global, { return, list } ] ),

	NoDotName = re:replace( NoDashName, "\\.+", "_",
		[ global, { return, list } ] ),

	Prefix ++ re:replace( NoDotName, "/+", "_",
		[ global, { return, list } ] ).



% Removes all upper levels of a path (absolute or not), as well as the extension
% of the resulting file name:
%
remove_upper_levels_and_extension( FilePath ) ->

	PathLevels = filename:split( FilePath ),

	FileName = lists:last( PathLevels ),

	case string:rchr( FileName, $. ) of

		0 ->
			FileName;

		Index ->
			string:sub_string( FileName, 1, Index-1 )

	end.




% Returns a list of the known file extensions that refer image files.
%
-spec get_image_extensions() -> [ extension() ].
get_image_extensions() ->
	% TIFF, TGA and al deemed deprecated:
	[ ".png", ".jpg", ".jpeg", ".bmp" ].



-define(ResourceDir,"resources").


% Returns the image path corresponding to the specified file.
%
-spec get_image_file_png( file_name() ) -> path().
get_image_file_png( Image ) ->
  filename:join( [ ?ResourceDir, "images", Image ++ ".png" ] ).



% Returns the image path corresponding to the specified file.
%
-spec get_image_file_gif( file_name() ) -> path().
get_image_file_gif( Image ) ->
  filename:join( [ ?ResourceDir, "images", Image ++ ".gif" ] ).




% I/O section.


% Opens the file corresponding to the specified filename, with specified list of
% options (as listed for file:open/2 in
% http://erlang.org/doc/man/file.html#open-2, i.e. read, write, append,
% exclusive, raw, etc.).
%
% Returns the file reference, or throws an exception.
%
% Will attempt to open the specified file only once, as looping endlessly does
% not seem a viable solution right now (risk of exhausting the descriptors,
% making the VM fail for example when loading a new BEAM).
%
-spec open( file_name(), [ file_open_mode() ] ) -> file().
open( Filename, Options ) ->
	open( Filename, Options, _Default=try_once ).



% Opens the file corresponding to specified filename (first parameter) with
% specified list of options (second parameter; refer to file:open/2 for detailed
% documentation).
%
% Third parameter is the "attempt mode", either 'try_once', 'try_endlessly' or
% 'try_endlessly_safer', depending respectively on whether we want to try to
% open the file once (no other attempt will be made), endlessly (until a file
% descriptor can be gained), possibly with a safer setting.
%
% Returns the file reference, or throws an exception.
%
% Will try to obtain a file descriptor iteratively (and endlessly) with
% process-specific random waitings, should no descriptor be available.
%
% A risk of that approach is that all available file descriptors will be
% taken, thus potentially preventing other processes (including the VM itself)
% to perform any file operation, like loading a new BEAM, ex:
% """
% File operation error: system_limit. Target:
% lib/erlang/lib/kernel-x.y.z/ebin/timer.beam. Function: get_file.
% Process: code_server.
% """
%
% This is done in order to support situations where potentially more Erlang
% processes than available file descriptors try to access to files. An effort is
% made to desynchronize these processes to smooth the use of descriptors.
%
-spec open( file_name(), [ file_open_mode() ],
		   'try_once' | 'try_endlessly' | 'try_endlessly_safer' ) -> file().
		% For the contents in above tuple(): reference to type #file_descriptor
		% of erlang module: file.hrl
open( Filename, Options, _AttemptMode=try_endlessly_safer ) ->

	File = open( Filename, Options, try_endlessly ),

	% We could check here that at least one descriptor remains, by adding a
	% dummy file open/close and catching emfile, however one could need more
	% than one spare descriptor.
	%
	% The correct solution would involve knowing the max number of descriptors
	% for that process and the current number of open ones, no information we
	% seems able to know.
	%
	% So for the moment we do not do anything more than 'try_endlessly':
	File;


open( Filename, Options, _AttemptMode=try_endlessly ) ->

	case file:open( Filename, Options ) of

		{ ok, File } ->
			 File;

		{ error, FileError } when FileError == emfile
				orelse FileError == system_limit ->

			% File descriptors exhausted for this OS process.
			% Kludge to desynchronize file opening in order to remain below 1024
			% file descriptor opened:
			Duration = basic_utils:get_process_specific_value(
										_Min=50, _Max=200 ),

			% Attempt not to use timer:sleep (anyway will trigger errors
			% afterwards when the system will try to look-up some BEAMs):
			receive

			after Duration ->

				open( Filename, Options, try_endlessly )

			end;

		{ error, OtherFileError } ->
			throw( { open_failed, { Filename, Options }, OtherFileError } )

	end;


open( Filename, Options, _AttemptMode=try_once ) ->

	case file:open( Filename, Options ) of

		{ ok, File } ->
			 File;

		{ error, emfile } ->
			throw( { too_many_open_files, { Filename, Options } } );

		{ error, system_limit } ->
			% Never had system_limit without this cause (yet!):
			throw( { too_many_open_files, { Filename, Options },
					 system_limit } );

		{ error, OtherError } ->
			throw( { open_failed, { Filename, Options }, OtherError } )

	end.



% Closes specified file reference.
%
% Throws an exception on failure.
%
-spec close( file() ) -> basic_utils:void().
close( File ) ->
	close( File, throw_if_failed ).



% Closes specified file reference.
%
% Throws an exception on failure or not, depending on specified failure mode.
%
-spec close( file(), 'overcome_failure' | 'throw_if_failed' ) ->
				   basic_utils:void().
close( File, _FailureMode=throw_if_failed ) ->

	case file:close( File ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { file_closing_failed, Reason } )

	end;

close( File, _FailureMode=overcome_failure ) ->
	file:close( File ).



% Reads specified number of bytes/characters from the specified file.
%
% Returns either { ok, Data } if at least some data could be read, or eof if at
% least one element was to read and end of file was reached before anything at
% all could be read.
%
% Throws an exception on failure.
%
-spec read( file(), basic_utils:count() ) ->
				  { 'ok', string() | binary() } | 'eof'.
read( File, Count ) ->

	case file:read( File, Count ) of

		R={ ok, _Data } ->
			R;

		eof ->
			eof;

		{ error, Reason } ->
			throw( { read_failed, Reason } )

	end.



% Writes specified content into specified file.
%
% Throws an exception on failure.
%
-spec write( file(), iodata() ) -> basic_utils:void().
write( File, Content ) ->

	case file:write( File, Content ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { write_failed, Reason } )

	end.



% Writes specified formatted content into specified file.
%
% Throws an exception on failure.
%
-spec write( file(), text_utils:format_string(), [ term() ] ) ->
				   basic_utils:void().
write( File, FormatString, Values ) ->

	Text = io_lib:format( FormatString, Values ),

	case file:write( File, Text ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { write_failed, Reason } )

	end.



% Reads the content of the specified file, based on its filename specified as a
% plain string, and returns the corresponding binary, or throws an exception on
% failure.
%
% See also: read_terms/1 to read directly Erlang terms.
%
-spec read_whole( file_name() ) -> binary().
read_whole( Filename ) ->

	case file:read_file( Filename ) of

		{ ok, Binary } ->
			Binary;

		{ error, Error } ->
			throw( { read_whole_failed, Filename, Error } )

	end.



% Writes the specified binary in specified file, whose filename is specified as
% a plain string. Throws an exception on failure.
%
-spec write_whole( file_name(), binary() ) -> basic_utils:void().
write_whole( Filename, Binary ) ->

	case file:write_file( Filename, Binary ) of

		ok ->
			ok;

		{ error, Error } ->
			throw( { write_whole_failed, Filename, Error } )

	end.



% Reads specified file, tries to read a list of terms from it, and returns it.
%
% Throws an exception on error.
%
read_terms( Filename ) ->

	case file:consult( Filename ) of

		{ ok, Terms } ->
			Terms;

		{ error, Error } when is_atom( Error ) ->
			throw( { reading_failed, Filename, Error } );

		{ error, Error } ->
			Reason = file:format_error( Error ),
			throw( { interpretation_failed, Filename, Reason } )

	end.



% Compression-related operations.


% Returns the file extension corresponding to filenames compressed with
% specified format.
%
-spec get_extension_for( compression_format() ) -> extension().
get_extension_for( _CompressionFormat=zip ) ->
	".zip";

get_extension_for( _CompressionFormat=bzip2 ) ->
	".bz2";

get_extension_for( _CompressionFormat=xz ) ->
	".xz".



% Compresses specified file: creates a new, compressed version thereof, whose
% filename, established based on usual conventions, is returned. If a file with
% that name already exists, it will be overwritten.
%
% For example, compress( "hello.png", zip ) will generate a "hello.png.zip"
% file.
%
% The original file remain as is.
%
% Note: this function just takes care of compressing a single file, even if some
% compressors (ex: zip) include features to create an archive of multiple files
% first.
%
-spec compress( file_name(), compression_format() ) -> file_name().
compress( Filename, _CompressionFormat=zip ) ->

	% Rather than using a standalone zip tool, we use the Erlang support here:

	%ZipExec = executable_utils:get_default_zip_compress_tool(),

	ZipFilename = Filename ++ get_extension_for( zip ),

	% Exactly this one file in the archive:
	%Command = ZipExec ++ " --quiet " ++ ZipFilename ++ " " ++ Filename,

	%[] = os:cmd( Command ),

	zip:zip( ZipFilename, [ Filename ] ),

	% Check:
	true = is_existing_file( ZipFilename ),

	ZipFilename;


compress( Filename, _CompressionFormat=bzip2 ) ->

	Bzip2Exec = executable_utils:get_default_bzip2_compress_tool(),

	% --keep allows to avoid that bzip2 removes the original file:
	case system_utils:run_executable( Bzip2Exec ++ " --keep --force --quiet "
									  ++ Filename ) of

		{ _ExitCode=0, _Output=[] } ->
			% Check:
			Bzip2Filename = Filename ++ get_extension_for( bzip2 ),
			true = is_existing_file( Bzip2Filename ),
			Bzip2Filename;

		{ _ExitCode=0, Output } ->
			throw( { bzip2_compress_failed, Filename, Output } );

		{ ExitCode, Output } ->
			throw( { bzip2_compress_failed, Filename, ExitCode, Output } )

	end;


compress( Filename, _CompressionFormat=xz ) ->

	XZExec = executable_utils:get_default_xz_compress_tool(),

	% --keep allows to avoid that bzip2 removes the original file:
	case system_utils:run_executable( XZExec ++ " --keep --force --quiet "
									   ++ Filename ) of

		{ _ExitCode=0, _Output=[] } ->
			% Check:
			XZFilename = Filename ++ get_extension_for( xz ),
			true = is_existing_file( XZFilename ),
			XZFilename;

		{ _ExitCode=0, Output } ->
			throw( { xz_compress_failed, Filename, Output } );

		{ ExitCode, Output } ->
			throw( { xz_compress_failed, Filename, ExitCode, Output } )

	end;

compress( _Filename, CompressionFormat ) ->
	throw( { unsupported_compression_format, CompressionFormat } ).



% Decompresses specified compressed file, expected to bear the extension
% corresponding to the specified format: recreates the original, decompressed
% version thereof, whose filename, established based on usual conventions, is
% returned: the name of the input file without its extension.
%
% This function works in pair with compress/2, and as such expects that each
% compressed file contains exactly one file, bear the same filename except the
% compressor extension.
%
% Typically, when a format MY_FORMAT is specified, converts a compressed file
% name foo.extension_of(MY_FORMAT) into an uncompressed version of it named
% 'foo'.
%
% So, for example, decompress( "foo.xz", xz ) will generate a "foo" file.
%
% If a file with that name already exists, it will be overwritten.
%
% The compressed file remains as is.
%
-spec decompress( file_name(), compression_format() ) -> file_name().
decompress( ZipFilename, _CompressionFormat=zip ) ->

	% An annoying problem with zip is that the name of the (single) file in the
	% archive might differ from the filename deduced from the archive name (ex:
	% "foo.zi"p might contain "bar" instead of "foo"). We need to return "bar",
	% not "foo".

	% Rather than using a standalone zip tool, we use the Erlang support here:

	%UnzipExec = executable_utils:get_default_zip_decompress_tool(),

	% Checks and removes extension:
	%Filename = replace_extension( ZipFilename, get_extension_for( zip ), "" ),

	% Quiet, overwrite:
	%Command = UnzipExec ++ " -q -o " ++ ZipFilename,

	%[] = os:cmd( Command ),

	% Exactly one file per such archives:
	{ ok, [ Filename ] } = zip:unzip( ZipFilename ),

	% We expect here than only the compression feature (not the archive-making
	% feature) of zip has been used, as for all other compressors:
	%
	true = is_existing_file( Filename ),

	Filename;


decompress( Bzip2Filename, _CompressionFormat=bzip2 ) ->

	Bzip2Exec = executable_utils:get_default_bzip2_decompress_tool(),

	% Checks and removes extension:
	Filename = replace_extension( Bzip2Filename, get_extension_for( bzip2 ),
								  "" ),

	% The result will be named Filename by bunzip2:

	case system_utils:run_executable( Bzip2Exec ++ " --keep --force --quiet "
									   ++ Bzip2Filename ) of

		{ _ExitCode=0, _Output=[] } ->
			% Check:
			Bzip2Filename = Filename ++ get_extension_for( bzip2 ),
			true = is_existing_file( Filename ),
			Filename;

		{ _ExitCode=0, Output } ->
			throw( { bzip2_decompress_failed, Filename, Output } );

		{ ExitCode, Output } ->
			throw( { bzip2_decompress_failed, Filename, ExitCode, Output } )

	end;


decompress( XzFilename, _CompressionFormat=xz ) ->

	XZExec = executable_utils:get_default_xz_decompress_tool(),

	% Checks and removes extension:
	Filename = replace_extension( XzFilename, get_extension_for( xz ), "" ),

	case system_utils:run_executable( XZExec ++ " --keep --force --quiet "
									   ++ XzFilename ) of

		{ _ExitCode=0, _Output=[] } ->
			% Check:
			true = is_existing_file( Filename ),
			Filename;

		{ _ExitCode=0, Output } ->
			throw( { xz_decompress_failed, Filename, Output } );

		{ ExitCode, Output } ->
			throw( { xz_decompress_failed, Filename, ExitCode, Output } )

	end;


decompress( _Filename, CompressionFormat ) ->
	throw( { unsupported_compression_format, CompressionFormat } ).





% Reads in memory the file specified from its filename, zips the corresponding
% term, and returns it, as a compressed binary.
%
% Note: useful for network transfers of small files.
%
% Larger ones should be transferred with TCP/IP and by chunks.
%
% Returns a binary.
%
-spec file_to_zipped_term( file_name() ) -> binary().
file_to_zipped_term( Filename ) ->

	DummyFileName = "dummy",

	{ ok, { _DummyFileName, Bin } } =
		%zip:zip( DummyFileName, [ Filename ], [ verbose, memory ] ),
		zip:zip( DummyFileName, [ Filename ], [ memory ] ),

	Bin.



% Reads specified binary, extracts the zipped file in it and writes it on disk,
% in current directory.
%
% Returns the filename of the unzipped file.
%
-spec zipped_term_to_unzipped_file( binary() ) -> file_name().
zipped_term_to_unzipped_file( ZippedTerm ) ->
	%zip:unzip( ZippedTerm, [ verbose ] ).
	{ ok, [ FileName ] } = zip:unzip( ZippedTerm ),
	FileName.



% Reads specified binary, extracts the zipped file in it and writes it on disk,
% in current directory, under specified filename instead of under filename
% stored in the zip archive.
%
% Any pre-existing file will be overwritten.
%
% Note: only one file is expected to be stored in the specified archive.
%
-spec zipped_term_to_unzipped_file( binary(), file_name() )
								  -> basic_utils:void().
zipped_term_to_unzipped_file( ZippedTerm, TargetFilename ) ->

	{ ok, [ { _AFilename, Binary } ] } = zip:unzip( ZippedTerm, [ memory ] ),

	% { ok, File } = file:open( TargetFilename, [ write ] ),
	% ok = io:format( File, "~s", [ binary_to_list(Binary) ] ),
	% ok = file:write_file( File, "~s", [ binary_to_list(Binary) ] ),
	% ok = file:close( File ).
	write_whole( TargetFilename, Binary ).



% Reads in memory the files specified from their filenames, zips the
% corresponding term, and returns it.
%
% Note: useful for network transfers of small files.
%
% Larger ones should be transferred with TCP/IP and by chunks.
%
% Returns a binary.
%
-spec files_to_zipped_term( [file_name()] ) -> binary().
files_to_zipped_term( FilenameList ) ->

	DummyFileName = "dummy",

	{ ok, { _DummyFileName, Bin } } = zip:zip( DummyFileName, FilenameList,
										[ memory ] ),

	Bin.



% Reads in memory the files specified from their filenames, assuming their path
% is relative to the specified base directory, zips the corresponding term, and
% returns it.
%
% Note: useful for network transfers of small files.
%
% Larger ones should be transferred with TCP/IP and by chunks.
%
% Returns a binary.
%
-spec files_to_zipped_term( [ file_name() ], directory_name() ) -> binary().
files_to_zipped_term( FilenameList, BaseDirectory ) ->

	DummyFileName = "dummy",

	%io:format( "files_to_zipped_term operating from ~s on files: ~p.~n",
	%		  [ BaseDirectory, FilenameList ] ),

	 case zip:zip( DummyFileName, FilenameList,
						[ memory, { cwd, BaseDirectory } ] ) of

		 { ok, { _DummyFileName, Bin } } ->
			 Bin;


		 { error, enoent } ->

			 % Such a short error might be difficult to diagnose:

			 %io:format( "~nfiles_to_zipped_term/2 failed from '~s':~n"
			 %			"~n - directory '~p' exists? ~p",
			 %		[ get_current_directory(), BaseDirectory,
			 %			is_existing_directory( BaseDirectory ) ] ),

			 % [ io:format( "~n - file '~p' exists? ~p", [ F,
			 %	   is_existing_file( F ) ] ) || F <- FilenameList ],

			 throw( { zip_failed, BaseDirectory, FilenameList } );

		 { error, Other } ->
			 throw( { zip_failed, Other, BaseDirectory, FilenameList } )

	 end.



% Reads specified binary, extracts the zipped files stored in it and writes them
% on disk, in current directory.
%
% Returns the list of filenames corresponding to the unzipped files.
%
-spec zipped_term_to_unzipped_files( binary() ) -> [ file_name() ].
zipped_term_to_unzipped_files( ZippedTerm ) ->
	%{ ok, FileNames } = zip:unzip( ZippedTerm, [ verbose ] ),
	{ ok, FileNames } = zip:unzip( ZippedTerm ),
	FileNames.



% Reads specified binary, extracts the zipped files in it and writes them on
% disk, in specified directory.
%
% Returns the list of filenames corresponding to the unzipped files.
%
-spec zipped_term_to_unzipped_files( binary(), directory_name() )
		-> [ file_name() ].
zipped_term_to_unzipped_files( ZippedTerm, TargetDirectory ) ->

	%{ ok, FileNames } = zip:unzip( ZippedTerm, [ verbose ] ),

	case is_existing_directory( TargetDirectory ) of

		true ->
			{ ok, FileNames } = zip:unzip( ZippedTerm,
										   [ { cwd, TargetDirectory } ] ),
			FileNames;

		false ->
			throw( { non_existing_unzip_directory, TargetDirectory } )

	end.
