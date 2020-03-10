% Copyright (C) 2003-2020 Olivier Boudeville
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
% Creation date: Saturday, July 12, 2008.


% Gathering of various convenient facilities regarding files.
%
% See file_utils_test.erl for the corresponding test.
%
-module(file_utils).


% Related standard modules: file, filename.


% Note: using the file module has been known to cause synchronization overheads,
% often prim_file is used instead.



% Filename-related operations.
%
% See also: filename:dirname/1 and filename:basename/1.
%
% Ex:
%  - filename:dirname("/aaa/bbb.txt") = "/aaa"
%  - filename:basename("/aaa/bbb.txt") = "bbb.txt"
%
-export([ join/1, join/2, convert_to_filename/1,

		  get_extensions/1, get_extension/1, remove_extension/1,
		  replace_extension/3,

		  exists/1, get_type_of/1, get_owner_of/1, get_group_of/1,
		  is_file/1,
		  is_existing_file/1, is_existing_link/1,
		  is_existing_file_or_link/1,
		  is_executable/1, is_directory/1, is_existing_directory/1,
		  is_existing_directory_or_link/1,
		  list_dir_elements/1,

		  get_size/1, get_last_modification_time/1, touch/1,
		  create_empty_file/1,

		  get_current_directory/0, get_bin_current_directory/0,
		  set_current_directory/1,

		  filter_by_extension/2, filter_by_extensions/2,
		  filter_by_included_suffixes/2, filter_by_excluded_suffixes/2,

		  find_files_from/1, find_files_from/2,
		  find_regular_files_from/1, find_links_from/1,
		  find_files_with_extension_from/2, find_files_with_extension_from/3,
		  find_files_with_excluded_dirs/2,
		  find_files_with_excluded_suffixes/2,
		  find_files_with_excluded_suffixes/3,
		  find_files_with_excluded_dirs_and_suffixes/3,
		  find_directories_from/1,

		  create_directory/1, create_directory/2,
		  create_directory_if_not_existing/1,
		  create_directory_if_not_existing/2,
		  create_temporary_directory/0,

		  remove_file/1, remove_file_if_existing/1,
		  remove_files/1, remove_files_if_existing/1,

		  remove_empty_directory/1, remove_empty_path/1, remove_empty_tree/1,
		  remove_directory/1,

		  copy_file/2, try_copy_file/2, copy_file_if_existing/2, copy_file_in/2,
		  copy_tree/2,

		  rename/2, move_file/2, create_link/2,

		  get_non_clashing_entry_name_from/1,

		  append_file/2, change_permissions/2,

		  is_absolute_path/1,
		  ensure_path_is_absolute/1, ensure_path_is_absolute/2,
		  normalise_path/1, make_relative/1, make_relative/2,
		  get_longest_common_path/1,

		  is_leaf_among/2,

		  update_with_keywords/3,

		  path_to_variable_name/1, path_to_variable_name/2,

		  remove_upper_levels_and_extension/1,

		  get_image_extensions/0, get_image_file_png/1, get_image_file_gif/1 ]).



% I/O section.
-export([ open/2, open/3, close/1, close/2,
		  read/2, write/2, write/3,
		  read_whole/1, write_whole/2,
		  read_terms/1, write_terms/2, write_terms/4, write_direct_terms/2 ]).


% Compression-related operations.
-export([ get_extension_for/1,
		  compress/1, compress/2, decompress/1, decompress/2,
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

% We do not believe that atoms shall be legit paths:
-type any_path() :: path() | bin_path().


% Designates a filename, generally without a path (ex: "foobar.txt"):
-type file_name() :: path().

% Just a convenience alias:
-type filename() :: file_name().


% Designates a path to a file (including its filename); ex:
% "../my_dir/other/foobar.txt").
%
-type file_path() :: path().


-type bin_file_name() :: binary().
-type bin_file_path() :: binary().


% Could also be the more general file:name_all():
-type any_file_name() :: file_name() | bin_file_name().

-type any_file_path() :: file_path() | bin_file_path().


% The name of a (symbolic) link:
-type link_name() :: string().


% Designates an executable, generally without a path (ex: "foobar"):
-type executable_name() :: file_name().


% Designates a path to an executable; ex: "../my_dir/other/run.exe").
-type executable_path() :: file_path().

% Designates a path to an executable, as a binary.
-type bin_executable_path() :: bin_file_path().


% Designates a path to an (executable) script; ex: "../my_dir/other/run.sh").
-type script_path() :: file_path().

% Designates a path to an (executable) script, as a binary.
-type bin_script_path() :: bin_file_path().


-type directory_name() :: path().
-type bin_directory_name() :: binary().

-type any_directory_name() :: directory_name() | bin_directory_name().


-type directory_path() :: path().
-type bin_directory_path() :: binary().

-type any_directory_path() :: directory_path() | bin_directory_path().



% An extension in a filename (ex: "baz", in "foobar.baz.json"):
-type extension() :: string().


% A leaf name, i.e. the final part of a path (possibly a file or directory).
%
% Ex: in 'aaa/bbb/ccc', 'aaa' is the root, and 'ccc' is the leaf.
%
-type leaf_name() :: string().



% All known types of file entries:
-type entry_type() :: 'device' | 'directory' | 'other' | 'regular' | 'symlink'.


% Tells whether parent directories shall be created:
-type parent_creation() :: 'create_no_parent' | 'create_parents'.


% Relevant flags when opening a file (ex: read, write, append, exclusive, raw,
% etc.).
%
% See http://erlang.org/doc/man/file.html#open-2 for their detailed description.
%
% (file:mode() not exported currently unfortunately, see
% lib/kernel/src/file.erl)
%
%-type file_open_mode() :: file:mode() | 'ram'.
-type file_open_mode() :: tuple() | atom() | 'ram'.


% The supported compression formats:
-type compression_format() :: 'zip' | 'bzip2' | 'xz'.


% Corresponds to the handle to an open file (typically a file descriptor
% counterpart), but also, possibly, 'standard_io' (for standard output,
% descriptor 1), 'standard_error' (for standard error, descriptor 2), a
% registered name (as an atom), or any PID handling the I/O protocols:
%
-type file() :: file:io_device().

-type file_info() :: #file_info{}.



% The various permissions that can be combined for file-like elements:
-type permission() :: 'owner_read'  | 'owner_write' | 'owner_execute'
					| 'group_read'  | 'group_write' | 'group_execute'
					| 'other_read'  | 'other_write' | 'other_execute'
					| 'set_user_id' | 'set_group_id'.


-export_type([ path/0, bin_path/0, any_path/0,
			   file_name/0, filename/0, file_path/0,
			   bin_file_name/0, bin_file_path/0,
			   any_file_name/0, any_file_path/0,
			   any_directory_name/0, any_directory_path/0,
			   executable_name/0, executable_path/0, bin_executable_path/0,
			   script_path/0, bin_script_path/0,
			   directory_name/0, bin_directory_name/0,
			   directory_path/0, bin_directory_path/0,
			   extension/0,
			   entry_type/0, parent_creation/0, permission/0,
			   compression_format/0,
			   file/0, file_info/0 ]).



% Filename-related operations.


% Platform-specific:

% UNIX conventions:
-define( directory_separator, $/ ).

% Windows conventions:
%-define( directory_separator, $\ ).



% Joins the specified list of path elements.
%
% This function has been added back to this module; filename:join( Components )
% could be used instead (at least to some extent), however filename:join( [ "",
% "my_dir" ] ) results in "/my_dir", whereas often we would want "my_dir"
% instead - which is returned by our function; moreover, if one of the
% components includes an absolute path (such as "/xxx" with Unix conventions),
% the preceding components, if any, were removed from the result (which does not
% seem desirable); here we throw an exception instead.
%
% So we deem our version simpler and less prone to surprise (least
% astonishment).
%
% Plain and binary strings can be freely used as arguments, and a plain string
% is returned in all cases.
%
% See filename:split/1 for the reverse operation.
%
-spec join( [ any_path() ] ) -> path().
join( ComponentList ) when is_list( ComponentList ) ->
	lists:foldr( fun join/2, _Acc0="", _List=ComponentList );

join( NonList ) ->
	throw( { cannot_join, NonList } ).



% Joins the two specified path elements.
%
% This function has been added back to this module; filename:join( Name1, Name2
% ) could be used instead (at least to some extent); however filename:join( "",
% "my_dir" ) results in "/my_dir", whereas often we would want "my_dir" - which
% is returned by our function; moreover filename:join( SomePath, AbsPath= [
% ?directory_separator | _ ] ) returns AbsPath, dropping SomePath for some
% reason (which does not seem desirable); here we throw an exception instead.
%
% So we deem our version simpler and less prone to surprise (least
% astonishment).
%
% Plain and binary strings can be freely used as arguments, and returns a plain
% string in all cases.
%
% See filename:split/1 for the reverse operation.
%
-spec join( any_path(), any_path() ) -> path().
% Skips only initial empty paths of all sorts:
join( _FirstPath="", SecondPath ) ->
	SecondPath ;

join( _FirstPath= <<"">>, SecondPath ) ->
	SecondPath ;

% If second is string (already):
join( FirstPath, SecondPath=[ ?directory_separator | _ ] ) ->
	throw( { rightwise_absolute_directory, SecondPath,
			 { leftwise, FirstPath } } );

join( FirstPath, SecondPath ) when is_binary( FirstPath ) ->
	join( text_utils:binary_to_string( FirstPath ), SecondPath );

% Second as string to match separator above:
join( FirstPath, SecondPath ) when is_binary( SecondPath ) ->
	join( FirstPath, text_utils:binary_to_string( SecondPath ) );

% Both are strings from this point:
join( FirstPath, _SecondPath="" ) ->
	% We do not want to add a trailing separator (ex: "/home/lisa", not
	% "/home/lisa/"):
	%
	FirstPath;

join( FirstPath, SecondPath ) ->

	% First as at least one element by design; avoid adding an extra separator:
	%
	% (we do not use list_utils:get_last_element/1 here as we do not want this
	% very common function of the file_utils module to depend on a list_utils
	% one):
	%
	case get_last_element( FirstPath ) of

		?directory_separator ->
			text_utils:format( "~s~s", [ FirstPath, SecondPath ] );

		_ ->
			text_utils:format( "~s~c~s",
					   [ FirstPath, ?directory_separator, SecondPath ] )

	end.



% Duplicated verbatim from list_utils, so that file_utils can remain a mostly
% autonomous, pioneer module.
%
% Returns the last element of the specified list.
%
% Note: not computationnally efficient, usually having to retrieve the last
% element suggests a bad code design.
%
% Crashes (with 'no function clause') if the input list is empty.
%
%-spec get_last_element( list() ) -> element().
get_last_element( _List=[ SingleElement ] ) ->
	SingleElement;

get_last_element( _List=[ _H | T ] ) ->
	get_last_element( T ).



% Converts specified name to an acceptable filename, filesystem-wise.
-spec convert_to_filename( text_utils:any_string() ) -> file_name().
convert_to_filename( Name ) when is_binary( Name ) ->
	convert_to_filename( text_utils:binary_to_string( Name ) );

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
	re:replace( lists:flatten( Name ),
				"( |<|>|,|\\(|\\)|'|\"|/|\\\\|\&|~|"
				"#|@|{|}|\\[|\\]|\\||\\$|\\*|\\?|!|\\+|;|:)+", "_",
				[ global, { return, list } ] ).



% Returns the (ordered) extension(s) of the specified filename.
%
% Ex: [ "baz", "json" ] = get_extensions( "foobar.baz.json" )
%
-spec get_extensions( file_name() ) -> [ extension() ] | 'no_extension'.
get_extensions( Filename ) ->

	case text_utils:split( Filename, _Delimiters=[ $. ] ) of

		[] ->
			no_extension;

		[ _Basename ] ->
			no_extension;

		[ _Basename | Extensions ] ->
			Extensions;

		_ ->
			no_extension

	end.



% Returns the (last) extension of the specified filename.
%
% Ex: "json" = get_extension( "foobar.baz.json" )
%
-spec get_extension( file_name() ) -> extension() | 'no_extension'.
get_extension( Filename ) ->

	case get_extensions( Filename ) of

		no_extension ->
			no_extension;

		Extensions ->
			list_utils:get_last_element( Extensions )

	end.



% Removes the (last) extension of the specified filename.
%
% Ex: "/home/jack/rosie.tmp" = remove_extension( "/home/jack/rosie.tmp.ttf" )
%
-spec remove_extension( file_name() ) -> file_name().
remove_extension( Filename ) ->

	case text_utils:split( Filename, _Delimiters=[ $. ] ) of

		% Returning an empty string for an empty string:
		[] ->
			Filename;

		[ Filename ] ->
			Filename;

		[ Basename | Extensions ] ->
			text_utils:join( $., [ Basename | list_utils:remove_last_element( Extensions ) ] )

	end.



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
-spec exists( any_path() ) -> boolean().
exists( EntryName ) ->

	case file:read_file_info( EntryName ) of

		{ ok, _FileInfo } ->
			true;

		{ error, _Reason } ->
			false

	end.



% Returns the type of the specified file entry.
-spec get_type_of( any_path() ) -> entry_type().
get_type_of( EntryName ) ->

	% We used to rely on file:read_file_info/1, but an existing symlink pointing
	% to a non-existing entry was triggering the enoent error, while we just
	% wanted to know that the specified entry is an existing (yet dead) symlink.

	% Some tools (e.g. emacs) used thus to get in the way, as apparently they
	% create dead symlinks on purpose, to store information.

	case file:read_link_info( EntryName ) of

		{ ok, #file_info{ type=FileType } } ->
			FileType;

		{ error, eloop } ->
			% Probably a recursive symlink:
			throw( { too_many_symlink_levels, EntryName } );

		{ error, enoent } ->
			throw( { non_existing_entry, EntryName } )

	end.



% Returns the user identifier (uid) of the owner of the specified file entry.
-spec get_owner_of( any_path() ) -> system_utils:user_id().
get_owner_of( EntryName ) ->

	case file:read_file_info( EntryName ) of

		{ ok, #file_info{ uid=UID } } ->
			UID;

		{ error, Reason } ->
			throw( { file_info_failure, Reason, EntryName } )

	end.



% Returns the group identifier (gid) of the group of the specified file entry.
-spec get_group_of( any_path() ) -> system_utils:group_id().
get_group_of( EntryName ) ->

	case file:read_file_info( EntryName ) of

		{ ok, #file_info{ gid=GID } } ->
			GID;

		{ error, Reason } ->
			throw( { file_info_failure, Reason, EntryName } )

	end.



% Returns whether the specified entry, supposedly existing, is a regular file.
%
% If the specified entry happens not to exist, a
% '{ non_existing_entry, EntryName }' exception will be thrown.
%
-spec is_file( any_path() ) -> boolean().
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
-spec is_existing_file( any_path() ) -> boolean().
is_existing_file( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		regular ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is a symbolic file.
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_existing_link( any_path() ) -> boolean().
is_existing_link( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		symlink ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is either a regular file or a
% symbolic link.
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_existing_file_or_link( any_path() ) -> boolean().
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
-spec is_executable( any_path() ) -> boolean().
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
-spec is_directory( any_path() ) -> boolean().
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
-spec is_existing_directory( any_path() ) -> boolean().
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
-spec is_existing_directory_or_link( any_path() ) -> boolean().
is_existing_directory_or_link( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		directory ->
			true ;

		symlink ->
			true ;

		_ ->
			false

	end.



% Returns a tuple containing five lists corresponding to the per-type
% dispatching of all filesystem elements local to specified directory (hence not
% recursively traversed), namely:
% { RegularFiles, Symlinks, Directories, OtherFiles, Devices }.
%
% Note that symbolic links may or may not be dead.
%
-spec list_dir_elements( directory_name() ) ->
			 { [ file_name() ], [ file_name() ], [ directory_name() ],
			   [ file_name() ], [ file_name() ] }.
list_dir_elements( Dirname ) ->

	%io:format( "list_dir_elements for '~s'.~n", [ Dirname ] ),

	% Previously file:list_dir_all/1 was tested in order to collect raw
	% filenames as well (hoping to avoid warning reports such as "Non-unicode
	% filename <<"XXX">> ignored"), yet the returned names were mangled, leading
	% to enoent whenever trying to open them (refer to
	% http://erlang.org/doc/apps/stdlib/unicode_usage.html#notes-about-raw-filenames):
	%
	{ ok, LocalDirElements } = file:list_dir( Dirname ),

	classify_dir_elements( Dirname, LocalDirElements, _Devices=[],
			_Directories=[], _Files=[], _Symlinks=[], _OtherFiles=[] ).



% Returns the size, in bytes, of the specified file entry.
-spec get_size( file_name() ) -> system_utils:byte_size().
get_size( Filename ) ->

	case file:read_file_info( Filename ) of

		{ ok, #file_info{ size=Size } } ->
			Size;

		{ error, Reason } ->
			throw( { file_info_failure, Reason, Filename } )

	end.



% Returns the last time at which the content of specified file entry was
% modified (not counting attribute or permission changes), according to the
% filesystem.
%
% Said time will be expressed as an integer number of seconds since (or before)
% Unix time epoch, which is 1970-01-01 00:00 UTC.
%
-spec get_last_modification_time(  any_path() ) -> time_utils:posix_seconds().
get_last_modification_time( Filename ) ->

	case file:read_file_info( Filename, [ { time, posix } ] ) of

		{ ok, #file_info{ mtime=Seconds } } ->
			Seconds;

		{ error, Reason } ->
			throw( { file_info_failure, Reason, Filename } )

	end.



% Updates the modification time (the last time at which its content was reported
% as modified according to the filesystem) of the specified file entry, which
% must already exist.
%
% Note: leaves last access time unchanged, updates both modification and change
% times.
%
% See also: create_empty_file/1
%
-spec touch( any_path() ) -> void().
touch( FileEntry ) ->

	case exists( FileEntry ) of

		true ->
			% -c: do not create any file
			% -m: change only the modification time
			%
			case system_utils:run_executable( "/bin/touch -c -m '"
												  ++ FileEntry ++ "'" ) of

				{ 0, _Output } ->
					ok;

				{ ErrorCode, Output } ->
					throw( { touch_failed, Output, ErrorCode, FileEntry } )

			end;

		false ->
			throw( { non_existing_file_element_to_touch, FileEntry } )

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
-spec create_empty_file( file_name() ) -> void().
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
-spec get_current_directory() -> directory_path().
get_current_directory() ->

	case file:get_cwd() of

		{ ok, Dir } ->
			Dir;

		{ error, Reason } ->
			throw( { failed_to_determine_current_directory, Reason } )

	end.



% Returns the current directory, as a binary string.
%
% Throws an exception on failure.
%
-spec get_bin_current_directory() -> bin_directory_path().
get_bin_current_directory() ->
	text_utils:string_to_binary( get_current_directory() ).



% Sets the specified directory as current directory.
%
% Throws an exception on failure.
%
-spec set_current_directory( directory_name() ) -> void().
set_current_directory( DirName ) ->

	 % For more detail of { 'error', atom() }, refer to type specifications of
	 % erlang files: file.erl and file.hrl.

	case file:set_cwd( DirName ) of

		ok ->
			ok;

		{ error, Error } ->
			throw( { set_current_directory_failed, DirName, Error } )

	end.



% (helper)
%
% Returns a tuple containing five lists corresponding to the sorting of all
% filesystem elements, namely { RegularFiles, Symlinks, Directories, OtherFiles,
% Devices }.
%
% Note that Files include symbolic links (dead or not).
%
classify_dir_elements( _Dirname, _Elements=[], Devices, Directories, Files,
					   Symlinks, OtherFiles ) ->
	% Note the reordering:
	{ Files, Symlinks, Directories, OtherFiles, Devices };

classify_dir_elements( Dirname, _Elements=[ H | T ], Devices, Directories,
					   Files, Symlinks, OtherFiles ) ->

	 case get_type_of( filename:join( Dirname, H ) ) of

		device ->
			classify_dir_elements( Dirname, T, [ H | Devices ], Directories,
								   Files, Symlinks, OtherFiles );

		directory ->
			classify_dir_elements( Dirname, T, Devices, [ H | Directories ],
								   Files, Symlinks, OtherFiles );

		regular ->
			classify_dir_elements( Dirname, T, Devices, Directories,
								   [ H | Files ], Symlinks, OtherFiles );

		% Used to be managed as regular files:
		symlink ->
			classify_dir_elements( Dirname, T, Devices, Directories, Files,
								   [ H | Symlinks ], OtherFiles );

		other ->
			classify_dir_elements( Dirname, T, Devices, Directories,
								   Files, Symlinks, [ H | OtherFiles ] )

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



% (helper)
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



% Returns the list of all files (regular ones and symlinks) found from the root,
% in the whole subtree (i.e. recursively).
%
% All extensions and suffixes accepted, no excluded directories.
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_files_from( any_directory_name() ) -> [ file_name() ].
find_files_from( RootDir ) ->
	Res = find_files_from( RootDir, _CurrentRelativeDir="",
						   _IncludeSymlinks=true, _Acc=[] ),
	%trace_utils:debug_fmt( "Files found from '~s':~n~p", [ RootDir, Res ] ),
	Res.



% Returns the list of all regular files found from the root, in the whole
% subtree (i.e. recursively).
%
% All extensions and suffixes accepted, no excluded directories.
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_regular_files_from( any_directory_name() ) -> [ file_name() ].
find_regular_files_from( RootDir ) ->
	find_files_from( RootDir, _IncludeSymlinks=false ).



% Returns the list of all files (regular ones and, if requested, symlinks) found
% from the root, in the whole subtree (i.e. recursively).
%
% All extensions and suffixes accepted, no excluded directories.
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_files_from( any_directory_name(), boolean() ) -> [ file_name() ].
find_files_from( RootDir, IncludeSymlinks ) ->
	Res = find_files_from( RootDir, _CurrentRelativeDir="", IncludeSymlinks,
						   _Acc=[] ),
	%trace_utils:debug_fmt( "Files found from '~s':~n~p", [ RootDir, Res ] ),
	Res.



% (helper)
find_files_from( RootDir, CurrentRelativeDir, IncludeSymlinks, Acc ) ->

	%trace_utils:debug_fmt( "find_files_from with root = '~s', current = '~s'.",
	%						[ RootDir, CurrentRelativeDir ] ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( join( RootDir, CurrentRelativeDir ) ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	Acc ++ list_files_in_subdirs( Directories, RootDir, CurrentRelativeDir,
								  IncludeSymlinks, _NextAcc=[] )
		++ prefix_files_with( CurrentRelativeDir, Files ).


% Specific helper for find_files_from/4 above:
list_files_in_subdirs( _Dirs=[], _RootDir, _CurrentRelativeDir,
					   _IncludeSymlinks, Acc ) ->
	Acc;

list_files_in_subdirs( _Dirs=[ H | T ], RootDir, CurrentRelativeDir,
					   IncludeSymlinks, Acc ) ->

	%io:format( "list_files_in_subdirs with root = '~s', current = '~s' "
	%	"and H='~s'.~n", [ RootDir, CurrentRelativeDir, H ] ),

	list_files_in_subdirs( T, RootDir, CurrentRelativeDir, IncludeSymlinks,
		find_files_from( RootDir, join( CurrentRelativeDir, H ),
						 IncludeSymlinks, _NextAcc=[] ) ++ Acc ).



% Returns the list of all symlinks found from the root, in the whole subtree
% (i.e. recursively).
%
% All extensions and suffixes accepted, no excluded directories.
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_links_from( any_directory_name() ) -> [ file_name() ].
find_links_from( RootDir ) ->
	find_links_from( RootDir, _CurrentRelativeDir="", _Acc=[] ).


% (helper)
find_links_from( RootDir, CurrentRelativeDir, Acc ) ->

	%trace_utils:debug_fmt( "find_links_from with root = '~s', current = '~s'.",
	%						[ RootDir, CurrentRelativeDir ] ),

	{ _RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( join( RootDir, CurrentRelativeDir ) ),

	Acc ++ list_links_in_subdirs( Directories, RootDir, CurrentRelativeDir,
								  _NextAcc=[] )
		++ prefix_files_with( CurrentRelativeDir, Symlinks ).



% Specific helper for find_links_from/3 above:
list_links_in_subdirs( _Dirs=[], _RootDir, _CurrentRelativeDir, Acc ) ->
	Acc;

list_links_in_subdirs( _Dirs=[ H | T ], RootDir, CurrentRelativeDir, Acc ) ->

	%io:format( "list_links_in_subdirs with root = '~s', current = '~s' "
	%	"and H='~s'.~n", [ RootDir, CurrentRelativeDir, H ] ),

	list_links_in_subdirs( T, RootDir, CurrentRelativeDir,
		find_links_from( RootDir, join( CurrentRelativeDir, H ),
						 _NextAcc=[] ) ++ Acc ).



% Returns the list of all files (regular ones and symlinks) found from the root
% with specified extension, in the whole subtree (i.e. recursively).
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_files_with_extension_from( any_directory_name(), extension() ) ->
											[ file_name() ].
find_files_with_extension_from( RootDir, Extension ) ->
	find_files_with_extension_from( RootDir, _CurrentRelativeDir="",
			_IncludeSymlinks=true, Extension, _Acc=[] ).



% Returns the list of all files (regular ones and, if requested, symlinks) found
% from the root with specified extension, in the whole subtree
% (i.e. recursively).
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_files_with_extension_from( any_directory_name(), extension(),
									  boolean() ) -> [ file_name() ].
find_files_with_extension_from( RootDir, Extension, IncludeSymlinks ) ->
	find_files_with_extension_from( RootDir, _CurrentRelativeDir="",
									IncludeSymlinks, Extension, _Acc=[] ).



% (helper)
find_files_with_extension_from( RootDir, CurrentRelativeDir, IncludeSymlinks,
								Extension, Acc ) ->

	%io:format( "find_files_with_extension_from in ~s.~n",
	%           [ CurrentRelativeDir ] ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( join( RootDir, CurrentRelativeDir ) ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	Acc ++ list_files_in_subdirs_with_extension( Directories, Extension,
				RootDir, CurrentRelativeDir, IncludeSymlinks, _NextAcc=[] )
		++ prefix_files_with( CurrentRelativeDir,
							  filter_by_extension( Files, Extension ) ).


% Helper for find_files_with_extension_from/4:
list_files_in_subdirs_with_extension( _Dirs=[], _Extension, _RootDir,
				_CurrentRelativeDir, _IncludeSymlinks, Acc ) ->
	Acc;

list_files_in_subdirs_with_extension( _Dirs=[ H | T ], Extension, RootDir,
				CurrentRelativeDir, IncludeSymlinks, Acc ) ->
	list_files_in_subdirs_with_extension( T, Extension, RootDir,
		CurrentRelativeDir, IncludeSymlinks,
		find_files_with_extension_from( RootDir, join( CurrentRelativeDir, H ),
			IncludeSymlinks, Extension, _NextAcc=[] ) ++ Acc ).



% Returns the list of all files (regular ones and symlinks) found from the root,
% in the whole subtree (i.e. recursively), with specified directories excluded.
%
% Note that the excluded directories can be specified as a full path (ex:
% "foo/bar/not-wanted"), or just as a final directory name (ex:
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
-spec find_files_with_excluded_dirs( any_directory_name(),
									 [ directory_name() ] ) -> [ file_name() ].
find_files_with_excluded_dirs( RootDir, ExcludedDirList ) ->
	find_files_with_excluded_dirs( RootDir, ExcludedDirList,
								   _IncludeSymlinks=true ).



% Returns the list of all files (regular ones and, if requested, symlinks) found
% from the root, in the whole subtree (i.e. recursively), with specified
% directories excluded.
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
-spec find_files_with_excluded_dirs( any_directory_name(), [ directory_name() ],
									 boolean() ) -> [ file_name() ].
find_files_with_excluded_dirs( RootDir, ExcludedDirList, IncludeSymlinks ) ->
	find_files_with_excluded_dirs( RootDir, _CurrentRelativeDir="",
								   ExcludedDirList, IncludeSymlinks, _Acc=[] ).


% (helper)
find_files_with_excluded_dirs( RootDir, CurrentRelativeDir, ExcludedDirList,
							   IncludeSymlinks, Acc ) ->

	%io:format( "find_files_with_excluded_dirs in ~s.~n",
	% [ CurrentRelativeDir ] ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( join( RootDir, CurrentRelativeDir ) ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	% If for example ExcludedDirList=[ ".svn" ], we want to eliminate not only
	% ".svn" but also all "foo/bar/.svn", i.e. all directories having the same
	% (last) name:
	%
	FilteredDirectories = [ D || D <- Directories,
		not ( lists:member( join( CurrentRelativeDir, D ), ExcludedDirList )
			  orelse lists:member( D, ExcludedDirList ) ) ],

	Acc ++ list_files_in_subdirs_excluded_dirs( FilteredDirectories, RootDir,
				CurrentRelativeDir, ExcludedDirList, IncludeSymlinks, _Acc=[] )
		++ prefix_files_with( CurrentRelativeDir, Files ).


% Specific helper for find_files_with_excluded_dirs/4 above:
list_files_in_subdirs_excluded_dirs( _Dirs=[], _RootDir,
		_CurrentRelativeDir, _ExcludedDirList, _IncludeSymlinks, Acc ) ->
	Acc;

list_files_in_subdirs_excluded_dirs( _Dirs=[ H | T ], RootDir,
		CurrentRelativeDir, ExcludedDirList, IncludeSymlinks, Acc ) ->
	list_files_in_subdirs_excluded_dirs( T, RootDir, CurrentRelativeDir,
		ExcludedDirList, IncludeSymlinks,
		find_files_with_excluded_dirs( RootDir, join( CurrentRelativeDir, H ),
			ExcludedDirList, IncludeSymlinks, _NextAcc=[] ) ++ Acc ).





% Returns the list of all files (regular ones or symlinks) found from the root
% which do not match any of the specified suffixes, in the whole subtree
% (i.e. recursively).
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_files_with_excluded_suffixes( any_directory_name(), [ string() ] ) ->
											   [ file_name() ].
find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes ) ->
	find_files_with_excluded_suffixes( RootDir, _CurrentRelativeDir="",
				ExcludedSuffixes, _IncludeSymlinks=true, _Acc=[] ).



% Returns the list of all files (regular ones or, if requested, symlinks) found
% from the root which do not match any of the specified suffixes, in the whole
% subtree (i.e. recursively).
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
%
-spec find_files_with_excluded_suffixes( any_directory_name(), [ string() ],
										 boolean() ) -> [ file_name() ].
find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes,
								   IncludeSymlinks ) ->
	find_files_with_excluded_suffixes( RootDir, _CurrentRelativeDir="",
				ExcludedSuffixes, IncludeSymlinks, _Acc=[] ).



% (helper)
find_files_with_excluded_suffixes( RootDir, CurrentRelativeDir,
								   ExcludedSuffixes, IncludeSymlinks, Acc ) ->

	%io:format( "find_files_with_excluded_suffixes in ~s.~n",
	% [ CurrentRelativeDir ] ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( join( RootDir, CurrentRelativeDir ) ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	Acc ++ list_files_in_subdirs_with_excluded_suffixes( Directories,
			ExcludedSuffixes, RootDir, CurrentRelativeDir, IncludeSymlinks,
			_NextAcc=[] ) ++ prefix_files_with( CurrentRelativeDir,
			  filter_by_excluded_suffixes( Files, ExcludedSuffixes ) ).




% Helper for find_files_with_excluded_suffixes/4:
-spec list_files_in_subdirs_with_excluded_suffixes( list(), [ string() ],
		directory_name(), directory_name(), boolean(), [ file_name() ] ) ->
														  [ file_name() ].
list_files_in_subdirs_with_excluded_suffixes( [], _ExcludedSuffixes, _RootDir,
							  _CurrentRelativeDir, _IncludeSymlinks, Acc ) ->
	Acc;

list_files_in_subdirs_with_excluded_suffixes( [ H | T ], ExcludedSuffixes,
					  RootDir, CurrentRelativeDir, IncludeSymlinks, Acc ) ->
	list_files_in_subdirs_with_excluded_suffixes( T, ExcludedSuffixes, RootDir,
		CurrentRelativeDir, IncludeSymlinks,
		find_files_with_excluded_suffixes( RootDir,
			join( CurrentRelativeDir, H ), ExcludedSuffixes, IncludeSymlinks,
			_NextAcc=[] ) ++ Acc ).




% Returns the list of all files (regular ones and symlinks) found from the root
% with specified suffix, in the whole subtree (i.e. recursively), with specified
% directories excluded.
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
-spec find_files_with_excluded_dirs_and_suffixes( any_directory_name(),
		[ directory_name() ], [ string() ] ) -> [ file_name() ].
find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirList,
											ExcludedSuffixes ) ->
	find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirList,
								ExcludedSuffixes, _IncludeSymlinks=true ).



% Returns the list of all files (regular ones and, if requested, symlinks) found
% from the root with specified suffix, in the whole subtree (i.e. recursively),
% with specified directories excluded.
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
-spec find_files_with_excluded_dirs_and_suffixes( any_directory_name(),
		[ directory_name() ], [ string() ], boolean() ) -> [ file_name() ].
find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirList,
							ExcludedSuffixes, IncludeSymlinks ) ->

	%{ ok, CurrentDir } = file:get_cwd(),
	%io:format( "find_files_with_excluded_dirs_and_suffixes: current is ~s, "
	%			"root is ~s.~n", [ CurrentDir, RootDir ] ),

	find_files_with_excluded_dirs_and_suffixes( RootDir,
			_CurrentRelativeDir="", ExcludedDirList, ExcludedSuffixes,
			IncludeSymlinks, _Acc=[] ).


% (helper)
find_files_with_excluded_dirs_and_suffixes( RootDir, CurrentRelativeDir,
		ExcludedDirList, ExcludedSuffixes, IncludeSymlinks, Acc ) ->

	%io:format( "find_files_with_excluded_dirs_and_suffixes in ~s / ~s.~n",
	%           [ RootDir, CurrentRelativeDir ] ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( join( RootDir, CurrentRelativeDir ) ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	% If for example ExcludedDirList=[ ".svn" ], we want to eliminate not only
	% ".svn" but also all "foo/bar/.svn", i.e. all directories having the same
	% (last) name:
	%
	FilteredDirectories = [ D || D <- Directories,
		not ( lists:member( join( CurrentRelativeDir, D ), ExcludedDirList )
			 orelse lists:member( D, ExcludedDirList ) ) ],

	Acc ++ list_files_in_subdirs_excluded_dirs_and_suffixes(
			FilteredDirectories, RootDir, CurrentRelativeDir,
			ExcludedDirList, ExcludedSuffixes, IncludeSymlinks, _Acc=[] )
		++ prefix_files_with( CurrentRelativeDir,
			filter_by_excluded_suffixes( Files, ExcludedSuffixes ) ).




% Specific helper for find_files_with_excluded_dirs_and_suffixes/5 above:
list_files_in_subdirs_excluded_dirs_and_suffixes( _Dirs=[], _RootDir,
		_CurrentRelativeDir, _ExcludedDirList, _ExcludedSuffixes,
		_IncludeSymlinks, Acc ) ->
	Acc;

list_files_in_subdirs_excluded_dirs_and_suffixes( _Dirs=[ H | T ], RootDir,
		CurrentRelativeDir, ExcludedDirList, ExcludedSuffixes, IncludeSymlinks,
		Acc ) ->
	list_files_in_subdirs_excluded_dirs_and_suffixes( T, RootDir,
		CurrentRelativeDir, ExcludedDirList, ExcludedSuffixes, IncludeSymlinks,
		find_files_with_excluded_dirs_and_suffixes( RootDir,
			join( CurrentRelativeDir, H ), ExcludedDirList, ExcludedSuffixes,
			IncludeSymlinks, _NextAcc=[] ) ++ Acc ).


% Prefixes specified paths with specified root directory.
-spec prefix_files_with( directory_name(), [ file_name() ] ) -> [ file_name() ].
prefix_files_with( RootDir, Files ) ->
	%io:format( "Prefixing ~p with '~s'.~n", [ Files, RootDir ] ),
	prefix_files_with( RootDir, Files, _Acc=[] ).


% (helper)
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
-spec find_directories_from( any_directory_name() ) -> [ directory_name() ].
find_directories_from( RootDir ) ->
	find_directories_from( RootDir, "", _Acc=[] ).


% (helper)
find_directories_from( RootDir, CurrentRelativeDir, Acc ) ->

	%io:format( "find_directories_from in ~s.~n", [ CurrentRelativeDir ] ),

	{ _RegularFiles, _Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( join( RootDir, CurrentRelativeDir ) ),

	Acc ++ list_directories_in_subdirs( Directories,
			RootDir, CurrentRelativeDir, _Acc=[] )
		++ prefix_files_with( CurrentRelativeDir, Directories ).



% (helper)
list_directories_in_subdirs( _Dirs=[], _RootDir, _CurrentRelativeDir, Acc ) ->
	Acc;

list_directories_in_subdirs( _Dirs=[ H | T ], RootDir, CurrentRelativeDir,
							 Acc ) ->
	list_directories_in_subdirs( T, RootDir, CurrentRelativeDir,
		find_directories_from( RootDir, join( CurrentRelativeDir, H ), _Acc=[] )
		++ Acc ).



% Creates specified directory ("mkdir"), without creating any intermediate
% (parent) directory that would not exist.
%
% Throws an exception if the operation failed.
%
-spec create_directory( directory_name() ) -> void().
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
-spec create_directory( directory_name(), parent_creation() ) -> void().
create_directory( Dirname, create_no_parent ) ->

	case file:make_dir( Dirname ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { create_directory_failed, Dirname, Reason } )

	end;

create_directory( Dirname, create_parents ) ->
	create_dir_elem( filename:split( Dirname ), "" ).



% (helper)
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



% Creates specified directory (but not any parent thereof), if not already
% existing.
%
% Throws an exception if the operation fails.
%
-spec create_directory_if_not_existing( directory_name() ) -> void().
create_directory_if_not_existing( Dirname ) ->
	create_directory_if_not_existing( Dirname, create_no_parent ).


% Creates specified directory (and, if specified, any needed parent as well), if
% not already existing.
%
% Throws an exception if the operation fails.
%
-spec create_directory_if_not_existing( directory_name(), parent_creation() ) -> void().
create_directory_if_not_existing( Dirname, ParentCreation ) ->

	case is_existing_directory( Dirname ) of

		true ->
			ok;

		false ->
			create_directory( Dirname, ParentCreation )

	end.



% Creates a non previously existing temporary directory, and returns its full
% path.
%
-spec create_temporary_directory() -> directory_name().
create_temporary_directory() ->

	TmpDir = join( [ "/tmp", system_utils:get_user_name(),
					 id_utils:generate_uuid() ] ),

	case exists( TmpDir ) of

		true ->
			% Very bad luck apparently, or same random root:
			create_temporary_directory();

		false ->
			create_directory( TmpDir, create_parents ),
			TmpDir

	end.



% Removes (deletes) specified file, specified as any kind of string.
%
% Throws an exception if any problem occurs.
%
-spec remove_file( any_file_path() ) -> void().
remove_file( Filename ) ->

	%trace_utils:warning_fmt( "Removing file '~s'.", [ Filename ] ),

	case file:delete( Filename ) of
	%case ok of

		ok ->
			ok;

		Error ->
			throw( { remove_file_failed, Filename, Error } )

	end.



% Removes (deletes) specified files, specified as a list of any kind of strings.
-spec remove_files( [ any_file_path() ] ) -> void().
remove_files( FilenameList ) ->

	%trace_utils:warning_fmt( "Removing following files: ~s",
	%						 [ text_utils:strings_to_string( FilenameList ) ] ),

	[ remove_file( Filename ) || Filename <- FilenameList ].



% Removes specified file, specified as any kind of string, iff it is already
% existing, otherwise does nothing.
%
-spec remove_file_if_existing( any_file_path() ) -> void().
remove_file_if_existing( Filename ) ->

	case is_existing_file( Filename ) of

		true ->
			remove_file( Filename );

		false ->
			ok

	end.



% Removes each specified file, in specified list of any kind of strings, iff it
% is already existing.
%
-spec remove_files_if_existing( [ any_file_path() ] ) -> void().
remove_files_if_existing( FilenameList ) ->
	[ remove_file_if_existing( Filename ) || Filename <- FilenameList ].



% Removes specified directory, which must be empty (so: behaves mostly like
% the 'rmdir' shell command).
%
-spec remove_empty_directory( any_directory_path() ) -> void().
remove_empty_directory( DirectoryPath ) ->

	%trace_utils:warning_fmt( "## Removing empty directory '~s'.",
	%                         [ DirectoryPath ] ),

	case file:del_dir( DirectoryPath ) of

		ok ->
			ok;

		{ error, Reason } ->
			% Probably not so empty:
			throw( { remove_empty_directory_failed, Reason, DirectoryPath } )

	end.



% Removes all (supposedly) empty directories pertaining to the specified local,
% relative directory path, i.e. this path (ex: a/b/c) and all its ancestors
% (hence a/b and a are - if empty - removed as well, and none of their possible
% siblings of course); so behaves mostly like the 'rmdir --parents' shell
% command.
%
% Note: does not remove an (empty) tree, just a given directory and its local
% ancestors.
%
-spec remove_empty_path( any_directory_path() ) -> void().
remove_empty_path( DirectoryPath ) ->

	%trace_utils:warning_fmt( "## Removing empty directory '~s'.",
	%                         [ DirectoryPath ] ),

	remove_empty_path_helper( DirectoryPath ).


% (helper)
remove_empty_path_helper( _DirectoryPath="." ) ->
	ok;

remove_empty_path_helper( DirectoryPath ) ->
	remove_empty_directory( DirectoryPath ),
	remove_empty_path_helper( filename:dirname( DirectoryPath ) ).



% Removes all (supposedly) empty directories found from specified directory,
% expected to be the root of a tree that contains only (possibly nested)
% directories (and no other kind of filesystem entry).
%
-spec remove_empty_tree( any_directory_path() ) -> void().
remove_empty_tree( DirectoryPath ) ->

	%trace_utils:warning_fmt( "## Removing empty tree '~s'.",
	%						 [ DirectoryPath ] ),

	% For clarity:
	case is_existing_directory( DirectoryPath ) of

		true ->
			ok;

		false ->
			throw( { directory_not_found, DirectoryPath } )

	end,

	{ RegularFiles, Symlinks, Directories, OtherFiles, Devices } =
		list_dir_elements( DirectoryPath ),

	case RegularFiles of

		[] ->
			ok;

		_ ->
			throw( { regular_files_found, RegularFiles } )

	end,

	case Symlinks of

		[] ->
			ok;

		_ ->
			throw( { symbolic_links_found, Symlinks } )

	end,

	case OtherFiles of

		[] ->
			ok;

		_ ->
			throw( { other_files_found, OtherFiles } )

	end,

	case Devices of

		[] ->
			ok;

		_ ->
			throw( { devices_found, Devices } )

	end,

	[ remove_empty_tree( join( DirectoryPath, D ) ) || D <- Directories ],

	% Now an empty directory, so:
	remove_directory( DirectoryPath ).



% Removes specified (possibly non-empty) directory as a whole, recursively (so:
% behaves mostly like the 'rm -rf ' shell command; of course to use with care).
%
% Note that if any unusual file entry is found in the tree (ex: device or file
% that is neither regular nor a symbolic link), the operation will stop on error
% (whereas elements may already have been removed).
%
-spec remove_directory( any_directory_name() ) -> void().
remove_directory( DirectoryName ) ->

	%trace_utils:warning_fmt( "## Removing recursively directory '~s'.",
	%                         [ DirectoryName ] ),

	% We do it programmatically, rather than running a command like '/bin/rm -rf
	% ...':

	% All local elements:
	{ RegularFiles, Symlinks, Directories, OtherFiles, Devices } =
		list_dir_elements( DirectoryName ),

	case Devices of

		[] ->
			ok;

		_ ->
			trace_utils:error_fmt( "Interrupting removal of directory '~s', as "
						   "device entries have been found: ~p.", [ Devices ] ),

			throw( { device_entries_found, Devices } )

	end,

	case OtherFiles of

		[] ->
			ok;

		_ ->
			trace_utils:error_fmt( "Interrupting removal of directory '~s', as "
						"unexpected filesystem entries have been found: ~p.",
						[ OtherFiles ] ),

			throw( { unexpected_entries_found, OtherFiles } )

	end,

	% Depth-first of course:
	[ remove_directory( join( DirectoryName, SubDir ) )
	  || SubDir <- Directories ],

	% Then removing all local regular files and symlinks:
	[ remove_file( join( DirectoryName, F ) )
	  || F <- Symlinks ++ RegularFiles ],

	% Finally removing this (now empty) directory as well:
	remove_empty_directory( DirectoryName ).



% Copies a specified file to a given destination filename (not a directory name,
% see copy_file_in/2 for that), overwriting any previous file.
%
% Note: content is copied and permissions are preserved (ex: the copy of an
% executable file will be itself executable, other permissions as well, unlike
% /bin/cp which relies on umask).
%
-spec copy_file( file_name(), file_name() ) -> void().
copy_file( SourceFilename, DestinationFilename ) ->

	case try_copy_file( SourceFilename, DestinationFilename ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { copy_file_failed, SourceFilename, Reason } )

	end.



% Copies a specified file to a given destination filename (not a directory name,
% see copy_file_in/2 for that), overwriting any previous file.
%
% Note: content is copied and permissions are preserved (ex: the copy of an
% executable file will be itself executable, other permissions as well, unlike
% /bin/cp that relies on umask).
%
-spec try_copy_file( file_name(), file_name() ) -> basic_utils:base_status().
try_copy_file( SourceFilename, DestinationFilename ) ->

	% First, checks the source file exists and retrieves its meta-information:
	case file:read_file_info( SourceFilename ) of

		{ ok, #file_info{ mode=Mode } } ->

			case file:copy( SourceFilename, DestinationFilename ) of

				{ ok, _ByteCount } ->
					% Now sets the permissions of the copy:
					case file:change_mode( DestinationFilename, Mode ) of

						ok ->
							ok;

						ChgModeError ->
							ChgModeError

					end;

				CopyError ->
					CopyError

			end;

		ReadError ->
			ReadError

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



% Copies a specified file to a given destination iff this source file is already
% existing.
%
% Note: content is copied and permissions are preserved (ex: the copy of an
% executable file will be itself executable).
%
-spec copy_file_if_existing( file_name(), file_name() ) -> void().
copy_file_if_existing( SourceFilename, DestinationFilename ) ->

	case is_existing_file( SourceFilename ) of

		true ->
			copy_file( SourceFilename, DestinationFilename );

		false ->
			ok

	end.



% Copies specified source tree in specified target directory.
-spec copy_tree( directory_path(), directory_path() ) -> void().
copy_tree( SourceTreePath, TargetDirectory ) ->

	case file_utils:is_existing_directory_or_link( SourceTreePath ) of

		true ->
			ok;

		false ->
			throw( { non_existing_source_tree, SourceTreePath } )

	end,

	case file_utils:is_existing_directory_or_link( TargetDirectory ) of

		true ->
			ok;

		false ->
			throw( { non_existing_target_directory, TargetDirectory } )

	end,

	Cmd = text_utils:format( "/bin/cp -r '~s' '~s'",
							 [ SourceTreePath, TargetDirectory ] ),

	case system_utils:run_executable( Cmd ) of

		{ _ExitCode=0, _Output=[] } ->
			ok;

		{ ExitCode, ErrorOutput } ->
			throw( { copy_tree_failed, { SourceTreePath, TargetDirectory },
					 ExitCode, ErrorOutput } )

	end.


% Renames specified file.
%
% Returns, for convenience, the new name.
%
-spec rename( file_name(), file_name() ) -> file_name().
rename( SourceFilename, DestinationFilename ) ->
	move_file( SourceFilename, DestinationFilename ).



% Moves specified file so that it is now designated by specified filename.
%
% Returns, for convenience, the new name.
%
-spec move_file( file_name(), file_name() ) -> file_name().
move_file( SourceFilename, DestinationFilename ) ->

	%trace_utils:warning_fmt( "## Moving file '~s' to '~s'.",
	%						  [ SourceFilename, DestinationFilename ] ),

	%copy_file( SourceFilename, DestinationFilename ),
	%remove_file( SourceFilename ).

	% Simpler, better, yet does not works across filesystems:
	case file:rename( SourceFilename, DestinationFilename ) of

		ok ->
			DestinationFilename;

		{ error, exdev } ->
			%trace_utils:trace_fmt( "Moving across filesystems '~s' to '~s'.",
			%					   [ SourceFilename, DestinationFilename ] ),
			copy_file( SourceFilename, DestinationFilename ),
			remove_file( SourceFilename );

		Error ->
			throw( { move_file_failed, Error, SourceFilename,
					 DestinationFilename } )

	end.



% Creates a symbolic link pointing to specified target path, bearing specified
% (link) name.
%
-spec create_link( path(), link_name() ) -> void().
create_link( TargetPath, LinkName ) ->

	%trace_utils:debug_fmt( "Creating a link '~s' to '~s', while in '~s'.",
	%					   [ LinkName, TargetPath, get_current_directory() ] ),

	case file:make_symlink( TargetPath, LinkName ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { link_creation_failed, { target, TargetPath },
					 { link, LinkName }, Reason } )

	end.



% Returns a path deriving from specified one so that it does not clash with any
% pre-existing entry.
%
-spec get_non_clashing_entry_name_from( path() ) -> path().
get_non_clashing_entry_name_from( Path ) ->

	% Ex:
	% - if "aaa/bbb/foobar.txt" is specified, returns "aaa/bbb/foobar.txt-1"
	% - if "aaa/bbb/foobar.txt-4" is specified, returns "aaa/bbb/foobar.txt-5"

	case exists( Path ) of

		true ->
			case string:split( Path, _SearchPattern="-", _Where=trailing ) of

				[ _Path ] ->
					text_utils:format( "~s-1", [ Path ] );

				[ BasePath, FinalPart ] ->
					case text_utils:try_string_to_integer( FinalPart ) of

						undefined ->
							text_utils:format( "~s-1", [ Path ] );

						Count ->
							TestedPath = text_utils:format( "~s-~B",
														[ BasePath, Count+1 ] ),

							% As clashes may happen for any name:
							get_non_clashing_entry_name_from( TestedPath )


				end

			end;

		false ->
			Path

	end.



% Appends, at the end of the first specified file, the content of the second
% specified one: concatenates the second with the first one.
%
-spec append_file( file_name(), file_name() ) -> void().
append_file( TargetFilename, ToAppendFilename ) ->

	ToAppendBin = read_whole( ToAppendFilename ),

	% Test needed, otherwise the next append could create from scratch the
	% target file (would be a masked failure):
	%
	case is_existing_file_or_link( TargetFilename ) of

		true ->
			TargetFile = open( TargetFilename, _Opts=[ append ] ),

			write( TargetFile, ToAppendBin ),

			close( TargetFile );

		false ->
			throw( { append_target_file_not_found, TargetFilename } )

	end.



% Returns the low-level permission associated to specified one.
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



% Changes the permissions ("chmod") of specified filesystem element.
-spec change_permissions( any_path(), permission() | [ permission() ] ) ->
								void().
change_permissions( Path, NewPermissions ) ->

	ActualPerms = get_permission_for( NewPermissions ),

	case file:change_mode( Path, ActualPerms ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { change_permission_failed, Reason, Path, NewPermissions } )

	end.



% Tells whether the specified path is an absolute one.
%
% A path is deemed absolute iff it starts with "/".
%
-spec is_absolute_path( path() ) -> boolean().
is_absolute_path( _Path=[ $/ | _Rest ] ) ->
	true;

% Not wanting to let for example atoms slip through:
is_absolute_path( Path ) when is_list( Path )->
	false.



% Returns an absolute, normalised path corresponding to specified path.
%
% If it is not already absolute, it will made so by using the current working
% directory.
%
-spec ensure_path_is_absolute( path() ) -> path();
							 ( bin_path() ) -> bin_path().
ensure_path_is_absolute( Path ) when is_list( Path ) ->

	AbsPath = case is_absolute_path( Path ) of

		true ->
			% Already absolute:
			Path;

		false ->
			% Relative, using current directory as base:
			join( get_current_directory(), Path )

	end,

	normalise_path( AbsPath );

ensure_path_is_absolute( BinPath ) ->
	Path = text_utils:binary_to_string( BinPath ),
	text_utils:string_to_binary( ensure_path_is_absolute( Path ) ).




% Returns an absolute, normalised path corresponding to the specified target
% path, using base path as root directory (this must be an absolute path).
%
% Ex: ensure_path_is_absolute( "tmp/foo", "/home/dalton" ) will return
% "/home/dalton/tmp/foo".
%
-spec ensure_path_is_absolute( path(), path() ) -> path();
							 ( bin_path(), bin_path() ) -> bin_path().
ensure_path_is_absolute( TargetPath, BasePath ) when is_list( TargetPath ) ->

	case is_absolute_path( TargetPath ) of

		true ->
			% Already absolute:
			normalise_path( TargetPath );

		false ->
			PlainBasePath = case is_list( BasePath ) of

				true ->
					BasePath;

				false ->
					text_utils:binary_to_string( BasePath )

			end,

			% Relative, using specified base directory:
			case is_absolute_path( PlainBasePath ) of

				true ->
					normalise_path( join( PlainBasePath, TargetPath ) );

				false ->
					throw( { base_path_not_absolute, PlainBasePath } )
			end

	end;

% Here at least TargetPath is a binary:
ensure_path_is_absolute( BinTargetPath, BasePath ) ->
	PlainTargetPath = text_utils:binary_to_string( BinTargetPath ),
	AbsPath = ensure_path_is_absolute( PlainTargetPath, BasePath ),
	text_utils:string_to_binary( AbsPath ).




% Normalises specified path (canonicalises it), by translating it so that no
% intermediate, superfluous '.' or '..' is present afterwards.
%
% For example, "/home/garfield/../lisa/./src/.././tube" shall be normalised in
% "/home/lisa/tube".
%
-spec normalise_path( path() ) -> path();
					( bin_path() ) -> bin_path().
normalise_path( _Path="." ) ->
	".";
	%get_current_directory();

normalise_path( Path ) when is_list( Path ) ->

	%trace_utils:debug_fmt( "Normalising path '~s'.", [ Path ] ),

	ElemList = filename:split( Path ),

	%trace_utils:debug_fmt( "ElemList: ~p", [ ElemList ] ),

	join( filter_elems( ElemList, _Acc=[] ) );

normalise_path( BinPath ) when is_binary( BinPath ) ->

	Path = text_utils:binary_to_string( BinPath ),

	text_utils:string_to_binary( normalise_path( Path ) ).



% (helper)
filter_elems( _ElemList=[], Acc ) ->
	lists:reverse( Acc );

filter_elems( _ElemList=[ "." | T ], Acc ) ->
	filter_elems( T, Acc );

% We can remove one level iff there is at least one:
filter_elems( _ElemList=[ ".." | T ], _Acc=[ _ | AccT ] ) ->
	filter_elems( T, AccT );

% No level left, so this ".." should not be filtered out:
%
% (however this clause is a special case of the next, hence can be commented
% out)
%
%filter_elems( _ElemList=[ PathElement=".." | T ], Acc ) ->
%	filter_elems( T, [ PathElement | Acc ] );

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





% Returns a version of the specified path that is relative to the current
% directory.
%
-spec make_relative( path() ) -> directory_path().
make_relative( Path ) ->
	make_relative( Path, _RefDir=get_current_directory() ).



% Returns a version of the first specified path that is relative to the
% specified second reference directory.
%
-spec make_relative( any_path(), directory_path() ) -> directory_path().
make_relative( Path, RefDir ) when is_list( Path ) andalso is_list( RefDir ) ->

	AbsPath = ensure_path_is_absolute( Path ),

	AbsRefDir = ensure_path_is_absolute( RefDir ),

	%trace_utils:debug_fmt( "Making path '~s' (absolute form: '~s') relative "
	%					   "to reference directory '~s' (absolute form: '~s').",
	%					   [ Path, AbsPath, RefDir, AbsRefDir ] ),

	TargetPathElems = filename:split( AbsPath ),
	RefPathElems = filename:split( AbsRefDir ),

	make_relative_helper( TargetPathElems, RefPathElems );


make_relative( BinPath, RefDir ) when is_binary( BinPath ) ->
	make_relative( text_utils:binary_to_string( BinPath ), RefDir );

make_relative( Path, RefDir ) ->
	throw( { invalid_parameters, Path, RefDir } ).



% First, drop any common path prefix:
make_relative_helper( [ E | TPathElems ], [ E | TRefPathElems ] ) ->
	make_relative_helper( TPathElems, TRefPathElems );

% Found first non-matching directory element:
make_relative_helper( PathElems, RefPathElems ) ->

	%trace_utils:debug_fmt( "Paths split at : ~p vs ~p.",
	%					   [ PathElems, RefPathElems ] ),

	FromRef = [ ".." || _ <- lists:seq( 1, length( RefPathElems ) ) ],

	Res = join( FromRef ++ PathElems ),

	%trace_utils:debug_fmt( "Returned path: '~s'.", [ Res ] ),

	Res.



% Returns a pair made of the longest path common to all specified directory
% paths, and the corresponding suffixes, i.e. an (underordered) list of the
% input paths (as lists of path elements) once the common prefix elements have
% been removed.
%
% Like text_utils:get_longest_common_prefix/1, except that operates on path
% elements, not characters.
%
-spec get_longest_common_path( [ directory_path() ] ) ->
					 { directory_path(), [ directory_path() ] }.
get_longest_common_path( DirPaths ) ->
	DirElems = [ filename:split( D ) || D <- DirPaths ],
	get_longest_common_path_helper( DirElems, _AccCommon=[] ).


% (helper)
get_longest_common_path_helper( DirElems, AccCommon ) ->

	%trace_utils:debug_fmt( "get_longest_common_path_helper from ~p "
	%						"(acc being ~p).", [ DirElems, AccCommon ] ),

	case get_common_head_of( DirElems, _Acc=[] ) of

		{ none, Tails } ->
			%trace_utils:debug_fmt( "Finished, with common path ~s and "
			%                       "tails: ~p.", [ AccCommon, Tails ] ),
			{ join( lists:reverse( AccCommon ) ), Tails };

		{ Elem, DirElemsTails } ->
			get_longest_common_path_helper( DirElemsTails,
											[ Elem | AccCommon ] )

	end.



% (sub-helper)
get_common_head_of( _DirElemsTails=[], Acc ) ->
	{ none, Acc };

% We use the head (if any) of the first list as the one to check in the head of
% all others:
%
get_common_head_of( _DirElemsTails=[ _First=[] | _Others ], Acc ) ->
	{ none, Acc };

get_common_head_of( _DirElemsTails=[ _First=[ Elem | T ] | Others ], Acc ) ->
	case try_behead_with( Elem, Others ) of

		non_matching ->
			{ none, Acc };

		NewOthers ->
			{ Elem, [ T | NewOthers ] }

	end.


% (sub-sub-helper)
try_behead_with( Elem, Others ) ->
	%trace_utils:debug_fmt( "Beheading of ~p from '~s'", [ Others, Elem ] ),
	try_behead_with( Elem, Others, _Acc=[] ).


% Others depleted:
try_behead_with( _Elem, _Others=[], Acc ) ->
	 Acc;

% A good Other:
try_behead_with( Elem, _Others=[ [ Elem | R ] | T ], Acc ) ->
	try_behead_with( Elem, T, [ R | Acc ] );

% A bad Other:
% Corresponds to: try_behead_with( Elem, Others=[ [ OtherElem | _R ] | _T ], _Acc ) ->
% or to:          try_behead_with( Elem, Others=[ [] | _T ], _Acc ) ->
try_behead_with( _Elem, _Others, _Acc ) ->
	%trace_utils:debug_fmt( "'~s' could not be removed from ~p", [ Elem, Others ] ),
	non_matching.




% Tells whether specified basename (ex: a pathless filename) is among the
% specified list of full paths; returns either false or the first full path
% found corresponding to that leaf element.
%
% Ex:
%  false = file_utils:is_leaf_among( "xx", [ "a/b/c/yy", "d/e/zz" ] )
%  "a/b/c/xx"  = file_utils:is_leaf_among( "xx", [ "a/b/c/xx", "d/e/zz" ] )
%
-spec is_leaf_among( leaf_name(), [ path() ] ) -> { 'false' | path() }.
is_leaf_among( _LeafName, _PathList=[] ) ->
	false;

is_leaf_among( LeafName, _PathList=[ Path | T ] ) ->

	case filename:basename( Path ) of

		LeafName ->
			Path;

		_  ->
			is_leaf_among( LeafName, T )

	end.



% Updates specified file with specified keywords, i.e. copies the original file
% into a target, updated one (supposedly non-already existing), in which all the
% specified keywords (the keys of the translation table) are replaced with their
% associated value (i.e. the value in table corresponding to that key).
%
% Ex: file_utils:update_with_keywords( "original.txt", "updated.txt", table:new(
%  [ { "hello", "goodbye" }, { "Blue", "Red" } ] ).
%
-spec update_with_keywords( any_file_path(), any_file_path(),
							text_utils:translation_table() ) -> void().
update_with_keywords( OriginalFilePath, TargetFilePath, TranslationTable ) ->

	case exists( TargetFilePath ) of

		true ->
			throw( { already_existing, TargetFilePath } );

		false ->
			ok

	end,

	BinOrigContent = read_whole( OriginalFilePath ),

	BinUpdatedContent = text_utils:update_with_keywords( BinOrigContent,
														 TranslationTable ),

	write_whole( TargetFilePath, BinUpdatedContent ).



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


% (helper)
% Removes any leading './'.
-spec path_to_variable_name( path(), string() ) -> string().
path_to_variable_name( [ $.,$/ | T ], Prefix ) ->
	convert( T, Prefix );

path_to_variable_name( Filename, Prefix ) ->
	convert( Filename, Prefix ).



% (helper)
convert( Filename, Prefix ) ->

	NoDashName = re:replace( lists:flatten( Filename ), "-+", "_",
		[ global, { return, list } ] ),

	NoDotName = re:replace( NoDashName, "\\.+", "_",
		[ global, { return, list } ] ),

	Prefix ++ re:replace( NoDotName, "/+", "_",
		[ global, { return, list } ] ).



% Removes all upper levels of a path (absolute or not), as well as the extension
% of the resulting file name.
%
% Ex: "foobar" =
%           file_utils:remove_upper_levels_and_extension( "aa/bb/foobar.txt" ).
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




% Returns a list of the known file extensions that refer to image files.
-spec get_image_extensions() -> [ extension() ].
get_image_extensions() ->
	% TIFF, TGA and al deemed deprecated:
	[ ".png", ".jpg", ".jpeg", ".bmp" ].



-define(ResourceDir,"resources").


% Returns the image path corresponding to the specified file.
-spec get_image_file_png( file_name() ) -> path().
get_image_file_png( Image ) ->
  filename:join( [ ?ResourceDir, "images", Image ++ ".png" ] ).



% Returns the image path corresponding to the specified file.
-spec get_image_file_gif( file_name() ) -> path().
get_image_file_gif( Image ) ->
  filename:join( [ ?ResourceDir, "images", Image ++ ".gif" ] ).




% I/O section.


% Opens the file corresponding to the specified filename, with specified list of
% options (as listed for file:open/2 in
% http://erlang.org/doc/man/file.html#open-2, i.e. read, write, append,
% exclusive, raw, etc.).
%
% Note that using 'raw' may cause problems with encodings.
%
% See read_terms/1 if planning to read that content as terms later, notably with
% regard to encoding.
%
% Returns the file reference, or throws an exception.
%
% Will attempt to open the specified file only once, as looping endlessly does
% not seem a viable solution right now (risk of exhausting the descriptors,
% making the VM fail for example when loading a new BEAM).
%
% Note: if an opened file fails to be correctly read encoding-wise (characters
% like 'à' being not only displayed but also read garbled, and if setting
% {encoding,unicode} returns an error such as
% {read_error,{no_translation,unicode,unicode}}, then this may be an
% (unfortunate) side-effect of having run the VM with the -noinput option; in
% this case, the best option is to execute once, preferably early (ex: as first
% statement) system_utils:force_unicode_support/0.
%
-spec open( any_file_name(), [ file_open_mode() ] ) -> file().
open( Filename, Options ) ->
	open( Filename, Options, _Default=try_once ).



% Opens the file corresponding to specified filename (first parameter) with
% specified list of options (second parameter; refer to file:open/2 for detailed
% documentation, see http://erlang.org/doc/man/file.html#open-2).
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
% Note: if an opened file fails to be correctly read encoding-wise (characters
% like 'à' being not only displayed but also read garbled, and if setting
% {encoding,unicode} returns an error such as
% {read_error,{no_translation,unicode,unicode}}, then this may be an
% (unfortunate) side-effect of having run the VM with the -noinput option; in
% this case, the best option is to execute once, preferably early (ex: as first
% statement) system_utils:force_unicode_support/0.
%
-spec open( any_file_name(), [ file_open_mode() ],
		   'try_once' | 'try_endlessly' | 'try_endlessly_safer' ) -> file().
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
-spec close( file() ) -> void().
close( File ) ->
	close( File, throw_if_failed ).



% Closes specified file reference.
%
% Throws an exception on failure or not, depending on specified failure mode.
%
-spec close( file(), 'overcome_failure' | 'throw_if_failed' ) ->
				   void().
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
% Operates on files opened in raw mode (only way to do so), or not (works for
% normal mode as well).
%
% Throws an exception on failure.
%
-spec write( file(), iodata() ) -> void().
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
-spec write( file(), text_utils:format_string(), [ term() ] ) -> void().
write( File, FormatString, Values ) ->

	Text = text_utils:format( FormatString, Values ),

	case file:write( File, Text ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { write_failed, Reason } )

	end.



% Reads the content of the specified file, based on its filename specified as
% any kind of string (plain, binary, atom, etc.), and returns the corresponding
% binary, or throws an exception on failure.
%
% See also: read_terms/1 to read directly Erlang terms.
%
-spec read_whole( any_file_name() ) -> binary().
read_whole( Filename ) ->

	%trace_utils:debug_fmt( "Reading as a whole '~s'.", [ Filename ] ),

	case file:read_file( Filename ) of

		{ ok, Binary } ->
			Binary;

		{ error, Error } ->
			throw( { read_whole_failed, Filename, Error } )

	end.



% Writes the specified content in specified file, whose filename is specified as
% any kind of string.
%
% Throws an exception on failure.
%
-spec write_whole( any_file_name(), string() | binary() ) -> void().
write_whole( Filename, StringContent ) when is_list( StringContent ) ->
	write_whole( Filename, text_utils:string_to_binary( StringContent ) );

write_whole( Filename, BinaryContent ) ->

	%trace_utils:debug_fmt( "Writing to '~s' following content:~n~s",
	%					   [ Filename, BinaryContent ] ),

	case file:write_file( Filename, BinaryContent ) of

		ok ->
			% Useless, paranoid checking:
			%case is_existing_file( Filename ) of
			%
			%	true ->
			%		trace_utils:debug_fmt( "'~s' written as a whole.",
			%							   [ Filename ] ),
			%		ok;
			%
			%	false ->
			%		throw( { write_whole_failed, Filename, no_file } )
			%
			%end;
			ok;

		{ error, Error } ->
			throw( { write_whole_failed, Filename, Error } )

	end.



% Reads specified file, tries to parse a list of terms from it, and returns it.
%
% If expecting to read UTF-8 content for a file, it should:
%
%  - have been opened for writing typically while including the { encoding, utf8
%  } option
%
%  - start with a '%% -*- coding: utf-8 -*-' header
%
% Throws an exception on error.
%
-spec read_terms( file_path() ) -> [ term() ].
read_terms( Filename ) ->

	case file:consult( Filename ) of

		{ ok, Terms } ->
			Terms;

		{ error, Error } when is_atom( Error ) ->
			throw( { reading_failed, Filename, Error } );

		{ error, Error={ Line, Module, Term } } ->
			Reason = file:format_error( Error ),
			throw( { interpretation_failed, Filename, { line, Line },
					 { module, Module }, { term, Term }, Reason } )

	end.



% Writes specified terms into specified file, with no specific header or footer.
%
% Heavily inspired from Joe Armstrong's lib_misc:unconsult/2.
%
-spec write_terms( [ term() ], file_path() ) -> void().
write_terms( Terms, Filename ) ->
	write_terms( Terms, _Header=undefined, _Footer=undefined, Filename ).



% Writes specified terms into specified file, with specified header and footer.
%
% Heavily inspired from Joe Armstrong's lib_misc:unconsult/2.
%
-spec write_terms( [ term() ], maybe( string() ), maybe( string() ),
				   file_path() ) -> void().
write_terms( Terms, Header, Footer, Filename ) ->

	F = open( Filename, [ write, raw, delayed_write ] ),

	case Header of

		undefined ->
			ok;

		_ ->
			write( F, text_utils:format( "% ~n~n~n", [ Header ] ) )

	end,

	write_direct_terms( Terms, F ),

	case Footer of

		undefined ->
			ok;

		_ ->
			write( F, text_utils:format( "~n~n% ~s~n", [ Footer ] ) )

	end,

	close( F ).



% Writes directly specified terms into specified already opened file.

% Heavily inspired from Joe Armstrong's lib_misc:unconsult/2.
%
-spec write_direct_terms( file(), [ term() ] ) -> void().
write_direct_terms( File, Terms ) ->
	[ write( File, text_utils:format( "~p.~n", [ T ] ) ) || T <- Terms ].



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



% Compresses specified file: creates a new, compressed version thereof (using
% the most efficient, compacity-wise, compression tool available), whose
% filename, established based on usual conventions, is returned. If a file with
% that name already exists, it will be overwritten.
%
% For example, compress( "hello.png" ) will generate a "hello.png.xz"
% file.
%
% The original file remain as is.
%
% Note: this function just takes care of compressing a single file, even if some
% compressors (ex: zip) include features to create an archive of multiple files
% first.
%
-spec compress( file_name() ) -> file_name().
compress( Filename ) ->
	compress( Filename, _CompressionFormat=xz ).



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
% corresponding to the implicit, most compact format: recreates the original,
% decompressed version thereof, whose filename, established based on usual
% conventions, is returned: the name of the input file without its extension.
%
% This function works in pair with compress/2, and as such expects that each
% compressed file contains exactly one file, bear the same filename except the
% compressor extension.
%
% Typically, when a format MY_FORMAT is specified, converts a compressed file
% name foo.extension_of(MY_FORMAT) into an uncompressed version of it named
% 'foo'.
%
% So, for example, decompress( "foo.xz" ) will generate a "foo" file.
%
% If a file with that name already exists, it will be overwritten.
%
% The compressed file remains as is.
%
-spec decompress( file_name() ) -> file_name().
decompress( Filename ) ->
	decompress( Filename, _CompressionFormat=xz ).



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
-spec zipped_term_to_unzipped_file( binary(), file_name() ) -> void().
zipped_term_to_unzipped_file( ZippedTerm, TargetFilename ) ->

	{ ok, [ { _AFilename, Binary } ] } = zip:unzip( ZippedTerm, [ memory ] ),

	% { ok, File } = file:open( TargetFilename, [ write ] ),
	% ok = io:format( File, "~s", [ binary_to_list(Binary) ] ),
	% ok = file:write_file( File, "~s", [ binary_to_list(Binary) ] ),
	% ok = file:close( File ).
	write_whole( TargetFilename, Binary ).



% Reads in memory the files specified from their filenames (as plain strings),
% zips the corresponding term, and returns it.
%
% Note: useful for network transfers of small files.
%
% Larger ones should be transferred with TCP/IP and by chunks.
%
% Returns a binary.
%
-spec files_to_zipped_term( [ file_name() ] ) -> binary().
files_to_zipped_term( FilenameList ) ->

	DummyFileName = "dummy",

	{ ok, { _DummyFileName, Bin } } =
		zip:zip( DummyFileName, FilenameList, [ memory ] ),

	Bin.



% Reads in memory the files specified from their filenames (as plain strings),
% assuming their path is relative to the specified base directory, zips the
% corresponding term, and returns it.
%
% Note: useful for network transfers of small files.
%
% Larger ones should be transferred with TCP/IP and by chunks.
%
% Returns a binary.
%
-spec files_to_zipped_term( [ file_name() ], any_directory_name() ) -> binary().
files_to_zipped_term( FilenameList, BaseDirectory ) ->

	DummyFileName = "dummy",

	%trace_utils:info_fmt( "files_to_zipped_term operating, from '~s', "
	%					  "on following ~B file(s): ~s",
	%					  [ BaseDirectory, length( FilenameList ),
	%						text_utils:terms_to_string( FilenameList ) ] ),

	 case zip:zip( DummyFileName, FilenameList,
				   [ memory, { cwd, BaseDirectory } ] ) of

		 { ok, { _DummyFileName, Bin } } ->
			 Bin;


		 { error, enoent } ->

			 % Such a short error might be difficult to diagnose:

			 %trace_utils:warning_fmt( "files_to_zipped_term/2 failed "
			 %  "from '~s':~n~n - directory '~p' exists? ~p",
			 %		[ get_current_directory(), BaseDirectory,
			 %		  is_existing_directory( BaseDirectory ) ] ),

			 % [ trace_utils:warning_fmt( "~n - file '~p' exists? ~p", [ F,
			 %	   is_existing_file( F ) ] ) || F <- FilenameList ],

			 throw( { zip_failed, BaseDirectory, FilenameList } );

		 % einval might mean for example that at least some filenames are
		 % binaries rather that plain strings:
		 %
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
