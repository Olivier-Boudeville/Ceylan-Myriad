% Copyright (C) 2003-2013 Olivier Boudeville
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
% See file_utils_test.erl for the corresponding test.
-module(file_utils).


% Related standard modules: file, filename.



% Filename-related operations.
-export([ join/1, join/2, convert_to_filename/1, replace_extension/3, exists/1,
	get_type_of/1, is_file/1, is_existing_file/1,
	is_directory/1, is_existing_directory/1, list_dir_elements/1,
	get_current_directory/0,
	filter_by_extension/2,
	filter_by_included_suffixes/2, filter_by_excluded_suffixes/2,
	find_files_from/1, find_files_with_extension_from/2,
	find_files_with_excluded_dirs/2,
	find_files_with_excluded_suffixes/2,
	find_files_with_excluded_dirs_and_suffixes/3,
	find_directories_from/1,
	create_directory/1, create_directory/2,
	remove_file/1, remove_file_if_existing/1,
	remove_files/1, remove_files_if_existing/1,
	path_to_variable_name/1, path_to_variable_name/2,
	get_image_file_png/1, get_image_file_gif/1 ]).


% I/O section.
-export([ open/2, open/3, read_whole/1, write_whole/2 ]).


% Zip-related operations.
-export([ file_to_zipped_term/1, zipped_term_to_unzipped_file/1,
	zipped_term_to_unzipped_file/2, files_to_zipped_term/1,
	zipped_term_to_unzipped_files/1, zipped_term_to_unzipped_files/2 ]).



% For the file_info record:
-include_lib("kernel/include/file.hrl").




% Filename-related operations.


% Joins the specified list of path elements.
%
% Note: join/1 added back to file_utils, filename:join(Components) can be used
% instead.
%
% However filename:join(["","my_dir"]) results in "/my_dir", whereas often we
% would want "my_dir", which is returned by file_utils:join/1.
%
join( [""|T] ) ->
	filename:join( T );

join( ComponentList ) ->
	filename:join( ComponentList ).


% Joins the two specified path elements.
%
% Note: join/2 added back to file_utils, filename:join(Name1, Name2) can be used
% instead.
%
% However filename:join("","my_dir") results in "/my_dir", whereas often we
% would want "my_dir", which is returned by file_utils:join/2.
%
join( "", SecondPath ) ->
	SecondPath ;

join( FirstPath, SecondPath ) ->
	filename:join( FirstPath, SecondPath ).





% Converts specified name to an acceptable filename, filesystem-wise.
convert_to_filename(Name) ->

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
	re:replace( lists:flatten(Name),
			   "( |<|>|,|\\(|\\)|'|\"|/|\\\\|\&|~|"
			   "#|@|{|}|\\[|\\]|\\||\\$|\\*|\\?|!|\\+|;|:)+", "_",
		 [global,{return, list}] ).



% Returns a new filename whose extension has been updated.
%
% Ex: replace_extension( "/home/jack/rosie.ttf", ".ttf", ".wav" ) should return
% "/home/jack/rosie.wav".
replace_extension( Filename, SourceExtension, TargetExtension ) ->
	Index = string:rstr( Filename, SourceExtension ),
	string:substr( Filename, 1, Index-1 ) ++ TargetExtension.



% Tells whether specified file entry exists, regardless of its type.
exists(EntryName) ->
	case file:read_file_info(EntryName) of

		{ok,_FileInfo} ->
			true;

		{error,_Reason} ->
			false

	end.



% Returns the type of the specified file entry, in:
% device | directory | regular | other.
get_type_of(EntryName) ->
	case file:read_file_info(EntryName) of

		{ok,FileInfo} ->
			#file_info{ type = FileType } = FileInfo,
			FileType;

		{error,eloop} ->
			% Probably a recursive symlink:
			throw({too_many_symlink_levels,EntryName});

		{error,enoent} ->
			throw({non_existing_entry,EntryName})

	end.



% Returns whether the specified entry, supposedly existing, is a regular file.
%
% If the specified entry happens not to exist, a
% '{non_existing_entry,EntryName}' exception will be thrown.
%
is_file(EntryName) ->
	case get_type_of(EntryName) of

		regular ->
			true ;

		_ ->
			false

	end.


% Returns whether the specified entry exists and is a regular file.
%
% Returns true or false, and cannot trigger an exception.
is_existing_file(EntryName) ->
	case exists(EntryName) andalso get_type_of(EntryName) of

		regular ->
			true ;

		_ ->
			false

	end.


% Returns whether the specified entry, supposedly existing, is a directory.
%
% If the specified entry happens not to exist, a
% '{non_existing_entry,EntryName}' exception will be thrown.
%
is_directory(EntryName) ->
	case get_type_of(EntryName) of

		directory ->
			true ;

		_ ->
			false

	end.


% Returns whether the specified entry exists and is a directory.
%
% Returns true or false, and cannot trigger an exception.
is_existing_directory(EntryName) ->
	case exists(EntryName) andalso get_type_of(EntryName) of

		directory ->
			true ;

		_ ->
			false

	end.



% Returns a tuple made of a four lists describing the file elements found in
% specified directory: {RegularFiles,Directories,OtherFiles,Devices}.
list_dir_elements(Dirname) ->
	%io:format( "list_dir_elements for '~s'.~n", [Dirname] ),
	{ok,LocalDirElements} = file:list_dir(Dirname),
	classify_dir_elements( Dirname, LocalDirElements, [], [], [], [] ).



% Returns the current directory, as a plain string.
%
% Throws an exception on failure.
%
get_current_directory() ->

	case file:get_cwd() of

		{ok,Dir} ->
			Dir;

		{error,Reason} ->
			throw( {failed_to_determine_current_directory,Reason} )

	end.



% Returns a tuple containing four lists corresponding to the sorting of all
% file elements: {Directories,RegularFiles,Devices,OtherFiles}.
classify_dir_elements( _Dirname, [],
		Devices, Directories, RegularFiles, OtherFiles ) ->
	% Note the reordering:
	{RegularFiles,Directories,OtherFiles,Devices};

classify_dir_elements( Dirname, [H|T],
		Devices, Directories, RegularFiles, OtherFiles ) ->

	 case get_type_of( filename:join( Dirname, H ) ) of

		device ->
			classify_dir_elements( Dirname, T,
				[H|Devices], Directories, RegularFiles, OtherFiles ) ;

		directory ->
			classify_dir_elements( Dirname, T,
				Devices, [H|Directories], RegularFiles, OtherFiles ) ;

		regular ->
			classify_dir_elements( Dirname, T,
				Devices, Directories, [H|RegularFiles], OtherFiles ) ;

		other ->
			classify_dir_elements( Dirname, T,
				Devices, Directories, RegularFiles, [H|OtherFiles] )

	end.



% Returns a list containing all elements of Filenames list whose extension is
% the specified one.
filter_by_extension( Filenames, Extension ) ->
	filter_by_extension( Filenames, Extension, [] ).


filter_by_extension( [], _Extension, Acc ) ->
	Acc ;

filter_by_extension( [H|T], Extension, Acc ) ->
	case filename:extension(H) of

		Extension ->
			filter_by_extension( T, Extension, [H|Acc] ) ;

		_Other ->
			filter_by_extension( T, Extension, Acc )

	end.



% Returns a list containing all elements of the Filenames list which match any
% of the specified suffixes.
filter_by_included_suffixes( Filenames, IncludedSuffixes ) ->
	[ F || F <- Filenames, has_matching_suffix( F, IncludedSuffixes )].


% Returns a list containing all elements of the Filenames list which do not
% match any of the specified suffixes.
filter_by_excluded_suffixes( Filenames, ExcludedSuffixes ) ->
	[ F || F <- Filenames, not has_matching_suffix( F, ExcludedSuffixes )].



has_matching_suffix( _Filename, _ExcludedSuffixes=[] ) ->
	false;

has_matching_suffix( Filename, [S|OtherS] ) ->

	% We have to avoid feeding string:substr/2 with a start position that is not
	% strictly positive, otherwise we would have a function clause:

	LenFile = length(Filename),
	LenSuffix = length(S),

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
find_files_from( RootDir ) ->
	find_files_from( RootDir, _CurrentRelativeDir="", _Acc=[] ).


find_files_from( RootDir, CurrentRelativeDir, Acc ) ->

	%io:format( "find_files_from with root = '~s', current = '~s'.~n",
	%	[RootDir,CurrentRelativeDir] ),

	{RegularFiles,Directories,_OtherFiles,_Devices} = list_dir_elements(
		join( RootDir, CurrentRelativeDir ) ),

	Acc ++ list_files_in_subdirs( Directories,
			RootDir, CurrentRelativeDir, [] )
		++ prefix_files_with( CurrentRelativeDir, RegularFiles ).


% Specific helper for find_files_from/3 above:
list_files_in_subdirs( _Dirs=[], _RootDir, _CurrentRelativeDir, Acc ) ->
	Acc;

list_files_in_subdirs( _Dirs=[H|T], RootDir, CurrentRelativeDir, Acc ) ->

	%io:format( "list_files_in_subdirs with root = '~s', current = '~s' "
	%	"and H='~s'.~n", [RootDir,CurrentRelativeDir,H] ),

	list_files_in_subdirs( T, RootDir, CurrentRelativeDir,
		find_files_from( RootDir, join(CurrentRelativeDir,H), [] ) ++ Acc ).




% Returns the list of all regular files found from the root with specified
% extension, in the whole subtree (i.e. recursively).
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
find_files_with_extension_from( RootDir, Extension ) ->
	find_files_with_extension_from( RootDir, "", Extension, [] ).


find_files_with_extension_from( RootDir, CurrentRelativeDir, Extension, Acc ) ->

	%io:format( "find_files_from in ~s.~n", [CurrentRelativeDir] ),

	{RegularFiles,Directories,_OtherFiles,_Devices} = list_dir_elements(
		join(RootDir,CurrentRelativeDir) ),

	Acc ++ list_files_in_subdirs_with_extension( Directories, Extension,
			RootDir, CurrentRelativeDir, [] )
		++ prefix_files_with( CurrentRelativeDir,
			filter_by_extension(RegularFiles,Extension) ).


% Helper for find_files_with_extension_from/4:
list_files_in_subdirs_with_extension( [], _Extension, _RootDir,
									  _CurrentRelativeDir, Acc) ->
	Acc;

list_files_in_subdirs_with_extension( [H|T], Extension, RootDir,
									  CurrentRelativeDir, Acc ) ->
	list_files_in_subdirs_with_extension( T, Extension, RootDir,
		CurrentRelativeDir,
		find_files_with_extension_from( RootDir, join(CurrentRelativeDir,H),
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
find_files_with_excluded_dirs( RootDir, ExcludedDirList ) ->
	find_files_with_excluded_dirs( RootDir, _CurrentRelativeDir="",
								  ExcludedDirList, _Acc=[] ).

find_files_with_excluded_dirs( RootDir, CurrentRelativeDir, ExcludedDirList,
							Acc ) ->

	%io:format( "find_files_with_excluded_dirs in ~s.~n",
	% [CurrentRelativeDir] ),

	{RegularFiles,Directories,_OtherFiles,_Devices} = list_dir_elements(
		join( RootDir, CurrentRelativeDir ) ),

	% If for example ExcludedDirList=[ ".svn" ], we want to eliminate not only
	% ".svn" but also all "foo/bar/.svn", i.e. all directories having the same
	% (last) name:
	FilteredDirectories = [ D || D <- Directories,
		not ( lists:member( join(CurrentRelativeDir,D), ExcludedDirList )
			 or lists:member( D, ExcludedDirList ) )],

	Acc ++ list_files_in_subdirs_excluded_dirs( FilteredDirectories, RootDir,
								CurrentRelativeDir, ExcludedDirList, _Acc=[] )
		++ prefix_files_with( CurrentRelativeDir, RegularFiles ).


% Specific helper for find_files_with_excluded_dirs/4 above:
list_files_in_subdirs_excluded_dirs( _Dirs=[], _RootDir,
		_CurrentRelativeDir, _ExcludedDirList, Acc ) ->
	Acc;

list_files_in_subdirs_excluded_dirs( _Dirs=[H|T], RootDir,
		CurrentRelativeDir, ExcludedDirList, Acc ) ->

	list_files_in_subdirs_excluded_dirs( T, RootDir, CurrentRelativeDir,
		ExcludedDirList,
		find_files_with_excluded_dirs( RootDir, join(CurrentRelativeDir,H),
			ExcludedDirList, [] ) ++ Acc ).





% Returns the list of all regular files found from the root which do not match
% any of the specified suffixes, in the whole subtree (i.e. recursively).
%
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes ) ->
	find_files_with_excluded_suffixes( RootDir, _CurrentRelativeDir="",
									  ExcludedSuffixes, _Acc=[] ).


find_files_with_excluded_suffixes( RootDir, CurrentRelativeDir,
										ExcludedSuffixes, Acc ) ->

	%io:format( "find_files_with_excluded_suffixes in ~s.~n",
	% [CurrentRelativeDir] ),

	{RegularFiles,Directories,_OtherFiles,_Devices} = list_dir_elements(
		join( RootDir, CurrentRelativeDir ) ),

	Acc ++ list_files_in_subdirs_with_excluded_suffixes( Directories,
			ExcludedSuffixes, RootDir, CurrentRelativeDir, [] )
		++ prefix_files_with( CurrentRelativeDir,
			filter_by_excluded_suffixes(RegularFiles,ExcludedSuffixes) ).


% Helper for find_files_with_excluded_suffixes/4:
list_files_in_subdirs_with_excluded_suffixes( [], _ExcludedSuffixes, _RootDir,
									_CurrentRelativeDir, Acc) ->
	Acc;

list_files_in_subdirs_with_excluded_suffixes( [H|T], ExcludedSuffixes, RootDir,
									CurrentRelativeDir, Acc ) ->
	list_files_in_subdirs_with_excluded_suffixes( T, ExcludedSuffixes, RootDir,
		CurrentRelativeDir,
		find_files_with_excluded_suffixes( RootDir, join(CurrentRelativeDir,H),
			ExcludedSuffixes, [] ) ++ Acc ).






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
find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirList,
										   ExcludedSuffixes ) ->

	%{ok,CurrentDir} = file:get_cwd(),
	%io:format( "find_files_with_excluded_dirs_and_suffixes: current is ~s, "
	%		  "root is ~s.~n", [CurrentDir,RootDir] ),

	find_files_with_excluded_dirs_and_suffixes( RootDir,
			_CurrentRelativeDir="", ExcludedDirList, ExcludedSuffixes, _Acc=[]
											   ).


find_files_with_excluded_dirs_and_suffixes( RootDir, CurrentRelativeDir,
		ExcludedDirList, ExcludedSuffixes, Acc ) ->

	%io:format( "find_files_with_excluded_dirs_and_suffixes in ~s / ~s.~n",
	% [RootDir, CurrentRelativeDir] ),

	{RegularFiles,Directories,_OtherFiles,_Devices} = list_dir_elements(
		join( RootDir, CurrentRelativeDir ) ),

	% If for example ExcludedDirList=[ ".svn" ], we want to eliminate not only
	% ".svn" but also all "foo/bar/.svn", i.e. all directories having the same
	% (last) name:
	FilteredDirectories = [ D || D <- Directories,
		not ( lists:member( join(CurrentRelativeDir,D), ExcludedDirList )
			 or lists:member( D, ExcludedDirList ) )],

	Acc ++ list_files_in_subdirs_excluded_dirs_and_suffixes(
			FilteredDirectories, RootDir, CurrentRelativeDir,
			ExcludedDirList, ExcludedSuffixes, _Acc=[] )
		++ prefix_files_with( CurrentRelativeDir,
			filter_by_excluded_suffixes( RegularFiles, ExcludedSuffixes ) ).



% Specific helper for find_files_with_excluded_dirs_and_suffixes/5 above:
list_files_in_subdirs_excluded_dirs_and_suffixes( _Dirs=[], _RootDir,
		_CurrentRelativeDir, _ExcludedDirList, _ExcludedSuffixes, Acc ) ->
	Acc;

list_files_in_subdirs_excluded_dirs_and_suffixes( _Dirs=[H|T], RootDir,
		CurrentRelativeDir, ExcludedDirList, ExcludedSuffixes, Acc ) ->
	list_files_in_subdirs_excluded_dirs_and_suffixes( T, RootDir,
		CurrentRelativeDir, ExcludedDirList, ExcludedSuffixes,
		find_files_with_excluded_dirs_and_suffixes( RootDir,
			join(CurrentRelativeDir,H),	ExcludedDirList, ExcludedSuffixes, [] )
		++ Acc ).





prefix_files_with( RootDir, Files ) ->
	%io:format( "Prefixing ~p with '~s'.~n", [Files,RootDir] ),
	prefix_files_with( RootDir, Files, [] ).


prefix_files_with( _RootDir, [], Acc ) ->
	Acc;

prefix_files_with( RootDir, [H|T], Acc ) ->
	prefix_files_with( RootDir, T, [join(RootDir,H)|Acc] ).








% Returns the list of all directories found from the root, in the
%  whole subtree (i.e. recursively).
% All returned pathnames are relative to this root.
% Ex: [ "./my-dir", "./tmp/other-dir" ].
find_directories_from( RootDir ) ->
	find_directories_from( RootDir, "", [] ).


find_directories_from( RootDir, CurrentRelativeDir, Acc ) ->
	%io:format( "find_directories_from in ~s.~n", [CurrentRelativeDir] ),
	{_RegularFiles,Directories,_OtherFiles,_Devices} = list_dir_elements(
		join(RootDir,CurrentRelativeDir) ),
	Acc ++ list_directories_in_subdirs( Directories,
			RootDir, CurrentRelativeDir, [] )
		++ prefix_files_with( CurrentRelativeDir, Directories ).



list_directories_in_subdirs( [], _RootDir, _CurrentRelativeDir, Acc ) ->
	Acc;

list_directories_in_subdirs( [H|T], RootDir, CurrentRelativeDir, Acc ) ->
	list_directories_in_subdirs( T, RootDir, CurrentRelativeDir,
		find_directories_from( RootDir, join(CurrentRelativeDir,H), [] )
		++ Acc ).



% Creates specified directory, without creating any intermediate (parent)
% directory that would not exist.
% Throws an exception if the operation failed.
create_directory( Dirname ) ->
	create_directory( Dirname, create_no_parent ).


% Creates specified directory.
%
% If 'create_no_parent' is specified, no intermediate (parent) directory will be
% created.
%
% If 'create_parents' is specified, any non-existing intermediate (parent)
% directory will be created.
%
% Throws an exception if the operation fails.
create_directory( Dirname, create_no_parent ) ->
	case file:make_dir( Dirname ) of

		ok ->
			ok;

		{error,Reason} ->
			throw( {create_directory_failed,Dirname,Reason} )

	end;

create_directory( Dirname, create_parents ) ->
	create_dir_elem( filename:split( Dirname ), "" ).



create_dir_elem( [], _Prefix ) ->
	ok;

create_dir_elem( [H|T], Prefix ) ->
	NewPrefix = join(Prefix,H),
	case exists( NewPrefix ) of

		true ->
			ok ;

		false ->
			create_directory( NewPrefix, create_no_parent )

	end,
	create_dir_elem( T, NewPrefix ).



% Removes specified file, specified as a plain string.
remove_file( Filename ) ->

	case file:delete( Filename ) of

		ok ->
			ok;

		Error ->
			throw( {remove_file_failed,Filename,Error} )

	end.



% Removes specified files, specified as a list of plain strings.
remove_files( FilenameList ) ->
	[ remove_file( Filename ) || Filename <- FilenameList ].



% Removes specified file, specified as a plain string, iff it is already
% existing, otherwise does nothing.
remove_file_if_existing( Filename ) ->

	case is_existing_file( Filename ) of

		true ->
			remove_file( Filename );

		false ->
			ok

	end.



% Removes each specified file, in specified list of plain strings, iff it is
% already existing.
remove_files_if_existing( FilenameList ) ->
	[ remove_file( Filename ) || Filename <- FilenameList ].



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
path_to_variable_name(Filename) ->
	path_to_variable_name(Filename,"File_").


% Removes any leading './':
path_to_variable_name([$.,$/|T],Prefix) ->
	convert(T,Prefix);

path_to_variable_name(Filename,Prefix) ->
	convert(Filename,Prefix).



% Helper function.
convert(Filename,Prefix) ->
	NoDashName = re:replace( lists:flatten(Filename), "-+", "_",
		[global,{return, list}] ),
	NoDotName = re:replace( NoDashName, "\\.+", "_",
		[global,{return, list}] ),
	Prefix ++ re:replace( NoDotName, "/+", "_",
		[global,{return, list}] ).




-define(ResourceDir,"resources").


% Returns the image path corresponding to the specified file.
get_image_file_png(Image) ->
  filename:join([ ?ResourceDir, "images", Image ++ ".png" ]).



% Returns the image path corresponding to the specified file.
get_image_file_gif(Image) ->
  filename:join([ ?ResourceDir, "images", Image ++ ".gif" ]).




% I/O section.


% Opens the file corresponding to specified filename with specified list of
% options.
%
% Returns the file reference, or throws an exception.
%
% Will attempt to open the specified file only once, as looping endlessly does
% not seem a viable solution right now (risk of exhausting the descriptors,
% making the VM fail for example when loading a new BEAM).
%
open( Filename, Options ) ->
	open( Filename, Options, _Default=try_once ).


% Opens the file corresponding to specified filename (first parameter) with
% specified list of options (second parameter).
%
% Third parameter is either 'try_once' or 'try_endlessly', depending
% respectively on whether we want to try to open the file once (no other attempt
% will be made) or endlessly, until a file descriptor can be gained.
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
%                                 %
% This is done in order to support situations where potentially more Erlang
% processes than available file descriptors try to access to files. An effort is
% made to desynchroniopen/2ze these processes to smooth the use of descriptors.
open( Filename, Options, try_endlessly_safer ) ->

	File = open( Filename, Options, try_endlessly ),

	% We could check here that at least one descriptor remains, by adding a
	% dummy file open/close and catching emfile, however one could need more
	% than one spare descriptor.
	%
	% The correct solution would involve knowing the max number of descriptors
	% for that process and the current number of open ones, none information we
	% seems able to know.
	%
	% So for the moment we do not do anything more:
	File;

open( Filename, Options, try_endlessly ) ->

	case file:open( Filename, Options ) of

		{ok,File} ->
			 File;

		{error,FileError} when FileError == emfile
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

		{error,OtherFileError} ->
			throw( {open_failed, {Filename,Options}, OtherFileError } )

	end;

open( Filename, Options, try_once ) ->

	case file:open( Filename, Options ) of

		{ok,File} ->
			 File;

		{error,emfile} ->
			throw( {too_many_open_files, {Filename,Options}} );

		{error,system_limit} ->
			% Never had system_limit without this cause (yet!):
			throw( {too_many_open_files, {Filename,Options}, system_limit} );

		{error,OtherError} ->
			throw( {open_failed, {Filename,Options}, OtherError } )

	end.



% Reads the content of the specified file, based on its filename specified as a
% plain string, and returns the corresponding binary, or throws an exception on
% failure.
read_whole( Filename ) ->

	case file:read_file( Filename ) of

		{ok,Binary} ->
			Binary;

		{error,Error} ->
			throw( {read_whole_failed,Filename,Error} )

	end.



% Writes the specified binary in specified file, whose filename is specified as
% a plain string. Throws an exception on failure.
write_whole( Filename, Binary ) ->

	case file:write_file( Filename, Binary ) of

		ok ->
			ok;

		{error,Error} ->
			throw( {write_whole_failed,Filename,Error} )

	end.




% Zip-related operations.


% Reads in memory the file specified from its filename, zips the corresponding
% term, and returns it.
%
% Note: useful for network transfers of small files.
%
% Larger ones should be transferred with TCP/IP and by chunks.
%
% Returns a binary.
file_to_zipped_term(Filename)  ->
	DummyFileName = "dummy",
	{ok,{_DummyFileName,Bin}} =
		%zip:zip( DummyFileName, [Filename], [verbose,memory] ),
		zip:zip( DummyFileName, [Filename], [memory] ),
	Bin.



% Reads specified binary, extracts the zipped file in it and writes it on disk,
% in current directory.
%
% Returns the filename of the unzipped file.
zipped_term_to_unzipped_file(ZippedTerm) ->
	%zip:unzip(ZippedTerm,[verbose]).
	{ok,[FileName]} = zip:unzip(ZippedTerm),
	FileName.



% Reads specified binary, extracts the zipped file in it and writes it on disk,
% in current directory, under specified filename instead of under filename
% stored in the zip archive.
%
% Any pre-existing file will be overwritten.
%
% Note: only one file is expected to be stored in the specified archive.
%
zipped_term_to_unzipped_file(ZippedTerm,TargetFilename) ->
	{ok,[{_AFilename,Binary}]} = zip:unzip(ZippedTerm,[memory]),
	%% {ok,File} = file:open( TargetFilename, [write] ),
	%% ok = io:format( File, "~s", [ binary_to_list(Binary) ] ),
	%% ok = file:write_file( File, "~s", [ binary_to_list(Binary) ] ),
	%% ok = file:close(File).
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
files_to_zipped_term( FilenameList )  ->
	DummyFileName = "dummy",
	{ok,{_DummyFileName,Bin}} =	zip:zip( DummyFileName, FilenameList,
										[memory] ),
	Bin.



% Reads specified binary, extracts the zipped files stored in it and writes them
% on disk, in current directory.
%
% Returns the list of filenames corresponding to the unzipped files.
%
zipped_term_to_unzipped_files( ZippedTerm ) ->
	%{ok,FileNames} = zip:unzip(ZippedTerm,[verbose]),
	{ok,FileNames} = zip:unzip(ZippedTerm),
	FileNames.


% Reads specified binary, extracts the zipped files in it and writes them on
% disk, in specified directory.
%
% Returns the list of filenames corresponding to the unzipped files.
%
zipped_term_to_unzipped_files( ZippedTerm, TargetDirectory ) ->
	%{ok,FileNames} = zip:unzip(ZippedTerm,[verbose]),
	case is_existing_directory(TargetDirectory) of

		true ->
			{ok,FileNames} = zip:unzip( ZippedTerm, [ {cwd,TargetDirectory} ] ),
			FileNames;

		false ->
			throw( {non_existing_unzip_directory,TargetDirectory} )

	end.
