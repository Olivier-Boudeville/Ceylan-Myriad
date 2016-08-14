#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable +A 16

% Commented out: -pa ../utils


% Copyright (C) 2016-2016 Olivier Boudeville (olivier.boudeville@esperide.com)

% Released as LGPL software.

-define( merge_cache_filename, ".merge-tree.cache" ).

% Version of this tool:
-define( merge_script_version, "0.1" ).

-define( default_log_filename, "merge-tree.log" ).

% Short of having the parse transform allowing 'table':
-define( table, map_hashtable ).


-export([ create_merge_cache_file_for/3,
		  tree_entry_to_string/1, file_entry_to_string/1,
		  trace/2, trace/3, trace_debug/2, trace_debug/3
		]).


% Entry associated to a content tree.
%
-record( tree_entry, {

		   % Base, absolute path of that tree:
		   root :: file_utils:directory_name(),

		   % Each key is the MD5 sum of a file content, each value is a list of
		   % the file entries whose content matches that sum (hence are supposed
		   % the same).
		   %
		   entries = ?table:new() :: ?table:?table( executable_utils:md5_sum(),
													[ file_entry() ] ),

		   % Count of the regular files found in tree:
		   file_count = 0 :: basic_utils:count(),

		   % Count of the directories found in tree:
		   directory_count = 0 :: basic_utils:count(),

		   % Count of the symbolic links found in tree:
		   symlink_count = 0 :: basic_utils:count(),

		   % Count of the device found in tree:
		   device_count = 0 :: basic_utils:count(),

		   % Count of the other elements found in tree:
		   other_count = 0 :: basic_utils:count()

}).

-type tree_entry() :: #tree_entry{}.



% Entry associated to a given file-like element.
%
% Note: entries might be stored in tables, the associate key potentially
% duplicating them (no problem).
%
-record( file_entry, {

		   % Path of this file, relative to the tree root:
		   path :: file_utils:path(),

		   % Type of the file element:
		   type :: file_utils:entry_type(),

		   % Precise size, in bytes, of that file:
		   size :: system_utils:byte_size(),

		   % Timestamp of the last content modification known from the
		   % filesystem:
		   timestamp :: time_utils:posix_seconds(),

		   % MD5 sum of the content of that file:
		   md5_sum :: executable_utils:md5_sum()

}).

-type file_entry() :: #file_entry{}.


% User-related state:
-type user_state() :: { ui:ui_state(), 'undefined' | file_utils:file() }.


-export_type([ tree_entry/0, file_entry/0 ]).


get_usage() ->
	"   Usage:\n"
	"      - either: merge-tree.escript SOURCE_TREE TARGET_TREE\n"
	"      - or: merge-tree.escript --scan TREE\n\n"
	"   Ensures that all the changes in a supposedly more up-to-date tree (SOURCE_TREE) are merged back to the reference tree (TARGET_TREE), from which the source one derivated. Once executed, only a refreshed reference target tree will exist, as the input SOURCE_TREE will be removed since all its content will have been put back in the reference TARGET_TREE.\n"
	"   In-tree duplicated content will be removed and replaced by symbolic links, to keep a single version of each actual content.\n"
	"   All the timestamps of the files in the reference tree will be set to the current time, and, at the root of the reference tree, a '" ?merge_cache_filename "' file will be stored, in order to spare later computations of the file checksums.\n"
	"   If the --scan option is used, then the specified tree will be inspected and a corresponding '" ?merge_cache_filename "' file will be created (potentially reused for a later merge)".


% This script depends on the 'Common' layer, and only on that code.


% Implementation notes:
%
% - merge cache files could/should use a compressed form ('compress' option).
%
% - the 'table' pseudo-module cannot be used, as escripts are (currently) not
% parse-transformed; '?table' is used instead
%
% - at least currently we only focus on (regular) files, hence the counts for
% directories and all remain null



% Entry point of the script.
main( [ "-h" ] ) ->
	io:format( "~s", [ get_usage() ] );

main( [ "--help" ] ) ->
	io:format( "~s", [get_usage()] );

main( [ "--scan", TreePath ] ) ->

	% First enable all possible helper code:
	update_code_path_for_common(),

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	AbsTreePath = file_utils:ensure_path_is_absolute( TreePath ),

	% Best, reasonable usage:
	Analyzers = spawn_entry_analyzers( system_utils:get_core_count() + 1,
									   UserState ),

	AnalyzerRing = list_utils:list_to_ring( Analyzers ),

	TreeEntry = update_content_tree( AbsTreePath, AnalyzerRing, UserState ),

	_NewTreeEntry = diagnose_tree( TreeEntry, UserState ),

	terminate_entry_analyzers( Analyzers, UserState ),

	stop_user_service( UserState );

main( [ SourceTree, TargetTree ] ) ->

	% First enable all possible helper code:
	update_code_path_for_common(),

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	check_content_trees( SourceTree, TargetTree ),

	trace( "Merging tree '~s' into tree '~s'...",
		   [ SourceTree, TargetTree ], UserState ),

	% Best, reasonable usage:
	Analyzers = spawn_entry_analyzers( system_utils:get_core_count() + 1,
									   UserState ),

	AnalyzerRing = list_utils:list_to_ring( Analyzers ),

	update_content_tree( SourceTree, AnalyzerRing, UserState ),

	terminate_entry_analyzers( Analyzers, UserState ),

	stop_user_service( UserState );

main( _ ) ->
	io:format( "~n   Error, exactly two parameters should be specified.~n~n~s",
			   [ get_usage() ] ).






% Helpers.


% Starts user-related services.
%
-spec start_user_service( file_utils:filename() ) -> user_state().
start_user_service( LogFilename ) ->

	% We append to the log file, if it already exists:
	LogFile = file_utils:open( LogFilename,
							   _Opts=[ append, raw, delayed_write ] ),

	file_utils:write( LogFile, "~nStarting new merge session on ~s "
					  "(version ~s) at ~s.~n",
					  [ net_utils:localhost(), ?merge_script_version,
						time_utils:get_textual_timestamp() ] ),

	UIState = ui:start(),

	{ UIState, LogFile }.



% Displays and logs specified text.
%
-spec trace( text_utils:string(), user_state() ) -> user_state().
trace( Message, _UserState={ UIState, LogFile } ) ->
	NewUIState = ui:trace( Message, UIState ),
	file_utils:write( LogFile, Message ++ "\n" ),
	{ NewUIState, LogFile }.



% Displays and logs specified formatted text.
%
-spec trace( text_utils:format_string(), [ term() ], user_state() ) ->
				   user_state().
trace( FormatString, Values, _UserState={ UIState, LogFile } ) ->
	Msg = text_utils:format( FormatString, Values ),
	NewUIState = ui:trace( Msg, UIState ),
	file_utils:write( LogFile, Msg ++ "\n" ),
	{ NewUIState, LogFile }.



% Displays and logs specified debug text.
%
-spec trace_debug( text_utils:string(), user_state() ) -> user_state().
trace_debug( Message, _UserState={ UIState, LogFile } ) ->
	%NewUIState = ui:trace( Message, UIState ),
	file_utils:write( LogFile, Message ++ "\n" ),
	{ UIState, LogFile }.



% Displays and logs specified debug formatted text.
%
-spec trace_debug( text_utils:format_string(), [ term() ], user_state() ) ->
				   user_state().
trace_debug( FormatString, Values, _UserState={ UIState, LogFile } ) ->
	Msg = text_utils:format( FormatString, Values ),
	%NewUIState = ui:trace( Msg, UIState ),
	file_utils:write( LogFile, Msg ++ "\n" ),
	{ UIState, LogFile }.




% Stops user-related services.
%
-spec stop_user_service( user_state() ) -> basic_utils:void().
stop_user_service( _UserState={ UIState, LogFile } ) ->

	ui:stop( UIState ),

	file_utils:write( LogFile, "Stopping merge session.~n", [] ),

	file_utils:close( LogFile ).



% Checks that the source and target trees exist.
%
check_content_trees( SourceTree, TargetTree ) ->

	case file_utils:is_existing_directory( SourceTree ) of

		true ->
			ok;

		false ->
			throw( { non_existing_source_content_tree, SourceTree } )

	end,

	case file_utils:is_existing_directory( TargetTree ) of

		true ->
			ok;

		false ->
			throw( { non_existing_target_content_tree, TargetTree } )

	end.



% Updates specified content tree: verifies that it exists, that a merge cache
% file exists and is up to date (otherwise rebuilds it) and returns the
% corresponding datastrucuture.
%
-spec update_content_tree( file_utils:directory_name(),
		   list_utils:ring( pid() ), user_state() ) -> basic_utils:void().
update_content_tree( Tree, AnalyzerRing, UserState ) ->

	case file_utils:is_existing_directory( Tree ) of

		true ->
			ok;

		false ->
			throw( { non_existing_content_tree, Tree } )

	end,

	CacheFilename = file_utils:join( Tree, ?merge_cache_filename ),

	case file_utils:is_existing_file( CacheFilename ) of

		true ->
			throw( fixme );

		false ->
			create_merge_cache_file_for( Tree, CacheFilename, AnalyzerRing,
										 UserState )

	end.



% Creates merge cache file for specified content tree (overwriting any priorly
% existing merge cache file).
%
-spec create_merge_cache_file_for( file_utils:directory_name(),
					list_utils:ring( pid() ), user_state() ) -> tree_entry().
create_merge_cache_file_for( Tree, AnalyzerRing, UserState ) ->
	CacheFilename = file_utils:join( Tree, ?merge_cache_filename ),
	create_merge_cache_file_for( Tree, CacheFilename, AnalyzerRing, UserState ).



% Creates merge cache file with specified name, for specified content tree.
%
-spec create_merge_cache_file_for( file_utils:directory_name(),
		  file_utils:file_name(), list_utils:ring( pid() ), user_state() ) ->
										 tree_entry().
create_merge_cache_file_for( Tree, CacheFilename, AnalyzerRing, UserState ) ->

	AbsTreePath = file_utils:ensure_path_is_absolute( Tree ),

	case file_utils:is_existing_directory( AbsTreePath ) of

		true ->
			ok;

		false ->
			throw( { non_existing_content_tree, AbsTreePath } )

	end,

	trace( "Creating merge cache file '~s'.", [ CacheFilename ], UserState ),

	MergeFile = file_utils:open( CacheFilename,
								 _Opts=[ write, raw, delayed_write ] ),

	ScriptName = filename:basename( escript:script_name() ),

	file_utils:write( MergeFile, "% Merge cache file written by '~s' "
								 "(version ~s):~n"
								 "% - on host '~s'~n"
								 "% - for content tree '~s'~n"
								 "% - at ~s~n",
					  [ ScriptName, ?merge_script_version,
						net_utils:localhost(), AbsTreePath,
						time_utils:get_textual_timestamp() ] ),

	_BlankEntryTable = ?table:new(),

	_CurrentPosixTime = os:system_time(),

	TreeEntry = scan_tree( AbsTreePath, AnalyzerRing, UserState ),

	trace( "Scanned tree: " ++ tree_entry_to_string( TreeEntry ), UserState ),

	file_utils:write( MergeFile, "~n~n% End of merge cache file (at ~s).",
					  [ time_utils:get_textual_timestamp() ] ),

	file_utils:close( MergeFile ),

	TreeEntry.




% Spawns the specified number of entry analyzers and returns their PID.
%
-spec spawn_entry_analyzers( basic_utils:count(), user_state() ) -> [ pid() ].
spawn_entry_analyzers( Count, UserState ) ->
	trace_debug( "Spawning ~B entry analyzers.", [ Count ], UserState ),
	[ spawn_link( fun() -> analyze_loop() end )
	  || _C <- lists:seq( 1,Count ) ].


% Terminates specified entry analyzers.
%
-spec terminate_entry_analyzers( [ pid() ], user_state() ) ->
									   basic_utils:void().
terminate_entry_analyzers( PidList, UserState ) ->
	trace_debug( "Terminating ~B entry analyzers (~p).",
				 [ length( PidList ), PidList ], UserState ),
	[ P ! terminate || P <- PidList ].


-spec scan_tree( file_utils:path(), list_utils:ring( pid() ), user_state() ) ->
					   tree_entry().
scan_tree( AbsTreePath, AnalyzerRing, UserState ) ->

	trace( "Scanning tree '~s'...", [ AbsTreePath ], UserState ),

	AllFiles = file_utils:find_files_from( AbsTreePath ),

	% Not wanting to index our own files:
	FilteredFiles = lists:delete( ?merge_cache_filename, AllFiles ),

	trace_debug( "Found ~B files:~s", [ length( FilteredFiles ),
			text_utils:strings_to_string( FilteredFiles ) ], UserState ),

	scan_files( FilteredFiles, AbsTreePath, AnalyzerRing ).



% Scans specified content files, using for that the specified analyzers,
% returning the corresponding tree entry.
%
-spec scan_files( [ file_utils:file_name() ], file_utils:path(),
				  list_utils:ring( pid() ) ) ->	tree_entry().
scan_files( Files, AbsTreePath, AnalyzerRing ) ->

	InitialTreeEntry = #tree_entry{ root=AbsTreePath },

	scan_files( Files, AnalyzerRing, InitialTreeEntry, _WaitedCount=0 ).


scan_files( _Files=[], _AnalyzerRing, TreeEntry, _WaitedCount=0 ) ->
	% In final state (none waited), hence directly returned:
	%io:format( "All file entries retrieved.~n" ),
	TreeEntry;

scan_files( _Files=[], _AnalyzerRing, TreeEntry, WaitedCount ) ->
	% Will return an updated tree entry once all answers received:
	%io:format( "Final waiting for ~B entries.~n", [ WaitedCount ] ),
	wait_entries( TreeEntry, WaitedCount );

scan_files( _Files=[ Filename | T ], AnalyzerRing,
			TreeEntry=#tree_entry{ root=AbsTreePath }, WaitedCount ) ->

	FullPath = filename:join( AbsTreePath, Filename ),

	{ Analyzer, NewRing } = list_utils:head( AnalyzerRing ),

	%io:format( "Requesting analysis of '~s' by ~w.~n",
	%   [ FullPath, Analyzer ] ),

	Analyzer ! { analyze_file, FullPath, self() },

	% Helps controlling flow and avoiding too large mailboxes on either side
	% (this main script, being slowed down, or the analyzers):
	%
	receive

		{ analyzed, FileEntry } ->

			NewTreeEntry = manage_received_entry( FileEntry, TreeEntry ),
			% Plus one minus one:
			scan_files( T, NewRing, NewTreeEntry, WaitedCount )

	after 0 ->

			scan_files( T, NewRing, TreeEntry, WaitedCount+1 )

	end.



% Manages specified received file entry, and returns an updated tree entry.
%
-spec manage_received_entry( file_entry(), tree_entry() ) -> tree_entry().
manage_received_entry( FileEntry=#file_entry{ type=Type, md5_sum=Sum },
					   TreeEntry=#tree_entry{ entries=Entries,
											  file_count=FileCount,
											  directory_count=DirCount,
											  symlink_count=SymlinkCount,
											  device_count=DeviceCount,
											  other_count=OtherCount } ) ->

	%io:format( "Entry received: ~s~n",
	%		   [ file_entry_to_string( FileEntry ) ] ),

	% Ensures we associate a list to each MD5 sum:
	NewEntries = case ?table:lookupEntry( Sum, Entries ) of

		key_not_found ->
			?table:addEntry( Sum, [ FileEntry ], Entries );

		{ value, SumEntries } ->
			?table:addEntry( Sum, [ FileEntry | SumEntries ], Entries )

	end,

	NewTreeEntry = TreeEntry#tree_entry{ entries=NewEntries },

	case Type of

		regular ->
			NewTreeEntry#tree_entry{ file_count=FileCount+1 };

		directory ->
			NewTreeEntry#tree_entry{ directory_count=DirCount+1 };

		symlink ->
			NewTreeEntry#tree_entry{ symlink_count=SymlinkCount+1 };

		device ->
			NewTreeEntry#tree_entry{ device_count=DeviceCount+1 };

		other ->
			NewTreeEntry#tree_entry{ other_count=OtherCount+1 }

	end.



% Waits for the remaining file entries.
%
wait_entries( TreeEntry, _WaitedCount=0 ) ->
	%io:format( "All file entries waited for finally obtained.~n" ),
	TreeEntry;

wait_entries( TreeEntry, WaitedCount ) ->
	%io:format( "Still waiting for ~B file entries.~n", [ WaitedCount ] ),

	receive

		{ analyzed, FileEntry } ->
			NewTreeEntry = manage_received_entry( FileEntry, TreeEntry ),
			wait_entries( NewTreeEntry, WaitedCount-1 )

	end.







% The loop run by an analyzer process.
%
-spec analyze_loop() -> basic_utils:void().
analyze_loop() ->

	%io:format( "Analyzer ~w waiting...~n", [ self() ] ),

	receive

		{ analyze_file, FilePath, SenderPid } ->
			%io:format( "Analyzer ~w taking in charge '~s'...~n",
			%		   [ self(), FilePath ] ),
			FileEntry = #file_entry{
						   path=FilePath,
						   type=file_utils:get_type_of( FilePath ),
						   size=file_utils:get_size( FilePath ),
						   timestamp=file_utils:get_last_modification_time(
									   FilePath ),
						   md5_sum=executable_utils:compute_md5_sum( FilePath )
			},

			SenderPid ! { analyzed, FileEntry },
			analyze_loop();

		terminate ->
			%io:format( "Analyzer ~w terminated.~n", [ self() ] ),
			ok

	end.



% Returns a textual diagnosis of specified tree.
%
-spec diagnose_tree( tree_entry(), user_state() ) -> tree_entry().
diagnose_tree( #tree_entry{
		   root=_RootDir,
		   entries=ContentTable,
		   file_count=FileCount }, UserState ) ->

	DuplicateCount = FileCount - ?table:size( ContentTable ),

	{ NewContentTable, RemovedDuplicateCount } = manage_duplicates(
												   ContentTable, UserState ),

	case DuplicateCount - RemovedDuplicateCount of

		0 ->
			trace( "All ~B duplicates removed.", [ DuplicateCount ],
				   UserState );

		Count when Count > 0 ->
			trace( "Out of the ~B duplicates detected, ~B remain (~B removed).",
				   [ DuplicateCount, DuplicateCount, RemovedDuplicateCount ],
				   UserState )

	end,

	NewContentTable.



% Manages all duplicatees found in specified table, returns an updated table and
% the number of duplicates removed.
%
-spec manage_duplicates( ?table:?table(), user_state() ) ->
							   { ?table:?table(), basic_utils:count() }.
manage_duplicates( ContentTable, UserState ) ->

	ContentEntries = ?table:enumerate( ContentTable ),

	% We could have forced that no duplication at all exists afterwards (and
	% then a given MD5 sum would be associated to exactly one content), however
	% it would be too strict, hence we kept a list associated to each MD5 sum:
	%
	% Two passes: one to establish and count the duplications, another to solve
	% them; returns a list of duplications, and a content table referencing all
	% non-duplicate entries.
	%
	{ DuplicationCases, UniqueTable } = lists:foldl(

				   fun( { Md5Key, V=[ _SingleContent ] },
						_Acc={ AccDupEntries, AccUniqueTable } ) ->
						   % Single content, hence unique:
						   { AccDupEntries,
								 ?table:addEntry( Md5Key, V, 
												  AccUniqueTable ) } ;

					  ( Entry={ _Md5Key, _DuplicateList },
						_Acc={ AccDupEntries, AccUniqueTable } ) ->
						   % Here, at least one duplicate:
						   { [ Entry | AccDupEntries ], AccUniqueTable }

				   end,
				   _Acc0={ [], ?table:new() },
				   _List=ContentEntries ),

	case length( DuplicationCases ) of

		0 ->
			ui:display( "No duplicated content detected.", UserState ),
			{ UniqueTable, _RemoveCount=0 };

		TotalDupCaseCount ->
			ui:display( "~B cases of content duplication detected, "
						"examining them in turn.",
						[ TotalDupCaseCount ], UserState ),

			{ FinalTable, _TotalDupCaseCount, TotalRemoved } = lists:foldl(

				   fun( { Md5Key, DuplicateList },
						_Acc={ AccTable, AccDupCount, AccRemoveCount } ) ->
						   Size = check_duplicates( Md5Key, DuplicateList ),
						   SelectedFileEntries = manage_duplicate(
								DuplicateList, AccDupCount, TotalDupCaseCount,
								Size, UserState ),
						   NewAccTable = ?table:addEntry( Md5Key,
											SelectedFileEntries, AccTable ),

						   { NewAccTable, AccDupCount+1, AccRemoveCount
							 + length( DuplicateList )
							 - length( SelectedFileEntries ) }

				   end,
				   _Acc0={ UniqueTable, _InitialDupCount=1, _InitialRemoved=0 },
				   _List=DuplicationCases ),

			{ FinalTable, TotalRemoved }

	end.



% Checks a duplication set: same MD5 sum and size must be found for all file
% entries.
%
-spec check_duplicates( executable_utils:md5_sum(), [ file_entry() ] ) ->
							  basic_utils:void().
% Not possible: check_duplicates( _MD5Sum, _DuplicateList=[] ) ->
%	ok;

check_duplicates( MD5Sum, _DuplicateList=[
	   #file_entry{ md5_sum=MD5Sum, size=Size } | T ] ) ->
	check_duplicates( MD5Sum, Size, T ).


check_duplicates( _MD5Sum, Size, _DuplicateList=[] ) ->
	Size;

check_duplicates( MD5Sum, Size, _DuplicateList=[
	   #file_entry{ md5_sum=MD5Sum, size=Size } | T ] ) ->
	check_duplicates( MD5Sum, Size, T ).



% Manages specified duplicated entries.
%
-spec manage_duplicate( [ file_entry() ], basic_utils:count(),
						basic_utils:count(), system_utils:byte_size(),
						user_state() ) -> [ file_entry() ].
manage_duplicate( FileEntries, DuplicationCaseCount, TotalDupCaseCount, Size,
				  UserState ) ->

	SizeString = system_utils:interpret_byte_size_with_unit( Size ),

	Label = text_utils:format( "Examining duplication case ~B/~B: "
							   "following ~B files have the exact same "
							   "content (and thus size, of ~s):",
							   [ DuplicationCaseCount, TotalDupCaseCount,
								 length( FileEntries ), SizeString ] ),

	Choices = [ E#file_entry.path  || E <- FileEntries ],

	ui:display_numbered_list( Label, Choices, UserState ),

	_Options = [ { 'l', "leave them as they are" },
				{ 'e', "elect a reference file, replacing each other by "
					   "a symbolic link pointing to it" },
				{ 'a', "abort" } ],

	throw( the_end ).
	%% case ui:select_option( Options ) of

	%% 	'l' ->

	%% ui:display( "Choose among: (l) leave them as they are
	%% %case ui:
	%% FileEntries.


% Returns a textual description of specified tree entry.
%
-spec tree_entry_to_string( tree_entry() ) -> text_utils:string().
tree_entry_to_string( #tree_entry{
		   root=RootDir,
		   entries=Table,
		   file_count=FileCount,
		   directory_count=_DirCount,
		   symlink_count=_SymlinkCount } ) ->

	% Only looking for files:
	%text_utils:format( "tree '~s' having ~B entries (~B files, ~B directories,"
	%				   " ~B symbolic links)",
	%				   [ RootDir, ?table:size( Table ), FileCount, DirCount,
	%					 SymlinkCount ] ).

	case ?table:size( Table ) of

		FileCount ->
			text_utils:format( "tree '~s' having ~B files, "
							   "each with unique content",
							   [ RootDir, FileCount ] );

		ContentCount ->
			text_utils:format( "tree '~s' having ~B files, corresponding "
							   "only to ~B different contents "
							   "(hence ~B duplicates)",
							   [ RootDir, FileCount, ContentCount,
								 FileCount -  ContentCount ] )

	end.



% Returns a textual description of specified file entry.
%
-spec file_entry_to_string( file_entry() ) -> text_utils:string().
file_entry_to_string( #file_entry{
						 path=Path,
						 size=Size,
						 timestamp=Timestamp,
						 md5_sum=Sum } ) ->

	SizeString = system_utils:interpret_byte_size_with_unit( Size ),

	text_utils:format( "file '~s' whose size is ~s, MD5 sum is ~s and "
					   "timestamp is ~p",
					   [ Path, SizeString, Sum, Timestamp ] ).



% Verbatime section.


% Copied verbatim from common/src/utils/script_utils.erl:

% Updates the VM code path so that all modules of the 'Common' layer can be
% readily used.
%
% Note: this function and its helpers might be copied verbatim to the target
% escript so that it can really be used from anywhere (not only from the
% directory it is stored).
%
-spec update_code_path_for_common() -> basic_utils:void().
update_code_path_for_common() ->

	CommonRootDir = get_root_of_common(),

	CommonSrcDir = filename:join( CommonRootDir, "src" ),

	CommonBeamSubDirs = [ "utils", "user-interface", "maths",
						  "data-management" ],

	CommonBeamDirs = [ filename:join( CommonSrcDir, D )
					   || D <- CommonBeamSubDirs ],

	%io:format( "'Common' beam dirs: ~s~n", [ CommonBeamDirs ] ),

	ok = code:add_pathsa( CommonBeamDirs ).



% Returns the base directory of that script, i.e. where it is stored (regardless
% of the possibly relative path whence it was launched).
%
% Note: useful to locate resources (ex: other modules) defined with that script
% and needed by it.
%
-spec get_script_base_directory() -> file_utils:path().
get_script_base_directory() ->

	% filename:absname/1 could be used instead:
	FullPath = case escript:script_name() of

		ScriptPath=( "/" ++ _ ) ->
			% Is already absolute here:
			ScriptPath;

		RelativePath ->
			% Let's make it absolute then:
			{ ok, CurrentDir } = file:get_cwd(),
			filename:join( CurrentDir, RelativePath )

	end,

	filename:dirname( FullPath ).



% Returns the root directory of the Common layer.
%
-spec get_root_of_common() -> file_utils:path().
get_root_of_common() ->
	filename:join( [ get_script_base_directory(), "..", ".." ] ).
