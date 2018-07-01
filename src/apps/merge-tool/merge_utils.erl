% Copyright (C) 2016-2018 Olivier Boudeville
%
% Transferred from merge-tree.escript to benefit from a more user-friendly
% debugging.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Released as LGPL software.
%
-module(merge_utils).


% Implementation notes:
%
% - merge cache files could/should use a compressed form ('compress' option)
%
% - at least currently we only focus on (regular) files, hence the counts for
% directories and all remain null


-define( merge_cache_filename, ".merge-tree.cache" ).


% Version of this tool:
-define( merge_script_version, "0.0.2" ).


-define( default_log_filename, "merge-tree.log" ).


-export([ create_merge_cache_file_for/3,
		  tree_data_to_string/1, file_data_to_string/1,
		  trace/2, trace/3, trace_debug/2, trace_debug/3 ]).


% Shorthands:
-type sha1() :: executable_utils:sha1_sum().
-type count() :: basic_utils:count().


% Data associated to a given file-like element.
%
% Note: these records are typically stored in tables, the associated key potentially
% duplicating their path (not a problem).
%
-record( file_data, {

		   % Path of this file (an identifier), relative to the tree root:
		   path :: file_utils:bin_path(),

		   % Type of the file element:
		   type :: file_utils:entry_type(),

		   % Precise size, in bytes, of that file:
		   size :: system_utils:byte_size(),

		   % Timestamp of the last content modification of this file, as known
		   % of the filesystem:
		   %
		   timestamp :: time_utils:posix_seconds(),

		   % SHA1 sum of the content of that file:
		   sha1_sum :: sha1()

}).

-type file_data() :: #file_data{}.



% Table referencing file entries based on their SHA1:
%
% (exactly one file_data record per SHA1 key, once the tree is uniquified)
%
-type sha1_table() :: table( sha1(), [ file_data() ] ).


% Pair entries of a sha1_table/0:
-type sha1_entry() :: { sha1(), [ file_data() ] }.


% Data associated to a content tree.
%
-record( tree_data, {

		   % Base, absolute (binary) path of the root of that tree:
		   root :: file_utils:bin_directory_name(),

		   % Each key is the SHA1 sum of a file content, each value is a list of
		   % the file entries whose content matches that sum (hence are supposed
		   % the same).
		   %
		   entries = table:new() :: sha1_table(),

		   % Total count of the regular files found in this tree:
		   file_count = 0 :: count(),

		   % Total count of the directories found in this tree:
		   directory_count = 0 :: count(),

		   % Total count of the symbolic links found in this tree:
		   symlink_count = 0 :: count(),

		   % Total count of the devices found in this tree:
		   device_count = 0 :: count(),

		   % Total count of the other elements found in this tree:
		   other_count = 0 :: count()

}).

-type tree_data() :: #tree_data{}.


-export_type([ file_data/0, tree_data/0 ]).


-record( user_state, {

	log_file = undefined :: maybe( file_utils:file() )

}).


% User-related state:
-type user_state() :: #user_state{}.



% To run from the interpreter rather than as an escript:
-export([ run/0, scan/1, main/1 ]).


% The PID of an analyzer process:
-type analyzer_pid() :: pid().


% Ring of analyzer processes:
%
-type analyzer_ring() :: ring_utils:ring( analyzer_pid() ).


% This script depends on the 'Myriad' layer, and only on that code.
%
% Note: ensure it is already built first!


-spec get_usage() -> void().
get_usage() ->
	"   Usage:\n"
	"      - either: 'merge-tree.escript --input INPUT_TREE --reference REFERENCE_TREE'\n"
	"      - or: 'merge-tree.escript --scan A_TREE'\n\n"
	"   Ensures, for the first form, that all the changes in a possibly more up-to-date, \"newer\" tree (INPUT_TREE) are merged back to the reference tree (REFERENCE_TREE), from which the first tree may have derived. Once executed, only a refreshed reference tree will exist, as the input tree will be removed: all its original content (i.e. its content that was not already in the reference tree) will have been transferred in the reference tree.\n"
	"   In the reference tree, in-tree duplicated content will be either removed as a whole or replaced by symbolic links, to keep only a single version of each actual content.\n"
	"   At the root of the reference tree, a '" ?merge_cache_filename "' file will be stored, in order to avoid any later recomputations of the checksums of the files that it contains, should they not have changed. As a result, once that merge is done, the reference tree will contain an uniquified version of the union of the two specified trees, and the tree to scan will not exist anymore.\n"
	"   For the second form (--scan option), the specified tree will be inspected and will be uniquified (duplicates being removed or replaced with symbolic links), and a corresponding '" ?merge_cache_filename "' file will be created at its root (to be potentially reused by a later merge).".



% Typically for testing:
%
-spec run() -> void().
run() ->
	ArgTable = executable_utils:get_argument_table(),
	main( ArgTable ).



% Sole entry point for this merge service, either triggered by run/0 or by the
% associated escript.
%
-spec main( executable_utils:argument_table() ) -> void().
main( ArgTable ) ->

	trace_utils:info( "Running..." ),

	FilteredArgTable = ui:start( _Opts=[], ArgTable ),

	ui:set_settings( [ { title, "Merging" },
					   { backtitle, "Merging now..." } ] ),

	trace_utils:debug_fmt( "Script-specific arguments: ~s",
		   [ executable_utils:argument_table_to_string( FilteredArgTable ) ] ),

	case list_table:hasEntry( 'h', FilteredArgTable )
		orelse list_table:hasEntry( '-help', FilteredArgTable ) of

		true ->
			display_usage();

		false ->

			% If there is a --reference option, it is a merge, and there must be
			% a --input option as well:
			%
			case list_table:extractEntryWithDefaults( '-reference', undefined,
													  FilteredArgTable ) of

				{ undefined, NoRefArgTable } ->

					% No reference, it must then be a pure scan here:

					case list_table:extractEntryWithDefaults( '-scan',
											   undefined, NoRefArgTable ) of

						{ undefined, NoScanArgTable } ->
							Msg = text_utils:format( "no operation specified "
								"(no scan nor merge).~n~n~nInstead ~s",
								[ executable_utils:argument_table_to_string(
									NoScanArgTable ) ] ),
							stop_on_option_error( Msg, 20 );

						{ [ ScanTreePath ], NoScanArgTable }
								when is_list( ScanTreePath ) ->

							% Check no unknown option remains:
							case list_table:isEmpty( NoScanArgTable ) of

								true ->
									scan( ScanTreePath );

								false ->
									Msg = text_utils:format( "unexpected "
											"extra options specified: ~s",
									[ executable_utils:argument_table_to_string(
										NoScanArgTable ) ] ),
									stop_on_option_error( Msg, 21 )

							end;

						{ UnexpectedScanTreeOpts, _NoScanArgTable } ->
							ScanString = text_utils:format(
										   "unexpected scan tree options: ~p",
										   [ UnexpectedScanTreeOpts ] ),

							stop_on_option_error( ScanString, 22 )


					end;


				% Here there is a --reference, hence we expect a --input as
				% well:
				%
				{ [ RefTreePath ], NoRefArgTable }
				  when is_list( RefTreePath ) ->

					case list_table:extractEntryWithDefaults( '-input',
											 undefined, NoRefArgTable ) of

						{ undefined, NoInputArgTable } ->

							InputString = text_utils:format( "no input tree "
								"specified; options were: ~s",
								[ executable_utils:argument_table_to_string(
									NoInputArgTable ) ] ),

							stop_on_option_error( InputString, 23 );

						% Here, an input tree specified as well:
						{ [ InputTreePath ], NoInputArgTable }
						  when is_list( InputTreePath ) ->

							% Check no unknown option remains:
							case list_table:isEmpty( NoInputArgTable ) of

								true ->
									merge( InputTreePath, RefTreePath );

								false ->
									Msg = text_utils:format( "unexpected "
											"extra options specified: ~s",
									[ executable_utils:argument_table_to_string(
										 NoInputArgTable) ] ),
									stop_on_option_error( Msg, 24 )

							end;

						{ UnexpectedInputTreeOpts, _NoInputArgTable } ->
							ScanString = text_utils:format(
										   "unexpected --input options: ~p",
										   [ UnexpectedInputTreeOpts ] ),

							stop_on_option_error( ScanString, 25 )

					end;


				{ UnexpectedRefTreeOpts, _ } ->
					RefString = text_utils:format(
								  "unexpected reference tree options: ~p",
								  [ UnexpectedRefTreeOpts ] ),

					stop_on_option_error( RefString, 26 )

			end


	end.



% Displays the usage of this service, and stops (with no error).
%
display_usage() ->
	ui:display( "~s", [ get_usage() ] ),
	basic_utils:stop( _ErrorCode=0 ).



% Reports an error related to command-line option, reminds the usage, and stops
% (on error).
%
stop_on_option_error( Message, ErrorCode ) ->
	ui:display_error( "Error, ~s~n~n~n~s", [ Message, get_usage() ] ),
	basic_utils:stop( ErrorCode ).




% Scans specified tree.
%
-spec scan( file_utils:directory_name() ) -> void().
scan( TreePath ) ->

	trace_utils:debug_fmt( "Request to scan '~s'.", [ TreePath ] ),

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	AbsTreePath = file_utils:ensure_path_is_absolute( TreePath ),

	% Best, reasonable CPU usage:
	Analyzers = spawn_data_analyzers( system_utils:get_core_count() + 1,
									  UserState ),

	AnalyzerRing = ring_utils:from_list( Analyzers ),

	TreeData = update_content_tree( AbsTreePath, AnalyzerRing, UserState ),

	_NewTreeData = diagnose_tree( TreeData, UserState ),

	terminate_data_analyzers( Analyzers, UserState ),

	stop_user_service( UserState ).



% Merges the (supposedly more up-to-date) input tree into the target, reference
% one.
%
-spec merge( file_utils:directory_name(), file_utils:directory_name() ) ->
				   void().
merge( InputTreePath, ReferenceTreePath ) ->

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	check_content_trees( InputTreePath, ReferenceTreePath ),

	trace( "Merging (possibly newer) tree '~s' into reference tree '~s'...",
		   [ InputTreePath, ReferenceTreePath ], UserState ),

	% Best, reasonable usage:
	Analyzers = spawn_data_analyzers( system_utils:get_core_count() + 1,
									  UserState ),

	AnalyzerRing = ring_utils:from_list( Analyzers ),

	update_content_tree( InputTreePath, AnalyzerRing, UserState ),

	terminate_data_analyzers( Analyzers, UserState ),

	stop_user_service( UserState ).





% Helpers.


% Starts user-related services.
%
-spec start_user_service( file_utils:file_name() ) -> user_state().
start_user_service( LogFilename ) ->

	trace_utils:debug_fmt( "Logs will be written to '~s'.", [ LogFilename ] ),

	% We append to the log file (not resetting it), if it already exists:
	LogFile = file_utils:open( LogFilename,
							   _Opts=[ append, raw, delayed_write ] ),

	file_utils:write( LogFile, "~nStarting new merge session on ~s "
					  "(version ~s) at ~s.~n",
					  [ net_utils:localhost(), ?merge_script_version,
						time_utils:get_textual_timestamp() ] ),

	#user_state{ log_file=LogFile }.



% Displays and logs specified text.
%
-spec trace( string(), user_state() ) -> user_state().
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



% Logs specified debug text.
%
-spec trace_debug( string(), user_state() ) -> user_state().
trace_debug( Message, _UserState={ UIState, LogFile } ) ->
	%NewUIState = ui:trace( Message, UIState ),
	file_utils:write( LogFile, Message ++ "\n" ),
	{ UIState, LogFile }.



% Logs specified debug formatted text.
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
stop_user_service( _UserState=#user_state{ log_file=LogFile } ) ->

	ui:stop(),

	file_utils:write( LogFile, "Stopping merge session.~n", [] ),

	file_utils:close( LogFile ).



% Checks that the source and target trees exist.
%
-spec check_content_trees( tree_data(), tree_data() ) -> void().
check_content_trees( InputTree, ReferenceTreePath ) ->

	case file_utils:is_existing_directory( InputTree ) of

		true ->
			ok;

		false ->
			throw( { non_existing_input_tree, InputTree } )

	end,

	case file_utils:is_existing_directory( ReferenceTreePath ) of

		true ->
			ok;

		false ->
			throw( { non_existing_reference_tree, ReferenceTreePath } )

	end.


% Returns the path of the cache file corresponding to the specified tree path.
%
-spec get_cache_path_for( file_utils:directory_name() ) ->
								file_utils:file_name().
get_cache_path_for( TreePath ) ->
	file_utils:join( TreePath, ?merge_cache_filename ).



% Ensures that specified tree path exists.
%
-spec check_tree_path_exists( file_utils:directory_name() ) -> void().
check_tree_path_exists( TreePath ) ->

	case file_utils:is_existing_directory( TreePath ) of

		true ->
			ok;

		false ->
			throw( { non_existing_content_tree, TreePath } )

	end.



% Updates specified content tree: verifies that it exists, that a merge cache
% file exists and is up to date (otherwise rebuilds it), and returns the
% corresponding datastructure.
%
-spec update_content_tree( file_utils:directory_name(), analyzer_ring(),
						   user_state() ) -> void().
update_content_tree( TreePath, AnalyzerRing, UserState ) ->

	CacheFilename = get_cache_path_for( TreePath ),

	case file_utils:is_existing_file( CacheFilename ) of

		true ->

			trace_utils:debug_fmt( "Using existing cache file '~s'.",
								   [ CacheFilename ] ),

			% Load it, if trusted (typically if not older from the newest
			% element in tree):
			%
			throw( fixme_2 );

		false ->
			create_merge_cache_file_for( TreePath, CacheFilename, AnalyzerRing,
										 UserState )

	end.



% Creates an automatically named merge cache file for specified content tree
% (overwriting any priorly existing merge cache file), and returns that tree.
%
-spec create_merge_cache_file_for( file_utils:directory_name(),
					   analyzer_ring(), user_state() ) -> tree_data().
create_merge_cache_file_for( TreePath, AnalyzerRing, UserState ) ->

	CacheFilename = get_cache_path_for( TreePath ),

	create_merge_cache_file_for( TreePath, CacheFilename, AnalyzerRing,
								 UserState ).



% Creates merge cache file with specified name, for specified content tree.
%
-spec create_merge_cache_file_for( file_utils:directory_name(),
		  file_utils:file_name(), analyzer_ring(), user_state() ) ->
										 tree_data().
create_merge_cache_file_for( TreePath, CacheFilename, AnalyzerRing,
							 UserState ) ->

	AbsTreePath = file_utils:ensure_path_is_absolute( TreePath ),

	check_tree_path_exists( AbsTreePath ),

	trace( "Creating merge cache file '~s'.", [ CacheFilename ], UserState ),

	MergeFile = file_utils:open( CacheFilename,
								 _Opts=[ write, raw, delayed_write ] ),

	%ScriptName = filename:basename( escript:script_name() ),
	ScriptName = ?MODULE,

	file_utils:write( MergeFile, "% Merge cache file written by '~s' "
								 "(version ~s):~n"
								 "% - on host '~s'~n"
								 "% - for content tree '~s'~n"
								 "% - on ~s~n",
					  [ ScriptName, ?merge_script_version,
						net_utils:localhost(), AbsTreePath,
						time_utils:get_textual_timestamp() ] ),

	_BlankDataTable = table:new(),

	_CurrentPosixTime = os:system_time(),

	TreeData = scan_tree( AbsTreePath, AnalyzerRing, UserState ),

	trace( "Scanned tree: " ++ tree_data_to_string( TreeData ), UserState ),

	file_utils:write( MergeFile, "~n~n% End of merge cache file (at ~s).",
					  [ time_utils:get_textual_timestamp() ] ),

	file_utils:close( MergeFile ),

	TreeData.




% Spawns the specified number of data analyzers, and returns their PID.
%
-spec spawn_data_analyzers( count(), user_state() ) -> [ analyzer_pid() ].
spawn_data_analyzers( Count, UserState ) ->
	trace_debug( "Spawning ~B data analyzers.", [ Count ], UserState ),
	[ spawn_link( fun() -> analyze_loop() end )
	  || _C <- lists:seq( 1, Count ) ].


% Terminates specified data analyzers.
%
-spec terminate_data_analyzers( [ analyzer_pid() ], user_state() ) ->
									  void().
terminate_data_analyzers( PidList, UserState ) ->
	trace_debug( "Terminating ~B data analyzers (~p).",
				 [ length( PidList ), PidList ], UserState ),
	[ P ! terminate || P <- PidList ].



% Scans for good the specified tree, whose path is expected to exist.
%
-spec scan_tree( file_utils:path(), analyzer_ring(), user_state() ) ->
					   tree_data().
scan_tree( AbsTreePath, AnalyzerRing, UserState ) ->

	trace( "Scanning tree '~s'...", [ AbsTreePath ], UserState ),

	AllFiles = file_utils:find_files_from( AbsTreePath ),

	% Not wanting to index our own files (if any already exists):
	FilteredFiles = lists:delete( ?merge_cache_filename, AllFiles ),

	trace_debug( "Found ~B files:~s", [ length( FilteredFiles ),
			text_utils:strings_to_string( FilteredFiles ) ], UserState ),

	% For lighter message sendings and storage:
	FilteredBinFiles = text_utils:strings_to_binaries( FilteredFiles ),

	scan_files( FilteredBinFiles, AbsTreePath, AnalyzerRing ).



% Scans specified content files, using for that the specified analyzers,
% returning the corresponding tree data.
%
-spec scan_files( [ file_utils:bin_file_name() ], file_utils:path(),
				  analyzer_ring() ) -> tree_data().
scan_files( Files, AbsTreePath, AnalyzerRing ) ->

	InitialTreeData = #tree_data{
						 root=text_utils:string_to_binary( AbsTreePath ) },

	scan_files( Files, AnalyzerRing, InitialTreeData, _WaitedCount=0 ).


scan_files( _Files=[], _AnalyzerRing, TreeData, _WaitedCount=0 ) ->
	% In final state (none waited), hence directly returned:
	%trace_info( "All file entries retrieved." ),
	TreeData;

scan_files( _Files=[], _AnalyzerRing, TreeData, WaitedCount ) ->
	% Will return an updated tree data, once all answers are received:
	%trace_info( "Final waiting for ~B entries.", [ WaitedCount ] ),
	wait_entries( TreeData, WaitedCount );

scan_files( _Files=[ Filename | T ], AnalyzerRing,
			TreeData=#tree_data{ root=AbsTreePath }, WaitedCount ) ->

	{ AnalyzerPid, NewRing } = ring_utils:head( AnalyzerRing ),

	%trace_debug( "Requesting analysis of '~s' by ~w.",
	%			 [ FullPath, AnalyzerPid ] ),

	% WOOPER-style request:
	AnalyzerPid ! { analyzeFile, [ AbsTreePath, Filename ], self() },

	% Helps controlling flow and avoiding too large mailboxes on either side
	% (this main script, being slowed down, or the analyzers), by attempting to
	% receive once after each sending:
	%
	receive

		{ file_analyzed, FileData } ->

			NewTreeData = manage_received_data( FileData, TreeData ),
			% Plus one (sending) minus one (receiving):
			scan_files( T, NewRing, NewTreeData, WaitedCount )

	after 0 ->

		% One sending, and no receiving here:
		scan_files( T, NewRing, TreeData, WaitedCount+1 )

	end.



% Manages specified received file data, and returns an updated tree data.
%
-spec manage_received_data( file_data(), tree_data() ) -> tree_data().
manage_received_data( FileData=#file_data{ type=Type, sha1_sum=Sum },
					  TreeData=#tree_data{ entries=Entries,
										   file_count=FileCount,
										   directory_count=DirCount,
										   symlink_count=SymlinkCount,
										   device_count=DeviceCount,
										   other_count=OtherCount } ) ->

	%trace_debug( "Data received: ~s",
	%			 [ file_data_to_string( FileData ) ] ),

	% Ensures that we associate a list to each SHA1 sum:
	NewEntries = case table:lookupEntry( Sum, Entries ) of

		key_not_found ->
			table:addEntry( Sum, [ FileData ], Entries );

		{ value, SumEntries } ->
			table:addEntry( Sum, [ FileData | SumEntries ], Entries )

	end,

	NewTreeData = TreeData#tree_data{ entries=NewEntries },

	case Type of

		regular ->
			NewTreeData#tree_data{ file_count=FileCount+1 };

		directory ->
			NewTreeData#tree_data{ directory_count=DirCount+1 };

		symlink ->
			NewTreeData#tree_data{ symlink_count=SymlinkCount+1 };

		device ->
			NewTreeData#tree_data{ device_count=DeviceCount+1 };

		other ->
			NewTreeData#tree_data{ other_count=OtherCount+1 }

	end.



% Waits for the remaining file entries to be analyzed.
%
wait_entries( TreeData, _WaitedCount=0 ) ->
	%trace_debug( "All file entries waited for finally obtained." ),
	TreeData;

wait_entries( TreeData, WaitedCount ) ->

	%trace_debug( "Still waiting for ~B file entries.", [ WaitedCount ] ),

	receive

		{ file_analyzed, FileData } ->
			NewTreeData = manage_received_data( FileData, TreeData ),
			wait_entries( NewTreeData, WaitedCount-1 )

	end.



% The loop run by each analyzer process.
%
-spec analyze_loop() -> void().
analyze_loop() ->

	%trace_debug( "Analyzer ~w waiting...", [ self() ] ),

	receive

		{ analyzeFile, [ AbsTreeBinPath, RelativeBinFilename ], SenderPid } ->

			AbsTreePath = text_utils:binary_to_string( AbsTreeBinPath ),

			RelativeFilename = text_utils:binary_to_string(
								 RelativeBinFilename ),

			FilePath = file_utils:join( AbsTreePath, RelativeFilename ),

			FileBinPath = text_utils:string_to_binary( FilePath ),

			%trace_debug( "Analyzer ~w taking in charge '~s'...",
			%			  [ self(), FullPath ] ),

			FileData = #file_data{
				path=FileBinPath,
				type=file_utils:get_type_of( FilePath ),
				size=file_utils:get_size( FilePath ),
				timestamp=file_utils:get_last_modification_time( FilePath ),
				sha1_sum=executable_utils:compute_sha1_sum( FilePath ) },

			SenderPid ! { file_analyzed, FileData },
			analyze_loop();

		terminate ->
			%trace_debug( "Analyzer ~w terminated.", [ self() ] ),
			ok

	end.



% Returns a textual diagnosis of specified tree.
%
-spec diagnose_tree( tree_data(), user_state() ) -> tree_data().
diagnose_tree( TreeData=#tree_data{ root=_RootDir,
								   entries=EntryTable,
								   file_count=FileCount }, UserState ) ->

	DuplicateCount = FileCount - table:size( EntryTable ),

	{ NewEntryTable, RemovedDuplicateCount } = manage_duplicates(
												   EntryTable, UserState ),

	RemainingDuplicateCount = DuplicateCount - RemovedDuplicateCount,

	case RemainingDuplicateCount of

		0 ->
			trace( "All ~B duplicates removed.", [ DuplicateCount ],
				   UserState );

		Count when Count > 0 ->
			trace( "Out of the ~B duplicates detected, ~B remain (~B removed).",
				   [ DuplicateCount, Count, RemovedDuplicateCount ],
				   UserState )

	end,

	NewFileCount = table:size( NewEntryTable ),

	trace( "~B unique entries remain.", [ NewFileCount ], UserState ),

	TreeData#tree_data{ entries=NewEntryTable,
						file_count=NewFileCount }.



% Manages all duplicates found in specified table, returns an updated table and
% the number of duplicates removed.
%
-spec manage_duplicates( sha1_table(), user_state() ) ->
							   { sha1_table(), count() }.
manage_duplicates( EntryTable, UserState ) ->

	ContentEntries = table:enumerate( EntryTable ),

	% We could have forced that no duplication at all exists afterwards (and
	% then a given SHA1 sum would be associated to exactly one content), however
	% it would be too strict, hence we kept a list associated to each SHA1 sum:
	%
	% Two passes: one to establish and count the duplications, another to solve
	% them; returns a list of duplications, and a content table referencing all
	% non-duplicated entries.
	%
	{ DuplicationCases, UniqueTable } = filter_duplications( ContentEntries ),

	case length( DuplicationCases ) of

		0 ->
			ui:display( "No duplicated content detected.~n" ),
			{ UniqueTable, _RemoveCount=0 };


		TotalDupCaseCount ->
			ui:display( "~B case(s) of content duplication detected, "
						"examining them in turn.~n",
						[ TotalDupCaseCount ] ),

			process_duplications( DuplicationCases, TotalDupCaseCount,
								  UniqueTable, UserState )

	end.



% Filters the duplications from specified content entries: returns the actual
% duplications in a list, put the unique files in a new table.
%
-spec filter_duplications( [ sha1_entry() ] ) ->
								 { [ sha1_entry() ], sha1_table() }.
filter_duplications( SHA1Entries ) ->
	% Far better than a fold:
	filter_duplications( SHA1Entries, _Acc={ [], table:new() } ).


% Returns { AccDupEntries, AccUniqueTable }:
%
filter_duplications( _SHA1Entry=[], Acc ) ->
	Acc;

% By design V is never empty:
filter_duplications( _SHA1Entry=[ { Sha1Key, V=[ _SingleContent ] } | T ],
				   _Acc={ AccDupEntries, AccUniqueTable } ) ->
	% Single content, hence unique:
	NewTable = table:addEntry( Sha1Key, V, AccUniqueTable ),
	filter_duplications( T, { AccDupEntries, NewTable } );

% SHA1Entry is { Sha1Key, V } with at least two elements in V here:
filter_duplications( _SHA1Entries=[ SHA1Entry | T ],
					 _Acc={ AccDupEntries, AccUniqueTable } ) ->
	% At least one duplicate here:
	NewDupEntries = [ SHA1Entry | AccDupEntries ],
	filter_duplications( T, { NewDupEntries, AccUniqueTable } ).



% Processes the spotted duplications by asking the user.
%
-spec process_duplications( [ sha1_entry() ], count(), sha1_table(),
							user_state() ) -> { sha1_table(), count() }.
process_duplications( DuplicationCases, TotalDupCaseCount, UniqueTable,
					  UserState ) ->
	Acc0 = { UniqueTable, _InitialDupCount=1, _InitialRemoved=0 },
	process_duplications_helper( DuplicationCases, TotalDupCaseCount, Acc0,
								 UserState ).



% (helper)
process_duplications_helper( _DupCases=[], _TotalDupCount,
							 _Acc={ AccTable, _AccDupCount, AccRemoveCount },
							 _UserState ) ->
	{ AccTable, AccRemoveCount };

process_duplications_helper( _DupCases=[ { Sha1Key, DuplicateList } | T ],
							 TotalDupCount,
							 _Acc={ AccTable, AccDupCount, AccRemoveCount },
							 UserState ) ->

	Size = check_duplicates( Sha1Key, DuplicateList ),
	SelectedFileEntries = manage_duplication( DuplicateList, AccDupCount,
							  TotalDupCount, Size, UserState ),
	NewAccTable = table:addEntry( Sha1Key, SelectedFileEntries, AccTable ),
	NewRemoveCount = AccRemoveCount + length( DuplicateList )
		- length( SelectedFileEntries ),
	NewAcc = { NewAccTable, AccDupCount+1, NewRemoveCount },
	process_duplications_helper( T, TotalDupCount, NewAcc, UserState ).



% Checks a duplication set: same SHA1 sum and also size must be found for all
% file entries (would most probably detect any SHA1 collision, however unlikely
% it maybe).
%
-spec check_duplicates( sha1(), [ file_data() ] ) -> basic_utils:void().
% Not possible: check_duplicates( _SHA1Sum, _DuplicateList=[] ) ->
%	ok;

% Use the first element to determine the (common) size:
check_duplicates( SHA1Sum, _DuplicateList=[
	   #file_data{ path=FirstPath, sha1_sum=SHA1Sum, size=Size } | T ] ) ->
	check_duplicates( SHA1Sum, FirstPath, Size, T ).


% (helper)
check_duplicates( _SHA1Sum, _FirstPath, Size, _DuplicateList=[] ) ->
	Size;

check_duplicates( SHA1Sum, FirstPath, Size, _DuplicateList=[
	   #file_data{ sha1_sum=SHA1Sum, size=Size } | T ] ) ->
	check_duplicates( SHA1Sum, FirstPath, Size, T );

check_duplicates( SHA1Sum, FirstPath, Size, _DuplicateList=[
	   #file_data{ path=OtherPath, sha1_sum=SHA1Sum, size=OtherSize }
															| _T ] ) ->
	throw( { sha1_collision_detected, SHA1Sum, { FirstPath, Size },
			 { OtherPath, OtherSize } } ).




% Manages specified duplicated entries.
%
-spec manage_duplication( [ file_data() ], count(), count(),
					system_utils:byte_size(), user_state() ) -> [ file_data() ].
manage_duplication( FileEntries, DuplicationCaseCount, TotalDupCaseCount, Size,
					UserState ) ->

	SizeString = system_utils:interpret_byte_size_with_unit( Size ),

	DuplicateString = text_utils:binaries_to_string(
						[ E#file_data.path || E <- FileEntries ] ),

	ui:add_separation(),

	Label = text_utils:format( "Examining duplication case ~B/~B: "
							   "following ~B files have the exact same "
							   "content (and thus size, of ~s): ~s~n~n"
							   "Choices are:",
							   [ DuplicationCaseCount, TotalDupCaseCount,
								 length( FileEntries ), SizeString,
								 DuplicateString ] ),


	Choices = [ { 'l', "leave them as they are" },
				{ 'e', "elect a reference file, replacing each other by "
				  "a symbolic link pointing to it" },
				{ 'a', "abort" } ],

	SelectedChoice = ui:choose_designated_item( Label, Choices ),

	trace( "Selected choice: ~p", [ SelectedChoice ], UserState ),

	case SelectedChoice of

		'l' ->
			trace( "[~B/~B] Leaving as they are:~s",
				   [ DuplicationCaseCount, TotalDupCaseCount,
					 DuplicateString ], UserState ),
			FileEntries;

		'e' ->
			trace( "TODO", UserState ),
			FileEntries;

		'a' ->
			trace( "(request to abort the merge)", UserState ),
			basic_utils:stop( 5 )

	end.



% Returns a textual description of specified tree data.
%
-spec tree_data_to_string( tree_data() ) -> string().
tree_data_to_string( #tree_data{ root=RootDir,
								 entries=Table,
								 file_count=FileCount,
								 directory_count=_DirCount,
								 symlink_count=_SymlinkCount,
								 device_count=_DeviceCount,
								 other_count=_OtherCount } ) ->

	% Only looking for files:
	%text_utils:format( "tree '~s' having ~B entries (~B files, ~B directories,"
	%				   " ~B symbolic links)",
	%				   [ RootDir, table:size( Table ), FileCount, DirCount,
	%					 SymlinkCount ] ).

	case table:size( Table ) of

		FileCount ->
			text_utils:format( "tree '~s' having ~B files, "
							   "each with unique content",
							   [ RootDir, FileCount ] );

		ContentCount ->
			text_utils:format( "tree '~s' having ~B files, corresponding "
							   "only to ~B different contents "
							   "(hence there are ~B duplicates)",
							   [ RootDir, FileCount, ContentCount,
								 FileCount -  ContentCount ] )

	end.



% Returns a textual description of specified file data.
%
-spec file_data_to_string( file_data() ) -> string().
file_data_to_string( #file_data{
						 path=Path,
						 size=Size,
						 timestamp=Timestamp,
						 sha1_sum=Sum } ) ->

	SizeString = system_utils:interpret_byte_size_with_unit( Size ),

	text_utils:format( "file '~s' whose size is ~s, SHA1 sum is ~s and "
					   "timestamp is ~p",
					   [ Path, SizeString, Sum, Timestamp ] ).
