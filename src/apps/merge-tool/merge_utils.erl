% Copyright (C) 2016-2019 Olivier Boudeville
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
-define( merge_script_version, "0.0.3" ).


-define( default_log_filename, "merge-tree.log" ).


-export([ create_merge_cache_file_for/3,
		  tree_data_to_string/1, file_data_to_string/1,
		  trace/2, trace/3, trace_debug/2, trace_debug/3 ]).


% Shorthands:
-type sha1() :: executable_utils:sha1_sum().
-type count() :: basic_utils:count().

-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().

-type directory_path() :: file_utils:directory_path().
-type file() :: file_utils:file().


% Data associated to a given file-like element.
%
% Note: these records are typically values stored in tables, whose associated
% key is potentially duplicating their path (not a problem).
%
-record( file_data, {

		   % Path of this file (an identifier thereof), relative to the tree
		   % root:
		   %
		   path :: file_utils:bin_path(),

		   % Type of the file element:
		   type :: file_utils:entry_type(),

		   % Precise size, in bytes, of that file:
		   size :: system_utils:byte_size(),

		   % Timestamp of the last content modification of this file, as known
		   % of the filesystem, and as an integer number of seconds since
		   % 1970-01-01 00:00 UTC:
		   %
		   timestamp :: time_utils:posix_seconds(),

		   % SHA1 sum of the content of that file:
		   sha1_sum :: sha1()

}).

-type file_data() :: #file_data{}.



% Table referencing file entries based on their SHA1:
%
% (a list of exactly one file_data record per SHA1 key, once the tree is
% uniquified)
%
-type sha1_table() :: table( sha1(), [ file_data() ] ).

%-type sha1_set() :: set_utils:set( sha1() ).

% Pair entries of a sha1_table/0:
-type sha1_entry() :: { sha1(), [ file_data() ] }.


% Data associated to a content tree.
-record( tree_data, {

		   % Base, absolute (binary) path of the root of that tree in the
		   % filesystem:
		   %
		   root :: directory_path(),

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

	% File handle (if any) to write logs:
	log_file = undefined :: maybe( file() )

}).


% User-related state:
-type user_state() :: #user_state{}.



% In order to run from the interpreter rather than through an escript:
-export([ run/0, scan/3, main/1 ]).


% The PID of an analyzer process:
-type analyzer_pid() :: pid().


% Ring of analyzer processes:
-type analyzer_ring() :: ring_utils:ring( analyzer_pid() ).


% This script depends on the 'Myriad' layer, and only on that code.
%
% Note: ensure it is already built first!


% Not run anymore as an escript, as raised issues with term_ui (i.e. dialog):
%-define( exec_name, "merge-tree.escript" ).
-define( exec_name, "merge.sh" ).


% The subdirectory in the reference tree in which selected content will be
% transferred:
%
-define( merge_dir, "content_from_merge" ).


% For myriad_spawn*:
-include("spawn_utils.hrl").


-spec get_usage() -> void().
get_usage() ->
	" Usage: following operations can be triggered: \n"
	"  - '"?exec_name" --input INPUT_TREE --reference REFERENCE_TREE'\n"
	"  - '"?exec_name" --scan A_TREE'\n"
	"  - '"?exec_name" --uniquify A_TREE'\n"
	"  - '"?exec_name" -h' or '"?exec_name" --help'\n\n"
	"   Ensures, for the first form, that all the changes in a possibly more up-to-date, \"newer\" tree (INPUT_TREE) are merged back to the reference tree (REFERENCE_TREE), from which the first tree may have derived. Once executed, only a refreshed, complemented reference tree will exist, as the input tree will have been removed: all its original content (i.e. its content that was not already in the reference tree) will have been transferred in the reference tree.\n"
	"   In the reference tree, in-tree duplicated content will be either kept as it is, or removed as a whole (to keep only one copy thereof), or replaced by symbolic links in order to keep only a single reference version of each actual content.\n"
	"   At the root of the reference tree, a '" ?merge_cache_filename "' file will be stored, in order to avoid any later recomputations of the checksums of the files that it contains, should they not have changed. As a result, once a merge is done, the reference tree may contain an uniquified version of the union of the two specified trees, and the tree to scan will not exist anymore.\n\n"
	"   For the second form (--scan option), the specified tree will simply be inspected for duplicates, and a corresponding '" ?merge_cache_filename "' file will be created at its root (to be potentially reused by a later operation).\n\n"
	"   For the third form (--uniquify option), the specified tree will be scanned first (see previous operation), and then the user will be offered various actions regarding found duplicates (being kept as are, or removed, or replaced with symbolic links), and once done a corresponding '" ?merge_cache_filename "' file will be created at its root (to be potentially reused by a later operation).\n\n"
	"   For the fourth form (-h / --help option), displays this help\n\n"
	"   When a cache file is found, it can be either ignored (and thus recreated) or re-used, either as it is or after a weak check, where only sizes and timestamps are then verified.".



% Typically for testing:
-spec run() -> void().
run() ->
	ArgTable = executable_utils:get_argument_table(),
	main( ArgTable ).



% Sole entry point for this merge service, either triggered by run/0 or by the
% associated escript.
%
-spec main( executable_utils:argument_table() ) -> void().
main( ArgTable ) ->

	%trace_utils:info( "Running..." ),

	UIOptions = [ log_console ],
	%UIOptions = [],

	FilteredArgTable = ui:start( UIOptions, ArgTable ),

	%trace_utils:debug_fmt( "Script-specific argument(s): ~s",
	%	   [ executable_utils:argument_table_to_string( FilteredArgTable ) ] ),

	case list_table:has_entry( 'h', FilteredArgTable )
		orelse list_table:has_entry( '-help', FilteredArgTable ) of

		true ->
			display_usage();

		false ->
			case list_table:extract_entry_with_defaults( '-reference',
									 undefined, FilteredArgTable ) of

				{ [ RefTreePath ], NoRefArgTable }
				  when is_list( RefTreePath ) ->
					handle_reference_option( RefTreePath, NoRefArgTable );

				{ undefined, NoRefArgTable } ->
					handle_non_reference_option( NoRefArgTable );

				% Typically more than one reference option specified:
				{ UnexpectedRefTreeOpts, _NoRefArgTable } ->
					RefString = text_utils:format(
								  "unexpected reference tree options: ~p",
								  [ UnexpectedRefTreeOpts ] ),
					stop_on_option_error( RefString, 26 )

			end


	end.



% Handles the command-line whenever the --reference option was specified, with a
% single corresponding parameter, of type list.
%
handle_reference_option( RefTreePath, ArgumentTable ) ->

	ui:set_settings( [ { title, "Merging" },
					   { backtitle, "Merging now..." } ] ),

	% If there is a --reference option, it is a merge, and there must be a
	% --input option as well:

	case list_table:extract_entry_with_defaults( '-input', undefined,
												 ArgumentTable ) of

		{ undefined, NewArgumentTable } ->
			InputString = text_utils:format(
			  "no input tree specified; options were: ~s",
			  [ executable_utils:argument_table_to_string(
				  NewArgumentTable ) ] ),
			stop_on_option_error( InputString, 23 );


		% Here, an input tree was specified as well:
		{ [ InputTreePath ], NewArgumentTable } when is_list( InputTreePath ) ->

			%trace_utils:debug_fmt( "InputTreePath: ~p", [ InputTreePath ] ),

			% Check that no unknown option remains:
			case list_table:is_empty( NewArgumentTable ) of

				true ->
					merge( InputTreePath, RefTreePath );

				false ->
					Msg = text_utils:format(
							"unexpected extra options specified: ~s",
							[ executable_utils:argument_table_to_string(
								NewArgumentTable ) ] ),
					stop_on_option_error( Msg, 24 )

			end;


		% Typically more than one input option specified:
		{ UnexpectedInputTreeOpts, _NewArgumentTable } ->
			InputString = text_utils:format( "unexpected --input options: ~s",
											 [ UnexpectedInputTreeOpts ] ),
			stop_on_option_error( InputString, 27 )


	end.



% Handles the command-line whenever the --reference option was not specified.
handle_non_reference_option( ArgumentTable ) ->

	% No reference, it must then either be a pure scan or a uniquify here:
	case list_table:extract_entry_with_defaults( '-scan', undefined,
												 ArgumentTable ) of

		% Not a scan, then a uniquify?
		{ undefined, NoScanArgTable } ->

			case list_table:extract_entry_with_defaults( '-uniquify', undefined,
														 NoScanArgTable ) of

				{ undefined, NoUniqArgTable } ->

					AddedString = case list_table:is_empty( NoUniqArgTable ) of

						true ->
							" (no command-line option specified)";

						false ->
							"; instead: " ++
								executable_utils:argument_table_to_string(
								  NoScanArgTable )

							end,

					Msg = text_utils:format( "no operation specified~s",
											 [ AddedString ] ),

					stop_on_option_error( Msg, 20 );


				{ [ UniqTreePath ], NoUniqArgTable }
				  when is_list( UniqTreePath ) ->

					% Check no unknown option remains:
					case list_table:is_empty( NoUniqArgTable ) of

						true ->
							uniquify( UniqTreePath );

						false ->
							Msg = text_utils:format(
									"unexpected extra options specified: ~s",
									[ executable_utils:argument_table_to_string(
										NoUniqArgTable ) ] ),
							stop_on_option_error( Msg, 21 )

					end;


				{ UnexpectedUniqTreeOpts, _NoUniqArgTable } ->
					UniqString = text_utils:format(
								   "unexpected scan tree options: ~p",
								   [ UnexpectedUniqTreeOpts ] ),

					stop_on_option_error( UniqString, 22 )

			end;


		% A scan was requested:
		{ [ ScanTreePath ], ScanArgTable }  when is_list( ScanTreePath ) ->

			% Check no unknown option remains:
			case list_table:is_empty( ScanArgTable ) of

				true ->
					% Prepare for various outputs:
					UserState = start_user_service( ?default_log_filename ),

					AnalyzerRing = create_analyzer_ring( UserState ),

					scan( ScanTreePath, AnalyzerRing, UserState ),

					terminate_data_analyzers(
					  ring_utils:to_list( AnalyzerRing ), UserState ),

					stop_user_service( UserState ),

					basic_utils:stop( _ErrorCode=0 );

				false ->
					Msg = text_utils:format(
							"unexpected extra options specified: ~s",
							[ executable_utils:argument_table_to_string(
								ScanArgTable ) ] ),
					stop_on_option_error( Msg, 23 )

			end;

		{ UnexpectedScanTreeOpts, _ScanArgTable } ->
			ScanString = text_utils:format( "unexpected scan tree options: ~p",
											[ UnexpectedScanTreeOpts ] ),
			stop_on_option_error( ScanString, 24 )

	end.



% Displays the usage of this service, and stops (with no error).
display_usage() ->
	ui:display( "~s", [ get_usage() ] ),
	basic_utils:stop( _ErrorCode=0 ).



% Reports an error related to command-line option, reminds the usage, and stops
% (on error).
%
stop_on_option_error( Message, ErrorCode ) ->
	ui:display_error( "Error, ~s.~n~n~s", [ Message, get_usage() ] ),
	basic_utils:stop( ErrorCode ).




% Scans specified tree, returning the corresponding datastructure.
-spec scan( file_utils:directory_name(), analyzer_ring(), user_state() ) ->
				  tree_data().
scan( TreePath, AnalyzerRing, UserState ) ->

	trace_utils:debug_fmt( "Requested to scan '~s'.", [ TreePath ] ),

	AbsTreePath = file_utils:ensure_path_is_absolute( TreePath ),

	ui:set_settings( [ { 'backtitle',
					 text_utils:format( "Scan of ~s", [ AbsTreePath ] ) },
					   { 'title', "Scan report" } ] ),

	CacheFilename = get_cache_path_for( TreePath ),

	case file_utils:is_existing_file( CacheFilename ) of

		true ->

			Label = text_utils:format( "A cache file already exists for '~s'. "
									   "We can:", [ TreePath ] ),

			% No 'strong_check' deemed useful (synonym of recreating from
			% scratch, hence of 'ignore').

			Choices = [
				{ weak_check, "Re-use this file, provided that it passes "
				  "a weak check (based on sizes and timestamps), otherwise "
				  "recreate it" },
				{ ignore, "Ignore this version, and recreate this file "
				  "unconditionally" },
				{ no_check, "Re-use this file as it is, with no specific "
				  "check involved" },
				{ abort, "Abort scan" } ],

			case ui:choose_designated_item( Label, Choices ) of

				weak_check ->
					% No need to restate the tree, is in the path of the cache
					% file:
					%
					ui:display( "Performing a weak check of '~s'.",
								[ CacheFilename ] ),
					update_content_tree( TreePath, AnalyzerRing, UserState );

				ignore ->
					ui:display( "Ignoring existing cache file (~s), "
								"performing now a full scan to recreate it.",
								[ CacheFilename ] ),
					perform_scan( TreePath, CacheFilename, UserState );

				no_check ->
					ui:display( "Re-using '~s' with no specific check.",
								[ CacheFilename ] ),
					read_cache_file( CacheFilename );

				abort ->
					ui:display( "Scan aborted, cache file (~s) left as it was.",
								[ CacheFilename ] ),
					trace( "(requested to abort the scan)", UserState ),
					basic_utils:stop( 0 )

			end;

		false ->
			ui:display( "No cache file (~s) found, performing full scan "
						"to recreate it.", [ CacheFilename ] ),
			perform_scan( TreePath, CacheFilename, UserState )

	end.


% (helper)
perform_scan( TreePath, CacheFilename, UserState ) ->

	TreeData = scan_helper( TreePath, CacheFilename, UserState ),

	ui:display( "Scan result stored in '~s': ~s",
				[ CacheFilename, tree_data_to_string( TreeData ) ] ),

	TreeData.



% Reads as it is specified cache file, and returns the corresponding tree data.
%
% Note that the cache file is expected to exist.
%
-spec read_cache_file( file_utils:file_path() ) -> tree_data().
read_cache_file( CacheFilename ) ->

	[ _RootInfo={ root, CachedTreePath } | FileInfos ] =
		file_utils:read_terms( CacheFilename ),

	#tree_data{ root=CachedTreePath,
				entries=build_entry_table( FileInfos ),
				file_count=length( FileInfos )
				% Not managed (at least yet): the other counts.
			  }.


% (helper)
create_analyzer_ring( UserState ) ->

	% Best, reasonable CPU usage:
	Analyzers = spawn_data_analyzers( system_utils:get_core_count() + 1,
									  UserState ),

	% Returns the ring:
	ring_utils:from_list( Analyzers ).



% Actual scanning of specified path, producing specified cache file from
% scratch.
%
scan_helper( TreePath, CacheFilename, UserState ) ->

	AnalyzerRing = create_analyzer_ring( UserState ),

	TreeData = create_merge_cache_file_for( TreePath, CacheFilename,
											AnalyzerRing, UserState ),

	trace_utils:debug( "Scan finished." ),

	terminate_data_analyzers( ring_utils:to_list( AnalyzerRing ), UserState ),

	TreeData.



% Uniquifies specified tree.
-spec uniquify( file_utils:directory_name() ) -> void().
uniquify( TreePath ) ->

	trace_utils:debug_fmt( "Requested to uniquify '~s'.", [ TreePath ] ),

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	AbsTreePath = file_utils:ensure_path_is_absolute( TreePath ),

	ui:set_settings( [ { 'backtitle', text_utils:format(
								 "Uniquification of ~s", [ AbsTreePath ] ) },
					   { 'title', "Uniquification report" } ] ),

	% Best, reasonable CPU usage:
	Analyzers = spawn_data_analyzers( system_utils:get_core_count() + 1,
									  UserState ),

	AnalyzerRing = ring_utils:from_list( Analyzers ),

	TreeData = update_content_tree( AbsTreePath, AnalyzerRing, UserState ),

	NewTreeData = deduplicate_tree( TreeData, UserState ),

	trace_debug( "Uniquification finished, resulting on following tree: ~s",
				 [ tree_data_to_string( NewTreeData ) ], UserState ),

	terminate_data_analyzers( Analyzers, UserState ),

	% We leave an up-to-date cache file:
	create_merge_cache_file_from( NewTreeData, UserState ),

	stop_user_service( UserState ),

	basic_utils:stop( 0 ).



% Creates (typically after a tree update) a merge cache file (overwriting any
% prior one) with specified name, for specified content tree.
%
-spec create_merge_cache_file_from( tree_data(), user_state() ) ->
										  file_path().
create_merge_cache_file_from( TreeData=#tree_data{ root=RootDir },
							  UserState ) ->

	CacheFilename = get_cache_path_for( RootDir ),

	CacheFile = file_utils:open( CacheFilename,
								 _Opts=[ write, raw, delayed_write ] ),

	write_cache_header( CacheFile ),

	write_tree_data( CacheFile, TreeData, UserState ),

	write_cache_footer( CacheFile ),

	file_utils:close( CacheFile ),

	CacheFilename.



% Merges the (supposedly more up-to-date) input tree into the target, reference
% one.
%
-spec merge( file_utils:directory_name(), file_utils:directory_name() ) ->
				   void().
merge( InputTreePath, ReferenceTreePath ) ->

	trace_utils:debug_fmt( "Requested to merge '~s' into '~s'.",
						   [ InputTreePath, ReferenceTreePath ] ),

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	trace( "Merging (possibly newer) tree '~s' into reference tree '~s'...",
		   [ InputTreePath, ReferenceTreePath ], UserState ),

	check_content_trees( InputTreePath, ReferenceTreePath ),

	% Best, reasonable usage:
	Analyzers = spawn_data_analyzers( system_utils:get_core_count() + 1,
									  UserState ),

	AnalyzerRing = ring_utils:from_list( Analyzers ),

	InputTree = update_content_tree( InputTreePath, AnalyzerRing, UserState ),

	ReferenceTree = update_content_tree( ReferenceTreePath, AnalyzerRing,
										 UserState ),

	merge_trees( InputTree, ReferenceTree, UserState ),

	terminate_data_analyzers( Analyzers, UserState ),

	stop_user_service( UserState ).


% Merges the specified input tree into the reference one.
-spec merge_trees( tree_data(), tree_data(), user_state() ) -> tree_data().
merge_trees( _InputTree=#tree_data{ root=InputRootDir,
									entries=InputEntries },
			 ReferenceTree=#tree_data{ root=ReferenceRootDir,
									   entries=ReferenceEntries },
			 UserState ) ->

	InputSHA1Set = set_utils:new( table:keys( InputEntries ) ),
	ReferenceSHA1Set = set_utils:new( table:keys( ReferenceEntries ) ),

	LackingInRefSet = set_utils:difference( InputSHA1Set, ReferenceSHA1Set ),

	ui:set_setting( 'backtitle', text_utils:format( "Merging in ~s",
													[ ReferenceRootDir ] ) ),

	ToMerge = set_utils:to_list( LackingInRefSet ),

	TargetDir = file_utils:join( ReferenceRootDir, ?merge_dir ),

	case set_utils:size( LackingInRefSet ) of

		0 ->
			ui:display( "The content of the input tree path ('~s') is strictly "
						"included into the one of the reference tree ('~s'), "
						"hence nothing special is to merge, "
						"removing directly the input tree.",
						[ InputRootDir, ReferenceRootDir ] ),

			%trace_utils:warning_fmt( "Removing recursively directory '~s'.",
			%						 [ InputRootDir ] );
			file_utils:remove_directory( InputRootDir ),
			ReferenceTree;

		LackingCount ->
			Label = text_utils:format( "Exactly ~B contents are present in the "
									   "input tree ('~s') but are lacking in "
									   "the reference one ('~s').",
									   [ LackingCount, InputRootDir,
										 ReferenceRootDir ] ),

			Choices = [ { move, "Move this content (once uniquified) "
						  "as a whole in the reference tree" },
						{ cherry_pick,
						  "Cherry-pick which content to move to reference tree "
						  "or to delete" },
						{ delete, "Delete as a whole this content (it will "
						  "thus be permanently lost afterwards)" },
						{ abort, "Abort merge" } ],

			NewReferenceEntries = case ui:choose_designated_item(
					   text_utils:format( "~s~nChoices are:", [ Label ] ),
					   Choices ) of

				move ->
					file_utils:create_directory_if_not_existing( TargetDir ),
					move_content_to_merge( ToMerge, InputRootDir, InputEntries,
										   ReferenceRootDir, ReferenceEntries,
										   TargetDir, UserState );

				cherry_pick ->
					file_utils:create_directory_if_not_existing( TargetDir ),
					cherry_pick_content_to_merge( ToMerge,
							InputRootDir, InputEntries, ReferenceRootDir,
							ReferenceEntries, TargetDir, UserState );

				delete ->
					delete_content_to_merge( ToMerge, InputRootDir,
											 InputEntries, UserState ),
					ReferenceEntries;

				abort ->
					trace( "(requested to abort the merge)", UserState ),
					basic_utils:stop( 0 )

			end,

			ReferenceTree#tree_data{ entries=NewReferenceEntries }

	end.



% Moves all specified content in the reference tree, and returns an updated view
% thereof.
%
-spec move_content_to_merge( [ sha1() ], directory_path(), sha1_table(),
		 directory_path(), sha1_table(), directory_path(), user_state() ) ->
							sha1_table().
move_content_to_merge( _ToMove=[], InputRootDir, InputEntries,
				_ReferenceRootDir, ReferenceEntries, _TargetDir, _UserState ) ->

	% Removing the content that does not have been moved (hence shall be
	% deleted):
	%
	file_utils:remove_files( list_utils:flatten_once( [
		  [ file_utils:join( InputRootDir, FD#file_data.path ) || FD <- FDL ]
							  || FDL <- table:values( InputEntries ) ] ) ),

	% Input tree shall be now void of content and thus removed:
	file_utils:remove_file( get_cache_path_for( InputRootDir ) ),
	file_utils:remove_empty_tree( InputRootDir ),

	ReferenceEntries;

move_content_to_merge( _ToMove=[ SHA1 | T ], InputRootDir, InputEntries,
				ReferenceRootDir, ReferenceEntries, TargetDir, UserState ) ->

	% Single element, as expected to be uniquified:
	{ FileDatas, NewInputEntries } =
		table:extract_entry( SHA1, InputEntries ),

	ElectedFileData = case FileDatas of

		[ FileData ] ->
			FileData;

		_ ->
			Label = text_utils:format( "~B files correspond to the same input "
					"content; please select the unique one that shall be "
					"copied in the reference tree:", [ length( FileDatas ) ] ),

			Choices = [ FD#file_data.path || FD <- FileDatas ],

			DefaultChoiceIndex = 1,

			Index = ui:choose_numbered_item_with_default( Label, Choices,
														  DefaultChoiceIndex ),

			{ Selected, Others } = list_utils:extract_element_at( FileDatas,
																  Index ),
				ToRemove = [ file_utils:join( InputRootDir,
							   FD#file_data.path ) || FD <- Others ],

				file_utils:remove_files( ToRemove ),

				trace_utils:trace_fmt( "Removed ~B file(s): ~s",
					   [ length( ToRemove ),
						 text_utils:strings_to_string( ToRemove ) ] ),

				Selected

	end,

	RelPath = ElectedFileData#file_data.path,

	% With a check:
	SHA1 = ElectedFileData#file_data.sha1_sum,

	SrcPath = file_utils:join( InputRootDir, RelPath ),

	% We do not preserve the directory structure of the input tree; this may be
	% wanted, though.

	Filename = filename:basename( RelPath ),
	TgtPath = file_utils:join( TargetDir, Filename ),

	% As clashes may happen in the elected target directory:
	NewPath = case file_utils:exists( TgtPath ) of

		true ->
			AutoPath = file_utils:get_non_clashing_entry_name_from( TgtPath ),
			%Msg = text_utils:format( "When moving '~s' in the reference target"
			%						 " directory ('~s'), an entry with that "
			%						 "was found already existing.~n"
			%						 "Shall we rename it automatically to ~s, "
			%						 "or ask for a new name?",
			%						 [ Filename, TargetDir, AutoPath ] ),
			%ui:ask_yes_no( "

			% At least for the moment, we stick to auto-renaming only; returning
			% the new path:
			%
			file_utils:move_file( SrcPath, AutoPath );

		false ->
			file_utils:move_file( SrcPath, TgtPath )

	end,

	trace_utils:trace_fmt( "Moved '~s' to '~s'.", [ SrcPath, NewPath ] ),

	% Make the new path relative to the root of the reference tree (TargetDir
	% being itself relative to it):
	%
	NewRelPath = case string:prefix( NewPath, _Prefix=ReferenceRootDir ) of

		nomatch ->
			throw( { relative_path_not_found, ReferenceRootDir, NewPath } );

		TrailingPath ->
			TrailingPath

	end,

	% Selective update:
	NewFileData = ElectedFileData#file_data{
			path=NewRelPath,
			% To avoid any kind of discrepancy:
			timestamp=file_utils:get_last_modification_time( NewPath ) },

	NewReferenceEntries =
		table:add_new_entry( SHA1, NewFileData, ReferenceEntries ),

	move_content_to_merge( T, InputRootDir, NewInputEntries, ReferenceRootDir,
						   NewReferenceEntries, TargetDir, UserState ).



% Selects which of the specified elements among the input entries shall be
% merged in the reference content, and how.
%
-spec cherry_pick_content_to_merge( [ sha1() ], directory_path(), sha1_table(),
		   directory_path(), sha1_table(), directory_path(), user_state() ) ->
										  sha1_table().
cherry_pick_content_to_merge( ToPick, InputRootDir, InputEntries,
				  ReferenceRootDir, ReferenceEntries, TargetDir, UserState ) ->

	TotalContentCount = length( ToPick ),

	PickChoices = [ { move, text_utils:format( "Move this "
						   "content in reference tree (in '~s')",
											   [ ReferenceRootDir ] ) },
					{ delete, "Delete this content" },
					{ abort, "Abort merge" } ],

	cherry_pick_files( ToPick, InputRootDir, InputEntries, ReferenceRootDir,
					   ReferenceEntries, TargetDir, PickChoices, _Count=1,
					   TotalContentCount, UserState ).



% Allows the user to cherry-pick the files that shall be copied (others being
% removed).
%
cherry_pick_files( _ToPick=[], InputRootDir, _InputEntries, _ReferenceRootDir,
				   ReferenceEntries, _TargetDir, _PickChoices, _Count,
				   _TotalContentCount, _UserState ) ->

	% Input tree shall be now void of content and thus removed:
	file_utils:remove_file( get_cache_path_for( InputRootDir ) ),
	file_utils:remove_empty_tree( InputRootDir ),

	ReferenceEntries;

cherry_pick_files( _ToPick=[ _SHA1 | T ], InputRootDir, InputEntries,
				   ReferenceRootDir, ReferenceEntries, TargetDir, PickChoices,
				   Count, TotalContentCount, UserState ) ->

	throw( todo_cherry_pick ),

	cherry_pick_files( T, InputRootDir, InputEntries,
					   ReferenceRootDir, ReferenceEntries, TargetDir,
					   PickChoices, Count, TotalContentCount, UserState ).



% Deletes all specified content.
%
% Does it on a per-content basic rather than doing nothing before the input tree
% is removed as whole, as more control is preferred (to check the final input
% tree has been indeed emptied of all content).
%
-spec delete_content_to_merge( [ sha1() ], directory_path(), sha1_table(),
						user_state() ) -> sha1_table().
delete_content_to_merge( ToDelete, InputRootDir, InputEntries, _UserState ) ->

	Paths = [ begin
				  FileData = table:get_value( SHA1, InputEntries ),
				  file_utils:join( InputRootDir, FileData#file_data.path )
			  end || SHA1 <- ToDelete ],

	file_utils:remove_files( [ get_cache_path_for( InputRootDir ) | Paths ] ),

	% Shall be now void of content:
	file_utils:remove_empty_tree( InputRootDir ).




% Helpers.


% Starts user-related services.
-spec start_user_service( file_utils:file_name() ) -> user_state().
start_user_service( LogFilename ) ->

	trace_utils:debug_fmt( "Logs will be written to '~s'.", [ LogFilename ] ),

	% We append to the log file (not resetting it), if it already exists:
	% (no delayed_write, to avoid missing logs when halting on error)
	%
	LogFile = file_utils:open( LogFilename, _Opts=[ append, raw ] ),

	file_utils:write( LogFile, "~nStarting new merge session "
		  "(merge tool version ~s) on ~s at ~s.~n",
		  [ ?merge_script_version, net_utils:localhost(),
			time_utils:get_textual_timestamp() ] ),

	#user_state{ log_file=LogFile }.



% Displays (if set so) and logs specified text.
-spec trace( string(), user_state() ) -> user_state().
trace( Message, UserState=#user_state{ log_file=LogFile } ) ->
	file_utils:write( LogFile, Message ++ "\n" ),
	ui:trace( Message ),
	UserState.



% Displays (if set so) and logs specified formatted text.
-spec trace( text_utils:format_string(), [ term() ], user_state() ) ->
				   user_state().
trace( FormatString, Values, UserState=#user_state{ log_file=LogFile } ) ->
	Msg = text_utils:format( FormatString, Values ),
	file_utils:write( LogFile, Msg ++ "\n" ),
	ui:trace( Msg ),
	UserState.



% Logs specified debug text.
-spec trace_debug( string(), user_state() ) -> user_state().
trace_debug( Message, UserState=#user_state{ log_file=LogFile } ) ->
	file_utils:write( LogFile, Message ++ "\n" ),
	UserState.


% Logs specified debug formatted text.
-spec trace_debug( text_utils:format_string(), [ term() ], user_state() ) ->
				   user_state().
trace_debug( FormatString, Values,
			 UserState=#user_state{ log_file=LogFile } ) ->
	Msg = text_utils:format( FormatString, Values ),
	file_utils:write( LogFile, Msg ++ "\n" ),
	UserState.



% Stops user-related services.
-spec stop_user_service( user_state() ) -> basic_utils:void().
stop_user_service( _UserState=#user_state{ log_file=LogFile } ) ->

	trace_utils:debug( "Stopping user service." ),

	ui:stop(),

	file_utils:write( LogFile, "Stopping merge session.~n", [] ),

	file_utils:close( LogFile ),

	trace_utils:debug( "Stopped." ),

	basic_utils:stop( _ErrorCode=0 ).





% Checks that the source and target trees exist.
-spec check_content_trees( tree_data(), tree_data() ) -> void().
check_content_trees( InputTree, ReferenceTreePath ) ->

	case file_utils:is_existing_directory( InputTree ) of

		true ->
			ok;

		false ->
			trace_utils:error_fmt( "Error, specified input tree ('~s') "
								   "does not exist.", [ InputTree ] ),
			throw( { non_existing_input_tree, InputTree } )

	end,

	case file_utils:is_existing_directory( ReferenceTreePath ) of

		true ->
			ok;

		false ->
			trace_utils:error_fmt( "Error, specified reference tree ('~s') "
								   "does not exist.", [ ReferenceTreePath ] ),
			throw( { non_existing_reference_tree, ReferenceTreePath } )

	end.



% Returns the path of the cache file corresponding to the specified tree path.
-spec get_cache_path_for( file_utils:directory_name() ) ->
								file_utils:file_name().
get_cache_path_for( TreePath ) ->
	file_utils:join( TreePath, ?merge_cache_filename ).



% Ensures that specified tree path exists.
-spec check_tree_path_exists( file_utils:directory_name() ) -> void().
check_tree_path_exists( TreePath ) ->

	case file_utils:is_existing_directory( TreePath ) of

		true ->
			ok;

		false ->
			ui:display_error( "The path '~s' does not exist.", [ TreePath ] ),
			throw( { non_existing_content_tree, TreePath } )

	end.



% Updates specified content tree (based on a "weak" check): verifies that it
% exists, that a merge cache file exists and is up to date (otherwise rebuilds
% it), and returns the corresponding tree datastructure.
%
-spec update_content_tree( file_utils:directory_name(), analyzer_ring(),
						   user_state() ) -> tree_data().
update_content_tree( TreePath, AnalyzerRing, UserState ) ->

	CacheFilePath = get_cache_path_for( TreePath ),

	MaybeTreeData = case file_utils:is_existing_file( CacheFilePath ) of

		true ->
			trace_utils:debug_fmt( "Found existing cache file '~s'.",
								   [ CacheFilePath ] ),

			% Load it, if trusted (typically if not older from the newest
			% file in tree):
			%
			{ NewestTimestamp, ContentFiles } =
				find_newest_timestamp_from( TreePath, CacheFilePath ),

			case file_utils:get_last_modification_time( CacheFilePath ) of

				CacheTimestamp when CacheTimestamp < NewestTimestamp ->
					trace_utils:debug_fmt( "Timestamp of cache file (~p) older "
						"than most recent file timestamp in tree (~p), "
						"rebuilding cache file.",
						[ CacheTimestamp, NewestTimestamp ] ),
					undefined;


				CacheTimestamp ->

					trace_utils:debug_fmt( "Timestamp of cache file is "
						"acceptable (as ~p is not older than the most recent "
						"file timestamp in tree, ~p), just performing a quick "
						"check of file existences and sizes to further "
						"validate it.",
						[ CacheTimestamp, NewestTimestamp ] ),

					case quick_cache_check( CacheFilePath, ContentFiles,
											TreePath ) of

						undefined ->
							trace_utils:debug( "Cache file does not match "
								"actual tree, rebuilding cache file." ),
							undefined;

						TreeData ->
							trace_utils:debug( "Cache file seems to match "
								"actual tree, considering it legit." ),
							TreeData

					end

			end;

		false ->
			trace_utils:debug_fmt( "No cache file found for '~s', creating it.",
								   [ TreePath ] ),
			undefined

	end,

	case MaybeTreeData of

		undefined ->
			create_merge_cache_file_for( TreePath, CacheFilePath, AnalyzerRing,
										 UserState );

		_ ->
			MaybeTreeData

	end.



% Returns the last content modification timestamp of the most recently modified
% file (the merge cache file excluded) in specified tree, and a list of the
% actual files (as relative paths).
%
-spec find_newest_timestamp_from( directory_path(), file_path() ) ->
	  { maybe( time_utils:posix_seconds() ), [ file_path() ] }.
find_newest_timestamp_from( RootPath, CacheFilePath ) ->

	CacheFilename = filename:basename( CacheFilePath ),

	ActualFileRelPaths = list_utils:delete_existing( CacheFilename,
		 file_utils:find_files_from( RootPath, _IncludeSymlinks=false ) ),

	%trace_utils:debug_fmt( "ActualFileRelPaths = ~p", [ ActualFileRelPaths ] ),

	MaybeTimestamp = case ActualFileRelPaths of

		% Any atom is deemed superior to any integer, so the cache file will be
		% considered up to date:
		%
		[] ->
			undefined;

		_ ->
			% Any actual timestamp will shadow a null one:
			get_newest_timestamp( ActualFileRelPaths, RootPath,
								  _MostRecentTimestamp=0 )

	end,

	% Files returned to avoid performing multiple traversals:
	{ MaybeTimestamp, ActualFileRelPaths }.



% Returns the lastest modification timestamp among the specified files.
get_newest_timestamp( _ContentFiles=[], _RootPath, MostRecentTimestamp ) ->
	MostRecentTimestamp;

get_newest_timestamp( _ContentFiles=[ F | T ], RootPath,
					  MostRecentTimestamp ) ->

	FilePath = file_utils:join( RootPath, F ),

	case file_utils:get_last_modification_time( FilePath ) of

		Timestamp when Timestamp > MostRecentTimestamp ->
			get_newest_timestamp( T, RootPath, Timestamp );

		_ ->
			get_newest_timestamp( T, RootPath, MostRecentTimestamp )

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



% Creates (typically from scratch) a merge cache file with specified name, for
% specified content tree.
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

	write_cache_header( MergeFile ),

	_BlankDataTable = table:new(),

	_CurrentPosixTime = os:system_time(),

	TreeData = scan_tree( AbsTreePath, AnalyzerRing, UserState ),

	trace( "Scanned tree: ~s.",
		   [ tree_data_to_string( TreeData ) ], UserState ),

	write_tree_data( MergeFile, TreeData, UserState ),

	write_cache_footer( MergeFile ),

	file_utils:close( MergeFile ),

	TreeData.



% Writes the header of specified cache file.
-spec write_cache_header( file() ) -> void().
write_cache_header( File ) ->

	%ScriptName = filename:basename( escript:script_name() ),
	ScriptName = ?MODULE,

	file_utils:write( File, "% Merge cache file written by '~s' (version ~s),~n"
							"% on host '~s', at ~s.~n~n"
							"% Structure of file entries: SHA1, "
							"relative path, size in bytes, timestamp~n~n" ,
					  [ ScriptName, ?merge_script_version,
						net_utils:localhost(),
						time_utils:get_textual_timestamp() ] ).



% Writes the footer of specified cache file.
-spec write_cache_footer( file() ) -> void().
write_cache_footer( File ) ->
	file_utils:write( File, "~n% End of merge cache file (at ~s).",
					  [ time_utils:get_textual_timestamp() ] ).



% Writes the specified tree data into specified file.
write_tree_data( MergeFile, #tree_data{ root=RootDir,
										entries=Entries }, _UserState ) ->

	% Converting file_data records into file_entry elements to be stored
	% in-file:
	%
	EntryContent = lists:foldl( fun( { SHA1, FileData }, Acc ) ->
									get_file_content_for( SHA1, FileData )
											++ Acc
								end,
								% Strings preferred to binaries, as shorter and
								% more standard in files (no << and >> added):
								%
								_Acc0=[ { root, RootDir } ],
								_List=table:enumerate( Entries ) ),

	file_utils:write_direct_terms( MergeFile, lists:reverse( EntryContent ) ).



% Checking on the SHA1:
get_file_content_for( SHA1, FileDataElems ) ->
	% Storage format a bit different from working one:
	[ { file_entry, SHA1, text_utils:binary_to_string( RelativePath ), Size,
		Timestamp }
	  || #file_data{ path=RelativePath,
					 % type: not to store
					 size=Size,
					 timestamp=Timestamp,
					 sha1_sum=RecSHA1 } <- FileDataElems, RecSHA1 =:= SHA1 ].



% Spawns the specified number of data analyzers, and returns their PID.
-spec spawn_data_analyzers( count(), user_state() ) -> [ analyzer_pid() ].
spawn_data_analyzers( Count, _UserState ) ->
	%trace_debug( "Spawning ~B data analyzers.", [ Count ], UserState ),
	[ ?myriad_spawn_link( fun() -> analyze_loop() end )
	  || _C <- lists:seq( 1, Count ) ].



% Terminates specified data analyzers.
-spec terminate_data_analyzers( [ analyzer_pid() ], user_state() ) -> void().
terminate_data_analyzers( Analyzers, _UserState ) ->

	%trace_debug( "Terminating ~B data analyzers (~p).",
	%			 [ length( Analyzers ), Analyzers ], UserState ),

	[ P ! terminate || P <- Analyzers ].



% Scans for good the specified tree, whose path is expected to exist.
-spec scan_tree( file_utils:path(), analyzer_ring(), user_state() ) ->
					   tree_data().
scan_tree( AbsTreePath, AnalyzerRing, UserState ) ->

	trace( "Scanning tree '~s'...", [ AbsTreePath ], UserState ),

	AllFiles = file_utils:find_files_from( AbsTreePath ),

	% Not wanting to index our own files (if any already exists):
	FilteredFiles = lists:delete( ?merge_cache_filename, AllFiles ),

	trace_debug( "Found ~B files: ~s", [ length( FilteredFiles ),
			text_utils:strings_to_string( FilteredFiles ) ], UserState ),

	% For lighter message sendings and storage:
	FilteredBinFiles = text_utils:strings_to_binaries( FilteredFiles ),

	scan_files( FilteredBinFiles, AbsTreePath, AnalyzerRing ).



% Scans specified content files, using for that the specified analyzers,
% returning the corresponding tree data.
%
-spec scan_files( [ bin_file_path() ], file_utils:path(), analyzer_ring() ) ->
						tree_data().
scan_files( Files, AbsTreePath, AnalyzerRing ) ->

	InitialTreeData = #tree_data{ root=AbsTreePath },

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
	AnalyzerPid ! { analyzeFile, [ text_utils:string_to_binary( AbsTreePath ),
								   Filename ], self() },

	% Helps controlling flow and avoiding too large mailboxes on either side
	% (this main script, being slowed down, or the analyzers), by attempting to
	% receive once after each sending:
	%
	receive

		{ file_analyzed, FileData } ->

			NewTreeData = manage_received_data( FileData, TreeData ),
			% Plus one (sending) minus one (receiving) waited:
			scan_files( T, NewRing, NewTreeData, WaitedCount )

	after 0 ->

		% One sending, and no receiving here:
		scan_files( T, NewRing, TreeData, WaitedCount+1 )

	end.



% Manages specified received file data, and returns an updated tree data.
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
	NewEntries = case table:lookup_entry( Sum, Entries ) of

		key_not_found ->
			table:add_entry( Sum, [ FileData ], Entries );

		{ value, SumEntries } ->
			table:add_entry( Sum, [ FileData | SumEntries ], Entries )

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
wait_entries( TreeData, _WaitedCount=0 ) ->
	%trace_debug( "All file entries waited for finally obtained." ),
	TreeData;

wait_entries( TreeData, WaitedCount ) ->

	%trace_debug( "Still waiting for ~B file entries.", [ WaitedCount ] ),

	receive

		{ file_analyzed, FileData } ->
			NewTreeData = manage_received_data( FileData, TreeData ),
			wait_entries( NewTreeData, WaitedCount-1 );

		{ file_disappeared, BinFilePath } ->
			trace_utils:debug_fmt( "File '~s' reported as having disappeared.",
								   [ BinFilePath ] ),
			wait_entries( TreeData, WaitedCount-1 )

	end.



% The loop run by each analyzer process.
-spec analyze_loop() -> void().
analyze_loop() ->

	%trace_debug( "Analyzer ~w waiting...", [ self() ] ),

	receive

		{ analyzeFile, [ AbsTreeBinPath, RelativeBinFilename ], SenderPid } ->

			AbsTreePath = text_utils:binary_to_string( AbsTreeBinPath ),

			RelativeFilename = text_utils:binary_to_string(
								 RelativeBinFilename ),

			FilePath = file_utils:join( AbsTreePath, RelativeFilename ),

			%FileBinPath = text_utils:string_to_binary( FilePath ),

			%trace_debug( "Analyzer ~w taking in charge '~s'...",
			%			  [ self(), FullPath ] ),

			case file_utils:is_existing_file( FilePath ) of

				true ->
					FileData = #file_data{
					   % We prefer storing relative filenames:
					   path=RelativeBinFilename,
					   type=file_utils:get_type_of( FilePath ),
					   size=file_utils:get_size( FilePath ),
					   timestamp=file_utils:get_last_modification_time(
								   FilePath ),
					   sha1_sum=executable_utils:compute_sha1_sum( FilePath ) },

					SenderPid ! { file_analyzed, FileData },
					analyze_loop();

				false ->
					case file_utils:exists( FilePath ) of

						true ->
							Type = file_utils:get_type_of( FilePath ),
							trace_utils:info_fmt( "The type of entry '~s' "
								"switched from regular (file) to ~s.",
								[ FilePath, Type ] );

						false ->
							trace_utils:info_fmt( "The file '~s' does not "
								"exist anymore.", [ FilePath ] )

					end,

					BinFilePath = text_utils:string_to_binary( FilePath ),
					SenderPid ! { file_disappeared, BinFilePath },
					analyze_loop()

			end;

		terminate ->
			%trace_debug( "Analyzer ~w terminated.", [ self() ] ),
			ok

	end.


% Interacts with the user so that the specified tree can be deduplicated.
-spec deduplicate_tree( tree_data(), user_state() ) -> tree_data().
deduplicate_tree( TreeData=#tree_data{ root=RootDir,
									   entries=EntryTable,
									   file_count=FileCount }, UserState ) ->

	DuplicateCount = FileCount - table:size( EntryTable ),

	{ NewEntryTable, RemovedDuplicateCount } =
		manage_duplicates( EntryTable, RootDir, UserState ),

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
-spec manage_duplicates( sha1_table(), directory_path(),
						 user_state() ) -> { sha1_table(), count() }.
manage_duplicates( EntryTable, RootDir, UserState ) ->

	ContentEntries = table:enumerate( EntryTable ),

	% We could have forced that no duplication at all exists afterwards (and
	% then a given SHA1 sum would be associated to exactly one content), however
	% it would be too strict, hence we kept a list associated to each SHA1 sum.
	%
	% Two passes: one to establish and count the duplications, another to solve
	% them; returns a list of duplications, and a content table referencing all
	% non-duplicated entries.
	%
	{ DuplicationCases, UniqueTable } = filter_duplications( ContentEntries ),

	% UniqueTable contains all unique elements, while DuplicationCases contains
	% all non-unique ones.

	case length( DuplicationCases ) of

		0 ->
			ui:display( "No duplicated content detected." ),
			{ UniqueTable, _RemoveCount=0 };


		TotalDupCaseCount ->
			ui:display( "~B case(s) of content duplication detected, "
						"examining them in turn.~n", [ TotalDupCaseCount ] ),

			process_duplications( DuplicationCases, TotalDupCaseCount,
								  UniqueTable, RootDir, UserState )

	end.



% Filters the duplications from specified content entries: returns the actual
% duplications in a list, put the unique files in a new table.
%
-spec filter_duplications( [ sha1_entry() ] ) ->
								 { [ sha1_entry() ], sha1_table() }.
filter_duplications( SHA1Entries ) ->
	% Far better than a fold:
	filter_duplications( SHA1Entries, _Acc={ _DupEntries=[], table:new() } ).


% Returns { AccDupEntries, AccUniqueTable }:
filter_duplications( _SHA1Entry=[], Acc ) ->
	Acc;

% By design V is never empty:
filter_duplications( _SHA1Entry=[ { Sha1Key, V=[ _SingleContent ] } | T ],
					 _Acc={ AccDupEntries, AccUniqueTable } ) ->
	% Single content, hence unique:
	NewTable = table:add_entry( Sha1Key, V, AccUniqueTable ),
	filter_duplications( T, { AccDupEntries, NewTable } );

% SHA1Entry is { Sha1Key, V } with at least two elements in V here:
filter_duplications( _SHA1Entries=[ SHA1Entry | T ],
					 _Acc={ AccDupEntries, AccUniqueTable } ) ->
	% So at least one duplicate here:
	NewDupEntries = [ SHA1Entry | AccDupEntries ],
	filter_duplications( T, { NewDupEntries, AccUniqueTable } ).



% Processes the spotted duplications by asking the user.
-spec process_duplications( [ sha1_entry() ], count(), sha1_table(),
	directory_path(), user_state() ) -> { sha1_table(), count() }.
process_duplications( DuplicationCases, TotalDupCaseCount, UniqueTable,
					  RootDir, UserState ) ->

	%trace_utils:debug_fmt( "Pre-deduplication unique table: ~s",
	%					   [ table:to_string( UniqueTable ) ] ),

	Acc0 = { UniqueTable, _InitialDupCount=1, _InitialRemoved=0 },
	process_duplications_helper( DuplicationCases, TotalDupCaseCount, Acc0,
								 RootDir, UserState ).




% Helper returning { sha1_table(), count() }:
process_duplications_helper( _DupCases=[], _TotalDupCount,
	  _Acc={ AccTable, _AccDupCount, AccRemoveCount }, _RootDir, _UserState ) ->

	trace_utils:debug_fmt( "Post-deduplication unique table: ~s",
						   [ table:to_string( AccTable ) ] ),

	{ AccTable, AccRemoveCount };

process_duplications_helper( _DupCases=[ { Sha1Key, DuplicateList } | T ],
							 TotalDupCount,
							 _Acc={ AccTable, AccDupCount, AccRemoveCount },
							 RootDir, UserState ) ->

	Size = check_duplicates( Sha1Key, DuplicateList ),

	SelectedFileEntries = manage_duplication_case( DuplicateList, AccDupCount,
							  TotalDupCount, Size, RootDir, UserState ),

	NewAccTable = table:add_entry( Sha1Key, SelectedFileEntries, AccTable ),

	NewRemoveCount = AccRemoveCount + length( DuplicateList )
		- length( SelectedFileEntries ),

	NewAcc = { NewAccTable, AccDupCount+1, NewRemoveCount },

	process_duplications_helper( T, TotalDupCount, NewAcc, RootDir, UserState ).



% Checks a duplication set: same SHA1 sum and also size must be found for all
% file entries (would most probably detect any SHA1 collision, however unlikely
% it maybe); returns the (common) size.
%
-spec check_duplicates( sha1(), [ file_data() ] ) -> system_utils:byte_size().
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

% (and a different SHA1 sum would trigger a case clause)
check_duplicates( SHA1Sum, FirstPath, Size, _DuplicateList=[
	  #file_data{ path=OtherPath, sha1_sum=SHA1Sum, size=OtherSize } | _T ] ) ->
	throw( { sha1_collision_detected, SHA1Sum, { FirstPath, Size },
			 { OtherPath, OtherSize } } ).



% Manages specified duplicated entries.
%
% Returns the (regular) files that remain for that content.
%
-spec manage_duplication_case( [ file_data() ], count(), count(),
	system_utils:byte_size(), directory_path(), user_state() ) ->
									 [ file_data() ].
manage_duplication_case( FileEntries, DuplicationCaseCount, TotalDupCaseCount,
						 Size, RootDir, UserState ) ->

	SizeString = system_utils:interpret_byte_size_with_unit( Size ),

	PathStrings = lists:sort( [ text_utils:binary_to_string( E#file_data.path )
					|| E <- FileEntries ] ),

	%trace_utils:debug_fmt( "PathStrings = ~p", [ PathStrings ] ),

	% As we do not want a common prefix to include any basename:
	Dirnames = [ filename:dirname( P ) || P <- PathStrings ],

	ui:add_separation(),

	Title = text_utils:format( "Examining duplication case ~B/~B",
							   [ DuplicationCaseCount, TotalDupCaseCount ] ),

	ui:set_setting( 'title', Title ),

	Count = length( FileEntries ),

	% By design more than one path: text_utils:get_longest_common_prefix/1
	% should not be used as for example a foobar-new directory could be a
	% sibling of a foobar directory, resulting in -new/... meaningless suffixes;
	% so:
	%
	{ Label, Prefix, ShortenPaths } =
		case file_utils:get_longest_common_path( Dirnames ) of

		% No common prefix at all here:
		"" ->

			Lbl = text_utils:format( "Following ~B files have the exact same "
									 "content (and thus size, of ~s)",
									 [ Count, SizeString ] ),
			{ Lbl, _Prefix="", PathStrings };

		% We do not re-reuse the remaining, prefixless strings as they do not
		% comprise the basename (only the dirname):
		%
		{ Prfx, _Tails } ->

			PrefixLen = length( Prfx ),

			% +1 for offset and +1 to remove leading /:
			TrimmedPaths = [ string:substr( P, PrefixLen + 2 )
							 || P <- PathStrings ],

			Lbl = text_utils:format( "Following ~B files have the exact same "
									 "content (and thus size, of ~s) and "
									 "all start with the same prefix, '~s' "
									 "(omitted below)",
									 [ Count, SizeString, Prfx ] ),
			{ Lbl, Prfx, TrimmedPaths }

	end,

	%trace_utils:debug_fmt( "ShortenPaths = ~p", [ ShortenPaths ] ),

	DuplicateString = text_utils:format( ": ~s",
					[ text_utils:strings_to_string( ShortenPaths ) ] ),

	%trace_utils:debug_fmt( "DuplicateString = ~p", [ DuplicateString ] ),


	FullLabel = Label ++ DuplicateString,

	Choices = [ { keep, "Keep only one of these files" },
				{ elect, "Elect a reference file, replacing each other by "
				  "a symbolic link pointing to it" },
				{ leave, "Leave them as they are" },
				{ abort, "Abort" } ],

	SelectedChoice = ui:choose_designated_item(
					   text_utils:format( "~s~nChoices are:", [ FullLabel ] ),
					   Choices ),

	ui:unset_setting( 'title' ),

	%trace( "Selected choice: ~p", [ SelectedChoice ], UserState ),

	case SelectedChoice of


		keep ->
			KeptFilePath = keep_only_one( Prefix, ShortenPaths, PathStrings,
										  RootDir, UserState ),

			trace( "Kept only reference file '~s'", [ KeptFilePath ],
				   UserState ),

			%trace_utils:debug_fmt( "Entries to scan: ~p", [ FileEntries ] ),

			% As this is a list of file_data:
			[ find_data_entry_for( KeptFilePath, FileEntries ) ];


		elect ->
			% Symlinks ignored:
			ElectedFilePath = elect_and_link( Prefix, ShortenPaths, PathStrings,
											  RootDir, UserState ),

			% As this is a list of file_data:
			[ find_data_entry_for( ElectedFilePath, FileEntries ) ];


		leave ->
			PrefixString = case Prefix of

				"" ->
					"";

				Prefix ->
					text_utils:format( " (prefix: '~s')", [ Prefix ] )

			end,

			trace( "[~B/~B] Leaving as they are~s: ~s",
				   [ DuplicationCaseCount, TotalDupCaseCount, PrefixString,
					 DuplicateString ], UserState ),
			FileEntries;


		abort ->
			ui:display( "Uniquification aborted, stopping now." ),
			trace( "(requested to abort the merge)", UserState ),
			basic_utils:stop( 0 )

	end.


% Returns the file_data record in the specified list that corresponds to the
% specified path.
%
-spec find_data_entry_for( file_path(), [ file_data() ] ) -> file_data().
find_data_entry_for( FilePath, _FileEntries=[] ) ->
	throw( { not_found, FilePath } );

% Match:
find_data_entry_for( FilePath,
					 _FileEntries=[ FD=#file_data{ path=FilePath } | _T ] ) ->
	FD;

find_data_entry_for( FilePath, _FileEntries=[ _FD | T ] ) ->
	find_data_entry_for( FilePath, T ).



% Selects among the specified files the single one that shall be kept while the
% others are removed, and returns its filename as a binary.
%
-spec keep_only_one( string(), [ file_path() ], [ file_path() ],
					 directory_path(), user_state() ) -> bin_file_path().
keep_only_one( Prefix, TrimmedPaths, PathStrings, RootDir, UserState ) ->

	ui:set_setting( title,
					_Title="Selecting the unique reference version to keep, "
					"whereas the others are to be removed" ),

	BaseLabel = "~nPlease choose the (single) file to keep "
				  "(others being removed), among:",

	Label = case Prefix of

		"" ->
			BaseLabel;

		_ ->
			text_utils:format( "~s~n(common prefix '~s' omitted)",
							   [ BaseLabel, Prefix ] )

	end,

	KeptIndex = ui:choose_numbered_item( Label, _Choices=TrimmedPaths ),

	ui:unset_setting( title ),

	{ KeptFilePath, ToRemovePaths } =
		list_utils:extract_element_at( PathStrings, KeptIndex ),

	trace( "Keeping '~s', removing (based on common prefix '~s' and "
		   "root directory '~s'): ~s ",
		   [ KeptFilePath, Prefix, RootDir,
			 text_utils:strings_to_string( ToRemovePaths ) ], UserState ),

	ToRemoveFullPaths = [ file_utils:join( RootDir, P )
						  || P <- ToRemovePaths ],

	file_utils:remove_files( ToRemoveFullPaths ),

	text_utils:string_to_binary( KeptFilePath ).



% Selects among the specified files the single one that shall be elected and
% kept, while the others are removed and replaced by symlinks pointing to that
% file, and returns it as a binary.
%
-spec elect_and_link( string(), [ file_path() ], [ file_path() ],
					  directory_path(), user_state() ) -> bin_file_path().
elect_and_link( Prefix, TrimmedPaths, PathStrings, RootDir, UserState ) ->

	ui:set_setting( title, _Title="Selecting the unique version to elect "
					"as a reference, whereas the others will be replaced "
					"by symbolic links pointing to it" ),

	BaseLabel = "~nPlease choose the (single) file to elect, among:",

	Label = case Prefix of

		"" ->
			BaseLabel;

		_ ->
			text_utils:format( "~s~n(common prefix '~s' omitted)",
							   [ BaseLabel, Prefix ] )

	end,

	ElectedIndex = ui:choose_numbered_item( Label, _Choices=TrimmedPaths ),

	ui:unset_setting( title ),

	{ ElectedFilePath, FutureLinkPaths } =
		list_utils:extract_element_at( PathStrings, ElectedIndex ),

	trace( "Electing '~s', replacing by symlinks (based on common prefix '~s' "
		   "and root directory '~s'): ~s",
		   [ ElectedFilePath, Prefix, RootDir,
			 text_utils:strings_to_string( FutureLinkPaths ) ], UserState ),

	ToRemoveFullPaths = [ file_utils:join( RootDir, P )
						  || P <- FutureLinkPaths ],

	file_utils:remove_files( ToRemoveFullPaths ),

	create_links_to( ElectedFilePath, FutureLinkPaths, RootDir ),

	text_utils:string_to_binary( ElectedFilePath ).



% Creates relative, symbolic links to the specified file path.
create_links_to( _TargetFilePath, _LinkPaths=[], _RootDir ) ->
	ok;

create_links_to( TargetFilePath, _LinkPaths= [ Link | T ], RootDir ) ->

	% We want to create the (shortest) relative link, from source to target:

	LinkDir = filename:dirname( Link ),

	RelativeTargetFilePath =
		file_utils:make_relative( TargetFilePath, LinkDir ),

	file_utils:create_link( RelativeTargetFilePath,
							file_utils:join( RootDir, Link ) ),

	create_links_to( TargetFilePath, T, RootDir ).



% Performs a quick check (i.e. with no checksum computed of the file contents)
% of the specified tree, against the specified cache file: check that both file
% sets match (no extra element on either size) and that the cached and actual
% file sizes match as well.
%
-spec quick_cache_check( file_path(), [ file_path() ], directory_path() ) ->
							   maybe( tree_data() ).
quick_cache_check( CacheFilename, ContentFiles, TreePath ) ->

	case file_utils:read_terms( CacheFilename ) of

		[ _RootInfo={ root, CachedTreePath } | FileInfos ] ->
			quick_cache_check_helper( ContentFiles, TreePath, CachedTreePath,
									  FileInfos );

		_Other ->
			trace_utils:warning_fmt( "Invalid cache file '~s', removing it "
									 "and recreating it.", [ CacheFilename ] ),
			file_utils:remove_file( CacheFilename ),
			undefined

	end.


% (helper)
quick_cache_check_helper( ContentFiles, TreePath, CachedTreePath, FileInfos ) ->

	AbsTreePath = file_utils:ensure_path_is_absolute( TreePath ),

	case CachedTreePath of

		AbsTreePath ->
			ok;

		_ ->
			trace_utils:error_fmt( "The actual tree path ('~s') does not match "
								   "the one found in its cache file ('~s').",
								   [ AbsTreePath, CachedTreePath ] ),
			throw( { non_matching_tree_paths, CachedTreePath, TreePath } )

	end,

	ActualFileCount = length( ContentFiles ),

	CachedFileCount = length( FileInfos ),

	CachedFilePairs = [ { Path, Size }
		  || { file_entry, _SHA1, Path, Size, _Timestamp } <- FileInfos ],

	case ActualFileCount of

		CachedFileCount ->
			trace_utils:debug_fmt( "Cached and actual file counts match "
								   "(~B files).", [ CachedFileCount ] );

		_ ->
			% Does not fail immediately, for a better error report:
			trace_utils:trace_fmt( "The cached and actual file counts do not "
				"match: ~B are referenced in cache, ~B exist "
				"in the filesystem.",
				[ CachedFileCount, ActualFileCount ] )

	end,

	CachedFilenames = [ FilePath
						|| { FilePath, _FileSize } <- CachedFilePairs ],

	CachedFileset = set_utils:new( CachedFilenames ),
	ActualFileset = set_utils:new( ContentFiles ),

	{ OnlyCachedSet, OnlyActualSet } =
		set_utils:differences( CachedFileset, ActualFileset ),

	MustRescanFirst = case set_utils:is_empty( OnlyCachedSet ) of

		true ->
			false;

		false ->
			OnlyCacheList = set_utils:to_list( OnlyCachedSet ),
			trace_utils:trace_fmt( "Following ~B files are referenced in "
				"cache, yet do not exist on the filesystem: ~s",
				[ length( OnlyCacheList ),
				  text_utils:strings_to_string( OnlyCacheList ) ] ),
			true

	end,

	MustRescan = case set_utils:is_empty( OnlyActualSet ) of

		true ->
			MustRescanFirst;

		false ->
			OnlyActualList = set_utils:to_list( OnlyActualSet ),
			trace_utils:trace_fmt( "Following ~B files exist on the "
				"filesystem, yet are not referenced in cache: ~s",
				[ length( OnlyActualList ),
				  text_utils:strings_to_string( OnlyActualList ) ] ),
			true

	end,

	case MustRescan of

		true ->
			% Will trigger a rescan:
			undefined;

		false ->

			trace_utils:debug( "The file paths and names match." ),

			% The two sets match, yet do they agree on the file sizes as well?
			%
			% (CachedFilePairs tells us both the paths and the expected sizes,
			% hence no need for ContentFiles)
			%
			case check_file_sizes_match( CachedFilePairs, TreePath ) of

				% Alles gut, so create the corresponding receptacle:
				true ->
					trace_utils:debug_fmt( "All sizes of the ~B files match.",
										   [ CachedFileCount ] ),

					#tree_data{ root=CachedTreePath,
								entries=build_entry_table( FileInfos ),
								file_count=CachedFileCount
								% Not managed (at least yet): the other counts.
								};

				false ->
					% Will trigger a rescan:
					undefined

			end

	end.



% Builds the entry table from specified terms.
-spec build_entry_table( [ tuple() ] ) -> sha1_table().
build_entry_table( FileInfos ) ->

	EntryTable = table:new(),

	build_entry_table( FileInfos, EntryTable ).


% (helper)
build_entry_table( _FileInfos=[], EntryTable ) ->
	EntryTable;

build_entry_table(
  _FileInfos=[ { file_entry, SHA1, RelativePath, Size, Timestamp } | T  ],
  EntryTable ) ->

	FileData = #file_data{ path=text_utils:string_to_binary( RelativePath ),
						   type=regular,
						   size=Size,
						   timestamp=Timestamp,
						   sha1_sum=SHA1 },

	NewEntryTable = table:append_to_entry( SHA1, FileData, EntryTable ),

	build_entry_table( T, NewEntryTable ).




% Checks that the actual file sizes match the specified ones.
-spec check_file_sizes_match(
		[ { file_path(), system_utils:byte_size() } ],
		directory_path() ) -> boolean().
check_file_sizes_match( _FilePairs=[], _TreePath ) ->
	true;

check_file_sizes_match( _FilePairs=[ { FilePath, FileSize } | T ], TreePath ) ->

	FileFullPath = file_utils:join( TreePath, FilePath ),

	case file_utils:get_size( FileFullPath ) of

		FileSize ->
			check_file_sizes_match( T, TreePath );

		ActualSize ->
			trace_utils:debug_fmt( "For file '~s', cached size is ~s (~B bytes)"
				", whereas actual size is ~s (~B bytes), "
				"invalidating thus cache file.",
				[ FileFullPath, system_utils:interpret_byte_size( FileSize ),
				  FileSize, system_utils:interpret_byte_size( ActualSize ),
				  ActualSize ] ),
			false

	end.



% Returns a textual description of specified tree data.
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

		0 ->
			"empty tree";

		FileCount ->
			text_utils:format( "tree '~s' having ~B files, "
							   "each with unique content",
							   [ RootDir, FileCount ] );

		ContentCount ->
			text_utils:format( "tree '~s' having ~B files, corresponding "
							   "only to ~B different contents "
							   "(hence with ~B duplicates)",
							   [ RootDir, FileCount, ContentCount,
								 FileCount - ContentCount ] )

	end.



% Returns a textual description of specified file data.
-spec file_data_to_string( file_data() ) -> string().
file_data_to_string( #file_data{ path=Path,
								 size=Size,
								 timestamp=Timestamp,
								 sha1_sum=Sum } ) ->

	SizeString = system_utils:interpret_byte_size_with_unit( Size ),

	text_utils:format( "file '~s' whose size is ~s, SHA1 sum is ~s and "
					   "timestamp is ~p",
					   [ Path, SizeString, Sum, Timestamp ] ).
