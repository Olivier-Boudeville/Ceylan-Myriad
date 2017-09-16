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
% Author: Jingxuan Ma (jingxuan.ma@edf.fr)


% Tests for comparing and illustrating the differences of following types of
% hashtables:
%
% - hashtable
% - tracked hashtable
% - lazy hashtable
% - map hashtable
% - list hashtable
%
% See also hashtable.erl, tracked_hashtable.erl, lazy_hashtable.erl,
% map_hashtable.erl, list_hashtable.erl and their respective test modules.
%
% Directly depends on the following modules: hashtable, tracked_hashtable,
% lazy_hashtable, map_hashtable, list_hashtable.
%
% Note that the 'table' pseudo-module is not tested here: not only it is
% actually one of the previous implementations, but also modules are applied
% dynamically here, hence the parse transform will not replace anything.
%
-module(hashtables_comparison_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-define(MyFirstKey,  'MyFirstKey').
-define(MySecondKey, 'MySecondKey').
-define(MyThirdKey,  'MyThirdKey').
-define(MyFourthKey, 'MyFourthKey').


-export([ run_basic_tests/0, run_performance_tests/0 ]).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	%run_basic_tests(),

	run_performance_tests(),

	test_facilities:stop().



display_separator() ->
	test_facilities:display( "  -----------------------------~n~n" ).



% Performs the same set of operations of an instance of each type of hashtable.
%
run_basic_tests() ->

	% Tests for each kind of table should be separated.

	display_separator(),

	test_facilities:display( " ~n Comparison of tables creation: " ),
	MyH1 = hashtable:new( 10 ),
	true = hashtable:isEmpty( MyH1 ),
	hashtable:display( "Vanilla hashtable", MyH1 ),
	MyH1Optimised = hashtable:optimise( MyH1 ),
	hashtable:display( "Optimized hash table", MyH1Optimised ),


	MyTH1 = tracked_hashtable:new( 10 ),
	true = tracked_hashtable:isEmpty( MyTH1 ),
	tracked_hashtable:display( "Vanilla tracked table ", MyTH1 ),

	MyLH1 = lazy_hashtable:new( 10 ),
	true = lazy_hashtable:isEmpty( MyLH1 ),
	lazy_hashtable:display( "Vanilla lazy hashtable", MyLH1 ),

	display_separator(),
	MyM1 = map_hashtable:new( 10 ),
	true = map_hashtable:isEmpty( MyM1 ),
	map_hashtable:display( "Vanilla map hashtable", MyM1 ),

	display_separator(),
	MyL1 = list_hashtable:new( 10 ),
	true = list_hashtable:isEmpty( MyL1 ),
	list_hashtable:display( "Vanilla list hashtable", MyL1 ),


	test_facilities:display( "End of the comparison of tables creation." ),
	display_separator(),



	test_facilities:display(
						 "Comparison of tables' state after adding entries:" ),


	test_facilities:display( "Adding entries in hash table:" ),
	MyH2 = hashtable:new( 4 ),
	MyH3 = hashtable:addEntry( ?MyFirstKey, "MyFirstValue", MyH2 ),
	false = hashtable:isEmpty( MyH3 ),

	MyH4 = hashtable:addEntry( ?MySecondKey, "MySecondValue", MyH3 ),
	false = hashtable:isEmpty( MyH4 ),

	MyH5 = hashtable:addEntry( ?MyThirdKey, [1,2,3], MyH4 ),
	false = hashtable:isEmpty( MyH5 ),
	hashtable:display( MyH5 ),

	MyH5Optimised = hashtable:optimise( MyH5 ),
	hashtable:display( "Optimised hashtable", MyH5Optimised ),


	test_facilities:display( "Adding entries in tracked hashtable:"),
	MyTH2 = tracked_hashtable:new(4 ),
	MyTH3 = tracked_hashtable:addEntry( ?MyFirstKey, "MyFirstValue", MyTH2 ),
	false = tracked_hashtable:isEmpty( MyTH3 ),

	MyTH4 = tracked_hashtable:addEntry( ?MySecondKey, "MySecondValue", MyTH3 ),
	false = tracked_hashtable:isEmpty( MyTH4 ),

	MyTH5 = tracked_hashtable:addEntry( ?MyThirdKey, [1,2,3], MyTH4 ),
	false = tracked_hashtable:isEmpty( MyTH5 ),
	tracked_hashtable:display( "Tracked hashtable: ", MyTH5 ),

	MyTH5Optimised = tracked_hashtable:optimise( MyTH5 ),
	tracked_hashtable:display( "Optimised tracked hashtable", MyTH5Optimised ),


	test_facilities:display( "Adding entries in lazy hashtable:" ),
	MyLH2 = lazy_hashtable:new( 4 ),
	MyLH3 = lazy_hashtable:addEntry( ?MyFirstKey, "MyFirstValue", MyLH2 ),
	false = lazy_hashtable:isEmpty( MyLH3 ),

	MyLH4 = lazy_hashtable:addEntry( ?MySecondKey, "MySecondValue", MyLH3 ),
	false = lazy_hashtable:isEmpty( MyLH4 ),

	MyLH5 = lazy_hashtable:addEntry( ?MyThirdKey, [1,2,3], MyLH4 ),
	false = lazy_hashtable:isEmpty( MyLH5 ),
	lazy_hashtable:display( "Lazy hashtable: ", MyLH5 ),

	MyLH5Optimised = lazy_hashtable:optimise( MyLH5 ),
	lazy_hashtable:display( "Optimised lazy hashtable", MyLH5Optimised ),


	test_facilities:display( "Adding entries in map hashtable:" ),
	MyMH2 = map_hashtable:new( 4 ),
	MyMH3 = map_hashtable:addEntry( ?MyFirstKey, "MyFirstValue", MyMH2 ),
	false = map_hashtable:isEmpty( MyMH3 ),

	MyMH4 = map_hashtable:addEntry( ?MySecondKey, "MySecondValue", MyMH3 ),
	false = map_hashtable:isEmpty( MyMH4 ),

	MyMH5 = map_hashtable:addEntry( ?MyThirdKey, [1,2,3], MyMH4 ),
	false = map_hashtable:isEmpty( MyMH5 ),
	map_hashtable:display( "Map hashtable: ", MyMH5 ),


	test_facilities:display( "Adding entries in list hashtable:" ),
	MyL2 = list_hashtable:new( 4 ),
	MyL3 = list_hashtable:addEntry( ?MyFirstKey, "MyFirstValue", MyL2 ),
	false = list_hashtable:isEmpty( MyL3 ),

	MyL4 = list_hashtable:addEntry( ?MySecondKey, "MySecondValue", MyL3 ),
	false = list_hashtable:isEmpty( MyL4 ),

	MyL5 = list_hashtable:addEntry( ?MyThirdKey, [1,2,3], MyL4 ),
	false = list_hashtable:isEmpty( MyL5 ),
	list_hashtable:display( "List hashtable: ", MyL5 ),


	display_separator(),


	test_facilities:display( "Looking up for ~s in hashtable: ~p",
		   [ ?MyFirstKey, hashtable:lookupEntry( ?MyFirstKey, MyH5 ) ] ),

	{ value, "MyFirstValue" } = hashtable:lookupEntry( ?MyFirstKey, MyH5 ),

	test_facilities:display( "Removing that entry." ),
	MyH6 = hashtable:removeEntry( ?MyFirstKey, MyH5 ),
	false = hashtable:isEmpty( MyH6 ),
	test_facilities:display( "Looking up for ~s hashtable: ~p", [ ?MyFirstKey,
		hashtable:lookupEntry( ?MyFirstKey, MyH6 ) ] ),

	key_not_found = hashtable:lookupEntry( ?MyFirstKey, MyH6 ),

	% removeEntry can also be used if the specified key is not here, will return
	% an identical table.
	hashtable:display( "Hashtable", MyH6 ),



	test_facilities:display( "Looking up for ~s in tracked hashtable: ~p",
		[ ?MyFirstKey,
		  tracked_hashtable:lookupEntry( ?MyFirstKey, MyTH5 ) ] ),

	{ value, "MyFirstValue" } =
		tracked_hashtable:lookupEntry( ?MyFirstKey, MyTH5 ),

	test_facilities:display( "Removing that entry." ),
	MyTH6 = tracked_hashtable:removeEntry( ?MyFirstKey, MyTH5 ),
	false = tracked_hashtable:isEmpty( MyTH6 ),

	test_facilities:display( "Looking up for ~s in tracked hashtable: ~p",
		[ ?MyFirstKey,
		  tracked_hashtable:lookupEntry( ?MyFirstKey, MyTH6 ) ] ),

	key_not_found = tracked_hashtable:lookupEntry( ?MyFirstKey,
															 MyTH6 ),

	% removeEntry can also be used if the specified key is not here, will return
	% an identical table.
	tracked_hashtable:display( "Tracked hashtable", MyTH6 ),



	test_facilities:display( "Looking up for ~s in lazy hashtable: ~p",
		[ ?MyFirstKey, lazy_hashtable:lookupEntry( ?MyFirstKey, MyLH5 ) ] ),

	{ value, "MyFirstValue" } = lazy_hashtable:lookupEntry( ?MyFirstKey,
														   MyLH5 ),

	test_facilities:display( "Removing that entry." ),
	MyLH6 = lazy_hashtable:removeEntry( ?MyFirstKey, MyLH5 ),
	false = lazy_hashtable:isEmpty( MyLH6 ),
	test_facilities:display( "Looking up for ~s in lazy hashtable: ~p",
		[ ?MyFirstKey, lazy_hashtable:lookupEntry( ?MyFirstKey, MyLH6 ) ] ),

	key_not_found = lazy_hashtable:lookupEntry( ?MyFirstKey, MyLH6 ),

	% removeEntry can also be used if the specified key is not here, will return
	% an identical table.
	lazy_hashtable:display( "Lazy hashtable", MyLH6 ),



	test_facilities:display( "Looking up for ~s in map hashtable: ~p",
		[ ?MyFirstKey, map_hashtable:lookupEntry( ?MyFirstKey, MyMH5 ) ] ),

	{ value, "MyFirstValue" } = map_hashtable:lookupEntry( ?MyFirstKey,
														   MyMH5 ),

	test_facilities:display( "Removing that entry." ),
	MyMH6 = map_hashtable:removeEntry( ?MyFirstKey, MyMH5 ),
	false = map_hashtable:isEmpty( MyMH6 ),
	test_facilities:display( "Looking up for ~s in map hashtable: ~p",
		[ ?MyFirstKey, map_hashtable:lookupEntry( ?MyFirstKey, MyMH6 ) ] ),

	key_not_found = map_hashtable:lookupEntry( ?MyFirstKey, MyMH6 ),


	test_facilities:display( "Looking up for ~s in list hashtable: ~p",
		[ ?MyFirstKey, list_hashtable:lookupEntry( ?MyFirstKey, MyL5 ) ] ),

	{ value, "MyFirstValue" } = list_hashtable:lookupEntry( ?MyFirstKey,
															MyL5 ),

	test_facilities:display( "Removing that entry." ),
	MyL6 = list_hashtable:removeEntry( ?MyFirstKey, MyL5 ),
	false = list_hashtable:isEmpty( MyL6 )
		,
	test_facilities:display( "Looking up for ~s in list hashtable: ~p",
		[ ?MyFirstKey, list_hashtable:lookupEntry( ?MyFirstKey, MyL6 ) ] ),

	key_not_found = list_hashtable:lookupEntry( ?MyFirstKey, MyL6 ),

	% removeEntry can also be used if the specified key is not here, will return
	% an identical table.
	list_hashtable:display( "List hashtable", MyL6 ),


	display_separator(),



	test_facilities:display( "Testing double key registering." ),

	MyH7 = hashtable:addEntry( ?MyThirdKey, anything, MyH6 ),
	hashtable:display( MyH7 ),

	test_facilities:display( "Enumerating the hash table: ~p",
		[ hashtable:enumerate( MyH6 ) ] ),

	test_facilities:display( "Listing the hash table keys: ~p",
		[ hashtable:keys( MyH6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey, ?MyThirdKey ],
										 hashtable:keys( MyH6 ) ),

	MyH8 = hashtable:addEntries( [ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyH7 ),

	MyH9 = hashtable:merge( MyH4, MyH8 ),
	test_facilities:display( "Merged table: ~s, size of buckets is ~B.",
		[ hashtable:toString( MyH9 ), hashtable:get_bucket_count( MyH9 ) ] ),

	MyH10 = hashtable:optimise( MyH9 ),
	test_facilities:display( "The optimised table: ~s size of buckets is ~B.",
		[ hashtable:toString( MyH10 ), hashtable:get_bucket_count( MyH10 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys in table ~p:"
					"~n ~p", [ Keys, hashtable:selectEntries( Keys, MyH10 ) ] ),



	MyTH7 = tracked_hashtable:addEntry( ?MyThirdKey, anything, MyTH6 ),
	tracked_hashtable:display( MyTH7 ),

	test_facilities:display( "Enumerating the tracked hash table: ~p.",
		[ tracked_hashtable:enumerate( MyTH6 ) ] ),

	test_facilities:display( "Listing the tracked table keys: ~p.",
		[ tracked_hashtable:keys( MyTH6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey,?MyThirdKey ],
										 tracked_hashtable:keys( MyTH6 ) ),

	MyTH8 = tracked_hashtable:addEntries(
							  [ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyTH7 ),

	MyTH9 = tracked_hashtable:merge( MyTH4, MyTH8 ),

	test_facilities:display( "Merged tracked table: ~s",
				[ tracked_hashtable:toString( MyTH9 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys ~p in tracked table:"
		" ~n ~p", [ Keys, tracked_hashtable:selectEntries( Keys, MyTH9 ) ] ),



	MyLH7 = lazy_hashtable:addEntry( ?MyThirdKey, anything, MyLH6 ),
	lazy_hashtable:display( MyLH7 ),

	test_facilities:display( "Enumerating the lazy table: ~p.",
		[ lazy_hashtable:enumerate( MyLH6 ) ] ),

	test_facilities:display( "Listing the lazy table keys: ~p.",
		[ lazy_hashtable:keys( MyLH6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey, ?MyThirdKey ],
										 lazy_hashtable:keys( MyLH6 ) ),

	MyLH8 = lazy_hashtable:addEntries(
			[ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyLH7 ),

	MyLH9 = lazy_hashtable:merge( MyLH4, MyLH8 ),

	test_facilities:display( "Merged lazy table: ~s",
			[ lazy_hashtable:toString( MyLH9 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys in lazy table ~p:"
		"~n ~p", [ Keys, lazy_hashtable:selectEntries( Keys, MyLH9 ) ] ),




	MyMH7 = map_hashtable:addEntry( ?MyThirdKey, anything, MyMH6 ),
	map_hashtable:display( MyMH7 ),

	test_facilities:display( "Enumerating the map table: ~p.",
		[ map_hashtable:enumerate( MyMH6 ) ] ),

	test_facilities:display( "Listing the map table keys: ~p.",
		[ map_hashtable:keys( MyMH6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey, ?MyThirdKey ],
										 map_hashtable:keys( MyMH6 ) ),

	MyMH8 = map_hashtable:addEntries(
			[ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyMH7 ),

	MyMH9 = map_hashtable:merge( MyMH4, MyMH8 ),


	test_facilities:display( "Merged map table: ~s",
			[ map_hashtable:toString( MyMH9 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys in map table ~p:"
							 "~n ~p",
							 [ Keys, map_hashtable:selectEntries( Keys, MyMH9 ) ] ),



	MyL7 = list_hashtable:addEntry( ?MyThirdKey, anything, MyL6 ),
	list_hashtable:display( MyL7 ),

	test_facilities:display( "Enumerating the list table: ~p.",
		[ list_hashtable:enumerate( MyL6 ) ] ),

	test_facilities:display( "Listing the list table keys: ~p.",
		[ list_hashtable:keys( MyL6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey, ?MyThirdKey ],
										 list_hashtable:keys( MyL6 ) ),

	MyL8 = list_hashtable:addEntries(
			[ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyL7 ),

	MyL9 = list_hashtable:merge( MyL4, MyL8 ),


	test_facilities:display( "Merged list table: ~s",
			[ list_hashtable:toString( MyL9 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys in list table ~p:"
		"~n ~p", [ Keys, list_hashtable:selectEntries( Keys, MyL9 ) ] ).



-define( series_count, 12 ).
%-define( series_count, 120 ).


% Returns a suitable set of 12*7=84 (presumably the typical size of a table of
% interest) key/value pairs (respectively an atom and a value of various types)
% to test tables.
%
get_pairs() ->
	get_pairs( _Series=?series_count, _Count=1, _Acc=[] ).


get_pairs( _Series=0, _Count, Acc ) ->
	Acc;

get_pairs( Series, Count, Acc ) ->

	ToAddStrings = [

			 { io_lib:format( "key-~B", [ Count + 1 ] ) , self() },
			 { io_lib:format( "key-~B", [ Count + 2 ] ) , "hello world!" },
			 { io_lib:format( "key-~B", [ Count + 3 ] ) , an_atom },
			 { io_lib:format( "key-~B", [ Count + 4 ] ) , [ "a", 123, list ] },
			 { io_lib:format( "key-~B", [ Count + 5 ] ) , { 23, 45, 67, 90 } },
			 { io_lib:format( "key-~B", [ Count + 6 ] ) , 1.0 },
			 { io_lib:format( "key-~B", [ Count + 7 ] ) , << "A binary" >> }

	],

	ToAdd = [ { text_utils:string_to_atom( lists:flatten( K ) ), V }
			  || { K, V } <- ToAddStrings ],

	get_pairs( Series - 1, Count + length( ToAdd ), ToAdd ++ Acc ).



% Returns different key/value pairs (the same keys associated to different
% values, and as many new keys as well).
%
get_other_pairs( Pairs ) ->
	get_other_pairs( Pairs, _Acc=[] ).

get_other_pairs( _Pairs=[], Acc ) ->

	NewPairs = get_pairs( _Series=?series_count, _Count=length( Acc ) + 1,
						  [] ),

	list_utils:random_permute( Acc ++ NewPairs );

get_other_pairs( _Pairs=[ { K, _V } | T ], Acc ) ->

	% Same key, different value (should not matter much):
	NewAcc = [ { K, io_lib:format( "Value for ~p", [ K ] ) } | Acc ],

	get_other_pairs( T, NewAcc ).




-define( run_count, 100 ).


% Feeds specified table and returns the corresponding average duration, in
% milliseconds (on average over Count runs).
%
feed_table( Table, Module, Pairs ) ->

	Count = ?run_count,

	Start = time_utils:get_precise_timestamp(),

	NewTables = [ lists:foldl( fun( { K, V }, T ) ->
									Module:addEntry( K, V, T )
							   end,
							   _Acc0=Table,
							   _List=Pairs ) || _C <- lists:seq( 1, Count ) ],

	Stop = time_utils:get_precise_timestamp(),

	Timing = time_utils:get_precise_duration( Start, Stop ) / Count,

	[ First | Others ] = NewTables,

	% Checking:
	[ First = OtherTable || OtherTable <- Others ],

	{ Module, First, Timing }.



% Updates specified table and returns the corresponding average duration, in
% milliseconds (on average over Count runs).
%
update_table( Table, Module, Pairs ) ->

	Count = ?run_count,

	Start = time_utils:get_precise_timestamp(),

	NewTables = [ lists:foldl( fun( { K, V }, T ) ->
									Module:addEntry( K, V, T )
							   end,
							   _Acc0=Table,
							   _List=Pairs ) || _C <- lists:seq( 1, Count ) ],

	Stop = time_utils:get_precise_timestamp(),

	Timing = time_utils:get_precise_duration( Start, Stop ) / Count,

	[ First | Others ] = NewTables,

	% Checking:
	[ First = OtherTable || OtherTable <- Others ],

	{ Module, First, Timing }.



% Benchmarks look-up durations.
%
benchmark_look_ups( Table, Module, Pairs ) ->

	%io:format( "Benchmarking ~s on:~n~s~n",
	%		   [ Module, Module:toString( Table ) ] ),

	Count = 100 * ?run_count,

	Start = time_utils:get_precise_timestamp(),

	_Values = [ lists:foldl( fun( { K, V }, T ) ->
									 V = Module:getEntry( K, T ),
									 T
							   end,
							   _Acc0=Table,
							   _List=Pairs ) || _C <- lists:seq( 1, Count ) ],

	Stop = time_utils:get_precise_timestamp(),

	Timing = time_utils:get_precise_duration( Start, Stop ) / Count,

	{ Module, Timing }.



% Runs performance tests, to compare the various table implementations.
%
run_performance_tests() ->

	Implementations = [ hashtable, tracked_hashtable, lazy_hashtable,
						map_hashtable, list_hashtable ],

	% Change the order to see whether result variy:
	ActualImplementations = lists:reverse( Implementations ),

	EmptyTables = [ { M, M:new() } || M <- ActualImplementations ],

	Pairs = get_pairs(),

	io:format( "Feeding empty tables with ~B initial key/value pairs.~n",
			   [ length( Pairs ) ] ),

	% Do it 5 times at blank to avoid transition effects:
	_FedTablesWithTimings = [ [ feed_table( T, M, Pairs )
							 || { M, T } <- EmptyTables ]
							  || _C <- lists:seq( 1, 5 ) ],

	FedTablesWithTimings = [ feed_table( T, M, Pairs )
							 || { M, T } <- EmptyTables ],

	FedTimeStrings = [ text_utils:format( "for ~s: ~.3f ms", [ M, Timing ] )
				   || { M, _T, Timing } <- FedTablesWithTimings ],

	io:format( "~nFeed durations:~s",
			   [ text_utils:strings_to_string( FedTimeStrings ) ] ),



	FedSizeStrings = [ text_utils:format( "for ~s: ~s", [ M,
			 system_utils:interpret_byte_size_with_unit( basic_utils:size( T ) ) ] )
					   || { M, T, _Timing } <- FedTablesWithTimings ],

	io:format( "~nSizes:~s",
			   [ text_utils:strings_to_string( FedSizeStrings ) ] ),


	OtherPairs = get_other_pairs( Pairs ),

	io:format( "~n~nUpdating these tables with ~B key/value pairs "
			   "(equal mix of updated and new keys).~n",
			   [ length( OtherPairs ) ] ),

	FedTables = [ { M, T } || { M, T, _Timing } <- FedTablesWithTimings ],

	UpdatedTablesWithTimings = [ update_table( T, M, OtherPairs )
							   || { M, T } <- FedTables ],


	UpTimeStrings = [ text_utils:format( "for ~s: ~.3f ms", [ M, Timing ] )
				   || { M, _T, Timing } <- UpdatedTablesWithTimings ],

	io:format( "~nUpdate durations:~s",
			   [ text_utils:strings_to_string( UpTimeStrings ) ] ),


	UpSizeStrings = [ text_utils:format( "for ~s: ~s", [ M,
		system_utils:interpret_byte_size_with_unit( basic_utils:size( T ) ) ] )
					   || { M, T, _Timing } <- UpdatedTablesWithTimings ],

	io:format( "~nSizes:~s",
			   [ text_utils:strings_to_string( UpSizeStrings ) ] ),


	io:format( "~nBenchmarking look-ups (with no optimisation).~n" ),

	ShuffledPairs = list_utils:random_permute( OtherPairs ),

	LookedUpTimings = [ benchmark_look_ups( T, M, ShuffledPairs )
						|| { M, T, _Timings } <- UpdatedTablesWithTimings ],

	LookedUpStrings = [ text_utils:format( "for ~s: ~.1f microsec",
										   [ M, 1000 * Timing ] )
				   || { M, Timing } <- LookedUpTimings ],

	io:format( "~nLook-up durations:~s",
			   [ text_utils:strings_to_string( LookedUpStrings ) ] ),


	OptimisedTables = [ { M, M:optimise( T ), undefined }
						|| { M, T, _Timing } <- UpdatedTablesWithTimings ],


	io:format( "~nBenchmarking look-ups (after optimisation).~n" ),

	NewLookedUpTimings = [ benchmark_look_ups( T, M, ShuffledPairs )
						|| { M, T, _Timings } <- OptimisedTables ],

	NewLookedUpStrings = [ text_utils:format( "for ~s: ~.1f microsec",
											  [ M, 1000 * Timing ] )
				   || { M, Timing } <- NewLookedUpTimings ],

	io:format( "~nLook-up durations:~s",
			   [ text_utils:strings_to_string( NewLookedUpStrings ) ] ),


	FinalTablesWithTimings = UpdatedTablesWithTimings,

	AllListedTables = [ { _, FirstList } | Others ] =
		[ { M, lists:sort( M:enumerate( T ) ) }
		  || { M, T, _Timing } <- FinalTablesWithTimings ],

	Hashes = [ { M, erlang:phash2( L ) }
			   || { M, L } <- AllListedTables ],

	%io:format( "Checking content:~s", [ text_utils:strings_to_string(
	%									  [ io_lib:format( "for ~s: ~s",
	%												 [ M, M:toString( T ) ] )
	%			   || { M, T, _Timing } <- FinalTablesWithTimings ] ) ] ),

	io:format( "~nChecking hashes:~s", [ text_utils:strings_to_string(
					[ io_lib:format( "for ~s: ~p", [ M, H ] )
									   || { M, H } <- Hashes ] ) ] ),

	io:format( "~nChecking final states: ok~n" ),

	%io:format( "Reference: ~s~n", [ hashtable:toString( First ) ] ),


	[ FirstList= OtherListedTable || { _, OtherListedTable } <- Others ].
