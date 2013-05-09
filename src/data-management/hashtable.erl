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
% Creation date: July 2, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Generic hash table implementation.
% See hashtable_test.erl for the corresponding test.

% An hashtable is basically a tuple whose size (number of elements) is the
% number of buckets in the hashtable. Each element of the tuple is a list
% containing key/value pairs.

-module(hashtable).
% Directly depends on the text_utils module.

% Heavily inspired of the tupleStore example from
% 'Concurrent Programming in Erlang', section 9.8.



% The hashtable is implemented thanks to a tuple whose size is the number of
% buckets specified at the hashtable creation.
%
% Each tuple element (hence each bucket) is a list of key/value pairs.

% Maybe the ETS module, proplists, dict, etc. could/should be used instead.

% When the table holds less than 50 elements, probably using functions like
% lists:keystore/4 and lists:keymember/3 would be faster.



-export([ new/0, new/1, addEntry/3, addEntries/2,
	removeEntry/2, lookupEntry/2, hasEntry/2,
	getEntry/2, addToEntry/3, subtractFromEntry/3, toggleEntry/2,
	appendToEntry/3, deleteFromEntry/3, popFromEntry/2,
	enumerate/1, keys/1, getEntryCount/1, merge/2, toString/1, display/1 ]).



% The default number of hash buckets:
-define(DefaultNumberOfBuckets,256).



% Creates a new empty hash table with the default number of buckets.
new() ->
	new(?DefaultNumberOfBuckets).



% Returns a new empty hash table with specified number of buckets.
% For efficient access, there should be more buckets than entries.
new( NumberOfBuckets ) ->
	createTuple( NumberOfBuckets, [] ).



% Adds specified key/value pair into the specified hash table.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
addEntry( Key, Value, HashTable ) ->
	KeyIndex = erlang:phash2( Key, size(HashTable) ) + 1,
	% Retrieve appropriate tuple slot:
	PreviousList = element( KeyIndex, HashTable ),
	NewList = replaceBucket( Key, Value, PreviousList, [] ),
	setelement( KeyIndex, HashTable, NewList ).



% Adds specified list of key/value pair into the specified hash table.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
addEntries([],HashTable) ->
	HashTable;

addEntries( [ {EntryName,EntryValue} | Rest ], HashTable ) ->
	addEntries( Rest, addEntry( EntryName, EntryValue, HashTable ) ).



% Removes specified key/value pair from the specified hash table.
%
% Does nothing if the key is not found.
removeEntry( Key, HashTable ) ->
	KeyIndex = erlang:phash2( Key, size(HashTable) ) + 1,
	PreviousList = element( KeyIndex, HashTable ),
	NewList = deleteBucket( Key, PreviousList, [] ),
	setelement( KeyIndex, HashTable, NewList ).


% Looks-up specified entry (designated by its key) in specified hash table.
%
% Returns either {hashtable_key_not_found,Key} if no such key is registered in
% the table, or {value,Value}, with Value being the value associated to the
% specified key.
lookupEntry( Key, HashTable ) ->
	lookupInList(Key, element( erlang:phash2(Key,size(HashTable) ) + 1 ,
		HashTable)).



% Tells whether the specified key exists in the table: returns true or false.
hasEntry( Key, HashTable ) ->

	case lookupInList(Key,
			element(erlang:phash2(Key,size(HashTable))+1,HashTable) ) of

		{value,_} ->
			true;

		{hashtable_key_not_found,_Key} ->
			false

	end.



% Retrieves the value corresponding to specified key and returns it directly.
%
% The key/value pair is expected to exist already, otherwise a bad match is
% triggered.
getEntry( Key, HashTable ) ->
	{value,Value} = lookupInList( Key,
		element(erlang:phash2(Key,size(HashTable))+1, HashTable )),
	Value.



% Adds specified value to the value, supposed to be numerical, associated to
% specified key.
%
% A case clause is triggered if the key did not exist, a bad arithm is triggered
% if no addition can be performed on the associated value.
addToEntry( Key, Value, HashTable ) ->
	{value,Number} = lookupInList( Key,
		element(erlang:phash2(Key,size(HashTable))+1, HashTable )),
	addEntry(Key,Number+Value,HashTable).



% Subtracts specified value to the value, supposed to be numerical, associated
% to specified key.
%
% A case clause is triggered if the key did not exist, a bad arithm is triggered
% if no subtraction can be performed on the associated value.
subtractFromEntry( Key, Value, HashTable ) ->
	{value,Number} = lookupInList( Key,
		element(erlang:phash2(Key,size(HashTable))+1, HashTable )),
	addEntry(Key,Number-Value,HashTable).



% Toggles the boolean value associated with specified key: if true will be
% false, if false will be true.
%
% A case clause is triggered if the entry does not exist or it is not a boolean
% value.
toggleEntry( Key, HashTable ) ->

	case lookupInList( Key,
			element(erlang:phash2(Key,size(HashTable))+1, HashTable )) of

		{value,true} ->
			addEntry(Key,false,HashTable);

		{value,false} ->
			addEntry(Key,true,HashTable)

	end.



% Appends specified element to the value, supposed to be a list, associated to
% specified key.
%
% A case clause is triggered if the entry does not exist.
% Note: no check is performed to ensure the value is a list indeed, and the
% '[|]' operation will not complain if not.
appendToEntry( Key, Element, HashTable ) ->
	{value,List} = lookupInList( Key,
		element(erlang:phash2(Key,size(HashTable))+1, HashTable )),
	addEntry( Key, [Element|List], HashTable ).



% Appends specified element to the value, supposed to be a list, associated to
% specified key.
%
% Note: no check is performed to ensure the value is a list indeed, and the
% '[|]' operation will not complain if not.
%
% Deletes the first match of specified element from the value specified from
% key, that value being supposed to be a list.
%
% A case clause is triggered if the entry did not exist.  If the element is not
% in the specified list, the list will not be modified.
deleteFromEntry( Key, Element, HashTable ) ->
	{value,List} = lookupInList(Key,
		element(erlang:phash2(Key,size(HashTable))+1,HashTable)),
	addEntry(Key,lists:delete(Element,List),HashTable).



% Pops the head of the value (supposed to be a list) associated to specified
% key, and returns a pair made of the popped head and the new hashtable.
popFromEntry(Key,HashTable) ->
	{value,[H|T]} = lookupEntry(Key,HashTable),
	{H,addEntry(Key,T,HashTable)}.





% Returns a flat list whose elements are all the key/value pairs of the
% hashtable.
%
% Ex: [ {K1,V1}, {K2,V2},.. ].
enumerate(Hashtable) ->
	lists:flatten( tuple_to_list(Hashtable) ).



% Returns a list containing all the keys of this hashtable.
keys(Hashtable) ->
	get_keys_from_buckets( tuple_to_list(Hashtable), [] ).



% Returns the number of entries (key/value pairs) stored in specified hashtable.
getEntryCount(Hashtable) ->
	erlang:length( enumerate(Hashtable) ).



% Returns a new hashtable, which started from HashTableBase and was enriched
% with the HashTableAdd entries whose keys where not already in HashTableBase
% (if a key is in both tables, the one from HashTableBase will be kept).
merge(HashTableBase,HashTableAdd) ->
	% Uses the fact that when two entries with the same key are added, the final
	% associated value is the one of the latest to be added.
	lists:foldl(
		fun({Key,Value},Acc) -> addEntry(Key,Value,Acc) end,
		HashTableAdd,
		enumerate(HashTableBase)).


% Returns a textual description (plain string) of the state of this hashtable.
toString(HashTable) when size(HashTable) > 0 ->
	lists:foldl(
		fun(Bucket,Acc) ->
			Acc ++ io_lib:format(
				"  + ~s~n",[bucket_toString(Bucket)])
		end,
		io_lib:format( "Hashtable with ~B bucket(s) and ~B entry(ies): ~n",
			[ size(HashTable), hashtable:getEntryCount(HashTable) ]),
		tuple_to_list(HashTable));

toString(_) ->
	io_lib:format("Empty hashtable~n",[]).




% Displays in the standard output a textual description (plain string) of the
% state of this hashtable
display(HashTable) ->
	io:format( "~s",[ toString(HashTable) ]).





% Helper functions.


% Returns a new tuple, whose size is length and whose elements are all set to
% specified default value (Default).
createTuple(Length,Default) ->
	createTuple(Length,Default,[]).


% Final step:
createTuple(0,_,Accumulator) ->
	list_to_tuple(Accumulator);


% Building from n-1 to n elements:
createTuple(N,Default,Accumulator) ->
	createTuple(N-1,Default,[Default|Accumulator]).



% Removes pair entry from list when the key matches the specified one:
% (returns an identical list if the key is not found)
deleteBucket( Key, [{Key,_Value}|T], Accumulator ) ->
	% Skips the key if matching:
	lists:append( T, Accumulator );


deleteBucket( Key, [H|T], Acc ) ->
	% Keeps everything else:
	deleteBucket( Key, T, [H|Acc] );


deleteBucket( _Key, [], Acc ) ->
	Acc.



% Replaces in specified list a key/value pair by another:
replaceBucket( Key, Value, [], Acc )	->
	[{Key,Value}|Acc];

replaceBucket( Key, Value, [{Key,_}|T], Acc ) ->
	[{Key,Value}|lists:append(T,Acc)];

replaceBucket( Key, Value, [H|T], Acc ) ->
	replaceBucket( Key, Value, T, [H|Acc] ).




% Returns a string describing a hashtable bucket (list of key/value pairs):
bucket_toString(Bucket) when length(Bucket) > 0 ->
	lists:foldl(
		fun({Key,Value},Acc) ->
			Acc ++ io_lib:format( "     * ~s -> ~s~n",
				[ text_utils:term_toString(Key),
				  text_utils:term_toString(Value) ])
		end,
		io_lib:format( "Bucket with ~B element(s):~n",
			[length(Bucket)]),
		Bucket);

bucket_toString(_) ->
	"Empty bucket".



% Returns the value corresponding to the key in the specified list:
lookupInList( Key, [] ) ->
	{hashtable_key_not_found,Key};

lookupInList( Key, [{Key,Value}|_] ) ->
	{value,Value};

lookupInList( Key, [_|T] ) ->
	lookupInList(Key,T).


get_keys_from_buckets( [], Acc ) ->
	Acc;

get_keys_from_buckets( [H|T], Acc ) ->
	get_keys_from_buckets( T, [ Key || {Key,_Value} <- H ] ++ Acc ).
