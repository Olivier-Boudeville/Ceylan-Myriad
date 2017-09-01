% Copyright (C) 2014-2017 Olivier Boudeville
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

% Creation date: Tuesday, December 2, 2014
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)



% Implementation of an hashtable based on the then newly-introduced standard
% type: the map, supposedly the most efficient available implementation of an
% associative table.
%
% See map_table_test.erl for the corresponding test.
% See hashtable.erl for parent, base implementation.
%
%
% We provide different multiple types of hashtables, including:
%
% - 'hashtable', the most basic, safest, reference implementation
% - and quite efficient as well
%
% - 'tracked_hashtable', an attempt of optimisation of it (not necessarily the
% best)
%
% - 'lazy_hashtable', deciding to optimise in a less costly way
% than 'tracked_hashtable'
%
% - 'map_hashtable' (this module), which is probably the most efficient
% implementation (speed/size compromise)
%
% - 'list_hashtable', a list-based implementation, efficient for smaller table
% (and only them)
%
% All these types of tables are to provide the same API (signatures and
% contracts), yet one should note that this module is the one that tends to
% supersede all others, and that over time features have been added that may not
% have been back-ported to the other table types.
%
-module(map_hashtable).


% Exact same API as the one of hashtable:
%
-export([ new/0, new/1,
		  addEntry/3, addEntries/2, addNewEntry/3, addNewEntries/2,
		  updateEntry/3, updateEntries/2,
		  removeEntry/2, removeExistingEntry/2,
		  removeEntries/2, removeExistingEntries/2,
		  lookupEntry/2, hasEntry/2, getEntry/2,
		  extractEntry/2, getEntries/2,
		  getValue/2, getValues/2, getValueWithDefaults/3, getAllValues/2,
		  addToEntry/3, subtractFromEntry/3, toggleEntry/2,
		  appendToExistingEntry/3, appendListToExistingEntry/3,
		  appendToEntry/3, appendListToEntry/3,
		  concatToEntry/3,
		  deleteFromEntry/3, deleteExistingFromEntry/3,
		  popFromEntry/2,
		  enumerate/1, selectEntries/2, keys/1, values/1,
		  isEmpty/1, size/1, getEntryCount/1,
		  mapOnEntries/2, mapOnValues/2,
		  foldOnEntries/3,
		  merge/2, optimise/1, toString/1, toString/2, display/1, display/2 ]).



-type key() :: hashtable:key().

-type value() :: hashtable:value().

-type entry() :: hashtable:entry().

-type entries() :: [ entry() ].

-type entry_count() :: basic_utils:count().


-opaque map_hashtable() :: map().

% Since 18.0, map/2 does not seem to exist anymore:
%-opaque map_hashtable( K, V ) :: map( K, V ).
-opaque map_hashtable( _K, _V ) :: map().


-export_type([ key/0, value/0, entry/0, entries/0, entry_count/0,
			   map_hashtable/0, map_hashtable/2 ]).



% Implementation notes:
%
% This module is bootstrapped, as it is used by the meta_utils module to keep
% track of the spotted functions in such a map-based table. As a consequence at
% least the subset of the functions defined here and used by meta_utils should
% themselves not rely on any other module (whose BEAM would then not be
% available).

% Due to the partial support of maps in 17.3, some parts are commented and
% replaced by less idiomatic counterparts. Later they will be reactivated.




% Returns a new empty map table.
%
-spec new() -> map_hashtable().
new() ->
	% Empty map:
	#{}.



% As map hashtables manage by themselves their size, no need to specify any
% target size. This function is only defined so that we can transparently switch
% APIs with the hashtable module.
%
-spec new( hashtable:entry_count() | hashtable:entries() ) -> map_hashtable().
new( ExpectedNumberOfEntries ) when is_integer( ExpectedNumberOfEntries ) ->
	#{};

new( InitialEntries ) when is_list( InitialEntries ) ->
	maps:from_list( InitialEntries ).



% Hints for the addition of key/value pairs to a table:
%
% - if one does not know or care whether the specified key is already used in
% the table, one should use addEntry/3 and addEntries/2 (and no specific
% checking about the key will be done)
%
% - if one knows that the specified key *is not* already used in the table, one
% should use addNewEntry/3 and addNewEntries/2 (and the absence of the key will
% be checked)
%
% - if one knows that the specified key *is* already used in the table, one
% should use updateEntry/3 and updateEntries/2 (and the presence of the key will
% be checked)
%
%
% Note: in non-debug mode, these extra-checkings may be removed.



% Adds specified key/value pair into the specified map hashtable.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one (hence does not check whether or not the key
% already exist in this table).
%
-spec addEntry( key(), value(), map_hashtable() ) -> map_hashtable().
addEntry( Key, Value, MapHashtable ) ->
	% Not supported in 17.3: MapHashtable#{ Key => Value }.
	maps:put( Key, Value, MapHashtable ).



% Adds specified list of key/value pairs into the specified map table.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one (hence does not check whether or not keys
% already exist in this table).
%
-spec addEntries( hashtable:entries(), map_hashtable() ) -> map_hashtable().
addEntries( EntryList, MapHashtable ) ->

	lists:foldl( fun( { K, V }, Map ) ->
						 %Map#{ K => V }
						 maps:put( K, V, Map )
				 end,
				 _Acc0=MapHashtable,
				 _List=EntryList ).



% Adds specified key/value pair into the specified map hashtable, expecting this
% key not to be already defined in this table.
%
-spec addNewEntry( key(), value(), map_hashtable() ) -> map_hashtable().
addNewEntry( Key, Value, MapHashtable ) ->

	% A tad expensive, could be replaced by an inlined addEntry/3 in non-debug
	% mode:
	%
	case hasEntry( Key, MapHashtable ) of

		false ->
			addEntry( Key, Value, MapHashtable );

		true ->
			throw( { key_already_existing, Key } )

	end.



% Adds specified list of key/value pairs into the specified map table, expecting
% that none of these keys is already defined in this table.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec addNewEntries( hashtable:entries(), map_hashtable() ) -> map_hashtable().
addNewEntries( EntryList, MapHashtable ) ->

	lists:foldl( fun( { K, V }, Map ) ->
						 addNewEntry( K, V, Map )
				 end,
				 _Acc0=MapHashtable,
				 _List=EntryList ).



% Updates the specified key with the specified value in the specified map
% hashtable.
%
% A pair with this key is expected to already exist in this table.
%
-spec updateEntry( key(), value(), map_hashtable() ) -> map_hashtable().
updateEntry( Key, Value, MapHashtable ) ->

	% A tad expensive, could be replaced by an inlined addEntry/3 in non-debug
	% mode:
	%
	case hasEntry( Key, MapHashtable ) of

		true ->
			addEntry( Key, Value, MapHashtable );

		false ->
			throw( { key_not_already_existing, Key } )

	end.



% Updates specified list of keys with specified values in specified map
% hashtable.
%
% For each of the listed keys, a corresponding pair is expected to already exist
% in this table.
%
-spec updateEntries( hashtable:entries(), map_hashtable() ) -> map_hashtable().
updateEntries( EntryList, MapHashtable ) ->

	% Should be optimised:
	%
	lists:foldl( fun( { K, V }, Map ) ->
					updateEntry( K, V, Map )
				 end,
				 _Acc0=MapHashtable,
				 _List=EntryList ).



% Removes specified key/value pair, as designated by the key, from the specified
% table.
%
% Does nothing if the key is not found.
%
% Returns an updated table.
%
-spec removeEntry( key(), map_hashtable() ) -> map_hashtable().
removeEntry( Key, MapHashtable ) ->
	% Same semantics:
	maps:remove( Key, MapHashtable ).



% Removes specified key/value pair, as designated by the key, from the specified
% table.
%
% Throws an exception if the key is not found.
%
% Returns an updated table.
%
-spec removeExistingEntry( key(), map_hashtable() ) -> map_hashtable().
removeExistingEntry( Key, MapHashtable ) ->

	case hasEntry( Key, MapHashtable ) of

		true ->
			removeEntry( Key, MapHashtable ) ;

		false ->
			throw( { non_existing_key, Key } )

	end.



% Removes specified key/value pairs, as designated by the keys, from the
% specified table.
%
% Specifying a non-existing key is accepted.
%
% Returns an updated table.
%
-spec removeEntries( [ key() ], map_hashtable() ) -> map_hashtable().
removeEntries( Keys, MapHashtable ) ->
	lists:foldl( fun( K, AccTable ) ->
						 maps:remove( K, AccTable )
				 end,
				 _InitAcc=MapHashtable,
				 _List=Keys ).



% Removes specified key/value pairs, as designated by the keys, from the
% specified table.
%
% Throws an exception if a key is not found.
%
% Returns an updated table.
%
-spec removeExistingEntries( [ key() ], map_hashtable() ) -> map_hashtable().
removeExistingEntries( Keys, MapHashtable ) ->
	lists:foldl( fun( K, AccTable ) ->
						 removeExistingEntry( K, AccTable )
				 end,
				 _InitAcc=MapHashtable,
				 _List=Keys ).



% Looks-up specified entry (designated by its key) in specified map table.
%
% Returns either 'key_not_found' if no such key is registered in the
% table, or { value, Value }, with Value being the value associated to the
% specified key.
%
-spec lookupEntry( key(), map_hashtable() ) ->
						 'key_not_found' | { 'value', value() }.
% Not supported in 17.3:
% lookupEntry( Key, #{ Key := Value } ) ->
%	{ value, Key };

% lookupEntry( _Key, _MapHashtable ) ->
%	key_not_found.
lookupEntry( Key, MapHashtable ) ->

	case maps:find( Key, MapHashtable ) of

		{ ok, Value } ->
			{ value, Value };

		error ->
			key_not_found

	end.



% Tells whether the specified key exists in the table: returns true or false.
%
-spec hasEntry( key(), map_hashtable() ) -> boolean().
hasEntry( Key, MapHashtable ) ->
	maps:is_key( Key, MapHashtable ).

% hasEntry( Key, #{ Key := _Value } ) ->
%	true;

% hasEntry( _Key, _MapHashtable ) ->
%	false.



% Retrieves the value corresponding to specified (existing) key and returns it
% directly.
%
% The key/value pair is expected to exist already, otherwise an exception
% ({bad_key,Key}) is triggered.
%
% Note: could have been named as well getValue/2.
%
-spec getEntry( key(), map_hashtable() ) -> value().
%getEntry( Key,  #{ Key := Value } ) ->
%	Value.
getEntry( Key, MapHashtable ) ->
	maps:get( Key, MapHashtable ).



% Retrieves the value corresponding to specified (existing) key and returns it
% directly.
%
% The key/value pair is expected to exist already, otherwise an exception
% ({bad_key,Key}) is triggered.
%
% Note: defined for naming consistency of the API.
%
-spec getValue( key(), map_hashtable() ) -> value().
getValue( Key, MapHashtable ) ->
	getEntry( Key, MapHashtable ).




% Returns the (ordered) list of values that correspond to the specified
% (ordered) list of keys of this table.
%
% The key/value pairs are expected to exist already, otherwise an exception is
% raised.
%
% Ex: [ Color, Age, Mass ] = map_hashtable:getEntries( [ color, age, mass ],
%                                                      MyMapTable ] )
%
% Note: could have been named as well getValues/2.
%
-spec getEntries( [ key() ], map_hashtable() ) -> [ value() ].
getEntries( Keys, Hashtable ) ->

	{ RevValues, _FinalTable } = lists:foldl(

				fun( _Elem=Key, _Acc={ Values, Table } ) ->

					   { Value, ShrunkTable } = extractEntry( Key, Table ),
					   { [ Value | Values ], ShrunkTable }

				end,
				_Acc0={ [], Hashtable },
				_List=Keys ),

	lists:reverse( RevValues ).



% Looks for specified entry in specified table and, if found, returns the
% associated value; otherwise returns the specified default value.
%
-spec getValueWithDefaults( key(), value(), map_hashtable() ) -> value().
getValueWithDefaults( Key, DefaultValue, MapHashtable ) ->

	case maps:find( Key, MapHashtable ) of

		{ ok, Value } ->
			Value;

		error ->
			DefaultValue

	end.



% Returns the (ordered) list of values that correspond to the specified
% (ordered) list of keys of this table.
%
% The key/value pairs are expected to exist already, otherwise an exception is
% raised.
%
% Ex: [ Color, Age, Mass ] = map_hashtable:getValues( [ color, age, mass ],
%                                                     MyMapTable ] )
%
% Note: defined for naming consistency of the API.
%
-spec getValues( [ key() ], map_hashtable() ) -> [ value() ].
getValues( Keys, Hashtable ) ->
	getEntries( Keys, Hashtable ).



% Returns the (ordered) list of values that correspond to the specified
% (ordered) list of keys of this table, ensuring all entries have been read,
% otherwise throwing an exception.
%
% The key/value pairs are expected to exist already, otherwise an exception is
% raised.
%
% Ex: [ Color=red, Age=23, Mass=51 ] = map_hashtable:getAllValues( [ color,
%   age, mass ], [ { color, red }, { mass, 51 }, { age, 23 } ] )
%
-spec getAllValues( [ key() ], map_hashtable() ) -> [ value() ].
getAllValues( Keys, Hashtable ) ->

	{ RevValues, FinalTable } = lists:foldl(
		   fun( _Elem=Key, _Acc={ Values, Table } ) ->

				   { Value, ShrunkTable } = extractEntry( Key, Table ),
				   { [ Value | Values ], ShrunkTable }

		   end,
		   _Acc0={ [], Hashtable },
		   _List=Keys ),

	case isEmpty( FinalTable ) of

		true ->
			lists:reverse( RevValues );

		false ->
			throw( { remaining_keys, keys( FinalTable ) } )

	end.



% Extracts specified entry from specified hashtable, i.e. returns the associated
% value and removes that entry from the table.
%
% The key/value pair is expected to exist already, otherwise an exception is
% raised (typically {badkey,KeyNotFound}).
%
-spec extractEntry( key(), map_hashtable() ) -> { value(), map_hashtable() }.
%extractEntry( Key, MapHashtable=#{ Key := Value} ) ->
%	{ Value, maps:remove( Key, MapHashtable ) }.
%
extractEntry( Key, MapHashtable ) ->
	Value = maps:get( Key, MapHashtable ),
	{ Value, maps:remove( Key, MapHashtable ) }.



% Applies (maps) the specified anonymous function to each of the key-value
% entries contained in this hashtable.
%
% Allows to apply "in-place" an operation on all entries without having to
% enumerate the content of the hashtable and iterate on it (hence without having
% to duplicate the whole content in memory).
%
% Note: as the fun may return modified keys, the whole structure of the
% hashtable may change (ex: different buckets used for replaced entries,
% colliding keys resulting in having less entries afterwards, etc.).
%
% One may request the returned hashtable to be optimised after this call.
%
-spec mapOnEntries( fun( ( entry() ) -> entry() ), map_hashtable() ) ->
						  map_hashtable().
mapOnEntries( Fun, MapHashtable ) ->

	% maps:map/2 keeps the same keys, not relevant here.

	Entries = maps:to_list( MapHashtable ),

	NewEntries = lists:map( Fun, Entries ),

	maps:from_list( NewEntries ).



% Applies (maps) the specified anonymous function to each of the values
% contained in this hashtable.
%
% Allows to apply "in-place" an operation on all values without having to
% enumerate the content of the hashtable and iterate on it (hence without having
% to duplicate the whole content in memory).
%
% Note: the keys are left as are, hence the structure of the hashtable does not
% change.
%
-spec mapOnValues( fun( ( value() ) -> value() ), map_hashtable() ) ->
						 map_hashtable().
mapOnValues( Fun, MapHashtable ) ->

	% Still not maps:map/2, whose fun takes an entry, not just a value:
	NewEntries = [ { K, Fun( V ) }
				   || { K, V } <- maps:to_list( MapHashtable ) ],

	maps:from_list( NewEntries ).




% Folds specified anonymous function on all entries of the specified map
% hashtable.
%
% The order of transformation for entries is not specified.
%
% Returns the final accumulator.
%
-spec foldOnEntries( fun( ( entry(), basic_utils:accumulator() )
						  -> basic_utils:accumulator() ),
					 basic_utils:accumulator(),
					 map_hashtable() ) -> basic_utils:accumulator().
foldOnEntries( Fun, InitialAcc, MapHashtable ) ->

	% Not exactly as maps:fold/3: we want f( { X, Y }, Acc ), not
	% f( X, Y, Acc ).

	ConversionFun = fun( K, V, Acc ) ->
							Fun( { K, V }, Acc )
					end,

	maps:fold( ConversionFun, InitialAcc, MapHashtable ).

	% Another solution is to implement it by ourselves:
	%Entries = maps:to_list( MapHashtable ),
	%lists:foldl( Fun, InitialAcc, Entries ).






% Adds specified value to the value, supposed to be numerical, associated to
% specified key.
%
% An exception is thrown if the key does not exist, a bad arithm is triggered if
% no addition can be performed on the associated value.
%
-spec addToEntry( key(), number(), map_hashtable() ) -> map_hashtable().
% addToEntry( Key, Value, MapHashtable=#{ Key => BaseValue } ) ->
%	MapHashtable#{ Key => BaseValue + Value };
%
% addToEntry( _Key, _Value, _MapHashtable ) ->
%	throw( { key_not_found, Key } ).
%
addToEntry( Key, Value, MapHashtable ) ->

	case maps:find( Key, MapHashtable ) of

		{ ok, BaseValue } ->
			maps:put( Key, BaseValue + Value, MapHashtable );

		error ->
			throw( { key_not_found, Key } )

	end.



% Subtracts specified value to the value, supposed to be numerical, associated
% to specified key.
%
% An exception is thrown if the key does not exist, a bad arithm is triggered if
% no subtraction can be performed on the associated value.
%
-spec subtractFromEntry( key(), number(), map_hashtable() ) -> map_hashtable().
% subtractFromEntry( Key, Value, MapHashtable=#{ Key => BaseValue } ) ->
%	MapHashtable#{ Key => BaseValue - Value };
%
% subtractFromEntry( _Key, _Value, _MapHashtable ) ->
%	throw( { key_not_found, Key } ).
%
subtractFromEntry( Key, Value, MapHashtable ) ->

	case maps:find( Key, MapHashtable ) of

		{ ok, BaseValue } ->
			maps:put( Key, BaseValue - Value, MapHashtable );

		error ->
			throw( { key_not_found, Key } )

	end.



% Toggles the boolean value associated with specified key: if true will be
% false, if false will be true.
%
% An exception is thrown if the key does not exist or if its associated value is
% not a boolean.
%
-spec toggleEntry( key(), map_hashtable() ) -> map_hashtable().
% toggleEntry( Key, MapHashtable=#{ Key => true } ) ->
%	MapHashtable#{ Key => false };
%
% toggleEntry( Key, MapHashtable=#{ Key => false } ) ->
%	MapHashtable#{ Key => true }.
toggleEntry( Key, MapHashtable )->

	case maps:get( Key, MapHashtable ) of

		true ->
			maps:put( Key, false, MapHashtable );

		false ->
			maps:put( Key, true, MapHashtable )

	end.



% Returns a new map hashtable, which started from MapHashtableBase and was
% enriched with the MapHashtableAdd entries whose keys where not already in
% MapHashtableBase (if a key is in both tables, the one from MapHashtableBase
% will be kept).
%
% Said differently: if a key exists in both tables, the value in MapHashtableAdd
% will be superseded by the value in MapHashtableBase.
%
% Note: not the standard merge that one would expect, should values be lists.
%
-spec merge( map_hashtable(), map_hashtable() ) -> map_hashtable().
merge( MapHashtableBase, MapHashtableAdd ) ->
	% Order matters:
	maps:merge( MapHashtableAdd, MapHashtableBase ).



% Optimises this hashtable.
%
% A no-operation for map hashtables.
%
-spec optimise( map_hashtable() ) -> map_hashtable().
optimise( Hashtable ) ->
	Hashtable.



% Appends specified element to the value, supposed to be a list, associated to
% specified key, which must already exist in that table.
%
% An exception is thrown if the key does not exist.
%
% Note: no check is performed to ensure the value is a list indeed, and the
% '[|]' operation will not complain if not.
%
-spec appendToExistingEntry( key(), term(), map_hashtable() ) ->
								   map_hashtable().
%appendToExistingEntry( Key, Element, MapHashtable=#{ Key => ListValue } ) ->
%	MapHashtable#{ Key => [ Element | ListValue ] };
%
%appendToExistingEntry( Key, _Element, _MapHashtable ) ->
%	throw( { key_not_found, Key } ).
%
appendToExistingEntry( Key, Element, MapHashtable ) ->

	ListValue = maps:get( Key, MapHashtable ),

	maps:put( Key, [ Element | ListValue ], MapHashtable ).



% Appends specified elements to the value, supposed to be a list, associated to
% specified key.
%
% An exception is thrown if the key does not exist.
%
-spec appendListToExistingEntry( key(), [ term() ], map_hashtable() ) ->
								   map_hashtable().
appendListToExistingEntry( Key, Elements, Hashtable ) ->

	ListValue = maps:get( Key, Hashtable ),

	maps:put( Key, Elements ++ ListValue, Hashtable ).




% Appends specified element to the value, supposed to be a list, associated to
% specified key.
%
% If that key does not already exist, it will be created and associated to a
% list containing only the specified element.
%
-spec appendToEntry( key(), term(), map_hashtable() ) -> map_hashtable().
appendToEntry( Key, Element, MapHashtable ) ->

	case lookupEntry( Key, MapHashtable ) of

		key_not_found ->
			addEntry( Key, [ Element ], MapHashtable );

		{ value, CurrentList } ->
			addEntry( Key, [ Element | CurrentList ], MapHashtable )

	end.



% Appends specified elements to the value, supposed to be a list, associated to
% specified key.
%
% If that key does not already exist, it will be created and associated to a
% list containing only the specified elements.
%
-spec appendListToEntry( key(), [ term() ], map_hashtable() ) ->
							   map_hashtable().
appendListToEntry( Key, Elements, MapHashtable ) ->

	case lookupEntry( Key, MapHashtable ) of

		'key_not_found' ->
			addEntry( Key, Elements, MapHashtable );

		{ value, CurrentList } ->
			addEntry( Key, Elements ++ CurrentList, MapHashtable )

	end.



% Concatenes (on the left) specified list to the value, supposed to be a list as
% well, associated to specified key.
%
% If that key does not already exist, it will be created and associated to the
% specified list (as if beforehand the key was associated to an empty list)
%
-spec concatToEntry( key(), list(), map_hashtable() ) -> map_hashtable().
concatToEntry( Key, ListToConcat, MapHashtable ) when is_list( ListToConcat ) ->

	case lookupEntry( Key, MapHashtable ) of

		'key_not_found' ->
			addEntry( Key, ListToConcat, MapHashtable );

		{ value, CurrentList } ->
			addEntry( Key, ListToConcat ++ CurrentList, MapHashtable )

	end.



% Deletes the first match of the specified element in the value associated to
% specified key, this value being assumed to be a list.
%
% An exception is thrown if the key does not exist.
%
% If the element is not in the specified list, the list will not be modified.
%
-spec deleteFromEntry( key(), term(), map_hashtable() ) -> map_hashtable().
%deleteFromEntry( Key, Element, MapHashtable=#{ Key => ListValue } ) ->
%	MapHashtable#{ Key => lists:delete( Element, ListValue ) };
%
%deleteFromEntry( Key, _Element, _MapHashtable ) ->
%	throw( { key_not_found, Key } ).
%
deleteFromEntry( Key, Element, MapHashtable ) ->

	ListValue = maps:get( Key, MapHashtable ),

	maps:put( Key, lists:delete( Element, ListValue ), MapHashtable ).



% Deletes the first match of the specified element in the value associated to
% specified key, this value being assumed to be a list.
%
% An exception is thrown if the key does not exist, or if the element is not in
% the targeted list.
%
-spec deleteExistingFromEntry( key(), term(), map_hashtable() ) ->
									 map_hashtable().
deleteExistingFromEntry( Key, Element, MapHashtable ) ->

	ListValue = maps:get( Key, MapHashtable ),

	maps:put( Key, list_utils:delete_existing( Element, ListValue ),
			  MapHashtable ).



% Pops the head of the value (supposed to be a list) associated to specified
% key, and returns a pair made of the popped head and the new hashtable.
%
-spec popFromEntry( key(), map_hashtable() ) -> { term(), map_hashtable() }.
%popFromEntry( Key, MapHashtable=#{ Key => [ H | T ] } ) ->
%	{ H, MapHashtable#{ Key => T } };
%
%popFromEntry( Key, _MapHashtable ) ->
%	throw( { key_not_found, Key } ).
%
popFromEntry( Key, MapHashtable ) ->

	[ H | T ] = maps:get( Key, MapHashtable ),

	{ H, maps:put( Key, T, MapHashtable ) }.



% Returns a flat list whose elements are all the key/value pairs of the
% hashtable, in no particular order.
%
% Ex: [ {K1,V1}, {K2,V2}, ... ].
%
-spec enumerate( map_hashtable() ) -> hashtable:entries().
enumerate( MapHashtable ) ->
	maps:to_list( MapHashtable ).



% Returns a list of key/value pairs corresponding to the list of specified keys,
% or throws a badmatch is at least one key is not found.
%
-spec selectEntries( [ key() ], map_hashtable() ) -> hashtable:entries().
selectEntries( Keys, MapHashtable ) ->

	SubMap = maps:with( Keys, MapHashtable ),

	maps:to_list( SubMap ).



% Returns a list containing all the keys of this hashtable.
%
-spec keys( map_hashtable() ) -> [ key() ].
keys( MapHashtable ) ->
	maps:keys( MapHashtable ).


% Returns a list containing all the values of this hashtable.
%
% Ex: useful if the key was used as an index to generate this table first.
%
-spec values( map_hashtable() ) -> [ value() ].
values( MapHashtable ) ->
	maps:values( MapHashtable ).



% Returns whether the specified hashtable is empty (not storing any key/value
% pair).
%
-spec isEmpty( map_hashtable() ) -> boolean().
%isEmpty( _MapHashtable=#{} ) ->
%	true;
%
%isEmpty( _MapHashtable ) ->
%	false.
%
isEmpty( MapHashtable ) when map_size( MapHashtable ) > 0 ->
	false;

isEmpty( _MapHashtable ) ->
	true.



% Returns the size (number of entries) of this hashtable.
%
-spec size( map_hashtable() ) -> hashtable:entry_count().
size( MapHashtable ) ->
	map_size( MapHashtable ).



% Returns the number of entries (key/value pairs) stored in the specified map
% hashtable.
%
-spec getEntryCount( map_hashtable() ) -> hashtable:entry_count().
getEntryCount( MapHashtable ) ->
	map_size( MapHashtable ).



% Returns a textual description of the specified hashtable.
%
-spec toString( map_hashtable() ) -> string().
toString( MapHashtable ) ->
	toString( MapHashtable, _DefaultBullet=" + " ).


% Returns a textual description of the specified hashtable.
%
% Either a bullet is specified, or the returned string is either quite raw (if
% using 'internal') or a bit more elaborate (if using 'user_friendly').
%
% For this implementation, the requested description type does not matter.
%
-spec toString( map_hashtable(), string() | 'internal' | 'user_friendly' ) ->
					  string().
toString( MapHashtable, Bullet ) when is_list( Bullet ) ->

	case maps:to_list( MapHashtable ) of

		[] ->
			"Empty hashtable";

		L ->

			% Enforces a consistent order:
			Strings = [ io_lib:format( "~p: ~p", [ K, V ] )
						|| { K, V } <- lists:sort( L ) ],

			% Flatten is needed, in order to use the result with ~s:
			lists:flatten( io_lib:format( "Hashtable with ~B entry(ies):~s",
				[ map_size( MapHashtable ),
				  text_utils:string_list_to_string( Strings, Bullet ) ] ) )

	end;

toString( MapHashtable, _DescriptionType ) ->
	toString( MapHashtable ).



% Displays the specified hashtable on the standard output.
%
-spec display( map_hashtable() ) -> basic_utils:void().
display( MapHashtable ) ->
	io:format( "~s~n", [ toString( MapHashtable ) ] ).



% Displays the specified hashtable on the standard output, with the specified
% title on top.
%
-spec display( string(), map_hashtable() ) -> basic_utils:void().
display( Title, MapHashtable ) ->
	io:format( "~s:~n~s~n", [ Title, toString( MapHashtable ) ] ).
