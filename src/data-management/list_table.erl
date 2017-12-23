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
%
% Creation date: Monday, December 22, 2014
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)



% Table implementation relying on a simple list of key/value pairs.
%
% See list_table_test.erl for the corresponding test.
%
% We provide different multiple types of tables, including:
%
% - 'hashtable', the most basic, safest, reference implementation - and quite
% efficient as well
%
% - 'tracked_table', an attempt of optimisation of it (not necessarily the best)
%
% - 'lazy_table', deciding to optimise in a less costly way than 'tracked_table'
%
% - 'map_table', which is probably the most efficient implementation (speed/size
% compromise)
%
% - 'list_table' (this module), a list-based implementation, efficient for
% smaller table (and only them)
%
% They are to provide the same API (signatures and contracts).

-module(list_table).


% The standard table API:
%
-export([ new/0, new/1, addEntry/3, addEntries/2,
		  removeEntry/2, lookupEntry/2, hasEntry/2, getEntry/2,
		  extractEntry/2, getValueWithDefaults/3, getValues/2, getAllValues/2,
		  addToEntry/3, subtractFromEntry/3, toggleEntry/2,
		  appendToExistingEntry/3, appendListToExistingEntry/3,
		  appendToEntry/3, appendListToEntry/3,
		  deleteFromEntry/3, popFromEntry/2,
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


-opaque list_table() :: [ { key(), value() } ].

-opaque list_table( K, V ) :: [ { K, V } ].

% Preferred naming:
-opaque table() :: list_table().
-opaque table( K, V ) :: list_table( K, V ).

-export_type([ key/0, value/0, entry/0, entries/0, entry_count/0,
			   list_table/0, list_table/2, table/0, table/2 ]).



% Implementation notes:
%
% We always rely on the first element whose key matches a specified key; so here
% a given key should never be present more than once in a given list.
%
% The proplists module could be used as well.



% Returns a new empty table dimensioned for the default number of entries.
%
-spec new() -> list_table().
new() ->
	[].


% Returns a new empty table dimensioned for the specified expected number of
% entries.
%
-spec new( entry_count() | entries() ) -> list_table().
new( ExpectedNumberOfEntries ) when is_integer( ExpectedNumberOfEntries ) ->
	[];


new( InitialEntries ) when is_list( InitialEntries ) ->

	% We do not keep the specified list as it is, as we want to check that it
	% only contains pairs and, more importantly, that there is no key
	% duplication in our (then) inner list:
	%
	addEntries( InitialEntries, [] ).



% Adds specified key/value pair into the specified table.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec addEntry( key(), value(), list_table() ) -> list_table().
addEntry( Key, Value, Table ) ->
	lists:keystore( Key, _N=1, Table, _NewTuple={ Key, Value } ).



% Adds specified list of key/value pairs into the specified table.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec addEntries( entries(), list_table() ) -> list_table().
addEntries( _EntryList=[], Table ) ->
	Table;

addEntries( [ { EntryName, EntryValue } | Rest ], Table ) ->
	addEntries( Rest, addEntry( EntryName, EntryValue, Table ) ).



% Removes specified key/value pair, as designated by the key, from the specified
% table.
%
% Does nothing if the key is not found.
%
% Returns an updated table.
%
-spec removeEntry( key(), list_table() ) -> list_table().
removeEntry( Key, Table ) ->
	lists:keydelete( Key, _N=1, Table ).



% Looks-up specified entry (designated by its key) in specified table.
%
% Returns either 'key_not_found' if no such key is registered in the table, or {
% value, Value }, with Value being the value associated to the specified key.
%
-spec lookupEntry( key(), list_table() ) ->
				 'key_not_found' | { 'value', value() }.
lookupEntry( Key, Table ) ->

	case lists:keyfind( Key, _N=1, Table ) of

		false ->
			key_not_found;

		{ Key, Value } ->
			{ value, Value }

	end.



% Tells whether the specified key exists in the table: returns true or false.
%
-spec hasEntry( key(), list_table() ) -> boolean().
hasEntry( Key, Table ) ->
	lists:keymember( Key, _N=1, Table ).



% Retrieves the value corresponding to specified (existing) key and returns it
% directly.
%
% The key/value pair is expected to exist already, otherwise an exception is
% raised.
%
-spec getEntry( key(), list_table() ) -> value().
getEntry( Key, Table ) ->

	case lists:keyfind( Key, _N=1, Table ) of

		% Most likely case first:
		{ Key, Value } ->
			Value;

		false ->
			% Badmatches are not informative enough:
			throw( { key_not_found, Key } )

	end.



% Extracts specified entry from specified table, i.e. returns the associated
% value and removes that entry from the table.
%
% The key/value pair is expected to exist already, otherwise an exception is
% raised.
%
-spec extractEntry( key(), list_table() ) -> { value(), list_table() }.
extractEntry( Key, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, Value }, ShrunkTable } ->
			{ Value, ShrunkTable };

		false ->
			% Badmatches are not informative enough:
			throw( { key_not_found, Key } )

	end.



% Looks for specified entry in specified table and, if found, returns the
% associated value; otherwise returns the specified default value.
%
-spec getValueWithDefaults( key(), value(), list_table() ) -> value().
getValueWithDefaults( Key, DefaultValue, Table ) ->

	case lists:keyfind( Key, _N=1, Table ) of

		{ Key, Value } ->
			Value;

		false ->
			DefaultValue

	end.



% Returns the (ordered) list of values that correspond to the specified
% (ordered) list of keys of this table.
%
% The key/value pairs are expected to exist already, otherwise an exception is
% raised.
%
% Ex: [ Color=red, Age=23, Mass=51 ] = list_table:getValues( [ color, age, mass
% ], [ { color, red }, { mass, 51 }, { age, 23 } ] )
%
-spec getValues( [ key() ], list_table() ) -> [ value() ].
getValues( Keys, Table ) ->

	{ RevValues, _FinalTable } = lists:foldl(

				fun( _Elem=Key, _Acc={ Values, AccTable } ) ->

					   { Value, ShrunkTable } = extractEntry( Key, AccTable ),
					   { [ Value | Values ], ShrunkTable }

				end,
				_Acc0={ [], Table },
				_List=Keys ),

	lists:reverse( RevValues ).



% Returns the (ordered) list of values that correspond to the specified
% (ordered) list of keys of this table, ensuring all entries have been read,
% otherwise throwing an exception.
%
% The key/value pairs are expected to exist already, otherwise an exception is
% raised.
%
% Ex: [ Color=red, Age=23, Mass=51 ] = list_table:getAllValues( [ color, age,
% mass ], [ { color, red }, { mass, 51 }, { age, 23 } ] )
%
-spec getAllValues( [ key() ], list_table() ) -> [ value() ].
getAllValues( Keys, Table ) ->

	case lists:foldl(
		   fun( _Elem=Key, _Acc={ Values, AccTable } ) ->

				   { Value, ShrunkTable } = extractEntry( Key, AccTable ),
				   { [ Value | Values ], ShrunkTable }

		   end,
		   _Acc0={ [], Table },
		   _List=Keys ) of

		{ RevValues, _FinalTable=[] } ->
			lists:reverse( RevValues );

		{ _RevValues, FinalTable } ->
			throw( { remaining_keys, keys( FinalTable ) } )

	end.




% Applies (maps) the specified anonymous function to each of the key-value
% entries contained in this table.
%
% Allows to apply "in-place" an operation on all entries without having to
% enumerate the content of the table and iterate on it (hence without having to
% duplicate the whole content in memory).
%
% Note: as the fun may return modified keys, the whole structure of the table
% may change (ex: different buckets used for replaced entries, colliding keys
% resulting in having less entries afterwards, etc.).
%
% One may request the returned table to be optimised after this call.
%
-spec mapOnEntries( fun( ( entry() ) -> entry() ), list_table() ) ->
						  list_table().
mapOnEntries( Fun, Table ) ->
	[ Fun( E ) || E <- Table ].




% Applies (maps) the specified anonymous function to each of the values
% contained in this table.
%
% Allows to apply "in-place" an operation on all values without having to
% enumerate the content of the table and iterate on it (hence without having to
% duplicate the whole content in memory).
%
% Note: the keys are left as are, hence the structure of the table does not
% change.
%
-spec mapOnValues( fun( ( value() ) -> value() ), list_table() ) ->
						 list_table().
mapOnValues( Fun, Table ) ->
	lists:keymap( Fun, _N=2, Table ).



% Folds specified anonymous function on all entries of the specified table.
%
% The order of transformation for entries is not specified.
%
% Returns the final accumulator.
%
-spec foldOnEntries( fun( ( entry(), basic_utils:accumulator() )
						  -> basic_utils:accumulator() ),
					 basic_utils:accumulator(), list_table() ) ->
						   basic_utils:accumulator().
foldOnEntries( Fun, InitialAcc, Table ) ->
	lists:foldl( Fun, InitialAcc, Table ).



% Adds specified number to the value, supposed to be numerical, associated to
% specified key.
%
% An exception is thrown if the key does not exist, a bad arithm is triggered if
% no addition can be performed on the associated value.
%
-spec addToEntry( key(), number(), list_table() ) -> list_table().
addToEntry( Key, Number, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, Value }, ShrunkTable } ->
			[ { Key, Value + Number } | ShrunkTable ];


		false ->
			throw( { key_not_found, Key } )

	end.



% Subtracts specified number from the value, supposed to be numerical,
% associated to specified key.
%
% An exception is thrown if the key does not exist, a bad arithm is triggered if
% no subtraction can be performed on the associated value.
%
-spec subtractFromEntry( key(), number(), list_table() ) ->
							   list_table().
subtractFromEntry( Key, Number, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, Value }, ShrunkTable } ->
			[ { Key, Value - Number } | ShrunkTable ];


		false ->
			throw( { key_not_found, Key } )

	end.



% Toggles the boolean value associated with specified key: if true will be
% false, if false will be true.
%
% An exception is thrown if the key does not exist or if its associated value is
% not a boolean.
%
-spec toggleEntry( key(), list_table() ) -> list_table().
toggleEntry( Key, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, true }, ShrunkTable } ->
			[ { Key, false } | ShrunkTable ];

		{ value, { _Key, false }, ShrunkTable } ->
			[ { Key, true } | ShrunkTable ];

		{ value, { _Key, Other }, _ShrunkTable } ->
			throw( { non_boolean_value, Other } );

		false ->
			throw( { key_not_found, Key } )

	end.



% Returns a new table, which started from TableBase and was enriched with the
% TableAdd entries whose keys where not already in TableBase (if a key is in
% both tables, the one from TableBase will be kept).
%
% Note: not the standard merge that one would expect, should values be lists.
%
-spec merge( list_table(), list_table() ) -> list_table().
merge( TableBase, TableAdd ) ->

	Base = lists:ukeysort( _N=1, TableBase ),

	Add = lists:ukeysort( _N=1, TableAdd ),

	lists:umerge( Base, Add ).



% Appends specified element to the value, supposed to be a list, associated to
% specified key.
%
% An exception is thrown if the key does not exist.
%
% Note: no check is performed to ensure the value is a list indeed, and the
% '[|]' operation will not complain if not.
%
-spec appendToExistingEntry( key(), term(), list_table() ) ->
								   list_table().
appendToExistingEntry( Key, Element, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, ListValue }, ShrunkTable } ->
			[ { Key, [ Element | ListValue ] } | ShrunkTable ];

		false ->
			throw( { key_not_found, Key } )

	end.



% Appends specified elements to the value, supposed to be a list, associated to
% specified key.
%
% An exception is thrown if the key does not exist.
%
-spec appendListToExistingEntry( key(), [ term() ], list_table() ) ->
								   list_table().
appendListToExistingEntry( Key, Elements, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, ListValue }, ShrunkTable } ->
			[ { Key, Elements ++ ListValue } | ShrunkTable ];

		false ->
			throw( { key_not_found, Key } )

	end.



% Appends specified element to the value, supposed to be a list, associated to
% specified key.
%
% If that key does not already exist, it will be created and associated to a
% list containing only the specified element.
%
% Note: no check is performed to ensure the value is a list indeed, and the
% '[|]' operation will not complain if not.
%
-spec appendToEntry( key(), term(), list_table() ) -> list_table().
appendToEntry( Key, Element, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, ListValue }, ShrunkTable } ->
			[ { Key, [ Element | ListValue ] } | ShrunkTable ];

		false ->
			[ { Key, [ Element ] } | Table ]

	end.



% Appends specified elements to the value, supposed to be a list, associated to
% specified key.
%
% If that key does not already exist, it will be created and associated to a
% list containing only the specified elements.
%
-spec appendListToEntry( key(), [ term() ], list_table() ) ->
							   list_table().
appendListToEntry( Key, Elements, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, ListValue }, ShrunkTable } ->
			[ { Key, Elements ++ ListValue } | ShrunkTable ];

		false ->
			[ { Key, Elements } | Table ]

	end.



% Deletes the first match of the specified element in the value associated to
% specified key, this value being assumed to be a list.
%
% An exception is thrown if the key does not exist.
%
% If the element is not in the specified list, the list will not be modified.
%
-spec deleteFromEntry( key(), term(), list_table() ) -> list_table().
deleteFromEntry( Key, Element, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, ListValue }, ShrunkTable } ->
			[ { Key, lists:delete( Element, ListValue ) } | ShrunkTable ];

		false ->
			throw( { key_not_found, Key } )

	end.



% Pops the head of the value (supposed to be a list) associated to specified
% key, and returns a pair made of the popped head and of the new table.
%
-spec popFromEntry( key(), list_table() ) -> { term(), list_table() }.
popFromEntry( Key, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, [ H | T ] }, ShrunkTable } ->
			NewTable = [ { Key, T } | ShrunkTable ],
			{ H, NewTable };

		false ->
			throw( { key_not_found, Key } )

	end.



% Returns a flat list whose elements are all the key/value pairs of the table,
% in no particular order.
%
% Ex: [ {K1,V1}, {K2,V2}, ... ].
%
-spec enumerate( list_table() ) -> entries().
enumerate( Table ) ->
	Table.



% Returns a list of key/value pairs corresponding to the list of specified keys,
% or throws a badmatch is at least one key is not found.
%
-spec selectEntries( [ key() ], list_table() ) -> entries().
selectEntries( Keys, Table ) ->
	selectEntries( Keys, Table, _Acc=[] ).

selectEntries( _Keys=[], _Table, Acc ) ->
	Acc;

selectEntries( _Keys=[ K | T ], Table, Acc ) ->

	case lists:keyfind( K, _N=1, Table ) of

		false ->
			% Badmatches are not informative enough:
			throw( { key_not_found, K } );

		%{ K, V } ->
		Entry ->
			selectEntries( T, Table, [ Entry | Acc ] )

	end.



% Returns a list containing all the keys of this table.
%
-spec keys( list_table() ) -> [ key() ].
keys( Table ) ->
	[ K || { K, _V } <- Table ].



% Returns a list containing all the values of this table.
%
% Ex: useful if the key was used as an index to generate this table first.
%
-spec values( list_table() ) -> [ value() ].
values( Table ) ->
	[ V || { _K, V } <- Table ].



% Returns whether the specified table is empty (not storing any key/value pair).
%
-spec isEmpty( list_table() ) -> boolean().
isEmpty( _Table=[] ) ->
	true;

isEmpty( _Table ) ->
	false.



% Returns the size (number of entries) of this table.
%
-spec size( list_table() ) -> entry_count().
size( Table ) ->
	length( Table ).



% Returns the number of entries (key/value pairs) stored in the specified table.
%
-spec getEntryCount( list_table() ) -> entry_count().
getEntryCount( Table ) ->
	length( Table ).



% Optimises this table.
%
% Nothing to be done with this implementation.
%
-spec optimise( list_table() ) -> list_table().
optimise( Table ) ->
	Table.



% Returns a textual description of the specified table.
%
-spec toString( list_table() ) -> string().
toString( Table ) ->
	toString( Table, user_friendly ).



% Returned string is either quite raw (if using 'internal') or a bit more
% elaborate (if using 'user_friendly').
%
-spec toString( list_table(), 'internal' | 'user_friendly' ) -> string().
toString( Table, _Displaytype ) ->

	case enumerate( Table ) of

		[] ->
			"Empty table";

		L ->

			% Enforces a consistent order:
			Strings = [ io_lib:format( "~p: ~p", [ K, V ] )
						|| { K, V } <- lists:sort( L ) ],

			% Flatten is needed, in order to use the result with ~s:
			lists:flatten( io_lib:format( "Table with ~B entry(ies):~s~n",
				[ length( L ),
				  text_utils:strings_to_string( Strings ) ] ) )

	end.



% Displays the specified table on the standard output.
%
-spec display( list_table() ) -> basic_utils:void().
display( Table ) ->
	io:format( "~s~n", [ toString( Table ) ] ).



% Displays the specified table on the standard output, with the specified title
% on top.
%
-spec display( string(), list_table() ) -> basic_utils:void().
display( Title, Table ) ->
	io:format( "~s:~n~s~n", [ Title, toString( Table ) ] ).
