% Copyright (C) 2014-2025 Olivier Boudeville
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
% Creation date: Monday, December 22, 2014.

-module(list_table).

-moduledoc """
Implementation of an **associative table relying on a simple list of key/value
pairs**.

Beware of multiple entries having the same key in such a list_table, as the
first one will eclipse all others; this may happen if not building explicitly,
with this API, one's list_table instance.

See `list_table_test.erl` for the corresponding test.

We provide different multiple types of tables, including:
- 'hashtable', the most basic, safest, reference implementation - and quite
efficient as well
- 'tracked_table', an attempt of optimisation of it (not necessarily the best)
- 'lazy_table', deciding to optimise in a less costly way than 'tracked_table'
- 'map_table', which is probably the most efficient implementation (speed/size
compromise)
- 'list_table' (this module), a list-based implementation, efficient for smaller
table (and only them)

They are to provide the same API (signatures and contracts).

See also: `list_utils.erl` if having to deal with tagged lists, that is: lists
possibly containing pairs and also single atoms (e.g. see
`list_utils:extract_{atom,pair}_*/*`.
""".



% The standard table API:
-export([ new/0, new/1, check_proper/1,
		  add_entry/3, add_entries/2, add_new_entry/3, add_new_entries/2,
		  remove_entry/2, remove_entries/2,
		  lookup_entry/2, has_entry/2,
		  extract_entry/2, extract_entry_with_default/3,
		  extract_entry_if_existing/2,
		  extract_entries/2, extract_entries_if_existing/2,
		  extract_entries_with_defaults/2,
		  get_value/2, get_value_with_default/3,
		  get_values/2, get_all_values/2,
		  add_to_entry/3, subtract_from_entry/3, toggle_entry/2,
		  append_to_existing_entry/3, append_list_to_existing_entry/3,
		  append_to_entry/3, append_list_to_entry/3,
		  delete_from_entry/3, pop_from_entry/2,
		  enumerate/1, select_entries/2, keys/1, values/1,
		  is_empty/1, size/1,
		  map_on_entries/2, map_on_values/2,
		  fold_on_entries/3,
		  merge/2, merge_in_key/3, merge_in_keys/2,
		  optimise/1, to_string/1, to_string/2, display/1, display/2 ]).



-type key() :: hashtable:key().

-type value() :: hashtable:value().

-type default_value() :: hashtable:default_value().

-type entry() :: hashtable:entry().


-type entries() :: [ entry() ].


-type entry_count() :: basic_utils:count().


% Cannot be kept opaque:
-doc """
A list-based associative table.

Not exactly as `proplists:proplist/0` (here: a list only of pairs - not atoms or
other tuples).
""".
-type list_table() :: [ { key(), value() } ].


% Cannot be kept opaque:
-doc """
A list-based associative table.

Not exactly as proplists:proplist/0 (pairs only, and any() as key).
""".
-type list_table( K, V ) :: [ { K, V } ].


% Possible alternate naming:
-doc "Alternative naming here for list-based associative tables.".
-type table() :: list_table().


-doc "Alternative naming here for list-based associative tables.".
-type table( K, V ) :: list_table( K, V ).


-export_type([ key/0, value/0, entry/0, entries/0, entry_count/0,
			   list_table/0, list_table/2, table/0, table/2 ]).


% Type shorthands:

-type accumulator() :: basic_utils:accumulator().

-type ustring() :: text_utils:ustring().



% Implementation notes:
%
% We always rely on the first element whose key matches a specified key; so here
% a given key should never be present more than once in a given list.
%
% The proplists module could be used as well.


-compile( { inline, [ has_entry/2, add_entry/3, extract_entry/2 ] } ).


-define( default_bullet, " + " ).



-doc """
Returns an empty table dimensioned for the default number of entries.
""".
-spec new() -> list_table().
new() ->
	[].



-doc """
Returns an empty table dimensioned for the specified expected number of entries
/ a table containing the specified (initial) entries, whose keys are checked for
uniqueness.
""".
-spec new( entry_count() | entries() ) -> list_table().
new( ExpectedNumberOfEntries ) when is_integer( ExpectedNumberOfEntries ) ->
	% ExpectedNumberOfEntries not relevant for this implementation:
	[];

new( InitialEntries ) when is_list( InitialEntries ) ->

	% We do not keep the specified list as it is, as we want to check that it
	% only contains pairs and, more importantly, that there is no key
	% duplication in our (then) inner list:
	%
	add_new_entries( InitialEntries, _InitTable=[] ).



-doc """
Checks that the specified term corresponds to a so-called "proper" table, i.e. a
list_table with no duplicate key, i.e. whether:

- its structure is legit, i.e. that it is a list of pairs, whose first value is
  a atom; if not, throws an exception

- all its keys are different, as no duplicates are accepted when defining a
proper table; if not, returns a table (i.e. a map_hashtable) whose keys are the
duplicated keys in the input table, and whose values are their associated
duplicate values

Return `ok` if the table is correct.

For example `list_table:check_proper([{a, 11}, {b,7}, {a,10}, {c,2}])`
shall return a table with a single entry: `{a, [10,11]}`.

Typically useful to vet (user-specified) configuration settings; allows better /
more flexible feedback for the caller than `new/1`.
""".
-spec check_proper( list_table() ) -> 'ok' | table().
check_proper( Table ) ->
    % So using a map_hashtable here for duplicates:
    check_proper( Table, _DupTable=table:new() ).


% (helper)
check_proper( _CheckedTable=[], DupTable ) ->

    CleanedDupTable = table:fold(
        % Drop entry if there is no duplicate:
        fun( _K, [ _SingleV ], RetMap ) ->
             RetMap;
           ( K, Dups, RetMap ) ->
             table:add_entry( K, Dups, RetMap )
        end,
		_RetMap0=table:new(),
		_FoldedTable=DupTable ),

    case table:is_empty( CleanedDupTable ) of

        true ->
            ok;

        false ->
            CleanedDupTable

     end;

check_proper( _CheckedTable=[ { K, V } | T ], DupTable ) when is_atom( K ) ->
    NewDupTable = table:append_to_entry( K, V, DupTable ),
    check_proper( T, NewDupTable );

check_proper( _CheckedTable=[ { K, _V } | _T ], _DupTable ) ->
    throw( { non_atom_key, K } );

check_proper( _CheckedTable=[ InvE | _T ], _DupTable ) ->
    throw( { non_pair, InvE } );

check_proper( NonList, _DupTable ) ->
    throw( { non_list, NonList } ).



-doc """
Adds the specified key/value pair in the specified table.

If there is already a pair with this key, then its previous value will be
replaced by the specified one.
""".
-spec add_entry( key(), value(), list_table() ) -> list_table().
add_entry( Key, Value, Table ) ->
	lists:keystore( Key, _N=1, Table, _NewTuple={ Key, Value } ).



-doc """
Adds the specified list of key/value pairs in the specified table.

If there is already a pair with this key, then its previous value will be
replaced by the specified one.
""".
-spec add_entries( entries(), list_table() ) -> list_table().
add_entries( _EntryList=[], Table ) ->
	Table;

add_entries( [ { EntryName, EntryValue } | Rest ], Table ) ->
	add_entries( Rest, add_entry( EntryName, EntryValue, Table ) );

add_entries( [ Other | _Rest ], _Table ) ->
	throw( { invalid_entry, Other } ).



-doc """
Adds the specified key/value pair in the specified table, expecting this key not
to be already defined in this table.
""".
-spec add_new_entry( key(), value(), list_table() ) -> list_table().
add_new_entry( Key, Value, Table ) ->

	% A tad expensive, could be replaced by an inlined add_entry/3 in non-debug
	% mode:
	%
	case has_entry( Key, Table ) of

		false ->
			add_entry( Key, Value, Table );

		true ->
			throw( { key_already_existing, Key } )

	end.



-doc """
Adds the specified list of key/value pairs in the specified table, expecting
that none of these keys is already defined in this table (otherwise an exception
is thrown).
""".
-spec add_new_entries( hashtable:entries(), list_table() ) -> list_table().
add_new_entries( EntryList, Table ) ->

	lists:foldl( fun( { K, V }, Map ) ->
					add_new_entry( K, V, Map )
				 end,
				 _Acc0=Table,
				 _List=EntryList ).



-doc """
Removes the entry designated by the specified key, from the specified table.

Does nothing if the key is not found.

Returns an updated table.
""".
-spec remove_entry( key(), list_table() ) -> list_table().
remove_entry( Key, Table ) ->
	%trace_utils:debug_fmt( "Removing any entry of key '~p'.", [ Key ] ),
	lists:keydelete( Key, _N=1, Table ).



-doc """
Removes the key/value pairs designated by the specified keys, from the specified
table.

Does nothing if a key is not found.

Returns an updated table.
""".
-spec remove_entries( [ key() ], list_table() ) -> list_table().
remove_entries( Keys, Table ) ->
	lists:foldl( fun( K, AccTable ) ->
					lists:keydelete( K, _N=1, AccTable )
				 end,
				 _Acc0=Table,
				 Keys ).



-doc """
Looks-up the specified entry (designated by its key) in the specified table.

Returns either 'key_not_found' if no such key is registered in the table, or
{value, Value}, with Value being the value associated to the specified key.
""".
-spec lookup_entry( key(), list_table() ) ->
								'key_not_found' | { 'value', value() }.
lookup_entry( Key, Table ) ->

	case lists:keyfind( Key, _N=1, Table ) of

		false ->
			key_not_found;

		{ Key, Value } ->
			{ value, Value }

	end.



-doc """
Tells whether the specified key exists in the specified table: returns true or
false.
""".
-spec has_entry( key(), list_table() ) -> boolean().
has_entry( Key, Table ) ->
	lists:keymember( Key, _N=1, Table ).



-doc """
Retrieves the value corresponding to specified (existing) key and returns it
directly.

The key/value pair is expected to exist already, otherwise an exception is
thrown.
""".
-spec get_value( key(), list_table() ) -> value().
get_value( Key, Table ) ->

	case lists:keyfind( Key, _N=1, Table ) of

		% Most likely case first:
		{ Key, Value } ->
			Value;

		false ->
			% Badmatches are not informative enough:
			trace_utils:error_fmt( "No key '~p' found in the following table "
				"(process: ~w): ~ts", [ Key, self(), to_string( Table ) ] ),
			throw( { key_not_found, Key } )

	end.



-doc """
Extracts the specified entry from the specified table, that is returns the
associated value and removes that entry from the table.

The key/value pair is expected to exist already in the specified table,
otherwise an exception is thrown.
""".
-spec extract_entry( key(), list_table() ) -> { value(), list_table() }.
extract_entry( Key, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, Value }, ShrunkTable } ->
			{ Value, ShrunkTable };

		false ->
			% Badmatches are not informative enough:
			throw( { key_not_found, Key } )

	end.



-doc """
Extracts the specified entry from the specified table, that is returns the
associated value and removes that entry from the table.

If no such key is available, returns the specified default value and the
original table.
""".
-spec extract_entry_with_default( key(), value(), list_table() ) ->
										{ value(), list_table() }.
extract_entry_with_default( Key, DefaultValue, Table ) ->

	case has_entry( Key, Table ) of

		true ->
			extract_entry( Key, Table );

		false ->
			{ DefaultValue, Table }

	end.



-doc """
Extracts the specified entry (if any) from the specified table, that is returns
its associated value and removes that entry from the returned table.

Otherwise, that is if that entry does not exist, returns false.
""".
-spec extract_entry_if_existing( key(), list_table() ) ->
					'false' | { value(), list_table() }.
extract_entry_if_existing( Key, Table ) ->

	case has_entry( Key, Table ) of

		true ->
			extract_entry( Key, Table );

		false ->
			false

	end.



-doc """
Extracts the specified entries from the specified table, that is returns their
associated values (in-order) and removes these entries from the returned table.

Each key/value pair is expected to exist already, otherwise an exception is
raised (typically {badkey, KeyNotFound}).

For example: {[RedValue, GreenValue, BlueValue], ShrunkTable} =
   list_table:extract_entries([red, green, blue], MyTable)
""".
-spec extract_entries( [ key() ], list_table() ) ->
										{ [ value() ], list_table() }.
extract_entries( Keys, ListTable ) ->

	{ RevValues, FinalTable } = lists:foldl(
		fun( K, { AccValues, AccTable } ) ->
			{ V, ShrunkTable } = extract_entry( K, AccTable ),
			{ [ V | AccValues ], ShrunkTable }
		end,
		_Acc0={ [], ListTable },
		_List=Keys ),

	{ lists:reverse( RevValues ), FinalTable }.



-doc """
Extracts the specified entries (if any) from the specified table, that is
returns them (in-order), and removes them from the returned table.

If a key is not present in the table, it is skipped.

For example, if no 'green' key exists in MyTable:
```
{[{red,RedValue}, {blue,BlueValue}], ShrunkTable} =
   list_table:extract_entries_if_existing([red, green, blue], MyTable)
```
""".
-spec extract_entries_if_existing( [ key() ], list_table() ) ->
										{ entries(), list_table() }.
extract_entries_if_existing( Keys, ListTable ) ->

	{ RevEntries, FinalTable } = lists:foldl(

		fun( _Elem=Key, Acc={ Values, AccTable } ) ->

			case extract_entry_if_existing( Key, AccTable ) of

				false ->
					Acc;

				{ V, ShrunkTable } ->
					{ [ _E={ Key, V } | Values ], ShrunkTable }

			end

		end,

		_Acc0={ [], ListTable },
		_List=Keys ),

	{ lists:reverse( RevEntries ), FinalTable }.



-doc """
Extracts the specified entries from the specified table, that is returns their
associated values (in-order) if found, otherwise their specified default value,
and removes these entries from the returned table.

For example:
```
{[RedMaybeDefValue, GreenMaybeDefValue, BlueMaybeDefValue],
ShrunkTable} = list_table:extract_entries([{red,RedDefault},
{green,GreenDefault}, {blue,BlueDefault}], MyTable)
```
""".
-spec extract_entries_with_defaults( [ { key(), default_value()} ],
				list_table() ) -> { [ value() ], list_table() }.
extract_entries_with_defaults( KeyDefPairs, ListTable ) ->

	{ RevValues, FinalTable } = lists:foldl(
		fun( { K, DefK }, { AccValues, AccTable } ) ->
			{ V, ShrunkTable } =
				extract_entry_with_default( K, DefK, AccTable ),
			{ [ V | AccValues ], ShrunkTable }
		end,
		_Acc0={ [], ListTable },
		_List=KeyDefPairs ),

	{ lists:reverse( RevValues ), FinalTable }.



-doc """
Looks for the specified entry in the specified table and, if found, returns its
associated value; otherwise returns the specified default value.
""".
-spec get_value_with_default( key(), value(), list_table() ) -> value().
get_value_with_default( Key, DefaultValue, Table ) ->

	%trace_utils:debug_fmt( "Getting value of key '~p' (default: '~p') "
	%   "for table:~n ~p", [ Key, DefaultValue, Table ] ),

	case lists:keyfind( Key, _N=1, Table ) of

		{ Key, Value } ->
			Value;

		false ->
			DefaultValue

	end.



-doc """
Returns the (ordered) list of values that correspond to the specified (ordered)
list of keys of this table.

The key/value pairs are expected to exist already, otherwise an exception is
thrown.

For example:
```
[Color=red, Age=23, Mass=51] = list_table:get_values(
	[color, age, mass], [{color, red}, {mass,51}, {age, 23}])
```
""".
-spec get_values( [ key() ], list_table() ) -> [ value() ].
get_values( Keys, Table ) ->

	{ RevValues, _FinalTable } = lists:foldl(

		fun( _Elem=Key, _Acc={ Values, AccTable } ) ->

			{ Value, ShrunkTable } = extract_entry( Key, AccTable ),
			{ [ Value | Values ], ShrunkTable }

		end,
		_Acc0={ [], Table },
		_List=Keys ),

	lists:reverse( RevValues ).



-doc """
Returns the (ordered) list of values that correspond to the specified (ordered)
list of keys of this table, ensuring all entries have been read, otherwise
throwing an exception.

The key/value pairs are expected to exist already, otherwise an exception is
thrown.

For example:
```
[Color=red, Age=23, Mass=51] = list_table:get_all_values(
	[color, age, mass], [{color, red}, {mass, 51}, {age, 23}])
```
""".
-spec get_all_values( [ key() ], list_table() ) -> [ value() ].
get_all_values( Keys, Table ) ->

	case lists:foldl(
			fun( _Elem=Key, _Acc={ Values, AccTable } ) ->

				{ Value, ShrunkTable } = extract_entry( Key, AccTable ),
				{ [ Value | Values ], ShrunkTable }

			end,
			_Acc0={ [], Table },
			_List=Keys ) of

		{ RevValues, _FinalTable=[] } ->
			lists:reverse( RevValues );

		{ _RevValues, FinalTable } ->
			throw( { remaining_keys, keys( FinalTable ) } )

	end.



-doc """
Applies (maps) the specified anonymous function to each of the key-value entries
contained in this table.

Allows to apply "in-place" an operation on all entries without having to
enumerate the content of the table and iterate on it (hence without having to
duplicate the whole content in memory).

Note: as the fun may return modified keys, the whole structure of the table may
change (e.g. different buckets used for replaced entries, colliding keys
resulting in having less entries afterwards, etc.).

One may request the returned table to be optimised after this call.
""".
-spec map_on_entries( fun( ( entry() ) -> entry() ), list_table() ) ->
										list_table().
map_on_entries( Fun, Table ) ->
	[ Fun( E ) || E <- Table ].



-doc """
Applies (maps) the specified anonymous function to each of the values contained
in this table.

Allows to apply "in-place" an operation on all values without having to
enumerate the content of the table and iterate on it (hence without having to
duplicate the whole content in memory).

Note: the keys are left as are, hence the structure of the table does not
change.
""".
-spec map_on_values( fun( ( value() ) -> value() ), list_table() ) ->
												list_table().
map_on_values( Fun, Table ) ->
	lists:keymap( Fun, _N=2, Table ).



-doc """
Folds the specified anonymous function on all entries of the specified table.

The order of transformation for entries is not specified.

Returns the final accumulator.
""".
-spec fold_on_entries( fun( ( entry(), accumulator() ) -> accumulator() ),
					   accumulator(), list_table() ) -> accumulator().
fold_on_entries( Fun, InitialAcc, Table ) ->
	lists:foldl( Fun, InitialAcc, Table ).



-doc """
Adds the specified number to the value, supposed to be numerical, associated to
specified key.

An exception is thrown if the key does not exist, a bad arithm is triggered if
no addition can be performed on the associated value.
""".
-spec add_to_entry( key(), number(), list_table() ) -> list_table().
add_to_entry( Key, Number, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, Value }, ShrunkTable } ->
			[ { Key, Value + Number } | ShrunkTable ];

		false ->
			throw( { key_not_found, Key } )

	end.



-doc """
Subtracts the specified number from the value, supposed to be numerical,
associated to the specified key.

An exception is thrown if the key does not exist, a bad arithm is triggered if
no subtraction can be performed on the associated value.
""".
-spec subtract_from_entry( key(), number(), list_table() ) -> list_table().
subtract_from_entry( Key, Number, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, Value }, ShrunkTable } ->
			[ { Key, Value - Number } | ShrunkTable ];

		false ->
			throw( { key_not_found, Key } )

	end.



-doc """
Toggles the boolean value associated with specified key: if true will be false,
if false will be true.

An exception is thrown if the key does not exist or if its associated value is
not a boolean.
""".
-spec toggle_entry( key(), list_table() ) -> list_table().
toggle_entry( Key, Table ) ->

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



-doc """
Returns a table that started from TableBase and was enriched with the TableAdd
entries whose keys were not already in TableBase (if a key is in both tables,
the one from TableBase will be kept).

Note: not the standard merge that one would expect, should values be lists.
""".
-spec merge( list_table(), list_table() ) -> list_table().
merge( TableBase, TableAdd ) ->

	Index = 1,

	Base = lists:ukeysort( Index, TableBase ),

	Add = lists:ukeysort( Index, TableAdd ),

	lists:umerge( Base, Add ).



-doc """
Gathers, from a table whose values are expected to be lists, all the values
associated to the keys listed in AlternateKeys and associates them to
ReferenceKey instead (in addition to any value that would already be associated
to it).

Useful for example to gather in a single entry the values associated to aliases
in terms of command-line options, like the values associated to a '--length'
command-line option (hence associated to the '-length' key) and also to the '-l'
and '--len' alias command-line options (hence associated to the 'l' and '-len'
keys).

For example MergedTable = merge_in_key('-length', ['l', '-len'], MyTable).
""".
-spec merge_in_key( key(), [ key() ], list_table() ) -> list_table().
merge_in_key( _ReferenceKey, _AlternateKeys=[], Table ) ->
	Table;

merge_in_key( ReferenceKey, _AlternateKeys=[ K | T ], Table ) ->
	case has_entry( K, Table ) of

		true ->
			{ ValueList, ShrunkTable } = extract_entry( K, Table ),
			NewTable =
				append_list_to_entry( ReferenceKey, ValueList, ShrunkTable ),
			merge_in_key( ReferenceKey, T, NewTable );

		false ->
			merge_in_key( ReferenceKey, T, Table )

	end.



-doc """
Performs a key merge, as merge_in_key/3, however not for a single reference key
/ aliases entries, but for a set thereof.

For example:
```
 MergedTable = merge_in_keys([{'-length', [ 'l', '-len' ]},
								  {'-help', [ 'h' ]} ], MyTable).
```
""".
-spec merge_in_keys( list_table(), list_table() ) -> list_table().
merge_in_keys( _KeyAssoc=[], Table ) ->
	Table;

merge_in_keys( _KeyAssoc=[ { K, AltKeys } | T ], Table ) ->
	MergedTable = merge_in_key( K, AltKeys, Table ),
	merge_in_keys( T, MergedTable ).



-doc """
Appends the specified element to the value, supposed to be a list, associated to
the specified key.

An exception is thrown if the key does not exist.

Note: no check is performed to ensure that the value is a list indeed, and the
cons (`[|]`) operation will not complain if not.
""".
-spec append_to_existing_entry( key(), term(), list_table() ) -> list_table().
append_to_existing_entry( Key, Element, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, ListValue }, ShrunkTable } ->
			[ { Key, [ Element | ListValue ] } | ShrunkTable ];

		false ->
			throw( { key_not_found, Key } )

	end.



-doc """
Appends the specified elements to the value, supposed to be a list, associated
to the specified key.

An exception is thrown if the key does not exist.
""".
-spec append_list_to_existing_entry( key(), [ term() ], list_table() ) ->
											list_table().
append_list_to_existing_entry( Key, Elements, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, ListValue }, ShrunkTable } ->
			[ { Key, Elements ++ ListValue } | ShrunkTable ];

		false ->
			throw( { key_not_found, Key } )

	end.



-doc """
Appends the specified element to the value, supposed to be a list, associated to
the specified key.

If that key does not already exist, it will be created and associated to a list
containing only the specified element.

Note: no check is performed to ensure the value is a list indeed, and the cons
(`[|]`) operation will not complain if not.
""".
-spec append_to_entry( key(), term(), list_table() ) -> list_table().
append_to_entry( Key, Element, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, ListValue }, ShrunkTable } ->
			[ { Key, [ Element | ListValue ] } | ShrunkTable ];

		false ->
			[ { Key, [ Element ] } | Table ]

	end.



-doc """
Appends the specified elements to the value, supposed to be a list, associated
to the specified key.

If that key does not already exist, it will be created and associated to a list
containing only the specified elements.
""".
-spec append_list_to_entry( key(), [ term() ], list_table() ) -> list_table().
append_list_to_entry( Key, Elements, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, ListValue }, ShrunkTable } ->
			[ { Key, Elements ++ ListValue } | ShrunkTable ];

		false ->
			[ { Key, Elements } | Table ]

	end.



-doc """
Deletes the first match of the specified element in the value associated to
specified key, this value being assumed to be a list.

An exception is thrown if the key does not exist.

If the element is not in the specified list, the list will not be modified.
""".
-spec delete_from_entry( key(), term(), list_table() ) -> list_table().
delete_from_entry( Key, Element, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, ListValue }, ShrunkTable } ->
			[ { Key, lists:delete( Element, ListValue ) } | ShrunkTable ];

		false ->
			throw( { key_not_found, Key } )

	end.



-doc """
Pops the head of the value (supposed to be a list) associated to the specified
key, and returns a pair made of the popped head and of the new table.
""".
-spec pop_from_entry( key(), list_table() ) -> { term(), list_table() }.
pop_from_entry( Key, Table ) ->

	case lists:keytake( Key, _N=1, Table ) of

		{ value, { _Key, [ H | T ] }, ShrunkTable } ->
			NewTable = [ { Key, T } | ShrunkTable ],
			{ H, NewTable };

		false ->
			throw( { key_not_found, Key } )

	end.



-doc """
Returns a flat list whose elements are all the key/value pairs of the table, in
no particular order.

For example [{K1,V1}, {K2,V2}, ...].
""".
-spec enumerate( list_table() ) -> entries().
enumerate( Table ) ->
	Table.



-doc """
Returns a list of key/value pairs corresponding to the list of specified keys,
or throws a badmatch is at least one key is not found.
""".
-spec select_entries( [ key() ], list_table() ) -> entries().
select_entries( Keys, Table ) ->
	select_entries( Keys, Table, _Acc=[] ).

select_entries( _Keys=[], _Table, Acc ) ->
	Acc;

select_entries( _Keys=[ K | T ], Table, Acc ) ->

	case lists:keyfind( K, _N=1, Table ) of

		false ->
			% Badmatches are not informative enough:
			throw( { key_not_found, K } );

		%{ K, V } ->
		Entry ->
			select_entries( T, Table, [ Entry | Acc ] )

	end.



-doc """
Returns a list containing all the keys of this table (with no duplicate).
""".
-spec keys( list_table() ) -> [ key() ].
keys( Table ) ->
	list_utils:uniquify( [ K || { K, _V } <- Table ] ).



-doc """
Returns a list containing all the values of this table.

For example useful if the key was used as an index to generate this table first.
""".
-spec values( list_table() ) -> [ value() ].
values( Table ) ->
	[ V || { _K, V } <- Table ].



-doc """
Returns whether the specified table is empty (not storing any key/value pair).
""".
-spec is_empty( list_table() ) -> boolean().
is_empty( _Table=[] ) ->
	true;

is_empty( _Table ) ->
	false.



-doc """
Returns the size (number of entries, that is of key/value pairs) of the
specified table.
""".
-spec size( list_table() ) -> entry_count().
size( Table ) ->
	length( Table ).


-doc """
Optimises the specified table.

Nothing to be done with this implementation.
""".
-spec optimise( list_table() ) -> list_table().
optimise( Table ) ->
	Table.



-doc "Returns a textual description of the specified table.".
-spec to_string( list_table() ) -> ustring().
to_string( Table ) ->
	to_string( Table, user_friendly ).



-doc """
Returns a textual description of the specified table.

Either a bullet is specified, or the returned string is ellipsed if needed (if
using 'user_friendly'), or quite raw and non-ellipsed (if using 'full'), or even
completly raw ('internal').
""".
-spec to_string( list_table(), hashtable:description_type() ) -> ustring().
to_string( Table, DescriptionType ) ->

	case enumerate( Table ) of

		[] ->
			"empty table";

		[ { K, V } ] ->
			case DescriptionType of

				user_friendly ->
					text_utils:format_ellipsed( "table with a single entry, "
						"key being ~p, value being ~p", [ K, V ] );

				_ ->
					text_utils:format( "table with a single entry, "
						"key being ~p, value being ~p", [ K, V ] )

			end;


		L ->

			% Enforces a consistent order; flatten below is needed, in order to
			% use the result with ~ts:
			%
			case DescriptionType of

				user_friendly ->
					Strs = [ text_utils:format_ellipsed( "~p: ~p", [ K, V ] )
								|| { K, V } <- lists:sort( L ) ],

					lists:flatten( io_lib:format( "table with ~B entries: ~ts",
						[ length( L ), text_utils:strings_to_string( Strs,
													?default_bullet ) ] ) );

				DescType when DescType =:= full orelse DescType =:= internal ->
					Strs = [ text_utils:format( "~p: ~p", [ K, V ] )
								|| { K, V } <- lists:sort( L ) ],

					lists:flatten( io_lib:format( "table with ~B entries: ~ts",
						[ length( L ),
						  text_utils:strings_to_string( Strs,
														?default_bullet ) ] ) );

				% Here, ellipsed and with specified bullet:
				Bullet ->
					Strs = [ text_utils:format_ellipsed( "~p: ~p", [ K, V ] )
								|| { K, V } <- lists:sort( L ) ],

					lists:flatten( io_lib:format( "table with ~B entries: ~ts",
						[ length( L ),
						  text_utils:strings_to_string( Strs, Bullet ) ] ) )

			end

	end.



-doc "Displays the specified table on the standard output.".
-spec display( list_table() ) -> void().
display( Table ) ->
	io:format( "~ts~n", [ to_string( Table ) ] ).



-doc """
Displays the specified table on the standard output, with the specified title on
top.
""".
-spec display( ustring(), list_table() ) -> void().
display( Title, Table ) ->
	io:format( "~ts:~n~ts~n", [ Title, to_string( Table ) ] ).
