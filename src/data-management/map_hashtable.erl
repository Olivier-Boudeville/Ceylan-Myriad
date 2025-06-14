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
% Creation date: Tuesday, December 2, 2014.

-module(map_hashtable).

-moduledoc """
Implementation of an **associative table based on the `map` module**.

Supposedly the most efficient native available implementation of an associative
table.

See `map_table_test.erl' for the corresponding test.
See `hashtable.erl' for the parent, base implementation.

We provide different multiple types of hashtables, including:
- 'hashtable', the most basic, safest, reference implementation - and quite
efficient as well
- 'tracked_hashtable', an attempt of optimisation of it (not necessarily the
best)
- 'lazy_hashtable', deciding to optimise in a less costly way than
'tracked_hashtable'
- 'map_hashtable' (this module), which is the newest, probably most efficient in
most cases implementation (speed/size compromise)
- 'list_table', a list-based implementation, efficient for smaller tables (and
only them)

All these types of tables are to provide the same API (signatures and
contracts), yet one should note that this module is the one that tends to
supersede all others, and that over time features have been added that may not
have been back-ported to the other table types. The second most useful is
list_table, to read user-specified settings.
""".



% Mostly the same API as the one of hashtable (but richer):
-export([ new/0, singleton/2, new/1, new_from_unique_entries/1,
		  add_entry/3, add_entries/2, add_new_entry/3, add_new_entries/2,
		  add_option_entry/3, add_option_entries/2,
		  update_entry/3, update_entries/2, update_existing_entries/2,
		  swap_value/3,
		  remove_entry/2, remove_existing_entry/2,
		  remove_entries/2, remove_existing_entries/2,
		  lookup_entry/2, has_entry/2,
		  extract_entry/2, extract_entry_with_default/3,
		  extract_entry_if_existing/2,
		  extract_entries/2, extract_entries_if_existing/2,
		  get_value/2, get_values/2, get_value_with_default/3,
		  get_all_values/2,
		  add_to_entry/3, subtract_from_entry/3, toggle_entry/2,
		  append_to_existing_entry/3, append_list_to_existing_entry/3,
		  append_to_entry/3, append_list_to_entry/3,
		  concat_to_entry/3, concat_list_to_entries/2,
		  delete_from_entry/3, delete_existing_from_entry/3,
		  pop_from_entry/2,
		  enumerate/1, select_entries/2, keys/1, values/1,
		  is_empty/1, size/1,
		  map/2, map_on_entries/2, map_on_values/2,
		  fold/3, fold_on_entries/3,
		  merge/1, merge/2, merge_unique/2, merge_unique/1,
		  optimise/1, to_string/1, to_string/2, display/1, display/2 ]).



-type key() :: hashtable:key().

-type value() :: hashtable:value().

-type entry() :: hashtable:entry().

-type option_entry() :: hashtable:option_entry().

-type option_entries() :: hashtable:option_entries().


-type entries() :: hashtable:entries().

-type entry_count() :: basic_utils:count().


-doc "A map-based associative table.".
-type map_hashtable() :: map().


% Opaqueness difficult to preserve:
-doc "A map-based associative table.".
-type map_hashtable( K, V ) :: type_utils:map( K, V ).


-export_type([ key/0, value/0, entry/0, entries/0,
			   entry_count/0, option_entry/0, option_entries/0,
			   map_hashtable/0, map_hashtable/2 ]).


% Type shorthands:

% As this module is not parse-transformed:
%
% (if a 'type option(_) is unused' error is reported for this type, this is the
% sign that this module is recompiled with the Myriad parse transform, whereas
% it should not; its compilation should be triggered from the root of Myriad,
% rather than from the current directory of this module; not a hard problem
% though)
%
-doc "A Maybe-type.".
-type option( T ) :: T | 'undefined'.


% Implementation notes:
%
% This module is bootstrapped, as it is used by the meta_utils module to keep
% track of the spotted functions in such a map-based table. As a consequence at
% least the subset of the functions defined here and used by meta_utils should
% themselves not rely on any other module (whose BEAM would then not be
% available).

% Due to the partial support of maps in 17.3, some parts are commented and
% replaced by less idiomatic counterparts. Later they will be reactivated.


-compile( { inline, [ has_entry/2, add_entry/3, extract_entry/2 ] } ).


-define( default_bullet, " + " ).


% Type shorthands:

-type accumulator() :: basic_utils:accumulator().

-type void() :: type_utils:void().

-type ustring() :: text_utils:ustring().



-doc "Returns an empty, map-based hashtable.".
-spec new() -> map_hashtable().
new() ->
	% Empty map:
	#{}.



-doc """
Returns a map-based hashtable comprising only the specified entry.

More elegant than `map_hashtable:new([{K,V}], map_hashtable:new())`.
""".
-spec singleton( key(), value() ) -> map_hashtable().
singleton( Key, Value ) ->
	#{ Key => Value }.



-doc """
Creates a table, either from the specified initial entries or (solely for API
interoperability) from the specified target number of entries.

As map tables manage by themselves their size, no need to specify any target
size. This clause is only defined so that we can transparently switch APIs with
the other table modules.

If the same key appears more than once, the latter (right-most) value is used
and the previous values are ignored.

Throws bad argument (`bad_arg`) if a non-pair term is found in this list.
""".
-spec new( entries() | entry_count() ) -> map_hashtable().
new( ExpectedNumberOfEntries ) when is_integer( ExpectedNumberOfEntries ) ->
	#{};

new( InitialEntries ) when is_list( InitialEntries ) ->
	maps:from_list( InitialEntries ).



-doc """
Creates a table from the specified list of key/values pairs, expecting no
duplicate in the keys, otherwise throwing an exception.

Allows to safely load entries in a table without risking a silent overwrite of
any entry.
""".
new_from_unique_entries( InitialEntries ) when is_list( InitialEntries ) ->

	EntryCount = length( InitialEntries ),

	Table = new( InitialEntries ),

	case map_size( Table ) of

		EntryCount ->
			Table;

		_Other ->
			DupKeys = list_utils:get_duplicates(
				[ K || { K, _V } <- InitialEntries ] ),
			SortedEntries = lists:sort( InitialEntries ),
			throw( { duplicate_keys_found, DupKeys, SortedEntries } )

	end.



% Hints for the addition of key/value pairs to a table:
%
% - if one does not know or care whether the specified key is already used in
% the table, one should use add_entry/3 and add_entries/2 (and no specific
% checking about the key will be done)
%
% - if one knows that the specified key *is not* already used in the table, one
% should use add_new_entry/3 and add_new_entries/2 (and the absence of the key
% will be checked)
%
% - if one knows that the specified key *is* already used in the table, one
% should use update_entry/3 and update_entries/2 (and the presence of the key
% will be checked)
%
%
% Note: in non-debug mode, these extra checkings may be removed.



-doc """
Adds the specified key/value pair into the specified map hashtable.

If there is already a pair with this key, then its previous value will be
replaced by the specified one (hence does not check whether or not the key
already exist in this table).
""".
-spec add_entry( key(), value(), map_hashtable() ) -> map_hashtable().
add_entry( Key, Value, MapHashtable ) ->
	% Not supported in 17.3: MapHashtable#{ Key => Value }.
	maps:put( Key, Value, MapHashtable ).



-doc """
Adds the specified list of key/value pairs into the specified map table.

If there is already a pair with this key, then its previous value will be
replaced by the specified one (hence does not check whether or not keys already
exist in this table).
""".
-spec add_entries( entries(), map_hashtable() ) -> map_hashtable().
add_entries( Entries, MapHashtable ) ->
	lists:foldl( fun( { K, V }, Map ) ->
					%Map#{ K => V }
					maps:put( K, V, Map )
				 end,
				 _Acc0=MapHashtable,
				 _List=Entries ).



-doc """
Adds the specified key/value pair into the specified map hashtable, provided
that the value is not undefined. Useful to add a maybe-entry to a map.

If there is already a pair with this key, then its previous value will be
replaced by the specified one (hence does not check whether or not the key
already exist in this table).
""".
-spec add_option_entry( key(), option( value() ), map_hashtable() ) ->
											map_hashtable().
add_option_entry( _Key, _MaybeValue=undefined, MapHashtable ) ->
	MapHashtable;

add_option_entry( Key, MaybeValue, MapHashtable ) ->
	add_entry( Key, MaybeValue, MapHashtable ).



-doc """
Adds the specified list of key/value pairs into the specified map table, each
entry being added iff its value is not undefined. Useful to add maybe-entries to
a map.

If there is already a pair with this key, then its previous value will be
replaced by the specified one (hence does not check whether or not keys already
exist in this table).
""".
-spec add_option_entries( option_entries(), map_hashtable() ) ->
											map_hashtable().
add_option_entries( MaybeEntries, MapHashtable ) ->
	lists:foldl( fun( { K, MV }, Map ) ->
					add_option_entry( K, MV, Map )
				 end,
				 _Acc0=MapHashtable,
				 _List=MaybeEntries ).



-doc """
Adds the specified key/value pair into the specified map hashtable, expecting
this key not to be already defined in this table.
""".
-spec add_new_entry( key(), value(), map_hashtable() ) -> map_hashtable().
add_new_entry( Key, Value, MapHashtable ) ->

	% A tad expensive, could be replaced by an inlined add_entry/3 in non-debug
	% mode:
	%
	case has_entry( Key, MapHashtable ) of

		false ->
			add_entry( Key, Value, MapHashtable );

		true ->
			throw( { key_already_existing, Key } )

	end.



-doc """
Adds the specified list of key/value pairs into the specified map table,
expecting that none of these keys is already defined in this table (otherwise an
exception is thrown).
""".
-spec add_new_entries( entries(), map_hashtable() ) -> map_hashtable().
add_new_entries( Entries, MapHashtable ) ->
	lists:foldl( fun( { K, V }, Map ) ->
					add_new_entry( K, V, Map )
				 end,
				 _Acc0=MapHashtable,
				 _List=Entries ).



-doc """
Updates the specified key with the specified value in the specified map
hashtable.

An entry with this key is expected to already exist in this table, otherwise an
exception is thrown.
""".
-spec update_entry( key(), value(), map_hashtable() ) -> map_hashtable().
update_entry( Key, Value, MapHashtable ) ->

	% A tad expensive, could be replaced by an inlined add_entry/3 in non-debug
	% mode:
	%
	case has_entry( Key, MapHashtable ) of

		true ->
			add_entry( Key, Value, MapHashtable );

		false ->
			throw( { key_not_already_existing, Key } )

	end.



-doc """
Updates the specified list of keys with the specified values in the specified
map hashtable.

For each of the listed keys, a corresponding pair is expected to already exist
in this table.
""".
-spec update_entries( entries(), map_hashtable() ) -> map_hashtable().
update_entries( Entries, MapHashtable ) ->

	% Should be optimised:
	lists:foldl( fun( { K, V }, Map ) ->
					update_entry( K, V, Map )
				 end,
				 _Acc0=MapHashtable,
				 _List=Entries ).



-doc """
Returns the specified table when its already-existing keys have been updated
from the specified entries.

Specified entries whose keys are not already in the specified table are ignored.
""".
-spec update_existing_entries( entries(), map_hashtable() ) -> map_hashtable().
update_existing_entries( Entries, MapHashtable ) ->

	% Should be optimised:
	lists:foldl( fun( { K, V }, Map ) when is_map_key( K, Map ) ->
						Map#{ K => V };

					% K not present, thus ignore entry:
					( _E, Map ) ->
						Map
				 end,
				 _Acc0=MapHashtable,
				 _List=Entries ).



-doc """
Swaps in specified table the current value associated to the specified key with
the specified new value.

Returns the value previously associated to that key and an updated table.

The entry designated by the specified key is expected to exist already,
otherwise a `{bad_key,Key}` exception is triggered.
""".
-spec swap_value( key(), value(), map_hashtable() ) ->
							{ value(), map_hashtable() }.
swap_value( Key, NewValue, MapHashtable ) ->
	PreviousValue = maps:get( Key, MapHashtable ),
	NewMapHashtable = maps:put( Key, NewValue, MapHashtable ),
	{ PreviousValue, NewMapHashtable }.



-doc """
Removes the specified entry, as designated by its key, from the specified table.

Does nothing if the key is not found.

Returns an updated table.
""".
-spec remove_entry( key(), map_hashtable() ) -> map_hashtable().
remove_entry( Key, MapHashtable ) ->
	% Same semantics:
	maps:remove( Key, MapHashtable ).



-doc """
Removes the specified entry, as designated by its key, from the specified table.

Throws an exception if the key is not found.

Returns an updated table.
""".
-spec remove_existing_entry( key(), map_hashtable() ) -> map_hashtable().
remove_existing_entry( Key, MapHashtable ) ->

	case has_entry( Key, MapHashtable ) of

		true ->
			remove_entry( Key, MapHashtable ) ;

		false ->
			throw( { non_existing_key, Key } )

	end.



-doc """
Removes the specified key/value pairs, as designated by the keys, from the
specified table.

Specifying a non-existing key is accepted.

Returns an updated table.
""".
-spec remove_entries( [ key() ], map_hashtable() ) -> map_hashtable().
remove_entries( Keys, MapHashtable ) ->
	lists:foldl( fun( K, AccTable ) ->
					maps:remove( K, AccTable )
				 end,
				 _InitAcc=MapHashtable,
				 _List=Keys ).



-doc """
Removes the specified key/value pairs, as designated by the keys, from the
specified table.

Throws an exception if a key is not found.

Returns an updated table.
""".
-spec remove_existing_entries( [ key() ], map_hashtable() ) -> map_hashtable().
remove_existing_entries( Keys, MapHashtable ) ->
	lists:foldl( fun( K, AccTable ) ->
					remove_existing_entry( K, AccTable )
				 end,
				 _InitAcc=MapHashtable,
				 _List=Keys ).



-doc """
Looks-up the specified entry (designated by its key) in the specified map table.

Returns either `key_not_found` if no such key is registered in the table, or
`{value, Value}`, with Value being the value associated to the specified key.
""".
-spec lookup_entry( key(), map_hashtable() ) ->
						'key_not_found' | { 'value', value() }.
% Not supported in 17.3:
% lookup_entry( Key, #{ Key := Value } ) ->
%   { value, Value };

% lookup_entry( _Key, _MapHashtable ) ->
%   key_not_found.
lookup_entry( Key, MapHashtable ) ->

	case maps:find( Key, MapHashtable ) of

		{ ok, Value } ->
			{ value, Value };

		error ->
			key_not_found

	end.



-doc "Tells whether the specified key exists in the table.".
-spec has_entry( key(), map_hashtable() ) -> boolean().
has_entry( Key, MapHashtable ) ->
	maps:is_key( Key, MapHashtable ).

% has_entry( Key, #{ Key := _Value } ) ->
%   true;

% has_entry( _Key, _MapHashtable ) ->
%   false.



-doc """
Retrieves the value corresponding to the specified (existing) key, and returns
it directly.

The key/value pair is expected to exist already in the table, otherwise an
exception is raised.
""".
-spec get_value( key(), map_hashtable() ) -> value().
%get_value( Key, #{ Key := Value } ) ->
%   Value.
get_value( Key, MapHashtable ) ->
	try

		maps:get( Key, MapHashtable )

	catch

		error:{ badkey, _K } ->
			trace_utils:error_fmt( "No key '~p' found in the following table "
				"(process: ~w): ~ts",
				[ Key, self(), to_string( MapHashtable ) ] ),
			throw( { key_not_found, Key } )

	end.



-doc """
Returns the (ordered) list of values that correspond to the specified (ordered)
list of keys of this table.

The key/value pairs are expected to exist already in the table, otherwise an
exception is raised.

For example:
```[Color, Age, Mass] =
	map_hashtable:get_values([color, age, mass], MyMapTable)
```
""".
-spec get_values( [ key() ], map_hashtable() ) -> [ value() ].
get_values( Keys, Hashtable ) ->

	% We used to extract (not get) entries (presumably to handle smaller tables
	% in the process), yet the caller may specify a given key more than once,
	% which would then result into an abnormal crash; so:

	%{ RevValues, _FinalTable } = lists:foldl(
	%
	%   fun( _Elem=Key, _Acc={ Values, Table } ) ->
	%
	%       { Value, ShrunkTable } = extract_entry( Key, Table ),
	%       { [ Value | Values ], ShrunkTable }
	%
	%   end,
	%   _Acc0={ [], Hashtable },
	%   _List=Keys ),
	%
	%lists:reverse( RevValues ).

	[ get_value( K, Hashtable ) || K <- Keys ].



-doc """
Looks for the specified entry in the specified table and, if found, returns the
associated value; otherwise returns the specified default value.

Allows to perform in a single operation a look-up followed by a fetch.

A popular default value is `undefined`, so that this function can be considered
a returning a `option(value())`; no `get_maybe_value/2` function was introduced,
as it could be ambiguous, since in the general case a legit value could be
`undefined`.
""".
-spec get_value_with_default( key(), value(), map_hashtable() ) -> value().
get_value_with_default( Key, DefaultValue, MapHashtable ) ->

	case maps:find( Key, MapHashtable ) of

		{ ok, Value } ->
			Value;

		error ->
			DefaultValue

	end.



-doc """
Returns the (ordered) list of values that correspond to the specified (ordered)
list of keys of this table, ensuring that all entries in the specified table
have been read, otherwise throwing an exception.

The key/value pairs are expected to exist already, otherwise an exception is
raised.

For example:
```
[Color=red, Age=23, Mass=51] = map_hashtable:get_all_values(
	[color, age, mass], [{color, red}, {mass, 51}, {age, 23}])
```
""".
-spec get_all_values( [ key() ], map_hashtable() ) -> [ value() ].
get_all_values( Keys, Hashtable ) ->

	{ RevValues, FinalTable } = lists:foldl(
		fun( _Elem=Key, _Acc={ Values, Table } ) ->

			{ Value, ShrunkTable } = extract_entry( Key, Table ),
				{ [ Value | Values ], ShrunkTable }

		end,
		_Acc0={ [], Hashtable },
		_List=Keys ),

	case is_empty( FinalTable ) of

		true ->
			lists:reverse( RevValues );

		false ->
			throw( { remaining_keys, keys( FinalTable ) } )

	end.



-doc """
Extracts the specified entry from the specified hashtable, that is returns its
associated value and removes that entry from the returned table.

The key/value pair is expected to exist already, otherwise an exception is
raised (typically `{badkey, KeyNotFound}`).
""".
-spec extract_entry( key(), map_hashtable() ) -> { value(), map_hashtable() }.
%extract_entry( Key, MapHashtable=#{ Key := Value} ) ->
%   { Value, maps:remove( Key, MapHashtable ) }.
%
extract_entry( Key, MapHashtable ) ->
	Value = maps:get( Key, MapHashtable ),
	{ Value, maps:remove( Key, MapHashtable ) }.



-doc """
Extracts the specified entry from the specified table, that is returns the
associated value and removes that entry from the table.

If no such key is available, returns the specified default value and the
original table.
""".
-spec extract_entry_with_default( key(), value(), map_hashtable() ) ->
										{ value(), map_hashtable() }.
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

Typically useful to iterate over options stored as a table and extracting them
in turn, then to check that the resulting final table is empty as expected.
""".
-spec extract_entry_if_existing( key(), map_hashtable() ) ->
										'false' | { value(), map_hashtable() }.
extract_entry_if_existing( Key, MapHashtable ) ->

	case maps:is_key( Key, MapHashtable ) of

		true ->
			Value = maps:get( Key, MapHashtable ),
			{ Value, maps:remove( Key, MapHashtable ) };

		false ->
			false

	end.



-doc """
Extracts the specified entries from the specified table, that is returns their
associated values (in-order) and removes these entries from the returned table.

Each key/value pair is expected to exist already, otherwise an exception is
raised (typically `{badkey, KeyNotFound}`).

For example: `{[RedValue, GreenValue, BlueValue], ShrunkTable} =
   map_hashtable:extract_entries([red, green, blue], MyTable)`.
""".
-spec extract_entries( [ key() ], map_hashtable() ) ->
							{ [ value() ], map_hashtable() }.
extract_entries( Keys, MapHashtable ) ->
	{ RevValues, FinalTable } = lists:foldl(
		fun( K, { AccValues, AccTable } ) ->
			{ V, NewAccTable } = extract_entry( K, AccTable ),
			{ [ V | AccValues ], NewAccTable }
		end,
		_Acc0={ [], MapHashtable },
		_List=Keys ),

	{ lists:reverse( RevValues ), FinalTable }.



-doc """
Extracts the specified entries (if any) from the specified table, that is
returns them (in-order), and removes them from the returned table.

If a key is not present in the table, it is skipped.

For example, if no `green` key exists in MyTable:
```
{[{red,RedValue}, {blue,BlueValue}], ShrunkTable} =
   map_hashtable:extract_entries_if_existing([red, green, blue], MyTable)
```
""".
-spec extract_entries_if_existing( [ key() ], map_hashtable() ) ->
										{ entries(), map_hashtable() }.
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
Applies (maps) the specified anonymous function to each of the values contained
in this hashtable: to each key will be associated the value returned by this
function when applied to that key and its current value, as two arguments.

Allows to apply "in-place" an operation on all values without having to
enumerate the content of the hashtable and iterate on it (hence without having
to duplicate the whole content in memory).

See also: `map_on_values/2`.
""".
-spec map( fun( ( key(), value() ) -> value() ), map_hashtable() ) ->
						map_hashtable().
map( Fun, MapHashtable ) ->
	maps:map( Fun, MapHashtable ).



-doc """
Applies (maps) the specified anonymous function to each of the key-value entries
contained in this hashtable.

Allows to apply "in-place" an operation on all entries without having to
enumerate the content of the hashtable and iterate on it (hence without having
to duplicate the whole content in memory).

Note: same as `map/2` above, except that the lambda takes one argument (the
entry, as a pair) instead of two (the key and then the value), and returns an
entry, not a mere value to associate the same key (and thus may change the
structure , number of entries because of collisions, etc. of the table).
""".
-spec map_on_entries( fun( ( entry() ) -> entry() ), map_hashtable() ) ->
							map_hashtable().
map_on_entries( Fun, MapHashtable ) ->

	% maps:map/2 keeps the same keys, not relevant here.

	Entries = maps:to_list( MapHashtable ),

	NewEntries = lists:map( Fun, Entries ),

	maps:from_list( NewEntries ).



-doc """
Applies (maps) the specified anonymous function to each of the values contained
in this hashtable.

Allows to apply "in-place" an operation on all values without having to
enumerate the content of the hashtable, to iterate on it (hence without having
to duplicate the whole content in memory), and to recreate the table.

Note: the keys are left as are, hence the structure of the hashtable does not
change.

Note: same as `map/2` above, except that the lambda does not know about the
keys.
""".
-spec map_on_values( fun( ( value() ) -> value() ), map_hashtable() ) ->
							map_hashtable().
map_on_values( Fun, MapHashtable ) ->

	% Still not maps:map/2, whose fun takes an entry, not just a value:
	NewEntries = [ { K, Fun( V ) }
						|| { K, V } <- maps:to_list( MapHashtable ) ],

	maps:from_list( NewEntries ).



-doc """
Folds the specified anonymous function on all key/value pairs of the specified
map hashtable, based on the specified initial accumulator.

The order of transformation for entries is not specified.

Returns the final accumulator.
""".
-spec fold( fun( ( key(), value(), accumulator() ) -> accumulator() ),
			accumulator(), map_hashtable() ) -> accumulator().
fold( Fun, InitialAcc, Table ) ->
	maps:fold( Fun, InitialAcc, Table ).



-doc """
Folds the specified anonymous function on all entries of the specified map
hashtable.

The order of transformation for entries is not specified.

Returns the final accumulator.

`fold/3` may be preferred (being more efficient) to this version.
""".
-spec fold_on_entries( fun( ( entry(), accumulator() ) -> accumulator() ),
					   accumulator(), map_hashtable() ) -> accumulator().
fold_on_entries( Fun, InitialAcc, MapHashtable ) ->

	% Not exactly as maps:fold/3: we want f({K,V}, Acc), not f(K, V, Acc).

	ConversionFun = fun( K, V, Acc ) ->
						Fun( { K, V }, Acc )
					end,

	maps:fold( ConversionFun, InitialAcc, MapHashtable ).

	% Another solution is to implement it by ourselves:
	%Entries = maps:to_list( MapHashtable ),
	%lists:foldl( Fun, InitialAcc, Entries ).



-doc """
Adds the specified value to the value, supposed to be numerical, associated to
the specified key.

An exception is thrown if the key does not exist, a bad arithm is triggered if
no addition can be performed on the associated value.
""".
-spec add_to_entry( key(), number(), map_hashtable() ) -> map_hashtable().
% add_to_entry( Key, Value, MapHashtable=#{ Key => BaseValue } ) ->
%   MapHashtable#{ Key => BaseValue + Value };
%
% add_to_entry( _Key, _Value, _MapHashtable ) ->
%   throw( { key_not_found, Key } ).
%
add_to_entry( Key, Value, MapHashtable ) ->

	case maps:find( Key, MapHashtable ) of

		{ ok, BaseValue } ->
			maps:put( Key, BaseValue + Value, MapHashtable );

		error ->
			throw( { key_not_found, Key } )

	end.



-doc """
Subtracts the specified value to the value, supposed to be numerical, associated
to the specified key.

An exception is thrown if the key does not exist, a `bad arithm` is triggered if
no subtraction can be performed on the associated value.
""".
-spec subtract_from_entry( key(), number(), map_hashtable() ) ->
								map_hashtable().
% subtract_from_entry( Key, Value, MapHashtable=#{ Key => BaseValue } ) ->
%   MapHashtable#{ Key => BaseValue - Value };
%
% subtract_from_entry( _Key, _Value, _MapHashtable ) ->
%   throw( { key_not_found, Key } ).
%
subtract_from_entry( Key, Value, MapHashtable ) ->

	case maps:find( Key, MapHashtable ) of

		{ ok, BaseValue } ->
			maps:put( Key, BaseValue - Value, MapHashtable );

		error ->
			throw( { key_not_found, Key } )

	end.



-doc """
Toggles the boolean value associated with the specified key: if true will be
false, if false will be true.

An exception is thrown if the key does not exist or if its associated value is
not a boolean.
""".
-spec toggle_entry( key(), map_hashtable() ) -> map_hashtable().
% toggle_entry( Key, MapHashtable=#{ Key => true } ) ->
%   MapHashtable#{ Key => false };
%
% toggle_entry( Key, MapHashtable=#{ Key => false } ) ->
%   MapHashtable#{ Key => true }.
toggle_entry( Key, MapHashtable )->
	V = maps:get( Key, MapHashtable ),
	maps:put( Key, not V, MapHashtable ).



-doc """
Returns a new map hashtable, which started from `MapHashtableRef` and was
enriched with the entries from `MapHashtableOnlyForAdditions` whose keys were
*not* already in `MapHashtableRef` (if a key is in both tables, the one from
`MapHashtableRef` is the one kept).

Said differently: if a key exists in both tables, the value in
`MapHashtableOnlyForAdditions` will be dropped (will *not* supersede the value
in `MapHashtableRef`, which is the "prioritary" one).

Note: not the standard merge that one would expect, should values be lists.
""".
-spec merge( map_hashtable(), map_hashtable() ) -> map_hashtable().
merge( MapHashtableRef, MapHashtableOnlyForAdditions ) ->
	% Order matters (note the swap of variables):
	maps:merge( MapHashtableOnlyForAdditions, MapHashtableRef ).



-doc """
Returns a new map hashtable, which merged the specified map hashtables in their
listed order (enriching the current entries with the ones of the next table,
provided that they are not already present)

Note: not the standard merge that one would expect, should values be lists.
""".
-spec merge( [ map_hashtable() ] ) -> map_hashtable().
merge( MapHashtables ) ->
	lists:foldl( fun( Table, AccTable ) ->
					% Order matters:
					maps:merge( Table, AccTable )
				 end,
				 _Acc0=#{},
				 _List=MapHashtables ).



-doc """
Merges the two specified tables into one, expecting that their keys are unique
(that is that they do not intersect), otherwise throws an exception.

Note: for an improved efficiency, ideally the smaller table shall be the first
one.
""".
-spec merge_unique( map_hashtable(), map_hashtable() ) -> map_hashtable().
merge_unique( FirstHashtable, SecondHashtable ) ->
	FirstEntries = enumerate( FirstHashtable ),
	add_new_entries( _ToAdd=FirstEntries, SecondHashtable ).



-doc """
Merges all specified tables into one, expecting that their keys are unique
(i.e. that they do not intersect), otherwise throws an exception.

Note: for an improved efficiency, ideally the tables shall be listed by
increasing sizes.
""".
-spec merge_unique( [ map_hashtable() ] ) -> map_hashtable().
% (no empty list expected)
merge_unique( _Tables=[ Table ] ) ->
	Table;

% To avoid recreating from scratch the first table:
merge_unique( _Tables=[ HTable | T ] ) ->
	lists:foldl( fun( Table, AccTable ) ->
					merge_unique( Table, AccTable )
				 end,
				 _Acc0=HTable,
				 _List=T ).



-doc """
Optimises this hashtable.

A no-operation for map hashtables.
""".
-spec optimise( map_hashtable() ) -> map_hashtable().
optimise( Hashtable ) ->
	Hashtable.



-doc """
Appends the specified element to the value, supposed to be a list, associated to
the specified key, which must already exist in that table.

An exception is thrown if the key does not exist.

Note: no check is performed to ensure that the value is already a list indeed,
and the cons (`[|]`) operation will not complain if not.
""".
-spec append_to_existing_entry( key(), term(), map_hashtable() ) ->
									map_hashtable().
%append_to_existing_entry( Key, Element, MapHashtable=#{ Key => ListValue } ) ->
%   MapHashtable#{ Key => [Element | ListValue] };
%
%append_to_existing_entry( Key, _Element, _MapHashtable ) ->
%   throw( { key_not_found, Key } ).
%
append_to_existing_entry( Key, Element, MapHashtable ) ->

	ListValue = maps:get( Key, MapHashtable ),

	maps:put( Key, [ Element | ListValue ], MapHashtable ).



-doc """
Appends the specified elements to the value, supposed to be a list, associated
to the specified key.

An exception is thrown if the key does not exist.
""".
-spec append_list_to_existing_entry( key(), [ term() ], map_hashtable() ) ->
								map_hashtable().
append_list_to_existing_entry( Key, Elements, Hashtable ) ->

	ListValue = maps:get( Key, Hashtable ),

	maps:put( Key, Elements ++ ListValue, Hashtable ).



-doc """
Appends the specified element to the value, supposed to be a list, associated to
the specified key.

If that key does not already exist, it will be created and associated to a list
containing only the specified element.
""".
-spec append_to_entry( key(), term(), map_hashtable() ) -> map_hashtable().
append_to_entry( Key, Element, MapHashtable ) ->

	case lookup_entry( Key, MapHashtable ) of

		key_not_found ->
			add_entry( Key, [ Element ], MapHashtable );

		{ value, CurrentList } ->
			add_entry( Key, [ Element | CurrentList ], MapHashtable )

	end.



-doc """
Appends the specified elements to the value, supposed to be a list, associated
to the specified key.

If that key does not already exist, it will be created and associated to a list
containing only the specified elements.
""".
-spec append_list_to_entry( key(), [ term() ], map_hashtable() ) ->
								map_hashtable().
append_list_to_entry( Key, Elements, MapHashtable ) ->

	case lookup_entry( Key, MapHashtable ) of

		key_not_found ->
			add_entry( Key, Elements, MapHashtable );

		{ value, CurrentList } ->
			add_entry( Key, Elements ++ CurrentList, MapHashtable )

	end.



-doc """
Concatenates (on the left) the specified list to the value, supposed to be a
list as well, associated to the specified key.

If that key does not already exist, it will be created and associated to the
specified list (as if beforehand the key was associated to an empty list).
""".
-spec concat_to_entry( key(), list(), map_hashtable() ) -> map_hashtable().
concat_to_entry( Key, ListToConcat, MapHashtable )
										when is_list( ListToConcat ) ->

	case lookup_entry( Key, MapHashtable ) of

		key_not_found ->
			add_entry( Key, ListToConcat, MapHashtable );

		{ value, CurrentList } ->
			add_entry( Key, ListToConcat ++ CurrentList, MapHashtable )

	end.



-doc """
Concatenates (on the left) the specified lists to the values, supposed to be
lists as well, associated to the specified keys, the input being thus a list of
key/list-value pairs.

If a key does not already exist, it will be created and associated to the
specified list (as if beforehand the key was associated to an empty list)

For example:
```
concat_list_to_entries( [{hello, [1, 2]}, {world, [4]}], MyTable )
```
""".
-spec concat_list_to_entries( list_table:list_table(), map_hashtable() ) ->
										map_hashtable().
concat_list_to_entries( KeyListValuePairs, MapHashtable )
							when is_list( KeyListValuePairs ) ->
	lists:foldl( fun( { Key, ListToConcat }, AccTable ) ->
					concat_to_entry( Key, ListToConcat, AccTable )
				 end,
				 _Acc0=MapHashtable,
				 _List=KeyListValuePairs ).



-doc """
Deletes the first match of the specified element in the value associated to the
specified key, this value being assumed to be a list.

An exception is thrown if the key does not exist.

If the element is not in the specified list, the list will not be modified.
""".
-spec delete_from_entry( key(), term(), map_hashtable() ) -> map_hashtable().
%delete_from_entry( Key, Element, MapHashtable=#{ Key => ListValue } ) ->
%   MapHashtable#{ Key => lists:delete( Element, ListValue ) };
%
%delete_from_entry( Key, _Element, _MapHashtable ) ->
%   throw( { key_not_found, Key } ).
%
delete_from_entry( Key, Element, MapHashtable ) ->

	ListValue = maps:get( Key, MapHashtable ),

	maps:put( Key, lists:delete( Element, ListValue ), MapHashtable ).



-doc """
Deletes the first match of the specified element in the value associated to the
specified key, this value being assumed to be a list.

An exception is thrown if the key does not exist, or if the element is not in
the targeted list.
""".
-spec delete_existing_from_entry( key(), term(), map_hashtable() ) ->
											map_hashtable().
delete_existing_from_entry( Key, Element, MapHashtable ) ->

	ListValue = maps:get( Key, MapHashtable ),

	maps:put( Key, list_utils:delete_existing( Element, ListValue ),
			  MapHashtable ).



-doc """
Pops the head of the value (supposed to be a list) associated to thet specified
key, and returns a pair made of the popped head and the new hashtable.
""".
-spec pop_from_entry( key(), map_hashtable() ) -> { term(), map_hashtable() }.
%pop_from_entry( Key, MapHashtable=#{ Key => [ H | T ] } ) ->
%   { H, MapHashtable#{ Key => T } };
%
%pop_from_entry( Key, _MapHashtable ) ->
%   throw( { key_not_found, Key } ).
%
pop_from_entry( Key, MapHashtable ) ->
	[ H | T ] = maps:get( Key, MapHashtable ),
	{ H, maps:put( Key, T, MapHashtable ) }.



-doc """
Returns a flat list whose elements are all the key/value pairs of the hashtable,
in no particular order.

For example `[{K1,V1}, {K2,V2}, ...]`.
""".
-spec enumerate( map_hashtable() ) -> entries().
enumerate( MapHashtable ) ->
	maps:to_list( MapHashtable ).



-doc """
Returns a list of key/value pairs corresponding to the list of specified keys,
or throws a `badmatch` exception if at least one key is not found.
""".
-spec select_entries( [ key() ], map_hashtable() ) -> entries().
select_entries( Keys, MapHashtable ) ->
	SubMap = maps:with( Keys, MapHashtable ),
	maps:to_list( SubMap ).



-doc "Returns a list containing all the keys of this hashtable.".
-spec keys( map_hashtable() ) -> [ key() ].
keys( MapHashtable ) ->
	maps:keys( MapHashtable ).



-doc """
Returns a list (in no particular order) containing all the values of this
hashtable.

For example useful if the key was used as an index to generate this table first
(no information loss).
""".
-spec values( map_hashtable() ) -> [ value() ].
values( MapHashtable ) ->
	maps:values( MapHashtable ).



-doc """
Returns whether the specified hashtable is empty (not storing any key/value
pair).
""".
-spec is_empty( map_hashtable() ) -> boolean().
%is_empty( _MapHashtable=#{} ) ->
%   true;
%
%is_empty( _MapHashtable ) ->
%   false.
%

% Most probably the most efficient implementation (even if looking as computing
% a size that is not really used), as a map includes its size in the header
% word, so map_size/1 is O(1); furthermore, the JIT will inline calls to
% map_size/1; so:
%
is_empty( MapHashtable ) when map_size( MapHashtable ) > 0 ->
	false;

is_empty( _MapHashtable ) ->
	true.



-doc """
Returns the size (number of entries, that is of key/value pairs) of the
specified table.
""".
-spec size( map_hashtable() ) -> entry_count().
size( MapHashtable ) ->
	map_size( MapHashtable ).



-doc """
Returns a textual description of the specified map hashtable.

See also `text_utils:table_to_string/2` for a synthetic description of a table
and its entries.
""".
-spec to_string( map_hashtable() ) -> ustring().
to_string( MapHashtable ) ->
	% Newer, better default (was: 'user_friendly'):
	to_string( MapHashtable, _DescriptionType=full ).



-doc """
Returns a textual description of the specified hashtable.

Either a bullet is specified, or the returned string is ellipsed if needed (if
using `user_friendly`), or quite raw and non-ellipsed (if using `full`), or even
completly raw (`internal`).

See also `text_utils:table_to_string/2` for a synthetic description of a table
and its entries.
""".
-spec to_string( map_hashtable(), hashtable:description_type() ) -> ustring().
to_string( MapHashtable, DescriptionType ) ->

	case maps:to_list( MapHashtable ) of

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
						[ map_size( MapHashtable ),
						  text_utils:strings_to_string( Strs,
														?default_bullet ) ] ) );

				DescType when DescType =:= full orelse DescType =:= internal ->
					Strs = [ text_utils:format( "~p: ~p", [ K, V ] )
								|| { K, V } <- lists:sort( L ) ],

					lists:flatten( io_lib:format( "table with ~B entries: ~ts",
						[ map_size( MapHashtable ),
						  text_utils:strings_to_string( Strs,
														?default_bullet ) ] ) );

				% Here, ellipsed and with specified bullet:
				Bullet ->
					Strs = [ text_utils:format_ellipsed( "~p: ~p", [ K, V ] )
								|| { K, V } <- lists:sort( L ) ],

					lists:flatten( io_lib:format( "table with ~B entries: ~ts",
						[ map_size( MapHashtable ),
						  text_utils:strings_to_string( Strs, Bullet ) ] ) )

			end

	end.



-doc "Displays the specified map hashtable on the standard output.".
-spec display( map_hashtable() ) -> void().
display( MapHashtable ) ->
	io:format( "~ts~n", [ to_string( MapHashtable ) ] ).



-doc """
Displays the specified map hashtable on the standard output, with the specified
title on top.
""".
-spec display( ustring(), map_hashtable() ) -> void().
display( Title, MapHashtable ) ->
	io:format( "~ts:~n~ts~n", [ Title, to_string( MapHashtable ) ] ).
