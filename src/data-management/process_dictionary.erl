% Copyright (C) 2018-2018 Olivier Boudeville
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
% Creation date: Monday, April 30, 2018



% Process dictionary-related support.
%
% While using the process dictionary is usually regarded with contempt for good
% reasons (impure, prone to unwanted side-effects), there are a few specific
% cases (ex: the state of a user interfaces being made implicit) where it might
% be nevertheless useful.
%
% We provide basic encapsulation of the ways of interacting with the process
% dictionary, notably so that it is easier to locate (ex: thanks to 'grep') the
% places where the process dictionary is used.
%
% This module could have been need 'impure_table' as well, and could have obeyed
% a table-like API.
%
-module(process_dictionary).


% Actually any term:
-type key() :: hashtable:key().

-type value() :: hashtable:value().

-type entry() :: hashtable:entry().

-type entries() :: [ entry() ].

-type entry_count() :: basic_utils:count().


% Explicit form thereof, as a term:
-type process_dictionary() :: list_table:list_table().


-export_type([ key/0, value/0, entry/0, entries/0, entry_count/0 ]).



-export([ put/2, get/1, remove/1,
		  get_dictionary/0, get_keys/0, get_keys_for/1,
		  blank/0 ]).


% Puts specified entry in the process dictionary; returns any value previously
% associated to that key.
%
-spec put( key(), value() ) -> maybe( value() ).
put( Key, Value ) ->
	erlang:put( Key, Value ).



% Returns the value (if any) associated to specified key in the process dictionary.
%
-spec get( key() ) -> maybe( value() ).
get( Key ) ->
	erlang:get( Key ).



% Removes any entry in the process dictionary corresponding to specified key,
% returning any value that was associated with it.
%
-spec remove( key() ) -> maybe( value() ).
remove( Key ) ->
	erlang:erase( Key ).



% Returns the full process dictionary, as a term.
%
-spec get_dictionary() -> process_dictionary().
get_dictionary() ->
	erlang:get().



% Returns a list of all keys present in the process dictionary.
%
-spec get_keys() -> [ key() ].
get_keys() ->
	erlang:get_keys().



% Returns a list of keys that are associated with the specified value in the process
% dictionary.
%
-spec get_keys_for( value() ) -> [ key() ].
get_keys_for( Value ) ->
	erlang:get_keys( Value ).



% Blanks the process dictionary (erases all entries), and returns its past
% content (as a term).
%
-spec blank() -> process_dictionary().
blank() ->
	erlang:erase().
