% Copyright (C) 2016-2017 Olivier Boudeville
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: July 1, 2007.


% Gathering of various facilities about sets.
%
% A set is a container that:
%
% - does not allow duplicates (adding an element more than once is like adding
% it once)
%
% - has no intrinsic order (yet can be iterated over)
%
% Note: we provide here a basic, general-purpose set support, and do not rely on
% data_utils.hrl which provides a much lighter, alternate level of indirection.
%
% See set_utils_test.erl for the corresponding test.
%
% See also: list_utils.erl and set_utils_test.erl.
%
-module(set_utils).



% Set-related operations are:
%
-export([ new/0, singleton/1, add/2, addAsNew/2, add_element_list/2,
		  union/2, union/1, intersection/2, intersection/1,
		  difference/2, is_set/1, check_set/1, is_subset/2,
		  from_list/1, to_list/1,
		  member/2, is_empty/1, size/1,
		  iterator/1, next/1,
		  delete/2, safe_delete/2, to_string/1 ]).


% Our default elected type of set:
%
% (ordsets may, perhaps in many cases, perform better, yet provide less
% features; ex: no iterators)
%
% Apparently gb_sets are stored as tuples, whose first, top-level element is
% their size (hence fetching the size of a set should be inexpensive)
%
-define( set_impl, gb_sets ).

-type set() :: gb_sets:set().


% For homogeneous sets:
%
% (strangely reported as unused by the compiler: 'type set(_) is unused',
% although exported)
%
%-type set( T ) :: gb_sets:set( T ).


% Element of a set:
-type element() :: term().


% Internally, a kind of enumeration (list) of the elements in the set:
-type iterator() :: gb_sets:iter().


-export_types([ set/0, set/1, element/0, iterator/0 ]).



% Design notes:
%
% The purpose is to provide a set-like container, iterable yet *not preserving
% order*, able to perform some operations (typically: element look-up) more
% efficiently than plain lists, especially when the number of elements becomes

% significant.



% Returns a new empty set.
%
-spec new() -> set().
new() ->
	?set_impl:new().



% Returns a set comprising only specified element.
%
% More elegant than set_utils:add( Foo, set_utils:new() ).
%
-spec singleton( element() ) -> set().
singleton( Element ) ->
	% Not defined for ordsets:
	%?set_impl:singleton( Element ).
	?set_impl:add_element( Element, ?set_impl:new() ).



% Returns a new set formed from the specified one with specified element
% inserted. If this element is already in the specified set, the returned set is
% the same.
%
-spec add( element(), set() ) -> set().
add( Element, Set ) ->
	?set_impl:add_element( Element, Set ).



% Returns a new set formed from the specified one with specified element
% inserted, checking that this element was not already in the original set
% (otherwise a batmatch exception is thrown).
%
-spec addAsNew( element(), set() ) -> set().
addAsNew( Element, Set ) ->
	case ?set_impl:is_member( Element, Set ) of

		false ->
			?set_impl:add_element( Element, Set );

		true ->
			throw( { already_in_set, Element, ?set_impl:to_list( Set ) } )

	end.



% Returns a set made of the specified set to which the elements of the specified
% plain list have been added.
%
-spec add_element_list( list(), set() ) -> set().
%add_element_list( _PlainList=[], Set ) ->
%Set;

%add_element_list( _PlainList=[ H | T ], Set ) ->
%NewSet = ?set_impl:add_element( H, SetImplSet ),
%add_element_list( T, NewSet ).
add_element_list( List, Set ) ->
	AddSet = ?set_impl:from_list( List ),
	?set_impl:union( AddSet, Set ).



% Returns the union of the two specified sets.
%
-spec union( set(), set() ) -> set().
union( FirstSet, SecondSet ) ->
	?set_impl:union( FirstSet, SecondSet ).


% Returns the union of the specified sets.
%
-spec union( [ set() ] ) -> set().
union( ListOfSets ) ->
	?set_impl:union( ListOfSets ).



% Returns the intersection of the two specified sets.
%
-spec intersection( set(), set() ) -> set().
intersection( FirstSet, SecondSet ) ->
	?set_impl:intersection( FirstSet, SecondSet ).


% Returns the intersection of the specified sets.
%
-spec intersection( [ set() ] ) -> set().
intersection( ListOfSets ) ->
	?set_impl:intersection( ListOfSets ).



% Returns the difference between the first specified set and the second,
% i.e. the elements of the first set that are not in the second one.
%
-spec difference( set(), set() ) -> set().
difference( FirstSet, SecondSet ) ->
	?set_impl:difference( FirstSet, SecondSet ).



% Returns whether the specified term appears to be a legit set.
%
-spec is_set( term() ) -> boolean().
is_set( Term ) ->
	?set_impl:is_set( Term ).



% Ensures that the specified term is a set, throws an exception if not.
%
-spec check_set( term() ) -> basic_utils:void().
check_set( Term ) ->
	case is_set( Term ) of

		true ->
			ok;

		false ->
			throw( { not_a_set, Term } )

	end.



% Tells whether the first set is a subset of the second, i.e. if each element of
% the first is also in the second.
%
-spec is_subset( set(), set() ) -> boolean().
is_subset( FirstSet, SecondSet ) ->
	?set_impl:is_subset( FirstSet, SecondSet ).



% Returns a set created from specified list.
%
-spec from_list( list() ) -> set().
from_list( List ) ->
	?set_impl:from_list( List ).



% Returns a list created from the elements of specified set.
%
-spec to_list( set() ) -> list().
to_list( Set ) ->
	?set_impl:to_list( Set ).



% Returns true iff specified element is an element of specified set.
%
-spec member( element(), set() ) -> boolean().
member( Element, Set ) ->
	?set_impl:is_member( Element, Set ).



% Returns whether the specified set is empty.
%
-spec is_empty( set() ) -> boolean().
is_empty( Set ) ->
	% Not defined for ordsets:
	%?set_impl:is_empty( Set ).
	0 =:= ?set_impl:size( Set ).



% Returns the number of elements in specified set.
%
-spec size( set() ) -> basic_utils:count().
size( Set ) ->
	?set_impl:size( Set ).



% Note: iterating could be done with a fold as well (ordsets).


% Returns an iterator that can be used for traversing the entries of the
% specified set.
%
% Note: the iterator is *not* the first iterated element of a set: next/1 shall
% be used even for the very first element.
%
-spec iterator( set() ) -> iterator().
iterator( Set ) ->
	?set_impl:iterator( Set ).



%-spec iterator_from( element(), set() ) ->
%iterator_from( Element, Set ) ->

% Allows the iterators to be gone through.
%
%
-spec next( iterator() ) -> { element(), iterator() } | 'none'.
next( Iterator ) ->
	?set_impl:next( Iterator ).



% Removes the specified element (if any) from the specified set, and returns the
% resulting set.
%
% Note: does not fail if the element was not in the set; use safe_delete/2 to
% ensure that the element was present.
%
-spec delete( term(), set() ) -> set().
delete( Element, Set ) ->
	?set_impl:del_element( Element, Set ).



% Ensures that the specified element was indeed in the specified set before
% removing it, and returning the resulting set.
%
% Note: use delete/2 to delete an element without checking whether the element
% is present in the set.
%
-spec safe_delete( term(), set() ) -> set().
safe_delete( Element, Set ) ->

	case ?set_impl:is_element( Element, Set ) of

		true ->
			?set_impl:del_element( Element, Set );

		false ->
			throw( { non_existing_element_to_delete, Element,
					 ?set_impl:to_list( Set ) } )

	end.



% Returns a textual representation of specified set.
%
-spec to_string( set() ) -> string().
to_string( Set ) ->
	case ?set_impl:size( Set ) of

		0 ->
			"empty set";

		1 ->
			[ Elem ] = ?set_impl:to_list( Set ),
			text_utils:format( "set containing a single element: ~p",
							   [ Elem ] );

		S ->
			ElemStrings = [ text_utils:format( "~p", [ E ] )
							|| E <- ?set_impl:to_list( Set ) ],

			text_utils:format( "set containing following ~B elements:~s",
					[ S, text_utils:strings_to_string( ElemStrings ) ] )

	end.
