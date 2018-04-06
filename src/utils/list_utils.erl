% Copyright (C) 2003-2018 Olivier Boudeville
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


% Gathering of various facilities about lists.
%
% See list_utils_test.erl for the corresponding test.
%
% See also: set_utils.erl
%
-module(list_utils).



% For list_impl:
-include("data_types.hrl").


% Checks regarding lists:
%
-export([ ensure_list/1, ensure_list_of_atoms/1, ensure_list_of_tuples/1,
		  ensure_list_of_pids/1 ]).



% Basic list operations:
%
-export([ get_element_at/2, insert_element_at/3,
		  remove_element_at/2, remove_last_element/1,
		  get_last_element/1, extract_last_element/1,
		  get_index_of/2, split_at/2, uniquify/1,
		  has_duplicates/1, get_duplicates/1, union/2, intersection/2,
		  cartesian_product/1,
		  subtract_all_duplicates/2, delete_existing/2, delete_if_existing/2,
		  delete_all_in/2, append_at_end/2, is_list_of_integers/1,
		  unordered_compare/2 ]).


% For list of tuples (ex: typically used by the HDF5 binding), extended flatten
% and al:
%
-export([ determine_tuple_info/1, flatten_tuples/1, reconstruct_tuples/2 ]).



% Random operations on lists:
%
-export([ random_permute/1, random_permute_reciprocal/1,
		  draw_element/1, draw_element/2, draw_element_weighted/1,
		  draw_elements_from/2 ]).


% Either a list of terms, or a term by itself.
%
% Note: different from maybe( list() ).
%
-type maybe_list( T ) :: [ T ] | T.


-export_type([ maybe_list/1 ]).



% Section for the checking of lists.


% Ensures that the specified argument is a list: encloses it in a list of its
% own if not already a list.
%
% Note: not to be applied on strings for example.
%
-spec ensure_list( maybe_list( T ) ) -> [ T ].
ensure_list( List ) when is_list( List ) ->
	List;

ensure_list( Term ) ->
	[ Term ].


% Ensures that the specified argument is a list of atoms: encloses any atom in
% a list of its own if not already a list, or check that this list is only
% populated of atoms.
%
ensure_list_of_atoms( Atom ) when is_atom( Atom ) ->
	[ Atom ];

ensure_list_of_atoms( List ) when is_list( List ) ->
	case meta_utils:is_homogeneous( List, _CommonType=atom ) of

		true ->
			List;

		false ->
			throw( { not_list_of_atoms, List } )

	end;

ensure_list_of_atoms( Other ) ->
	throw( { neither_list_nor_atom, Other } ).



% Ensures that the specified argument is a list of tuples: encloses any tuple in
% a list of its own if not already a list, or check that this list is only
% populated of tuples.
%
ensure_list_of_tuples( Tuple ) when is_tuple( Tuple ) ->
	[ Tuple ];

ensure_list_of_tuples( List ) when is_list( List ) ->
	case meta_utils:is_homogeneous( List, _CommonType=tuple ) of

		true ->
			List;

		false ->
			throw( { not_list_of_tuples, List } )

	end;

ensure_list_of_tuples( Other ) ->
	throw( { neither_list_nor_tuple, Other } ).



% Ensures that the specified argument is a list of PIDs: encloses any PID in a
% list of its own if not already a list, or check that this list is only
% populated of PIDs.
%

ensure_list_of_pids( Pid ) when is_pid( Pid ) ->
	[ Pid ];

ensure_list_of_pids( List ) when is_list( List ) ->
	case meta_utils:is_homogeneous( List, _CommonType=pid ) of

		true ->
			List;

		false ->
			throw( { not_list_of_pids, List } )

	end;

ensure_list_of_pids( Other ) ->
	throw( { neither_list_nor_pid, Other } ).



% Section for basic list operations.


% Index start at position #1, not #0.

% Returns the element in the list at the specified index, in [1..length(List)].
%
% If the index is out of bounds, a function_clause is raised.
%
% Note: usually these kinds of functions should not be used, recursive
% algorithms are a lot more effective, when applicable.
%
% Not tail recursive version:
%
%% get_element_at( List, 1 ) ->
%%	hd(List);
%
%% get_element_at( [ _H | T ], Index ) ->
%%	get_element_at( T, Index-1 ).
%
-spec get_element_at( list(), basic_utils:positive_index() ) -> any().
get_element_at( List, Index ) ->
	%io:format( " - getting element #~B of ~w~n", [ Index, List ] ),
	lists:nth( Index, List ).



% Inserts specified element at specified position in specified list.
%
% For example, insert_element_at( foo, [ a, b, c, d ], 3 ) will return
% [ a, b, foo, c, d ].
%
-spec insert_element_at( any(), list(), basic_utils:positive_index() ) ->
							   list().
insert_element_at( Element, List, Index ) ->

	%io:format( " - inserting element ~p at #~B in ~w~n",
	%		   [ Element, Index, List ] ),

	insert_element_at( Element, List, Index, _Acc=[] ).


insert_element_at( Element, _List=[], _Index=1, Acc ) ->
	lists:reverse( [ Element | Acc ] );

insert_element_at( _Element, _List=[], Index, Acc ) ->
	% Rebuilds input parameters:
	throw( { invalid_index, Index + length( Acc ), lists:reverse( Acc ) } );

insert_element_at( Element, List, _Index=1, Acc ) ->
	lists:reverse( [ Element | Acc ] ) ++ List;

insert_element_at( Element, _List=[ H | T ], Index, Acc ) ->
	insert_element_at( Element, T, Index-1, [ H | Acc ] ).




% Returns a list corresponding to the specified one with the element at
% specified index removed.
%
% If the index is out of bounds, a function_clause like
% '[{list_utils,remove_element_at,...}]' is triggered.
%
% Note: usually these kinds of functions should not be used, recursive
% algorithms are a lot more effective, when applicable.
%
% Signature: remove_element_at( List, Index ).
%
% Curiously lists:nth exists, but no function to remove an element specified by
% its index seems to be available in the lists module.
%
% Not tail recursive version:
%remove_element_at( [ _H | T ], _LastIndex=1 ) ->
%   T;
%
%remove_element_at( [ H | T ], _Index=N ) ->
%   [ H | remove_element_at( T, _NextIndex=N-1 ) ].
%
% Tail recursive version:
%
-spec remove_element_at( list(), basic_utils:positive_index()) -> list().
remove_element_at( List, Index ) ->
	remove_element_at( List, Index, _Result=[] ).

remove_element_at( [ _H | RemainingList ], 1, Result ) ->
	lists:reverse( Result ) ++ RemainingList;

remove_element_at( [ H | RemainingList ], Index, Result ) ->
	remove_element_at( RemainingList, Index-1, [ H | Result ] ).



% Removes the last element of the specified list.
%
% Crashes (with 'no function clause') if the input list is empty.
%
% Note: not computationnally efficient, usually removing the last element
% suggests a bad code design.
%
-spec remove_last_element( list() ) -> list().
remove_last_element( List ) ->
	lists:droplast( List ).

%	remove_last_element( List, _Acc=[] ).


%remove_last_element( _List=[ _Last ], Acc ) ->
%	lists:reverse( Acc );

%remove_last_element( _List=[ H | T ], Acc ) ->
%	remove_last_element( T, [ H | Acc ] ).



% Returns the last element of the specified list.
%
% Note: not computationnally efficient, usually having to retrieve the last
% element suggests a bad code design.
%
% Crashes (with 'no function clause') if the input list is empty.
%
-spec get_last_element( list() ) -> term().
get_last_element( _List=[ SingleElement ] ) ->
	SingleElement;

get_last_element( _List=[ _H | T ] ) ->
	get_last_element( T ).


% Extracts the last element of the specified list, returning a pair made of that
% element and of the remainder of the list (in its original order).
%
% Note: not computationnally efficient, usually having to retrieve the last
% element suggests a bad code design.
%
% Crashes (with 'no function clause') if the input list is empty.
%
-spec extract_last_element( list() ) -> { term(), list() }.
extract_last_element( List ) ->

	% Probably the most efficient variant:
	[ LastElement | RevRest ] = lists:reverse( List ),

	{ LastElement, lists:reverse( RevRest ) }.


% Variant:
%extract_last_element( List ) ->
%	extract_last_element( List, _Acc=[] ).


% (helper)
%extract_last_element( _List=[ LastElement ], Acc ) ->
%	{ LastElement, lists:reverse( Acc ) };
%
%extract_last_element( _List=[ H | T ], Acc ) ->
%	extract_last_element( T, [ H | Acc ] ).



% Returns the index, in [1..length(List)], of the (first occurrence of the)
% specified element in the specified list. Throws an exception if the element is
% not found.
%
% Ex: 3 = get_index_of( bar, [ foo, ugh, bar, baz ] )
%
-spec get_index_of( term(), list() ) -> basic_utils:count().
get_index_of( Element, List ) ->
	get_index_of( Element, List, _Count=1 ).


get_index_of( Element, _List=[], _Count ) ->
	throw( { non_existing_element, Element } );

get_index_of( Element, _List=[ Element | _T ], Count  ) ->
	Count;

get_index_of( Element, _List=[ _H | T ], Count ) ->
	get_index_of( Element, T, Count + 1 ).



% Splits the specified (plain) list in two parts (two plain lists, that are
% returned): the first contains the first elements, up to MaxLen (in reverse
% order), and the second the others (if any).
%
% Ex: split_at( [ a, b, c, d, e ], 3 ) = { [ c, b, a ], [ d, e] }
%
-spec split_at( list(), basic_utils:count() ) -> { list(), list() }.
split_at( List, MaxLen ) ->
	split_at( List, _Count=0, MaxLen, _Acc=[] ).


% (helper)
split_at( InputList, _Count=MaxLen, MaxLen, AccList ) ->
	% Max len reached, stopping here:
	{ AccList, InputList };

split_at( _InputList=[], _Count, _MaxLen, AccList ) ->
	% Input list exhausted:
	{ AccList, _InputList=[] };


split_at( _List=[ H | T ], Count, MaxLen, Acc ) ->
	split_at( T, Count + 1, MaxLen, [ H | Acc ] ).




% Returns a list whose elements are the ones of the specified list, except that
% they are unique (all their duplicates have been removed).
%
% No specific order is respected in the returned list.
%
% Ex: if L = [1,2,3,2,2,4,5,5,4,6,6,5], then basic_utils:uniquify(L) is:
% [3,6,2,5,1,4].
%
-spec uniquify( list() ) -> list().
uniquify( List ) ->
	% There is probably a more efficient way of doing the same:
	sets:to_list( sets:from_list( List ) ).



% Tells whether there are in the specified list elements that are present more
% than once.
%
has_duplicates( List ) ->
	length( uniquify( List ) ) =/= length( List ).



% Returns the duplicates in the specified list: returns an (unordered) list of {
% DuplicatedTerm, DuplicationCount } pairs, where each duplicated term (i.e. a
% term present more than once) is specified, alongside the total number of
% occurrences of that terms in the specified list.
%
% Ex: list_utils:get_duplicates([a,a,b,b,b,c,d,d]) = [{b,3},{d,2},{a,2}]
%
-spec get_duplicates( [ term() ] ) -> [ { term(), basic_utils:count() } ].
get_duplicates( List ) ->
	get_duplicates( List, _Acc=[] ).

get_duplicates( _List=[], Acc ) ->
	Acc;

get_duplicates( _List=[ Term | T ], Acc ) ->

	% trace_utils:debug_fmt( "Inquiring about term '~p' into ~p.",
	% [ Term, T ] ),

	case count_and_filter_term( Term, _InitialList=T, _FilteredList=[],
								_InitialCount=0 ) of

		not_found ->
			% No a duplicated element, just iterating on the next term:
			get_duplicates( T, Acc );

		{ TermCount, FilteredList } ->
			% We already extracted the first Term:
			get_duplicates( FilteredList, [ { Term, TermCount + 1 } | Acc ] )

   end.



count_and_filter_term( _Term, _List=[], _FilteredList, _CurrentCount=0 ) ->
	not_found;

count_and_filter_term( _Term, _List=[], FilteredList, CurrentCount ) ->
	{ CurrentCount, FilteredList };

% Term found:
count_and_filter_term( Term, _List=[ Term | H ], FilteredList, CurrentCount ) ->
	count_and_filter_term( Term, H, FilteredList, CurrentCount + 1 );

% Other term:
count_and_filter_term( Term, _List=[ OtherTerm | H ], FilteredList,
					   CurrentCount ) ->
	count_and_filter_term( Term, H, [ OtherTerm | FilteredList ],
						   CurrentCount ).



% Returns the union of the two specified lists, i.e. the list of all elements
% that are in either list.
%
-spec union( list(), list() ) -> list().
union( L1, L2 ) ->
	%uniquify( L1 ++ L2 ).
	set_utils:to_list( set_utils:union( set_utils:from_list( L1 ),
										set_utils:from_list( L2 ) ) ).



% Returns the intersection of the two specified lists, i.e. the list of all
% elements that are in both lists.
%
% See also: subtract_all_duplicates/2.
%
-spec intersection( list(), list() ) -> list().
intersection( L1, L2 ) ->
	%set_utils:to_list( set_utils:intersection( set_utils:from_list( L1 ),
	%											set_utils:from_list( L2 ) ) ).
	lists:filter( fun( E ) -> lists:member( E, L2 ) end, L1 ).



% Returns the cartesian product of the specified lists (collected in a top-level
% list).
%
% Ex: cartesian_product( [ [a,b,c], [d,e], [f] ] ) =
% [ [a,d,f], [a,e,f], [b,d,f], [b,e,f], [c,d,f], [c,e,f] ]
%
-spec cartesian_product( [ [ T ] ] ) -> [ [ T ] ].
cartesian_product( [ SingleList ] ) ->
	[ [ E ] || E <- SingleList ];

cartesian_product( [ List | OtherLists ] ) ->
	[ [ E | SubList ] || E <- List,
						 SubList <- cartesian_product( OtherLists ) ].



% Returns a list equal to L1 except that all elements found in L2 have been
% removed, even if in L1 they were duplicated.
%
% Note: like lists:subtract, except that *all* occurences from L2 in L1 (not
% only the first one) are removed.
%
% Example: [1,4] = basic_utils:subtract_all_duplicates( [1,2,3,4,2], [2,3] )
%
% Taken from
% http://www.trapexit.org/Finding_Elements_in_One_Array_but_Not_Another
%
-spec subtract_all_duplicates( list(), list() ) -> list().
subtract_all_duplicates( L1, L2 ) ->
	lists:filter( fun( E ) -> not lists:member( E, L2 ) end, L1 ).



% Returns a copy of the specified list where the first element matching Elem is
% deleted, ensuring at least one of these elements exists (as opposed to
% lists:delete/2). The order of the specified list is preserved.
%
-spec delete_existing( term(), list() ) -> list().
delete_existing( Elem, List ) ->

	% We keep a copy of the original list to be able to generate better error
	% messages:
	%
	delete_existing( Elem, List, _OriginalList=List, _Acc=[] ).


delete_existing( Elem, _List=[], OriginalList, _Acc ) ->
	throw( { element_to_delete_not_found, Elem, OriginalList } );

delete_existing( Elem, _List=[ Elem | T ], _OriginalList, Acc ) ->
	% The first element found stops the iteration:
	lists:reverse( Acc ) ++ T;

delete_existing( Elem, _List=[ H | T ], OriginalList, Acc ) ->
	delete_existing( Elem, T, OriginalList, [ H | Acc ] ).



% Deletes the first matching of specified element from specified list, returning
% whether an element has been removed: either the 'not_found' atom (in which
% case the list remained the same) or the corresponding new list (same order and
% content, except first occurrence removed).
%
% Note: allows to perform only one traversal of the list (compared for example
% to a lists:member/2 then a lists:delete/2).
%
-spec delete_if_existing( term(), list() ) -> 'not_found' | list().
delete_if_existing( Elem, List ) ->
	delete_if_existing( Elem, List, _Acc=[] ).


delete_if_existing( _Elem, _List=[], _Acc ) ->
	not_found;

delete_if_existing( Elem, _List=[ Elem | T ], Acc ) ->
	lists:reverse( Acc) ++ T;

delete_if_existing( Elem, _List=[ H | T ], Acc ) ->
	delete_if_existing( Elem, T, [ H | Acc ] ).



% Returns a copy of the specified list where all elements matching Elem are
% deleted, whether or not there is any.
%
% The element order of the specified list is preserved.
%
-spec delete_all_in( term(), list() ) -> list().
delete_all_in( Elem, List ) ->
	delete_all_in( Elem, List, _Acc=[] ).


delete_all_in( _Elem, _List=[], Acc ) ->
	lists:reverse( Acc );

delete_all_in( Elem, _List=[ Elem | T ], Acc ) ->
	% An element not to retain:
	delete_all_in( Elem, T, Acc );

delete_all_in( Elem, _List=[ H | T ], Acc ) ->
	% Non-matching, keep it:
	delete_all_in( Elem, T, [ H | Acc ] ).




% Appends specified element at the end of specified list, without changing the
% order of the list.
%
% Ex: append_at_end( d, [a,b,c] ) returns [a,b,c,d].
%
% Note: usually adding elements at the end of a list should be avoided, as it is
% costlier than adding them at head.
%
-spec append_at_end( list() | any(), list() ) -> nonempty_list().
append_at_end( ElemList, L ) when is_list( ElemList ) andalso is_list( L ) ->
	% Should be more efficient than:
	%lists:reverse( [ Elem | lists:reverse( L ) ] ):
	L ++ ElemList;

append_at_end( Elem, L ) when is_list( L ) ->
	% Should be more efficient than:
	%lists:reverse( [ Elem | lists:reverse( L ) ] ):
	L ++ [ Elem ].



% Returns whether the specified list contains only integers.
-spec is_list_of_integers( any() ) -> boolean().
is_list_of_integers( [] ) ->
	true;

is_list_of_integers( [ H | T ] ) when is_integer( H ) ->
	is_list_of_integers( T );

is_list_of_integers( _ ) ->
	false.



% Compares the two specified lists with no regard to the order of their
% elements: returns true iff they have the exact same elements (differentiating
% between 1 and 1.0 for example), possibly in a different order.
%
-spec unordered_compare( list(), list() ) -> boolean().
unordered_compare( L1, L2 ) ->
	lists:sort( L1 ) =:= lists:sort( L2 ).



% Determines tuple-related information about specified datastructure: returns {
% TupleCount, TupleSize }, supposing the list is made of tuples of uniform
% sizes.
%
-spec determine_tuple_info( [ tuple() ] ) ->
								  { basic_utils:count(), basic_utils:count() }.
determine_tuple_info( TupleList ) when is_list( TupleList ) ->

	case length( TupleList ) of

		0 ->
			throw( empty_list );

		L ->
			FirstTuple = hd( TupleList ),
			TupleSize = size( FirstTuple ),
			check_tuple_length( TupleList, TupleSize ),
			{ L, TupleSize }

	end.


% Helper.
check_tuple_length( _TupleList=[], _TupleSize ) ->
	ok;

check_tuple_length( _TupleList=[ Tuple | T ], TupleSize ) ->

	case size( Tuple ) of

		TupleSize ->
			check_tuple_length( T, TupleSize );

		OtherSize ->
			throw( { heterogeneous_tuple_size, { Tuple, OtherSize },
					 { expected, TupleSize } } )

	end.



% Flattens a list of tuples into a simple list of their elements, without
% tuples and in the same order.
%
% Ex: flatten_tuples( [ { 1, 2, 3 }, { 4, 5, 6 } ] ) = [ 1, 2, 3, 4, 5, 6 ] )
%
-spec flatten_tuples( [ tuple() ] ) -> list().
flatten_tuples( List ) ->
	flatten_tuples( List, _Acc=[] ).


flatten_tuples( _List=[], Acc ) ->
	lists:reverse( Acc );

flatten_tuples( [ H | T ], Acc ) ->
	NewAcc = lists:reverse( tuple_to_list( H ) ) ++ Acc,
	flatten_tuples( T, NewAcc ).



% Reconstructs a list of tuples of specified size from the specified flat list.
%
% Ex: reconstruct_tuples( [ 1, 2, 3, 4, 5, 6 ], 3 ) =
%                                     [ { 1, 2, 3 }, { 4, 5, 6 } ]
%
-spec reconstruct_tuples( list(), basic_utils:count() ) -> [ tuple() ].
reconstruct_tuples( List, _TupleSize=1 ) ->
	% Atomic elements do not need to be wrapped in a single-element tuple:
	List;

reconstruct_tuples( List, TupleSize ) ->
	reconstruct_tuples( List, TupleSize, _Acc=[] ).


reconstruct_tuples( _List=[], _TupleSize, Acc ) ->
	lists:reverse( Acc );

reconstruct_tuples( List, TupleSize, Acc ) ->
	{ TupleAsList, T } = lists:split( _Count=TupleSize, List ),
	reconstruct_tuples( T, TupleSize, [ list_to_tuple( TupleAsList ) | Acc ] ).



% Section to perform random operations on lists.



% Returns a random uniform permutation of the specified list.
%
% Inspired from http://paste.lisp.org/display/74804.
%
% All these algorithms would need random access to a list, which is not readily
% possible here, hence must be emulated.
%
% See also the 'Speedy unsort:shuffle/1,2' thread in the erlang-questions
% mailing list for counterparts.
%
-spec random_permute( list() ) -> list().
random_permute( List ) ->
	random_permute( List, length( List ) ).


random_permute( _List, _RemainingLen=0 ) ->
	[];

random_permute( List, RemainingLen ) ->

	% Checking is commented-out:
	%RemainingLen = length( List ),

	% (using remove_element_at/2 should be quicker than using
	% proplists:delete/2, as we stop at the first matching element found)
	%
	Index = random_utils:get_random_value( RemainingLen ),

	%io:format( "Index=~p, ", [ Index ] ),

	% We put the drawn element at head, and recurse in the remaining list:
	[ get_element_at( List, Index )
		| random_permute( remove_element_at( List, Index ),
						  RemainingLen-1 ) ].



% Returns a reciprocal random uniform permutation of the specified list,
% compared to random_permute/1.
%
% Consists on the reciprocal operation, so that, if starting from a random state
% S (see set_random_state/1) and if L2 = random_permute( L1 ), then, if starting
% again from S, L1 = random_permute_reciprocal( L2 ).
%
-spec random_permute_reciprocal( list() ) -> list().
random_permute_reciprocal( List ) ->

	% This is a little trickier than random_permute/1; we have to reverse
	% operations for latest to first, hence we must start with the latest drawn
	% value. So we draw them all first, and start by the end of that list,
	% taking into account that the range is decremented at each draw:
	%
	ReciprocalIndex = lists:reverse( [ random_utils:get_random_value( L )
			|| L <- lists:reverse( lists:seq( 1, length( List ) ) ) ] ),

	%io:format( "Reciprocal index = ~p~n", [ ReciprocalIndex ] ),

	random_permute_reciprocal( lists:reverse( List ), ReciprocalIndex,
							   _Acc=[] ).


random_permute_reciprocal( _List=[], _ReciprocalIndex=[], Acc ) ->
	Acc;

random_permute_reciprocal( _List=[ H | T ], _ReciprocalIndex=[ I | Is ],
						   Acc ) ->

	NewAcc = insert_element_at( _Elem=H, Acc, _Index=I ),

	random_permute_reciprocal( T, Is, NewAcc ).



% Draws one element at random of the specified list, knowing they all have the
% same probability of being drawn (uniform probability).
%
-spec draw_element( [ any() ] ) -> any().
draw_element( _ElementList=[] ) ->
	throw( cannot_draw_from_empty_list );

draw_element( ElementList ) ->
	Len = length( ElementList ),
	draw_element( ElementList, Len ).



% Draws one element at random of the specified list, whose length must be the
% specified one (allows to precompute it once for multiple drawings), knowing
% all elements have the same probability of being drawn (uniform probability).
%
-spec draw_element( [ any() ], basic_utils:count() ) -> any().
draw_element( ElementList, Length ) ->

	DrawnIndex = random_utils:get_random_value( Length ),

	get_element_at( ElementList, DrawnIndex ).

	% Alternate, less efficient, implementation:

	% Same probability:
	%UniformList = [ { Elem, 1 } || Elem <- ElementList ],
	%draw_element_weighted( UniformList ).



% Draws one element at random of the specified list, which is a list of {
% Element, Probability } pairs: returns the drawn element, knowing that it will
% be chosen according to the probability associated to each element.
%
% Probabilities are managed as integer values defined relatively to each other
% (and they do not have to sum up to 1.0); they must be positive or null
% integers, and their sum must not be null.
%
% Ex: ElementList = [ {first,1}, {second,2}, {third,1} ] is excepted to return
% on average 'second' twice as frequently as 'first' or 'third'.
%
% Using [ {first,1}, {second,0}, {third,1} ] instead would mean that 'second'
% would never be drawn.
%
-spec draw_element_weighted( [ { any(), integer() } ] ) -> any().
draw_element_weighted( ElementList ) ->
	draw_element_weighted( ElementList, sum_probabilities( ElementList ) ).


-spec sum_probabilities( [ { _Element, number() } ] ) -> number().
sum_probabilities( ElementList ) ->
	sum_probabilities( ElementList, _Acc=0 ).


sum_probabilities( _ElementList=[], Acc ) ->
	Acc;

sum_probabilities( _ElementList=[ { _Element, Probability } | T ], Acc ) ->
	sum_probabilities( T, Acc + Probability ).



% Sum must be equal to the sum of all probabilities in ElementList.
draw_element_weighted( _ElementList, 0 ) ->
	throw( null_total_probability );

draw_element_weighted( ElementList, Sum ) ->
	DrawnValue = random_utils:get_random_value( Sum ),
	%io:format( "draw_element: drawn ~B.~n", [ DrawnValue ] ),
	select_element( ElementList, DrawnValue, _CurrentSum=0 ).



select_element( [ { Element, Probability } | _T ], DrawnValue, CurrentSum )
		when Probability + CurrentSum >= DrawnValue ->
	% Just gone past the probability range:
	Element;

select_element( [ { _Element, Probability } | T ], DrawnValue, CurrentSum ) ->
	% Drawn value still not reached, continuing:
	select_element( T, DrawnValue, CurrentSum + Probability ).



% Draws the specified number of elements at random of the specified list,
% knowing they all have the same probability of being drawn initially, but when
% an element is drawn, it is removed from the candidate list so that the next
% drawing operates on the resulting shorten list.
%
-spec draw_elements_from( [ any() ], basic_utils:count() ) -> [ any() ].
draw_elements_from( ElementList, Count ) ->
	draw_elements_from( ElementList, Count, _Acc=[] ).


draw_elements_from( _ElementList, _Count=0, Acc ) ->
	Acc;

draw_elements_from( ElementList, Count, Acc ) ->
	Drawn = draw_element( ElementList ),
	ShortenList = lists:delete( Drawn, ElementList ),
	draw_elements_from( ShortenList, Count - 1, [ Drawn | Acc ] ).
