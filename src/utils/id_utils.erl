% Copyright (C) 2007-2018 Olivier Boudeville
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
% Creation date: Sunday, December 24, 2017.



% Management of various identifiers.
%
% See id_utils_test.erl for the corresponding test.
%
-module(id_utils).


% UUID section.


% A string UUID (ex: "ed64ffd4-74ee-43dc-adba-be37ed8735aa"):
-type uuid() :: string().

-export_type([ uuid/0 ]).


-export([ generate_uuid/0 ]).



% Sortable identifier section.


% No legit sortable identifier can be smaller than that one:
%
-define( lower_bound_id, [ 0 ] ).


% No legit sortable identifier can be higher than that one (in Erlang term
% order, lists are dominated (only) by bitstrings):
%
-define( upper_bound_id, <<"Max">> ).


% Corresponds to smart (sortable, insertion-friendly) identifiers.
%
% Sometimes identifiers that can be sorted and that allow introducing any number
% of new identifiers between any two successive ones are needed.
%
% We use non-empty lists of non-negative integers for that, whose last integer
% must be strictly positive (so that lower values can always be introduced).
%
% The Erlang default ordering for this datatype corresponds to this need.
%
% For example, if having defined two identifiers [7,2] and [7,3], we can
% introduce two identifiers between them, typically [7,2,1] and [7,2,2], since
% the Erlang term ordering tells us that [7,2] < [7,2,1] < [7,2,2] < [7,3].
%
% As a result, no need to define specific comparison operators, '=:=', '<' and
% '>', and thus 'lists:sort/1', 'lists:keysort/2' are already adequate for that.
%
% Example: lists:sort( [ [7,3], [7,2,1], [7,2,2], [7,2] ] ) =
%   [ [7,2], [7,2,1], [7,2,2], [7,3] ].
%
% The maximum lower bound is conventionally chosen to be [0] - which is not a
% valid sortable identifier (as it terminates with zero).
%
% Designed to be efficiently compared/ordered, at the cost of more expensive
% insertions.
%
% An additional goal is to generate identifiers that remain as short as
% possible.
%
% Not accepted: -type sortable_id() :: [ non_neg_integer() ]
%                                    | ?lower_bound_id | ?upper_bound_id.
% To include bounds:
-type sortable_id() :: [ integer() ] | bitstring().


-export_type([ sortable_id/0 ]).


-export([ get_initial_sortable_id/0, get_next_sortable_id/1,
		  get_sortable_id_between/2,
		  get_sortable_id_lower_bound/0, get_sortable_id_upper_bound/0,
		  check_sortable_id/1, get_successor_sortable_id/1,
		  get_higher_same_depth_sortable_id/1,
		  get_higher_next_depth_sortable_id/1,
		  sortable_id_to_string/1 ]).





% UUID section.


% Returns a string containing a new universally unique identifier (UUID), based
% on a random source and/or the system clock plus the system's ethernet hardware
% address, if present.
%
-spec generate_uuid() -> uuid().
generate_uuid() ->

	case executable_utils:lookup_executable( "uuidgen" ) of

		false ->
			trace_utils:warning( "No 'uuidgen' tool found on system, "
								 "defaulting to our failsafe implementation." ),
			uuidgen_internal();

		Exec ->

			% Random-based, rather than time-based (otherwise we end up
			% collecting a rather constant suffix):
			%
			case system_utils:run_executable( Exec ++ " -r" ) of

				{ _ExitCode=0, Res } ->
					Res;

				{ ExitCode, ErrorOutput } ->
					throw( { uuid_generation_failed, ExitCode, ErrorOutput } )

			end

	end.



% Quick and dirty replacement:
%
uuidgen_internal() ->

	% Using /dev/random instead would incur waiting of a few seconds that were
	% deemed too long for this use:
	%
	case system_utils:run_executable(
		   "/bin/dd if=/dev/urandom bs=1 count=32 2>/dev/null" ) of

		{ _ReturnCode=0, Output } ->
			% We translate these bytes into hexadecimal values:
			V = [ string:to_lower( hd(
				  io_lib:format( "~.16B", [ B rem 16 ] ) ) ) || B <- Output ],

			lists:flatten( io_lib:format(
							 "~s~s~s~s~s~s~s~s-~s~s~s~s-~s~s~s~s-~s~s~s~s-~s"
							 "~s~s~s~s~s~s~s~s~s~s~s", V ) );

		{ ErrorCode, ErrorOutput } ->
			throw( { uuidgen_internal_failed, ErrorCode, ErrorOutput } )

	end.



% Sortable identifier section.


% Returns a relevant, well-chosen initial low sortable identifier, yet not the
% lowest possible so that we can still introduce, if needed, an arbitrary number
% of identifiers lower than this one.
%
-spec get_initial_sortable_id() -> sortable_id().
get_initial_sortable_id() ->
	[ 1 ].


% Returns a relevant sortable identifier that is superior to the specified one.
%
-spec get_next_sortable_id( sortable_id() ) -> sortable_id().
get_next_sortable_id( Id ) ->
	get_higher_same_depth_sortable_id( Id ).



% Returns a sortable identifier that can be inserted between the two specified
% ones, which are presumably ordered.
%
% Ex: get_sortable_id_between( [1,1], [1,2] ) may return [1,1,1].
%
-spec get_sortable_id_between( sortable_id(), sortable_id() ) -> sortable_id().
get_sortable_id_between( ?upper_bound_id, _Id ) ->
	throw( cannot_exceed_upper_bound );

get_sortable_id_between( _Id, ?lower_bound_id ) ->
	throw( cannot_go_below_lower_bound );

get_sortable_id_between( ?lower_bound_id, _Id ) ->
	throw( { not_implemented, from_lower_bound } );

get_sortable_id_between( _Id, ?upper_bound_id ) ->
	throw( { not_implemented, to_upper_bound } );

get_sortable_id_between( Id, Id ) ->
	throw( { equal_sortable_identifiers, Id } );

get_sortable_id_between( LowerId, HigherId ) when LowerId > HigherId ->
	throw( { wrongly_ordered_sortable_identifiers, LowerId, HigherId } );

% Here LowerId < HigherId, and must be non-empty lists:
get_sortable_id_between( LowerId, HigherId ) ->
	get_sortable_id_between( LowerId, HigherId, _Acc=[] ).



% (helper)
%
% Here still in a common prefix:
get_sortable_id_between( _Lower=[ H | Tl ], _Higher=[ H | Th ],
						 Acc ) ->
	get_sortable_id_between( Tl, Th, [ H | Acc ] );


% In clauses below they differ for the first time:

% Here they are wrongly ordered:
get_sortable_id_between( Lower=[ Hl | _Tl ], Higher=[ Hh | _Th ], Acc )
  when Hl > Hh ->
	CommonPrefix = lists:reverse( Acc ),
	LowerId = CommonPrefix ++ Lower,
	HigherId = CommonPrefix ++ Higher,
	throw( { wrongly_ordered_sortable_identifiers, LowerId, HigherId } );


% Here by design Hl < Hh, and in the next clause we are at the end of Lower:
get_sortable_id_between( _Lower=[ Hl ], _Higher, Acc ) ->
	% By design higher than LowerId, lower than HigherId, and not the immediate
	% next element to LowerId:
	% (ex: if Lower = [1,4,5,8,2] and Higher = [1,4,5,8,3,...], returning:
	% [1,4,5,8,2,1]
	lists:reverse( [ 1, Hl | Acc ] );

% Here Hl < Hh, and we are not at the end of Lower:
get_sortable_id_between( _Lower=[ Hl, Hlnext | _Tl ], _Higher, Acc ) ->
	% Ex: if Lower = [1,4,5,8,2,X,...] and Higher = [1,4,5,8,3,...], returning:
	% [1,4,5,8,2,X+1] (which is at least as short as both)
	lists:reverse( [ Hlnext+1, Hl | Acc ] );

% Here we are not at the end of Lower, and Higher finishes with 0, abnormal
get_sortable_id_between( _LowerId=[], _HigherId=[ 0 ], Acc ) ->
	throw( { zero_ending_sortable_id, lists:reverse( [ 0 | Acc ] ) } );


% Here we are not at the end of Lower, and Higher has still digits:
get_sortable_id_between( _LowerId=[], _HigherId=[ 0 | T ], Acc ) ->
	% We cannot introduce an identifier, waiting for the first non-zero digits
	% (guaranteed to exist by design):
	%
	get_sortable_id_between( [], T, [ 0, Acc ] );


% H is non-zero below:

% H is 1 here, we introduce thus a 0, yet append a 1 to be non-zero terminated:
get_sortable_id_between( _LowerId=[], _HigherId=[ 1 | _T ], Acc ) ->
	% Thus by design still higher than LowerId, and lower than HigherId, and at
	% worse just one-digit longer than it:
	%
	lists:reverse( [ 1, 0 | Acc ] );

% H > 1 here, no need to go deeper:
get_sortable_id_between( _LowerId=[], _HigherId=[ H | _T ], Acc ) ->
	lists:reverse( [ H-1 | Acc ] );

get_sortable_id_between( _LowerId=[], _HigherId=[], Acc ) ->
	Id = lists:reverse( Acc ),
	throw( { equal_sortable_identifiers, Id } ).



% Returns the maximum lower bound of sortable identifiers - knowing that this
% value does not pertain to sortable identifiers.
%
-spec get_sortable_id_lower_bound() -> sortable_id().
get_sortable_id_lower_bound() ->
	?lower_bound_id.



% Returns the minimum upper bound of sortable identifiers - knowing that this
% value does not pertain to sortable identifiers.
%
-spec get_sortable_id_upper_bound() -> sortable_id().
get_sortable_id_upper_bound() ->
	?upper_bound_id.




% Checks that the specified sortable identifier is legit.
%
-spec check_sortable_id( sortable_id() ) -> basic_utils:void().
check_sortable_id( _Id=[] ) ->
	throw( { invalid_sortable_identifier, empty_list } );

check_sortable_id( Id ) when is_list( Id ) ->
	check_only_non_neg_integers( Id, Id );

check_sortable_id( Id ) ->
	throw( { non_list_sortable_identifier, Id } ).



% (helper)
check_only_non_neg_integers( [], _Id ) ->
	ok;

check_only_non_neg_integers( [ H | T ], Id )
  when is_integer( H ) andalso H >= 0 ->
	check_only_non_neg_integers( T, Id );

check_only_non_neg_integers( _, Id ) ->
	throw( { invalid_sortable_identifier, Id } ).




% Returns the immediate successor sortable identifier of the specified one,
% i.e. the one that is immediately superior to it.
%
% Note: generally *not* to be used, as by design no sortable identifier can be
% inserted between these two - which defeats the purpose of this datatype.
%
% Hence: mostly defined for reference purpose.
%
-spec get_successor_sortable_id( sortable_id() ) -> sortable_id().
get_successor_sortable_id( SortId ) ->
	Reversed = lists:reverse( SortId ),
	lists:reverse( [ 0 | Reversed ] ).



% Returns a sortable identifier higher than (i.e. coming after) the specified
% one, yet not finer (i.e. at the same depth).
%
% For example, if [1,4,2] is specified, then [1,4,3] is returned (rather than,
% say, [1,4,2,1]).
%
-spec get_higher_same_depth_sortable_id( sortable_id() ) -> sortable_id().
get_higher_same_depth_sortable_id( SortId ) ->

	% Ex: we have SortId = [1,4,2]; we want to return [1,4,3] (rather
	% than, say, [1,4,2,1]):

	% Ex: Last is 2, RevOthers is [4,1]
	[ Last | RevOthers ] = lists:reverse( SortId ),

	lists:reverse( [ Last+1 | RevOthers ] ).



% Returns a sortable identifier higher than (i.e. coming after) the specified
% one, at a next depth.
%
% For example, if [1,4,2] is specified, then [1,4,2,1] is returned (rather than,
% say, [1,4,3]).
%
-spec get_higher_next_depth_sortable_id( sortable_id() ) -> sortable_id().
get_higher_next_depth_sortable_id( SortId ) ->

	% Ex: we have SortId = [1,4,2]; we want to return [1,4,2,1] (rather
	% than, say, [1,4,3]):

	Reversed = lists:reverse( SortId ),

	% We add 1 as finer coordinate, not 0, so that we can always perform an
	% insertion *before* a sortable identifier that we produce that way.
	%
	% Ex: otherwise, no identifier could be inserted between [0] and [0,0]; thus
	% the "basic next identifier" of [0] is [0,1] instead, and we can still
	% insert for example [0,0,1] between them.
	%
	lists:reverse( [ 1 | Reversed ] ).



% Returns a textual representation of specified sortable identifier.
%
-spec sortable_id_to_string( sortable_id() ) -> text_utils:string().
sortable_id_to_string( _Id=?lower_bound_id ) ->
	"sortable identifier lower bound";

sortable_id_to_string( _Id=?upper_bound_id ) ->
	"sortable identifier upper bound";

sortable_id_to_string( Id ) ->
	% Better represented as tuple:
	text_utils:format( "~w", [ list_to_tuple( Id ) ] ).
