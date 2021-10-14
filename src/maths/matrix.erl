% Copyright (C) 2021-2021 Olivier Boudeville
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
% Creation date: Sunday, September 26, 2021.


% @doc Module implementing the support for matrices of <b>arbitrary
% dimensions</b>.
%
% See also:
% - the corresponding arbitrary vectors, in vector.erl
% - the specialised matrices, such as matrix{2,3,4}.erl
%
-module(matrix).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).



% Implementation notes:
%
% No dependent types, not able to declare a matrix(M,N) type.
%
% A matrix having M rows and N columns (indices starting at 1) is usually
% iterated through based on variables named R (thus in [1..M]) and C (in
% [1..N]).



-type user_row() :: user_vector().
% Matrix user-specified elements, left to right.

-type row() :: vector().
% Matrix elements, left to right.


-type column() :: vector().
% Matrix elements, top to bottom.


-type user_matrix() :: [ user_row() ].
% A user-specified matrix, as a list a rows comprising integer or floating-point
% coordinates.


-type matrix() :: [ row() ].
% A matrix of arbitrary dimensions (at least one row), stored in row-major
% order (all rows shall contain the same number of elements).


-type specialised_matrix() :: matrix2:matrix()
							| matrix3:matrix()
							| matrix4:matrix().
% Regroups all types of specialised matrices.


-type dimensions() :: { RowCount:: dimension(), ColumnCount :: dimension() }.
% Number of rows and columns of a matrix.


-export_type([ user_row/0, row/0, column/0,
			   user_matrix/0, matrix/0, specialised_matrix/0,
			   dimensions/0 ]).


-export([ new/1, null/1, null/2,
		  identity/1,
		  dimensions/1, row/2, column/2, get_element/3, set_element/4,
		  transpose/1,
		  add/2, mult/2,
		  get_specialised_module_of/1, get_specialised_module_for/1,
		  specialise/1, unspecialise/1,
		  check/1,
		  to_string/1, to_basic_string/1, to_user_string/1 ] ).


% Shorthands:

-type module_name() :: basic_utils:module_name().

-type ustring() :: text_utils:ustring().

-type coordinate() :: linear:coordinate().
-type dimension() :: linear:dimension().

-type vector() :: vector:vector().
-type user_vector() :: vector:user_vector().



% @doc Returns an (arbitrary) matrix corresponding to the user-specified one.
%
% No clause just based on a list of coordinates exists, as multiple combinations
% of dimensions could correspond.
%
-spec new( matrix() ) -> matrix().
new( UserMatrix ) ->

	M = [ vector:new( UR ) || UR <- UserMatrix ],

	cond_utils:if_defined( myriad_check_linear, check( M ), M ).



% @doc Returns a null, square (arbitrary) matrix of the specified dimension.
-spec null( dimension() ) -> matrix().
null( SquareDim ) ->
	null( SquareDim, SquareDim ).



% @doc Returns a null (arbitrary) matrix of the specified dimensions.
-spec null( dimension(), dimension() ) -> matrix().
null( RowCount, ColumnCount ) ->
	NullRow = lists:duplicate( ColumnCount, 0.0 ),
	lists:duplicate( RowCount, NullRow ).



% @doc Returns the identity matrix of the specified dimension.
-spec identity( dimension() ) -> matrix().
identity( Dim ) ->
	[ [ case R of C -> 1.0; _ -> 0.0 end
			|| C <- lists:seq( 1, Dim ) ] || R <- lists:seq( 1, Dim ) ].



% @doc Returns the dimensions of the specified matrix.
-spec dimensions( matrix() ) -> dimensions().
dimensions( M ) ->
	{ length( M ), length( hd( M ) ) }.



% @doc Returns the specified row of the specified matrix.
-spec row( dimension(), matrix() ) -> vector().
row( RowCount, Matrix ) ->
	lists:nth( RowCount, Matrix ).


% @doc Returns the specified column of the specified matrix.
-spec column( dimension(), matrix() ) -> vector().
column( ColCount, Matrix ) ->
	[ lists:nth( ColCount, R ) || R <- Matrix ].



% @doc Returns the element at specified row and column of the specified matrix.
-spec get_element( dimension(), dimension(), matrix() ) -> coordinate().
get_element( R, C, Matrix ) ->
	lists:nth( C, row( R, Matrix ) ).



% @doc Returns a matrix identical to the specified one except that its specified
% element at specied location has been set to the specified value.
%
-spec set_element( dimension(), dimension(), coordinate(), matrix() ) ->
									matrix().
set_element( R, C, Value, Matrix ) ->
	NewRow = list_utils:set_element_at( Value, row( R, Matrix ), _Index=C ),
	list_utils:set_element_at( NewRow, Matrix, R ).


% @doc Returns the transpose of the specified matrix.
%
% We proceed recursively, iterating in turn through all the elements of the
% first row (which will end up being the first column, i.e. each being the first
% element of the rows of the transpose matrix).
%
-spec transpose( matrix() ) -> matrix().
%transpose( _M=[ _FirstRow=[ FirstElem | OtherElems ] | OtherRows ] ) ->
%	[ [ E | || E <- FirstRow ].
transpose( M ) ->
	transpose( M, _AccTranspose=[] ).


% (helper)
%
% We proceed recursively, chopping all rows of one element (hence chopping a
% column as a whole), the resulting list being the row of the transpose.
%
% Here we exhausted all elements of the first row (and thus of all other rows as
% well)
%
transpose( _Rows=[ [] | _T ], AccTranspose ) ->
	% So that rows are enumerated in the right FIFO order:
	lists:reverse( AccTranspose );

transpose( NonExhaustedRows, AccTranspose ) ->
	{ TransposeRow, ChoppedRows } = extract_first_elements( NonExhaustedRows ),
	transpose( ChoppedRows, [ TransposeRow | AccTranspose ] ).



% Extracts the first element of each row, returning a pair made of all the
% extracted elements and of the shrunk rows: {ExtractedElements, ChoppedRows}.
%
% (helper)
extract_first_elements( Rows ) ->
	extract_first_elements( Rows, _AccElems=[], _AccRows=[] ).


extract_first_elements( _Rows=[], AccElems, AccRows ) ->
	{ lists:reverse( AccElems ), lists:reverse( AccRows ) };

extract_first_elements( _Rows=[ [ H | T ] | OtherRows ], AccElems, AccRows ) ->
	extract_first_elements( OtherRows, [ H | AccElems ], [ T | AccRows ] ).



% @doc Returns the sum of the two specified matrices, supposedly of the same
% dimensions.
%
-spec add( matrix(), matrix() ) -> matrix().
add( M1, M2 ) ->
	lists:zipwith( fun( R1, R2 ) ->
						vector:add( R1, R2 )
				   end,
				   M1, M2 ).



% @doc Returns the multiplication of the two specified matrices, supposedly of
% the right dimensions (the number of rows of one being equal to the number of
% columns of the other, and reciprocally).
%
-spec mult( matrix(), matrix() ) -> matrix().
mult( M1, M2 ) ->
	TranspM2 = transpose( M2 ),
	mult( M1, TranspM2, _AccRows=[] ).


% (helper)
mult( _M1=[], _M2, AccRows ) ->
	lists:reverse( AccRows );

mult( _M1=[ R1 | T1 ], TranspM2, AccRows ) ->
	MultRow = apply_columns( R1, TranspM2, _Acc=[] ),
	mult( T1, TranspM2, [ MultRow | AccRows ] ).


% Computes the dot products between the specified row and each column of the
% transposed matrix.
%
apply_columns( _R, _Columns=[], Acc ) ->
	lists:reverse( Acc );

apply_columns( R, _Columns=[ Col | T ], Acc ) ->
	Coord = vector:dot_product( R, Col ),
	apply_columns( R, T, [ Coord | Acc ] ).



% @doc Returns a specialised matrix corresponding to the specified arbitrary
% matrix.
%
-spec specialise( matrix() ) -> specialised_matrix().
specialise( M ) ->

	MatMod = get_specialised_module_for( M ),

	MatMod:from_matrix( M ).



% @doc Returns an arbitrary matrix corresponding to the specified specialised
% matrix.
%
-spec unspecialise( specialised_matrix() ) -> matrix().
unspecialise( M ) ->

	% Gets the record tag:
	MatMod = element( _RecordTagIndex=1, M ),

	MatMod:to_arbitrary( M ).



% @doc Returns the module corresponding to the specified specialised matrix.
-spec get_specialised_module_of( matrix() ) -> module_name().
get_specialised_module_of( M )  ->
	element( _RecordTagIndex=1, M ).



% @doc Returns the module corresponding to the specialised matrix version that
% would apply to specified (arbitrary) matrix.
%
-spec get_specialised_module_for( matrix() ) -> module_name().
% Determines row count:
get_specialised_module_for( M ) when length( M ) == 2 ->
   matrix2;

get_specialised_module_for( M ) when length( M ) == 2 ->
   matrix3;

get_specialised_module_for( M ) when length( M ) == 3 ->
   matrix4;

get_specialised_module_for( M ) ->
   throw( { unsupported_dimension, length( M ) } ).



% Determines row count:


% @doc Checks that the specified matrix is legit, and returns it.
-spec check( matrix() ) -> matrix().
check( M=[ FirstRow | OtherRows ] ) ->
	vector:check( FirstRow ),
	RowElemCount = length( FirstRow ),
	[ begin
			RowElemCount = length( R ),
			vector:check( R )
	  end || R <- OtherRows ],
	M.



% @doc Returns a textual representation of the specified (arbitrary) matrix;
% full float precision is shown.
%
-spec to_string( matrix() ) -> ustring().
to_string( Matrix ) ->
	to_user_string( Matrix ).


% @doc Returns a basic, not even fixed-width for floating-vector coordinates
% (see linear.hrl for width and precision) representation of the specified
% vector.
%
% Note: not a convincing representation, prefer the more expensive, truer
% to_user_string/1.
%
-spec to_basic_string( matrix() ) -> ustring().
to_basic_string( Matrix ) ->

	RowElemCount = length( hd( Matrix ) ),

	RowFormatStr = "[" ++
		text_utils:duplicate( RowElemCount, ?coord_float_format ) ++ " ]~n",

	%trace_utils:debug_fmt( "RowFormatStr = '~w'.", [ RowFormatStr ] ),

	RowCount = length( Matrix ),

	FormatStr = "~n" ++ text_utils:duplicate( RowCount, RowFormatStr ),

	Elems = list_utils:flatten_once( Matrix ),

	text_utils:format( FormatStr, Elems ).



% @doc Returns a textual, more user-friendly representation of the specified
% (arbitrary) matrix; full float precision is shown; all coordinates occupy the
% same space (the one with the longest representation).
%
% This is the recommended representation.
%
% Another version where minimal widths would be determined per-column.
%
-spec to_user_string( matrix() ) -> ustring().
to_user_string( Matrix ) ->

	%  Here we ensure that all coordinates use the same width:
	AllCoords = list_utils:flatten_once( Matrix ),

	Strs = linear:coords_to_best_width_strings( AllCoords ),

	RowLen = length( hd( Matrix ) ),

	% No need for ~ts here:
	RowFormatStr = "[ " ++ text_utils:duplicate( RowLen, "~s " ) ++ "]~n",

	RowCount = length( Matrix ),

	FormatStr = "~n" ++ text_utils:duplicate( RowCount, RowFormatStr ),

	text_utils:format( FormatStr, Strs ).
