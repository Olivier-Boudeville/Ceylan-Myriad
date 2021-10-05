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


% Relatively aggressive inlining for basic operations:
-compile( inline ).
-compile( { inline_size, 48 } ).


% Implementation notes:
%
% No dependent types, not able to declare a mat(M,N) type.
%

% For printout_*:
-include("linear.hrl").


-type row() :: vector().
% Matrix elements, left to right.

-type column() :: vector().
% Matrix elements, top to bottom.


-type matrix() :: [ row() ].
% A matrix of arbitrary dimensions (at least one row), stored in row-major
% order.


-type specialised_matrix() :: linear_2D:matrix()
							| linear_3D:matrix()
							| linear_4D:matrix().
% Regroups all types of specialised matrices.


-export_type([ row/0, column/0, matrix/0, specialised_matrix/0 ]).


-export([ new/1, null/1, null/2,
%identity/0,
		  get_specialised_module/1, specialise/1, unspecialise/1,
		  to_string/1 ] ).


% Shorthands:

-type ustring() :: text_utils:ustring().

%-type any_coordinate() :: linear:any_coordinate().
%-type coordinate() :: linear:coordinate().
-type dimension() :: linear:dimension().

-type vector() :: vector:vector().



% @doc Returns an (arbitrary) matrix corresponding to the user-specified one.
%
% No clause just based on a list of coordinates exists, as multiple combinations
% of dimensions could correspond.
%
-spec new( matrix() ) -> matrix().
new( UserMatrixVector ) ->

	% Just checking:
	cond_utils:if_defined( myriad_check_linear,
		begin
			[ FirstRow | OtherRows ] = UserMatrixVector,
			type_utils:check_floats( FirstRow ),
			RowElemCount = length( FirstRow ),
			[ begin
				RowElemCount = length( R ),
				type_utils:check_floats( R )
			  end || R <- OtherRows ]

		end ),

	UserMatrixVector.



% @doc Returns a null, square (arbitrary) matrix of the specified dimension.
-spec null( dimension() ) -> matrix().
null( SquareDim ) ->
	null( SquareDim, SquareDim ).



% @doc Returns a null (arbitrary) matrix of the specified dimensions.
-spec null( dimension(), dimension() ) -> matrix().
null( RowCount, ColumnCount ) ->
	NullRow = lists:duplicate( ColumnCount, 0.0 ),
	lists:duplicate( RowCount, NullRow ).



% @doc Returns a specialised matrix corresponding to the specified arbitrary
% matrix.
%
-spec specialise( matrix() ) -> specialised_matrix().
specialise( M ) ->

	MatMod = get_specialised_module( M ),

	MatMod:from_matrix( M ).



% @doc Returns an arbitrary matrix corresponding to the specified specialised
% matrix.
%
-spec unspecialise( specialised_matrix() ) -> matrix().
unspecialise( M ) ->

	% Gets the record tag:
	MatMod = case element( _RecordTagIndex=1, M ) of

		matrix2 ->
			linear_2D;

		matrix3 ->
			linear_3D;

		matrix4 ->
			linear_4D;

		Other ->
			throw( { unexpected_specialised_record, Other } )

	end,

	MatMod:to_arbitrary( M ).



% @doc Returns the module corresponding to the specialised matrix version that
% would apply to specified (arbitrary) matrix.
%
-spec get_specialised_module( matrix() ) -> basic_utils:module_name().
% Determines row count:
get_specialised_module( M ) when length( M ) == 2 ->
	linear_2D;

get_specialised_module( M ) when length( M ) == 2 ->
	linear_3D;

get_specialised_module( M ) when length( M ) == 3 ->
	linear_4D;

get_specialised_module( M ) ->
	throw( { unsupported_dimension, length( M ) } ).



% @doc Returns a textual representation of the specified (arbitrary) matrix.
-spec to_string( matrix() ) -> ustring().
to_string( M ) ->

	RowElemCount = length( hd( M ) ),

	ElemFormatStr = "~" ++ ?printout_width ++ "." ++ ?printout_precision
						  ++ ". f ",

	RowFormatStr = "[" ++ text_utils:duplicate( RowElemCount, ElemFormatStr )
						 ++ " ]~n",

	%trace_utils:debug_fmt( "RowFormatStr = '~w'.", [ RowFormatStr ] ),

	RowCount = length( M ),

	FormatStr = text_utils:duplicate( RowCount, RowFormatStr ),

	Elems = list_utils:flatten_once( M ),

	text_utils:format( FormatStr, Elems ).
