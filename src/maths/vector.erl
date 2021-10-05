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


% @doc Module implementing the support for vectors of <b>arbitrary
% dimension</b>.
%
% See also:
% - the corresponding arbitrary-dimensioned points (in point.erl) and matrices
% (in matrix.erl)
% - the specialised vectors, such as vector{2,3,4}.erl
%
-module(vector).


% Relatively aggressive inlining for basic operations:
-compile( inline ).
-compile( { inline_size, 48 } ).


% Implementation notes:
%
% No dependent types, not able to declare a vector(D) type.


% For printout_*:
-include("linear.hrl").


-type user_vector() :: [ any_coordinate() ].
% A user-specified vector, as a list (hence not a tuple) with integer or
% floating-point coordinates.



-type vector() :: [ coordinate() ].
% An (internal) vector of arbitrary dimension (with floating-point coordinates).


-type integer_vector() :: [ integer_coordinate() ].
% A vector of arbitrary dimension, with integer coordinates (ex: on-screen
% ones).


-type any_vector() :: vector() | integer_vector().
% A vector of any dimension, with any numerical coordinates.


-type specialised_vector() :: linear_2D:vector2()
							| linear_3D:vector3()
							| linear_4D:vector4().
% A specialised vector that is of one of the specifically supported dimensions.


-export_type([ user_vector/0, vector/0, integer_vector/0, any_vector/0,
			   specialised_vector/0 ]).


-export([ new/1, null/1, from_point/1, to_point/1,
		  to_string/1, to_short_string/1, to_basic_string/1,
		  to_user_string/1 ] ).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type dimension() :: linear:dimension().
-type coordinate() :: linear:coordinate().
-type integer_coordinate() :: linear:integer_coordinate().
-type any_coordinate() :: linear:any_coordinate().

-type point() :: linear:point().
-type any_point() :: linear:any_point().



% @doc Returns an (arbitrary) vector corresponding to the user-specified one.
-spec new( user_vector() ) -> vector().
%new( UserVector ) when is_tuple( UserVector ) ->
%	new( tuple_to_list( UserVector ) );
% Throws bad_generator anyway if a tuple:
new( UserVector ) -> %when is_list( UserVector ) ->
	[ type_utils:ensure_float( UC ) || UC <- UserVector ].



% @doc Returns an (arbitrary) vector of specified dimension whose coordinates
% are all null.
%
-spec null( dimension() ) -> vector().
null( Dim ) ->
	lists:duplicate( Dim, 0.0 ).



% @doc Returns an (arbitrary) vector corresponding to the specified point.
-spec from_point( any_point() ) -> vector().
from_point( P ) ->
	[ type_utils:ensure_float( C ) || C <- tuple_to_list( P ) ].


% @doc Returns an (arbitrary, and with floating-point coordinates) point
% corresponding to the specified vector.
%
-spec to_point( vector() ) -> point().
to_point( V ) ->
	point:from_vector( V ).



% @doc Returns a textual representation of the specified vector.
-spec to_string( vector() ) -> ustring().
to_string( Vector ) ->
	to_user_string( Vector ).



% @doc Returns a short, textual, informal representation of the specified
% vector.
%
-spec to_short_string( vector() ) -> ustring().
to_short_string( Vector ) ->

	%Ws = [ "~w" || _ <- Vector ],
	%FormatStr = "[ " ++ text_utils:join( _Sep=", ", Ws ) ++ " ]",
	%text_utils:format( FormatStr, Vector ).

	text_utils:format( "~w", [ Vector ] ).



% @doc Returns a basic, not even fixed-width for floating-vector coordinates
% (see linear.hrl for width and precision) representation of the specified
% vector.
%
-spec to_basic_string( any_vector() ) -> ustring().
to_basic_string( Vector ) ->

	% Vectors supposed to be lists of floats:
	ElemFormatStr = "[ " ++ ?coord_float_format ++ " ]~n",

	FormatStr = text_utils:duplicate( length( Vector ), ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; CoordList: ~w.",
	%                       [ FormatStr, CoordList ] ),

	text_utils:format( FormatStr, Vector ).



% @doc Returns a textual, more user-friendly representation of the specified
% vector.
%
% This is the recommended representation.
%
-spec to_user_string( vector() ) -> ustring().
to_user_string( Vector ) ->

	Strs = linear:coords_to_best_width_strings( Vector ),

	% No need for ~ts here:
	ElemFormatStr = "[ ~s ]~n",

	FormatStr = text_utils:duplicate( length( Vector ), ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; Strs: ~p.",
	%                       [ FormatStr, Strs ] ),

	text_utils:format( FormatStr, Strs ).
