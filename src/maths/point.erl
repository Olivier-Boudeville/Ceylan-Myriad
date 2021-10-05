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
% Creation date: Sunday, October 3, 2021.


% @doc Module implementing the support for points of <b>arbitrary dimension</b>.
%
% See also:
% - the corresponding arbitrary-dimensioned vectors (in vector.erl) and matrices
% (in matrix.erl)
% - the specialised points, such as points{2,3,4}.erl
%
-module(point).


% Relatively aggressive inlining for basic operations:
-compile( inline ).
-compile( { inline_size, 48 } ).


% Implementation notes:
%
% No dependent types, not able to declare a point(D) type.
%
% We call a container type-homogenous if all the coordinates that it gathers are
% all either integer or floating-point ones.


% For printout_*:
-include("linear.hrl").



-type user_point() :: any_point() | [ any_coordinate() ].
% A user-specified point, preferably as a tuple, otherwise as a list (hence as a
% vector), with integer or floating-point coordinates.


%-type point() :: tuple( coordinate() ).
-type point() :: tuple().
% A point in a space of arbitrary dimension, with floating-point coordinates.


%-type integer_point() :: tuple( integer_coordinate() ).
-type integer_point() :: tuple().
% A point of any dimension, with integer coordinates (ex: on-screen ones).


-type any_point() :: point() | integer_point().
% A point of any dimension, with any numerical coordinates.


-type specialised_point() :: linear_2D:point2()
						   | linear_3D:point3()
						   | linear_4D:point4().
% A specialised point that is of one of the specifically supported dimensions.


-export_type([ user_point/0, point/0, integer_point/0, any_point/0,
			   specialised_point/0 ]).


-export([ new/1, null/1, from_vector/1, to_vector/1, to_any_vector/1,
		  to_string/1, to_short_string/1, to_fixed_width_string/1,
		  to_user_string/1 ] ).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type dimension() :: linear:dimension().
%-type coordinate() :: linear:coordinate().
%-type integer_coordinate() :: linear:integer_coordinate().
-type any_coordinate() :: linear:any_coordinate().

-type vector() :: vector:vector().
-type any_vector() :: vector:any_vector().



% @doc Returns an (arbitrary) point corresponding to the user-specified one
% (preferably a tuple rather than a list).
%
% We do not check whether all coordinates are either integer or floating-point
% ones.
%
-spec new( user_point() ) -> any_point().
new( UserPoint ) when is_list( UserPoint ) ->
	list_to_tuple( UserPoint );

new( UserPoint ) when is_tuple( UserPoint ) ->
	UserPoint.



% @doc Returns an (arbitrary) point of specified dimension whose coordinates
% are all null.
%
-spec null( dimension() ) -> point().
null( Dim ) ->
	list_to_tuple( lists:duplicate( Dim, 0.0 ) ).



% @doc Returns an (arbitrary) point corresponding to the specified vector
% (expected to be type-homogeneous).
%
-spec from_vector( any_vector() ) -> any_point().
from_vector( V ) ->
	list_to_tuple( V ).



% @doc Returns an (arbitrary, and with floating-vector coordinates) vector
% corresponding to the specified point.
%
-spec to_vector( any_point() ) -> vector().
to_vector( P ) ->
	vector:from_point( P ).


% @doc Returns an (arbitrary, and with coordinates of the same type) vector
% corresponding to the specified point.
%
-spec to_any_vector( any_point() ) -> any_vector().
to_any_vector( P ) ->
	tuple_to_list( P ).



% @doc Returns a textual representation of the specified point.
-spec to_string( any_point() ) -> ustring().
to_string( Point ) ->
	to_short_string( Point ).



% @doc Returns a short, textual, informal representation of the specified
% point.
%
-spec to_short_string( any_point() ) -> ustring().
to_short_string( Point ) ->
	text_utils:format( "~w", [ Point ] ).



% @doc Returns a fixed-width (see linear.hrl for width and precision)
% representation of the specified point.
%
-spec to_fixed_width_string( any_point() ) -> ustring().
to_fixed_width_string( Point ) ->

	CoordList = tuple_to_list( Point ),

	% Points supposed to be homogeneous tuples:
	CoordFmt = case is_integer( hd( CoordList ) ) of

		true ->
			?coord_integer_format;

		false ->
			?coord_float_format

	end,

	ElemFormatStr = "{" ++ CoordFmt ++ " }~n",

	FormatStr = text_utils:duplicate( length( CoordList ), ElemFormatStr ),

	trace_utils:debug_fmt( "FormatStr: ~ts; CoordList: ~w.",
						   [ FormatStr, CoordList ] ),

	text_utils:format( FormatStr, CoordList ).



% @doc Returns a textual, more user-friendly representation of the specified
% point.
%
-spec to_user_string( any_point() ) -> ustring().
to_user_string( Point ) ->

	CoordList = tuple_to_list( Point ),

	Strs = linear:coords_to_best_width_strings( CoordList ),

	% No need for ~ts here:
	ElemFormatStr = "{ ~s }~n",

	FormatStr = text_utils:duplicate( length( CoordList ), ElemFormatStr ),

	trace_utils:debug_fmt( "FormatStr: ~ts; Strs: ~p.",
						   [ FormatStr, Strs ] ),

	text_utils:format( FormatStr, Strs ).
