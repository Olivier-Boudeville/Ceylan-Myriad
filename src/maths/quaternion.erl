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
% Creation date: Friday, October 8, 2021.


% @doc Gathering of various facilities for <b>quaternion-related</b> operations.
-module(quaternion).


-type quaternion() :: { A :: coordinate(), B :: coordinate(),
						C :: coordinate(), D :: coordinate() }.
% Q = A + B.i + C.j + D.k, where i, j and k are the basic quaternions.


-export_type([ quaternion/0 ]).

-export([ null/0, new/4, to_string/1, to_compact_string/1, to_user_string/1 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type coordinate() :: linear:coordinate().
-type any_coordinate() :: linear:any_coordinate().




% @doc Returns the null quaternion.
-spec null() -> quaternion().
null() ->
	Zero = 0.0,
	{ Zero, Zero, Zero, Zero }.



% @doc Returns a corresponding (checked) quaternion.
-spec new( any_coordinate(), any_coordinate(), any_coordinate(),
		   any_coordinate() ) -> quaternion().
new( A, B, C, D ) ->
	{ type_utils:ensure_float( A ), type_utils:ensure_float( B ),
	  type_utils:ensure_float( C ), type_utils:ensure_float( D ) }.



% @doc Returns a textual description of specified quaternion; full float
% precision is shown.
%
-spec to_string( quaternion() ) -> ustring().
to_string( Q ) ->
	to_compact_string( Q ).



% @doc Returns a compact, textual, informal representation of the specified
% quaternion.
%
% This is the recommended representation.
%
-spec to_compact_string( quaternion() ) -> ustring().
to_compact_string( _Q={ A, B, C, D } ) ->
	text_utils:format( "~w + ~w.i + ~w.j + ~w.k", [ A, B, C, D ] ).



% @doc Returns a textual, more user-friendly representation of the specified
% quaternion; full float precision is shown.
%
-spec to_user_string( quaternion() ) -> ustring().
to_user_string( Q ) ->

	Coords = tuple_to_list( Q ),

	Strs = linear:coords_to_best_width_strings( Coords ),

	% No need for ~ts here (different representation from vector):
	ElemFormatStr = "| ~s |~n",

	FormatStr = "~n" ++ text_utils:duplicate( length( Coords ), ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; Strs: ~p.",
	%                       [ FormatStr, Strs ] ),

	text_utils:format( FormatStr, Strs ).
