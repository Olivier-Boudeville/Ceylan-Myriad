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


% @doc Unit tests for the <b>arbitrary vector</b> facilities.
%
% See the vector tested module.
%
-module(vector_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	Null2D = [ 0.0, 0.0 ],

	Null2D = vector:null( _Dim=2 ),

	X1 = [ 1.0, 0.0, 0.0 ],
	X2 = [ 1, 0.0, 0 ],

	V3D = vector:new( X1 ),

	[ V3D = vector:new( X ) || X <- [ X1, X2 ] ],

	IntP = { 10, 25 },

	V = [ 10.0, 25.0 ],
	V = vector:from_point( IntP ),

	FloatP = { 1/3, 17.0 },
	VecP = tuple_to_list( FloatP ),
	FloatP = vector:to_point( VecP ),

	test_facilities:display( "Basic textual representation for ~w: ~ts",
							 [ V3D, vector:to_string( V3D ) ] ),

	test_facilities:display( "Short textual representation for ~w: ~ts",
							 [ V3D, vector:to_short_string( V3D ) ] ),

	test_facilities:display( "User-friendly textual representation "
		"for ~w:~n~ts", [ V3D, vector:to_user_string( V3D ) ] ),

	test_facilities:display( "User-friendly textual representation "
		"for ~w:~n~ts", [ VecP, vector:to_user_string( VecP ) ] ),

	test_facilities:stop().
