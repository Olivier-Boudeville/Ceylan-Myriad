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


% @doc Unit tests for the <b>arbitrary matrix</b> facilities.
%
% See the matrix tested module.
%
-module(matrix_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	Null3x2 = [ [ 0.0, 0.0 ], [ 0.0, 0.0 ], [ 0.0, 0.0 ] ],
	{ 3, 2 } = matrix:dimensions( Null3x2 ),

	Null3x2 = matrix:null( _RowCount=3, _ColumnCount=2 ),

	M = matrix:new( [ [ 0.0, 1.0, 2.0 ], [ 7777.0, 0.0, 1/3 ] ] ),

	test_facilities:display( "Base textual representation for ~w: ~ts",
							 [ M, matrix:to_string( M ) ] ),

	test_facilities:display( "Basic textual representation for ~w: ~ts",
							 [ M, matrix:to_basic_string( M ) ] ),

	test_facilities:display( "User-friendly textual representation "
		"for ~w: ~ts", [ M, matrix:to_user_string( M ) ] ),

	Dim = 5,

	Id = matrix:identity( Dim ),

	test_facilities:display( "Id(~B) = ~ts", [ Dim, matrix:to_string( Id ) ] ),

	M1 = matrix:new( [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ] ),

	8.0 = matrix:get_element( 3, 2, M1 ),

	NewM1 = matrix:set_element( 3, 2, 10.0, M1 ),
	test_facilities:display( "M1 = ~ts", [ matrix:to_string( M1 ) ] ),

	10.0 = matrix:get_element( 3, 2, NewM1 ),

	test_facilities:display( "NewM1 = ~ts", [ matrix:to_string( NewM1 ) ] ),

	TransposeM = [ [ 0.0, 7777.0 ], [ 1.0, 0.0 ], [2.0, 1/3 ] ],
	TransposeM = matrix:transpose( M ),

	test_facilities:display( "Transpose of M = ~ts is: ~ts",
		[ matrix:to_string( M ), matrix:to_string( TransposeM )] ),

	M2 = matrix:identity( 3 ),

	M3 = matrix:add( M1, M2 ),

	[ 7.0, 8.0, 10.0 ] = matrix:row( 3, M3 ),

	[ 2.0, 6.0, 8.0 ] = matrix:column( 2, M3 ),


	test_facilities:display( "M1 = ~tsM2 = ~tsM3 = M1 + M2 = ~ts",
		[ matrix:to_string( M1 ), matrix:to_string( M2 ),
		  matrix:to_string( M3 ) ] ),

	test_facilities:stop().
