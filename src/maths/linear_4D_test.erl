% Copyright (C) 2003-2017 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
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


% Unit tests for the linear 4D facilities.
%
% See the linear_4D tested module.
%
-module(linear_4D_test).



% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),


	test_facilities:display( "~nTesting vectors first." ),

	NullVector = linear_4D:null_vector(),

	test_facilities:display( " Null vector is: ~s",
							 [ linear_4D:to_string( NullVector ) ] ),

	V1 = { 9.0, 1.0, 0.0, 1.0 },

	test_facilities:display( " V1 is: ~s", [ linear_4D:to_string( V1 ) ] ),

	ScaleFactor = 2.0,
	Vscale = { 18.0, 2.0, 0.0, 2.0 } = linear_4D:scale( V1, ScaleFactor ),

	test_facilities:display( " V1 scaled by ~p is: ~s",
							 [ ScaleFactor, linear_4D:to_string( Vscale ) ] ),

	V1 = linear_4D:add( V1, NullVector ),

	V2 = { 10.0, 10.0, 5.0, 2.0 },
	test_facilities:display( " V2 is: ~s", [ linear_4D:to_string( V2 ) ] ),

	Vsum = { 19.0, 11.0, 5.0, 3.0 } = linear_4D:add( V1, V2 ),

	test_facilities:display( " V1+V2 is: ~s", [ linear_4D:to_string( Vsum ) ] ),


	V3 = { 0.0, 0.0, 0.0, 3.0 },
	test_facilities:display( " V3 is: ~s", [ linear_4D:to_string( V3 ) ] ),

	V4 = { 1.0, 2.0, 3.0, 4.0 },
	test_facilities:display( " V4 is: ~s", [ linear_4D:to_string( V4 ) ] ),

	Vectors = [ V1, V2, V3, V4 ],

	{ 20.0, 13.0, 8.0, 10.0 } = Sum = linear_4D:add( Vectors ),

	test_facilities:display( " Sum of vectors V1, V2, V3 and V4 is ~s.",
							 [ linear_4D:to_string( Sum ) ] ),


	test_facilities:display( "Testing matrices." ),

	NullMatrix = linear_4D:null_matrix(),

	test_facilities:display( " Null matrix is:~n~s",
							 [ linear_4D:to_string( NullMatrix ) ] ),

	Id = linear_4D:identity(),

	test_facilities:display( " Identity matrix is:~n~s",
							 [ linear_4D:to_string( Id ) ] ),

	ColMatrix = linear_4D:from_columns( V1, V2, V3, V4 ),

	test_facilities:display( " Matrix whose columns are V1, V2, V3 and V4 "
							 "is:~n~s", [ linear_4D:to_string( ColMatrix ) ] ),

	RowMatrix = linear_4D:from_rows( V1, V2, V3, V4 ),

	test_facilities:display( " Matrix whose rows are V1, V2, V3 and V4 "
							 "is:~n~s", [ linear_4D:to_string( RowMatrix ) ] ),



	test_facilities:stop().
