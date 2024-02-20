% Copyright (C) 2024-2024 Olivier Boudeville
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
% Creation date: Monday, February 5, 2024.


% @doc Unit tests for the <b>3D transformations based on 4x4 matrices</b>.
%
% See the transform4 tested module.
%
-module(transform4_test).


-include_lib("myriad/include/matrix4.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").


% Comparisons are made of this specialised vector4 implementation with the one
% for arbitrary vectors (see vector.erl).
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	TId = transform4:identity(),

	transform4:check( TId ),

	CM = #compact_matrix4{ m11=1, m12=8, m13=3, tx=1,
						   m21=4, m22=5, m23=6, ty=2,
						   m31=7, m32=8, m33=9, tz=3  },

	T1 = transform4:new( CM ),

	% Useless as done by most operations: transform4:check( T1 ),

	CM = transform4:get_reference( T1 ),

	true = matrix4:are_equal( matrix4:identity(),
		matrix4:mult( CM, transform4:get_inverse( T1 ) ) ),

	%Tt = transform4:translation

	test_facilities:display( "Base textual representation for T1 = ~w ~n; "
		"user-friendly one: ~ts", [ T1, transform4:to_string( T1 ) ] ),

	test_facilities:stop().
