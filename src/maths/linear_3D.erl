% Copyright (C) 2003-2021 Olivier Boudeville
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
% Creation date: Monday, February 15, 2010.


% @doc Gathering of various <b>three dimensional linear</b> facilities.
%
% See `linear_3D_test.erl' for the corresponding test.
%
-module(linear_3D).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% Shorthands:

-type factor() :: maths_utils:factor().

-type normal() :: vector:normal().
-type unit_normal() :: vector:unit_normal().



% Section about lines and planes.


-type line() :: { A :: factor(), B :: factor(), C :: factor(), D :: factor() }.
% A 3D line, whose equation A.x+B.y+C.z+D=0, can be defined from these four
% factors {A,B,C,D}.


-type plane() :: { normal(), factor() }.
% A plane, whose general equation is: A.x + B.y + C.z + D = 0, where:
%
% - P=(x,y,z) is a point belonging to this plane
%
% - N=(A,B,C) is a (non-necessarily unit) normal vector to this plane
%
% - P0=(x0,y0,z0) is a point of that plane
%
% - D= -A.x0 - B.y0 - C.z0
%
% See [http://mathworld.wolfram.com/Plane.html].
%
% So a plane may be described as (N,D).


-type hessian_plane() :: { unit_normal(), factor() }.
% A plane in Hessian normal form.
%
% See [http://mathworld.wolfram.com/HessianNormalForm.html].


-export_type([ line/0, plane/0, hessian_plane/0 ]).



% Section about shapes.


-type shape() :: 'sphere' | 'right_cuboid'.
% Various types of known 3D shapes (basic geometries).


-export_type([ shape/0 ]).
