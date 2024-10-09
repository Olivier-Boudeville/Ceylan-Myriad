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
% Creation date: Saturday, September 28, 2024.



% Describes a fly-style camera that can freely move around in a 3D scene.
-record( camera, {


	% The origin of the viewpoint corresponding to this camera, in world space:
	%
	% (generally designated as P={Px,Py,Pz})
	%
	position :: point3:point3(),


	% The actual direction at which this camera points, in world space, from its
	% position:
	%
	% (any negating of it due to conventions like OpenGL - whose cameras point
	% to their -Z axis - is done internally; this vector thus corresponds to the
	% +Z axis of the local coordinate system of this camera)
	%
	% (generally designated as A=[Ax,Ay,Az])
	%
	aim :: vector3:unit_vector3(),


	% The direction, in world space, of the upper part of this camera:
	%
	% (corresponds to the +Y axis of the local coordinate system of this camera)
	%
	% Note that this vector is supposed to be orthogonal to the aim one.
	%
	% (generally designated as U=[Ux,Uy,Uz])
	%
	up :: vector3:unit_vector3(),


	% The direction, in world space, of the rightmost part of this camera:
	%
	% (this could be a maybe-value or one that is not even stored, as it can be
	% deduced from the aim and up vectors; it corresponds to the +X axis of the
	% local coordinate system of this camera)
	%
	right :: option( vector3:unit_vector3() ),


	% Any view matrix (transition matrix from world space to camera space),
	% based on the current camera settings.
	%
	% As a consequence, any change in the previous field should result in
	% invalidating this one (i.e. setting it to 'undefined'), so that it gets
	% recomputed whenever needed.
	%
	% (a full transform4 is currently not felt needed here)
	%
	view_mat4 :: option( camera:view_matrix4() ) } ).
