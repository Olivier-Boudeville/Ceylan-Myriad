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

-module(camera).

-moduledoc """
Gathering of various facilities for **camera** management, to define a view
point on a 3D world.
""".


% For the camera record:
-include("camera.hrl").


-doc "Describes a camera.".
-type camera() :: #camera{}.


-doc """
A view matrix, that is a transition matrix from the world space to a camera
space.
""".
-type view_matrix4() :: transition_matrix4().


-export_type([ camera/0, view_matrix4/0 ]).



% Construction-related section.
-export([ create/3, get_view_matrix/1 ]).


% Operations on cameras:
-export([ to_string/1 ]).



% Other operations, lower-level:
-export([ compute_view_matrix/1 ]).


% For the compact_matrix4 record:
-include("matrix4.hrl").



% Type shorthands:

-type ustring() :: text_utils:ustring().

-type point3() :: point3:point3().
-type vector3() :: vector3:vector3().

%-type matrix4() :: matrix4:matrix4().
-type transition_matrix4() :: matrix4:transition_matrix4().

%-type unit_normal3() :: vector3:unit_normal3().

%-type rendering_info() :: mesh_render:rendering_info() .



% Implementation notes:




% Construction-related section.


-doc """
Returns a camera whose position and orientation are the specified ones.

The specified vectors are not required to be unit ones.
""".
-spec create( point3(), vector3(), vector3() ) -> camera().
create( Position, AimVec, UpVec ) ->

	% Probably safer to be done first:
	UnitAimVec = vector3:normalise( AimVec ),
	UnitUpVec = vector3:normalise( UpVec ),

	% As X = YxZ:
	%RightVec = vector3:cross_product( _Y=UnitUpVec, _Z=UnitAimVec ),
	% cond_utils:if_defined( myriad_check_linear,
	%                        vector:is_unitary( RightVec ) ),

	#camera{ position = Position,
			 aim=UnitAimVec,
			 up=UnitUpVec }.
			 %right=RightVec }.



-doc """
Returns the view matrix corresponding to the specified camera, with a possibly
updated version thereof.
""".
-spec get_view_matrix( camera() ) -> { view_matrix4(), camera() }.
get_view_matrix( Cam=#camera{ view_mat4=undefined } ) ->
	compute_view_matrix( Cam );

get_view_matrix( Cam=#camera{ view_mat4=ViewMat4 } ) ->
	{ ViewMat4, Cam }.



-doc """
Returns an updated camera in which the view matrix has been computed from its
position and orientation.
""".
-spec compute_view_matrix( camera() ) -> { view_matrix4(), camera() }.
compute_view_matrix( Cam=#camera{ position=_Position={ Px, Py, Pz },
								  aim=UnitAimVec=[ Ax, Ay, Az ],
								  up=UnitUpVec=[ Ux, Uy, Uz ],
								  right=MaybeUnitRightVec } ) ->

	UnitRightVec=[ Rx, Ry, Rz ] = case MaybeUnitRightVec of

		undefined ->
			RV = vector3:cross_product( UnitUpVec, UnitAimVec ),
			cond_utils:if_defined( myriad_check_linear,
								   vector:is_unitary( RV ) ),
			RV;

		RV ->
			RV

	end,

	% Could be done based on a quaternion, or as
	% https://learnopengl.com/Getting-started/Camera, or as glm (see
	% ext/matrix_transform.inl); knowing that 'up' is already supposed to be
	% orthogonal to 'aim', we preferred directly computing the transition matrix
	% from world (w) to camera (c) coordinates, Pw->c, that can easily be
	% computed from the coordinates of the axes of the world system in the
	% camera one. Unfortunately we have only the reciprocal information (the
	% coordinates of the camera in the world), and can only describe directly
	% the inverse, Pc->w.
	%
	% However, using the naming conventions specified in the camera record:
	% ViewMat4 = Ma.Mb where Ma:matrix([Rx,Ry,Rz,0], [Ux,Uy,Uz,0], [Ax,Ay,Az,0],
	% [0,0,0,1]) and Mb:matrix([1,0,0,-Px], [0,1,0,-Py], [0,0,1,-Pz],
	% [0,0,0,1]); so:
	% ViewMat4 = matrix([Rx, Ry, Rz, Pz*Rz-Py*Ry-Px*Rx],
	%                   [Ux, Uy, Uz, Pz*Uz-Py*Uy-Px*Ux],
	%                   [Ax, Ay, Az, Az*Pz-Ay*Py-Ax*Px],
	%                   [ 0,  0,  0,                 1])
	%
	% So:

	Tx = Pz*Rz - Py*Ry - Px*Rx,
	Ty = Pz*Uz - Py*Uy - Px*Ux,
	Tz = Az*Pz - Ay*Py - Ax*Px,

	ViewMat4 = #compact_matrix4{ m11=Rx, m12=Ry, m13=Rz, tx=Tx,
								 m21=Ux, m22=Uy, m23=Uz, ty=Ty,
								 m31=Ax, m32=Ay, m33=Az, tz=Tz },

	NewCam = Cam#camera{ right=UnitRightVec,
						 view_mat4=ViewMat4 },

	{ ViewMat4, NewCam }.



-doc "Returns a textual description of the specified camera.".
-spec to_string( camera() ) -> ustring().
to_string( #camera{ position=Pos,
					aim=UnitAimVec } ) ->
	text_utils:format( "camera located at ~ts, aimed at ~ts",
		[ point3:to_compact_string( Pos ),
		  vector3:to_compact_string( UnitAimVec ) ] ).
