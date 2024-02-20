% Copyright (C) 2022-2024 Olivier Boudeville
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
% Creation date: Wednesday, February 16, 2022.


% @doc Module implementing the support for <b>4x4 transformations</b> (hence for
% 3D computations), based on pairs (in logical terms - these are actually
% records) made of an homogeneous matrix and its inverse, which are updated and
% maintained through the operations applied to them.
%
% The operations triggered on a transformation (the said matrix pair) are the
% same (name, arguments and their order, contract) that would be triggered
% directly on its reference matrix only.
%
% So, instead of a matrix M, a transformation can be seen (logically) as a
% {M,InvM} pair, and instead of applying for example an operation O (whose
% matrix is Mo) on M with M' = M.Mo, we have here T' = T.Mo = {M.Mo,
% InvMo.InvM}.
%
% See also matrix4.erl, as this module offers a larger subset of its API (with
% any "homogeneous" removed in the function names, as it is implicit here). For
% non-covered functions, just get/set the reference/inverse matrices that a
% transform4 contains.
%
-module(transform4).



% Implementation notes:
%
% Requiring the inverse of an invertible transformation happens frequently, so
% such a transformation is a pair of matrices: a given reference matrix and its
% precomputed inverse, both updated by any operation, one by this actual
% operation and the other by its reciprocal, so that either remains directly
% available with no further computation.
%
% If such operations may not be invertible or even linear, we consider here that
% these are actually 3D operations represented thanks to 4D homogeneous matrices
% that may be invertible (e.g. for translations).
%
% The various types of matrix4 (canonical, compact, identity) are transparently
% managed. Whether storing any matrix4 instance or only homogeneous_matrix4 ones
% is still an open question.



% For records like matrix4:
-include("matrix4.hrl").

% For the #transform4 record:
-include("transform4.hrl").

-type transform4() :: #transform4{}.
% A 4x4 transformation, storing both its corresponding 4x4 matrix and its
% inverse.



-export_type([ transform4/0 ]).


-export([ new/1, new/2, identity/0,
		  get_reference/1, get_inverse/1,
		  translation/1, rotation/2, scaling/1, transition/4,
		  translate_left/2, translate_right/2,
		  rotate_left/3, rotate_right/3, 
		  scale_x/2, scale_y/2, scale_z/2, 
		  basis_change/3,
		  %from_columns/4, compact_from_columns/4, from_rows/4,
		  %from_coordinates/16, from_compact_coordinates/12,
		  %from_arbitrary/1, to_arbitrary/1, from_3D/2,
		  dimension/0, dimensions/0,
		  scale/2, %add/2, sub/2,
		  mult/1, mult/2,
		  are_equal/2,
		  determinant/1, inverse/1,
		  check/1,
		  to_string/1 ] ).


-define( dim, 4 ).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type factor() :: math_utils:factor().

-type radians() :: unit_utils:radians().

-type dimension() :: linear:dimension().
-type scalar() :: linear:scalar().

-type point3() :: point3:point3().

-type vector3() :: vector3:vector3().
-type unit_vector3() :: vector3:unit_vector3().

-type dimensions() :: matrix:dimensions().

-type user_matrix4() :: matrix4:user_matrix4().
-type matrix4() :: matrix4:matrix4().

-type homogeneous_matrix4() :: matrix4:homogeneous_matrix4().

-type transition_matrix4() :: matrix4:transition_matrix4().



% @doc Returns the 4x4 transformation whose reference matrix is the specified
% one.
%
-spec new( user_matrix4() | matrix4() ) -> transform4().
new( _M=identity_4 ) ->
	#transform4{};

new( CM ) when is_record( CM, compact_matrix4 ) ->
	T = case matrix4:inverse( CM ) of

		undefined ->
			throw( { non_invertible_matrix, CM } );

		InvCM ->
			#transform4{ reference=CM, inverse=InvCM }

	end,

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T;

new( UserMatrix ) ->
	M = matrix4:new( UserMatrix ),
	new( matrix4:to_compact( M ) ).



% @doc Returns the 4x4 transformation whose reference matrix and inverse one are
% directly the specified ones.
%
-spec new( homogeneous_matrix4(), homogeneous_matrix4() ) -> transform4().
new( HM, InvHM ) ->

	T = #transform4{ reference=HM, inverse=InvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T.


% No sensible null/0 here.


% @doc Returns the 4x4 identity transformation.
-spec identity() -> transform4().
identity() ->
	#transform4{}.



% @doc Returns the (4x4) reference matrix corresponding to the specified 4x4
% transformation.
%
-spec get_reference( transform4() ) -> homogeneous_matrix4().
get_reference( #transform4{ reference=HM } ) ->
	HM.


% @doc Returns the inverse of the (4x4) matrix corresponding to the specified
% 4x4 transformation.
%
-spec get_inverse( transform4() ) -> homogeneous_matrix4().
get_inverse( #transform4{ inverse=InvHM } ) ->
	InvHM.



% @doc Returns the 4x4 transformation corresponding to a translation of the
% specified (3D) vector.
%
-spec translation( vector3() ) -> transform4().
translation( VT ) ->

	% Hence a compact matrix:
	HM = matrix4:translation( VT ),

	% Inverse of a translation of VT is one of -VT:
	InvHM = matrix4:translation( vector3:negate( VT ) ),

	T = #transform4{ reference=HM, inverse=InvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T.



% @doc Returns the 4x4 transformation corresponding to a rotation of the
% specified angle around the 3D axis specified as a unit vector (and to no
% translation).
%
% This will be a counterclockwise rotation for an observer placed so that the
% specified axis points towards it.
%
% Note that this is not the general case of a rotation in 4D (which is of little
% use, at least here); this corresponds to (4x4) homogeneous matrices.
%
-spec rotation( unit_vector3(), radians() ) -> transform4().
rotation( UnitAxis, RadAngle ) ->

	HM = #compact_matrix4{ m12=M12, m13=M13,
						   m21=M21, m23=M23,
						   m31=M31, m32=M32 }
	   = matrix4:rotation( UnitAxis, RadAngle ),

	% More expensive:
	%InvM = matrix4:rotation( UnitAxis, -RadAngle ),

	% A transpose of the 3D part should do the trick as well; just having to
	% mirror-swap the extra-diagonal terms of the inner 3x3 matrix, knowing
	% that:
	% M = #compact_matrix4{ m11, m12, m13, tx=Zero,
	%                       m21, m22, m23, ty=Zero,
	%                       m31, m32, m33, tz=Zero }.
	%
	InvHM = HM#compact_matrix4{ m12=M21, m13=M31,
								m21=M12, m23=M32,
								m31=M13, m32=M23 },

	T = #transform4{ reference=HM, inverse=InvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T.



% @doc Returns the 4x4 transformation corresponding to the scaling of the
% specified (supposedly non-null) factors.
%
-spec scaling( { factor(), factor(), factor() } ) -> transform4().
scaling( Factors={ Sx, Sy, Sz } ) ->

	% Hence a compact matrix:
	HM = matrix4:scaling( Factors ),

	InvFactors = { 1/Sx, 1/Sy, 1/Sz },
	InvHM = matrix4:scaling( InvFactors ),

	T = #transform4{ reference=HM, inverse=InvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T.



% @doc Returns the 4x4 transition transformation from the current referential to
% one in which the origin and axes of the current referential are expressed.
%
% Refer to matrix4:transition/4 for further details.
%
-spec transition( point3(), unit_vector3(), unit_vector3(), unit_vector3() ) ->
								transition_matrix4().
transition( Origin, X, Y, Z ) ->

	% Hence a compact matrix:
	HM = matrix4:transition( Origin, X, Y, Z ),

	% The inverse of a transition matrix [R|T] is [Rt|-Rt.T] where Rt is the
	% transpose of R; so:

	{ R, T } = matrix4:to_3D( HM ),

	% As orthogonal:
	InvR = matrix3:transpose( R ),

	MinusT = matrix3:apply( InvR, T ),

	T = vector3:negate( MinusT ),

	% Hence a compact matrix:
	InvHM = matrix3:from_3D( InvR, T ),

	T = #transform4{ reference=HM, inverse=InvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	% TO-DO: fully inline the computation of InvHM, and check that it is equal
	% to the previous computation.

	T.



% @doc Returns the dimension of these transformations.
%
% Not useless, when using polymorphism based on module name.
%
-spec dimension() -> dimension().
dimension() ->
	?dim.



% @doc Returns the dimensions of these transformations.
%
% Not useless, when using polymorphism based on module name.
%
-spec dimensions() -> dimensions().
dimensions() ->
	{ ?dim, ?dim }.



% @doc Returns the specified 4x4 transformation (T) once multiplied on its left
% by a translation matrix (TM) corresponding to the specified vector (VT):
% returns therefore T' = TM.T.
%
% Corresponds to adding the specified translation vector to the right-most
% column of the reference homogeneous matrix, and updating its inverse
% accordingly.
%
-spec translate_left( vector3(), transform4() ) -> transform4().
translate_left( VT, #transform4{ reference=HM, inverse=InvHM } ) ->

	% So NewM = MVT.M:
	NewHM = matrix4:translate_homogeneous_left( VT, HM ),

	MinusVT = vector3:negate( VT ),

	% So NewInvHM = InvMH.InvMVT; nothing simpler than:
	NewInvHM = matrix4:mult( InvHM, matrix4:translation( MinusVT ) ),

	T = #transform4{ reference=NewHM, inverse=NewInvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T.



% @doc Returns the specified 4x4 transformation (T) once multiplied on its right
% by a translation matrix (TM) corresponding to the specified vector (VT):
% returns therefore T' = T.TM.
%
-spec translate_right( transform4(), vector3() ) -> transform4().
% Not helpful: corresponds to adding the opposite of the specified translation
% vector to the right-most column of the inverse homogeneous matrix, and
% updating its reference accordingly.
%
translate_right( #transform4{ reference=HM, inverse=InvHM }, VT ) ->

	% So NewHM = HM.MVT; nothing simpler than:
	NewHM = matrix4:mult( HM, matrix4:translation( VT ) ),

	% For the inverse now:
	MinusVT = vector3:negate( VT ),

	% So NewInvHM = InvMVT.InvHM:
	NewInvHM = matrix4:translate_homogeneous_left( MinusVT, InvHM ),

	T = #transform4{ reference=NewHM, inverse=NewInvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T.


% No real meaning/use with homogeneous matrices: transpose/1.




% @doc Updates the specified 4x4 transformation by applying on its left the
% specified rotation: returns therefore T' = RotM.T.
%
-spec rotate_left( unit_vector3(), radians(), transform4() ) -> transform4().
rotate_left( UnitAxis, RadAngle, #transform4{ reference=HM, inverse=InvHM } ) ->

	% NewHM = RotM.HM:
	NewHM = matrix4:rotate_homogeneous_left( UnitAxis, RadAngle, HM ),

	% NewInvM = InvHM.InvRotM, the inverse of a rotation being the opposite
	% angle around the same axis:
	%
	NewInvHM = matrix4:rotate_homogeneous_right( InvHM, UnitAxis, -RadAngle ),

	T = #transform4{ reference=NewHM, inverse=NewInvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T.



% @doc Updates the specified 4x4 transformation by applying on its right the
% specified rotation: returns therefore T' = T.RotM.
%
-spec rotate_right( transform4(), unit_vector3(), radians() ) -> transform4().
rotate_right( #transform4{ reference=HM, inverse=InvHM },
			  UnitAxis, RadAngle ) ->

	% NewHM = HM.RotM:
	NewHM = matrix4:rotate_homogeneous_right( HM, UnitAxis, RadAngle ),

	% NewInvHM = InvRotM.InvHM, the inverse of a rotation being the opposite
	% angle around the same axis:
	%
	NewInvHM = matrix4:rotate_homogeneous_left( UnitAxis, -RadAngle, InvHM ),

	T = #transform4{ reference=NewHM, inverse=NewInvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T.



% @doc Returns the specified 4x4 transformation once scaled by the specified
% (uniform, non-null) factor.
%
-spec scale( transform4(), factor() ) -> transform4().
scale( #transform4{ reference=HM, inverse=InvHM }, Factor ) ->

	NewHM = matrix4:scale_homogeneous( HM, Factor ),

	NewInvHM = matrix4:scale_homogeneous( InvHM, 1/Factor ),

	T = #transform4{ reference=NewHM, inverse=NewInvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T.



% @doc Updates the specified 4x4 transformation by applying the specified
% (uniform) shearing factor on the leftmost column (X) of its reference matrix,
% like when this matrix is multiplied on its right by a scaling matrix
% equal to the identity, except for its first diagonal element, which would be
% equal to the specified factor; returns therefore T' = T.SxM.
%
-spec scale_x( transform4(), factor() ) -> transform4().
scale_x( #transform4{ reference=HM, inverse=InvHM }, Factor ) ->
	NewHM = matrix4:scale_homogeneous_x( HM, Factor ),

	% If HM' = HM.SxM, then InvHM' = InvSxM.InvHM; InvSxM is like SxM but with
	% an inverse factor, so (transposed function version to account for the
	% reversed multiplication order):
	%
	NewInvHM = matrix4:scale_homogeneous_x_t( InvHM, 1/Factor ),

	T = #transform4{ reference=NewHM, inverse=NewInvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T.


% @doc Updates the specified 4x4 transformation by applying the specified
% (uniform) shearing factor on the second column (Y) of its reference matrix,
% like when this matrix is multiplied on its right by a scaling matrix
% equal to the identity, except for its second diagonal element, which would be
% equal to the specified factor; returns therefore T' = T.SyM.
%
-spec scale_y( transform4(), factor() ) -> transform4().
scale_y( #transform4{ reference=HM, inverse=InvHM }, Factor ) ->
	NewHM = matrix4:scale_homogeneous_y( HM, Factor ),

	% If HM' = HM.SyM, then InvHM' = InvSyM.InvHM; InvSyM is like SyM but with
	% an inverse factor, so (transposed function version to account for the
	% reversed multiplication order):
	%
	NewInvHM = matrix4:scale_homogeneous_y_t( InvHM, 1/Factor ),

	T = #transform4{ reference=NewHM, inverse=NewInvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T.



% @doc Updates the specified 4x4 transformation by applying the specified
% (uniform) shearing factor on the third column (Z) of its reference matrix,
% like when this matrix is multiplied on its right by a scaling matrix
% equal to the identity, except for its third diagonal element, which would be
% equal to the specified factor; returns therefore T' = T.SzM.
%
-spec scale_z( transform4(), factor() ) -> transform4().
scale_z( #transform4{ reference=HM, inverse=InvHM }, Factor ) ->
	NewHM = matrix4:scale_homogeneous_z( HM, Factor ),

	% If HM' = HM.SzM, then InvHM' = InvSzM.InvHM; InvSzM is like SzM but with
	% an inverse factor, so (transposed function version to account for the
	% reversed multiplication order):
	%
	NewInvHM = matrix4:scale_homogeneous_z_t( InvHM, 1/Factor ),

	T = #transform4{ reference=NewHM, inverse=NewInvHM },

	cond_utils:assert( myriad_check_linear, check( T ) ),

	T.



% @doc Returns the determinant of the specified matrix.
-spec determinant( transform4() ) -> scalar().
determinant( #transform4{ reference=HM } ) ->
	matrix4:determinant( HM ).



% @doc Returns the (4x4) transformation corresponding to the in-order
% multiplication of the two specified ones: returns therefore T = T1.T2.
%
-spec mult( transform4(), transform4() ) -> transform4().
mult( _T1=#transform4{ reference=HM1, inverse=InvHM1 },
	  _T2=#transform4{ reference=HM2, inverse=InvHM2 } ) ->
	M = matrix4:mult( HM1, HM2 ),
	InvM = matrix4:mult( InvHM2, InvHM1 ),
	T = #transform4{ reference=M, inverse=InvM },
	cond_utils:assert( myriad_check_linear, check( T ) ),
	T.


% @doc Returns the (4x4) transformation corresponding to the in-order
% multiplication of the specified ones.
%
-spec mult( [ transform4() ] ) -> transform4().
mult( _Transforms=[ T1, T2 | T ] ) ->
	mult( mult( T1, T2 ), T );

mult( _Transforms=[ T ] ) ->
	T.



% @doc Tells whether the two specified (4x4) transformations are equal.
-spec are_equal( transform4(), transform4() ) -> boolean().
are_equal( T1=#transform4{ reference=HM1, inverse=InvHM1 },
		   T2=#transform4{ reference=HM2, inverse=InvHM2 } ) ->
	cond_utils:if_defined( myriad_check_linear,
		begin
			FirstTest = matrix4:are_equal( HM1, HM2 ),
			SecondTest = matrix4:are_equal( InvHM1, InvHM2 ),
			FirstTest =:= SecondTest orelse
				throw( { inconsistent_equality, T1, T2 } ),
			FirstTest
		end,
		begin
			basic_utils:ignore_unused( [ T1, T2, InvHM1, InvHM2 ] ),
			matrix4:are_equal( HM1, HM2 )
		end ).



% @doc Returns a transition transformation whose reference matrix is a
% change-of-basis matrix from the current referential (R1) to one (R2) whose
% origin, forward and up directions are the specified ones (still in the current
% referential R1).
%
% So returns P1->2, allowing, for an (homogeneous) point P, to convert P1, its
% representation in current referential R1, into P2, its counterpart in R2:
% P2 = P1->2.P1.
%
% The inverse matrix in this transformation is thus P2->1.
%
-spec basis_change( point3(), vector3(), vector3() ) -> transform4().
basis_change( _O2InR1={ XO2, YO2, ZO2 }, FwdDir2InR1, UpDir2InR1 ) ->

	% A point whose coordinates are to convert from R1 to R2 shall first be
	% rotated, then translated; we determine here the axis vectors of R2, as
	% expressed in R1; refer to
	% https://howtos.esperide.org/ThreeDimensional.html#summary for more
	% information.

	% We now inline the definition of both matrices:

	_X2InR1 = [ XI2, YI2, ZI2 ] = FwdDir2InR1,

	_Z2InR1 = [ XK2, YK2, ZK2 ] = UpDir2InR1,

	% Y = Z^X:
	%_Y2InR1 = [ XJ2, YJ2, ZJ2 ] = vector3:cross_product( Z2InR1, X2InR1 ),

	XJ2 = YK2*ZI2 - ZK2*YI2,
	YJ2 = ZK2*XI2 - XK2*ZI2,
	ZJ2 = XK2*YI2 - YK2*XI2,

	%M = matrix4:compact_from_columns( X2InR1, Y2InR1, Z2InR1, O2InR1 ),
	M = #compact_matrix4{ m11=XI2, m12=XJ2, m13=XK2, tx=XO2,
						  m21=YI2, m22=YJ2, m23=YK2, ty=YO2,
						  m31=ZI2, m32=ZJ2, m33=ZK2, tz=ZO2 },

	% Reversed, reciprocal operations; we compute the inverse of M by applying
	% https://howtos.esperide.org/ThreeDimensional.html#summary:

	% Negation of the scalar product of new rows with new origin:
	InvTx = - ( XI2*XO2 + YI2*YO2 + ZI2*ZO2 ),
	InvTy = - ( XJ2*XO2 + YJ2*YO2 + ZJ2*ZO2 ),
	InvTz = - ( XK2*XO2 + YK2*YO2 + ZK2*ZO2 ),

	InvM = #compact_matrix4{ m11=XI2, m12=YI2, m13=ZI2, tx=InvTx,
							 m21=XJ2, m22=YJ2, m23=ZJ2, ty=InvTy,
							 m31=XK2, m32=YK2, m33=ZK2, tz=InvTz },

	#transform4{ reference=M, inverse=InvM }.



% @doc Returns the inverse transformation of the specified one.
-spec inverse( transform4() ) -> transform4().
inverse( #transform4{ reference=M, inverse=InvM } ) ->
	#transform4{ reference=InvM, inverse=M }.



% @doc Checks that this transformation is consistent; throws an exception if
% not.
%
-spec check( transform4() ) -> transform4().
check( T=#transform4{ reference=M, inverse=InvM } ) ->
	Mult = matrix4:mult( M, InvM ),

	matrix4:are_equal( Mult, matrix4:identity() )
		orelse throw( { inconsistent_transform4, T, Mult } ).



% @doc Returns a textual representation of the specified (4x4) transformation.
-spec to_string( transform4() ) -> ustring().
to_string( #transform4{ reference=M, inverse=InvM } ) ->
	text_utils:format( "4x4 transformation recording reference matrix ~ts and "
		"its inverse ~ts",
		[ matrix4:to_string( M ), matrix4:to_string( InvM ) ] ).
