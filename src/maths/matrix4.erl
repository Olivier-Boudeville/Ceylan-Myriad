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


% @doc Module implementing the support for <b>4x4 matrices</b>.
%
% See also:
% - the corresponding (4D) vectors, in vector4.erl
% - the (unspecialised) matrices of arbitrary dimensions, in matrix.erl
%
-module(matrix4).



% Implementation notes:
%


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For records like matrix3:
-include("matrix3.hrl").

% For records like matrix4:
-include("matrix4.hrl").

-type user_matrix4() :: user_matrix().
% A matrix4 can be specified as a list of same-size rows containing any kind of
% numerical coordinates.


-type matrix4() :: 'identity_4' | canonical_matrix4() | compact_matrix4().


% Alias for 4x4 canonical matrices:
-type canonical_matrix4() :: #matrix4{}.


% Aliases for 4x4 compact matrices:
-type compact_matrix4() :: #compact_matrix4{}.


-export_type([ user_matrix4/0, matrix4/0, canonical_matrix4/0,
			   compact_matrix4/0 ]).


-export([ new/1, null/0, identity/0, from_columns/4, from_rows/4,
		  from_coordinates/16, from_compact_coordinates/12, from_3D/2,
		  to_canonical/1, to_compact/1,
		  scale/2, mult/2,
		  are_equal/2,
		  to_string/1 ] ).


-import( math_utils, [ is_null/1, are_close/2 ] ).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type coordinate() :: linear:coordinate().
-type factor() :: linear:factor().

-type vector3() :: vector3:vector3().
-type vector4() :: vector4:vector4().

-type matrix3() :: matrix3:matrix3().

-type user_matrix() :: user_matrix().



% @doc Returns a 4D matrix corresponding to the user-specified one.
-spec new( user_matrix4() ) -> matrix4().
new( UserMatrix ) ->
	matrix:check( UserMatrix ),
	CoordList = list_utils:flatten_once( UserMatrix ),
	% Returns a #matrix4 record (i.e. tagged tuple):
	list_to_tuple( [ 'matrix4' | CoordList ] ).



% @doc Returns the null (4x4) matrix.
-spec null() -> canonical_matrix4().
null() ->
	Zero = 0.0,
	CoordList = lists:duplicate( _N=4*4, Zero ),
	list_to_tuple( [ 'matrix4' | CoordList ] ).



% @doc Returns the identity (4x4) matrix.
-spec identity() -> matrix4().
identity() ->
	identity_4.



% @doc Returns the 4x4 matrix whose columns correspond to the specified 4 4D
% vectors.
%
% Returns thus:
%  ```
%  [ Va Vb Vc Vd ]
%  [ |  |  |  |  ]
%  [ |  |  |  |  ]
%  [ |  |  |  |  ]
%  '''
%
-spec from_columns( vector4(), vector4(), vector4(), vector4() ) ->
							canonical_matrix4().
from_columns( _Va=[Xa,Ya,Za,Wa], _Vb=[Xb,Yb,Zb,Wb],
			  _Vc=[Xc,Yc,Zc,Wc], _Vd=[Xd,Yd,Zd,Wd] ) ->
	#matrix4{ m11=Xa, m12=Xb, m13=Xc, m14=Xd,
			  m21=Ya, m22=Yb, m23=Yc, m24=Yd,
			  m31=Za, m32=Zb, m33=Zc, m34=Zd,
			  m41=Wa, m42=Wb, m43=Wc, m44=Wd }.



% @doc Returns the 4x4 matrix whose rows correspond to the specified 4 4D
% vectors.
%
% Returns thus:
%  ```
% [ Va - - - ]
% [ Vb - - - ]
% [ Vc - - - ]
% [ Vd - - - ]
%  '''
%
-spec from_rows( vector4(), vector4(), vector4(), vector4() ) ->
						 canonical_matrix4().
from_rows( _Va=[Xa,Ya,Za,Wa], _Vb=[Xb,Yb,Zb,Wb],
		   _Vc=[Xc,Yc,Zc,Wc], _Vd=[Xd,Yd,Zd,Wd] ) ->
	#matrix4{ m11=Xa, m12=Ya, m13=Za, m14=Wa,
			  m21=Xb, m22=Yb, m23=Zb, m24=Wb,
			  m31=Xc, m32=Yc, m33=Zc, m34=Wc,
			  m41=Xd, m42=Yd, m43=Zd, m44=Wd }.



% @doc Returns the (4x4, canonical) matrix whose (16) coordinates are the
% specified ones, as listed row after row.
%
-spec from_coordinates( coordinate(), coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate(), coordinate() )
											-> canonical_matrix4().
from_coordinates( M11, M12, M13, M14,
				  M21, M22, M23, M24,
				  M31, M32, M33, M34,
				  M41, M42, M43, M44 ) ->
	#matrix4{ m11=M11, m12=M12, m13=M13, m14=M14,
			  m21=M21, m22=M22, m23=M23, m24=M24,
			  m31=M31, m32=M32, m33=M33, m34=M34,
			  m41=M41, m42=M42, m43=M43, m44=M44 }.



% @doc Returns the "4x4" (actually 3x4) compact matrix whose 12 actual
% coordinates are the specified ones, as listed row after row.
%
-spec from_compact_coordinates(
					coordinate(), coordinate(), coordinate(), coordinate(),
					coordinate(), coordinate(), coordinate(), coordinate(),
					coordinate(), coordinate(), coordinate(), coordinate() )
											-> compact_matrix4().
from_compact_coordinates( M11, M12, M13, Tx,
						  M21, M22, M23, Ty,
						  M31, M32, M33, Tz ) ->
	#compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
					  m21=M21, m22=M22, m23=M23, ty=Ty,
					  m31=M31, m32=M32, m33=M33, tz=Tz }.



% @doc Returns the 4x4 compact matrix obtained from specified 3x3 matrix and
% 3D vector.
%
-spec from_3D( matrix3(), vector3() ) -> compact_matrix4().
from_3D( #matrix3{ m11=M11, m12=M12, m13=M13,
				   m21=M21, m22=M22, m23=M23,
				   m31=M31, m32=M32, m33=M33 },
		 _Vec3=[ X, Y, Z ] ) ->
	#compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=X,
					  m21=M21, m22=M22, m23=M23, ty=Y,
					  m31=M31, m32=M32, m33=M33, tz=Z }.



% @doc Returns the canonical form of the specified 4x4 matrix.
-spec to_canonical( matrix4() ) -> canonical_matrix4().
to_canonical( #compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
								m21=M21, m22=M22, m23=M23, ty=Ty,
								m31=M31, m32=M32, m33=M33, tz=Tz } ) ->
	Zero = 0.0,
	#matrix4{ m11=M11,  m12=M12,  m13=M13,  m14=Tx,
			  m21=M21,  m22=M22,  m23=M23,  m24=Ty,
			  m31=M31,  m32=M32,  m33=M33,  m34=Tz,
			  m41=Zero, m42=Zero, m43=Zero, m44=1.0 };

to_canonical( identity_4 ) ->
	#matrix4{ m11=1.0, m12=0.0, m13=0.0, m14=0.0,
			  m21=0.0, m22=1.0, m23=0.0, m24=0.0,
			  m31=0.0, m32=0.0, m33=1.0, m34=0.0,
			  m41=0.0, m42=0.0, m43=0.0, m44=1.0 };

to_canonical( M ) when is_record( M, matrix4 ) ->
	M.



% @doc Returns the compact form of specified (4x4) matrix.
%
% Throws an exception if the specified matrix cannot be expressed as a compact
% one.
%
-spec to_compact( matrix4() ) -> compact_matrix4().
to_compact( M=#matrix4{ m11=M11, m12=M12, m13=M13, m14=M14,
						m21=M21, m22=M22, m23=M23, m24=M24,
						m31=M31, m32=M32, m33=M33, m34=M34,
						m41=M41, m42=M42, m43=M43, m44=M44 }) ->
	case is_null( M41 ) andalso is_null( M42 ) andalso is_null( M43 )
			andalso are_close( M44, 1.0 ) of

		true ->
			% Just drop the last row then:
			#compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=M14,
							  m21=M21, m22=M22, m23=M23, ty=M24,
							  m31=M31, m32=M32, m33=M33, tz=M34 };

		false ->
			trace_utils:error_fmt( "Canonical 4D matrix~n~ts~ncannot be "
				"expressed as a compact one.", [ to_string( M ) ] ),

			throw( { not_compactable, M } )

	end;

to_compact( identity_4 ) ->
	#compact_matrix4{ m11=1.0, m12=0.0, m13=0.0, tx=0.0,
					  m21=0.0, m22=1.0, m23=0.0, ty=0.0,
					  m31=0.0, m32=0.0, m33=1.0, tz=0.0 };

to_compact( CM ) when is_record( CM, compact_matrix4 ) ->
	CM.



% @doc Scales specified (4D) matrix of specified factor.
-spec scale( matrix4(), factor() ) -> matrix4().
scale( #compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
						 m21=M21, m22=M22, m23=M23, ty=Ty,
						 m31=M31, m32=M32, m33=M33, tz=Tz }, Factor ) ->
	#compact_matrix4{
		 m11=Factor*M11, m12=Factor*M12, m13=Factor*M13, tx=Factor*Tx,
		 m21=Factor*M21, m22=Factor*M22, m23=Factor*M23, ty=Factor*Ty,
		 m31=Factor*M31, m32=Factor*M32, m33=Factor*M33, tz=Factor*Tz };

scale( #matrix4{ m11=M11, m12=M12, m13=M13, m14=Tx,
				 m21=M21, m22=M22, m23=M23, m24=Ty,
				 m31=M31, m32=M32, m33=M33, m34=Tz,
				 m41=M41, m42=M42, m43=M43, m44=Tw }, Factor ) ->
	#matrix4{ m11=Factor*M11, m12=Factor*M12, m13=Factor*M13, m14=Factor*Tx,
			  m21=Factor*M21, m22=Factor*M22, m23=Factor*M23, m24=Factor*Ty,
			  m31=Factor*M31, m32=Factor*M32, m33=Factor*M33, m34=Factor*Tz,
			  m41=Factor*M41, m42=Factor*M42, m43=Factor*M43, m44=Factor*Tw };

scale( identity_4, Factor ) ->
	scale( to_canonical( identity_4 ), Factor ).



% @doc Multiplies the first matrix by the second one: returns Mc = Ma.Mb.
-spec mult( matrix4(), matrix4() ) -> matrix4().
mult( identity_4, M ) ->
	M;

mult( M, identity_4 ) ->
	M;

mult( _Ma=#matrix4{ m11=A11, m12=A12, m13=A13, m14=A14,
					m21=A21, m22=A22, m23=A23, m24=A24,
					m31=A31, m32=A32, m33=A33, m34=A34,
					m41=A41, m42=A42, m43=A43, m44=A44 },
	  _Mb=#matrix4{ m11=B11, m12=B12, m13=B13, m14=B14,
					m21=B21, m22=B22, m23=B23, m24=B24,
					m31=B31, m32=B32, m33=B33, m34=B34,
					m41=B41, m42=B42, m43=B43, m44=B44 } ) ->

	C11 = A11*B11 + A12*B21 + A13*B31 + A14*B41,
	C12 = A11*B12 + A12*B22 + A13*B32 + A14*B42,
	C13 = A11*B13 + A12*B23 + A13*B33 + A14*B43,
	C14 = A11*B14 + A12*B24 + A13*B34 + A14*B44,

	C21 = A21*B11 + A22*B21 + A23*B31 + A24*B41,
	C22 = A21*B12 + A22*B22 + A23*B32 + A24*B42,
	C23 = A21*B13 + A22*B23 + A23*B33 + A24*B43,
	C24 = A21*B14 + A22*B24 + A23*B34 + A24*B44,

	C31 = A31*B11 + A32*B21 + A33*B31 + A34*B41,
	C32 = A31*B12 + A32*B22 + A33*B32 + A34*B42,
	C33 = A31*B13 + A32*B23 + A33*B33 + A34*B43,
	C34 = A31*B14 + A32*B24 + A33*B34 + A34*B44,

	C41 = A41*B11 + A42*B21 + A43*B31 + A44*B41,
	C42 = A41*B12 + A42*B22 + A43*B32 + A44*B42,
	C43 = A41*B13 + A42*B23 + A43*B33 + A44*B43,
	C44 = A41*B14 + A42*B24 + A43*B34 + A44*B44,

	#matrix4{ m11=C11, m12=C12, m13=C13, m14=C14,
			  m21=C21, m22=C22, m23=C23, m24=C24,
			  m31=C31, m32=C32, m33=C33, m34=C34,
			  m41=C41, m42=C42, m43=C43, m44=C44 };

mult( _Ma=#compact_matrix4{ m11=A11, m12=A12, m13=A13, tx=Tx,
							m21=A21, m22=A22, m23=A23, ty=Ty,
							m31=A31, m32=A32, m33=A33, tz=Tz },
	  _Mb=#matrix4{ m11=B11, m12=B12, m13=B13, m14=B14,
					m21=B21, m22=B22, m23=B23, m24=B24,
					m31=B31, m32=B32, m33=B33, m34=B34,
					m41=B41, m42=B42, m43=B43, m44=B44 } ) ->


	C11 = A11*B11 + A12*B21 + A13*B31 + Tx*B41,
	C12 = A11*B12 + A12*B22 + A13*B32 + Tx*B42,
	C13 = A11*B13 + A12*B23 + A13*B33 + Tx*B43,
	C14 = A11*B14 + A12*B24 + A13*B34 + Tx*B44,

	C21 = A21*B11 + A22*B21 + A23*B31 + Ty*B41,
	C22 = A21*B12 + A22*B22 + A23*B32 + Ty*B42,
	C23 = A21*B13 + A22*B23 + A23*B33 + Ty*B43,
	C24 = A21*B14 + A22*B24 + A23*B34 + Ty*B44,

	C31 = A31*B11 + A32*B21 + A33*B31 + Tz*B41,
	C32 = A31*B12 + A32*B22 + A33*B32 + Tz*B42,
	C33 = A31*B13 + A32*B23 + A33*B33 + Tz*B43,
	C34 = A31*B14 + A32*B24 + A33*B34 + Tz*B44,

	C41 = B41,
	C42 = B42,
	C43 = B43,
	C44 = B44,

	#matrix4{ m11=C11, m12=C12, m13=C13, m14=C14,
			  m21=C21, m22=C22, m23=C23, m24=C24,
			  m31=C31, m32=C32, m33=C33, m34=C34,
			  m41=C41, m42=C42, m43=C43, m44=C44 };

mult( _Ma=#matrix4{ m11=A11, m12=A12, m13=A13, m14=A14,
					m21=A21, m22=A22, m23=A23, m24=A24,
					m31=A31, m32=A32, m33=A33, m34=A34,
					m41=A41, m42=A42, m43=A43, m44=A44 },
	  _Mb=#compact_matrix4{ m11=B11, m12=B12, m13=B13, tx=Tx,
							m21=B21, m22=B22, m23=B23, ty=Ty,
							m31=B31, m32=B32, m33=B33, tz=Tz } ) ->

	C11 = A11*B11 + A12*B21 + A13*B31,
	C12 = A11*B12 + A12*B22 + A13*B32,
	C13 = A11*B13 + A12*B23 + A13*B33,
	C14 = A11*Tx  + A12*Ty  + A13*Tz + A14,

	C21 = A21*B11 + A22*B21 + A23*B31,
	C22 = A21*B12 + A22*B22 + A23*B32,
	C23 = A21*B13 + A22*B23 + A23*B33,
	C24 = A21*Tx  + A22*Ty  + A23*Tz + A24,

	C31 = A31*B11 + A32*B21 + A33*B31,
	C32 = A31*B12 + A32*B22 + A33*B32,
	C33 = A31*B13 + A32*B23 + A33*B33,
	C34 = A31*Tx  + A32*Ty  + A33*Tz + A34,

	C41 = A41*B11 + A42*B21 + A43*B31,
	C42 = A41*B12 + A42*B22 + A43*B32,
	C43 = A41*B13 + A42*B23 + A43*B33,
	C44 = A41*Tx  + A42*Ty  + A43*Tz + A44,

	#matrix4{ m11=C11, m12=C12, m13=C13, m14=C14,
			  m21=C21, m22=C22, m23=C23, m24=C24,
			  m31=C31, m32=C32, m33=C33, m34=C34,
			  m41=C41, m42=C42, m43=C43, m44=C44 };

mult( _Ma=#compact_matrix4{ m11=A11, m12=A12, m13=A13, tx=Ax,
							m21=A21, m22=A22, m23=A23, ty=Ay,
							m31=A31, m32=A32, m33=A33, tz=Az },
	  _Mb=#compact_matrix4{ m11=B11, m12=B12, m13=B13, tx=Bx,
							m21=B21, m22=B22, m23=B23, ty=By,
							m31=B31, m32=B32, m33=B33, tz=Bz } ) ->

	C11 = A11*B11 + A12*B21 + A13*B31,
	C12 = A11*B12 + A12*B22 + A13*B32,
	C13 = A11*B13 + A12*B23 + A13*B33,
	Cx  = A11*Bx  + A12*By  + A13*Bz + Ax,

	C21 = A21*B11 + A22*B21 + A23*B31,
	C22 = A21*B12 + A22*B22 + A23*B32,
	C23 = A21*B13 + A22*B23 + A23*B33,
	Cy  = A21*Bx  + A22*By  + A23*Bz + Ay,

	C31 = A31*B11 + A32*B21 + A33*B31,
	C32 = A31*B12 + A32*B22 + A33*B32,
	C33 = A31*B13 + A32*B23 + A33*B33,
	Cz  = A31*Bx  + A32*By  + A33*Bz + Az,

	% C4{1,2,3} are 0.0, C44 is 1.0.

	#compact_matrix4{ m11=C11, m12=C12, m13=C13, tx=Cx,
					  m21=C21, m22=C22, m23=C23, ty=Cy,
					  m31=C31, m32=C32, m33=C33, tz=Cz }.



% @doc Tells whether the two specified (4x4) matrices are equal.
-spec are_equal( matrix4(), matrix4() ) -> boolean().
are_equal( _Ma=identity_4, _Mb=identity_4 ) ->
	true;

are_equal( _Ma=#matrix4{ m11=A11, m12=A12, m13=A13, m14=A14,
						 m21=A21, m22=A22, m23=A23, m24=A24,
						 m31=A31, m32=A32, m33=A33, m34=A34,
						 m41=A41, m42=A42, m43=A43, m44=A44 },
		   _Mb=#matrix4{ m11=B11, m12=B12, m13=B13, m14=B14,
						 m21=B21, m22=B22, m23=B23, m24=B24,
						 m31=B31, m32=B32, m33=B33, m34=B34,
						 m41=B41, m42=B42, m43=B43, m44=B44 } ) ->
				are_close( A11, B11 ) andalso are_close( A12, B12 )
		andalso are_close( A13, B13 ) andalso are_close( A14, B14 )
		andalso are_close( A21, B21 ) andalso are_close( A22, B22 )
		andalso are_close( A23, B23 ) andalso are_close( A24, B24 )
		andalso are_close( A31, B31 ) andalso are_close( A32, B32 )
		andalso are_close( A33, B33 ) andalso are_close( A34, B34 )
		andalso are_close( A41, B41 ) andalso are_close( A42, B42 )
		andalso are_close( A43, B43 ) andalso are_close( A44, B44 );

are_equal( _Ma=#compact_matrix4{ m11=A11, m12=A12, m13=A13, tx=Ax,
								 m21=A21, m22=A22, m23=A23, ty=Ay,
								 m31=A31, m32=A32, m33=A33, tz=Az },
		   _Mb=#compact_matrix4{ m11=B11, m12=B12, m13=B13, tx=Bx,
								 m21=B21, m22=B22, m23=B23, ty=By,
								 m31=B31, m32=B32, m33=B33, tz=Bz } ) ->
				are_close( A11, B11 ) andalso are_close( A12, B12 )
		andalso are_close( A13, B13 ) andalso are_close( Ax,  Bx )
		andalso are_close( A21, B21 ) andalso are_close( A22, B22 )
		andalso are_close( A23, B23 ) andalso are_close( Ay,  By )
		andalso are_close( A31, B31 ) andalso are_close( A32, B32 )
		andalso are_close( A33, B33 ) andalso are_close( Az,  Bz );

are_equal( _Ma=#matrix4{ m11=A11, m12=A12, m13=A13, m14=A14,
						 m21=A21, m22=A22, m23=A23, m24=A24,
						 m31=A31, m32=A32, m33=A33, m34=A34,
						 m41=A41, m42=A42, m43=A43, m44=A44 },
		   _Mb=#compact_matrix4{ m11=B11, m12=B12, m13=B13, tx=Bx,
								 m21=B21, m22=B22, m23=B23, ty=By,
								 m31=B31, m32=B32, m33=B33, tz=Bz } ) ->
				are_close( A11, B11 ) andalso are_close( A12, B12 )
		andalso are_close( A13, B13 ) andalso are_close( A14, Bx  )
		andalso are_close( A21, B21 ) andalso are_close( A22, B22 )
		andalso are_close( A23, B23 ) andalso are_close( A24, By  )
		andalso are_close( A31, B31 ) andalso are_close( A32, B32 )
		andalso are_close( A33, B33 ) andalso are_close( A34, Bz  )
		andalso is_null( A41 ) andalso is_null( A42 )
		andalso is_null( A43 ) andalso are_close( A44, 1.0 );

are_equal( Ma=#compact_matrix4{}, Mb=#matrix4{} ) ->
	are_equal( Mb, Ma );

are_equal( Ma, _Mb=identity_4 ) ->
	are_equal( Ma, to_compact( identity_4 ) );

are_equal( _Ma=identity_4, Mb ) ->
	are_equal( Mb, identity_4 ).



% @doc Returns a textual representation of specified (4x4) matrix.
-spec to_string( matrix4() ) -> ustring().
to_string( _Matrix=identity_4 ) ->
	"4x4 identity";

to_string( _Matrix=#matrix4{ m11=M11, m12=M12, m13=M13, m14=M14,
							 m21=M21, m22=M22, m23=M23, m24=M24,
							 m31=M31, m32=M32, m33=M33, m34=M34,
							 m41=M41, m42=M42, m43=M43, m44=M44 } ) ->
	% tuple_to_list could be relevant as well:
	AllCoords = [ M11, M12, M13, M14,
				  M21, M22, M23, M24,
				  M31, M32, M33, M34,
				  M41, M42, M43, M44 ],

	Strs = linear:coords_to_best_width_strings( AllCoords ),

	Dim = 4,

	% No need for ~ts here:
	RowFormatStr = "[ " ++ text_utils:duplicate( Dim, "~s " ) ++ "]~n",

	FormatStr = "~n" ++ text_utils:duplicate( Dim, RowFormatStr ),

	text_utils:format( FormatStr, Strs );


to_string( _Matrix=#compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
									 m21=M21, m22=M22, m23=M23, ty=Ty,
									 m31=M31, m32=M32, m33=M33, tz=Tz } ) ->
	AllCoords = [ M11, M12, M13, Tx,
				  M21, M22, M23, Ty,
				  M31, M32, M33, Tz,
				  0.0, 0.0, 0.0, 1.0 ],

	Strs = linear:coords_to_best_width_strings( AllCoords ),

	Dim = 4,

	% No need for ~ts here:
	RowFormatStr = "[ " ++ text_utils:duplicate( Dim, "~s " ) ++ "]~n",

	FormatStr = "~n" ++ text_utils:duplicate( Dim, RowFormatStr ),

	text_utils:format( FormatStr, Strs ).
