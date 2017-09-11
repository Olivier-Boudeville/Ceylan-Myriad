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
% Creation date: Monday, February 15, 2010.


% Gathering of various four dimensional linear facilities, mostly dealing with
% homogeneous matrices for 3D.
%
% See linear_4D_test.erl for the corresponding test.
%
-module(linear_4D).


% Relatively aggressive inlining for basic operations:
-compile( inline ).
-compile( { inline_size, 48 } ).


% For the mat4 record:
-include("linear_4D.hrl").

% For the mat3 record:
-include("linear_3D.hrl").


% Shorthands:
-type coordinate() :: linear:coordinate().
-type factor() :: linear:factor().


% A 4D vector, with floating-point coordinates.
%
% They are typically referenced as [ X, Y, Z, W ].
%
-type vector() :: { coordinate(), coordinate(), coordinate(), coordinate() }.


% Alias for 4x4 canonical matrices:
-type mat4() :: #mat4{}.
-type canonical_matrix() :: mat4().



% Aliases for 4x4 compact matrices:
-type cpt_mat4() :: #cpt_mat4{}.
-type compact_matrix() :: cpt_mat4().


-type matrix() :: 'identity_4' | canonical_matrix() | compact_matrix().


-export_type([ mat4/0, canonical_matrix/0,
			   cpt_mat4/0, compact_matrix/0,
			   matrix/0 ]).


% Vector-related operations:
-export([ null_vector/0, scale/2, add/2, add/1 ]).


% Matrix-related operations:
-export([ null_matrix/0, identity/0, from_columns/4, from_rows/4,
		  from_coordinates/16, from_3D/2,
		  to_canonical/1, to_compact/1,
		  to_string/1 ]).


-import( math_utils, [ is_null/1, are_close/2 ] ).
-import( linear, [ coord_to_string/1 ]).



% Implementation of vector-related operations.


% Returns the null (4D) vector.
%
-spec null_vector() -> vector().
null_vector() ->
	{ 0.0, 0.0, 0.0, 0.0 }.



% Scales specified (4D) vector of specified factor.
%
-spec scale( vector(), factor() ) -> vector().
scale( _V={X,Y,Z,W}, Factor ) ->
	{ Factor*X, Factor*Y, Factor*Z, Factor*W }.


% Adds the two specified (4D) vectors.
%
-spec add( vector(), vector() ) -> vector().
add( _Va={Xa,Ya,Za,Wa}, _Vb={Xb,Yb,Zb,Wb} ) ->
	{ Xa+Xb, Ya+Yb, Za+Zb, Wa+Wb }.



% Adds the specified (non-empty) list of (4D) vectors.
%
-spec add( [ vector() ] ) -> vector().
add( _Vectors=[ V | T ] ) ->
	add_vec_list( T, _Acc=V );

add( _Vectors=[] ) ->
	throw( cannot_add_empty_list ).


% (helper)
add_vec_list( _Vec=[], AccV ) ->
	AccV;

add_vec_list( _Vec=[ V | T ], AccV ) ->
	NewAccV = add( V, AccV ),
	add_vec_list( T, NewAccV ).



% Implementation of matrix-related operations.


% Returns the null (4x4) matrix.
%
-spec null_matrix() -> canonical_matrix().
null_matrix() ->
	#mat4{ m11=0.0, m12=0.0, m13=0.0, m14=0.0,
		   m21=0.0, m22=0.0, m23=0.0, m24=0.0,
		   m31=0.0, m32=0.0, m33=0.0, m34=0.0,
		   m41=0.0, m42=0.0, m43=0.0, m44=0.0 }.



% Returns the identity (4x4) matrix.
%
%
-spec identity() -> matrix().
identity() ->
	identity_4.


% Returns the (4x4) matrix whose columns correspond to the specified 4 vectors:
%  [ Va Vb Vc Vd ]
%  [ |  |  |  |  ]
%  [ |  |  |  |  ]
%  [ |  |  |  |  ]
%
-spec from_columns( vector(), vector(), vector(), vector() ) ->
						  canonical_matrix().
from_columns( _Va={Xa,Ya,Za,Wa}, _Vb={Xb,Yb,Zb,Wb},
			  _Vc={Xc,Yc,Zc,Wc}, _Vd={Xd,Yd,Zd,Wd} ) ->
	#mat4{ m11=Xa, m12=Xb, m13=Xc, m14=Xd,
		   m21=Ya, m22=Yb, m23=Yc, m24=Yd,
		   m31=Za, m32=Zb, m33=Zc, m34=Zd,
		   m41=Wa, m42=Wb, m43=Wc, m44=Wd }.



% Returns the (4x4) matrix whose columns correspond to the specified 4 vectors:
% [ Va - - - ]
% [ Vb - - - ]
% [ Vc - - - ]
% [ Vd - - - ]
%
-spec from_rows( vector(), vector(), vector(), vector() ) -> canonical_matrix().
from_rows( _Va={Xa,Ya,Za,Wa}, _Vb={Xb,Yb,Zb,Wb},
		   _Vc={Xc,Yc,Zc,Wc}, _Vd={Xd,Yd,Zd,Wd} ) ->
	#mat4{ m11=Xa, m12=Ya, m13=Za, m14=Wa,
		   m21=Xb, m22=Yb, m23=Zb, m24=Wb,
		   m31=Xc, m32=Yc, m33=Zc, m34=Wc,
		   m41=Xd, m42=Yd, m43=Zd, m44=Wd }.


% Returns the (4x4) matrix whose coordinates are the specified ones, specified
% rows after rows.
%
-spec from_coordinates( coordinate(), coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate(), coordinate() )
					  -> canonical_matrix().
from_coordinates( A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
				  A15, A16 ) ->
	#mat4{ m11=A1,  m12=A2,  m13=A3,  m14=A4,
		   m21=A5,  m22=A6,  m23=A7,  m24=A8,
		   m31=A9,  m32=A10, m33=A11, m34=A12,
		   m41=A13, m42=A14, m43=A15, m44=A16 }.



% Returns the (4x4) compact matrix obtained from specified 3x3 matrix and vector.
%
-spec from_3D( linear_3D:matrix(), linear_3D:vector() ) -> compact_matrix().
from_3D( #mat3{ m11=M11, m12=M12, m13=M13,
				m21=M21, m22=M22, m23=M23,
				m31=M31, m32=M32, m33=M33 },
		 _Vec3={ X, Y, Z } ) ->
	#cpt_mat4{ m11=M11, m12=M12, m13=M13, tx=X,
			   m21=M21, m22=M22, m23=M23, ty=Y,
			   m31=M31, m32=M32, m33=M33, tz=Z }.


% Returns the canonical form of specified (4x4) matrix.
%
-spec to_canonical( matrix() ) -> canonical_matrix().
to_canonical( #cpt_mat4{ m11=M11, m12=M12, m13=M13, tx=Tx,
						 m21=M21, m22=M22, m23=M23, ty=Ty,
						 m31=M31, m32=M32, m33=M33, tz=Tz } ) ->
	#mat4{ m11=M11, m12=M12, m13=M13, m14=Tx,
		   m21=M21, m22=M22, m23=M23, m24=Ty,
		   m31=M31, m32=M32, m33=M33, m34=Tz,
		   m41=0.0, m42=0.0, m43=0.0, m44=1.0 };

to_canonical( identity_4 ) ->
	#mat4{ m11=1.0, m12=0.0, m13=0.0, m14=0.0,
		   m21=0.0, m22=1.0, m23=0.0, m24=0.0,
		   m31=0.0, m32=0.0, m33=1.0, m34=0.0,
		   m41=0.0, m42=0.0, m43=0.0, m44=1.0 };

to_canonical( M ) when is_record( M, mat4 ) ->
	M.




% Returns the compact form of specified (4x4) matrix.
%
% Throws an exception if the specified matrix cannot be expressed as a compact
% one.
%
-spec to_compact( matrix() ) -> compact_matrix().
to_compact( M=#mat4{ m11=M11, m12=M12, m13=M13, m14=M14,
					 m21=M21, m22=M22, m23=M23, m24=M24,
					 m31=M31, m32=M32, m33=M33, m34=M34,
					 m41=M41, m42=M42, m43=M43, m44=M44 }) ->
	case is_null( M41 ) andalso is_null( M42 ) andalso is_null( M43 )
		andalso are_close( M44, 1.0 ) of

		true ->
			% Just drop the last row:
			#cpt_mat4{ m11=M11, m12=M12, m13=M13, tx=M14,
					   m21=M21, m22=M22, m23=M23, ty=M24,
					   m31=M31, m32=M32, m33=M33, tz=M34 };

		false ->
			trace_utils:error_fmt( "Canonical matrix~n~s~ncannot be expressed "
								   "as a compact one.", [ to_string( M ) ] ),

			throw( { not_compactable, M } )

	end;


to_compact( identity_4 ) ->
	#cpt_mat4{ m11=1.0, m12=0.0, m13=0.0, tx=0.0,
			   m21=0.0, m22=1.0, m23=0.0, ty=0.0,
			   m31=0.0, m32=0.0, m33=1.0, tz=0.0 };


to_compact( M ) when is_record( M, cpt_mat4 ) ->
	M.




% Returns a textual representation of specified (4x4) vector or matrix.
%
-spec to_string( vector() | matrix() ) -> string().
to_string( _Vector={X,Y,Z,W} ) ->
	text_utils:format( "[ ~s, ~s, ~s, ~s ]",
					   [ coord_to_string( X ), coord_to_string( Y ),
						 coord_to_string( Z ), coord_to_string( W ) ] );

to_string( _Matrix=identity_4 ) ->
	"4x4 identity";

to_string( _Matrix=#mat4{ m11=M11, m12=M12, m13=M13, m14=M14,
						  m21=M21, m22=M22, m23=M23, m24=M24,
						  m31=M31, m32=M32, m33=M33, m34=M34,
						  m41=M41, m42=M42, m43=M43, m44=M44 } ) ->
	Elements = [ M11, M12, M13, M14,
				 M21, M22, M23, M24,
				 M31, M32, M33, M34,
				 M41, M42, M43, M44 ],

	ElemStrings = [ coord_to_string( E ) || E <- Elements ],

	text_utils:format( "[ ~s, ~s, ~s, ~s ]~n"
					   "[ ~s, ~s, ~s, ~s ]~n"
					   "[ ~s, ~s, ~s, ~s ]~n"
					   "[ ~s, ~s, ~s, ~s ]~n",
					   ElemStrings );

to_string( _Matrix=#cpt_mat4{ m11=M11, m12=M12, m13=M13, tx=Tx,
							  m21=M21, m22=M22, m23=M23, ty=Ty,
							  m31=M31, m32=M32, m33=M33, tz=Tz } ) ->
	Elements = [ M11, M12, M13, Tx,
				 M21, M22, M23, Ty,
				 M31, M32, M33, Tz ],

	ElemStrings = [ coord_to_string( E ) || E <- Elements ],

	text_utils:format( "[ ~s, ~s, ~s, ~s ]~n"
					   "[ ~s, ~s, ~s, ~s ]~n"
					   "[ ~s, ~s, ~s, ~s ]~n",
					   ElemStrings ).
