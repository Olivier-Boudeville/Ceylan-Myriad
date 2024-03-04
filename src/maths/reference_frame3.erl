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
% Creation date: Saturday, March 2, 2024.


% @doc Module implementing the support for <b>3D reference frames</b> (frames of
% references), based on 4x4 transformations.
%
% @see https://en.wikipedia.org/wiki/Frame_of_reference for further information
% @see transform4
%
-module(reference_frame3).



% Implementation notes:
%



% For the corresponding record:
-include("matrix4.hrl").

% For the transform4 record:
-include("reference_frame3.hrl").

-type reference_frame3() :: #reference_frame3{}.
% A 3D frame of reference, defining a coordinate system.

-type ref3() :: reference_frame3().
% Shorthand of reference_frame3().


-type ref3_pid() :: pid().
% The PID of any kind of process implementing the ref3 protocol, ultimately
% equivalent in terms of semantics to a 3D reference frame.


-type ref3_designator() :: ref3() | ref3_pid().
% Any way of designating an actual reference_frame3() instance.


-type ref3_id() :: count().
% An identifier of a reference_frame3() instance.
%
% This is typically a key in an (implicit) ref3_table().


-type ref3_table() :: table( ref3_id(), ref3_designator() ).
% A table associating a 3D reference designator to a given identifier thereof.


-export_type([ reference_frame3/0, ref3/0,
			   ref3_pid/0, ref3_designator/0,
			   ref3_id/0, ref3_table/0 ]).


-export([ new/1, new/2,
		  to_string/1 ] ).


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type transform4() :: transform4:transform4().



% @doc Creates an absolute (3D) reference frame, based on the specified
% transformation.
%
-spec new( transform4() ) -> reference_frame3().
new( Transf4 ) ->

	cond_utils:if_defined( myriad_debug_ref_frames,
						   transform4:check_type( Transf4 ) ),

	% Parentless:
	#reference_frame3{ transform=Transf4 }.



% @doc Creates a (3D) reference frame, based on the specified transformation
% relative to the designated parent reference frame.
%
-spec new( transform4(), ref3_designator() ) -> reference_frame3().
new( Transf4, ParentRefDesig3 ) ->

	cond_utils:if_defined( myriad_debug_ref_frames,
		begin
			transform4:check_type( Transf4 ),
			reference_frame:check_ref_designator( ParentRefDesig3 )
		end ),

	#reference_frame3{ parent=ParentRefDesig3, transform=Transf4 }.




% @doc Returns a textual representation of the specified (3D) reference frame
% designator.
%
-spec ref3_designator_to_string( ref3_designator() ) -> ustring().
ref3_designator_to_string( DesigId ) when is_integer( DesigId ) ->
	text_utils:format( "the 3D reference frame #~B", [ DesigId ] );

ref3_designator_to_string( DesigPid ) when is_pid( DesigPid ) ->
	text_utils:format( "the 3D reference frame ~w", [ DesigPid ] ).



% @doc Returns a textual representation of the specified (3D) reference frame.
-spec to_string( reference_frame3() ) -> ustring().
to_string( #reference_frame3{ parent=MaybeParent, transform=Transf4 } ) ->

	case MaybeParent of

		undefined ->
			"absolute 3D reference frame";

		ParentDesignator ->
			text_utils:format( "3D reference frame defined relatively to ~ts",
				[ ref3_designator_to_string( ParentDesignator ) ] )

	end ++ ", based on a " ++ transform4:to_string( Transf4 ).
