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


% @doc Base module for the support of <b>reference frames</b> (frames of
% references) of any dimension.
%
% A frame of reference is a part of reference tree; it records the
% transformation between this frame and its parent frame (if any).
%
% @see https://en.wikipedia.org/wiki/Frame_of_reference for further information
%
-module(reference_frame).



% Implementation notes:
%

% https://howtos.esperide.org/reference-frame-tree.png

% For the matrix4 record:
-include("matrix4.hrl").

% For the reference_frame3 record:
-include("reference_frame3.hrl").


-type reference_frame() :: reference_frame3().
% A frame of reference, defining a coordinate system, for each of the supported
% dimensions.

-type ref() :: reference_frame().
% Shorthand of reference_frame().


-type ref_pid() :: pid().
% The PID of any kind of process implementing the ref protocol, ultimately
% equivalent in terms of semantics, once resolved, to a reference frame.


-type designated_ref() :: ref() | ref_pid().
% Any way of designating an actual reference_frame() instance.


-type ref_id() :: count().
% An identifier of a 3D reference frame.
%
% This is typically a key in an (implicit) ref3_table().
%
% The null (zero) identifier is reserved. It is used, typically by reference
% trees, to designate the (implicit) root, absolute reference frame.


-type ref_table() :: table( ref_id(), designated_ref() ).
% A table associating to a given identifier a designated reference frame.


-export_type([ reference_frame/0, ref/0,
			   ref_pid/0, designated_ref/0,
			   ref_id/0, ref_table/0 ]).


-export([ is_designated_ref/1, check_designated_ref/1,
		  designated_ref_to_string/1 ]).


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type reference_frame3() :: reference_frame3:reference_frame3().



% @doc Tells whether the specified term is a reference frame designator.
-spec is_designated_ref( term() ) -> boolean().
%is_designated_ref( Ref ) when is_record( Ref, reference_frame3 ) ->
is_designated_ref( RefId ) when is_integer( RefId ) ->
	true;

is_designated_ref( RefPid ) when is_pid( RefPid ) ->
	true;

is_designated_ref( _ ) ->
	false.


% @doc Ensures that the specified term is a reference frame designator indeed.
-spec check_designated_ref( term() ) -> designated_ref().
check_designated_ref( T ) ->
	is_designated_ref( T ) orelse throw( { not_a_designated_ref, T } ).



% @doc Returns a textual representation of the specified designated reference
% frame.
%
-spec designated_ref_to_string( designated_ref() ) -> ustring().
designated_ref_to_string( Ref3 ) when is_record( Ref3, reference_frame3 ) ->
	reference_frame3:to_string( Ref3 );

designated_ref_to_string( DesigPid ) when is_pid( DesigPid ) ->
	text_utils:format( "the reference frame held by ~w", [ DesigPid ] ).
