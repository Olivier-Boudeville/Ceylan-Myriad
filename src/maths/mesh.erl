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
% Creation date: Saturday, November 13, 2021.


% @doc Gathering of various facilities for <b>mesh</b> management, to define the
% geometry of 3D objects.
%
% See `mesh_test.erl' for the corresponding test.
%
-module(mesh).

% For the mesh record:
-include("mesh.hrl").


-type mesh() :: #mesh{}.
% Describes a mesh, convex or not.


-type vertex_indice() :: indice().
% The indice of a vertex in a container thereof.


-type face() :: [ vertex_indice() ].
% Describes the face of a mesh, based on a list of vertices (often 3 of them, to
% define a triangle).


-type normal_type() :: 'per_vertex' | 'per_face'.
% Defines to which geometric element a normal corresponds.


-type coloring_type() :: 'per_vertex' | 'per_face'.
% Defines how an (uniform) coloring shall be applied to a geometry.

-type texture_coordinate() :: point2:any_point2().
% A (2D) texture coordinate.


-type rendering_info() :: 'none'
					  | { 'wireframe', HiddenFaceRemoval :: boolean() }
					  | { 'color', coloring_type(), [ render_rgb_color() ] }.
% | { 'texture', ...
% Defines how a mesh shall be rendered.



-type index() :: basic_utils:positive_index().
% An index of an element (ex: vertex, normal, texture coordinate), typically in
% a data container such as a list or a binary buffer.


-type indexed_triangle() :: { index(), index(), index() }.
% Made of the corresponding three vertices.


-export_type([ mesh/0, face/0, vertex_indice/0, normal_type/0,
			   coloring_type/0, texture_coordinate/0,
			   rendering_info/0,
			   index/0, indexed_triangle/0 ]).


% For the right_cuboid, sphere records and al:
-include("bounding_box3.hrl").


% Construction-related section.
-export([ create_mesh/5 ]).


% Operations on meshes.
-export([ %get_diameter/1, get_smallest_enclosing_rectangle/1, get_area/1,
		  %is_in_clockwise_order/1, is_convex/1,
		  %render/2,
		  to_string/1, to_compact_string/1 ]).


% Color-related section.
%-export([ set_edge_color/2, get_edge_color/1,
%		  set_fill_color/2, get_fill_color/1 ]).


% Bounding-box related section.
%-export([ update_bounding_box/2 ]).


% Shorthands:

%-type option_list() :: option_list:option_list().

-type ustring() :: text_utils:ustring().


-type render_rgb_color() :: gui_color:render_rgb_color().
%-type canvas() :: gui:canvas().

%-type distance() :: linear:distance().
%-type square_distance() :: linear:square_distance().

-type indice() :: linear:indice().
-type vertex3() :: point3:vertex3().

-type unit_normal3() :: vector3:unit_normal3().



% Construction-related section.


% @doc Returns a new mesh whose vertices, faces, normals (of specified type),
% rendering information are the specified ones, with no specific bounding-box
% set.
%
-spec create_mesh( [ vertex3() ], [ face() ], normal_type(),
				   [ unit_normal3() ], rendering_info() ) -> mesh().
create_mesh( Vertices, Faces, NormalType, Normals, RenderingInfo ) ->

	cond_utils:if_defined( myriad_check_mesh,
						   vector3:check_unit_vectors( Normals ) ),

	#mesh{ vertices=Vertices,
		   faces=Faces,
		   normal_type=NormalType,
		   normals=Normals,
		   rendering_info=RenderingInfo }.



% Operations on meshes.



% @doc Returns a (rather full) textual description of the specified mesh.
-spec to_string( mesh() ) -> ustring().
to_string( #mesh{ vertices=Vertices,
				  faces=Faces,
				  normal_type=NormalType,
				  normals=Normals,
				  rendering_info=RenderingInfo,
				  bounding_box=MaybeBoundingBox } ) ->

	BBStr = case MaybeBoundingBox of

		undefined ->
			"none available";

		BB ->
			bounding_box3:to_string( BB )

	end,

	text_utils:format( "mesh defined by:~n"
	  " - ~B vertices: ~w~n"
	  " - ~B faces: ~w~n"
	  " - ~B ~ts normals: ~w~n"
	  " - ~ts"
	  " - bounding-box: ~ts~n",
	  [ length( Vertices ), Vertices, length( Faces ), Faces,
		length( Normals ), normal_type_to_string( NormalType ), Normals,
		rendering_info_to_string( RenderingInfo ), BBStr ] ).



% @doc Returns a compact textual description of the specified mesh.
-spec to_compact_string( mesh() ) -> ustring().
to_compact_string( #mesh{ vertices=Vertices,
						  faces=Faces,
						  normal_type=NormalType,
						  normals=Normals,
						  rendering_info=RenderingInfo,
						  bounding_box=MaybeBoundingBox } ) ->

	BBStr = case MaybeBoundingBox of

		undefined ->
			"none available";

		BB ->
			bounding_box3:to_string( BB )

	end,

	text_utils:format( "mesh with ~B vertices, ~B faces, "
		"~B ~ts normals, with ~ts and ~ts~n",
	  [ length( Vertices ), length( Faces ), length( Normals ),
		normal_type_to_string( NormalType ),
		rendering_info_to_compact_string( RenderingInfo ), BBStr ] ).



% @doc Returns a (rather full) textual description of the specified rendering
% information.
%
-spec rendering_info_to_string( rendering_info() ) -> ustring().
rendering_info_to_string( _RI=none ) ->
	"no rendering set";

rendering_info_to_string( _RI={ wireframe, HiddenFaceRemoval } ) ->
	"wireframe rendering (" ++ case HiddenFaceRemoval of
									true -> "";
									false -> "no "
							   end ++ "hidden-face removal";

rendering_info_to_string( _RI={ color, ColoringType, Colors } ) ->
	case ColoringType of
		per_vertex -> "per-vertex";
		per_face -> "per-face"
	end ++ text_utils:format( " rendering with ~B colors: ~w",
							  [ length( Colors ), Colors ] ).



% @doc Returns a compact textual description of the specified rendering
% information.
-spec rendering_info_to_compact_string( rendering_info() ) -> ustring().
rendering_info_to_compact_string( _RI=none ) ->
	"no rendering set";

rendering_info_to_compact_string(
						_RI={ wireframe, _HiddenFaceRemoval=true } ) ->
	"culled wireframe rendering";

rendering_info_to_compact_string(
						_RI={ wireframe, _HiddenFaceRemoval=false } ) ->
	"unculled wireframe rendering";

rendering_info_to_compact_string( _RI={ color, ColoringType, Colors } ) ->
	case ColoringType of
		per_vertex -> "per-vertex";
		per_face -> "per-face"
	end ++ text_utils:format( " rendering with ~B colors: ~w",
							  [ length( Colors ), Colors ] ).




-spec normal_type_to_string( normal_type() ) -> ustring().
normal_type_to_string( per_vertex ) ->
	"per-vertex";

normal_type_to_string( per_face ) ->
	"per-face".



% Bounding-box related section.


% @doc Updates, for the specified mesh, its internal bounding-box, with
% regard to the specified bounding-box algorithm.
%
% Returns a mesh with updated information.
%
% @end
%
% The lazy circle bounding box is fast to determine, but not optimal:
%-spec update_bounding_box( bounding_algorithm(), mesh() ) -> mesh().
%update_bounding_box( lazy_circle, Mesh ) ->

%	CircleBBox = bounding_box2:get_lazy_circle_box( Mesh#mesh.vertices ),

%	Mesh#mesh{ bounding_box=CircleBBox }.




% Helper functions.
