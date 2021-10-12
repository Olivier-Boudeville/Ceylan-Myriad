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

% Creation date: Friday, October 8, 2021.
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



-define( gltf_version_string, "2.0" ).


% Gathers all information regarding a glTF 2.0 content.
-record( gltf_content, {

	% The index of the default scene (if any) of this content:
	default_scene = undefined ::
			basic_utils:maybe( gltf_support:scene_index() ),

	% The in-order definition of all known scenes:
	scenes = [] :: [ gltf_support:scene() ],

	% The in-order definition of all known nodes:
	nodes = [] :: [ gltf_support:scene_node() ],

	% The in-order definition of all known materials:
	materials = [] :: [ gltf_support:material() ],

	% The in-order definition of all known meshes:
	meshes = [] :: [ gltf_support:mesh() ],

	% The in-order definition of all known accessors:
	accessors = [] :: [ gltf_support:accessor() ],

	% The in-order definition of all known buffers:
	buffers = [] :: [ gltf_support:buffer() ],

	% The in-order definition of all known buffer views:
	buffer_views = [] :: [ gltf_support:buffer_view() ] } ).



% A scene defined in a glTf content.
-record( gltf_scene, {

	% The name (if any) of this scene:
	name :: basic_utils:maybe( text_utils:bin_string() ),

	% The indices of the nodes of this scene:
	nodes :: [ gltf_support:node_index() ] } ).



% A node defined in a glTf content.
-record( gltf_scene_node, {

	% The name (if any) of this node:
	name :: basic_utils:maybe( text_utils:bin_string() ),

	% The index of the mesh (if any) associated to this node:
	mesh :: basic_utils:maybe( gltf_support:mesh_index() ),

	% The quaternion (if any) defining the rotation associated to this node:
	rotation :: basic_utils:maybe( quaternion:quaternion() ),

	% The translation (if any) associated to this node:
	translation :: basic_utils:maybe( vector3:vector3() ) } ).



% A mesh defined in a glTf content.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#meshes-overview
%
-record( gltf_mesh, {

	% The name (if any) of this mesh:
	name :: basic_utils:maybe( text_utils:bin_string() ),

	primitives :: [ gltf_support:primitive() ] } ).



% A primitive defined in a mesh, corresponding to the data required for GPU draw
% calls.
%
% Primitives specify one or more attributes, corresponding to the vertex
% attributes used in the draw calls.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-mesh-primitive
%
-record( gltf_primitive, {

	attributes :: gltf_support:attributes(),

	indices :: basic_utils:maybe( gltf_support:accessor_index() ),

	material :: basic_utils:maybe( gltf_support:accessor_index() ) } ).



% Defines the attributes of a primitive, corresponding to the vertex attributes
% used in the draw calls.
%
-record( gltf_attributes, {

	position   :: basic_utils:maybe( gltf_support:accessor_index() ),
	normal     :: basic_utils:maybe( gltf_support:accessor_index() ),
	tangent    :: basic_utils:maybe( gltf_support:accessor_index() ),
	texcoord_0 :: basic_utils:maybe( gltf_support:accessor_index() )

	% Also: TEXCOORD_n, COLOR_n, JOINTS_n, WEIGHTS_n.

						  } ).



% A material defined in a glTf content.
-record( gltf_material, {

	% The name (if any) of this material:
	name :: basic_utils:maybe( text_utils:bin_string() ),

	% Tells whether this mesh is double-sided:
	double_sided :: basic_utils:maybe( boolean() ),

	% The Physically-Based Rendering (PBR) metallic roughness of this material:
	pbr_metallic_roughness ::
					basic_utils:maybe( gltf_support:pbr_metallic_roughness() )

						} ).



% Describes the metallic roughtness of a material, based on the Physically-Based
% Rendering (PBR) methodology.
%
-record( gltf_pbr_metallic_roughness, {

	base_color_factor :: gui_color:render_color(),

	metallic_factor :: math_utils:factor(),

	roughness_factor :: math_utils:factor() } ).



% A typed view into a buffer view that contains raw binary data.
%
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-accessor
%
-record( gltf_accessor, {

	buffer_view :: basic_utils:maybe( gltf_support:buffer_view_index() ),

	% Specifies if the accessor’s elements are scalars, vectors, or matrices:
	type :: gltf_support:element_type(),

	% The datatype of a component of an accessor.
	component_type :: gltf_support:component_type(),

	% The number of elements referenced by this accessor:
	count :: basic_utils:count(),

	% The maximum value (if any) of each component in this accessor:
	max :: basic_utils:maybe( [ gltf_support:component_value() ] ),

	% The minimum value (if any) of each component in this accessor:
	min :: basic_utils:maybe( [ gltf_support:component_value() ] ) } ).



% A buffer of raw data, stored as a binary blob. The buffer may contain any
% combination of binary geometry, animation, skins, and images.
%
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-buffer
%
-record( gltf_buffer, {

	% The URI designating the data to be fetched for the content of this buffer:
	uri :: web_utils:uri(),

	% The total size of this buffer:
	size :: system_utils:byte_size() } ).




% A view into a buffer generally representing a subset of the buffer.
%
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-bufferview
%
-record( gltf_buffer_view, {

	% The index of the target buffer:
	buffer :: gltf_support:buffer_index(),

	% The byte offset of the beginning of this view compared to the beginning of
	% its buffer:
	%
	offset  :: basic_utils:maybe( system_utils:byte_offset() ),

	% The size of this view into its buffer:
	size :: system_utils:byte_size() } ).
