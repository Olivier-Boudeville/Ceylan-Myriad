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



-define( gltf_version, <<"2.0">> ).


% Gathers all information regarding a glTF 2.0 content.
-record( gltf_content, {

	% The default scene (if any) of this content:
	default_scene = undefined :: maybe( gltf_support:scene_index() ),

	% The in-order definition of all known scenes:
	scenes = [] :: [ gltf_support:scene() ],

	% The in-order definition of all known nodes:
	nodes = [] :: [ gltf_support:scene_node() ],

	% The in-order definition of all known materials:
	materials = [] :: [ gltf_support:material() ]

} ).



% A scene defined in a glTf content.
-record( gltf_scene, {

	% The name of this scene.
	name :: text_utils:ustring(),

	% The nodes of this scene:
	nodes :: [ gltf_support:node_index() ]

					   } ).



% A node defined in a glTf content.
-record( gltf_scene_node, {

	% The name of this node.
	name :: text_utils:ustring(),

	% The mesh (if any) associated to this node.
	mesh :: gltf_support:mesh(),

	rotation :: maybe( quaternion:quaternion() ),
	% The quaternion (if any) defining the rotation associated to this node.

	translation :: maybe( vector3:vector3() ) } ).



% A mesh defined in a glTf content.
-record( gltf_mesh, {

	% The name of this mesh.
	name :: text_utils:ustring()

 } ).



% A material defined in a glTf content.
-record( gltf_material, {

	% The name of this mesh.
	name :: text_utils:ustring(),

	% Tells whether this mesh is double-sided:
	double_sided :: boolean(),

	% The Physically-Based Rendering (PBR) metallic roughness of this material:
	pbr_metallic_roughness :: gltf_support:pbr_metallic_roughness()

 } ).



% Describes the metallic roughtness of a material, based on the Physically-Based
% Rendering (PBR) methodology.
%
-record( gltf_pbr_metallic_roughness, {

	base_color_factor :: gui_color:render_color(),

	metallic_factor :: math_utils:factor(),

	roughness_factor :: math_utils:factor() } ).




% A buffer of raw data.
-record( gltf_buffer, {

	% The URI designating the data to be fetched.
	uri :: web_utils:uri(),

	% The total size of this buffer.
	size :: system_utils:byte_size() } ).




% A view onto a given buffer.
-record( gltf_buffer_view, {

	% The index of the target buffer.
	buffer :: gltf_support:buffer_index(),

	% The byte offset of the beginning of this view compared to the beginning of
	% its buffer.
	%
	offset = 0 :: system_utils:byte_offset(),

	% The size in byte of this view into its buffer.
	size :: system_utils:byte_size() } ).
