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
% Creation date: Monday, October 4, 2021.
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% @doc Gathering of various facilities about the <b>glTF 2.0</b> file format (GL
% Transmission Format).
%
% See [https://en.wikipedia.org/wiki/GlTF].
%
-module(gltf_support).


-type enum() :: pos_integer().
% An enumeration, in the glTf sense.


% For the various gltf_* records:
-include("gltf_support.hrl").


-type content() :: #gltf_content{}.
% Gathers all information regarding a glTF 2.0 content.



-type scene_index() :: zero_index().
% Index of a scene in a glTf content.

-type scene() :: #gltf_scene{}.
% A scene defined in a glTf content.



-type node_index() :: zero_index().
% Index of a node in a glTf content.


% node/0 being a builtin:
-type scene_node() :: #gltf_scene_node{}.
% A node defined in a glTf content.



-type mesh_index() :: zero_index().
% Index of a mesh in a glTf content.


-type mesh() :: #gltf_mesh{}.
% A mesh defined in a glTf content.


-type primitive_index() :: zero_index().
% Index of a primitive in a glTf mesh.


-type primitive() :: #gltf_primitive{}.
% A primitive defined in a mesh in a glTf content, corresponding to the data
% required for GPU draw calls.


-type attributes() :: #gltf_attributes{}.
% The attributes of a primitive, corresponding to the vertex attributes used in
% the draw calls.


-type material_index() :: zero_index().
% Index of a material in a glTf content.


-type material() :: #gltf_material{}.
% A material defined in a glTf content.


-type pbr_metallic_roughness() :: #gltf_pbr_metallic_roughness{}.
% Describes the metallic roughness of a material, based on the Physically-Based
% Rendering (PBR) methodology.



-type light_index() :: zero_index().
% Index of a light (actually: a node, as no specific type exists for light) in a
% glTf content.


-type light() :: #gltf_scene_node{}.
% A light defined in a glTf content.
%
% As no specific type exists for light, it is a mere glTf node.



-type camera_type_index() :: zero_index().
% Index of a camera type in a glTf content.


-type camera_type() :: #gltf_orthographic_camera{} | #gltf_perspective_camera{}.
% A type of camera defined in a glTf content.
%
% This corresponds to a camera type rather than a camera instance, as the actual
% cameras are created based on nodes referring to a camera type.


-type camera_node_index() :: node_index().
% Index of a node of a camera in a glTf content.



-type buffer_index() :: zero_index().
% Index of a buffer in a glTf content.


-type buffer() :: #gltf_buffer{}.
% A buffer of raw data, whose elements could be vertex indices, vertex
% attributes, animation keyframes, etc.



-type buffer_view_index() :: zero_index().
% Index of a buffer view in a glTf content.


-type buffer_view() :: #gltf_buffer_view{}.
% A view onto a given buffer.



-type accessor_index() :: zero_index().
% Index of an accessor in a glTf content.


-type accessor() :: #gltf_accessor{}.
% A typed view into a buffer view that contains raw binary data.



-type element_type() :: 'scalar' | 'vector2' | 'vector3' | 'vector4'
					  | 'matrix2' | 'matrix3' | 'matrix4'.
% Specifies if the glTf elements of an accessor are scalars, vectors, or
% matrices.
%
% The (Myriad-defined) datatype of a component of that element is to be
% specified with component_type/0.


-type gltf_element_type() :: bin_string().
% Lower-level glTf specification of the datatype of a component.



-type component_type() :: type_utils:low_level_type().
% The datatype of a component of an accessor, for instance 'uint16'.

-type gltf_component_type() :: enum().
% A glTf lower-level type identifier. Ex: '5120' for sint8.


-type component_value() :: number().
% The value of a component of an accessor.


-type gltf_topology_type() :: enum().
% Lower-level glTf specification of the datatype of a component.


-type topology() :: [ indexed_triangle() ].
% An index-based actual topology of a mesh.


-type buffer_view_target() :: 'array_buffer' | 'element_array_buffer'.
% The hint representing the intended GPU buffer type to use with this buffer
% view.


-type gltf_buffer_view_target() :: enum().
% The glTf lower-level hint representing the intended GPU buffer type to use
% with this buffer view.


-type generator_name() :: ustring().
% The name chosen for this glTf generator.


-type basic_content_settings() :: { scene_index(), material_index(),
		light_index(), camera_type_index(), camera_node_index(), content() }.
% All settings corresponding to a basic content.


-export_type([ enum/0, content/0,
			   scene_index/0, scene/0,
			   node_index/0, scene_node/0,
			   mesh_index/0, mesh/0,
			   primitive_index/0, primitive/0,
			   attributes/0,
			   material_index/0, material/0,
			   pbr_metallic_roughness/0,
			   light_index/0, light/0,
			   camera_type_index/0, camera_type/0, camera_node_index/0,
			   buffer_index/0, buffer/0,
			   buffer_view_index/0, buffer_view/0,
			   accessor_index/0, accessor/0,
			   element_type/0, gltf_element_type/0,
			   component_type/0, gltf_component_type/0,
			   component_value/0,
			   gltf_topology_type/0, topology/0,
			   buffer_view_target/0, gltf_buffer_view_target/0,
			   generator_name/0, basic_content_settings/0 ]).


-export([ get_blank_content/0, get_basic_content/0,

		  % Basic additions:

		  add_basics_to_content/1,

		  add_basic_mesh_to_content/2,
		  add_metallic_material_to_content/1,
		  add_basic_light_to_content/1,
		  add_basic_camera_to_content/1, add_camera_to_content/3,
		  add_full_scene_to_content/1,


		  % Generic setters:

		  set_default_scene/2,

		  add_scene_to_content/2,
		  add_node_to_content/2, add_light_to_content/2,
		  add_mesh_to_content/2, add_primitive_to_mesh/2,
		  add_material_to_content/2, add_camera_type_to_content/2,
		  add_accessor_to_content/2,
		  add_buffer_to_content/2, add_buffer_view_to_content/2,


		  % Basic, default glTf elements:

		  get_basic_mesh/0,
		  get_metallic_material/0,
		  get_basic_camera_settings/0, get_basic_camera_type/0,
		  get_basic_orthographic_camera_type/0,
		  get_basic_perspective_camera_type/0,
		  get_basic_camera_node/0,

		  write_gltf_content/3, write_gltf_content/4,
		  read_gltf_content/2,

		  add_primitive/7,
		  decode_primitive/4,
		  decode_vertices/5,

		  generate_buffer/3,
		  extract_points/4, extract_vectors/4,

		  file_to_gltf_buffer_embedded/1, bin_to_gltf_buffer_embedded/1,
		  gltf_buffer_embedded_to_bin/1,

		  gltf_content_to_json/3, json_to_gltf_content/2,

		  get_element_type_associations/0,
		  element_type_to_gltf/1, gltf_to_element_type/1,

		  get_component_type_associations/0,
		  component_type_to_gltf/1, gltf_to_component_type/1,

		  get_topology_type_associations/0,
		  topology_type_to_gltf/1, gltf_to_topology_type/1,

		  get_buffer_view_target_associations/0,
		  buffer_view_target_to_gltf/1, gltf_to_buffer_view_target/1,

		  triangles_to_indices/1, indices_to_triangles/1 ]).



% Shorthands:

-type count() :: basic_utils:count().

-type zero_index() :: basic_utils:zero_index().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type any_file_path() :: file_utils:any_file_path().

-type byte_size() :: system_utils:byte_size().

-type bijective_table( F, S ) :: bijective_table:bijective_table( F, S ).

-type json() :: json_utils:json().
-type json_term() :: json_utils:json_term().
-type parser_state() :: json_utils:parser_state().


-type dimension() :: linear:dimension().
-type indice() :: linear:indice().
-type indexed_triangle() :: linear:indexed_triangle().

% Default is 'triangles':
-type topology_type() :: linear_2D:topology_type().


-type coordinate() :: linear:coordinate().

-type specialised_point() :: linear:specialised_point().
-type specialised_vector() :: linear:specialised_vector().

-type specialised_vertex() :: linear:specialised_vertex().

-type specialised_normal() :: linear:specialised_normal().

-type specialised_texture_coordinates() ::
		linear:specialised_texture_coordinates().

-type specialised_type() :: linear:specialised_type().


-type scalar() :: linear:scalar().

-type point2() :: point2:point2().
-type point3() :: point3:point3().
-type point4() :: point4:point4().

-type vector2() :: vector2:vector2().
-type vector3() :: vector3:vector3().
-type vector4() :: vector4:vector4().

-type matrix2() :: matrix2:matrix2().
-type matrix3() :: matrix3:matrix3().
-type matrix4() :: matrix4:matrix4().

%-type quaternion() :: quaternion:quaternion().


% Local types:

-type final_type() :: 'point' | 'vector'.
% As, when decoding, we prefer discriminating points (ex: vertices) and vectors
% (ex: normals).


% No indice() here (they are uint16 scalar()):
-type buffer_elements() :: scalar()
						 | point2() | point3() | point4()
						 | vector2() | vector3() | vector4()
						 | matrix2() | matrix3() | matrix4().
% The elements that can be written to or extracted from a buffer-view.


-type buffer_table() :: table( buffer_index(), binary() ).
% A table associating to a given buffer index its in-memory, decoded,
% readily-usable binary.


% Design notes:
%
% Materials are defined based on the Physically-Based Rendering (PBR)
% methodology.



% Implementation notes:
%
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#binary-data-storage
%
% All buffer data MUST use little endian byte order.
%
% Floating-point data MUST use IEEE-754 single (not double) precision format,
% hence on 32 bit.
%
% Values of NaN, +Infinity, and -Infinity MUST NOT be present.

% In order to encode/decode, we study each primitive defined a given mesh. A
% primitive defines in its attributes typically where position, normal and
% texture coordinates can be found, by designating for each the index of an
% accessor.



% So that we can use the 'table' pseudo-module, as JSON parsers rely on maps:
-define( table_type, map_hashtable ).



-define( default_generator_name, "Ceylan-Myriad glTf exporter" ).



% @doc Returns a blank glTf content.
-spec get_blank_content() -> content().
get_blank_content() ->
	#gltf_content{}.



% @doc Returns all settings regarding a basic glTf content, with defaults in
% terms of scene, material, light, camera, etc.
%
-spec get_basic_content() -> basic_content_settings().
get_basic_content() ->
	add_basics_to_content( get_blank_content() ).



% @doc Adds the basics to the specified glTf content: default scene, material,
% light, camera, etc.
%
-spec add_basics_to_content( content() ) -> basic_content_settings().
add_basics_to_content( Content ) ->

	{ MaterialIndex, MatContent } = add_metallic_material_to_content( Content ),

	{ LightNodeIndex, LightContent } = add_basic_light_to_content( MatContent ),

	{ CameraTypeIndex, CameraNodeIndex, CamContent } =
		add_basic_camera_to_content( LightContent ),

	{ SceneIndex, SceneContent } = add_full_scene_to_content( CamContent ),

	DefContent = set_default_scene( SceneIndex, SceneContent ),

	{ SceneIndex, MaterialIndex, LightNodeIndex,
	  CameraTypeIndex, CameraNodeIndex, DefContent }.



% @doc Adds a basic mesh, based on specified primitive, to the specified glTf
% content, by creating a dedicated basic node; returns the index of that mesh,
% and the updated content.
%
-spec add_basic_mesh_to_content( primitive(), content() ) ->
		{ node_index(), mesh_index(), primitive_index(), content() }.
add_basic_mesh_to_content( Primitive, Content ) ->

	% By design PrimIndex=0:
	{ PrimIndex, MeshWithPrim } =
		add_primitive_to_mesh( Primitive, get_basic_mesh() ),

	% Registers this new mesh:
	{ MeshIndex, ContentWithMesh } =
		add_mesh_to_content( MeshWithPrim, Content ),

	% Creates a suitable node referencing that new mesh:
	NodeWithMesh = set_node_mesh( MeshIndex, get_basic_node() ),

	{ NodeIndex, ContentWithNode } =
		add_node_to_content( NodeWithMesh, ContentWithMesh ),

	{ NodeIndex, MeshIndex, PrimIndex, ContentWithNode }.



% @doc Adds a basic, metallic material to the specified glTf content; returns
% the index of that material and the updated content.
%
-spec add_metallic_material_to_content( content() ) ->
								{ material_index(), content() }.
add_metallic_material_to_content( Content ) ->
	add_material_to_content( get_metallic_material(), Content ).



% @doc Adds a basic light to the specified glTf content; returns the index of
% that light and the updated content.
%
-spec add_basic_light_to_content( content() ) -> { light_index(), content() }.
add_basic_light_to_content( Content ) ->
	add_light_to_content( get_basic_light(), Content ).



% @doc Adds a basic camera to the specified glTf content; returns the index of
% that camera type and node, and the updated content.
%
-spec add_basic_camera_to_content( content() ) ->
			{ camera_type_index(), camera_node_index(), content() }.
add_basic_camera_to_content( Content ) ->
	{ BasicCamType, BasicCamNode } = get_basic_camera_settings(),
	add_camera_to_content( BasicCamType, BasicCamNode, Content ).



% @doc Adds specified camera to the specified glTf content; returns the index of
% that camera type and of a node instantiating it, and the updated content.
%
% Updates the specified node so that it references this camera type (hence
% creating an instance thereof); any previous camera is replaced.
%
-spec add_camera_to_content( camera_type(), scene_node(), content() ) ->
			{ camera_type_index(), node_index(), content() }.
add_camera_to_content( CameraType, CameraNode,
					   Content=#gltf_content{ nodes=Nodes,
											  camera_types=CameraTypes } )
  when ( is_record( CameraType, gltf_orthographic_camera )
		 orelse is_record( CameraType, gltf_perspective_camera ) )
	   andalso is_record( CameraNode, gltf_scene_node ) ->

	% As these indices start at 0:
	CameraTypeIndex = length( CameraTypes ),

	NewCameraTypes = list_utils:append_at_end( CameraType, CameraTypes ),


	CameraNodeIndex = length( Nodes ),

	UpdatedCameraNode = CameraNode#gltf_scene_node{ camera=CameraTypeIndex },

	NewNodes = list_utils:append_at_end( UpdatedCameraNode, Nodes ),

	{ CameraTypeIndex, CameraNodeIndex, Content#gltf_content{ nodes=NewNodes,
												camera_types=NewCameraTypes } }.



% @doc Adds a full scene to the specified glTf content, that is a scene
% comprising all known nodes.
%
-spec add_full_scene_to_content( content() ) -> { scene_index(), content() }.
add_full_scene_to_content( Content=#gltf_content{ nodes=Nodes } ) ->

	% Enumerate all nodes:
	NodeIndices = lists:seq( _From=0, _To=length( Nodes )-1 ),

	% All content nodes selected:
	FullScene = #gltf_scene{ name="Basic Myriad full scene",
							 nodes=NodeIndices },

	add_scene_to_content( FullScene, Content ).




% Section for generic setters of glTf elements.


% @doc Sets the specified scene as the default one (overriding any prior).
-spec set_default_scene( scene_index(), content() ) -> content().
set_default_scene( SceneIndex, Content ) ->
	Content#gltf_content{ default_scene=SceneIndex }.



% @doc Sets the specified mesh index of the specified glTf node; returns the
% updated node.
%
-spec set_node_mesh( mesh_index(), scene_node() ) -> scene_node().
set_node_mesh( MeshIndex, Node ) ->
	Node#gltf_scene_node{ mesh=MeshIndex }.



% @doc Adds specified scene to the specified glTf content; returns the index of
% that scene, and the updated content.
%
-spec add_scene_to_content( scene(), content() ) ->
								{ scene_index(), content() }.
add_scene_to_content( Scene,
					  Content=#gltf_content{ scenes=Scenes } )
							when is_record( Scene, gltf_scene ) ->

	SceneIndex = length( Scenes ),

	NewScenes = list_utils:append_at_end( Scene, Scenes ),

	{ SceneIndex, Content#gltf_content{ scenes=NewScenes } }.



% @doc Adds specified node to the specified glTf content; returns the index of
% that node, and the updated content.
%
-spec add_node_to_content( scene_node(), content() ) ->
							{ node_index(), content() }.
add_node_to_content( Node,
					 Content=#gltf_content{ nodes=Nodes } )
						when is_record( Node, gltf_scene_node ) ->

	NodeIndex = length( Nodes ),

	NewNodes = list_utils:append_at_end( Node, Nodes ),

	{ NodeIndex, Content#gltf_content{ nodes=NewNodes } }.



% @doc Adds specified light to the specified glTf content; returns the index of
% that light and the updated content.
%
% Note that lights do not exist per se for glTf: they are mere nodes.
%
-spec add_light_to_content( light(), content() ) ->
			{ light_index(), content() }.
add_light_to_content( Light,
					  Content=#gltf_content{ nodes=Nodes } )
							when is_record( Light, gltf_scene_node )->

	% As these indices start at 0:
	LightNodeIndex = length( Nodes ),

	NewNodes = list_utils:append_at_end( Light, Nodes ),

	{ LightNodeIndex, Content#gltf_content{ nodes=NewNodes } }.



% @doc Adds specified material to the specified glTf content; returns the index
% of that material, and the updated content.
%
-spec add_material_to_content( material(), content() ) ->
									{ material_index(), content() }.
add_material_to_content( Material,
						 Content=#gltf_content{ materials=Materials } )
		when is_record( Material, gltf_material ) ->

	MaterialIndex = length( Materials ),

	NewMaterials = list_utils:append_at_end( Material, Materials ),

	{ MaterialIndex, Content#gltf_content{ materials=NewMaterials } }.



% @doc Adds specified camera type to the specified glTf content; returns the
% index of that camera type, and the updated content.
%
-spec add_camera_type_to_content( camera_type(), content() ) ->
									{ camera_type_index(), content() }.
add_camera_type_to_content( CameraType,
		Content=#gltf_content{ camera_types=CameraTypes } )
  when is_record( CameraType, gltf_orthographic_camera )
	   orelse is_record( CameraType, gltf_perspective_camera ) ->

	CameraTypeIndex = length( CameraTypes ),

	NewCameraTypes = list_utils:append_at_end( CameraType, CameraTypes ),

	{ CameraTypeIndex, Content#gltf_content{ camera_types=NewCameraTypes } }.




% @doc Adds specified mesh to the specified glTf content; returns the index of
% that mesh, and the updated content.
%
-spec add_mesh_to_content( mesh(), content() ) -> { mesh_index(), content() }.
add_mesh_to_content( Mesh, Content=#gltf_content{ meshes=Meshes } )
								when is_record( Mesh, gltf_mesh ) ->

	MeshIndex = length( Meshes ),

	NewMeshes = list_utils:append_at_end( Mesh, Meshes ),

	{ MeshIndex, Content#gltf_content{ meshes=NewMeshes } }.



% @doc Adds specified primitive to the specified glTf mesh; returns the index of
% that primitive, and the updated mesh.
%
-spec add_primitive_to_mesh( primitive(), mesh() ) ->
									{ primitive_index(), mesh() }.
add_primitive_to_mesh( Primitive,
					   Mesh=#gltf_mesh{ primitives=Primitives } )
								when is_record( Primitive, gltf_primitive ) ->

	PrimitiveIndex = length( Primitives ),

	NewPrimitives = list_utils:append_at_end( Primitive, Primitives ),

	{ PrimitiveIndex, Mesh#gltf_mesh{ primitives=NewPrimitives } }.



% @doc Adds specified accessor to the specified glTf content; returns the index
% of that accessor, and the updated content.
%
-spec add_accessor_to_content( accessor(), content() ) ->
											{ accessor_index(), content() }.
add_accessor_to_content( Accessor,
						 Content=#gltf_content{ accessors=Accessors } )
					when is_record( Accessor, gltf_accessor ) ->

	AccessorIndex = length( Accessors ),

	NewAccessors = list_utils:append_at_end( Accessor, Accessors ),

	{ AccessorIndex, Content#gltf_content{ accessors=NewAccessors } }.



% @doc Adds specified buffer to the specified glTf content; returns the index of
% that buffer, and the updated content.
%
-spec add_buffer_to_content( buffer(), content() ) ->
										{ buffer_index(), content() }.
add_buffer_to_content( Buffer,
					   Content=#gltf_content{ buffers=Buffers } )
					when is_record( Buffer, gltf_buffer ) ->

	BufferIndex = length( Buffers ),

	NewBuffers = list_utils:append_at_end( Buffer, Buffers ),

	{ BufferIndex, Content#gltf_content{ buffers=NewBuffers } }.



% @doc Adds specified buffer-view to the specified glTf content; returns the
% index of that buffer-view, and the updated content.
%
-spec add_buffer_view_to_content( buffer_view(), content() ) ->
							{ buffer_view_index(), content() }.
add_buffer_view_to_content( BufferView,
							Content=#gltf_content{ buffer_views=BufferViews } )
				when is_record( BufferView, gltf_buffer_view ) ->

	BufferViewIndex = length( BufferViews ),

	NewBufferViews = list_utils:append_at_end( BufferView, BufferViews ),

	{ BufferViewIndex, Content#gltf_content{ buffer_views=NewBufferViews } }.




% Section for basic, default glTf elements.



% @doc Returns a basic, empty, unregistered scene node.
-spec get_basic_node() -> scene_node().
get_basic_node() ->
	#gltf_scene_node{ name="Basic Myriad node" }.



% @doc Returns a basic, empty mesh.
-spec get_basic_mesh() -> mesh().
get_basic_mesh() ->
	#gltf_mesh{ name="Basic Myriad mesh" }.



% @doc Returns a basic, metallic, double-sided material.
-spec get_metallic_material() -> material().
get_metallic_material() ->

	ColorCoord = 0.800000011920929,

	MetalRoughness = #gltf_pbr_metallic_roughness{
		base_color_factor={ ColorCoord, ColorCoord, ColorCoord, 1.0 },
		metallic_factor=0.0,
		roughness_factor=0.4000000059604645 },

	#gltf_material{ name="Basic Myriad metallic material",
					double_sided=true,
					pbr_metallic_roughness=MetalRoughness }.



% @doc Returns a basic light.
-spec get_basic_light() -> light().
get_basic_light() ->

	% In glTf, a light is nothing but a node:

	LightRotQuaternion = [ 0.16907575726509094,
						   0.7558803558349609,
						   -0.27217137813568115,
						   0.570947527885437 ],

	LightPosition = [ 4.076245307922363,
					  5.903861999511719,
					  -1.0054539442062378 ],

	#gltf_scene_node{ name="Basic Myriad light",
					  rotation=LightRotQuaternion,
					  translation=LightPosition }.



% @doc Returns basic camera settings: a (perspective) camera type and a node
% (not yet referencing it, as the camera type has not an index yet).
%
-spec get_basic_camera_settings() -> { camera_type(), scene_node() }.
get_basic_camera_settings() ->
	{ get_basic_camera_type(), get_basic_camera_node() }.



% @doc Returns a basic (perspective) camera type.
-spec get_basic_camera_type() -> camera_type().
get_basic_camera_type() ->
	get_basic_perspective_camera_type().




% @doc Returns a basic orthographic camera type.
-spec get_basic_orthographic_camera_type() -> camera_type().
get_basic_orthographic_camera_type() ->
	#gltf_orthographic_camera{ x_magnification=1.0,
							   y_magnification=1.0,
							   z_near_distance=0.01,
							   z_far_distance=1000.0 }.



% @doc Returns a basic perspective camera type.
-spec get_basic_perspective_camera_type() -> camera_type().
get_basic_perspective_camera_type() ->
	#gltf_perspective_camera{ aspect_ratio=1.5,
							  % About 37.8°:
							  y_field_of_view=0.660593,
							  z_near_distance=0.0,
							  % Preferring here infinite perspective:
							  z_far_distance=undefined }.



% @doc Returns a basic perspective camera node.
-spec get_basic_camera_node() -> scene_node().
get_basic_camera_node() ->

	CameraRotQuaternion = [ 0.483536034822464,
							0.33687159419059753,
							-0.20870360732078552,
							0.7804827094078064 ],

	CameraPosition = [ 7.358891487121582,
					   4.958309173583984,
					   6.925790786743164 ],

	#gltf_scene_node{ name="Basic Myriad camera node",
					  rotation=CameraRotQuaternion,
					  translation=CameraPosition }.



% @doc Writes the specified glTF content in the specified file, which is
% expected not to exist, using a default generator name and the specified state
% of the JSON parser.
%
-spec write_gltf_content( content(), any_file_path(), parser_state() ) ->
			void().
write_gltf_content( GlTfContent, OutputFilePath, ParserState ) ->
	write_gltf_content( GlTfContent, OutputFilePath, ?default_generator_name,
						ParserState ).



% @doc Writes the specified glTF content in the specified file, which is
% expected not to exist, using specified generator name and the specified state
% of the JSON parser.
%
-spec write_gltf_content( content(), any_file_path(), generator_name(),
						  parser_state() ) -> void().
write_gltf_content( GlTfContent, OutputFilePath, GeneratorName,
					ParserState ) ->

	cond_utils:if_defined( myriad_debug_gltf_support, trace_utils:debug_fmt(
		"Writing glTf content in '~ts' as '~ts'.",
		[ OutputFilePath, GeneratorName ] ) ),

	BinJsonContent = gltf_content_to_json( GlTfContent, GeneratorName,
										   ParserState ),

	file_utils:write_whole( OutputFilePath, BinJsonContent ).



% @doc Reads the glTf content defined in the specified file.
-spec read_gltf_content( any_file_path(), parser_state() ) -> content().
read_gltf_content( InputFilePath, ParserState ) ->

	cond_utils:if_defined( myriad_debug_gltf_support, trace_utils:debug_fmt(
		"Reading glTf content from '~ts'.", [ InputFilePath ] ) ),

	case file_utils:is_existing_file_or_link( InputFilePath ) of

		true ->
			ok;

		false ->
			trace_utils:error_fmt( "Error, input glTf file '~ts' not found.",
								   [ InputFilePath ] )

	end,

	BinJsonFContent = file_utils:read_whole( InputFilePath ),

	json_to_gltf_content( BinJsonFContent, ParserState ).



% @doc Returns a glTf buffer corresponding to the specified (binary) file,
% embedding the file content directly into a relevant base64-encoded URI.
%
-spec file_to_gltf_buffer_embedded( any_file_path() ) -> buffer().
file_to_gltf_buffer_embedded( FilePath ) ->

	BinContent = file_utils:read_whole( FilePath ),

	cond_utils:if_defined( myriad_debug_gltf_support, trace_utils:debug_fmt(
		"Embedding file '~ts' (size: ~B bytes) in a glTf buffer.",
		[ FilePath, size( BinContent ) ] ) ),

	bin_to_gltf_buffer_embedded( BinContent ).



% @doc Returns a glTf buffer corresponding to the specified binary, embedding
% its content directly into a relevant base64-encoded URI.
%
-spec bin_to_gltf_buffer_embedded( binary() ) -> buffer().
bin_to_gltf_buffer_embedded( BinContent ) ->

	Base64Uri = "data:application/octet-stream;base64,"
					++ base64:encode_to_string( BinContent ),

	ByteCount = size( BinContent ),

	#gltf_buffer{ uri=Base64Uri,
				  size=ByteCount }.



% @doc Returns a binary corresponding to the specified glTf buffer.
-spec gltf_buffer_embedded_to_bin( buffer() ) -> binary().
gltf_buffer_embedded_to_bin( #gltf_buffer{ uri=Base64Uri,
										   size=ByteCount } ) ->

	case Base64Uri of

		"data:application/octet-stream;base64," ++ Base64Content ->
			Bin = base64:decode( Base64Content ),

			case size( Bin ) of

				ByteCount ->
					Bin;

				OtherCount ->
					throw( { binary_decoding_failed, wrong_size,
						{ expected, ByteCount }, { got, OtherCount } } )

			end;

		_ ->
			throw( { mime_prefix_not_found, Base64Uri } )

	end.



% Subsection to convert from internal (glTf) representation to JSON.


% @doc Converts the specified glTf content into a JSON counterpart.
-spec gltf_content_to_json( content(), generator_name(), parser_state() ) ->
			json().
gltf_content_to_json( #gltf_content{ default_scene=DefaultSceneId,
									 scenes=Scenes,
									 nodes=Nodes,
									 materials=Materials,
									 camera_types=CameraTypes,
									 meshes=Meshes,
									 accessors=Accessors,
									 buffer_views=BufferViews,
									 buffers=Buffers },
					  GeneratorName, ParserState ) ->

	% We create a (table-based) json_term() according to the mapping rules:

	BinGeneratorName = text_utils:string_to_binary( GeneratorName ),
	BinGlTfVersionString = text_utils:string_to_binary( ?gltf_version_string ),

	BaseTable = table:new( [

		{ <<"asset">>, #{ <<"generator">> => BinGeneratorName,
						  <<"version">> => BinGlTfVersionString } },

		{ <<"scene">>, DefaultSceneId },

		{ <<"scenes">>, gltf_scenes_to_json( Scenes ) },

		{ <<"nodes">>, gltf_nodes_to_json( Nodes ) },

		{ <<"materials">>, gltf_materials_to_json( Materials ) },

		{ <<"cameras">>, gltf_camera_types_to_json( CameraTypes ) },

		{ <<"meshes">>, gltf_meshes_to_json( Meshes ) },

		{ <<"accessors">>, gltf_accessors_to_json( Accessors ) },

		{ <<"bufferViews">>, gltf_buffer_views_to_json( BufferViews ) },

		{ <<"buffers">>, gltf_buffers_to_json( Buffers ) } ] ),

	% Thus from json_term() to json():
	JsonContent = json_utils:to_json( BaseTable, ParserState ),

	cond_utils:if_defined( myriad_debug_gltf_support, trace_utils:debug_fmt(
		"Converted glTf content in following JSON:~n~p", [ JsonContent ] ) ),

	JsonContent.



-spec gltf_scenes_to_json( [ scene() ] ) -> json_term().
gltf_scenes_to_json( Scenes ) ->
	[ gltf_scene_to_json( S ) || S <- Scenes ].


-spec gltf_scene_to_json( scene() ) -> json_term().
gltf_scene_to_json( #gltf_scene{ name=MaybeName,
								 nodes=NodeIds } ) ->

	BaseTable = table:new( [ { <<"nodes">>, NodeIds } ] ),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) } ],
		BaseTable ).



-spec gltf_nodes_to_json( [ scene_node() ] ) -> json_term().
gltf_nodes_to_json( Nodes ) ->
	[ gltf_node_to_json( N ) || N <- Nodes ].


-spec gltf_node_to_json( scene_node() ) -> json_term().
gltf_node_to_json( #gltf_scene_node{ name=MaybeName,
									 mesh=MaybeMeshId,
									 rotation=MaybeRotQuat,
									 translation=MaybeTransVec,
									 camera=MaybeCamId } ) ->

	BaseTable = table:new(),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) },
		{ <<"mesh">>, MaybeMeshId },
		{ <<"rotation">>, MaybeRotQuat },
		{ <<"translation">>, MaybeTransVec },
		{ <<"camera">>, MaybeCamId } ], BaseTable ).



-spec gltf_materials_to_json( [ material() ] ) -> json_term().
gltf_materials_to_json( Materials ) ->
	[ gltf_material_to_json( M ) || M <- Materials ].


-spec gltf_material_to_json( material() ) -> json_term().
gltf_material_to_json( #gltf_material{ name=MaybeName,
									   double_sided=MaybeDoubleSided,
									   pbr_metallic_roughness=Roughness } ) ->

	BaseTable = table:new(),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) },
		{ <<"doubleSided">>, MaybeDoubleSided },
		{ <<"pbrMetallicRoughness">>, gltf_roughness_to_json( Roughness ) } ],
							 BaseTable ).


-spec gltf_roughness_to_json( pbr_metallic_roughness() ) -> json_term().
gltf_roughness_to_json( #gltf_pbr_metallic_roughness{
							base_color_factor=BaseRenderColor,
							metallic_factor=MetalF,
							roughness_factor=RoughF } ) ->

	table:new( [ { <<"baseColorFactor">>, tuple_to_list( BaseRenderColor ) },
				 { <<"metallicFactor">>, MetalF },
				 { <<"roughnessFactor">>, RoughF } ] ).



-spec gltf_camera_types_to_json( [ camera_type() ] ) -> json_term().
gltf_camera_types_to_json( CameraTypes ) ->
	[ gltf_camera_type_to_json( CT ) || CT <- CameraTypes ].


-spec gltf_camera_type_to_json( camera_type() ) -> json_term().
gltf_camera_type_to_json( #gltf_orthographic_camera{
								x_magnification=XMag,
								y_magnification=YMag,
								z_near_distance=ZNear,
								z_far_distance=ZFar } ) ->

	OrthoTable = table:new( [ { <<"xmag">>, XMag },
							  { <<"ymag">>, YMag },
							  { <<"znear">>, ZNear },
							  { <<"zfar">>, ZFar } ] ),

	table:new( [ { <<"type">>, <<"orthographic">> },
				 { <<"orthographic">>, OrthoTable } ] );


gltf_camera_type_to_json( #gltf_perspective_camera{
								aspect_ratio=MaybeAspectRatio,
								y_field_of_view=YFoV,
								z_near_distance=ZNear,
								z_far_distance=MaybeZFar } ) ->

	BasePerspTable = table:new( [ { <<"yfov">>, YFoV },
								  { <<"znear">>, ZNear } ] ),

	PerspTable = table:add_maybe_entries( [
					{ <<"aspectRatio">>, MaybeAspectRatio },
					{ <<"zfar">>, MaybeZFar } ], BasePerspTable ),

	table:new( [ { <<"type">>, <<"perspective">> },
				 { <<"perspective">>, PerspTable } ] ).




-spec gltf_meshes_to_json( [ mesh() ] ) -> json_term().
gltf_meshes_to_json( Meshes ) ->
	[ gltf_mesh_to_json( M ) || M <- Meshes ].


-spec gltf_mesh_to_json( mesh() ) -> json_term().
gltf_mesh_to_json( #gltf_mesh{ name=MaybeName,
							   primitives=Primitives } ) ->

	BaseTable = table:new( [
		{ <<"primitives">>, gltf_primitives_to_json( Primitives )  } ] ),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) } ],
		BaseTable ).



-spec gltf_primitives_to_json( [ primitive() ] ) -> json_term().
gltf_primitives_to_json( Primitives ) ->
	[ gltf_primitive_to_json( P ) || P <- Primitives ].


gltf_primitive_to_json( #gltf_primitive{ attributes=Attributes,
										 indices=MaybeAccessIdx,
										 material=MaybeMaterialIdx } ) ->

	BaseTable = table:new(
		[ { <<"attributes">>, gltf_attributes_to_json( Attributes ) } ] ),

	table:add_maybe_entries( [
		{ <<"indices">>, MaybeAccessIdx },
		{ <<"material">>, MaybeMaterialIdx } ], BaseTable ).




-spec gltf_attributes_to_json( attributes() ) -> json_term().
gltf_attributes_to_json( #gltf_attributes{ position=MaybePosition,
										   normal=MaybeNormal,
										   tangent=MaybeTangent,
										   texcoord_0=MaybeTexCoord0 } ) ->

	BaseTable = table:new(),

	table:add_maybe_entries( [
		{ <<"POSITION">>, MaybePosition },
		{ <<"NORMAL">>, MaybeNormal },
		{ <<"TANGENT">>, MaybeTangent },
		{ <<"TEXCOORD_0">>, MaybeTexCoord0 } ], BaseTable ).



-spec gltf_accessors_to_json( [ accessor() ] ) -> json_term().
gltf_accessors_to_json( Accessors ) ->
	[ gltf_accessor_to_json( A ) || A <- Accessors ].


gltf_accessor_to_json( #gltf_accessor{ buffer_view=MaybeBufferViewIndex,
									   element_type=ElemType,
									   component_type=ComponentType,
									   count=ElemCount,
									   max=MaybeMax,
									   min=MaybeMin } ) ->
	BaseTable = table:new( [
		{ <<"type">>, element_type_to_gltf( ElemType ) },
		{ <<"componentType">>, component_type_to_gltf( ComponentType ) },
		{ <<"count">>, ElemCount } ] ),

	table:add_maybe_entries( [
		{ <<"bufferView">>, MaybeBufferViewIndex },
		{ <<"max">>, MaybeMax },
		{ <<"min">>, MaybeMin } ], BaseTable ).





-spec gltf_buffer_views_to_json( [ buffer_view() ] ) -> json_term().
gltf_buffer_views_to_json( BufferViews ) ->
	[ gltf_buffer_view_to_json( BV ) || BV <- BufferViews ].


-spec gltf_buffer_view_to_json( buffer_view() ) -> json_term().
gltf_buffer_view_to_json( #gltf_buffer_view{ buffer=BufferIdx,
											 offset=MaybeOffset,
											 size=Size } ) ->

	BaseTable = table:new( [ { <<"buffer">>, BufferIdx },
							 { <<"byteLength">>, Size } ] ),

	table:add_maybe_entries( [ { <<"byteOffset">>, MaybeOffset } ], BaseTable );

gltf_buffer_view_to_json( Other ) ->
	throw( { unexpected_buffer_view, Other } ).




-spec gltf_buffers_to_json( [ buffer() ] ) -> json_term().
gltf_buffers_to_json( Buffers ) ->
	[ gltf_buffer_to_json( B ) || B <- Buffers ].


-spec gltf_buffer_to_json( buffer() ) -> json_term().
gltf_buffer_to_json( #gltf_buffer{ uri=UriStr,
								   size=Size } ) ->

	BinUri = text_utils:string_to_binary( UriStr ),

	BaseTable = table:new( [ { <<"byteLength">>, Size },
							 { <<"uri">>, BinUri } ] ),

	table:add_maybe_entries( [], BaseTable ).




% Subsection to convert from JSON to internal (glTf) representation.


% @doc Converts the specified JSON content into an (internal) glTf counterpart.
-spec json_to_gltf_content( json(), parser_state() ) -> content().
json_to_gltf_content( _JSonContent, _ParserState ) ->

	% Use gltf_to_*

	DefaultSceneId = fixme,
	Scenes  = fixme,
	Nodes = fixme,
	Materials = fixme,
	Meshes = fixme,
	Accessors = fixme,
	BufferViews = fixme,
	Buffers  = fixme,

	throw( not_implemented_yet ),

	#gltf_content{ default_scene=DefaultSceneId,
				   scenes=Scenes,
				   nodes=Nodes,
				   materials=Materials,
				   meshes=Meshes,
				   accessors=Accessors,
				   buffer_views=BufferViews,
				   buffers=Buffers }.



% Conversions between lower-level (glTf) and higher-level (Myriad) symbols.
%
% They are done through bijective tables whose first entries are Myriad ones,
% and whose second ones are glTf ones.



% @doc Returns the two-way associations regarding Myriad/glTf element types.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#accessor-data-types
%
-spec get_element_type_associations() ->
			bijective_table( element_type(), gltf_element_type() ).
get_element_type_associations() ->
	bijective_table:new( [ { scalar, <<"SCALAR">> },
						   { vector2, <<"VEC2">> },
						   { vector3, <<"VEC3">> },
						   { vector4, <<"VEC4">> },
						   { matrix2, <<"MAT2">> },
						   { matrix3, <<"MAT3">> },
						   { matrix4, <<"MAT4">> } ] ).


% @doc Converts a (Myriad-level) component type into a (lower-level) glTf one.
-spec element_type_to_gltf( element_type() ) -> gltf_element_type().
element_type_to_gltf( _ElemType=point2 ) ->
	vector2;

element_type_to_gltf( _ElemType=point3 ) ->
	vector3;

element_type_to_gltf( ElemType ) ->
	bijective_table:get_second_for( ElemType, get_element_type_associations() ).


% @doc Converts a (lower-level) glTf component type into a Myriad-level one.
-spec gltf_to_element_type( gltf_element_type() ) -> element_type().
gltf_to_element_type( GltfElemType ) ->
	bijective_table:get_first_for( GltfElemType,
								   get_element_type_associations() ).



% @doc Returns the two-way associations regarding Myriad/glTf component types.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#accessor-data-types
%
-spec get_component_type_associations() ->
			bijective_table( component_type(), gltf_component_type() ).
get_component_type_associations() ->
	bijective_table:new( [ { uint8, 5121 },
						   { sint8, 5120 },
						   { uint16, 5123 },
						   { sint16, 5122 },
						   % No sint32 supported by glTf.
						   { uint32, 5125 },
						   { float32, 5126} ] ).


% @doc Converts a (Myriad-level) component type into a (lower-level) glTf one.
-spec component_type_to_gltf( component_type() ) -> gltf_component_type().
component_type_to_gltf( ComponentType ) ->
	bijective_table:get_second_for( ComponentType,
									get_component_type_associations() ).


% @doc Converts a (lower-level) glTf component type into a Myriad-level one.
-spec gltf_to_component_type( gltf_component_type() ) -> component_type().
gltf_to_component_type( GltfComponentType ) ->
	bijective_table:get_first_for( GltfComponentType,
								   get_component_type_associations() ).




% @doc Returns the two-way associations regarding Myriad/glTf topology types.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#_mesh_primitive_mode
%
-spec get_topology_type_associations() ->
			bijective_table( topology_type(), gltf_topology_type() ).
get_topology_type_associations() ->
	bijective_table:new( [ { points, 0 },
						   { lines, 1 },
						   { line_loop, 2 },
						   { line_strip, 3 },
						   { triangles, 4 },
						   { triangle_strip, 5 },
						   { triangle_fan, 6 } ] ).


% @doc Converts a (Myriad-level) topology type into a (lower-level) glTf one.
-spec topology_type_to_gltf( topology_type() ) -> gltf_topology_type().
topology_type_to_gltf( TopologyType ) ->
	bijective_table:get_second_for( TopologyType,
									get_topology_type_associations() ).


% @doc Converts a (lower-level) glTf component type into a Myriad-level one.
-spec gltf_to_topology_type( gltf_topology_type() ) -> topology_type().
gltf_to_topology_type( GltfTopologyTypeType ) ->
	bijective_table:get_first_for( GltfTopologyTypeType,
								   get_topology_type_associations() ).



% @doc Returns the two-way associations regarding Myriad/glTf buffer-view
% targets.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#_buffer_view_target
%
-spec get_buffer_view_target_associations() ->
			bijective_table( buffer_view_target(), gltf_buffer_view_target() ).
get_buffer_view_target_associations() ->
	bijective_table:new( [ { array_buffer, 34962 },
						   { element_array_buffer, 34963 } ] ).


% @doc Converts a (Myriad-level) buffer-view target into a (lower-level) glTf
% one.
%
-spec buffer_view_target_to_gltf( buffer_view_target() ) ->
			gltf_buffer_view_target().
buffer_view_target_to_gltf( BufferViewTarget ) ->
	bijective_table:get_second_for( BufferViewTarget,
									get_buffer_view_target_associations() ).


% @doc Converts a (lower-level) glTf buffer-view target into a Myriad-level one.
-spec gltf_to_buffer_view_target( gltf_buffer_view_target() ) ->
			buffer_view_target().
gltf_to_buffer_view_target( GltfBufferViewTarget ) ->
	bijective_table:get_first_for( GltfBufferViewTarget,
								   get_buffer_view_target_associations() ).




% @doc Decodes the specified primitive of the specified mesh, as defined in the
% specified glTf content: returns its vertices, normals and texture coordinates.
%
-spec decode_primitive( mesh_index(), primitive_index(), content(),
						buffer_table() ) ->
			{ [ specialised_vertex() ], [ specialised_normal() ],
			  [ specialised_texture_coordinates() ], [ indice() ],
			  buffer_table() }.
decode_primitive( MeshIndex, PrimitiveIndex, #gltf_content{
												meshes=Meshes,
												accessors=Accessors,
												buffers=Buffers,
												buffer_views=BufferViews },
				  BufferTable ) ->

	trace_utils:debug_fmt( "Decoding primitive ~B of mesh ~B.",
						   [ PrimitiveIndex, MeshIndex ] ),

	_Mesh = #gltf_mesh{ primitives=Primitives } =
					list_utils:get_element_at( Meshes, MeshIndex+1 ),

	Prim = #gltf_primitive{ attributes=Attributes } =
		list_utils:get_element_at( Primitives, PrimitiveIndex+1 ),


	{ Vertices, VertBufferTable } = case Attributes#gltf_attributes.position of

		undefined ->
			BufferTable;

		PositionAccessorIndex ->
			VertP = { Verts, _VertBuffTable } = decode_vertices(
				PositionAccessorIndex, Accessors, Buffers, BufferViews,
				BufferTable ),

			trace_utils:debug_fmt( "The ~B extracted vertices are:~n~p",
								   [ length( Verts ), Verts ] ),

			VertP

	end,


	{ Normals, NormBufferTable } = case Attributes#gltf_attributes.normal of

		undefined ->
			VertBufferTable;

		NormalPositionAccessorIndex ->

			NormP = { Norms, _NormBuffTable } = decode_normals(
				NormalPositionAccessorIndex, Accessors, Buffers, BufferViews,
				VertBufferTable ),

			trace_utils:debug_fmt( "The ~B extracted normals are:~n~p",
								   [ length( Norms ), Norms ] ),

			NormP

	end,


	% No gltf_attributes.tangent managed here.


	{ TexCoords, Tex0BufferTable } =
			case Attributes#gltf_attributes.texcoord_0 of

		undefined ->
			NormBufferTable;

		TexCoord0AccessorIndex ->

			TexP = { TexCs, _Tex0BuffTable } = decode_texture_coordinates(
				TexCoord0AccessorIndex, Accessors, Buffers, BufferViews,
				NormBufferTable ),

			trace_utils:debug_fmt( "The ~B extracted texture coordinates "
				"are:~n~p", [ length( TexCs ), TexCs ] ),

			TexP

	end,


	{ Indices, IndicesBufferTable } = case Prim#gltf_primitive.indices of

		undefined ->
			Tex0BufferTable;

		IndicesPositionAccessorIndex ->

			InP = { Inds, _IndicesBuffTable } = decode_indices(
				IndicesPositionAccessorIndex, Accessors, Buffers, BufferViews,
				Tex0BufferTable ),

			trace_utils:debug_fmt( "The ~B extracted vertex indices are:~n~p",
								   [ length( Inds ), Inds ] ),

			InP

	end,

	{ Vertices, Normals, TexCoords, Indices, IndicesBufferTable }.



% @doc Decodes the vertices defined in the specified glTf content.
-spec decode_vertices( accessor_index(), [ accessor() ], [ buffer() ],
					   [ buffer_view() ], buffer_table() ) ->
			{ [ specialised_vertex() ], buffer_table() }.
decode_vertices( AccessorIndex, Accessors, Buffers, BufferViews,
				 BufferTable ) ->

	_PositionAccessor = #gltf_accessor{
							buffer_view=BufferViewIndex,
							element_type=AccessorElemType,
							component_type=AccessorComponentType,
							count=PointCount,
							max=PMax,
							min=PMin } =
		list_utils:get_element_at( Accessors, AccessorIndex+1 ),

	trace_utils:debug_fmt( "To decode vertices, expecting ~B ~ts elements "
		"of component type ~ts, whose minimum is ~ts and maximum is ~ts.",
		[ PointCount, AccessorElemType, AccessorComponentType,
		  point3:to_string( point3:from_vector( PMin ) ),
		  point3:to_string( point3:from_vector( PMax ) ) ] ),

	{ BinViewContent, NewBufferTable } = get_buffer_view_binary(
		BufferViewIndex, BufferViews, Buffers, BufferTable ),

	%trace_utils:debug_fmt( "Binary content of view is:~n~p",
	%                       [ BinViewContent ] ),

	{ extract_points( BinViewContent, PointCount, AccessorElemType,
					  AccessorComponentType ), NewBufferTable }.



% @doc Decodes the normals defined in the specified glTf content.
-spec decode_normals( accessor_index(), [ accessor() ], [ buffer() ],
					   [ buffer_view() ], buffer_table() ) ->
			{ [ specialised_vertex() ], buffer_table() }.
decode_normals( AccessorIndex, Accessors, Buffers, BufferViews,
				BufferTable ) ->

	_NormalAccessor = #gltf_accessor{
							buffer_view=BufferViewIndex,
							element_type=AccessorElemType,
							component_type=AccessorComponentType,
							count=VectorCount,
							max=NMax,
							min=NMin } =
		list_utils:get_element_at( Accessors, AccessorIndex+1 ),

	trace_utils:debug_fmt( "To decode normals, expecting ~B ~ts elements "
		"of component type ~ts, whose minimum is ~w and maximum is ~w.",
		[ VectorCount, AccessorElemType, AccessorComponentType,
		  NMin, NMax ] ),

	{ BinViewContent, NewBufferTable } = get_buffer_view_binary(
		BufferViewIndex, BufferViews, Buffers, BufferTable ),

	%trace_utils:debug_fmt( "Binary content of view is:~n~p",
	%                       [ BinViewContent ] ),

	{ extract_vectors( BinViewContent, VectorCount, AccessorElemType,
					   AccessorComponentType ), NewBufferTable }.



% @doc Decodes the texture coordinates defined in the specified glTf content.
-spec decode_texture_coordinates( accessor_index(), [ accessor() ],
						[ buffer() ], [ buffer_view() ], buffer_table() ) ->
			{ [ specialised_texture_coordinates() ], buffer_table() }.
decode_texture_coordinates( AccessorIndex, Accessors, Buffers, BufferViews,
							BufferTable ) ->

	_TexCoordAccessor = #gltf_accessor{
							buffer_view=BufferViewIndex,
							element_type=AccessorElemType,
							component_type=AccessorComponentType,
							count=CoordCount,
							max=TCMax,
							min=TCMin } =
		list_utils:get_element_at( Accessors, AccessorIndex+1 ),

	trace_utils:debug_fmt( "To decode texture coordinates, expecting ~B ~ts "
		"elements of component type ~ts, whose minimum is ~w "
		"and maximum is ~w.",
		[ CoordCount, AccessorElemType, AccessorComponentType,
		  TCMin, TCMax ] ),

	{ BinViewContent, NewBufferTable } = get_buffer_view_binary(
		BufferViewIndex, BufferViews, Buffers, BufferTable ),

	%trace_utils:debug_fmt( "Binary content of view is:~n~p",
	%                       [ BinViewContent ] ),

	{ extract_vectors( BinViewContent, CoordCount, AccessorElemType,
					   AccessorComponentType ), NewBufferTable }.



% @doc Decodes the indices defined in the specified glTf content.
-spec decode_indices( accessor_index(), [ accessor() ], [ buffer() ],
	[ buffer_view() ], buffer_table() ) -> { [ indice() ], buffer_table() }.
decode_indices( AccessorIndex, Accessors, Buffers, BufferViews,
				BufferTable ) ->

	_PositionAccessor = #gltf_accessor{
							buffer_view=BufferViewIndex,
							element_type=AccessorElemType,
							component_type=AccessorComponentType,
							count=PointCount,
							max=IMax,
							min=IMin } =
		list_utils:get_element_at( Accessors, AccessorIndex+1 ),

	trace_utils:debug_fmt( "To decode indices, expecting ~B ~ts elements "
		"of component type ~ts, whose minimum is ~ts and maximum is ~ts.",
		[ PointCount, AccessorElemType, AccessorComponentType, IMin, IMax ] ),

	{ BinViewContent, NewBufferTable } = get_buffer_view_binary(
		BufferViewIndex, BufferViews, Buffers, BufferTable ),

	%trace_utils:debug_fmt( "Binary content of view is:~n~p",
	%                       [ BinViewContent ] ),

	% Check:
	AccessorElemType = scalar,

	{ extract_indices( BinViewContent, PointCount, AccessorComponentType ),
	  NewBufferTable }.



% To be added later: add_pritimive( primitive(), content() ) -> content().


% @doc Encodes the specified primitive information as a new mesh into the
% specified glTF content.
%
% Directly embeds the resulting buffer; returns the index of the new mesh, the
% index of the new primitive and an updated glTf content.
%
% Does not create any node referencing that mesh.
%
-spec add_primitive( [ specialised_vertex() ], [ specialised_normal() ],
		[ specialised_texture_coordinates() ], topology_type(), topology(),
		material_index(), content() ) ->
			{ mesh_index(), primitive_index(), content() }.
add_primitive( Vertices, Normals, TexCoords, TopologyType=triangles,
			   IndexedTriangles, MaterialAccessorIndex,
			   Content=#gltf_content{ meshes=Meshes,
									  accessors=Accessors,
									  buffers=Buffers,
									  buffer_views=BufferViews } ) ->

	% Here, we rely on the following conventions:

	% - in the final buffer, first all positions are listed, then all normal,
	% then all texture coordinates, then all indices

	% As these indices start at 0:
	PrimBufferIndex = length( Buffers ),

	% - positions are referenced through PositionAccessor, referencing
	% PositionBufferView, whose elements are vector3() (hence components are
	% float())

	PositionAccessorIndex = length( Accessors ),
	PositionBufferViewIndex = length( BufferViews ),

	{ MinVec, MaxVec } = compute_gltf_extremas( Vertices ),

	PositionElementType = point3,
	PositionComponentType = float32,

	PositionCount = length( Vertices ),

	PositionAccessor = #gltf_accessor{ buffer_view=PositionBufferViewIndex,
									   element_type=PositionElementType,
									   component_type=PositionComponentType,
									   count=PositionCount,
									   min=MinVec,
									   max=MaxVec },

	PositionSize = get_size( PositionElementType, PositionComponentType,
							 PositionCount ),

	PositionOffset = 0,

	PositionBufferView = #gltf_buffer_view{ buffer=PrimBufferIndex,
											offset=PositionOffset,
											size=PositionSize },

	PositionBin = generate_buffer( PositionElementType,
								   PositionComponentType, Vertices ),


	% - normals are referenced through NormalAccessor, referencing
	% NormalBufferView, whose elements are vector3() (hence components are
	% float())

	NormalAccessorIndex = PositionAccessorIndex+1,
	NormalBufferViewIndex = PositionBufferViewIndex+1,

	NormalElementType = vector3,
	NormalComponentType = float32,

	NormalCount = length( Normals ),

	NormalAccessor = #gltf_accessor{ buffer_view=NormalBufferViewIndex,
									 element_type=NormalElementType,
									 component_type=NormalComponentType,
									 count=NormalCount },

	NormalSize = get_size( NormalElementType, NormalComponentType,
						   NormalCount ),

	NormalOffset = PositionSize,

	NormalBufferView = #gltf_buffer_view{ buffer=PrimBufferIndex,
										  offset=NormalOffset,
										  size=NormalSize },

	NormalBin = append_to_buffer( NormalElementType, NormalComponentType,
								  Normals, PositionBin ),



	% - texture coordinates are referenced through TexCoordAccessor,
	% referencing TexCoordBufferView, whose elements are vector2() (hence
	% components are float())

	TexCoordAccessorIndex = NormalAccessorIndex+1,
	TexCoordBufferViewIndex = NormalBufferViewIndex+1,

	TexCoordElementType = vector2,
	TexCoordComponentType = float32,

	TexCoordCount = length( TexCoords ),

	TexCoordAccessor = #gltf_accessor{ buffer_view=TexCoordBufferViewIndex,
									   element_type=TexCoordElementType,
									   component_type=TexCoordComponentType,
									   count=TexCoordCount },

	TexCoordSize = get_size( TexCoordElementType, TexCoordComponentType,
							 TexCoordCount ),

	TexCoordOffset = NormalOffset + NormalSize,

	TexCoordBufferView = #gltf_buffer_view{ buffer=PrimBufferIndex,
											offset=TexCoordOffset,
											size=TexCoordSize },

	TexCoordBin = append_to_buffer( TexCoordElementType, TexCoordComponentType,
									TexCoords, NormalBin ),


	% - indices are referenced through IndicesAccessor, referencing
	% IndicesBufferView, whose elements are scalar() (components are uint16)

	IndicesAccessorIndex = TexCoordAccessorIndex+1,
	IndicesBufferViewIndex = TexCoordBufferViewIndex+1,

	IndicesElementType = scalar,
	IndicesComponentType = uint16,

	Indices = triangles_to_indices( IndexedTriangles ),

	IndicesCount = length( Indices ),

	IndicesAccessor = #gltf_accessor{ buffer_view=IndicesBufferViewIndex,
									  element_type=IndicesElementType,
									  component_type=IndicesComponentType,
									  count=IndicesCount },

	IndicesSize = get_size( IndicesElementType, IndicesComponentType,
							IndicesCount ),

	IndicesOffset = TexCoordOffset + TexCoordSize,

	IndicesBufferView = #gltf_buffer_view{ buffer=PrimBufferIndex,
										   offset=IndicesOffset,
										   size=IndicesSize },

	IndicesBin = append_to_buffer( IndicesElementType, IndicesComponentType,
								   Indices, TexCoordBin ),

	ResultingBuffer = gltf_support:bin_to_gltf_buffer_embedded( IndicesBin ),

	NewBuffers = list_utils:append_at_end( ResultingBuffer, Buffers ),

	Attributes = #gltf_attributes{ position=PositionAccessorIndex,
								   normal=NormalAccessorIndex,
								   texcoord_0=TexCoordAccessorIndex },

	Primitive = #gltf_primitive{ attributes=Attributes,
								 indices=IndicesAccessorIndex,
								 material=MaterialAccessorIndex,
								 mode=TopologyType },

	% No name given, a single primitive:
	Mesh = #gltf_mesh{ primitives=[ Primitive ] },

	MeshIndex = length( Meshes ),

	NewMeshes = list_utils:append_at_end( Mesh, Meshes ),

	ExtraAccessors = [ PositionAccessor, NormalAccessor, TexCoordAccessor,
					   IndicesAccessor ],

	NewAccessors = Accessors ++ ExtraAccessors,


	ExtraBufferViews = [ PositionBufferView, NormalBufferView,
						 TexCoordBufferView, IndicesBufferView ],

	NewBufferViews = BufferViews ++ ExtraBufferViews,

	NewContent = Content#gltf_content{ meshes=NewMeshes,
									   accessors=NewAccessors,
									   buffers=NewBuffers,
									   buffer_views=NewBufferViews },

	{ MeshIndex, _PrimitiveIndex=0, NewContent }.





% @doc Returns a pair of vectors whose coordinates reflect the overall minimum
% and maximum values found in the specified list of points.
%
-spec compute_gltf_extremas( [ point3() ] ) -> { point3(), point3() }.
% No wanting to let 'undefined' go through:
compute_gltf_extremas( _Points=[] )->
	throw( no_points );

compute_gltf_extremas( Points )->

	% Cannot rely on term order, as number() is already the smallest.

	Undef3 = { undefined, undefined, undefined },

	compute_gltf_extremas( Points, Undef3, Undef3 ).



% (helper)
compute_gltf_extremas( _Points=[], MinP, MaxP ) ->
	{ tuple_to_list( MinP ), tuple_to_list( MaxP ) };


compute_gltf_extremas( _Points=[ {X,Y,Z} | T ], _MinP={ XMin, YMin, ZMin },
					   _MaxP={ XMax, YMax, ZMax } ) ->

	{ NewXMin, NewXMax } = update_coord( X, XMin, XMax ),
	{ NewYMin, NewYMax } = update_coord( Y, YMin, YMax ),
	{ NewZMin, NewZMax } = update_coord( Z, ZMin, ZMax ),

	NewMinP = { NewXMin, NewYMin, NewZMin },
	NewMaxP = { NewXMax, NewYMax, NewZMax },

	compute_gltf_extremas( T, NewMinP, NewMaxP ).



% (helper)
update_coord( C, Min, Max ) ->

	NewMin = case Min of

		undefined ->
			C;

		Min when C < Min ->
			C;

		_ ->
			Min

	end,

	NewMax = case Max of

		undefined ->
			C;

		Max when C > Max ->
			C;

		_ ->
			Max

	end,

	{ NewMin, NewMax }.



% @doc Returns the size in bytes of the specified array of elements.
-spec get_size( element_type(), component_type(), count() ) -> byte_size().
get_size( ElementType, ComponentType, Count ) ->
	linear:get_element_count( ElementType ) *
		type_utils:get_low_level_type_size( ComponentType ) * Count.



% @doc Returns a binary corresponding to the specified buffer-view.
-spec get_buffer_view_binary( buffer_view_index(), [ buffer_view() ],
			[ buffer() ], buffer_table() ) -> { binary(), buffer_table() }.
get_buffer_view_binary( BufferViewIndex, BufferViews, Buffers,
						BufferTable ) ->

	_BufferView = #gltf_buffer_view{ buffer=BufferIndex,
									 offset=MaybeViewOffset,
									 size=ViewSize }
		= list_utils:get_element_at( BufferViews, BufferViewIndex+1 ),

	ViewOffset = case MaybeViewOffset of

		undefined ->
			0;

		Offset ->
			Offset

	end,

	trace_utils:debug_fmt( "Buffer view to access buffer ~B with offset ~B, "
		"of view size ~B bytes.", [ BufferIndex, ViewOffset, ViewSize ] ),

	{ BinBufferContent, NewBufferTable } =
		get_buffer( BufferIndex, Buffers, BufferTable ),

	%trace_utils:debug_fmt( "Buffer content decoded from base-64 is:~n~p",
	%					   [ BinBufferContent ] ),

	% No stride managed:
	BinViewContent =
		binary:part( BinBufferContent, _Pos=ViewOffset, _Len=ViewSize ),

	{ BinViewContent, NewBufferTable }.



% @doc Returns the binary context of specified buffer, either cached, or decoded
% and cached once for all.
%
-spec get_buffer( buffer_index(), [ buffer() ], buffer_table() ) ->
			{ binary(), buffer_table() }.
get_buffer( BufferIndex, Buffers, BufferTable ) ->
	case table:lookup_entry( _K=BufferIndex, BufferTable ) of

		key_not_found ->
			Buffer = #gltf_buffer{ uri=Uri,
								   size=BufferSize }
				= list_utils:get_element_at( Buffers, BufferIndex+1 ),

			trace_utils:debug_fmt( "Caching, as entry ~B, buffer "
				"of size ~B bytes, whose URI is:~n~ts.",
				[ BufferIndex, BufferSize, Uri ] ),

			BinBufferContent = gltf_buffer_embedded_to_bin( Buffer ),

			% Check:
			BufferSize = size( BinBufferContent ),

			NewBufferTable = table:add_entry( BufferIndex, BinBufferContent,
											  BufferTable ),

			{ BinBufferContent, NewBufferTable };

		{ value, BinBufferContent } ->
			trace_utils:debug_fmt( "Returning buffer cached as entry ~B.",
								   [ BufferIndex ] ),
			{ BinBufferContent, BufferTable }

	end.



% @doc Generates a binary buffer corresponding to the specified elements.
-spec generate_buffer( element_type(), component_type(),
					   [ specialised_type() ] ) -> binary().
generate_buffer( ElementType, ComponentType, Elements ) ->
	append_to_buffer( ElementType, ComponentType, Elements, _AccBin= <<>> ).



% @doc Appends the binary version of the specified elements to the specified
% binary buffer.
%
-spec append_to_buffer( element_type(), component_type(),
						[ specialised_type() ], binary() ) -> binary().
append_to_buffer( _ElementType=scalar, _ComponentType=uint16, Elements,
				  Bin ) ->
	append_all_uint16_little( Elements, Bin );


append_to_buffer( ElementType, _ComponentType=float32, Elements,
				  Bin ) when ElementType =:= vector2
					  orelse ElementType =:= vector3
					  orelse ElementType =:= vector4 ->
	ComponentFloats = list_utils:flatten_once( Elements ),
	append_all_float32_little( ComponentFloats, Bin );

append_to_buffer( _ElementType=point4, _ComponentType=float32, Elements,
				  Bin ) ->
	ComponentFloats = get_point4( Elements ),
	append_all_float32_little( ComponentFloats, Bin );


append_to_buffer( _ElementType=point3, _ComponentType=float32, Elements,
				  Bin ) ->
	ComponentFloats = get_point3( Elements ),
	append_all_float32_little( ComponentFloats, Bin );

append_to_buffer( _ElementType=point2, _ComponentType=float32, Elements,
				  Bin ) ->
	ComponentFloats = get_point2( Elements ),
	append_all_float32_little( ComponentFloats, Bin ).




% @doc Returns a list of all, in-order coordinates of the specified points.
-spec get_point4( [ point4() ] ) -> [ coordinate() ].
get_point4( Elements ) ->
	get_point4( Elements, _Acc=[]  ).


% (helper)
get_point4( _Elements=[], Acc ) ->
	Acc;

get_point4( _Elements=[ {X,Y,Z,W} | T ], Acc ) ->
	% Will be reversed:
	get_point4( T, [ W, Z, Y, X | Acc ] ).



% @doc Returns a list of all, in-order coordinates of the specified points.
-spec get_point3( [ point3() ] ) -> [ coordinate() ].
get_point3( Elements ) ->
	get_point3( Elements, _Acc=[] ).


% (helper)
get_point3( _Elements=[], Acc ) ->
	Acc;

get_point3( _Elements=[ {X,Y,Z} | T ], Acc ) ->
	% Will be reversed:
	get_point3( T, [ Z, Y, X | Acc ] ).



% @doc Returns a list of all, in-order coordinates of the specified points.
-spec get_point2( [ point2() ] ) -> [ coordinate() ].
get_point2( Elements ) ->
	get_point2( Elements, _Acc=[] ).


% (helper)
get_point2( _Elements=[], Acc ) ->
	Acc;

get_point2( _Elements=[ {X,Y} | T ], Acc ) ->
	% Will be reversed:
	get_point2( T, [ Y, X | Acc ] ).



% @doc Appends to the specified binary all 16 bit unsigned integers specified,
% and returns the resulting binary.
%
-spec append_all_uint16_little( [ integer() ], binary() ) -> binary().
append_all_uint16_little( _Elements=[], Bin ) ->
	Bin;

append_all_uint16_little( _Elements=[ UI | T ], Bin ) ->
	NewBin = <<Bin/binary,UI/little-unsigned-integer>>,
	append_all_uint16_little( T, NewBin ).



% @doc Appends to the specified binary all single-precision (32 bit) floats
% specified, and returns the resulting binary.
%
-spec append_all_float32_little( [ float() ], binary() ) -> binary().
append_all_float32_little( _Elements=[], Bin ) ->
	Bin;

append_all_float32_little( _Elements=[ F | T ], Bin ) ->
	NewBin = <<Bin/binary,F/float-little>>,
	append_all_float32_little( T, NewBin ).



% @doc Extracts specified points from specified binary (typically obtained from
% a buffer view).
%
-spec extract_points( binary(), count(), element_type(), component_type() ) ->
			[ specialised_point() ].
extract_points( Bin, ElementCount, ElemType, ComponentType ) ->
	extract_elements( Bin, ElementCount, ElemType, ComponentType,
					  _FinalType=point ).


% @doc Extracts specified vectors from specified binary (typically obtained from
% a buffer view).
%
-spec extract_vectors( binary(), count(), element_type(), component_type() ) ->
			[ specialised_vector() ].
extract_vectors( Bin, ElementCount, ElemType, ComponentType ) ->
	extract_elements( Bin, ElementCount, ElemType, ComponentType,
					  _FinalType=vector ).


% @doc Extracts specified indices from specified binary (typically obtained from
% a buffer view).
%
-spec extract_indices( binary(), count(), component_type() ) -> [ indice() ].
extract_indices( Bin, ElementCount, ComponentType ) ->
	extract_elements( Bin, ElementCount, _ElemType=scalar, ComponentType,
					  _FinalType=undefined ).



% @doc Extracts specified elements from specified binary (typically obtained
% from a buffer view).
%
-spec extract_elements( binary(), count(), element_type(), component_type(),
						final_type() ) -> [ buffer_elements() ].
extract_elements( Bin, ElementCount, _ElemType=scalar,
				  _ComponentType=uint16, _FinalType ) ->

	ComponentInts = extract_all_uint16_little( Bin ),

	% Check:
	ElementCount = length( ComponentInts ),

	ComponentInts;


extract_elements( Bin, ElementCount, _ElemType=scalar,
				  _ComponentType=float32, _FinalType ) ->

	ComponentInts = extract_all_float32_little( Bin ),

	% Check:
	ElementCount = length( ComponentInts ),

	ComponentInts;


extract_elements( Bin, ElementCount, _ElemType=vector2,
				  _ComponentType=float32, FinalType ) ->

	ComponentFloats = extract_all_float32_little( Bin ),

	Elems = gather_as( FinalType, _Dim=2, ComponentFloats ),

	% Check:
	ElementCount = length( Elems ),

	Elems;


extract_elements( Bin, ElementCount, _ElemType=vector3,
				  _ComponentType=float32, FinalType ) ->

	ComponentFloats = extract_all_float32_little( Bin ),

	Elems = gather_as( FinalType, _Dim=3, ComponentFloats ),

	% Check:
	ElementCount = length( Elems ),

	Elems.


% @doc Extracts from the specified binary all 16 bit unsigned integers, supposed
% encoded in little-endian.
%
-spec extract_all_uint16_little( binary() ) -> [ integer() ].
extract_all_uint16_little( Bin ) ->
	extract_all_uint16_little( Bin, _Acc=[] ).


extract_all_uint16_little( _Bin= <<>>, Acc ) ->
	lists:reverse( Acc );

extract_all_uint16_little( _Bin= <<UI:16/little-unsigned-integer,Rest/binary>>,
						   Acc ) ->
	extract_all_uint16_little( Rest, [ UI | Acc ] ).



% @doc Extracts from the specified binary all single-precision (32 bit) floats,
% supposed encoded in little-endian.
%
-spec extract_all_float32_little( binary() ) -> [ float() ].
extract_all_float32_little( Bin ) ->
	extract_all_float32_little( Bin, _Acc=[] ).


extract_all_float32_little( _Bin= <<>>, Acc ) ->
	lists:reverse( Acc );

extract_all_float32_little( _Bin= <<F:32/float-little,Rest/binary>>, Acc ) ->
	extract_all_float32_little( Rest, [ F | Acc ] ).



% @doc Gathers specified components as a list of the elements of specified
% dimension of the specifed final type.
%
-spec gather_as( final_type(), dimension(), [ number() ] ) -> [ term() ].
gather_as( FinalType, Dim, Components ) ->
	gather_as( FinalType, Dim, Components, _Acc=[] ).


% (helper)
gather_as( _FinalType, _Dim, _Components=[], Acc ) ->
	lists:reverse( Acc );

gather_as( FinalType, Dim, Components, Acc ) ->

	{ Coords, Rest } = lists:split( _PrefixLen=Dim, Components ),

	Element = case FinalType of

		point ->
			list_to_tuple( Coords );

		vector ->
			Coords

	end,

	gather_as( FinalType, Dim, Rest, [ Element | Acc ] ).



% @doc Returns a flat list of vertex indices corresponding to the specified list
% of (indexed) triangles.
%
-spec triangles_to_indices( [ indexed_triangle() ] ) -> [ indice() ].
triangles_to_indices( Triangles ) ->
	triangles_to_indices( Triangles, _Acc=[] ).


% (helper)
triangles_to_indices( _Triangles=[], Acc ) ->
	lists:reverse( Acc );


triangles_to_indices( _Triangles=[ { I1, I2, I3 } | T ], Acc ) ->
	% As indices will be reversed as a whole:
	triangles_to_indices( T, [ I3, I2, I1 | Acc ] ).



% @doc Returns the list of (indexed) triangles corresponding to the specified
% flat list of vertex indices.
%
-spec indices_to_triangles( [ indice() ] ) -> [ indexed_triangle() ].
indices_to_triangles( Indices ) ->
	indices_to_triangles( Indices, _Acc=[] ).


% (helper)
indices_to_triangles( _Indices=[], Acc ) ->
	lists:reverse( Acc );

indices_to_triangles( _Indices=[ I1, I2, I3 | T ], Acc ) ->
	Triangle = { I1, I2, I3 },
	indices_to_triangles( T, [ Triangle | Acc ] ).
