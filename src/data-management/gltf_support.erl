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
% Index of a primitive in a glTf content.


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


-type topology_type() :: 'points' | 'lines' | 'line_loop' | 'line_strip'
						| 'triangles' | 'triangle_strip' | 'triangle_fan'.
% The (Myriad-defined) topology type of a primitive to render.
% Default is 'triangles'.


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


-export_type([ enum/0, content/0,
			   scene_index/0, scene/0,
			   node_index/0, scene_node/0,
			   mesh_index/0, mesh/0,
			   primitive_index/0, primitive/0,
			   attributes/0,
			   material_index/0, material/0,
			   pbr_metallic_roughness/0,
			   buffer_index/0, buffer/0,
			   buffer_view_index/0, buffer_view/0,
			   accessor_index/0, accessor/0,
			   element_type/0, gltf_element_type/0,
			   component_type/0, gltf_component_type/0,
			   component_value/0,
			   topology_type/0, gltf_topology_type/0, topology/0,
			   buffer_view_target/0, gltf_buffer_view_target/0,
			   generator_name/0 ]).


-export([ get_blank_content/0, write_gltf_content/3, write_gltf_content/4,
		  read_gltf_content/2,

		  add_primitive/6,
		  decode_primitive/4,
		  decode_vertices/5,

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

		  indices_to_triangles/1 ]).



% Shorthands:

-type count() :: basic_utils:count().

-type zero_index() :: basic_utils:zero_index().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type any_file_path() :: file_utils:any_file_path().

-type bijective_table( F, S ) :: bijective_table:bijective_table( F, S ).

-type json() :: json_utils:json().
-type json_term() :: json_utils:json_term().
-type parser_state() :: json_utils:parser_state().


-type dimension() :: linear:dimension().
-type indice() :: linear:indice().
-type indexed_triangle() :: linear:indexed_triangle().


-type specialised_point() :: linear:specialised_point().
-type specialised_vector() :: linear:specialised_vector().

-type specialised_vertex() :: linear:specialised_vertex().

-type specialised_normal() :: linear:specialised_normal().

-type specialised_texture_coordinates() ::
		linear:specialised_texture_coordinates().



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


-type extractable_elements() :: scalar()
							  | point2() | point3() | point4()
							  | vector2() | vector3() | vector4()
							  | matrix2() | matrix3() | matrix4().
% The elements that can be extracted from a buffer-view.

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
									 translation=MaybeTransVec } ) ->

	BaseTable = table:new(),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) },
		{ <<"mesh">>, MaybeMeshId },
		{ <<"rotation">>, MaybeRotQuat },
		{ <<"translation">>, MaybeTransVec } ], BaseTable ).



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
						   { uint32,5125 },
						   { float, 5126} ] ).


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
			  [ specialised_texture_coordinates() ] }.
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

	VertBufferTable = case Attributes#gltf_attributes.position of

		undefined ->
			BufferTable;

		PositionAccessorIndex ->
			{ Vertices, VertBuffTable } = decode_vertices(
				PositionAccessorIndex, Accessors, Buffers, BufferViews,
				BufferTable ),

			trace_utils:debug_fmt( "The ~B extracted vertices are:~n~p",
								   [ length( Vertices ), Vertices ] ),

			VertBuffTable

	end,


	NormBufferTable = case Attributes#gltf_attributes.normal of

		undefined ->
			VertBufferTable;

		NormalPositionAccessorIndex ->

			{ Normals, NormBuffTable } = decode_normals(
				NormalPositionAccessorIndex, Accessors, Buffers, BufferViews,
				VertBufferTable ),

			trace_utils:debug_fmt( "The ~B extracted normals are:~n~p",
								   [ length( Normals ), Normals ] ),

			NormBuffTable

	end,


	% No gltf_attributes.tangent managed here.


	Tex0BufferTable = case Attributes#gltf_attributes.texcoord_0 of

		undefined ->
			NormBufferTable;

		TexCoord0AccessorIndex ->

			{ TexCoords, Tex0BuffTable } = decode_texture_coordinates(
				TexCoord0AccessorIndex, Accessors, Buffers, BufferViews,
				NormBufferTable ),

			trace_utils:debug_fmt( "The ~B extracted texture coordinates "
				"are:~n~p", [ length( TexCoords ), TexCoords ] ),

			Tex0BuffTable

	end,


	_IndicesBufferTable = case Prim#gltf_primitive.indices of

		undefined ->
			Tex0BufferTable;

		IndicesPositionAccessorIndex ->

			{ Indices, IndicesBuffTable } = decode_indices(
				IndicesPositionAccessorIndex, Accessors, Buffers, BufferViews,
				Tex0BufferTable ),

			trace_utils:debug_fmt( "The ~B extracted vertex indices are:~n~p",
								   [ length( Indices ), Indices ] ),

			IndicesBuffTable

	end.




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



% @doc Encodes specified primitive information into specified glTF content.
%
% Directly embeds the resulting buffer.
%
-spec add_primitive( [ specialised_vertex() ], [ specialised_normal() ],
		[ specialised_texture_coordinates() ], topology_type(), topology(),
		content() ) -> content().
add_primitive( _Vertices, _Normals, _TexCoords, _TopologyType=triangles,
			   _IndexedTriangles, #gltf_content{} ) ->
	throw( todo ).



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
						final_type() ) -> [ extractable_elements() ].
extract_elements( Bin, ElementCount, _ElemType=scalar,
				  _ComponentType=uint16, _FinalType ) ->

	ComponentInts = extract_all_uint16_little( Bin ),

	% Check:
	ElementCount = length( ComponentInts ),

	ComponentInts;


extract_elements( Bin, ElementCount, _ElemType=vector2,
				  _ComponentType=float, FinalType ) ->

	ComponentFloats = extract_all_float32_little( Bin ),

	Elems = gather_as( FinalType, _Dim=2, ComponentFloats ),

	% Check:
	ElementCount = length( Elems ),

	Elems;


extract_elements( Bin, ElementCount, _ElemType=vector3,
				  _ComponentType=float, FinalType ) ->

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
