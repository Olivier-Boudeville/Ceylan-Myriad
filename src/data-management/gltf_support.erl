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


-type element_type() :: 'scalar' | 'vector2' | 'vector3'.
% Specifies if the elements of an accessor are scalars, vectors, or matrices.
% The datatype of a component of that element is to be specified with
% component_type/0.


-type component_type() :: type_utils:low_level_type().
% The datatype of a component of an accessor, for instance 'uint16'.

-type gltf_component_type() :: pos_integer().
% A glTf lower-level type identifier. Ex: '5120' for sint8.


-type component_value() :: number().
% The value of a component of an accessor.


-type generator_name() :: ustring().
% The name chosen for this glTf generator.


-export_type([ content/0,
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
			   component_type/0, gltf_component_type/0,
			   component_value/0,
			   generator_name/0 ]).


-export([ write_gltf_content/3, write_gltf_content/4,
		  file_to_gltf_buffer_embedded/1, bin_to_gltf_buffer_embedded/1,
		  element_type_to_gltf/1, component_type_to_gltf/1 ]).



% Shorthands:

-type zero_index() :: basic_utils:zero_index().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type any_file_path() :: file_utils:any_file_path().

-type json() :: json_utils:json().
-type json_term() :: json_utils:json_term().
-type parser_state() :: json_utils:parser_state().


%-type vector3() :: vector3:vector3().
%-type quaternion() :: quaternion:quaternion().

%-type render_color() :: gui_color:render_color().


% Design notes:
%
% Materials are defined based on the Physically-Based Rendering (PBR)
% methodology.



% Implementation notes:
%

% So that we can use the 'table' pseudo-module, as JSON parsers rely on maps:
-define( table_type, map_hashtable ).



-define( default_generator_name, "Ceylan-Myriad glTf exporter" ).



% @doc Writes the specified glTF scene in the specified file, which is expected
% not to exist, using a default generator name.
%
-spec write_gltf_content( content(), any_file_path(), parser_state() ) ->
			void().
write_gltf_content( GlTfContent, OutputFilePath, ParserState ) ->
	write_gltf_content( GlTfContent, OutputFilePath, ?default_generator_name,
						ParserState ).



% @doc Writes the specified glTF scene in the specified file, which is expected
% not to exist, using specified generator name.
%
-spec write_gltf_content( content(), any_file_path(), generator_name(),
						  parser_state() ) -> void().
write_gltf_content( GlTfContent, OutputFilePath, GeneratorName,
					ParserState ) ->

	cond_utils:if_defined( gltf_exporter_verbose, trace_utils:debug_fmt(
		"Writing glTf scene in '~ts' as '~ts'.",
		[ OutputFilePath, GeneratorName ] ) ),

	JsonContent = gltf_content_to_json( GlTfContent, GeneratorName,
										ParserState ),

	% Already JSON-encoded:
	%json_utils:to_json_file( JsonContent, OutputFilePath, ParserState ).

	file_utils:write_whole( OutputFilePath, JsonContent ).





% @doc Returns a glTf buffer corresponding to the specified (binary) file,
% embedding the file content directly into a relevant base64-encoded URI.
%
-spec file_to_gltf_buffer_embedded( any_file_path() ) -> buffer().
file_to_gltf_buffer_embedded( FilePath ) ->

	BinContent = file_utils:read_whole( FilePath ),

	cond_utils:if_defined( gltf_exporter_verbose, trace_utils:debug_fmt(
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

	cond_utils:if_defined( gltf_exporter_verbose, trace_utils:debug_fmt(
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
									   type=ElemType,
									   component_type=ComponentType,
									   count=ElemCount,
									   max=MaybeMax,
									   min=MaybeMin } ) ->
	BaseTable = table:new( [
		{ <<"type">>, element_type_to_gltf( ElemType ) },
		{ <<"component_type">>, ComponentType },
		{ <<"count">>, ElemCount } ] ),

	table:add_maybe_entries( [
		{ <<"bufferView">>, MaybeBufferViewIndex },
		{ <<"max">>, MaybeMax },
		{ <<"min">>, MaybeMin } ], BaseTable ).



% @doc Converts a component type into a (lower-level) glTf one.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#accessor-data-types
%
-spec element_type_to_gltf( element_type() ) -> bin_string().
element_type_to_gltf( scalar ) ->
	<<"SCALAR">>;

element_type_to_gltf( vector2 ) ->
	<<"VEC2">>;

element_type_to_gltf( vector3 ) ->
	<<"VEC3">>;

element_type_to_gltf( vector4 ) ->
	<<"VEC4">>;

element_type_to_gltf( matrix2 ) ->
	<<"MAT2">>;

element_type_to_gltf( matrix3 ) ->
	<<"MAT3">>;

element_type_to_gltf( matrix4 ) ->
	<<"MAT4">>.



% @doc Converts a component type into a (lower-level) glTf one.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#accessor-data-types
%
-spec component_type_to_gltf( component_type() ) -> gltf_component_type().
component_type_to_gltf( uint8 ) ->
	5121;

component_type_to_gltf( sint8 ) ->
	5120;

component_type_to_gltf( uint16 ) ->
	5123;

component_type_to_gltf( sint16 ) ->
	5122;

% No sint32 supported by glTf.

component_type_to_gltf( uint32 ) ->
	5125;

component_type_to_gltf( float ) ->
	5126.



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
