% Copyright (C) 2021-2024 Olivier Boudeville
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
% geometry of 3D objects; includes the OpenGL rendering thereof.
%
% See `mesh_test.erl' for the corresponding test.
%
-module(mesh).

% For the numerous GL defines notably:
-include("gui_opengl.hrl").

% For the mesh record:
-include("mesh.hrl").


-type mesh() :: #mesh{}.
% Describes a mesh, convex or not.

-type rendering_state() :: #rendering_state{}.
% Rendering (OpenGL-related) state of a mesh.



-type indice() :: linear:indice().
% The indice of an element (e.g. vertex, normal, texture coordinate), typically
% in a data container such as a list or a binary buffer.
%
% As always in Myriad, indices start at 1 (e.g. as opposed to zero-based indexes
% such as glTF).


-type vertex_indice() :: indice().
% The index of a vertex (typically a point3()) in an indexed face.


-type face_type() :: 'triangle' | 'quad'.
% The type of the faces of a mesh.


-type indexed_face() :: tuple( vertex_indice() ).
% Describes a face of a mesh, based on an (ordered) tuple of vertices (at least
% three of them), specified as indexes (for example 3 of them, to define a
% triangle).
%
% Note that usually the vertex order matters (regarding culling).


-type face_indice() :: indice().
% The index of a face in a container thereof.


-type indexed_triangle() ::
	{ vertex_indice(), vertex_indice(), vertex_indice() }.
% Made of the corresponding three vertices.
%
% Note that usually the vertex order matters (regarding culling).


-type indexed_quad() ::
	{ vertex_indice(), vertex_indice(), vertex_indice(), vertex_indice() }.
% Made of the corresponding four vertices.
%
% Note that usually the vertex order matters (regarding culling).


-type normal_type() :: 'per_vertex' | 'per_face'.
% Defines to which geometric element a normal corresponds.


-type face_coloring_type() :: 'per_vertex' | 'per_face'.
% Defines how a coloring shall be applied to a face.


-type texture_face_info() :: { texture(), [ uv_point() ] }.
% The texture information for a face, based on the texture that applies to this
% face and on the texture coordinates of its vertices, in their definition
% order.
%
% For example if a triangle face is defined based on vertices {VId1,VId2,VId3},
% is to be textured with texture Tex, and the texture coordinates associated
% to VIdk are {Tku,Tkv}, then the corresponding texture information for this
% face is: {Tex, [{T1u,T1v}, {T2u,T2v}, {T3u,T3v}]}.
%
% A texture identifier would not be enough: our texture record is needed to
% recalibrate the related texture coordinates.


% Indexed lists used instead:
%-type texture_table() :: table( indexed_face(), texture_face_info() ).
% A table associating to the index of a given face its texture information.


-type rendering_info() ::

	% No rendering info at all:
	'none'

	% Wireframe only:
  | { 'wireframe', EdgeColor :: render_rgb_color(),
	  HiddenFaceRemoval :: boolean() }

	% Per-vertex or per-face colors (color order being the one of the elements -
	% vertices or faces - at creation):
	%
	% (solid colors, no transparency here; RGB as triplets of integers, not in
	% [0.0,1.0])
	%
  | { 'color', face_coloring_type(), ElementColors :: [ color_by_decimal() ] }

	% Texture information for the face of the same index:
  | { 'texture', [ texture_face_info() ] }.
% Defines how a mesh shall be rendered.



-type vertex_count() :: count().
% A number of vertices.

-type normal_count() :: count().
% A number of normals.

-type edge_count() :: count().
% A number of edges.

-type face_count() :: count().
% A number of faces.



-export_type([ mesh/0, rendering_state/0, indice/0, vertex_indice/0,
			   face_type/0, indexed_face/0, face_indice/0,
			   indexed_triangle/0, indexed_quad/0,
			   normal_type/0,
			   face_coloring_type/0,
			   rendering_info/0,
			   vertex_count/0, normal_count/0, edge_count/0, face_count/0 ]).


% For the right_cuboid, sphere records and al:
-include("bounding_volume.hrl").


% Construction-related section.
-export([ create/4, create/6 ]).


% Operations on meshes.
-export([ tessellate/1, triangulate/1,
		  compute_normals/1,
		  indexed_face_to_triangle/1,
		  indexed_faces_to_triangles/1,

		  initialise_for_opengl/2, render_as_opengl/1, cleanup_for_opengl/1,

		  to_string/1, to_compact_string/1,
		  rendering_info_to_string/1, rendering_info_to_compact_string/1,
		  face_type_to_string/1, normal_type_to_string/1,
		  rendering_state_to_string/1 ]).



% Other operations, lower-level:
-export([ compute_normal/2, get_vertex_count_for_face_type/1 ]).


% Color-related section.
%-export([ set_edge_color/2, get_edge_color/1,
%          set_fill_color/2, get_fill_color/1 ]).


% Bounding volume related section.
%-export([ update_bounding_volume/2 ]).


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type tuple( T ) :: type_utils:tuple( T ).

-type color_by_decimal() :: gui_color:color_by_decimal().
-type render_rgb_color() :: gui_color:render_rgb_color().

%-type canvas() :: gui:canvas().

%-type distance() :: linear:distance().
%-type square_distance() :: linear:square_distance().

-type vertex3() :: point3:vertex3().

-type unit_normal3() :: vector3:unit_normal3().

-type texture() :: gui_texture:texture().
-type uv_point() :: gui_texture:uv_point().

-type vertex_attribute_series() :: gui_shader:vertex_attribute_series().
-type program_id() :: gui_shader:program_id().


% Implementation notes:

% Regarding OpenGL rendering.
%
% For a given mesh, we create a dedicated VAO referencing a VBO holding
% vertices, normals (if any) and texture coordinates (if any), and an EBO.
%
% We generally rely here on plain, real coordinates, rather than on NDC ones.
%
% Refer to gui_opengl_mesh_shader.{vertex,fragment}.glsl for typical examples of
% use.



% Construction-related section.


% @doc Returns a new mesh whose vertices, faces and rendering information are
% the specified ones, with no specific normal or bounding volume set.
%
-spec create( [ vertex3() ], face_type(), [ indexed_face() ],
			  rendering_info() ) -> mesh().
create( Vertices, FaceType, Faces, RenderingInfo ) ->
	create( Vertices, FaceType, Faces, _AnyNormalType=per_face,
			_MaybeNormals=[], RenderingInfo ).



% @doc Returns a new mesh whose vertices, faces, normals (of unit length, and of
% the specified type; if any), rendering information are the specified ones,
% with no specific bounding volume set.
%
-spec create( [ vertex3() ], face_type(), [ indexed_face() ],
	normal_type(), maybe( [ unit_normal3() ] ), rendering_info() ) -> mesh().
create( Vertices, FaceType, Faces, NormalType, MaybeNormals, RenderingInfo ) ->

	cond_utils:if_defined( myriad_check_mesh,
		begin
			ExpectedVertexCount = get_vertex_count_for_face_type( FaceType ),
			[ ExpectedVertexCount = size( F ) || F <- Faces ],
			MaybeNormals =:= undefined orelse
				vector3:check_unit_vectors( MaybeNormals )
		end ),

	#mesh{ vertices=Vertices,
		   face_type=FaceType,
		   faces=Faces,
		   normal_type=NormalType,
		   normals=MaybeNormals,
		   rendering_info=RenderingInfo }.



% @doc Returns the number of vertices per face to be expected for the specified
% face type.
%
-spec get_vertex_count_for_face_type( face_type() ) -> count().
get_vertex_count_for_face_type( _FaceType=triangle ) ->
	3;

get_vertex_count_for_face_type( _FaceType=quad ) ->
	4.



% Exported helpers.


% @doc "Tessellates" the specified mesh: returns a version of it whose faces are
% triangles (e.g. not quads).
%
-spec tessellate( mesh() ) -> mesh().
tessellate( Mesh=#mesh{ face_type=triangle } ) ->
	Mesh;

tessellate( Mesh=#mesh{ face_type=quad,
						faces=QuadFaces,
						normal_type=NormalType,
						normals=MaybeQuadNormals,
						rendering_info=QuadRendInfo
					  } ) ->

	% (a list comprehension would not suffice)
	TrigFaces = triangulate( QuadFaces ),

	MaybeTrigNormals = case MaybeQuadNormals of

		undefined ->
			undefined;

		QuadNormals ->
			case NormalType of

				per_vertex ->
					% No change in vertices:
					QuadNormals;

				per_face ->
					% The two triangles have the same normal as their quad:
					list_utils:repeat_elements( QuadNormals, _Count=2 )

			end

	end,

	TrigRendInfo = case QuadRendInfo of

		none ->
			none;

		WF={ wireframe, _EdgeColor, _HiddenFaceRemoval } ->
			WF;

		C={ color, _FaceColoringType=per_vertex, _ElemColors } ->
			C;

		{ color, _FaceColoringType=per_face, ElemColors } ->
			{ color, per_face,
			  list_utils:repeat_elements( ElemColors, _Cnt=2 ) };

		{ texture, TexFaceInfos } ->
			{ texture, adapt_texture_face_infos( TexFaceInfos ) }

	end,

	Mesh#mesh{ face_type=triangle,
			   faces=TrigFaces,
			   normals=MaybeTrigNormals,
			   rendering_info=TrigRendInfo }.



% @doc Converts each quad face (an indexed_quad(), defined by vertex indices
% {V1,V2,V3,V4}) into two triangle faces (indexed_triangle()): {V1,V2,V3} and
% {V3,V4,V1} (thus preserving the original conventional order).
%
% So the length of the returned (triangle) list is twice as long as the one of
% the specified (quad) one.
%
-spec triangulate( [ indexed_quad() ] ) -> [ indexed_triangle() ].
triangulate( QuadFaces ) ->
	% (a list comprehension would not suffice)
	triangulate( QuadFaces, _Acc=[] ).


% (helper)
triangulate( _QuadFaces=[], Acc ) ->
	lists:reverse( Acc );

triangulate( _QuadFaces=[ _QF={ V1Id, V2Id, V3Id, V4Id } | T ], Acc ) ->
	% Anticipate Acc reversal, even if without impact:
	NewAcc = [ { V3Id, V4Id, V1Id }, { V1Id, V2Id, V3Id } | Acc ],
	triangulate( T, NewAcc ).




% @doc Updates the specified mesh by computing automatically the "basic"
% per-face normals that can be deduced from the vertices of each face of this
% mesh, replacing any previously defined normals.
%
% Vertex ordering is supposed to respect Myriad's conventions (CCW from
% outside), so that the added normals are pointing outward.
%
-spec compute_normals( mesh() ) -> mesh().
compute_normals( Mesh=#mesh{ vertices=Vertices,
							 faces=Faces }) ->
	Normals = [ compute_normal( F, Vertices ) || F <- Faces ],
	Mesh#mesh{ normals=Normals }.


% @doc Updates the specified mesh by computing automatically the "basic"
% per-face normals that can be deduced from the vertices of each face of this
% mesh, replacing any previously defined normals.
%
% Vertex ordering is supposed to respect Myriad's conventions (CCW from
% outside), so that the added normals are pointing outward.
%
-spec compute_normal( indexed_face(), [ vertex3() ] ) -> unit_normal3().
compute_normal( Face, AllVertices ) ->

	% For that we fetch the first three vertices of that face.

	% A face being a tuple of vertex indices:

	V1Id = element( _FirstIndex=1, Face ),
	V1 = get_vertex_from_id( V1Id, AllVertices ),

	V2Id = element( 2, Face ),
	V2 = get_vertex_from_id( V2Id, AllVertices ),

	V3Id = element( 3, Face ),
	V3 = get_vertex_from_id( V3Id, AllVertices ),

	vector3:compute_normal( V1, V2, V3 ).



% @doc Adapts from quad to triangle the specified list of texture face
% information.
%
adapt_texture_face_infos( TexFaceInfos ) ->
	% Still no list comprehension; early, twice-as-small reversing:
	adapt_texture_face_infos( lists:reverse( TexFaceInfos ), _Acc=[] ).


% (helper)
adapt_texture_face_infos( _TexFaceInfos=[], Acc ) ->
	% Reversing already done:
	Acc;

adapt_texture_face_infos( _TexFaceInfos=[ { TexId,
		_UVPoints=[ UV1Coords, UV2Coords, UV3Coords, UV4Coords ] } | T ],
						  Acc ) ->
	% Anticipate Acc reversal, even if without impact; same order as the
	% vertices of the triangulated faces (in triangulate/2):
	%
	NewAcc = [ { TexId, [ UV3Coords, UV4Coords, UV1Coords ] },
			   { TexId, [ UV1Coords, UV2Coords, UV3Coords ] } | Acc ],

	adapt_texture_face_infos( T, NewAcc ).



% @doc Returns the indexed triangle corresponding to the specified indexed face
% (thus expected to comprise 3 vertices).
%
-spec indexed_face_to_triangle( indexed_face() ) -> indexed_triangle().
indexed_face_to_triangle( _F=[ V1, V2, V3 ] ) ->
	_IT={ V1, V2, V3 }.



% @doc Returns the indexed triangles corresponding to the specified indexed
% faces (thus expected to comprise 3 vertices each).
%
-spec indexed_faces_to_triangles( [ indexed_face() ] ) ->
												[ indexed_triangle() ].
indexed_faces_to_triangles( Faces ) ->
	[ indexed_face_to_triangle( F ) || F <- Faces ].



% @doc Registers the specified mesh within the current OpenGL context, so that
% the mesh can be readily rendered afterwards, and returns an updated mesh.
%
-spec initialise_for_opengl( mesh(), program_id() ) -> mesh().
% Only a subset of the combinations supported currently; first, the clause for
% triangle faces and solid colors:
%
initialise_for_opengl( Mesh=#mesh{
		vertices=Vertices,
		face_type=triangle,
		faces=IndexedFaces,
		% Normals currently ignored (useful for lighting only):
		%normal_type=per_{vertex,face},
		%normals=MaybeNormals,
		rendering_info={ color, per_face, FaceColors },
		rendering_state=undefined }, ProgramId ) ->

	trace_utils:debug_fmt( "Initialising for OpenGL ~ts.",
						   [ to_string( Mesh ) ] ),

	% Sanity check for input data:
	FaceCount = length( IndexedFaces ),
	FaceColorCount = length( FaceColors ),

	FaceCount =:= FaceColorCount orelse throw(
		{ mismatching_face_data, FaceCount, FaceColorCount } ),

	% Creates the VAO context we need for the upcoming VBO (vertices, possibly
	% normals - at least currently ignored - and no texture coordinates, just a
	% per-face color) and EBO (for indices in the VBO); we target a rendering in
	% terms of GL_TRIANGLES (no strip, fan, etc.) and will rely on the vtx3_rgb
	% VBO layout.
	%
	MeshVAOId = gui_shader:set_new_vao(),

	% OpenGL could have been requested to normalise the data by itself instead:
	FloatFaceColors = gui_color:decimal_to_render( FaceColors ),

	% We use an EBO, yet duplication will be needed, as a given vertex is
	% expected to pertain to multiple faces, each with its own (normal and)
	% color.
	%
	% We will iterate on faces to prepare the corresponding vertex attribute
	% compounds, so for the target VBO content we build adequate series of
	% vertex attributes. Here, with a triangle face_type, we have, per-face, 3
	% vertices and one color, so:
	%
	{ AttrSeries, ElemCount } = prepare_vattrs_single_face_color( IndexedFaces,
		FloatFaceColors, Vertices, _FaceVCount=3 ),

	% Creates a VBO from these two series, by merging them.
	%
	% We start at vertex attribute index #0 in this VAO; as there are two
	% series, the vertex attribute indices will be 0 and 1:
	%
	% (vertex attributes are automatically declared)
	%
	MeshVBOId = gui_shader:assign_new_vbo_from_attribute_series( AttrSeries ),

	% By design we just iterate over our pre-duplicated VBO; as a plain list of
	% indices (for example not a list of triplets of indices), still in CCW
	% order:
	%
	MeshEBOId = gui_shader:assign_indices_to_new_ebo(
		_FaceIndices=lists:seq( _From=1, _To=ElemCount ) ),

	% As the (single, here) VBO and the EBO were created whereas this VAO was
	% active, they are tracked by this VAO, which will rebind them automatically
	% the next time it will be itself bound:
	%
	gui_shader:unset_current_vao(),

	RenderState = #rendering_state{ program_id=ProgramId,
									vao_id=MeshVAOId,
									vbo_id=MeshVBOId,
									vbo_layout=vtx3_rgb,
									ebo_id=MeshEBOId,
									vertex_count=length( Vertices ) },

	Mesh#mesh{ rendering_state=RenderState }.

% Clause for the triangle faces and texture version:
%% initialise_for_opengl( Mesh=#mesh{
%%		vertices=Vertices,
%%		face_type=triangle,
%%		faces=IndexedFaces,
%%		% Normals currently ignored (useful for lighting only):
%%		%normal_type=per_{vertex,face},
%%		%normals=MaybeNormals,
%%		rendering_info={ texture, _TexFaceInfos={ Texture, TexCoords } },
%%		rendering_state=undefined } ) ->

%%	% Creates the VAO context we need for the upcoming VBO (vertices, possibly
%%	% normals and texture coordinates) and EBO (for indices in the VBO):
%%	%
%%	MeshVAOId = gui_shader:set_new_vao(),

%%	% To have correct texture coordinates in spite of padding:
%%	ActualTexCoords =
%%		gui_texture:recalibrate_coordinates_for( TexCoords, Texture ),

%%	% No normals used here:
%%	AttrSeries= [ Vertices, ActualTexCoords ],

%%	% Creates a VBO from these two series, by merging them.
%%	%
%%	% We start at vertex attribute index #0 in this VAO; as there are two
%%	% series, the vertex attribute indices will be 0 and 1:
%%	%
%%	MeshVBOId = gui_shader:assign_new_vbo_from_attribute_series( AttrSeries ),

%%	% As a plain list of indices (not for example a list of triplets of
%%	% indices), preferably in CCW order:
%%	%
%%	MeshEBOId = gui_shader:assign_indices_to_new_ebo( IndexedFaces ),

%%	% Specified while the triangle VBO and VAO are still active (VBO as it
%%	% specifies its structure, VAO so that it can record that attribute
%%	% specification):
%%	%
%%	gui_shader:declare_vertex_attribute( ?my_vertex_attribute_index ),


%%	% As the (single, here) VBO and the EBO were created whereas this VAO was
%%	% active, they are tracked by this VAO, which will rebind them automatically
%%	% the next time it will be itself bound:
%%	%
%%	gui_shader:unset_current_vao(),

%%	RenderState = #rendering_state{ vao_id=MeshVAOId,
%%									vbo_id=MeshVBOId,
%%									vbo_layout=vtx3_uv,
%%									ebo_id=MeshEBOId,
%%									vertex_count=length( Vertices ) },

%%	Mesh#mesh{ rendering_state=RenderState };




% @doc Returns, from the specified face information, a {AttrSeries, ElemCount}
% pair, where AttrSeries=[ToStoreVertices,ToStoreColors] contains two attribute
% series, and ElemCount is the length of each series.
%
% Each indexed face is expected to have the specified number of vertices, and is
% to be rendered with a given (solid, unique) color.
%
-spec prepare_vattrs_single_face_color( [ indexed_face() ],
		[ render_rgb_color() ], [ vertex3() ], count() ) ->
			{ [ vertex_attribute_series() ], count() }.
% So returns, as first element of the pair, a 2-element [A,B] list with A ::
% [vertex3()] and B :: [render_rgb_color()].
%
% Neither normals nor texture coordinates used here:
prepare_vattrs_single_face_color( IndexedFaces, FaceRenderColors, AllVertices,
								  FaceVCount ) ->

	%trace_utils:debug_fmt( "Preparing vertex attributes for faces of "
	%   "a single color: faces are ~w, colors are ~w.",
	%   [ IndexedFaces, FaceRenderColors ] ),

	% Expected to be uniform (cf. face_type):
	ActualFaceVCount = size( _Sample=hd( IndexedFaces ) ),
	ActualFaceVCount =:= FaceVCount orelse
		throw( { incorrect_per_face_vertex_count, FaceVCount,
				 ActualFaceVCount } ),

	% Preserving face order (at least clearer that way; possibly helping keeping
	% in sync with any EBO prebuilt indices); and reversing better done earlier
	% than later, after duplications (cheaper/simpler):
	%
	prepare_vattrs_single_face_color( lists:reverse( IndexedFaces ),
		lists:reverse( FaceRenderColors ), AllVertices,
		_ToStoreVerticesAcc=[], _ToStoreColorAcc=[], FaceVCount, _Count=0 ).


% (helper)
prepare_vattrs_single_face_color( _IndexedFaces=[], _FaceRenderColors=[],
		_AllVertices, ToStoreVerticesAcc, ToStoreColorAcc,
		_FaceVCount, Count ) ->
	% No reversing needed; as returning a list (not a tuple) of attribute
	% series:

	%trace_utils:debug_fmt( "Preparing vattrs: ~B vertices: ~p~n~B colors: ~p",
	%   [ length( ToStoreVerticesAcc ), ToStoreVerticesAcc,
	%     length( ToStoreColorAcc ), ToStoreColorAcc ] ),

	{ [ ToStoreVerticesAcc, ToStoreColorAcc ], Count };

prepare_vattrs_single_face_color( _IndexedFaces=[ VIdTuple | HIndexedFaces ],
		_FaceRenderColors=[ RenderColor | HColors ], AllVertices,
		ToStoreVerticesAcc, ToStoreColorAcc, FaceVCount, Count ) ->

	FaceVIds = tuple_to_list( VIdTuple ),
	FaceVs = get_vertices_from_ids( FaceVIds, AllVertices ),

	% Note that in the resulting buffer the vertex order will be preserved,
	% which matters at least for the outward convention / CCW order:
	%
	NewToStoreVerticesAcc = FaceVs ++ ToStoreVerticesAcc,

	% Each of the vertex of this face will have the (same) face color:
	NewToStoreColorAcc = list_utils:duplicate( _Elem=RenderColor,
		_Count=FaceVCount) ++ ToStoreColorAcc,

	prepare_vattrs_single_face_color( HIndexedFaces, HColors, AllVertices,
		NewToStoreVerticesAcc, NewToStoreColorAcc, FaceVCount,
		Count + FaceVCount ).




% @doc Renders the specified mesh based on its rendering state (if any) and on
% the current OpenGL context.
%
-spec render_as_opengl( mesh() ) -> void().
render_as_opengl( #mesh{ rendering_state=undefined } ) ->
	trace_utils:debug( "Mesh does not have a rendering state." );

render_as_opengl( #mesh{ rendering_state=#rendering_state{
										program_id=ProgramId,
										vao_id=VAOId,
										vbo_id=_VBOId,
										vbo_layout=VBOLayout,
										ebo_id=_EBOId,
										vertex_count=VertexCount } } ) ->

	% We rely on our shader program; operations that must be performed at each
	% rendering:

	% For wireframe rendering_info, use:
	%gui_opengl:set_polygon_raster_mode( _FacingMode=front_facing,
	%                                    _RasterMode=raster_filled ),

	PrimType = ?GL_TRIANGLES,

	gui_shader:set_vbo_layout( VBOLayout, ProgramId ),

	% Sets the VBO and the vertex attribute:
	gui_shader:set_current_vao_from_id( VAOId ),

	% So these three calls would be useless:
	%gui_shader:set_current_vbo_from_id( VBOId ),
	%gui_shader:enable_vertex_attribute( ?my_vertex_attribute_index ),
	%gui_shader:set_current_ebo_from_id( EBOId ),

	% Draws our splendid mesh (based on VertexCount vertex elements, starting at
	% index 0), using the currently active shaders, vertex attribute
	% configuration and with the VBO's vertex data (indirectly bound via the
	% VAO):
	%
	gui_shader:render_from_enabled_vbos( PrimType, VertexCount ),

	gui_shader:unset_current_vao(),

	% Useless, as reset at each rendering through VAOs:
	%gui_shader:disable_vertex_attribute( ?my_vertex_attribute_index ),

	ok.



% @doc Returns the specified mesh once its rendering state has been cleaned up
% and deallocated.
%
-spec cleanup_for_opengl( mesh() ) -> mesh().
cleanup_for_opengl( Mesh=#mesh{ rendering_state=undefined } ) ->
   Mesh;

cleanup_for_opengl( Mesh=#mesh{ rendering_state=#rendering_state{
													vao_id=VAOId,
													vbo_id=VBOId,
													ebo_id=EBOId } } ) ->

	cond_utils:if_defined( myriad_debug_mesh,
		trace_utils:debug( "Cleaning up mesh regarding OpenGL." ) ),

	gui_shader:delete_ebo( EBOId ),
	gui_shader:delete_vbo( VBOId ),
	gui_shader:delete_vao( VAOId ),

	Mesh#mesh{ rendering_state=undefined }.



% Basic helpers.


% @doc Returns the vertex of the specified identifier (indice) relative to the
% specified vertices.
%
-spec get_vertex_from_id( vertex_indice(), [ vertex3() ] ) -> vertex3().
get_vertex_from_id( VId, Vertices ) ->
	lists:nth( VId, Vertices ).


% @doc Returns the vertices of the specified identifiers (indices) relative to
% the specified vertices.
%
-spec get_vertices_from_ids( [ vertex_indice() ], [ vertex3() ] ) ->
											[ vertex3() ].
get_vertices_from_ids( VIds, Vertices ) ->
	[ get_vertex_from_id( VId, Vertices ) || VId <- VIds ].






% Operations on meshes.


% @doc Returns a (rather full) textual description of the specified mesh.
-spec to_string( mesh() ) -> ustring().
to_string( #mesh{ vertices=Vertices,
				  face_type=FaceType,
				  faces=Faces,
				  normal_type=NormalType,
				  normals=MaybeNormals,
				  rendering_info=RenderingInfo,
				  rendering_state=MaybeRenderState,
				  bounding_volume=MaybeBoundingVolume } ) ->

	NormalStr = case MaybeNormals of

		undefined ->
			"no normals";

		Normals ->
			text_utils:format( "~B ~ts normals: ~w", [ length( Normals ),
				normal_type_to_string( NormalType ), Normals ] )

	end,

	RenderStateStr = case MaybeRenderState of

		undefined ->
			"no rendering state available";

		RenderState ->
			rendering_state_to_string( RenderState )

	end,

	BVStr = case MaybeBoundingVolume of

		undefined ->
			"none available";

		BV ->
			bounding_volume:to_string( BV )

	end,

	text_utils:format( "mesh defined by:~n"
		" - ~B vertices: ~w~n"
		" - ~B ~ts faces: ~w~n"
		" - ~ts~n"
		" - ~ts~n"
		" - ~ts~n"
		" - bounding volume: ~ts",
		[ length( Vertices ), Vertices,
		  length( Faces ), face_type_to_string( FaceType ), Faces,
		  NormalStr, rendering_info_to_string( RenderingInfo ),
		  RenderStateStr, BVStr ] ).



% @doc Returns a compact textual description of the specified mesh.
-spec to_compact_string( mesh() ) -> ustring().
to_compact_string( #mesh{ vertices=Vertices,
						  face_type=FaceType,
						  faces=Faces,
						  normal_type=NormalType,
						  normals=MaybeNormals,
						  rendering_info=RenderingInfo,
						  rendering_state=MaybeRenderState,
						  bounding_volume=MaybeBoundingVolume } ) ->

	NormalStr = case MaybeNormals of

		undefined ->
			"no";

		Normals ->
			text_utils:format( "~B ~ts", [ length( Normals ),
				normal_type_to_string( NormalType ) ] )

	end,

	BVStr = case MaybeBoundingVolume of

		undefined ->
			"no bounding volume available";

		BV ->
			bounding_volume:to_string( BV )

	end,

	text_utils:format( "mesh with ~B vertices, ~B ~ts faces, "
		"~ts normals, with ~ts, ~ts and ~ts",
		[ length( Vertices ), length( Faces ), face_type_to_string( FaceType ),
		  NormalStr, rendering_info_to_compact_string( RenderingInfo ),
		  case MaybeRenderState of
				undefined -> "no";
				_ -> "a"
		  end ++ " rendering state",
		  BVStr ] ).



% @doc Returns a (rather full) textual description of the specified rendering
% information.
%
-spec rendering_info_to_string( rendering_info() ) -> ustring().
rendering_info_to_string( _RI=none ) ->
	"no rendering set";

rendering_info_to_string( _RI={ wireframe, HiddenFaceRemoval } ) ->
	"wireframe rendering (with " ++ case HiddenFaceRemoval of
										true -> "";
										false -> "no "
									end ++ "hidden-face removal";

rendering_info_to_string( _RI={ color, ColoringType, Colors } ) ->
	case ColoringType of
		per_vertex -> "per-vertex";
		per_face -> "per-face"
	end ++ text_utils:format( " rendering based on ~B colors: ~w",
							  [ length( Colors ), Colors ] );

rendering_info_to_string( _RI={ texture, TexInfos } ) ->
	text_utils:format( "rendering based on ~B texture information",
					   [ length( TexInfos ) ] ).



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
	end ++ text_utils:format( " rendering based on ~B colors: ~w",
							  [ length( Colors ), Colors ] );

rendering_info_to_compact_string( _RI={ texture, _TextureInfo } ) ->
	"textured rendering".



% @doc Returns a textual description of the specified face type.
-spec face_type_to_string( face_type() ) -> ustring().
face_type_to_string( triangle ) ->
	"triangle";

face_type_to_string( quad ) ->
	"quad".


% @doc Returns a textual description of the specified normal type.
-spec normal_type_to_string( normal_type() ) -> ustring().
normal_type_to_string( per_vertex ) ->
	"per-vertex";

normal_type_to_string( per_face ) ->
	"per-face".



% @doc Returns a textual description of the specified mesh rendering state.
-spec rendering_state_to_string( rendering_state() ) -> ustring().
rendering_state_to_string( #rendering_state{ program_id=ProgramId,
											 vao_id=VAOId,
											 vbo_id=VBOId,
											 vbo_layout=VBOLayout,
											 ebo_id=EBOId,
											 vertex_count=VCount } ) ->
	text_utils:format( "OpenGL rendering state in GLSL program #~B is VAO #~B, "
		"VBO #~B with a ~ts layout, and EBO #~B, "
		"for ~B vertex attribute compounds",
		[ ProgramId, VAOId, VBOId, VBOLayout, EBOId, VCount ] ).




% Bounding-volume related section.


% at-doc Updates, for the specified mesh, its internal bounding volume, with
% regard to the specified bounding-volume algorithm.
%
% Returns a mesh with updated information.
%
% at-end
%
% The lazy bounding sphere is fast to determine, but not optimal:
%-spec update_bounding_volume( bounding_algorithm(), mesh() ) -> mesh().
%update_bounding_volume( lazy_sphere, Mesh ) ->

%   SphereBVolume = bounding_volume:get_lazy_sphere( Mesh#mesh.vertices ),

%   Mesh#mesh{ bounding_volume=SphereBVolume }.




% Helper functions.
