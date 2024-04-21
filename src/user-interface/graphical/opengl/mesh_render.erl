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
% Creation date: Sunday, April 21, 2024.


% @doc Gathering of various facilities for <b>mesh rendering</b> management.
%
% Relies on OpenGL to render the geometry of 3D objects, possibly with the
% corresponding MyriadGUI base shaders.
%
% See `mesh.erl' for the management of meshes by themselves.
%
-module(mesh_render).


% For the numerous GL defines notably:
-include("gui_opengl.hrl").

% For the VAI defines:
-include("gui_shader.hrl").


% Higher-level mesh-related rendering information.
-type rendering_info() ::

	% No rendering info at all:
	'none'

	% Wireframe only:
  | { 'wireframe', RGBEdgeColor :: gui_color:color_by_decimal(),
	  AreHiddenFacesRemoved :: boolean() }

	% Per-vertex (then a single color is assigned to a vertex, for all the faces
	% to which it belongs) or per-face colors (the color order being the one of
	% the elements - vertices or faces - at specification):
	%
	% (solid colors, no transparency here; RGB as triplets of integers, not in
	% [0.0,1.0])
	%
  | { 'colored', mesh_render:face_coloring_type(),
	  ElementColors :: [ gui_color:color_by_decimal() ] }

	% We want to support (single) per-face textures.
	% Texture information for the face of the same index in their list:
  %| { 'textured', [ mesh_render:texture_face_info() ] }.
  % Using now a single texture atlas per mesh:
  | { 'textured', gui_texture:texture_spec_id(),
	  [ mesh_render:texture_face_info() ] }.
% Defines how a mesh shall be rendered.


% For the related mesh-related rendering records:
-include("mesh_render.hrl").

-type rendering_state() :: #rendering_state{}.
% Rendering (OpenGL-related) state of a mesh.


% For the mesh record:
-include("mesh.hrl").


-type face_coloring_type() :: 'per_vertex' | 'per_face'.
% Defines how a coloring shall be applied to a face.


% Now assuming a single texture atlas is used:
%-type texture_face_info() :: { texture_spec_id(), [ uv_point() ] }.
-type texture_face_info() :: [ uv_point() ].
% The texture information for a face, based on the identifier of the texture
% specification (held by a texture cache) that applies to this face and on the
% corresponding texture coordinates of its vertices, in their definition order.
%
% For example if a triangle face is defined based on vertices {VId1,VId2,VId3},
% is to be textured with texture of specification identifier TexSpecId, and the
% texture coordinates associated to VIdk are {Tku,Tkv}, then the corresponding
% texture information for this face is: {TexSpecId, [{T1u,T1v}, {T2u,T2v},
% {T3u,T3v}]}.
%
% An OpenGL texture identifier would not be enough (i.e. a texture spec is
% used), notably as the related texture coordinates shall be recalibrated, and
% the corresponding metadata shall be stored - here in a texture() record.


% Indexed lists used instead of a table (or an array):
%-type face_texture_table() :: table( indexed_face(), texture_face_info() ).
% A table associating to the index of a given face its texture information.


-type render_element() :: render_rgb_color() | uv_point().
% A render-related element, typically in a list, associated to a vertex or a
% face.


-export_type([ rendering_info/0, rendering_state/0,
			   face_coloring_type/0, texture_face_info/0,
			   render_element/0 ]).



% Rendering operations on meshes.

-export([ canonicalise_rendering_info/3, tessellate_rendering_info/2,

		  initialise_for_opengl/2, initialise_for_opengl/3,
		  render_as_opengl/1, cleanup_for_opengl/1,

		  rendering_info_to_string/1, rendering_info_to_compact_string/1,
		  rendering_state_to_string/1 ]).


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type render_rgb_color() :: gui_color:render_rgb_color().

-type vertex3() :: point3:vertex3().

-type texture() :: gui_texture:texture().
-type uv_point() :: gui_texture:uv_point().
-type texture_cache() :: gui_texture:texture_cache().

-type vertex_attribute_series() :: gui_shader:vertex_attribute_series().
-type program_id() :: gui_shader:program_id().
-type compound_count() :: gui_shader:compound_count().
-type indexed_face() :: mesh:indexed_face().
-type face_type() :: mesh:face_type().
-type mesh() :: mesh:mesh().



% Implementation notes:


% Regarding OpenGL rendering.
%
% For a given mesh, we create a dedicated VAO referencing a VBO holding
% vertices, normals (if any) and texture coordinates (if any), and an EBO.
%
% We generally rely here on plain, real coordinates, rather than on NDC ones.
%
% Refer to gui_opengl_base_shader.{vertex,fragment}.glsl for typical examples of
% use.



% Regarding multitexturing.
%
% When rendering a VBO, there is no way to switch the current texture or texture
% unit.
%
% Solutions could be:
%
% - render a subset of a VBO, switch texture, render another subset; possibly
% sort the (per-object) primitives per texture/material, and render them in a
% row
%
% - gather the various texture elements involved in a single, larger, per-object
% texture atlas, and use texture coordinates to select the elements; there would
% be then a single texture per object
%
% While the first approach could be implemented as well, we preferred the second
% one, so we suppose there is (up to) one texture per mesh.
%
% Quite often there would be no choice, as the structure will be driven by the
% assets and/or their 3D format (e.g. glTF).



% @doc Canonicalises and checks the specified, probably user-defined, term as a
% legit rendering information, and returns it.
%
-spec canonicalise_rendering_info( term(), [ vertex3() ],
								   [ indexed_face() ] ) -> rendering_info().
canonicalise_rendering_info( RenderInfo=none, _Vertices, _Faces ) ->
	RenderInfo;

canonicalise_rendering_info(
		RenderInfo={ wireframe, _RGBEdgeColor, _AreHiddenFacesRemoved },
		_Vertices, _Faces ) ->
	RenderInfo;

canonicalise_rendering_info( RenderInfo={ colored, _FaceColoringType=per_vertex,
										  ElementColors }, Vertices, _Faces ) ->
	VtxCount = length( Vertices ),
	case length( ElementColors ) of

		VtxCount ->
			ok;

		ColCount ->
			throw( { vertex_color_count_mismatch, VtxCount, ColCount } )

	end,

	RenderInfo;

canonicalise_rendering_info( RenderInfo={ colored, _FaceColoringType=per_face,
										  ElementColors }, _Vertices, Faces ) ->
	FaceCount = length( Faces ),
	case length( ElementColors ) of

		FaceCount ->
			ok;

		ColCount ->
			throw( { face_color_count_mismatch, FaceCount, ColCount } )

	end,

	RenderInfo;

canonicalise_rendering_info(
		RenderInfo={ textured, _TexSpecId, FaceInfos }, _Vertices, Faces ) ->

	FaceCount = length( Faces ),
	case length( FaceInfos ) of

		FaceCount ->
			ok;

		FICount ->
			throw( { face_information_count_mismatch, FaceCount, FICount } )

	end,

	RenderInfo;

canonicalise_rendering_info( Other, _Vertices, _Faces ) ->
	throw( { invalid_mesh_rendering_information, Other } ).



% @doc Returns a rendering information deriving from the specified one, suitable
% for triangle-based rendering.
%
-spec tessellate_rendering_info( face_type(), rendering_info() ) ->
										  rendering_info().
tessellate_rendering_info( _OrigFaceType, RenderInfo=none ) ->
	RenderInfo;

tessellate_rendering_info( _OrigFaceType=quad,
		RenderInfo={ wireframe, _EdgeColor, _AreHiddenFacesRemoved } ) ->
	RenderInfo;

tessellate_rendering_info( _OrigFaceType=quad,
		RenderInfo={ colored, _FaceColoringType=per_vertex, _ElemColors } ) ->
	RenderInfo;

tessellate_rendering_info( _OrigFaceType=quad,
		_RenderInfo={ colored, _FaceColoringType=per_face, ElemColors } ) ->
	% Each quad becoming two triangles of the same color:
	{ colored, per_face, list_utils:repeat_elements( ElemColors, _Count=2 ) };

tessellate_rendering_info( _OrigFaceType=quad,
		_RenderInfo={ textured, TexSpecId, TexFaceInfos } ) ->
	TrigTexFaceInfos = adapt_texture_face_infos( TexFaceInfos ),
	{ textured, TexSpecId, TrigTexFaceInfos }.



% @doc Adapts from quad to triangle the specified list of texture face
% information elements.
%
adapt_texture_face_infos( TexFaceInfos ) ->
	% Early, twice-as-small reversing:
	adapt_texture_face_infos( lists:reverse( TexFaceInfos ), _Acc=[] ).


% (helper)
adapt_texture_face_infos( _TexFaceInfos=[], Acc ) ->
	% Reversing already done:
	Acc;

%adapt_texture_face_infos( _TexFaceInfos=[ { TexId,
	%   _UVPoints=[ UV1Coords, UV2Coords, UV3Coords, UV4Coords ] } | T ],
	%               Acc ) ->
	% % Anticipate Acc reversal, even if without impact; same order as the
	% % vertices of the triangulated faces (in triangulate/2):
	% %
	% NewAcc = [ { TexId, [ UV3Coords, UV4Coords, UV1Coords ] },
	%            { TexId, [ UV1Coords, UV2Coords, UV3Coords ] } | Acc ],

	% adapt_texture_face_infos( T, NewAcc ).

adapt_texture_face_infos( _TexFaceInfos=_UVPoints=
		[ [ UV1Coords, UV2Coords, UV3Coords, UV4Coords ] | T ], Acc ) ->
	% Returns triangle faces, anticipate Acc reversal (1-2-3, then 3-4-1), even
	% if without impact; same order as the vertices of the triangulated faces
	% (in triangulate/2):
	%
	NewAcc = [ [ UV3Coords, UV4Coords, UV1Coords ],
			   [ UV1Coords, UV2Coords, UV3Coords ] | Acc ],

	adapt_texture_face_infos( T, NewAcc ).




% Initialisation section.


% @doc Registers the specified mesh(es) within the current OpenGL context, so
% that the mesh(es) can be readily rendered afterwards, and returns the updated
% mesh(es) (if multiple meshes are specified, in their original order), together
% with possibly a created texture cache.
%
% No specific cache used or returned.
-spec initialise_for_opengl( maybe_list( mesh() ), program_id() ) ->
			{ maybe_list( mesh() ), maybe( texture_cache() ) }.
initialise_for_opengl( Mesh, ProgramId ) ->
	initialise_for_opengl( Mesh, ProgramId, _MaybeTextureCache=undefined ).



% @doc Registers the specified mesh(es) within the current OpenGL context, so
% that the mesh(es) can be readily rendered afterwards, using any specified
% texture cache, and returns the updated mesh(es) (if multiple meshes are
% specified, in their original order), together with possibly a created or
% updated texture cache.
%
-spec initialise_for_opengl( maybe_list( mesh() ), program_id(),
							 maybe( texture_cache() ) ) ->
			{ maybe_list( mesh() ), maybe( texture_cache() ) }.
% Only a subset of the combinations supported currently; first, if no rendering
% requested:
%
initialise_for_opengl( Mesh=#mesh{
								rendering_info=none %, rendering_state=undefined
								 },
					   _ProgramId, MaybeTextureCache ) ->
	{ Mesh, MaybeTextureCache };

% For triangle faces in wireframe mode:
initialise_for_opengl( Mesh=#mesh{
		vertices=Vertices,
		face_type=triangle,
		faces=IndexedFaces,
		% Normals currently ignored (useful for lighting only):
		%normal_type=per_{vertex,face},
		%normals=MaybeNormals,
		rendering_info={ wireframe, RGBEdgeColor, _AreHiddenFacesRemoved },
		rendering_state=undefined }, ProgramId, MaybeTextureCache ) ->

	% Mostly the same as for the next case (triangle faces and solid colors),
	% see it for details; only rendering will differ:

	cond_utils:if_defined( myriad_debug_mesh,
		trace_utils:debug_fmt( "Initialising for OpenGL "
			"(triangle faces, wireframe) ~ts.", [ to_string( Mesh ) ] ) ),

	FloatEdgeColor = gui_color:decimal_to_render( RGBEdgeColor ),

	MeshVAOId = gui_shader:set_new_vao(),

	% One color per face:
	FloatFaceColors = list_utils:duplicate( _Elem=FloatEdgeColor,
											_Count=length( IndexedFaces ) ),

	{ AttrSeries, CompoundCount } = prepare_vattrs_per_face( IndexedFaces,
		_SingleList=[ FloatFaceColors ], Vertices, _FaceVCount=3 ),

	% vtx3_rgb layout: in the buffer, first write a vertex, at the conventional
	% index location for vertices, then a color, at their dedicated location as
	% well:
	%
	VAIs = [ ?myriad_gui_input_vertex_vai, ?myriad_gui_input_color_vai ],

	MeshVBOId = gui_shader:assign_new_vbo_from_attribute_series_with(
		AttrSeries, VAIs ),

	MeshEBOId = gui_shader:assign_indices_to_new_ebo(
		_FaceIndices=lists:seq( _From=1, _To=CompoundCount ) ),

	gui_shader:unset_current_vao(),

	RenderState = #rendering_state{ program_id=ProgramId,
									vao_id=MeshVAOId,
									vbo_id=MeshVBOId,
									vbo_layout=vtx3_rgb,
									compound_count=CompoundCount,
									ebo_id=MeshEBOId },

	{ Mesh#mesh{ rendering_state=RenderState }, MaybeTextureCache };


% For triangle faces and solid, per-face colors:
initialise_for_opengl( Mesh=#mesh{
		vertices=Vertices,
		face_type=triangle,
		faces=IndexedFaces,
		% Normals currently ignored (useful for lighting only):
		%normal_type=per_{vertex,face},
		%normals=MaybeNormals,
		rendering_info={ colored, per_face, FaceColors },
		rendering_state=undefined }, ProgramId, MaybeTextureCache ) ->

	cond_utils:if_defined( myriad_debug_mesh,
		trace_utils:debug_fmt( "Initialising for OpenGL ~ts.",
			"(triangle faces, per-face color) ~ts.", [ to_string( Mesh ) ] ) ),

	% Sanity check for input data:
	FaceCount = length( IndexedFaces ),
	FaceColorCount = length( FaceColors ),

	FaceCount =:= FaceColorCount orelse
		throw( { mismatching_face_data, FaceCount, FaceColorCount } ),

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
	{ AttrSeries, CompoundCount } = prepare_vattrs_per_face( IndexedFaces,
		_SingleList=[ FloatFaceColors ], Vertices, _FaceVCount=3 ),

	% vtx3_rgb layout: in the buffer, first write a vertex, at the conventional
	% index location for vertices, then a color, at their dedicated location as
	% well:
	%
	VAIs = [ ?myriad_gui_input_vertex_vai, ?myriad_gui_input_color_vai ],

	% Creates a VBO from these two series, by merging them.
	%
	% We start at vertex attribute index #0 in this VAO; as there are two
	% series, the vertex attribute indices will be 0 and 1:
	%
	% (vertex attributes are automatically declared)
	%
	MeshVBOId = gui_shader:assign_new_vbo_from_attribute_series_with(
		AttrSeries, VAIs ),

	% By design we just iterate over our pre-duplicated VBO; as a plain list of
	% indices (for example not a list of triplets of indices), still in CCW
	% order:
	%
	MeshEBOId = gui_shader:assign_indices_to_new_ebo(
		_FaceIndices=lists:seq( _From=1, _To=CompoundCount ) ),

	% As the (single, here) VBO and the EBO were created whereas this VAO was
	% active, they are tracked by this VAO, which will rebind them automatically
	% the next time it will be itself bound:
	%
	gui_shader:unset_current_vao(),

	RenderState = #rendering_state{ program_id=ProgramId,
									vao_id=MeshVAOId,
									vbo_id=MeshVBOId,
									vbo_layout=vtx3_rgb,
									compound_count=CompoundCount,
									ebo_id=MeshEBOId },

	{ Mesh#mesh{ rendering_state=RenderState }, MaybeTextureCache };


% For triangle faces and solid, per-vertex colors:
initialise_for_opengl( Mesh=#mesh{
		vertices=Vertices,
		face_type=triangle,
		faces=IndexedFaces,
		% Normals currently ignored (useful for lighting only):
		%normal_type=per_{vertex,face},
		%normals=MaybeNormals,
		rendering_info={ colored, per_vertex, VertexColors },
		rendering_state=undefined }, ProgramId, MaybeTextureCache ) ->

	cond_utils:if_defined( myriad_debug_mesh,
		trace_utils:debug_fmt( "Initialising for OpenGL ~ts.",
			"(triangle faces, per-vertex color) ~ts.",
			[ to_string( Mesh ) ] ) ),

	% Sanity check for input data (yet some vertices could not be used):
	VertexCount = length( Vertices ),
	VertexColorCount = length( VertexColors ),

	VertexCount =:= VertexColorCount orelse
		throw( { mismatching_vertex_data, VertexCount, VertexColorCount } ),

	% Creates the VAO context we need for the upcoming VBO (vertices, possibly
	% normals - at least currently ignored - and no texture coordinates, just a
	% per-vertex color) and EBO (for indices in the VBO); we target a rendering
	% in terms of GL_TRIANGLES (no strip, fan, etc.) and will rely on the
	% vtx3_rgb VBO layout.
	%
	MeshVAOId = gui_shader:set_new_vao(),

	% OpenGL could have been requested to normalise the data by itself instead:
	FloatVertexColors = gui_color:decimal_to_render( VertexColors ),

	% We use an EBO, yet duplication will be needed, as a given vertex is
	% expected to pertain to multiple faces, each with its own (normal and)
	% color.
	%
	% We will iterate on faces to prepare the corresponding vertex attribute
	% compounds, so for the target VBO content we build adequate series of
	% vertex attributes. Here, with a triangle face_type, we have, per-face, 3
	% vertices and three colors, so:
	%
	{ AttrSeries, CompoundCount } = prepare_vattrs_per_vertex( IndexedFaces,
		_SingleList=[ FloatVertexColors ], Vertices, _FaceVCount=3 ),

	% vtx3_rgb layout: in the buffer, first write a vertex, at the conventional
	% index location for vertices, then a color, at their dedicated location as
	% well:
	%
	VAIs = [ ?myriad_gui_input_vertex_vai, ?myriad_gui_input_color_vai ],

	% Creates a VBO from these two series, by merging them.
	%
	% We start at vertex attribute index #0 in this VAO; as there are two
	% series, the vertex attribute indices will be 0 and 1:
	%
	% (vertex attributes are automatically declared)
	%
	MeshVBOId = gui_shader:assign_new_vbo_from_attribute_series_with(
		AttrSeries, VAIs ),

	% By design we just iterate over our pre-duplicated VBO; as a plain list of
	% indices (for example not a list of triplets of indices), still in CCW
	% order:
	%
	MeshEBOId = gui_shader:assign_indices_to_new_ebo(
		_FaceIndices=lists:seq( _From=1, _To=CompoundCount ) ),

	% As the (single, here) VBO and the EBO were created whereas this VAO was
	% active, they are tracked by this VAO, which will rebind them automatically
	% the next time it will be itself bound:
	%
	gui_shader:unset_current_vao(),

	RenderState = #rendering_state{ program_id=ProgramId,
									vao_id=MeshVAOId,
									vbo_id=MeshVBOId,
									vbo_layout=vtx3_rgb,
									compound_count=CompoundCount,
									ebo_id=MeshEBOId },

	{ Mesh#mesh{ rendering_state=RenderState }, MaybeTextureCache };


% For triangle faces and textured version:
initialise_for_opengl( Mesh=#mesh{
		vertices=Vertices,
		face_type=triangle,
		faces=IndexedFaces,
		% Normals currently ignored (useful for lighting only):
		%normal_type=per_{vertex,face},
		%normals=MaybeNormals,
		rendering_info={ textured, TexSpecId, TexFaceInfos },
		rendering_state=undefined }, ProgramId, MaybeTextureCache ) ->

	cond_utils:if_defined( myriad_debug_mesh,
		trace_utils:debug_fmt( "Initialising for OpenGL ~ts.",
			"(triangle, textured faces) ~ts.",
			[ to_string( Mesh ) ] ) ),

	% Sanity check for input data:
	FaceCount = length( IndexedFaces ),
	TexCount = length( TexFaceInfos ),

	FaceCount =:= TexCount orelse
		throw( { mismatching_face_data, FaceCount, TexCount } ),

	TexCache = case MaybeTextureCache of

		undefined ->
			gui_texture:create_cache();

		TCache ->
			TCache

	end,

	% Creates the VAO context we need for the upcoming VBO (vertices, possibly
	% normals - at least currently ignored - and texture coordinates, no
	% per-face or per-vertex color) and EBO (for indices in the VBO); we target
	% a rendering in terms of GL_TRIANGLES (no strip, fan, etc.) and will rely
	% on the vtx3_uv VBO layout.
	%
	MeshVAOId = gui_shader:set_new_vao(),

	{ Texture, NewTexCache } = gui_texture:get_texture( TexSpecId, TexCache ),

	% To have correct texture coordinates in spite of padding:
	%{ ActualTexCoordss, NewTexCache } =
	%   recalibrate_tex_coords_for( TexFaceInfos, TexSpecId, TexCache ),
	FullTexCoords = recalibrate_tex_coords_for( TexFaceInfos, Texture ),

	% We use an EBO, yet duplication will be needed, as a given vertex is
	% expected to pertain to multiple faces, each with its own (normal and)
	% texture coordinates.
	%
	% We will iterate on faces to prepare the corresponding vertex attribute
	% compounds, so for the target VBO content we build adequate series of
	% vertex attributes. Here, with a triangle face_type, we have, per-face, 3
	% vertices and three UV coordinates, so:
	%
	{ AttrSeries, CompoundCount } = prepare_vattrs_per_vertex( IndexedFaces,
		_SingleList=[ FullTexCoords ], Vertices, _FaceVCount=3 ),

	% vtx3_uv layout: in the buffer, first write a vertex, at the conventional
	% index location for vertices, then the (2D) texture coordinates, at their
	% dedicated location as well:
	%
	VAIs = [ ?myriad_gui_input_vertex_vai, ?myriad_gui_input_texcoord_vai ],

	% Creates a VBO from these two series, by merging them.
	%
	% We start at vertex attribute index #0 in this VAO; as there are two
	% series, the vertex attribute indices will be 0 and 1:
	%
	MeshVBOId = gui_shader:assign_new_vbo_from_attribute_series_with(
		AttrSeries, VAIs ),

	ElemCount = length( Vertices ),

	% As a plain list of indices (not for example a list of triplets of
	% indices), preferably in CCW order:
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
									vbo_layout=vtx3_uv,
									compound_count=CompoundCount,
									ebo_id=MeshEBOId },

	{ Mesh#mesh{ rendering_state=RenderState }, NewTexCache };

% Multiple meshes now:
initialise_for_opengl( Meshes=[], _ProgramId, MaybeTextureCache ) ->
	{ Meshes, MaybeTextureCache };

initialise_for_opengl( _Meshes=[ M | T ], ProgramId, MaybeTextureCache ) ->

	{ NewM, NewMTexCache } =
		initialise_for_opengl( M, ProgramId, MaybeTextureCache ),

	{ NewT, NewTTexCache } =
		initialise_for_opengl( T, ProgramId, NewMTexCache ),

	{ [ NewM | NewT ], NewTTexCache }.



% @doc Recalibrates the texture coordinates for each face: returns a
% corresponding, single, overall list of texture coordinates.
%
%-spec recalibrate_tex_coords_for( [ texture_face_info() ], texture_spec_id(),
%				texture_cache() ) -> { [ [ uv_point() ] ], texture_cache() }.
-spec recalibrate_tex_coords_for( [ texture_face_info() ], texture() ) ->
												[ uv_point() ].
% Could be almost a simple list comprehension; avoiding useless reversings with
% body-recursion:
%
recalibrate_tex_coords_for( _TexFaceInfos=[], _Texture ) ->
	[];

recalibrate_tex_coords_for( _TexFaceInfos=[ UVPoints | T ], Texture ) ->
	RecalUVPoints =
		gui_texture:recalibrate_coordinates_for( UVPoints, Texture ),

	RecalUVPoints ++ recalibrate_tex_coords_for( T, Texture ).


% (helper)
%recalibrate_tex_coords_for( _TexFaceInfos=[], _Texture, AccUVPoints ) ->
%   lists:reverse( AccUVPoints );

%recalibrate_tex_coords_for( _TexFaceInfos=[ { TexSpecId, UVPoints } | T ],
%   { Texture, NewTexCache } = gui_texture:get_texture( TexSpecId, TexCache ),




% Notes about the preparation of the series of the actual values for vertex
% attributes.
%
% To avoid code duplication, we can generalise two kinds of building functions:
%
% - one for per-vertex elements, which associates to each vertex one or more
% corresponding elements (color, normal, texture coordinates, etc.), taken from
% as many lists; see prepare_vattrs_per_vertex/4
%
% - one for per-face elements, which associates to each vertex one or elements
% (color, normal, texture coordinates, etc.) taken from as many lists, these
% elements corresponding not specifically to this vertex but to all the vertices
% of the current face; see prepare_vattrs_per_face/4



% @doc Returns preprocessed information to generate suitable buffers for a list
% of faces (presumably all having the specified number of vertices), each vertex
% thereof having its own elements (in the general case, multiple elements per
% vertex), specified in as many lists, contained in an overall list.
%
% More precisely, from the specified:
%  - (indexed) faces
%  - vertex-related elements (list of the per-vertex elements, like color,
%  normal, texture coordinates, etc.),
%  - list of all vertices
%  - the (supposedly constant) number of vertices per face
%
% returns a {AttrSeries, CompoundCount} pair, where AttrSeries=[ToStoreVertices,
% ToStoreElements1, ToStoreElements2, ...] and CompoundCount is the common
% length of all these series (which corresponds to the number of vertices listed
% through all faces, i.e. the number of vertex attribute compounds of a VBO that
% would derive from these inputs).
%
% So each indexed face is expected to have the specified number of vertices
% (this constraint could be relaxed), and each vertex will be rendered with its
% own set of elements (that thus are not per-face).
%
-spec prepare_vattrs_per_vertex( [ indexed_face() ], [ [ render_element() ] ],
								 [ vertex3() ], compound_count() ) ->
			{ [ vertex_attribute_series() ], compound_count() }.
prepare_vattrs_per_vertex( IndexedFaces, Elementss, AllVertices, FaceVCount ) ->

	FaceCount = length( IndexedFaces ),

	trace_utils:debug_fmt( "Preparing vertex attributes with ~B elements "
		"per vertex: the ~B faces are ~w, the elements to associate are:~n ~w.",
		[ length( Elementss ), FaceCount, IndexedFaces, Elementss ] ),

	% Expected to be uniform (cf. face_type):
	ActualFaceVCount = size( _Sample=hd( IndexedFaces ) ),
	ActualFaceVCount =:= FaceVCount orelse
		throw( { incorrect_per_face_vertex_count, FaceVCount,
				 ActualFaceVCount } ),

	% As each face will result in FaceVCount vertex attribute compounds:
	VACompoundCount = FaceVCount * FaceCount,

	case list_utils:check_same_length( Elementss ) of

		VACompoundCount ->
			ok;

		ElemCount ->
			% Here all element lists have the same length, but it does not
			% correspond to the compound count:
			%
			throw( { elements_compound_counts_mismatch, VACompoundCount,
					 ElemCount } )

	end,

	% Same sublist order, each of them is reversed, like IndexedFaces will be:
	RevElemss = [ lists:reverse( Es ) || Es <- Elementss ],

	InitToStoreElemsAcc = list_utils:duplicate( _Term=[],
												_Count=length( Elementss ) ),

	% Preserving face order in the returned lists (at least clearer that way;
	% possibly helping keeping in sync with any EBO prebuilt indices); and
	% reversing better done earlier than later, after duplications
	% (cheaper/simpler):
	%
	prepare_vattrs_per_vertex( lists:reverse( IndexedFaces ), RevElemss,
		AllVertices, _ToStoreVtxAcc=[], InitToStoreElemsAcc, FaceVCount,
		_CompoundCount=0 ).



% (helper)
%
% By design, all lists in RevElemss are empty as well:
prepare_vattrs_per_vertex( _RevIndexedFaces=[], RevElemss, _AllVertices,
		ToStoreVtxAcc, ToStoreElemsAcc, _FaceVCount, CompoundCount ) ->

	% cond_utils:assert/3 not relevant here:
	cond_utils:if_defined( myriad_debug_shaders, basic_utils:assert_equal( [],
								list_utils:flatten_once( RevElemss ) ),
						   basic_utils:ignore_unused( RevElemss ) ),

	{ [ ToStoreVtxAcc | ToStoreElemsAcc ], CompoundCount };


prepare_vattrs_per_vertex( _RevIndexedFaces=[ VIdTuple | HIndexedFaces ],
		RevElemss, AllVertices, ToStoreVtxAcc, ToStoreElemsAcc,
		FaceVCount, CompoundCount ) ->

	FaceVIds = tuple_to_list( VIdTuple ),
	FaceVs = mesh:get_elements_from_ids( FaceVIds, AllVertices ),
	NewToStoreVtxAcc = FaceVs ++ ToStoreVtxAcc,

	% Chops all heads from RevElemss for this face:
	{ FaceHeadElems, FaceTailElems } =
		list_utils:split_heads_tails( RevElemss ),

	NewToStoreElemsAcc =
		list_utils:add_as_heads( FaceHeadElems, ToStoreElemsAcc ),

	prepare_vattrs_per_vertex( HIndexedFaces, FaceTailElems, AllVertices,
		NewToStoreVtxAcc, NewToStoreElemsAcc, FaceVCount,
		CompoundCount+FaceVCount ).



% @doc Returns preprocessed information to generate suitable buffers for a list
% of faces (presumably all having the specified number of vertices), each face
% having its own elements (in the general case, multiple elements per face),
% specified in as many lists, contained in an overall list.
%
% More precisely, from the specified:
%  - (indexed) faces
%  - face-related elements (list of the per-face elements, like color, normal,
%  texture coordinates, etc.),
%  - list of all vertices
%  - the (supposedly constant) number of vertices per face
%
% returns a {AttrSeries, CompoundCount} pair, where AttrSeries=[ToStoreVertices,
% ToStoreElements1, ToStoreElements2, ...] and CompoundCount is the common
% length of all these series (which corresponds to the number of vertices listed
% through all faces, i.e. the number of vertex attribute compounds of a VBO that
% would derive from these inputs).
%
% So each indexed face is expected to have the specified number of vertices
% (this constraint could be relaxed), and is to be rendered with a given set of
% elements (common to all the vertices of that face).
%
-spec prepare_vattrs_per_face( [ indexed_face() ], [ [ render_element() ] ],
							   [ vertex3() ], count() ) ->
			{ [ vertex_attribute_series() ], compound_count() }.
prepare_vattrs_per_face( IndexedFaces, Elementss, AllVertices, FaceVCount ) ->

	FaceCount = length( IndexedFaces ),

	trace_utils:debug_fmt( "Preparing vertex attributes with ~B elements "
		"per face: the ~B faces are ~w, the elements to associate are:~n ~w.",
		[ length( Elementss ), FaceCount, IndexedFaces, Elementss ] ),

	% Expected to be uniform (cf. face_type):
	ActualFaceVCount = size( _Sample=hd( IndexedFaces ) ),
	ActualFaceVCount =:= FaceVCount orelse
		throw( { incorrect_per_face_vertex_count, FaceVCount,
				 ActualFaceVCount } ),

	case list_utils:check_same_length( Elementss ) of

		FaceCount ->
			ok;

		ElemCount ->
			% Here all element lists have the same length, but it does not
			% correspond to the face count:
			%
			throw( { element_face_counts_mismatch, FaceCount, ElemCount } )

	end,

	% Same sublist order, each of them is reversed, like IndexedFaces will be:
	RevElemss = [ lists:reverse( Es ) || Es <- Elementss ],

	InitToStoreElemsAcc = list_utils:duplicate( _Term=[],
												_Count=length( Elementss ) ),

	% Preserving face order in the returned lists (at least clearer that way;
	% possibly helping keeping in sync with any EBO prebuilt indices); and
	% reversing better done earlier than later, after duplications
	% (cheaper/simpler):
	%
	prepare_vattrs_per_face( lists:reverse( IndexedFaces ), RevElemss,
		AllVertices, _ToStoreVtxAcc=[], InitToStoreElemsAcc, FaceVCount,
		_CompoundCount=0 ).



% (helper)
%
% By design, all lists in RevElemss are empty as well:
prepare_vattrs_per_face( _RevIndexedFaces=[], RevElemss, _AllVertices,
		ToStoreVtxAcc, ToStoreElemsAcc, _FaceVCount, CompoundCount ) ->

	% cond_utils:assert/3 not relevant here:
	cond_utils:if_defined( myriad_debug_shaders, basic_utils:assert_equal( [],
								list_utils:flatten_once( RevElemss ) ),
						   basic_utils:ignore_unused( RevElemss ) ),

	{ [ ToStoreVtxAcc | ToStoreElemsAcc ], CompoundCount };


prepare_vattrs_per_face( _RevIndexedFaces=[ VIdTuple | HIndexedFaces ],
		RevElemss, AllVertices, ToStoreVtxAcc, ToStoreElemsAcc,
		FaceVCount, CompoundCount ) ->

	FaceVIds = tuple_to_list( VIdTuple ),
	FaceVs = mesh:get_elements_from_ids( FaceVIds, AllVertices ),
	NewToStoreVtxAcc = FaceVs ++ ToStoreVtxAcc,

	% Chops all heads from RevElemss for this face:
	{ FaceHeadElems, FaceTailElems } =
		list_utils:split_heads_tails( RevElemss ),

	DuplFaceHElems = [ list_utils:duplicate( _Elem=H, _Cnt=FaceVCount )
										|| H <- FaceHeadElems ],

	NewToStoreElemsAcc = list_utils:concatenate_per_rank( DuplFaceHElems,
														  ToStoreElemsAcc ),

	prepare_vattrs_per_face( HIndexedFaces, FaceTailElems, AllVertices,
		NewToStoreVtxAcc, NewToStoreElemsAcc, FaceVCount,
		CompoundCount+FaceVCount ).



% Rendering section; based on OpenGL.



% @doc Renders the specified mesh based on its rendering state (if any) and on
% the current OpenGL context.
%
-spec render_as_opengl( mesh() ) -> void().
render_as_opengl( #mesh{ rendering_info=none } ) ->
	%trace_utils:debug( "(mesh: nothing to render)" );
	ok;

% Both information and state are needed:
render_as_opengl( #mesh{ rendering_state=undefined } ) ->
	%trace_utils:debug( "Mesh does not have a rendering state." );
	ok;


% For the textured case:
%
% (see the clause for non-textured cases for further details)
%
render_as_opengl( M=#mesh{ %rendering_info={ textured, _TexSpecId,
						   %                 _TexFaceInfo },
						   rendering_state=#rendering_state{
								program_id=ProgramId,
								vao_id=VAOId,
								vbo_layout=vtx3_uv,
								compound_count=CompoundCount } } ) ->

	trace_utils:debug_fmt( "Rendering ~ts.", [ mesh:to_string( M ) ] ),

	gui_opengl:set_polygon_raster_mode( _FacingMode=front_facing,
										_RasterMode=raster_filled ),

	gui_shader:set_vbo_layout( _VBOLayout=vtx3_uv, ProgramId ),

	% Sets the VBO and the vertex attributes:
	gui_shader:set_current_vao_from_id( VAOId ),

	% No need to assign the texture sampler of our base fragment shader to a
	% given unit, it has been done once for us in
	% gui_shader:deploy_base_program/0. So when we bind a texture, it is bound
	% through a relevant texture unit.

	gui_shader:render_from_enabled_vbos( _PrimType=?GL_TRIANGLES,
										 CompoundCount ),

	gui_shader:unset_current_vao();

% For non-textured cases:
render_as_opengl( _M=#mesh{ rendering_info=RenderingInfoTuple,
							rendering_state=#rendering_state{
								program_id=ProgramId,
								vao_id=VAOId,
								vbo_layout=VBOLayout,
								compound_count=CompoundCount } } ) ->

	%trace_utils:debug_fmt( "Rendering ~ts.", [ mesh:to_string( M ) ] ),

	% We rely on our shader program; operations that must be performed at each
	% rendering:

	% In all cases (even for wireframe - not ?GL_LINES):
	PrimType = ?GL_TRIANGLES,

	% For wireframe rendering_info, use:

	case RenderingInfoTuple of

		{ wireframe, _RGBEdgeColor, AreHiddenFacesRemoved } ->
			RasterMode = raster_as_lines,
			%RasterMode = raster_filled,
			case AreHiddenFacesRemoved of

				true ->
					gui_opengl:set_polygon_raster_mode(
						_FacingMode=front_facing, RasterMode );

				false ->
					gui_opengl:set_polygon_raster_mode(
						_FacingMode=front_and_back_facing, RasterMode )

			end;

		% Enforces/restores relevant settings:
		_ ->
			gui_opengl:set_polygon_raster_mode( _FacingMode=front_facing,
												_RasterMode=raster_filled )

	end,

	gui_shader:set_vbo_layout( VBOLayout, ProgramId ),

	% Sets the VBO and the vertex attributes:
	gui_shader:set_current_vao_from_id( VAOId ),

	% So these three calls would be useless:
	%gui_shader:set_current_vbo_from_id( VBOId ),
	%gui_shader:enable_vertex_attribute( ... ),
	%gui_shader:set_current_ebo_from_id( EBOId ),

	% Draws our splendid mesh (based on VertexCount vertex elements, starting at
	% index 0), using the currently active shaders, vertex attribute
	% configuration and with the VBO's vertex data (indirectly bound via the
	% VAO):
	%
	gui_shader:render_from_enabled_vbos( PrimType, CompoundCount ),

	gui_shader:unset_current_vao(),

	% Useless, as reset at each rendering through VAOs:
	%gui_shader:disable_vertex_attribute( ... ),

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




% @doc Returns a (rather full) textual description of the specified rendering
% information.
%
-spec rendering_info_to_string( rendering_info() ) -> ustring().
rendering_info_to_string( _RI=none ) ->
	"no rendering set";

rendering_info_to_string( _RI={ wireframe, RGBEdgeColor,
								AreHiddenFacesRemoved } ) ->
	text_utils:format( "wireframe rendering (with edge color ~ts and ~ts)",
		[ gui_color:to_string( RGBEdgeColor ),
		  case AreHiddenFacesRemoved of
				true -> "";
				false -> "no "
		  end ++ "hidden-face removal" ] );

rendering_info_to_string( _RI={ colored, ColoringType, Colors } ) ->
	text_utils:format( "~ts rendering based on ~B colors: ~w",
		[ face_coloring_type_to_string( ColoringType), length( Colors ),
		  Colors ] );

%rendering_info_to_string( _RI={ textured, TexInfos } ) ->
%	text_utils:format( "rendering based on ~B texture information",
%					   [ length( TexInfos ) ] ).
rendering_info_to_string( _RI={ textured, TexSpecId, TexCoords } ) ->
	text_utils:format( "rendering based on ~B texture coordinates relative "
		"to texture specification #~B", [ length( TexCoords ), TexSpecId ] ).



% @doc Returns a compact textual description of the specified rendering
% information.
-spec rendering_info_to_compact_string( rendering_info() ) -> ustring().
rendering_info_to_compact_string( _RI=none ) ->
	"no rendering set";

rendering_info_to_compact_string(
						_RI={ wireframe, _AreHiddenFacesRemoved=true } ) ->
	"culled wireframe rendering";

rendering_info_to_compact_string(
						_RI={ wireframe, _AreHiddenFacesRemoved=false } ) ->
	"unculled wireframe rendering";

rendering_info_to_compact_string( _RI={ colored, ColoringType, Colors } ) ->
	text_utils:format( "~ts rendering based on ~B colors: ~w",
		[ face_coloring_type_to_string( ColoringType), length( Colors ),
		  Colors ] );

%rendering_info_to_compact_string( _RI={ textured, _TextureInfo } ) ->
%   "textured rendering".

rendering_info_to_compact_string(
		_RI={ textured, TexSpecInfo, _TextureInfo } ) ->
	text_utils:format( "rendering based on texture whose specification "
					   "identifier is #B", [ TexSpecInfo ] ).




% @doc Returns a textual description of the specified mesh rendering state.
-spec rendering_state_to_string( rendering_state() ) -> ustring().
rendering_state_to_string( #rendering_state{ program_id=ProgramId,
											 vao_id=VAOId,
											 vbo_id=VBOId,
											 vbo_layout=VBOLayout,
											 compound_count=CompoundCount,
											 ebo_id=EBOId } ) ->
	text_utils:format( "OpenGL rendering state in GLSL program #~B is VAO #~B, "
		"VBO #~B with a ~ts layout (~B vertex attribute compounds), "
		"and EBO #~B",
		[ ProgramId, VAOId, VBOId, VBOLayout, CompoundCount, EBOId ] ).


% @doc Returns a textual description of the specified face coloring type.
-spec face_coloring_type_to_string( face_coloring_type() ) -> ustring().
face_coloring_type_to_string( _FCT=per_vertex ) ->
	"per-vertex";

face_coloring_type_to_string( _FCT=per_face ) ->
	"per-face".
