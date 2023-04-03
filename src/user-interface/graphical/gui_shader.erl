% Copyright (C) 2023-2023 Olivier Boudeville
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
% Creation date: Monday, March 13, 2023.


% @doc Gathering of various facilities for the <b>support of (OpenGL)
% shaders and programs</b>.
%
% Based on GLSL.
%
-module(gui_shader).



% Notably for the numerous GL defines:
-include("gui_opengl.hrl").


% For the UI32 define:
-include("type_utils.hrl").


-type shader_id() :: non_neg_integer().
% The identifier of (any kind of) a shader.


% There are 6 types of shaders; in pipeline order:

-type vertex_shader_id() :: shader_id().
% The identifier of a vertex shader, to run on a programmable vertex processor.


-type tessellation_control_shader_id() :: shader_id().
% The identifier of a tessellation shader, to run on a programmable tessellation
% processor in the control stage.


-type tessellation_evaluation_shader_id() :: shader_id().
% The identifier of a tessellation shader, to run on a programmable tessellation
% processor in the evaluation stage.


-type geometry_shader_id() :: shader_id().
% The identifier of a geometry shader, to run on a programmable geometry
% processor.


-type fragment_shader_id() :: shader_id().
% The identifier of a fragment shader, to run on a programmable fragment
% processor.


-type compute_shader_id() :: shader_id().
% The identifier of a compute shader, to run on a programmable compute
% processor.


-type program_id() :: non_neg_integer().
% The identifier of GLSL, shader-based program.



-type vertex_attribute_name() :: ustring().
% The name of a user-defined attribute, meant to be set through an associated
% index.
%
% By default a vertex attribute is disabled.


-type vertex_attribute_index() :: non_neg_integer().
% The index of a vertex attribute, in each data conveyed through the vertex
% stream.
%
% Such an index corresponds to a location for a shader (like in `layout
% (location = 0)').
%
% When specifying such an attribute, it is done relatively to the currently
% active VBO.


-type user_vertex_attribute() ::
		{ vertex_attribute_name(), vertex_attribute_index() }.
% A user-defined vertex attribute variable, meant to be associated to a generic
% vertex attribute index in a GLSL program.


-type vao_id() :: non_neg_integer().
% The identifier of a "Vertex Array Object" (VAO).
%
% A VAO is able to reference "vertex information":
% - the specification of multiple (enabled) vertex attributes
% - multiple VBOs (up to one for vertices, the others for per-vertex attributes)
% - up to one EOB
%
% The core profile requires a VAO to be explicitly used; a default VAO (number
% 0) exists only with the compatibility profile, so we recommend not using it.
%
% Once made active (bound) and until being unbound, a VAO keeps track of the
% EBOs that are bound. So if an EBO is still bound when a VAO is unbound, the
% EBO will be tracked by this VAO and be automatically bound when this VAO will
% be bound next. The same applies to EBO and to the specifications of vertex
% attributes.


-type array_buffer() :: gl_buffer().
% An OpenGL/GLSL array buffer; for example a VBO or an EBO.

-type array_buffer_id() :: gl_buffer_id().
% The identifier of an (OpenGL/GLSL) array buffer, i.e. a "array buffer object
% name".


-type array_bind_target() ::
	?GL_ARRAY_BUFFER              % Vertex attributes
  | ?GL_ATOMIC_COUNTER_BUFFER     % Atomic counter storage
  | ?GL_COPY_READ_BUFFER          % Buffer copy source
  | ?GL_COPY_WRITE_BUFFER         % Buffer copy destination
  | ?GL_DISPATCH_INDIRECT_BUFFER  % Indirect compute dispatch commands
  | ?GL_DRAW_INDIRECT_BUFFER      % Indirect command arguments
  | ?GL_ELEMENT_ARRAY_BUFFER      % Vertex array indices
  | ?GL_PIXEL_PACK_BUFFER         % Pixel read target
  | ?GL_PIXEL_UNPACK_BUFFER       % Texture data source
  | ?GL_QUERY_BUFFER              % Query result buffer
  | ?GL_SHADER_STORAGE_BUFFER     % Read-write storage for shaders
  | ?GL_TEXTURE_BUFFER            % Texture data buffer
  | ?GL_TRANSFORM_FEEDBACK_BUFFER % Transform feedback buffer
  | ?GL_UNIFORM_BUFFER.           % Uniform block storage
% Designates a type of buffer object, typically used as a binding target.


-type vbo() :: array_buffer().
% A "Vertex Buffer Object", that is a (GLSL) buffer storing a specific piece of
% information (vertex coordinates, or normals, or colors, or texture
% coordinates, etc.) for each element of a series of vertices (a.k.a. vertex
% stream).
%
% A VBO corresponds to an homogeneous chunk (an array) of data, sent from the
% CPU-space, in order to be stored (possibly durably) in the GPU-space.
%
% So a 3D object may have a VBO for its vertices, one for its normals, etc.
%
% The key point is that the data is retrieved once per vertex and passed to a
% given shader instance. If a buffer contains 3 elements, each will be sent to a
% different shader instance, all of them processing their data in
% parallel.
%
% Furthermore interpolation may be done, for example from the outputs of the
% vertex shader (one such output per vertex) to the many fragments placed as
% input to the fragment shaders (one per pixel).


-type vbo_id() :: gl_buffer_id().
% The identifier of a Vertex Buffer Object (VBO).



-type component_count() :: count().
% The number of component values (separate values of the same type) in a given
% vertex attribute.
%
% Must be in [1, 2, 3, 4].


-type component_value() :: integer() | float().
% A value of a component of an actual vertex attribute.
%
% For example one of the coordinates of a vertex position, normal, or texel.


-type vertex_attribute_value() :: type_utils:tuple( component_value() ).
% A value of a vertex attribute.
%
% For example a float triplet corresponding to a 3D vertex or a RGB color.


-type vertex_attribute_float_value() :: type_utils:tuple( float() ).
% A value of a float-based vertex attribute.
%
% For example a triplet of floats corresponding to a 3D vertex or a RGB color.


-type vertex_attribute_series() :: [ vertex_attribute_value() ].
% A (usually homogeneous) list of vertex attribute values, like a list of
% vertices, normals, texture coordinates, colors, etc.
%
% Series is a way of not mentioning "list" again.


-type vertex_attribute_float_series() :: [ vertex_attribute_float_value() ].
% A (usually homogeneous) list of float-based vertex attribute values, like a
% list of vertices, normals, texture coordinates, colors, etc.


% The position of a vertex attribute is determined based on the array stride and
% offset:

-type stride() :: byte_size().
% The number of bytes between two vertex attributes of a given type (comprising
% its own size).
%
% A null stride means that the buffer is tightly packed and that OpenGL will
% determine by itself the actual stride, equal here to the size of such a vertex
% attribute.


-type offset() :: byte_size().
% Offset (possibly null) at which the first vertex attribute begins in the
% buffer.
%
% This generally corresponds to the sum of the sizes of all previous attributes
% defined for a given vertex.


-type gl_primitive_type() :: ?GL_POINTS | ?GL_LINE_STRIP | ?GL_LINE_LOOP
	| ?GL_LINES | ?GL_LINE_STRIP_ADJACENCY | ?GL_LINES_ADJACENCY
	| ?GL_TRIANGLE_STRIP | ?GL_TRIANGLE_FAN | ?GL_TRIANGLES
	| ?GL_TRIANGLE_STRIP_ADJACENCY | ?GL_TRIANGLES_ADJACENCY | ?GL_PATCHES.
% A type of OpenGL primitive for rendering.


-type index() :: count().
% An index in an array, typically of an element in a VBO (then, a vertex index)
% or directly contained in an EBO.
%
% Starts at zero.


-type ebo() :: array_buffer().
% An Element Buffer Object, a (GLSL) buffer storing indices to vertex data.


-type ebo_id() :: gl_buffer_id().
% The identifier of an Element Buffer Object (EBO).
%
% It is an array of indices to vertex data.
%
% Indices start at zero, and by default are of the ?GL_UNSIGNED_INT type.



% Uniform variables.

-type uniform_id() :: integer().
% The identifier of a uniform variable; it represents the location of a specific
% uniform variable within a program object.


-type uniform_name() :: ustring().
% The name of a uniform variable.
%
% It must not be a structure, an array of structures, or a subcomponent of a
% vector or a matrix.
%
% No user-defined uniform name shall start with the reserved prefix "gl_".


-type uniform_value() :: any().
% A value that can be assigned or read from a uniform variable.



% Knowing that Myriad vectors are lists (contrary to points, which are tuples):

-type gl_vec2() :: { float(), float() }.
% A 2D vector directly suitable for use with the gl module.

-type gl_vec3() :: { float(), float(), float() }.
% A 3D vector directly suitable for use with the gl module.

-type gl_vec4() :: { float(), float(), float(), float() }.
% A 4D vector directly suitable for use with the gl module.

-type gl_vector() :: type_utils:tuple( float() ).
% A vector of any dimension directly suitable for use with the gl module.



% Usage notes:
%
% To avoid compilation problems, shaders may be encoded in the ANSI/ASCII
% format. Starting with OpenGL 4.2, shaders can be encoded as UTF-8
% strings. According to the GLSL spec, non-ASCII characters are only allowed in
% comments.
%
% How the variables of interest of a shader instance correspond to an element in
% a VBO can be determined through either of the following 3 approaches:
%
% - by specifying the association directly when generating the GLSL program,
% thanks to a list of user_attribute(); see generate_program_from/3 and
% generate_program/2; this is our preferred method
%
% - by specifying on both sides the same (numerical) index; typically the
% application would use 'gui_shader:declare_vertex_attribute(INDEX)' while the
% shader would specify 'layout(location = INDEX) in vec3 my_input_vertex;'; this
% approach will work starting from OpenGL 3.3 (e.g. some Mac OS X would not
% support it)
%
% - by letting the linker decide which location to give each input variable, and
% using the result of gl:getAttribLocation/2 for the first parameter of calls to
% gl:vertexAttribPointer/6 (not specifically supported by MyriadGUI)


-export_type([ shader_id/0, vertex_shader_id/0,
			   tessellation_control_shader_id/0,
			   tessellation_evaluation_shader_id/0, geometry_shader_id/0,
			   fragment_shader_id/0, compute_shader_id/0,

			   user_vertex_attribute/0,
			   vertex_attribute_index/0, vertex_attribute_name/0,

			   program_id/0,
			   vao_id/0,
			   vbo/0, vbo_id/0,
			   ebo/0, ebo_id/0,

			   component_count/0, component_value/0,
			   vertex_attribute_value/0, vertex_attribute_float_value/0,
			   vertex_attribute_series/0, vertex_attribute_float_series/0,

			   stride/0, offset/0,
			   gl_primitive_type/0, index/0,

			   uniform_id/0, uniform_name/0, uniform_value/0,

			   gl_vec2/0, gl_vec3/0, gl_vec4/0, gl_vector/0 ]).





% For the build of shader-based programs:
-export([ get_shading_language_version/0,

		  compile_vertex_shader/1, compile_tessellation_control_shader/1,
		  compile_tessellation_evaluation_shader/1, compile_geometry_shader/1,
		  compile_fragment_shader/1, compile_compute_shader/1,

		  generate_program_from/2, generate_program_from/3,
		  generate_program/1, generate_program/2,

		  install_program/1, delete_program/1 ]).



% For buffers:
-export([ generate_buffer_id/0, generate_buffer_ids/1,
		  assign_array/3,
		  delete_buffer/1, delete_buffers/1 ]).


% For vertex attributes:
-export([ declare_vertex_attribute/1, declare_vertex_attribute/7,
		  enable_vertex_attribute/1, disable_vertex_attribute/1 ]).


% For VAO:
-export([ generate_vao_id/0, generate_vao_ids/1, set_new_vao/0,
		  set_current_vao_from_id/1, unset_current_vao/0,
		  delete_vao/1, delete_vaos/1 ]).


% For VBO:
-export([ generate_vbo_id/0, generate_vbo_ids/1,
		  set_new_vbo/0, set_current_vbo_from_id/1,
		  assign_current_vbo/1, assign_current_vbo/2,
		  assign_new_vbo/1, assign_new_vbo/2,

		  assign_new_vbo_from_attribute_float_series/1,
		  assign_new_vbo_from_attribute_float_series/2,

		  assign_vertices_to_new_vbo/1, assign_vertices_to_new_vbo/2,

		  render_from_enabled_vbos/3,

		  delete_vbo/1, delete_vbos/1 ]).


% For EBO:
-export([ generate_ebo_id/0, generate_ebo_ids/1,
		  set_new_ebo/0, set_current_ebo_from_id/1,
		  assign_current_ebo/1, assign_current_ebo/2,

		  assign_indices_to_new_ebo/1, assign_indices_to_new_ebo/2,

		  render_from_enabled_ebos/2,

		  delete_ebo/1, delete_ebos/1 ]).


% For uniform variables:
-export([ get_uniform_id/2, get_maybe_uniform_id/2,
		  set_uniform_3f/2, set_uniform_3fv/2,
		  set_uniform_4f/2 ]).


% Conversion helpers:
-export([ to_gl_vectors/1 ]).

% Design notes:
%
% The file extensions of shaders could be the ones retained by the OpenGL's GLSL
% reference compiler:
%    .vert: vertex shader
%    .frag: fragment shader
%    .geom: geometry shader
%    .tesc: tessellation control shader
%    .tese: tessellation evaluation shader
%    .comp: compute shader
%
% See also our automatic rules to check GLSL files.
%
% For example: to check foo.vertex.glsl, run 'make check-foo.vertex.glsl' (the
% GLSL reference compiler does not return output if it detects no error).



% Implementation notes:
%
% On modern OpenGL, there are no default vertex/fragment shaders on the GPU, so
% each application must define at least a vertex and fragment shader of its own.

% Information sources:
% - https://learnopengl.com/
% - https://antongerdelan.net/opengl/vertexbuffers.html

% Default usage profile for VBOs:
-define( default_vbo_usage_hint, { draw, static } ).

% Default usage profile for EBOs:
-define( default_ebo_usage_hint, { draw, static } ).


% Shorthands:

-type count() :: basic_utils:count().

-type any_file_path() :: file_utils:any_file_path().

-type byte_size() :: system_utils:byte_size().

-type ustring() :: text_utils:ustring().

-type vector2() :: vector4:vector2().

-type vector3() :: vector3:vector3().
-type any_vertex3() :: point3:any_vertex3().

-type vector4() :: vector4:vector4().

-type vector() :: vector:vector().

-type gl_buffer() :: gui_opengl:gl_buffer().
-type gl_buffer_id() :: gui_opengl:gl_buffer_id().
-type buffer_usage_hint() :: gui_opengl:buffer_usage_hint().
-type gl_base_type() :: gui_opengl:gl_base_type().



% Section for the build of shader-based programs.


% @doc Returns the version /release number of the currently used OpenGL
% shading language.
%
% Example: "4.60 FOOBAR".
%
% Since OpenGL 3.3, the version numbers of GLSL match the version of OpenGL
% (GLSL version 420 corresponds to OpenGL version 4.2 for example).
%
-spec get_shading_language_version() -> ustring().
get_shading_language_version() ->
	Res = gl:getString( ?GL_SHADING_LANGUAGE_VERSION ),
	cond_utils:if_defined( myriad_check_opengl, gui_opengl:check_error() ),
	Res.



% GLSL section.


% Note that, apparently, for some reason compiling the shaders twice solves
% rendering issues on some Intel drivers.


% @doc Loads and compiles a vertex shader from the specified source file (whose
% extension is typically .vertex.glsl), and returns its identifier.
%
% Will have to be explicitly deleted (with gl:DeleteShader/1) once not useful
% anymore.
%
-spec compile_vertex_shader( any_file_path() ) -> vertex_shader_id().
compile_vertex_shader( VertexShaderPath ) ->

	VertexShaderBin = file_utils:read_whole( VertexShaderPath ),

	% Creates an empty shader object, and returns a non-zero value by which it
	% can be referenced:
	%
	VertexShaderId = gl:createShader( ?GL_VERTEX_SHADER ),

	cond_utils:if_defined( myriad_debug_shaders,
		trace_utils:debug_fmt( "Compiling vertex shader '~ts'.",
							   [ VertexShaderPath ] ) ),

	% Associates source to empty shader:
	ok = gl:shaderSource( VertexShaderId, [ VertexShaderBin ] ),

	ok = gl:compileShader( VertexShaderId ),

	MaybeLogStr = case gl:getShaderiv( VertexShaderId, ?GL_INFO_LOG_LENGTH ) of

		0 ->
			undefined;

		InfoLen ->
			gl:getShaderInfoLog( VertexShaderId, InfoLen )

	end,

	% Now checks compilation outcome:
	case gl:getShaderiv( VertexShaderId, ?GL_COMPILE_STATUS ) of

		?GL_TRUE ->
			MaybeLogStr =:= undefined orelse
				trace_utils:warning_fmt( "Compilation of the vertex shader "
					"defined in '~ts' succeeded, yet reported that '~ts'.",
					[ VertexShaderPath, MaybeLogStr ] );

		_ ->
			MsgStr = case MaybeLogStr of

				undefined ->
					"(no report)";

				LogStr ->
					LogStr

			end,

			trace_utils:error_fmt( "Compilation of the vertex shader in "
				"'~ts' failed: ~ts.", [ VertexShaderPath, MsgStr ] ),

			gl:deleteShader( VertexShaderId ),

			throw( { shader_compilation_failed, vertex_shader,
					 VertexShaderPath, MsgStr } )

	end,

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	VertexShaderId.



% @doc Loads and compiles a tessellation control shader from the specified
% source file (whose extension is typically .tess-ctrl.glsl), and returns its
% identifier.
%
% Will have to be explicitly deleted (with gl:DeleteShader/1) once not useful
% anymore.
%
-spec compile_tessellation_control_shader( any_file_path() ) ->
											tessellation_control_shader_id().
compile_tessellation_control_shader( TessCtrlShaderPath ) ->

	TessCtrlShaderBin = file_utils:read_whole( TessCtrlShaderPath ),

	% Creates an empty shader object, and returns a non-zero value by which it
	% can be referenced:
	%
	TessCtrlShaderId = gl:createShader( ?GL_TESS_CONTROL_SHADER ),

	cond_utils:if_defined( myriad_debug_shaders,
		trace_utils:debug_fmt( "Compiling tessellation control shader '~ts'.",
							   [ TessCtrlShaderPath ] ) ),

	% Associates source to empty shader:
	ok = gl:shaderSource( TessCtrlShaderId, [ TessCtrlShaderBin ] ),

	ok = gl:compileShader( TessCtrlShaderId ),

	MaybeLogStr = case gl:getShaderiv( TessCtrlShaderId,
									   ?GL_INFO_LOG_LENGTH ) of

		0 ->
			undefined;

		InfoLen ->
			gl:getShaderInfoLog( TessCtrlShaderId, InfoLen )

	end,

	% Now checks compilation outcome:
	case gl:getShaderiv( TessCtrlShaderId, ?GL_COMPILE_STATUS ) of

		?GL_TRUE ->
			MaybeLogStr =:= undefined orelse
				trace_utils:warning_fmt( "Compilation of the tessellation "
					"control shader defined in '~ts' succeeded, "
					"yet reported that '~ts'.",
					[ TessCtrlShaderPath, MaybeLogStr ] );

		_ ->
			MsgStr = case MaybeLogStr of

				undefined ->
					"(no report)";

				LogStr ->
					LogStr

			end,

			trace_utils:error_fmt( "Compilation of the tessellation control "
				"shader in '~ts' failed: ~ts.",
				[ TessCtrlShaderPath, MsgStr ] ),

			gl:deleteShader( TessCtrlShaderId ),

			throw( { shader_compilation_failed,
					 tessellation_control_shader, TessCtrlShaderPath, MsgStr } )

	end,

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	TessCtrlShaderId.



% @doc Loads and compiles a tessellation evaluation shader from the specified
% source file (whose extension is typically .tess-eval.glsl), and returns its
% identifier.
%
% Will have to be explicitly deleted (with gl:DeleteShader/1) once not useful
% anymore.
%
-spec compile_tessellation_evaluation_shader( any_file_path() ) ->
											tessellation_evaluation_shader_id().
compile_tessellation_evaluation_shader( TessEvalShaderPath ) ->

	TessEvalShaderBin = file_utils:read_whole( TessEvalShaderPath ),

	% Creates an empty shader object, and returns a non-zero value by which it
	% can be referenced:
	%
	TessEvalShaderId = gl:createShader( ?GL_TESS_EVALUATION_SHADER ),

	cond_utils:if_defined( myriad_debug_shaders,
		trace_utils:debug_fmt( "Compiling tessellation evaluation shader "
							   "'~ts'.", [ TessEvalShaderPath ] ) ),

	% Associates source to empty shader:
	ok = gl:shaderSource( TessEvalShaderId, [ TessEvalShaderBin ] ),

	ok = gl:compileShader( TessEvalShaderId ),

	MaybeLogStr = case gl:getShaderiv( TessEvalShaderId,
									   ?GL_INFO_LOG_LENGTH ) of

		0 ->
			undefined;

		InfoLen ->
			gl:getShaderInfoLog( TessEvalShaderId, InfoLen )

	end,

	% Now checks compilation outcome:
	case gl:getShaderiv( TessEvalShaderId, ?GL_COMPILE_STATUS ) of

		?GL_TRUE ->
			MaybeLogStr =:= undefined orelse
				trace_utils:warning_fmt( "Compilation of the tessellation "
					"evaluation shader defined in '~ts' succeeded, "
					"yet reported that '~ts'.",
					[ TessEvalShaderPath, MaybeLogStr ] );

		_ ->
			MsgStr = case MaybeLogStr of

				undefined ->
					"(no report)";

				LogStr ->
					LogStr

			end,

			trace_utils:error_fmt( "Compilation of the tessellation evaluation "
				"shader in '~ts' failed: ~ts.",
				[ TessEvalShaderPath, MsgStr ] ),

			gl:deleteShader( TessEvalShaderId ),

			throw( { shader_compilation_failed,
					 tessellation_evaluation_shader, TessEvalShaderPath,
					 MsgStr } )

	end,

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	TessEvalShaderId.



% @doc Loads and compiles a geometry shader from the specified source file
% (whose extension is typically .geometry.glsl), and returns its identifier.
%
% Will have to be explicitly deleted (with gl:DeleteShader/1) once not useful
% anymore.
%
-spec compile_geometry_shader( any_file_path() ) -> geometry_shader_id().
compile_geometry_shader( GeometryShaderPath ) ->

	GeometryShaderBin = file_utils:read_whole( GeometryShaderPath ),

	% Creates an empty shader object, and returns a non-zero value by which it
	% can be referenced:
	%
	GeometryShaderId = gl:createShader( ?GL_GEOMETRY_SHADER ),

	cond_utils:if_defined( myriad_debug_shaders,
		trace_utils:debug_fmt( "Compiling geometry shader '~ts'.",
							   [ GeometryShaderPath ] ) ),

	% Associates source to empty shader:
	ok = gl:shaderSource( GeometryShaderId, [ GeometryShaderBin ] ),

	ok = gl:compileShader( GeometryShaderId ),

	MaybeLogStr = case gl:getShaderiv( GeometryShaderId,
									   ?GL_INFO_LOG_LENGTH ) of

		0 ->
			undefined;

		InfoLen ->
			gl:getShaderInfoLog( GeometryShaderId, InfoLen )

	end,

	% Now checks compilation outcome:
	case gl:getShaderiv( GeometryShaderId, ?GL_COMPILE_STATUS ) of

		?GL_TRUE ->
			MaybeLogStr =:= undefined orelse
				trace_utils:warning_fmt( "Compilation of the geometry "
					"shader defined in '~ts' succeeded, yet reported "
					"that '~ts'.", [ GeometryShaderPath, MaybeLogStr ] );

		_ ->
			MsgStr = case MaybeLogStr of

				undefined ->
					"(no report)";

				LogStr ->
					LogStr

			end,

			gl:deleteShader( GeometryShaderId ),

			trace_utils:error_fmt( "Compilation of the geometry shader in "
				"'~ts' failed: ~ts.", [ GeometryShaderPath, MsgStr ] ),

			throw( { shader_compilation_failed, geometry_shader,
					 GeometryShaderPath, MsgStr } )

	end,

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	GeometryShaderId.



% @doc Loads and compiles a fragment shader from the specified source file
% (whose extension is typically fragment.glsl), and returns its identifier.
%
% Will have to be explicitly deleted (with gl:DeleteShader/1) once not useful
% anymore.
%
-spec compile_fragment_shader( any_file_path() ) -> fragment_shader_id().
compile_fragment_shader( FragmentShaderPath ) ->

	FragmentShaderBin = file_utils:read_whole( FragmentShaderPath ),

	% Creates an empty shader object, and returns a non-zero value by which it
	% can be referenced:
	%
	FragmentShaderId = gl:createShader( ?GL_FRAGMENT_SHADER ),

	cond_utils:if_defined( myriad_debug_shaders,
		trace_utils:debug_fmt( "Compiling fragment shader '~ts'.",
							   [ FragmentShaderPath ] ) ),

	% Associates source to empty shader:
	ok = gl:shaderSource( FragmentShaderId, [ FragmentShaderBin ] ),

	ok = gl:compileShader( FragmentShaderId ),

	MaybeLogStr =
			case gl:getShaderiv( FragmentShaderId, ?GL_INFO_LOG_LENGTH ) of

		0 ->
			undefined;

		InfoLen ->
			gl:getShaderInfoLog( FragmentShaderId, InfoLen )

	end,

	% Now checks compilation outcome:
	case gl:getShaderiv( FragmentShaderId, ?GL_COMPILE_STATUS ) of

		?GL_TRUE ->
			MaybeLogStr =:= undefined orelse
				trace_utils:warning_fmt( "Compilation of the fragment "
					"shader defined in '~ts' succeeded, yet reported "
					"that '~ts'.", [ FragmentShaderPath, MaybeLogStr ] );

		_ ->
			MsgStr = case MaybeLogStr of

				undefined ->
					"(no report)";

				LogStr ->
					LogStr

			end,

			trace_utils:error_fmt( "Compilation of the fragment shader in "
				"'~ts' failed: ~ts.", [ FragmentShaderPath, MsgStr ] ),

			gl:deleteShader( FragmentShaderId ),

			throw( { shader_compilation_failed, fragment_shader,
					 FragmentShaderPath, MsgStr } )

	end,

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	FragmentShaderId.



% @doc Loads and compiles a compute shader from the specified source file
% (whose extension is typically .compute.glsl), and returns its identifier.
%
% Will have to be explicitly deleted (with gl:DeleteShader/1) once not useful
% anymore.
%
-spec compile_compute_shader( any_file_path() ) -> compute_shader_id().
compile_compute_shader( ComputeShaderPath ) ->

	ComputeShaderBin = file_utils:read_whole( ComputeShaderPath ),

	% Creates an empty shader object, and returns a non-zero value by which it
	% can be referenced:
	%
	ComputeShaderId = gl:createShader( ?GL_COMPUTE_SHADER ),

	cond_utils:if_defined( myriad_debug_shaders,
		trace_utils:debug_fmt( "Compiling compute shader '~ts'.",
							   [ ComputeShaderPath ] ) ),

	% Associates source to empty shader:
	ok = gl:shaderSource( ComputeShaderId, [ ComputeShaderBin ] ),

	ok = gl:compileShader( ComputeShaderId ),

	MaybeLogStr = case gl:getShaderiv( ComputeShaderId,
									   ?GL_INFO_LOG_LENGTH ) of

		0 ->
			undefined;

		InfoLen ->
			gl:getShaderInfoLog( ComputeShaderId, InfoLen )

	end,

	% Now checks compilation outcome:
	case gl:getShaderiv( ComputeShaderId, ?GL_COMPILE_STATUS ) of

		?GL_TRUE ->
			MaybeLogStr =:= undefined orelse
				trace_utils:warning_fmt( "Compilation of the compute "
					"shader defined in '~ts' succeeded, yet reported "
					"that '~ts'.", [ ComputeShaderPath, MaybeLogStr ] );

		_ ->
			MsgStr = case MaybeLogStr of

				undefined ->
					"(no report)";

				LogStr ->
					LogStr

			end,

			trace_utils:error_fmt( "Compilation of the compute shader in "
				"'~ts' failed: ~ts.", [ ComputeShaderPath, MsgStr ] ),

			gl:deleteShader( ComputeShaderId ),

			throw( { shader_compilation_failed, compute_shader,
					 ComputeShaderPath, MsgStr } )

	end,

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	ComputeShaderId.




% @doc Generates a GLSL program from the shaders whose source files are
% specified: loads and compiles the specified vertex and fragment shaders (with
% no user-specified attributes defined), links them in a corresponding program,
% and returns its identifier.
%
-spec generate_program_from( any_file_path(), any_file_path() ) -> program_id().
generate_program_from( VertexShaderPath, FragmentShaderPath ) ->

	VertexShaderId = compile_vertex_shader( VertexShaderPath ),
	FragmentShaderId = compile_fragment_shader( FragmentShaderPath ),

	generate_program( _ShaderIds=[ VertexShaderId, FragmentShaderId ] ).



% @doc Generates a GLSL program from the shaders whose source files are
% specified: loads and compiles the specified vertex and fragment shaders, with
% user-specified attributes, links them in a corresponding program, and returns
% its identifier.
%
-spec generate_program_from( any_file_path(), any_file_path(),
							 [ user_vertex_attribute() ] ) -> program_id().
generate_program_from( VertexShaderPath, FragmentShaderPath, UserAttributes ) ->

	VertexShaderId = compile_vertex_shader( VertexShaderPath ),
	FragmentShaderId = compile_fragment_shader( FragmentShaderPath ),

	generate_program( _ShaderIds=[ VertexShaderId, FragmentShaderId ],
					  UserAttributes ).



% @doc Generates a GLSL program from the (already loaded and compiled) shaders
% whose identifiers are specified, with no user-specified attributes defined:
% links these shaders in a corresponding program, and returns its identifier.
%
% Deletes the specified shaders once the program is generated.
%
-spec generate_program( [ shader_id() ] ) -> program_id().
generate_program( ShaderIds ) ->
	generate_program( ShaderIds, _UserAttributes=[] ).



% @doc Generates a GLSL program from the (already loaded and compiled) shaders
% whose identifiers are specified, with user-specified attributes: links these
% shaders in a corresponding program, and returns its identifier.
%
% Deletes the specified shaders once the program is generated.
%
-spec generate_program( [ shader_id() ], [ user_vertex_attribute() ] ) ->
								program_id().
generate_program( ShaderIds, UserAttributes ) ->

	% Creates an empty program object and returns a non-zero value by which it
	% can be referenced:
	%
	ProgramId = gl:createProgram(),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	[ gl:attachShader( ProgramId, ShdId ) || ShdId <- ShaderIds ],
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	% Any attribute must be bound before linking:

	trace_utils:debug_fmt( "Binding user vertex attribute locations ~p.",
						   [ UserAttributes ] ),

	[ gl:bindAttribLocation( ProgramId, Idx, AttrName )
									|| { AttrName, Idx } <- UserAttributes ],
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	gl:linkProgram( ProgramId ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	MaybeLogStr = case gl:getProgramiv( ProgramId, ?GL_INFO_LOG_LENGTH ) of

		0 ->
			undefined;

		InfoLen ->
			gl:getProgramInfoLog( ProgramId, InfoLen )

	end,

	% Now checks linking outcome:
	case gl:getProgramiv( ProgramId, ?GL_LINK_STATUS ) of

		?GL_TRUE ->
			MaybeLogStr =:= undefined orelse
				trace_utils:warning_fmt( "Linking of the program from "
					"specified shaders succeeded, yet reported that '~ts'.",
					[ MaybeLogStr ] );

		?GL_FALSE ->
			MsgStr = case MaybeLogStr of

				undefined ->
					"(no report)";

				LogStr ->
					LogStr

			end,

			trace_utils:error_fmt( "Linking of the program from the specified "
				"shaders failed: ~ts.", [ MsgStr ] ),

			delete_program( ProgramId ),

			throw( { program_linking_failed, MsgStr } )

	end,
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	[ begin
		gl:detachShader( ProgramId, ShdId ),
		gl:deleteShader( ShdId )
	  end || ShdId <- ShaderIds ],
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	ProgramId.



% @doc Installs the specified GLSL program as part of current rendering state.
-spec install_program( program_id() ) -> void().
install_program( ProgramId ) ->
	gl:useProgram( ProgramId ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).


% @doc Deletes the specified GLSL program.
-spec delete_program( program_id() ) -> void().
delete_program( ProgramId ) ->
	gl:deleteProgram( ProgramId ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).




% Section for the management of the building blocks used by shaders.


% @doc Returns a new, unique, buffer identifier.
%
% Operates for all kinds of arrays.
%
-spec generate_buffer_id() -> gl_buffer_id().
generate_buffer_id() ->
	[ BufferId ] = gl:genBuffers( _Count=1 ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),
	BufferId.


% @doc Returns the specified number of new, unique, buffer identifier.s
%
% Operates for all kinds of arrays.
%
-spec generate_buffer_ids( count() ) -> [ gl_buffer_id() ].
generate_buffer_ids( Count ) ->
	BufferIds = gl:genBuffers( Count ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),
	BufferIds.


% @doc Assigns the specified buffer to the specified array in the OpenGL
% context, based on the specified usage hint.
%
-spec assign_array( array_buffer(), array_bind_target(),
					buffer_usage_hint() ) -> void().
assign_array( ArrayBuffer, BindTarget, BufferUsageHint ) ->

	% (this is typically a call that may result in a SEGV)
	gl:bufferData( BindTarget, byte_size( ArrayBuffer ), ArrayBuffer,
		gui_opengl:buffer_usage_hint_to_gl( BufferUsageHint ) ),

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).



% @doc Deletes the specified buffer.
-spec delete_buffer( array_buffer_id() ) -> void().
delete_buffer( BufferId ) ->
	delete_buffers( [ BufferId ] ).


% @doc Deletes the specified buffers.
-spec delete_buffers( [ array_buffer_id() ] ) -> void().
delete_buffers( BufferIds ) ->
	gl:deleteBuffers( BufferIds ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).




% Vertex attribute subsection.


% @doc Tells OpenGL how the specified vertex attribute shall be interpreted in
% the currently active VBO, supposing 3 floats per vertex attribute directly in
% a tightly-packed buffer, and enables this attribute.
%
-spec declare_vertex_attribute( vertex_attribute_index() ) -> void().
declare_vertex_attribute( TargetVAttrIndex ) ->
	declare_vertex_attribute( TargetVAttrIndex, _ComponentCount=3,
		_ComponentType=?GL_FLOAT, _DoNormalise=false, _AttrStride=0,
		_Offset=0, _DoEnable=true ).


% @doc Tells OpenGL how the specified vertex attribute shall be interpreted in
% the currently active VBO, and enables this attribute if requested.
%
% The various components corresponding to a single vertex shall be described in
% terms of component count (e.g. 3 coordinates for a 3D position, a normal or a
% RGB value) and component type (e.g. floats for coordinates, integers for color
% element, etc.).
%
% Normalisation applies only to (fixed-point) integer components.
%
% Will be stored in any already-bound VAO.
%
-spec declare_vertex_attribute( vertex_attribute_index(), component_count(),
	gl_base_type(), boolean(), stride(), offset(), boolean() ) -> void().
declare_vertex_attribute( TargetVAttrIndex, ComponentCount, ComponentType,
						  DoNormalise, AttrStride, Offset, DoEnable ) ->

	gl:vertexAttribPointer( TargetVAttrIndex, ComponentCount, ComponentType,
		gui_opengl:boolean_to_gl( DoNormalise ), AttrStride, Offset ),

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	DoEnable andalso enable_vertex_attribute( TargetVAttrIndex ).




% @doc Enables the specified vertex attribute.
%
% If enabled, the values in the generic vertex attribute array will be accessed
% and used for rendering when calls are made to vertex array commands.
%
-spec enable_vertex_attribute( vertex_attribute_index() ) -> void().
enable_vertex_attribute( TargetVAttrIndex ) ->

	gl:enableVertexAttribArray( TargetVAttrIndex ),

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).


% @doc Disables the specified vertex attribute.
-spec disable_vertex_attribute( vertex_attribute_index() ) -> void().
disable_vertex_attribute( TargetVAttrIndex ) ->

	gl:disableVertexAttribArray( TargetVAttrIndex ),

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).




% VAO subsection.


% @doc Returns a new, unique, VAO identifier.
-spec generate_vao_id() -> vao_id().
generate_vao_id() ->
	[ VAOId ] = gl:genVertexArrays( _Count=1 ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),
	VAOId.


% @doc Returns the specified number of new, unique, VAO identifiers.
-spec generate_vao_ids( count() ) -> [ vao_id() ].
generate_vao_ids( Count ) ->
	VAOIds = gl:genVertexArrays( Count ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),
	VAOIds.


% @doc Creates a VAO identifier, sets it as the currently active one, and
% returns it.
%
% From now on, all operations done regarding VAOs will be applied to this one.
%
-spec set_new_vao() -> vao_id().
set_new_vao() ->
	set_current_vao_from_id( generate_vao_id() ).


% @doc Sets the specified VBO as the currently active one.
%
% Returns, if useful, the specified identifier, for chaining.
%
-spec set_current_vao_from_id( vao_id() ) -> vao_id().
set_current_vao_from_id( VAOId ) ->

	% To attach the buffer specified from its ID to the currently active
	% (i.e. bound) array buffer object (VAO) in the GL context:
	%
	gl:bindVertexArray( VAOId ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	VAOId.


% @doc Unsets the current VAO from the context.
%
% This is the moment when this VAO records the vertex information that has been
% set since it was bound, in order to be able to set again that information at
% its next binding.
%
-spec unset_current_vao() -> void().
unset_current_vao() ->
	gl:bindVertexArray( _Unbind=0 ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).



% @doc Deletes the specified VAO.
%
% If a currently bound VAO is deleted, the binding for that object reverts to
% zero and the default VAO becomes current.
%
delete_vao( VAOId ) ->
	delete_vaos( [ VAOId ] ).


% @doc Deletes the specified VAOs.
%
% If a currently bound VAO is deleted, the binding for that object reverts to
% zero and the default VAO becomes current.
%
-spec delete_vaos( [ vao_id() ] ) -> void().
delete_vaos( VAOIds ) ->
	gl:deleteVertexArrays( VAOIds ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).




% VBO subsection.


% @doc Returns a new, unique, VBO identifier.
-spec generate_vbo_id() -> vbo_id().
generate_vbo_id() ->
	generate_buffer_id().


% @doc Returns the specified number of new, unique, VBO identifiers.
-spec generate_vbo_ids( count() ) -> [ vbo_id() ].
generate_vbo_ids( Count ) ->
	generate_buffer_ids( Count ).



% @doc Creates a VBO identifier, sets it as the currently active one, and
% returns it.
%
% From now on, all operations done regarding the ?GL_ARRAY_BUFFER target will be
% applied to this VBO.
%
-spec set_new_vbo() -> vbo_id().
set_new_vbo() ->
	set_current_vbo_from_id( generate_vbo_id() ).


% @doc Sets the specified VBO as the currently active one.
%
% From now on, all operations done regarding the ?GL_ARRAY_BUFFER target will be
% applied to this buffer.
%
% Lower-level, defined to centralise calls.
%
% Returns, if useful, the specified identifier, for chaining.
%
-spec set_current_vbo_from_id( vbo_id() ) -> vbo_id().
set_current_vbo_from_id( VBOId ) ->

	% To attach the buffer specified from its ID to the currently active
	% (i.e. bound) array buffer object (VBO) in the GL context:
	%
	gl:bindBuffer( ?GL_ARRAY_BUFFER, VBOId ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	VBOId.



% @doc Assigns the specified buffer to the currently active VBO, and associates
% it a default usage profile.
%
% The specified data will thus typically be transferred to the graphic card.
%
-spec assign_current_vbo( array_buffer() ) -> void().
assign_current_vbo( ArrayBuffer ) ->
	assign_current_vbo( ArrayBuffer, ?default_vbo_usage_hint ).


% @doc Assigns the specified buffer and associated usage settings to the
% currently active VBO.
%
% The specified data will thus typically be transferred to the graphic card.
%
-spec assign_current_vbo( array_buffer(), buffer_usage_hint() ) -> void().
assign_current_vbo( ArrayBuffer, BufferUsageHint ) ->
	assign_array( ArrayBuffer, _BindTarget=?GL_ARRAY_BUFFER, BufferUsageHint ).



% @doc Assigns the specified buffer to a new VBO that is made the currently
% active one, and associates it a default usage profile.
%
% The specified data will thus typically be transferred to the graphic card.
%
-spec assign_new_vbo( array_buffer() ) -> vbo_id().
assign_new_vbo( ArrayBuffer ) ->
	assign_new_vbo( ArrayBuffer, ?default_vbo_usage_hint ).


% @doc Assigns the specified buffer and associated usage settings to a new VBO
% that is made the currently active one.
%
% The specified data will thus typically be transferred to the graphic card.
%
-spec assign_new_vbo( array_buffer(), buffer_usage_hint() ) -> vbo_id().
assign_new_vbo( ArrayBuffer, BufferUsageHint ) ->
	NewVBOId = set_new_vbo(),
	assign_current_vbo( ArrayBuffer, BufferUsageHint ),
	NewVBOId.



% @doc Assigns the specified vertices to a new VBO (associated to a default
% usage profile) that is made the currently active one, and whose identifier is
% returned.
%
-spec assign_vertices_to_new_vbo( [ any_vertex3() ] ) -> vbo_id().
assign_vertices_to_new_vbo( Vertices ) ->
	assign_vertices_to_new_vbo( Vertices, ?default_vbo_usage_hint ).


% @doc Assigns the specified vertices to a new VBO that is made the currently
% active one, associates the specified usage profile, and returns the identifier
% of this VBO.
%
-spec assign_vertices_to_new_vbo( [ any_vertex3() ], buffer_usage_hint()  ) ->
											vbo_id().
assign_vertices_to_new_vbo( Vertices, BufferUsageHint ) ->

	VBOId = set_new_vbo(),

	VBOBuffer = point3:to_buffer( Vertices ),

	assign_current_vbo( VBOBuffer, BufferUsageHint ),

	VBOId.



% @doc Assigns a new VBO that is made the currently active one and which is
% created from the specified vertex attribute series, provided that the
% components of all vertex attributes are floats, and declare the corresponding
% vertex attributes, start with vertex attribute index #0.
%
% The parameters of the vertex attributes are automatically determined and
% declared.
%
-spec assign_new_vbo_from_attribute_float_series(
		vertex_attribute_float_series() ) -> vbo_id().
assign_new_vbo_from_attribute_float_series( VertAttrFloatSeries ) ->
	assign_new_vbo_from_attribute_float_series( VertAttrFloatSeries,
												_StartVAttrIndex=0 ).


% @doc Assigns a new VBO that is made the currently active one and which is
% created from the specified vertex attribute series, provided that the
% components of all vertex attributes are floats, and declare the corresponding
% vertex attributes.
%
% The parameters of the vertex attributes are automatically determined and
% declared.
%
-spec assign_new_vbo_from_attribute_float_series(
		vertex_attribute_float_series(),
		StartVAttrStartIndex :: vertex_attribute_index() ) -> vbo_id().
assign_new_vbo_from_attribute_float_series( VertAttrFloatSeries,
											StartVAttrIndex ) ->
	% Tightly-packed buffer:
	Buffer = merge_float_attribute_series( VertAttrFloatSeries ),
	VBOId = assign_new_vbo( Buffer ),
	declare_vertex_attributes_from( VertAttrFloatSeries, StartVAttrIndex ),
	VBOId.


% @doc Declares the vertex attributes corresponding to the specified vertex
% attribute series in the currently active VBO, starting from the specified
% vertex attribute index, supposing a tightly-packed buffer, and enables these
% attributes.
%
-spec declare_vertex_attributes_from( vertex_attribute_series(),
		StartVAttrStartIndex :: vertex_attribute_index() ) -> void().
declare_vertex_attributes_from( VertAttrSeries, VAttrIndex ) ->
	declare_vertex_attributes_from( VertAttrSeries, VAttrIndex,
									_DoEnable=true ).


% @doc Declares the vertex attributes corresponding to the specified vertex
% attribute series in the currently active VBO, supposing a tightly-packed
% buffer respecting our default type conventions, and enables these attributes
% if requested.
%
-spec declare_vertex_attributes_from( vertex_attribute_series(),
			VAttrStartIndex :: vertex_attribute_index(), boolean() ) -> void().
declare_vertex_attributes_from( _VertAttrSeries=[], _VAttrIndex, _DoEnable ) ->
	ok;

declare_vertex_attributes_from( _VertAttrSeries=[ _S=[ FirstTuple | _ ] | T ],
								VAttrIndex, DoEnable ) ->
	% All tuples of these series supposed to be of the same type:
	ComponentCount = size( FirstTuple ),
	ComponentType = infer_gl_component_type( element( _Index=1, FirstTuple ) ),

	% At least currently, never normalising (fixed-point) integer components;
	% null stride and offset as array supposed to be tightly-packed:
	%
	declare_vertex_attribute( VAttrIndex, ComponentCount, ComponentType,
		_DoNormalise=false, _AttrStride=0, _Offset=0, DoEnable ),

	declare_vertex_attributes_from( T, VAttrIndex+1, DoEnable ).



% @doc Generates a raw buffer, typically suitable to be assigned to a VBO, from
% the specified list of (generally homogeneous) series of floating-point vertex
% attributes.
%
% Each series usually contains a single, specific kind of data, like vertices,
% or normals, or colors.
%
% Each element of such series is a tuple of floats, of arbitrary size.
%
% For example: Bin = merge_attribute_lists([Vertices, TexCoords, Normals]) where
% Vertices and Normals could be lists of (float) triplets whereas TexCoords
% would be a list of (float) pairs.
%
% Interleaves all lists in a correctly-encoded binary, as 32-bit native floats;
% all lists of values shall have the same length (equal to the number of vertex
% attributes to consider) and contain only floats.
%
-spec merge_float_attribute_series( [ vertex_attribute_series() ] ) ->
											array_buffer().
merge_float_attribute_series( VertAttrFloatSeries ) ->
	% First we merge element per element all specified series:
	InterleavedFloatTuples = list_utils:zipn( VertAttrFloatSeries ),
	bin_utils:tuples_to_float32s_binary( InterleavedFloatTuples ).



% @doc Deduces, according to MyriadGUI's conventions, the OpenGL data type
% corresponding to the specified component in an array (like a VBO).
%
-spec infer_gl_component_type( term() ) -> gl_base_type().
% So never returns ?GL_BYTE, ?GL_UNSIGNED_SHORT, ?GL_SHORT, ?GL_INT or
% ?GL_DOUBLE:
infer_gl_component_type( F ) when is_float( F ) ->
	?GL_FLOAT;

infer_gl_component_type( I ) when is_integer( I ) ->
	% Preferred to ?GL_INT and others:
	?GL_UNSIGNED_INT.



% @doc Renders the specified primitives based on the enabled VBOs.
-spec render_from_enabled_vbos( gl_primitive_type(), index(), count() ) ->
												void().
render_from_enabled_vbos( PrimType, StartIndex, VertexCount ) ->

	gl:drawArrays( PrimType, StartIndex, VertexCount ),

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).



% @doc Deletes the specified VBO.
delete_vbo( VBOId ) ->
	delete_buffer( VBOId ).


% @doc Deletes the specified VBOs.
-spec delete_vbos( [ vbo_id() ] ) -> void().
delete_vbos( VBOIds ) ->
	delete_buffers( VBOIds ).



% EBO subsection.


% @doc Returns a new, unique, EBO identifier.
-spec generate_ebo_id() -> ebo_id().
generate_ebo_id() ->
	generate_buffer_id().


% @doc Returns the specified number of new, unique, EBO identifiers.
-spec generate_ebo_ids( count() ) -> [ ebo_id() ].
generate_ebo_ids( Count ) ->
	generate_buffer_ids( Count ).



% @doc Creates a EBO identifier, sets it as the currently active one, and
% returns it.
%
% From now on, all operations done regarding the ?GL_ELEMENT_ARRAY_BUFFER target
% will be applied to this EBO.
%
-spec set_new_ebo() -> ebo_id().
set_new_ebo() ->
	set_current_ebo_from_id( generate_ebo_id() ).


% @doc Sets the specified EBO as the currently active one.
%
% From now on, all operations done regarding the ?GL_ELEMENT_ARRAY_BUFFER target
% will be applied to this buffer.
%
% Lower-level, defined to centralise calls.
%
% Returns, if useful, the specified identifier, for chaining.
%
-spec set_current_ebo_from_id( ebo_id() ) -> ebo_id().
set_current_ebo_from_id( EBOId ) ->

	% To attach the buffer specified from its ID to the currently active
	% (i.e. bound) element buffer object (EBO) in the GL context:
	%
	gl:bindBuffer( ?GL_ELEMENT_ARRAY_BUFFER, EBOId ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	EBOId.



% @doc Assigns the specified buffer to the currently active EBO, and associates
% it a default usage profile.
%
% The specified data will thus typically be transferred to the graphic card.
%
-spec assign_current_ebo( array_buffer() ) -> void().
assign_current_ebo( ArrayBuffer ) ->
	assign_current_ebo( ArrayBuffer, ?default_ebo_usage_hint ).


% @doc Assigns the specified buffer and associated usage settings to the
% currently active EBO.
%
% The specified data will thus typically be transferred to the graphic card.
%
-spec assign_current_ebo( array_buffer(), buffer_usage_hint() ) -> void().
assign_current_ebo( ArrayBuffer, BufferUsageHint ) ->
	assign_array( ArrayBuffer, _BindTarget=?GL_ELEMENT_ARRAY_BUFFER,
				  BufferUsageHint ).



% @doc Assigns the specified vertices to a new EBO (associated to a default
% usage profile), whose identifier is returned.
%
-spec assign_indices_to_new_ebo( [ index() ] ) -> ebo_id().
assign_indices_to_new_ebo( Indices ) ->
	assign_indices_to_new_ebo( Indices, ?default_ebo_usage_hint ).


% @doc Assigns the specified vertices to a new EBO, associates the specified
% usage profile, and returns the identifier of this EBO.
%
-spec assign_indices_to_new_ebo( [ index() ], buffer_usage_hint()  ) ->
											ebo_id().
assign_indices_to_new_ebo( Indices, BufferUsageHint ) ->

	EBOId = set_new_ebo(),

	EBOBuffer = bin_utils:concatenate_as_uint32s( Indices ),

	assign_current_ebo( EBOBuffer, BufferUsageHint ),

	EBOId.



% @doc Renders the specified primitives based on the enabled EBOs.
-spec render_from_enabled_ebos( gl_primitive_type(), count() ) -> void().
render_from_enabled_ebos( PrimType, VertexCount ) ->

	gl:drawElements( PrimType, VertexCount, _IndexType=?GL_UNSIGNED_INT,
					 _OffsetOrIdArray=0 ),

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).



% @doc Deletes the specified EBO.
delete_ebo( EBOId ) ->
	delete_buffer( EBOId ).


% @doc Deletes the specified EBOs.
-spec delete_ebos( [ ebo_id() ] ) -> void().
delete_ebos( EBOIds ) ->
	delete_buffers( EBOIds ).



% Uniform variable subsection.


% @doc Returns the identifier of the active uniform variable (if any) specified
% by its name in the specified program.
%
% Refer to get_uniform_id/2 for further details.
%
-spec get_maybe_uniform_id( uniform_name(), program_id() ) ->
											maybe( uniform_id() ).
get_maybe_uniform_id( UniformName, ProgId ) ->

	MaybeUniformId = case gl:getUniformLocation( ProgId, UniformName ) of

		-1 ->
			undefined;

		Id ->
			Id

	end,

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	MaybeUniformId.



% @doc Returns the identifier of the active uniform variable specified by its
% name in the specified program. Throws an exception if such a variable does not
% exist.
%
% The actual locations assigned to uniform variables are not known until the
% program object is linked successfully.
%
% An uniform variable that is declared in shader(s) yet not used will be removed
% during the linking step, and thus not be found.
%
-spec get_uniform_id( uniform_name(), program_id() ) -> uniform_id().
get_uniform_id( UniformName, ProgId ) ->
	case get_maybe_uniform_id( UniformName, ProgId ) of

		undefined ->
			throw( { unknown_uniform_variable, UniformName, ProgId } );

		Id ->
			Id

	end.



% @doc Sets the specified uniform variable to the specified GLSL vector of 3
% floats, in the context of the currently installed shader program.
%
-spec set_uniform_3f( uniform_id(), vector3() ) -> void().
set_uniform_3f( UniformId, _Vec3=[ X, Y, Z ] ) ->
	gl:uniform3f( UniformId, X, Y, Z ),
%set_uniform_3f( UniformId, Vec3 ) ->
%	gl:uniform3fv( UniformId, [ list_to_tuple( Vec3 ) ] ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).


% @doc Sets the specified uniform array variable to the specified list of GLSL
% vectors of 3 floats, in the context of the currently installed shader program.
%
-spec set_uniform_3fv( uniform_id(), [ vector3() ] ) -> void().
set_uniform_3fv( UniformId, Vec3s ) ->
	gl:uniform3fv( UniformId, to_gl_vectors( Vec3s ) ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).


% @doc Sets the specified uniform variable to the specified GLSL vector of 4
% floats, in the context of the currently installed shaderprogram.
%
-spec set_uniform_4f( uniform_id(), vector4() ) -> void().
set_uniform_4f( UniformId, _Vec4=[ X, Y, Z, W ] ) ->
	gl:uniform4f( UniformId, X, Y, Z, W ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).



% @doc Converts the specified list of (Myriad) vectors (hence lists) of any
% dimension into a list of vectors suitable for the gl API (hence tuples).
%
-spec to_gl_vectors( [ vector2() ] ) -> [ gl_vec2()   ];
				   ( [ vector3() ] ) -> [ gl_vec3()   ];
				   ( [ vector4() ] ) -> [ gl_vec4()   ];
				   ( [ vector()  ] ) -> [ gl_vector() ].
to_gl_vectors( Vecs ) ->
	[ list_to_tuple( V ) || V <- Vecs ].


% Inlined versions (e.g. to_gl_vec2/1 could make sense).
