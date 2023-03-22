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


-type vao_id() :: non_neg_integer().
% The identifier of a Vertex Array Object (VAO).
%
% A VAO is able to store multiple VBOs (up to one for vertices, the others for
% per-vertex attributes); a VAO corresponds to an homogeneous chunk of data,
% sent from the CPU-space in order to be stored in the GPU-space.
%
% The core profile requires a VAO to be used. A default VAO exists.



-type array_buffer() :: gl_buffer().
% An OpenGL/GLSL array buffer; for example a VBO or an EBO.


-type vbo() :: array_buffer().
% A Vertex Buffer Object, a (GLSL) buffer storing a piece of information (vertex
% coordinates, or normal, or colors, or texture coordinates, etc.) for each
% element of a series of vertices.


-type vbo_id() :: gl_buffer_id().
% The identifier of a Vertex Buffer Object (VBO).



-type vertex_attribute_index() :: non_neg_integer().
% The index of a vertex attribute, in a VBO.
%
% Corresponds to a location for a shader (like in `layout (location = 0)').


-type attribute_name() :: ustring().
% The name of a user-defined attribute, meant to be set through an associated
% index.


-type user_attribute() :: { vertex_attribute_index(), attribute_name() }.
% A user-defined attribute variable, meant to be associated to a generic vertex
% attribute index in a GLSL program.


-type component_count() :: count().
% The number of components (separate values of the same type) in a given vertex
% attribute.


-type stride() :: byte_size().
% The number of bytes between two vertex attributes of a given type (comprising
% its size).
%
% A null stride means that the buffer is tightly packed and that OpenGL will
% determine by itself the actual stride, equal here to the size of such a vertex
% attribute.


-type offset() :: byte_size().
% Offset (possibly null) at which the first vertex attribute begins in the
% buffer.


-type gl_primitive_type() :: ?GL_POINTS | ?GL_LINE_STRIP | ?GL_LINE_LOOP
	| ?GL_LINES | ?GL_LINE_STRIP_ADJACENCY | ?GL_LINES_ADJACENCY
	| ?GL_TRIANGLE_STRIP | ?GL_TRIANGLE_FAN | ?GL_TRIANGLES
	| ?GL_TRIANGLE_STRIP_ADJACENCY | ?GL_TRIANGLES_ADJACENCY | ?GL_PATCHES.
% A type of OpenGL primitive for rendering.


-type index() :: count().
% An index in an array, typically a VBO (then, a vertex index).
%
% Starts at zero.


-type ebo() :: array_buffer().
% An Element Buffer Object, a (GLSL) buffer storing indices to vertex data.


-type ebo_id() :: gl_buffer_id().
% The identifier of an Element Buffer Object (EBO).
%
% It is an array of indices to vertex data.



-export_type([ shader_id/0, vertex_shader_id/0,
			   tessellation_control_shader_id/0,
			   tessellation_evaluation_shader_id/0, geometry_shader_id/0,
			   fragment_shader_id/0, compute_shader_id/0,

			   program_id/0,
			   vao_id/0, vbo_id/0, vbo/0,
			   vertex_attribute_index/0, attribute_name/0, user_attribute/0,
			   component_count/0, stride/0, offset/0,
			   gl_primitive_type/0, index/0,

			   ebo/0, ebo_id/0 ]).


-export([ get_shading_language_version/0,

		  compile_vertex_shader/1, compile_tessellation_control_shader/1,
		  compile_tessellation_evaluation_shader/1, compile_geometry_shader/1,
		  compile_fragment_shader/1, compile_compute_shader/1,

		  generate_program_from/2, generate_program/1, generate_program/2,
		  install_program/1, delete_program/1,

		  generate_buffer_id/0, generate_buffer_ids/1,

		  generate_vao_id/0, generate_vao_ids/1, set_new_vao/0,
		  set_current_vao_from_id/1, unset_current_vao/0,
		  delete_vao/1, delete_vaos/1,

		  generate_vbo_id/0, set_new_vbo/0, set_current_vbo_from_id/1,
		  assign_current_vbo/1, assign_current_vbo/2,

		  assign_vertices_to_new_vbo/1, assign_vertices_to_new_vbo/2,

		  specify_vertex_attribute/1, specify_vertex_attribute/7,
		  enable_vertex_attribute/1, disable_vertex_attribute/1,

		  render_from_enabled_vbos/3,

		  delete_vbo/1, delete_vbos/1 ]).



% Usage notes:
%
% To avoid compilation problems, shaders may be encoded in the ANSI/ASCII
% format. Starting with OpenGL 4.2, shaders can be encoded as UTF-8
% strings. According to the GLSL spec, non-ASCII characters are only allowed in
% comments.


% Design notes:
%
% Buffer arrays and VBO (Vertex Buffer Object) are the same notions here; so we
% use exclusively the latter, for more clarity.



% Implementation notes:
%
% On modern OpenGL, there are no default vertex/fragment shaders on the GPU, so
% each application must define at least a vertex and fragment shader of its own.


% Default usage profile for VBOs:
-define( default_vbo_usage_hint, { draw, static } ).



% Shorthands:

-type count() :: basic_utils:count().

-type any_file_path() :: file_utils:any_file_path().

-type byte_size() :: system_utils:byte_size().

-type ustring() :: text_utils:ustring().

-type any_vertex3() :: point3:any_vertex3().


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

	MaybeLogStr = case gl:getShaderiv( FragmentShaderId,
									   ?GL_INFO_LOG_LENGTH ) of

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
-spec generate_program( [ shader_id() ], [ user_attribute() ] ) -> program_id().
generate_program( ShaderIds, UserAttributes ) ->

	% Creates an empty program object and returns a non-zero value by which it
	% can be referenced:
	%
	ProgramId = gl:createProgram(),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	[ gl:attachShader( ProgramId, ShdId ) || ShdId <- ShaderIds ],
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	% Any attribute must be bound before linking:
	[ gl:bindAttribLocation( ProgramId, Idx, AttrName )
									|| { Idx, AttrName } <- UserAttributes ],
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

	% (this is typically a call that may result in a SEGV)
	gl:bufferData( _BindTarget=?GL_ARRAY_BUFFER, byte_size( ArrayBuffer ),
		ArrayBuffer, gui_opengl:buffer_usage_hint_to_gl( BufferUsageHint ) ),

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).



% @doc Assigns the specified vertices in a new VBO (associated to a default
% usage profile), whose identifier is returned.
%
-spec assign_vertices_to_new_vbo( [ any_vertex3() ] ) -> vbo_id().
assign_vertices_to_new_vbo( Vertices ) ->
	assign_vertices_to_new_vbo( Vertices, ?default_vbo_usage_hint ).


% @doc Assigns the specified vertices in a new VBO, associates the specified
% usage profile, and returns the identifier of this VBO.
%
-spec assign_vertices_to_new_vbo( [ any_vertex3() ], buffer_usage_hint()  ) ->
											vbo_id().
assign_vertices_to_new_vbo( Vertices, BufferUsageHint ) ->

	VBOId = set_new_vbo(),

	VBOBuffer = point3:to_buffer( Vertices ),

	assign_current_vbo( VBOBuffer, BufferUsageHint ),

	VBOId.



% @doc Deletes the specified VBO.
delete_vbo( VBOId ) ->
	delete_vbos( [ VBOId ] ).


% @doc Deletes the specified VBOs.
-spec delete_vbos( [ vbo_id() ] ) -> void().
delete_vbos( VBOIds ) ->
	gl:deleteBuffers( VBOIds ),
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).



% @doc Tells OpenGL how the specified vertex attribute shall be interpreted in
% the currently active VBO, supposing 3 floats per vertex attribute directly in
% a tightly-packed buffer, and enables this attribute.
%
-spec specify_vertex_attribute( vertex_attribute_index() ) -> void().
specify_vertex_attribute( TargetVAttrIndex ) ->
	specify_vertex_attribute( TargetVAttrIndex, _ComponentCount=3,
		_ComponentType=?GL_FLOAT, _DoNormalise=false, _AttrStride=0,
		_Offset=0, _DoEnable=true ).


% @doc Tells OpenGL how the specified vertex attribute shall be interpreted in
% the currently active VBO, and enables this attribute if requested.
%
% Normalisation applies only to integer components.
%
-spec specify_vertex_attribute( vertex_attribute_index(), component_count(),
	gl_base_type(), boolean(), stride(), offset(), boolean() ) -> void().
specify_vertex_attribute( TargetVAttrIndex, ComponentCount, ComponentType,
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


% @doc Renders the specified primitives based on the enabled arrays (VBOs).
-spec render_from_enabled_vbos( gl_primitive_type(), index(), count() ) ->
												void().
render_from_enabled_vbos( PrimType, StartIndex, VertexCount ) ->

	gl:drawArrays( PrimType, StartIndex, VertexCount ),

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ).
