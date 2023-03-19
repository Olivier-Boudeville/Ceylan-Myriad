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


-type buffer_id() :: non_neg_integer().
% The identifier of a buffer, i.e. a "buffer object name".

-type vbo_id() :: buffer_id().
% The identifier of a Vertex Buffer Object (VBO).


-type vertex_attribute_index() :: non_neg_integer().
% The index of a vertex attribute, in a VBO.

-type attribute_name() :: ustring().
% The name of a user-defined attribute, meant to be set through an associated
% index.

-type user_attribute() :: { vertex_attribute_index(), attribute_name() }.
% A user-defined attribute variable, meant to be associated to a generic vertex
% attribute index in a GLSL program.


-export_type([ shader_id/0, vertex_shader_id/0,
			   tessellation_control_shader_id/0,
			   tessellation_evaluation_shader_id/0, geometry_shader_id/0,
			   fragment_shader_id/0, compute_shader_id/0,

			   program_id/0,
			   vao_id/0, buffer_id/0, vbo_id/0,
			   vertex_attribute_index/0, attribute_name/0, user_attribute/0 ]).


-export([ get_shading_language_version/0,

		  compile_vertex_shader/1, compile_tessellation_control_shader/1,
		  compile_tessellation_evaluation_shader/1, compile_geometry_shader/1,
		  compile_fragment_shader/1, compile_compute_shader/1,

		  generate_program_from/2, generate_program/1, generate_program/2,

		  bind_vertex_buffer_object/2 ]).



% Usage notes:
%
% To avoid compilation problems, shaders may be encoded in the ANSI/ASCII
% format. Starting with OpenGL 4.2, shaders can be encoded as UTF-8
% strings. According to the GLSL spec, non-ASCII characters are only allowed in
% comments.



% Shorthands:

-type any_file_path() :: file_utils:any_file_path().

-type ustring() :: text_utils:ustring().

-type any_vertex3() :: point3:any_vertex3().

-type buffer_usage_hint() :: gui_opengl:buffer_usage_hint().



% @doc Returns the version /release number of the currently used OpenGL
% shading language.
%
% Example: "4.60 FOOBAR".
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

		   gl:deleteProgram( ProgramId ),

			throw( { program_linking_failed, MsgStr } )

	end,
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	[ begin
		gl:detachShader( ProgramId, ShdId ),
		gl:deleteShader( ShdId )
	  end || ShdId <- ShaderIds ],
	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	ProgramId.



% @doc Binds the specified vertices in a new vertex object buffer (VBO) whose
% identifier is returned.
%
-spec bind_vertex_buffer_object( [ any_vertex3() ], buffer_usage_hint() ) ->
										vbo_id().
bind_vertex_buffer_object( Vertices, UsageHint ) ->

	% One (integer) identifier of array buffer wanted:
	[ VertexBufferId ] = gl:genBuffers( _Count=1 ),

	gl:bindBuffer( ?GL_ARRAY_BUFFER, VertexBufferId ),

	VBuffer = point3:to_buffer( Vertices ),

	gl:bufferData( ?GL_ARRAY_BUFFER, byte_size( VBuffer ), VBuffer,
		gui_opengl:buffer_usage_hint_to_gl( UsageHint ) ),

	cond_utils:if_defined( myriad_check_shaders, gui_opengl:check_error() ),

	VertexBufferId.
