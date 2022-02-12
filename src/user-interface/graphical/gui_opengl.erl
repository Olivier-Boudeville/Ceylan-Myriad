% Copyright (C) 2017-2022 Olivier Boudeville
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
% Creation date: Monday, September 11, 2017.


% @doc Gathering of various facilities for <b>OpenGL rendering</b>, notably done
% through wxWidgets.
%
% See gui_opengl_test.erl for the corresponding test.
%
% See gui.erl for more general rendering topics.
%
% Refer to [https://myriad.esperide.org/#for-3d-applications] for further
% information.
%
-module(gui_opengl).


% Ex: for WX_GL_MIN_RED.
%-include_lib("wx/include/wx.hrl").

%-include_lib("wx/include/gl.hrl").
%-include_lib("wx/include/glu.hrl").


% Implementation notes:
%
% Of course at least some sorts of OpenGL software and/or hardware support must
% be available on the local host.
%
% See [https://myriad.esperide.org/#for-3d-applications]


% Of interest with the Erlang standard distribution, one may refer to the
% following modules:
%
% - gl: [https://www.erlang.org/doc/man/gl.html]
% - glu: [https://www.erlang.org/doc/man/glu.html]
%
% One may also refer to the wx:demo/0 function (see
% lib/wx/examples/demo/ex_gl.erl) and to lib/wx/test/wx_opengl_SUITE.erl.
%
% For some reason, they use wx defines (ex: ?WX_GL_DOUBLEBUFFER) rather than
% pure OpenGL ones (ex: ?GL_DOUBLEBUFFER). Of course we prefer the latter ones.
%
% Note that almost all OpenGL operations require that an OpenGL context already
% exists, otherwise an no_gl_context error report is expected to be triggered.

% Note: with OpenGL, angles are in degrees.

% For most OpenGL operations to succeed, first the wx/gl NIF must be loaded
% (hence after gui:start/0), and a GL context must be available (thus cannot
% happen before the main frame is shown).



% For the numerous GL defines notably:
-include("gui_opengl.hrl").

% For the mesh record:
-include("mesh.hrl").


-type enum() :: non_neg_integer().
% A value belonging to an OpenGL enumeration.


-type glxinfo_report() :: [ ustring() ].
% A report issued by the glxinfo executable.


-type vendor_name() :: bin_string().
% The name of the OpenGL vendor of a driver, that is the company responsible for
% the corresponding OpenGL implementation.
%
% For example: <<"FOOBAR Corporation">>.


-type renderer_name() :: bin_string().
% The name of the OpenGL renderer of a driver (typically specific to a
% particular configuration of a hardware platform).
%
% For example: <<"FOOBAR Frobinator GTX 1060 6GB/PCIe/SSE2">>.


-type platform_identifier() :: { vendor_name(), renderer_name() }.
% Uniquely identifies an (OpenGL driver) platform; does not change from release
% to release and should be used by platform-recognition algorithms.


-opaque gl_canvas() :: wxGLCanvas:wxGLCanvas().
% An OpenGL-based, back-buffered canvas (not to be mixed with a basic
% gui:canvas/0 one), to which an OpenGL context shall be set in order to execute
% OpenGL commands.


% See https://docs.wxwidgets.org/3.0/glcanvas_8h.html#wxGL_FLAGS for more
% backend details:
%
-type device_context_attribute() ::

	% Use true color (the default if no attributes at all are specified); do not
	% use a palette. Then each element contains all four components, each of
	% which is clamped to the range [0,1]:
	%
	'rgba' | 'bgra'

	% To enable double-buffering if present:
	| 'double_buffer'

	% Use red buffer with at least this number of bits:
	| { 'min_red_size', bit_size() }

	% Use green buffer with at least this number of bits:
	| { 'min_green_size', bit_size() }

	% Use blue buffer with at least this number of bits:
	| { 'min_blue_size', bit_size() }

	% The number of bits for Z-buffer (typically 0, 16 or 32):
	| { 'depth_buffer_size', bit_size() }.


-type gl_canvas_option() :: { 'gl_attributes', [ device_context_attribute() ] }
						  | gui_wx_backend:other_wx_device_context_attribute().
% Options of an OpenGL canvas.


-opaque gl_context() :: wxGLContext:wxGLContext().
% An OpenGL context represents the state of an OpenGL state machine and the
% connection between OpenGL and the running system.


-type factor() :: math_utils:factor().
% A floating-point factor, typically in [0.0,1.0].


-type length_factor() :: math_utils:factor().
% A floating-point factor, typically in [0.0,1.0], typically to designate
% widths, heights, etc.


-type texture_id() :: non_neg_integer().
% An OpenGL texture "name" (meant to be unique), an identifier.

-type texture() :: #texture{}.
% Information regarding to a (2D) texture.

-type mipmap_level() :: non_neg_integer().
% 0 is the base level.


-type matrix_stack() :: ?GL_MODELVIEW
					  | ?GL_PROJECTION
					  | ?GL_TEXTURE
					  | ?GL_COLOR.
% The various matrix stacks available, a.k.a. as the current matrix mode.


-type shader_id() :: non_neg_integer().
% The identifier of (any kind of) a shader.


% Types of shaders, in pipeline order:

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


-type vertex_array_id() :: non_neg_integer().
% The identifier of a vertex array.


-type buffer_id() :: non_neg_integer().
% The identifier of a buffer, i.e. a "buffer object name".

-type vertex_attribute_buffer_id() :: non_neg_integer().
% The identifier of a vertex attribute buffer.

-type vertex_attribute_index() :: non_neg_integer().
% The index of a vertex attribute, in a vertex attribute buffer.

-type attribute_name() :: ustring().
% The name of a user-defined attribute, meant to be set through an associated
% index.

-type user_attribute() :: { vertex_attribute_index(), attribute_name() }.
% A user-defined attribute variable, meant to be associated to a generic vertex
% attribute index in a GLSL program.


-type program_id() :: non_neg_integer().
% The identifier of GLSL, shader-based program.



-opaque gl_error() :: enum().
% An error code reported by OpenGL.

-opaque glu_error() :: enum().
% An error code reported by GLU.


-opaque any_error() :: gl_error() | glu_error().
% An error code reported by OpenGL or GLU.


-opaque glu_id() :: non_neg_integer().
% An identifier (actually a pointer) returned by GLU (ex: when creating a
% quadrics). A null value usually means that there was not enough memory to
% allocate the object.


-export_type([ enum/0, glxinfo_report/0,
			   vendor_name/0, renderer_name/0, platform_identifier/0,
			   gl_canvas/0, gl_canvas_option/0,
			   device_context_attribute/0, gl_context/0,
			   factor/0, length_factor/0,
			   texture_id/0, texture/0, mipmap_level/0,
			   matrix_stack/0,

			   shader_id/0, vertex_shader_id/0,
			   tessellation_control_shader_id/0,
			   tessellation_evaluation_shader_id/0, geometry_shader_id/0,
			   fragment_shader_id/0, compute_shader_id/0,

			   vertex_array_id/0,
			   buffer_id/0, vertex_attribute_buffer_id/0,

			   vertex_attribute_index/0, attribute_name/0, user_attribute/0,

			   program_id/0,

			   gl_error/0, glu_error/0, any_error/0,
			   glu_id/0 ]).



-export([ get_vendor_name/0, get_renderer_name/0, get_platform_identifier/0,
		  get_version/0, get_shading_language_version/0,
		  get_supported_extensions/0, get_support_description/0,

		  is_hardware_accelerated/0, is_hardware_accelerated/1,
		  get_glxinfo_strings/0,

		  create_canvas/1, create_canvas/2,
		  create_context/1, set_context/2, swap_buffers/1,

		  load_texture_from_image/1,
		  load_texture_from_file/1, load_texture_from_file/2,

		  create_texture_from_text/4, create_texture_from_text/5,

		  render_texture/2, render_texture/3,

		  delete_texture/1, delete_textures/1,

		  get_texture_dimensions/1, generate_texture_id/0,

		  render_mesh/1,

		  enter_2d_mode/1, leave_2d_mode/0,

		  set_matrix/1, get_matrix/1,

		  compile_vertex_shader/1, compile_tessellation_control_shader/1,
		  compile_tessellation_evaluation_shader/1, compile_geometry_shader/1,
		  compile_fragment_shader/1, compile_compute_shader/1,

		  generate_program_from/2, generate_program/1, generate_program/2,

		  bindVertexBufferObject/2,

		  check_error/0, interpret_error/1 ]).



% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type any_file_path() :: file_utils:any_file_path().

-type bit_size() :: system_utils:bit_size().

-type any_vertex3() :: point3:any_vertex3().
-type point3() :: point3:point3().

-type unit_normal3() :: vector3:unit_normal3().

-type matrix4() :: matrix4:matrix4().


-type mesh() :: mesh:mesh().
-type indexed_face() :: mesh:indexed_face().
-type face_count() :: mesh:face_count().

-type dimensions() :: gui:dimensions().
-type width() :: gui:width().
-type height() :: gui:height().
-type window() :: gui:window().
-type brush() :: gui:brush().
-type coordinate() :: gui:coordinate().
-type position() :: gui:position().

-type color_by_decimal() :: gui_color:color_by_decimal().
-type render_rgb_color() :: gui_color:render_rgb_color().

-type color_buffer() :: gui_color:color_buffer().
-type rgb_color_buffer() :: gui_color:rgb_color_buffer().
-type rgba_color_buffer() :: gui_color:rgba_color_buffer().
-type alpha_buffer() :: gui_color:alpha_buffer().

-type image() :: gui_image:image().
-type image_format() :: gui_image:image_format().

-type font() :: gui_font:font().



% @doc Returns the name of the OpenGL vendor of the current driver, that is the
% company responsible for this OpenGL implementation.
%
% For example: <<"FOOBAR Corporation">>.
%
-spec get_vendor_name() -> vendor_name().
get_vendor_name() ->
	Res= gl:getString( ?GL_VENDOR ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),
	Res.



% @doc Returns the name of the OpenGL renderer of the current driver (typically
% specific to a particular configuration of a hardware platform).
%
% For example: <<"FOOBAR Frobinator GTX 1060 6GB/PCIe/SSE2">>.
%
-spec get_renderer_name() -> renderer_name().
get_renderer_name() ->
	Res = gl:getString( ?GL_RENDERER ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),
	Res.



% @doc Returns the platform driver identifier.
%
% For example: {<<"FOOBAR Corporation">>, <<"FOOBAR Frobinator GTX 1060
% 6GB/PCIe/SSE2">>}.
%
-spec get_platform_identifier() -> platform_identifier().
 get_platform_identifier() ->
	{ get_vendor_name(), get_renderer_name() }.



% @doc Returns the version / release number of the currently used OpenGL
% implementation.
%
% Example: "4.6.0 FOOBAR 495.44".
%
-spec get_version() -> ustring().
get_version() ->
	Res = gl:getString( ?GL_VERSION ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),
	Res.


% @doc Returns the version /release number of the currently used OpenGL
% shading language.
%
% Example: "4.60 FOOBAR".
%
-spec get_shading_language_version() -> ustring().
get_shading_language_version() ->
	Res = gl:getString( ?GL_SHADING_LANGUAGE_VERSION ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),
	Res.



% @doc Returns a list of the supported extensions, as strings.
%
% Ex: 390 extensions like "GL_AMD_multi_draw_indirect",
% "GL_AMD_seamless_cubemap_per_texture", etc.
%
-spec get_supported_extensions() -> [ ustring() ].
get_supported_extensions() ->
	ExtStr = gl:getString( ?GL_EXTENSIONS ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),
	text_utils:split( ExtStr, _Delimiters=[ $ ] ).



% @doc Returns a synthetic string describing the host-local OpenGL support, also
% known as the current GL connection.
%
-spec get_support_description() -> ustring().
get_support_description() ->

	VendStr = text_utils:format( "driver vendor: ~ts", [ get_vendor_name() ] ),

	RendStr = text_utils:format( "driver renderer: ~ts",
								 [ get_renderer_name() ] ),

	ImplStr = text_utils:format( "implementation version: ~ts",
								 [ get_version() ] ),

	ShadStr = text_utils:format( "shading language version: ~ts",
								 [ get_shading_language_version() ] ),

	Exts = get_supported_extensions(),

	% Way too long (ex: 390 extensions returned):
	%ExtStr = text_utils:format( "~B extensions: ~ts", [ length( Exts ),
	%   text_utils:strings_to_listed_string( Exts ) ] ),

	ExtStr = text_utils:format( "~B extensions supported", [ length( Exts ) ] ),

	text_utils:strings_to_string(
		[ VendStr, RendStr, ImplStr, ShadStr, ExtStr ] ).



% @doc Tells whether OpenGL hardware acceleration is available on this host,
% based on the glxinfo executable.
%
-spec is_hardware_accelerated() -> boolean().
is_hardware_accelerated() ->

	case get_glxinfo_strings() of

		undefined ->
			trace_utils:warning( "No glxinfo status obtained, supposing no "
								 "OpenGL hardware acceleration available." ),
			false;

		GlxinfoStrs ->
			is_hardware_accelerated( GlxinfoStrs )

	end.



% @doc Tells whether OpenGL hardware acceleration is available on this host,
% based on specified glxinfo report.
%
-spec is_hardware_accelerated( glxinfo_report() ) -> boolean().
is_hardware_accelerated( GlxinfoStrs ) ->

	% Ex: "direct rendering: Yes"
	case list_utils:get_element_at( GlxinfoStrs, _Index=3 ) of

		"direct rendering: " ++ Next ->
			case Next of

				"Yes" ->
					true;

				"No" ->
					false

			end;

		OtherAnswer ->
			trace_utils:warning_fmt( "Unexpected status ('~ts') for "
				"direct rendering, supposing no OpenGL hardware "
				"acceleration available.", [ OtherAnswer ] ),
			false

	end.



% @doc Returns the list of strings (if any) returned by glxinfo when requesting
% basic information.
%
% Of course the 'glxinfo' executable must be available on the PATH (install it
% on Arch Linux with 'pacman -S mesa-utils').
%
-spec get_glxinfo_strings() -> maybe( glxinfo_report() ).
get_glxinfo_strings() ->

	% Best diagnosis we know:
	Tool = "glxinfo",

	case executable_utils:lookup_executable( Tool ) of

		false ->
			trace_utils:warning_fmt( "No '~ts' tool found, "
									 "no status reported.", [ Tool ] ),
			undefined;

		ExecPath ->
			% -B: brief output, print only the basics.
			case system_utils:run_executable( ExecPath, [ "-B" ] ) of

				{ _ReturnCode=0, ReturnedStr } ->
					text_utils:split( ReturnedStr, "\n" );

				{ ErrorCode, ReturnedStr } ->
					trace_utils:error_fmt( "The ~ts query returned an error "
						"(code: ~B, message: '~ts'), no status reported.",
						[ Tool, ErrorCode, ReturnedStr ] ),
					undefined

			end

	end.



% @doc Creates and returns an OpenGL canvas with default settings: RGBA and
% double-buffering.
%
% Note: not to be mixed up with gui:create_canvas/1.
%
-spec create_canvas( window() ) -> gl_canvas().
create_canvas( Parent ) ->
	DefaultGLAttributes = [ rgba, double_buffer ],
	create_canvas( Parent, _Opts=[ { gl_attributes, DefaultGLAttributes } ] ).



% @doc Creates and returns an OpenGL canvas with specified settings.
%
% If the device context attributes are not set, following default apply: RGBA
% and double-buffering.
%
% Note: not to be mixed up with gui:create_canvas/1.
%
-spec create_canvas( window(), [ gl_canvas_option() ] ) -> gl_canvas().
create_canvas( Parent, Opts ) ->

	{ Attrs, OtherOpts } = list_table:extract_entry_with_defaults(
		_K=gl_attributes, _Def=[ rgba, double_buffer ], Opts ),

	%trace_utils:debug_fmt( "Creating a GL canvas with Attrs = ~p~n
	%   "and OtherOpts = ~p", [ Attrs, OtherOpts ] ),

	WxAttrs = gui_wx_backend:to_wx_device_context_attributes( Attrs ),

	OtherWxOpts = gui_wx_backend:get_window_options( OtherOpts ),

	WxOpts = [ { attribList, WxAttrs } | OtherWxOpts ],

	%trace_utils:debug_fmt( "WxOpts = ~p", [ WxOpts ] ),

	Res = wxGLCanvas:new( Parent, WxOpts ),

	% Commented-out, as not relevant (an OpenGL context may not already exist):
	%cond_utils:if_defined( myriad_check_opengl, check_error() ),

	Res.



% @doc Returns the OpenGL context obtained from the specified OpenGL canvas; it
% is created but not bound yet (not set as current, hence not usable yet, no
% OpenGL command can be issued yet).
%
-spec create_context( gl_canvas() ) -> gl_context().
create_context( Canvas ) ->
	Res = wxGLContext:new( Canvas ),

	% Commented-out, as the OpenGL context is not set as current yet:
	%cond_utils:if_defined( myriad_check_opengl, check_error() ),

	Res.



% @doc Sets the specified (OpenGL) context to the specified (OpenGL) canvas, so
% that it applies to the next operations (OpenGL calls) made on it.
%
% To be only called when the parent window is shown on screen; see
% gui_opengl_test.erl for an example thereof.
%
-spec set_context( gl_canvas(), gl_context() ) -> void().
set_context( Canvas, Context ) ->
	case wxGLCanvas:setCurrent( Canvas, Context ) of

		true ->
			ok;

		false ->
			throw( failed_to_set_opengl_context )

	end,
	cond_utils:if_defined( myriad_check_opengl, check_error() ).



% @doc Swaps the double-buffer of the corresponding OpenGL canvas (making the
% back-buffer the front-buffer and vice versa), so that the output of the
% previous OpenGL commands is displayed on this window.
%
% The corresponding window must already be shown.
%
% Includes a gl:flush/0.
%
-spec swap_buffers( gl_canvas() ) -> void().
swap_buffers( Canvas ) ->

	% wxGLCanvas:swapBuffers/1 may or may not include any kind of gl:flush/0; so
	% it is preferable to trigger one by ourselves.

	% Ensures that the drawing commands are actually directly triggered
	% (i.e. started, not necessarily completed; use gl:finish/0 to make this
	% operation synchronous, i.e. to wait for its end) rather than stored in a
	% buffer awaiting additional OpenGL commands:
	%
	gl:flush(),
	% More expensive, as blocks: gl:finish(),

	case wxGLCanvas:swapBuffers( Canvas ) of

		true ->
			ok;

		false ->
			throw( failed_to_swap_buffers )

	end,
	cond_utils:if_defined( myriad_check_opengl, check_error() ).



% @doc Creates a texture from the specified image instance.
%
% The image instance is safe to be deallocated afterwards.
%
-spec load_texture_from_image( image() ) -> texture().
load_texture_from_image( Image ) ->

	ImgWidth = wxImage:getWidth( Image ),
	ImgHeight = wxImage:getHeight( Image ),

	{ Width, Height } = get_texture_dimensions( ImgWidth, ImgHeight ),

	trace_utils:debug_fmt( "Image dimensions: {~B,~B}; texture dimensions: "
		"{~B,~B}.", [ ImgWidth, ImgHeight, Width, Height ] ),

	% wxImage is either RGB or RGBA:
	ColorBuffer = get_color_buffer( Image ),

	% Let's create the OpenGL texture:

	TextureId = generate_texture_id(),

	gl:bindTexture( ?GL_TEXTURE_2D, TextureId ),

	% Sets parameters regarding the current texture:
	gl:texParameteri( _Target=?GL_TEXTURE_2D, _TexParam=?GL_TEXTURE_MAG_FILTER,
					  _ParamValue=?GL_NEAREST ),

	gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST ),

	Format = case wxImage:hasAlpha( Image ) of

		true ->
			%trace_utils:debug( "RGBA image detected." ),
			?GL_RGBA;

		false ->
			%trace_utils:debug( "RGB image detected." ),
			?GL_RGB

	end,

	% Specifies this two-dimensional texture image:
	gl:texImage2D( _Tgt=?GL_TEXTURE_2D, _LOD=0, _InternalTexFormat=Format,
		Width, Height, _Border=0, _InputBufferFormat=Format,
		_PixelDataType=?GL_UNSIGNED_BYTE, ColorBuffer ),

	cond_utils:if_defined( myriad_check_opengl, check_error() ),

	%trace_utils:debug( "Texture loaded from image." ),

	#texture{ id=TextureId, width=ImgWidth, height=ImgHeight,
			  min_x=0.0, min_y=0.0,
			  max_x=ImgWidth / Width, max_y=ImgHeight / Height }.



% @doc Creates a texture from the specified image file.
%
% Prefer load_texture_from_file/2 if applicable.
%
-spec load_texture_from_file( any_file_path() ) -> texture().
load_texture_from_file( ImagePath ) ->

	Image = gui_image:create_from_file( ImagePath ),

	%trace_utils:debug_fmt( "Image loaded from '~ts' of size ~Bx~B.",
	%   [ ImagePath, wxImage:getWidth( Image ), wxImage:getHeight( Image ) ] ),

	Tex = load_texture_from_image( Image ),
	gui_image:destruct( Image ),
	Tex.



% @doc Creates a texture from the specified image file of the specified type.
-spec load_texture_from_file( image_format(), any_file_path() ) -> texture().
load_texture_from_file( ImageFormat, ImagePath ) ->
	Image = gui_image:create_from_file( ImageFormat, ImagePath ),
	Tex = load_texture_from_image( Image ),
	gui_image:destruct( Image ),
	Tex.



% @doc Creates a texture corresponding to the specified text, rendered with
% specified font, brush and color.
%
-spec create_texture_from_text( ustring(), font(), brush(),
								color_by_decimal() ) -> texture().
create_texture_from_text( Text, Font, Brush, Color ) ->
	create_texture_from_text( Text, Font, Brush, Color, _Flip=false ).


% @doc Creates a texture corresponding to the specified text, rendered with
% specified font, brush and color, flipping it vertically if requested.
%
-spec create_texture_from_text( ustring(), font(), brush(), color_by_decimal(),
								boolean() ) -> texture().
create_texture_from_text( Text, Font, Brush, Color, Flip ) ->
	% Directly deriving from lib/wx/examples/demo/ex_gl.erl:

	StrDims = { StrW, StrH } = gui_font:get_text_extent( Text, Font ),

	{ Width, Height } = get_texture_dimensions( StrDims ),

	%trace_utils:debug_fmt( "Text dimensions: {~B,~B}; "
	%   "texture dimensions: {~B,~B}.", [ StrW, StrH, Width, Height ] ),

	Bmp = wxBitmap:new( Width, Height ),

	cond_utils:if_defined( myriad_debug_gui_memory,
						   true = wxBitmap:isOk( Bmp ) ),

	DC = wxMemoryDC:new( Bmp ),

	cond_utils:if_defined( myriad_debug_gui_memory, true = wxDC:isOk( DC ) ),

	wxMemoryDC:setFont( DC, Font ),

	wxMemoryDC:setBackground( DC, Brush ),

	wxMemoryDC:clear( DC ),

	wxMemoryDC:setTextForeground( DC, _WhiteRGB={ 255, 255, 255 } ),

	wxMemoryDC:drawText( DC, Text, _Origin={ 0, 0 } ),

	Img = wxBitmap:convertToImage( Bmp ),

	BaseImg = case Flip of

		true ->
			FlippedImg = wxImage:mirror( Img, [ { horizontally, false } ] ),
			wxImage:destroy( Img ),
			FlippedImg;

		false ->
			Img

	end,

	AlphaValues = wxImage:getData( BaseImg ),
	ColorizedData = gui_image:colorize( AlphaValues, Color ),

	wxImage:destroy( BaseImg ),
	wxBitmap:destroy( Bmp ),
	wxMemoryDC:destroy( DC ),

	TextureId = generate_texture_id(),

	gl:bindTexture( ?GL_TEXTURE_2D, TextureId ),

	gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR ),

	gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR ),

	gl:texEnvi( ?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE ),

	%gl:pixelStorei( ?GL_UNPACK_ROW_LENGTH, 0 ),
	%gl:pixelStorei( ?GL_UNPACK_ALIGNMENT, 2 ),

	% Specifies this two-dimensional texture image:
	gl:texImage2D( _Tgt=?GL_TEXTURE_2D, _LOD=0, _InternalTexFormat=?GL_RGBA,
		Width, Height, _Border=0, _DataFormat=?GL_RGBA, _Type=?GL_UNSIGNED_BYTE,
		ColorizedData ),

	cond_utils:if_defined( myriad_check_opengl, check_error() ),

	%trace_utils:debug( "Texture loaded from text." ),

	#texture{ id=TextureId, width=StrW, height=StrH, min_x=0.0, min_y=0.0,
			  max_x=StrW/Width, max_y=StrH/Height }.



% @doc Renders the specified texture, at the specified position.
-spec render_texture( texture(), position() ) -> void().
render_texture( Texture, _Pos={X,Y} ) ->
	render_texture( Texture, X, Y ).



% @doc Renders the specified texture, at the specified position.
-spec render_texture( texture(), coordinate(), coordinate() ) -> void().
render_texture( #texture{ id=TextureId,
						  width=Width,
						  height=Height,
						  min_x=MinXt,
						  min_y=MinYt,
						  max_x=MaxXt,
						  max_y=MaxYt }, Xp, Yp ) ->

	%trace_utils:debug_fmt( "Rendering texture ~w (size: ~wx~w), from {~w,~w} "
	%   "to {~w,~w}.", [ TextureId, Width, Height, MinX, MinY, MaxX, MaxY ] ),

	gl:bindTexture( ?GL_TEXTURE_2D, TextureId ),

	% Covers the rectangular texture area thanks to two (right-angled) triangles
	% sharing an edge:
	%
	% (a glRect*() might be relevant as well)
	%
	gl:'begin'( ?GL_TRIANGLE_STRIP ),

	OtherXp = Xp + Width div 2,
	OtherYp = Yp + Height div 2,
	%OtherXp = Xp + Width,
	%OtherY = Yp + Height,

	% Associating a (2D) texture coordinate to each vertex:
	gl:texCoord2f( MinXt, MinYt ), gl:vertex2i( Xp, Yp ),
	gl:texCoord2f( MaxXt, MinYt ), gl:vertex2i( OtherXp, Yp ),
	gl:texCoord2f( MinXt, MaxYt ), gl:vertex2i( Xp, OtherYp ),
	gl:texCoord2f( MaxXt, MaxYt ), gl:vertex2i( OtherXp, OtherYp ),

	gl:'end'().



% @doc Deletes the specified texture.
-spec delete_texture( texture() ) -> void().
delete_texture( #texture{ id=Id } ) ->
	gl:deleteTextures( [ Id ] ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ).


% @doc Deletes the specified textures.
-spec delete_textures( [ texture() ] ) -> void().
delete_textures( Textures ) ->
	gl:deleteTextures( [ Id || #texture{ id=Id } <- Textures ] ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ).


% @doc Returns texture dimensions that are suitable for a (2D) content of the
% specified dimensions.
%
-spec get_texture_dimensions( dimensions() ) -> dimensions().
get_texture_dimensions( _Dims={ W, H } ) ->
	get_texture_dimensions( W, H ).


% @doc Returns texture dimensions that are suitable for a (2D) content of the
% specified dimensions.
%
-spec get_texture_dimensions( width(), height() ) -> dimensions().
get_texture_dimensions( Width, Height ) ->
	{ math_utils:get_next_power_of_two( Width ),
	  math_utils:get_next_power_of_two( Height ) }.



% @doc Returns a new, unique, texture identifier.
-spec generate_texture_id() -> texture_id().
generate_texture_id() ->
	[ TextureId ] = gl:genTextures( _Count=1 ),
	TextureId.



% @doc Renders the specified mesh in a supposedly appropriate OpenGL context.
%
% See gui_opengl_test.erl for an usage example.
%
-spec render_mesh( mesh() ) -> void().
render_mesh( #mesh{ vertices=Vertices,
					faces=IndexedFaces,
					normal_type=per_face,
					normals=Normals,
					rendering_info={ color, per_vertex, Colors } } ) ->

	% We could batch the commands sent to the GUI backend (ex: with wx:batch/1
	% or wx:foreach/2).

	% We currently suppose we have quad-based faces:
	gl:'begin'( ?GL_QUADS ),

	render_faces( IndexedFaces, _FaceCount=1, Vertices, Normals, Colors ),

	gl:'end'().



% @doc Enters in 2D mode for the specified window: applies relevant general
% state changes, and specific to modelview (which is reset) and to projection (a
% projection matrix relevant for 2D operations is applied).
%
% Refer to https://myriad.esperide.org/#2d-referential for more details.
%
-spec enter_2d_mode( window() ) -> void().
enter_2d_mode( Window ) ->

	% Directly deriving from lib/wx/examples/demo/ex_gl.erl:

	{ Width, Height } = wxWindow:getClientSize( Window ),

	% General state changes; depending on the current OpenGL state, other
	% elements may have to be updated:

	gl:pushAttrib( ?GL_ENABLE_BIT ),

	gl:disable( ?GL_DEPTH_TEST ),
	gl:disable( ?GL_CULL_FACE ),
	gl:enable( ?GL_TEXTURE_2D ),

	% This allows the alpha blending of 2D textures with the scene:
	gl:enable( ?GL_BLEND ),
	gl:blendFunc( ?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA ),

	% Updating first the projection matrix for 2D:

	gl:matrixMode( ?GL_PROJECTION ),
	gl:pushMatrix(),
	gl:loadIdentity(),

	% In all MyriadGUI referentials mentioned, abscissas are to increase when
	% going from left to right.
	%
	% As for ordinates, with the Myriad 2D referential (refer to the 'Geometric
	% Conventions' in Myriad's technical manual), like for the backend
	% coordinates (ex: SDL, wxWidgets), they are to increase when going from top
	% to bottom.
	%
	% It is the opposite by default with OpenGL (increasing from bottom to top;
	% the elements would therefore be upside-down in the OpenGL world), so in
	% the next orthogonal projection bottom and top coordinates are mirrored;
	% then OpenGL complies with the previous convention.
	%
	% Doing so is more relevant than flipping the textures/images themselves, as
	% the projection also applies to mouse coordinates.

	% Multiplies the projection matrix with this orthographic one, assuming that
	% the eye is located at (0, 0, 0); implements the MyriadGUI 2D conventions,
	% with pixel-level coordinates (another option could have been to rely on
	% normalised, definition-independent coordinates, ranging in [0.0, 1.0]):
	%
	% (corresponds to glu:ortho2D/4)
	%
	gl:ortho( _Left=0.0, _Right=float( Width ), _Bottom=float( Height ),
			  _Top=0.0, _Near=-1.0, _Far=1.0 ),


	% Then reseting the modelview matrix:
	gl:matrixMode( ?GL_MODELVIEW ),
	gl:pushMatrix(),
	gl:loadIdentity(),

	cond_utils:if_defined( myriad_check_opengl, check_error() ).



% @doc Leaves the 2D mode, resets modelview and projection matrices.
-spec leave_2d_mode() -> void().
leave_2d_mode() ->
	gl:matrixMode( ?GL_MODELVIEW ),
	gl:popMatrix(),

	gl:matrixMode( ?GL_PROJECTION ),
	gl:popMatrix(),

	gl:popAttrib().



% @doc Replaces the current OpenGL matrix (top of the currently selected stack,
% see matrix_stack/0) with the specified matrix4 instance.
%
-spec set_matrix( matrix4() ) -> void().
set_matrix( M=identity_4  )  ->
	set_matrix( matrix4:to_canonical( M ) );

% Works both for canonical_matrix4 and compact_matrix4:
set_matrix( M ) ->

	% Removes the record tag, resulting in a tuple of 16 or 12 coordinates
	% respectively:
	%
	ShrunkTuple = erlang:delete_element( _Index=1, M ),

	% 'f' float suffix, not 'd' by definition of matrix4.
	%
	% Transpose, as gl:loadMatrixf expects column-major order, not row-major
	% one:
	%
	gl:loadTransposeMatrixf( ShrunkTuple ).



% @doc Returns the current OpenGL matrix at the top of the specified stack, as a
% matrix4 instance.
%
-spec get_matrix( matrix_stack() ) -> matrix4().
get_matrix( _Stack ) ->
	% Not relevant: gl:getDoublev( Stack )
	% and gl:get/1 not available through NIF.
	% gl:get( Stack ).
	throw( not_implemented ).



% GLSL section.


% @spec Loads and compiles a vertex shader from the specified source file (whose
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

	trace_utils:debug_fmt( "Compiling vertex shader '~ts'.",
						   [ VertexShaderPath ] ),

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
			case MaybeLogStr of

				undefined ->
					ok;

				LogStr ->
					trace_utils:warning_fmt( "Compilation of the vertex shader "
						"defined in '~ts' succeeded, yet reported that '~ts'.",
						[ VertexShaderPath, LogStr ] )

			end;

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

	VertexShaderId.



% @spec Loads and compiles a tessellation control shader from the specified
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

	trace_utils:debug_fmt( "Compiling tessellation control shader '~ts'.",
						   [ TessCtrlShaderPath ] ),

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
			case MaybeLogStr of

				undefined ->
					ok;

				LogStr ->
					trace_utils:warning_fmt( "Compilation of the tessellation "
						"control shader defined in '~ts' succeeded, "
						"yet reported that '~ts'.",
						[ TessCtrlShaderPath, LogStr ] )

			end;

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

	TessCtrlShaderId.



% @spec Loads and compiles a tessellation evaluation shader from the specified
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

	trace_utils:debug_fmt( "Compiling tessellation evaluation shader '~ts'.",
						   [ TessEvalShaderPath ] ),

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
			case MaybeLogStr of

				undefined ->
					ok;

				LogStr ->
					trace_utils:warning_fmt( "Compilation of the tessellation "
						"evaluation shader defined in '~ts' succeeded, "
						"yet reported that '~ts'.",
						[ TessEvalShaderPath, LogStr ] )

			end;

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

	TessEvalShaderId.




% @spec Loads and compiles a geometry shader from the specified source file
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

	trace_utils:debug_fmt( "Compiling geometry shader '~ts'.",
						   [ GeometryShaderPath ] ),

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
			case MaybeLogStr of

				undefined ->
					ok;

				LogStr ->
					trace_utils:warning_fmt( "Compilation of the geometry "
						"shader defined in '~ts' succeeded, yet reported "
						"that '~ts'.", [ GeometryShaderPath, LogStr ] )

			end;

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

	GeometryShaderId.




% @spec Loads and compiles a fragment shader from the specified source file
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

	trace_utils:debug_fmt( "Compiling fragment shader '~ts'.",
						   [ FragmentShaderPath ] ),

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
			case MaybeLogStr of

				undefined ->
					ok;

				LogStr ->
					trace_utils:warning_fmt( "Compilation of the fragment "
						"shader defined in '~ts' succeeded, yet reported "
						"that '~ts'.", [ FragmentShaderPath, LogStr ] )

			end;

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

	FragmentShaderId.


% @spec Loads and compiles a compute shader from the specified source file
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

	trace_utils:debug_fmt( "Compiling compute shader '~ts'.",
						   [ ComputeShaderPath ] ),

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
			case MaybeLogStr of

				undefined ->
					ok;

				LogStr ->
					trace_utils:warning_fmt( "Compilation of the compute "
						"shader defined in '~ts' succeeded, yet reported "
						"that '~ts'.", [ ComputeShaderPath, LogStr ] )

			end;

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

	ComputeShaderId.



% @spec Generates a GLSL program from the shaders whose source files are
% specified: loads and compiles the specified vertex and fragment shaders (with
% no user-specified attributes defined), links them in a corresponding program,
% and returns its identifier.
%
-spec generate_program_from( any_file_path(), any_file_path() ) -> program_id().
generate_program_from( VertexShaderPath, FragmentShaderPath ) ->

	VertexShaderId = compile_vertex_shader( VertexShaderPath ),
	FragmentShaderId = compile_fragment_shader( FragmentShaderPath ),

	generate_program( _ShaderIds= [ VertexShaderId, FragmentShaderId ] ).



% @spec Generates a GLSL program from the (already loaded and compiled) shaders
% whose identifiers are specified, with no user-specified attributes defined:
% links these shaders in a corresponding program, and returns its identifier.
%
% Deletes the specified shaders once the program is generated.
%
-spec generate_program( [ shader_id() ] ) -> program_id().
generate_program( ShaderIds ) ->
	generate_program( ShaderIds, _UserAttributes=[] ).



% @spec Generates a GLSL program from the (already loaded and compiled) shaders
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

	[ gl:attachShader( ProgramId, ShdId ) || ShdId <- ShaderIds ],

	% Any attribute must be bound before linking:
	[ gl:bindAttribLocation( ProgramId, Idx, AttrName )
									|| { Idx, AttrName } <- UserAttributes ],

	gl:linkProgram( ProgramId ),

	MaybeLogStr = case gl:getProgramiv( ProgramId, ?GL_INFO_LOG_LENGTH ) of

		0 ->
			undefined;

		InfoLen ->
			gl:getProgramInfoLog( ProgramId, InfoLen )

	end,

	% Now checks linking outcome:
	case gl:getProgramiv( ProgramId, ?GL_LINK_STATUS ) of

		?GL_TRUE ->
			case MaybeLogStr of

				undefined ->
					ok;

				LogStr ->
					trace_utils:warning_fmt( "Linking of the program from "
						"specified shaders succeeded, yet reported that '~ts'.",
						[ LogStr ] )

			end;

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

	[ begin
		gl:detachShader( ProgramId, ShdId ),
		gl:deleteShader( ShdId )
	  end || ShdId <- ShaderIds ],

	ProgramId.



% @doc Binds the specified vertices in a new vertex attribute buffer whose
% identifier is returned.
%
-spec bindVertexBufferObject( [ point3() ], enum() ) ->
			vertex_attribute_buffer_id().
bindVertexBufferObject( Vertices, UsageHint ) ->

	% One (integer) identifier of array buffer wanted:
	[ VertexBufferId ] = gl:genBuffers( _Count=1 ),

	gl:bindBuffer( ?GL_ARRAY_BUFFER, VertexBufferId ),

	VBuffer = point3:to_buffer( Vertices ),

	gl:bufferData( ?GL_ARRAY_BUFFER, byte_size( VBuffer ), VBuffer, UsageHint ),

	cond_utils:if_defined( myriad_check_opengl, check_error() ),

	VertexBufferId.



% @doc Renders the specified indexed faces.
-spec render_faces( [ indexed_face() ], face_count(), [ any_vertex3() ],
					[ unit_normal3() ], [ render_rgb_color() ] ) -> void().
render_faces( _IndexedFaces=[], _FaceCount, _Vertices, _Normals, _Colors ) ->
	ok;


% We have quad-based faces here:
render_faces( _IndexedFaces=[ [ V1Idx, V2Idx, V3Idx, V4Idx ] | T ], FaceCount,
			  Vertices, Normals, Colors ) ->

	gl:normal3fv( list_to_tuple( lists:nth( FaceCount, Normals ) ) ),

	gl:color3fv( lists:nth( V1Idx, Colors ) ),
	gl:texCoord2f( 0.0, 0.0 ),
	gl:vertex3fv( lists:nth( V1Idx, Vertices ) ),

	gl:color3fv( lists:nth( V2Idx, Colors ) ),
	gl:texCoord2f( 1.0, 0.0 ),
	gl:vertex3fv( lists:nth( V2Idx, Vertices ) ),

	gl:color3fv( lists:nth( V3Idx, Colors ) ),
	gl:texCoord2f( 1.0, 1.0 ),
	gl:vertex3fv( lists:nth( V3Idx, Vertices ) ),

	gl:color3fv( lists:nth( V4Idx, Colors ) ),
	gl:texCoord2f( 0.0, 1.0 ),
	gl:vertex3fv( lists:nth( V4Idx, Vertices ) ),

	render_faces( T, FaceCount+1, Vertices, Normals, Colors );

% We have triangles-based faces here:
render_faces( _IndexedFaces=[ [ V1Idx, V2Idx, V3Idx ] | T ], FaceCount,
			  Vertices, Normals, Colors ) ->

	gl:normal3fv( list_to_tuple( lists:nth( FaceCount, Normals ) ) ),

	gl:color3fv( lists:nth( V1Idx, Colors ) ),
	gl:texCoord2f( 0.0, 0.0 ),
	gl:vertex3fv( lists:nth( V1Idx, Vertices ) ),

	gl:color3fv( lists:nth( V2Idx, Colors ) ),
	gl:texCoord2f( 1.0, 0.0 ),
	gl:vertex3fv( lists:nth( V2Idx, Vertices ) ),

	gl:color3fv( lists:nth( V3Idx, Colors ) ),
	gl:texCoord2f( 1.0, 1.0 ),
	gl:vertex3fv( lists:nth( V3Idx, Vertices ) ),

	render_faces( T, FaceCount+1, Vertices, Normals, Colors ).



% @doc Returns the RGB or RGBA color buffer corresponding to the specified
% image.
%
% The returned buffer shall be "const", in the sense of being neither be
% deallocated nor assigned to any image.
%
-spec get_color_buffer( image() ) -> color_buffer().
get_color_buffer( Image ) ->

   RGBBuffer = wxImage:getData( Image ),

	case wxImage:hasAlpha( Image ) of

		true ->
			% Obtain a pointer to the array storing the alpha coordinates for
			% the pixels of this image:
			%
			AlphaBuffer = wxImage:getAlpha( Image ),
			merge_alpha( RGBBuffer, AlphaBuffer );

		false ->
			RGBBuffer

	end.



% @doc Merges the specified RGB and alpha buffers into a RGBA one.
-spec merge_alpha( rgb_color_buffer(), alpha_buffer() ) -> rgba_color_buffer().
merge_alpha( RGBBuffer, AlphaBuffer ) ->
	% These are bitstring generators:
	list_to_binary(
		lists:zipwith( fun( {R,G,B}, A ) ->
							<<R,G,B,A>>
					   end,
					   [ {R,G,B} || <<R,G,B>> <= RGBBuffer ],
					   [ A || <<A>> <= AlphaBuffer ] ) ).



% Error management section.


% @doc Checks whether an OpenGL-related error occurred; if yes, displays
% information regarding it, and throws an exception.
%
% Note that an OpenGL must already exist, otherwise a no_gl_context error will
% be triggered.
%
-spec check_error() -> void().
check_error() ->
	case gl:getError() of

		?GL_NO_ERROR ->
			ok;

		GlError ->
			Diagnosis = interpret_error( GlError ),
			trace_utils:error_fmt( "OpenGL error detected (~B): ~ts; aborting.",
								   [ GlError, Diagnosis ] ),

			% Stacktrace expected as may be useful (even if the error might have
			% happened some time before, after the last check):
			%
			throw( { opengl_error, GlError, Diagnosis } )

	end.


% @doc Returns a (textual) diagnosis regarding the specified OpenGL-related
% (including GLU) error.
%
-spec interpret_error( any_error() ) -> ustring().
% Reference being
% https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetError.xhtml:
interpret_error( ?GL_INVALID_ENUM ) ->
	"invald value specified for an enumerated argument (GL_INVALID_ENUM)";

interpret_error( ?GL_INVALID_VALUE ) ->
	"out-of-range numeric argument (GL_INVALID_VALUE)";

interpret_error( ?GL_INVALID_OPERATION ) ->
	"a specified operation is not allowed in the current state "
	"(GL_INVALID_OPERATION)";

interpret_error( ?GL_INVALID_FRAMEBUFFER_OPERATION ) ->
	"the framebuffer object is not complete (GL_INVALID_FRAMEBUFFER_OPERATION)";

interpret_error( ?GL_OUT_OF_MEMORY ) ->
	"there is not enough memory left to execute the command (GL_OUT_OF_MEMORY)";

interpret_error( ?GL_STACK_UNDERFLOW ) ->
	"an attempt has been made to perform an operation that would cause "
	"an internal stack to underflow (GL_STACK_UNDERFLOW)";

interpret_error( ?GL_STACK_OVERFLOW ) ->
	"an attempt has been made to perform an operation that would cause "
	"an internal stack to overflow (GL_STACK_OVERFLOW)";

interpret_error( ?GL_NO_ERROR ) ->
	"no OpenGL error reported (GL_NO_ERROR)";

interpret_error( OtherCode ) ->
	text_utils:format( "OpenGL-related error of code ~B, interpreted as '~ts'.",
		[ OtherCode, glu:errorString( OtherCode ) ] ).
