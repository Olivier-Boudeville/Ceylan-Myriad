% Copyright (C) 2010-2021 Olivier Boudeville
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


% For the numerous GL defines notably:
-include("gui_opengl.hrl").

% For the mesh record:
-include("mesh.hrl").


-type enum() :: non_neg_integer().
% A value belonging to an OpenGL enumeration.


-type glxinfo_report() :: [ ustring() ].
% A report issued by the glxinfo executable.


-opaque canvas() :: wxGLCanvas:wxGLCanvas().
% An OpenGL-based canvas (not to be mixed with a basic gui:canvas/0 one).


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


-type canvas_option() :: { 'gl_attributes', [ device_context_attribute() ] }
					   | gui_wx_backend:other_wx_device_context_attribute().


-opaque context() :: wxGLContext:wxGLContext().
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


-opaque gl_error() :: enum().
% An error code reported by OpenGL.


-opaque glu_id() :: non_neg_integer().
% An identifier (actually a pointer) returned by GLU (ex: when creating a
% quadrics). A null value usually means that there was not enough memory to
% allocate the object.


-export_type([ enum/0, glxinfo_report/0, canvas/0, canvas_option/0,
			   device_context_attribute/0, context/0,
			   factor/0, length_factor/0,
			   texture_id/0, texture/0, mipmap_level/0,
			   gl_error/0,
			   glu_id/0 ]).



-export([ get_vendor_name/0, get_renderer_name/0, get_version/0,
		  get_shading_language_version/0,

		  is_hardware_accelerated/0, is_hardware_accelerated/1,
		  get_glxinfo_strings/0,

		  create_canvas/1, create_canvas/2,
		  create_context/1, set_context/2, swap_buffers/1,

		  load_texture_from_image/1,
		  load_texture_from_file/1, load_texture_from_file/2,

		  create_texture_from_text/4, create_texture_from_text/5,
		  delete_texture/1, delete_textures/1,

		  get_texture_dimensions/1, generate_texture_id/0,

		  render_mesh/1,

		  check_error/0, interpret_error/1 ]).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type any_file_path() :: file_utils:any_file_path().

-type bit_size() :: system_utils:bit_size().

-type any_vertex3() :: point3:any_vertex3().
-type unit_normal3() :: vector3:unit_normal3().

-type mesh() :: mesh:mesh().
-type indexed_face() :: mesh:indexed_face().
-type face_count() :: mesh:face_count().

-type dimensions() :: gui:dimensions().
-type width() :: gui:width().
-type height() :: gui:height().
-type window() :: gui:window().
-type brush() :: gui:brush().


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
% For example: "FOOBAR Corporation".
%
-spec get_vendor_name() -> ustring().
get_vendor_name() ->
	Res= gl:getString( ?GL_VENDOR ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),
	Res.



% @doc Returns the name of the OpenGL renderer of the current driver (typically
% specific to a particular configuration of a hardware platform).
%
% For example: "FOOBAR Frobinator GTX 1060 6GB/PCIe/SSE2".
%
-spec get_renderer_name() -> ustring().
get_renderer_name() ->
	Res = gl:getString( ?GL_RENDERER ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),
	Res.



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
-spec create_canvas( window() ) -> canvas().
create_canvas( Parent ) ->
	create_canvas( Parent, _Opts=[ rgba, double_buffer ] ).



% @doc Creates and returns an OpenGL canvas with specified settings.
%
% If the device context attributes are not set, following default apply: RGBA
% and double-buffering.
%
% Note: not to be mixed up with gui:create_canvas/1.
%
-spec create_canvas( window(), [ canvas_option() ] ) -> canvas().
create_canvas( Parent, Opts ) ->

	{ Attrs, OtherOpts } = list_table:extract_entry_with_defaults(
		_K=gl_attributes, _Def=[ rgba, double_buffer ], Opts ),

	%trace_utils:debug_fmt( "Attrs = ~p~nOtherOpts = ~p",
	%   [ Attrs, OtherOpts ] ),

	WxAttrs = gui_wx_backend:to_wx_device_context_attributes( Attrs ),

	OtherWxOpts = gui_wx_backend:get_window_options( OtherOpts ),

	WxOpts = [ { attribList, WxAttrs } | OtherWxOpts ],

	%trace_utils:debug_fmt( "WxOpts = ~p", [ WxOpts ] ),

	Res = wxGLCanvas:new( Parent, WxOpts ),

	% Commented-out, as an OpenGL context may not already exist:
	%cond_utils:if_defined( myriad_check_opengl, check_error() ),

	Res.



% @doc Returns the OpenGL context obtained from the specified canvas.
-spec create_context( canvas() ) -> context().
create_context( Canvas ) ->
	Res = wxGLContext:new( Canvas ),

	% Commented-out, as an OpenGL context may not already exist:
	%cond_utils:if_defined( myriad_check_opengl, check_error() ),

	Res.



% @doc Sets the specified (OpenGL) context to the specified (OpenGL) canvas, so
% that it applies to the next operations (OpenGL calls) made on it.
%
% To be only called when the parent window is shown on screen. See
% gui_opengl_test.erl for an example thereof.
%
-spec set_context( canvas(), context() ) -> void().
set_context( Canvas, Context ) ->
	case wxGLCanvas:setCurrent( Canvas, Context ) of

		true ->
			ok;

		false ->
			throw( failed_to_set_opengl_context )

	end,
	cond_utils:if_defined( myriad_check_opengl, check_error() ).



% @doc Swaps the double-buffer of the corresponding window (making the
% back-buffer the front-buffer and vice versa), so that the output of the
% previous OpenGL commands is displayed on this window.
%
-spec swap_buffers( canvas() ) -> void().
swap_buffers( Canvas ) ->
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

	% Either RGB or RGBA:
	ColorBuffer = get_color_buffer( Image ),

	% Let's create the OpenGL texture:

	TextureId = generate_texture_id(),

	gl:bindTexture( ?GL_TEXTURE_2D, TextureId ),

	gl:texParameteri( _Target=?GL_TEXTURE_2D, _TexParam=?GL_TEXTURE_MAG_FILTER,
					  _ParamValue=?GL_NEAREST ),

	gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST ),

	Format = case wxImage:hasAlpha( Image ) of

		true ->
			?GL_RGBA;

		false ->
			?GL_RGB

	end,

	% Specifies this two-dimensional texture image:
	gl:texImage2D( _Tgt=?GL_TEXTURE_2D, _LOD=0, _InternalTexFormat=Format,
		Width, Height, _Border=0, _InputBufferFormat=Format,
		_PixelDataType=?GL_UNSIGNED_BYTE, ColorBuffer ),

	cond_utils:if_defined( myriad_check_opengl, check_error() ),

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

	{ W, H } = get_texture_dimensions( StrDims ),

	Bmp = wxBitmap:new( W, H ),

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
	ColourizedData = gui_image:colorize( AlphaValues, Color ),

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
		W, H, _Border=0, _DataFormat=?GL_RGBA, _Type=?GL_UNSIGNED_BYTE,
		ColourizedData ),

	#texture{ id=TextureId, width=StrW, height=StrH, min_x=0.0, min_y=0.0,
			  max_x=StrW / W, max_y=StrH / H }.


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
-spec render_mesh( mesh() ) -> void().
render_mesh( #mesh{ vertices=Vertices,
					faces=IndexedFaces,
					normal_type=per_face,
					normals=Normals,
					rendering_info={ color, per_vertex, Colors } } ) ->

	% We could batch the commands sent to the GUI backend (ex: with wx:batch/1):
	render_faces( IndexedFaces, _FaceCount=1, Vertices, Normals, Colors ).



% @doc Renders the specified indexed faces.
-spec render_faces( [ indexed_face() ], face_count(), [ any_vertex3() ],
					[ unit_normal3() ], [ render_rgb_color() ] ) -> void().
render_faces( _IndexedFaces=[], _FaceCount, _Vertices, _Normals, _Colors ) ->
	ok;


% We currently supposed we have quads:
render_faces( _IndexedFaces=[ { V1Idx, V2Idx, V3Idx, V4Idx } | T ], FaceCount,
			  Vertices, Normals, Colors ) ->

	gl:normal3fv( lists:nth( FaceCount, Normals ) ),

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

	render_faces( T, FaceCount+1, Vertices, Normals, Colors ).



% @doc Returns the RGB or RGBA color buffer corresponding to the specified
% image.
%
% The returned buffer shall be "const", in the sense of being neither be
% deallocated or assigned to any image.
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
			throw( { opengl_error, GlError, Diagnosis } )

	end.


% @doc Returns a (textual) diagnosis regarding the specified OpenGL error.
-spec interpret_error( gl_error() ) -> ustring().
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

interpret_error( Other ) ->
	text_utils:format( "unknown OpenGL error (abnormal), of code ~B.",
					   [ Other ] ).
