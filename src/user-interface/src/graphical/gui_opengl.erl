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


% For the numerous GL defines notably:
-include("gui_opengl.hrl").


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


-opaque gl_error() :: enum().
% An error code reported by OpenGL.


-export_type([ enum/0, glxinfo_report/0, canvas/0, canvas_option/0,
			   device_context_attribute/0, context/0, gl_error/0 ]).



-export([ get_vendor_name/0, get_renderer_name/0, get_version/0,
		  get_shading_language_version/0,

		  is_hardware_accelerated/0, is_hardware_accelerated/1,
		  get_glxinfo_strings/0,

		  create_canvas/1, create_canvas/2,
		  create_context/1, set_context/2, swap_buffers/1,

		  check_error/0, interpret_error/1 ]).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type bit_size() :: system_utils:bit_size().

-type window() :: gui:window().



% @doc Returns the name of the OpenGL vendor of the current driver, that is the
% company responsible for this OpenGL implementation.
%
% For example: "FOOBAR Corporation".
%
-spec get_vendor_name() -> ustring().
get_vendor_name() ->
	Res= gl:getString( ?GL_VENDOR ),
	cond_utils:if_defined( myriad_check_opengl_support, check_error() ),
	Res.



% @doc Returns the name of the OpenGL renderer of the current driver (typically
% specific to a particular configuration of a hardware platform).
%
% For example: "FOOBAR Frobinator GTX 1060 6GB/PCIe/SSE2".
%
-spec get_renderer_name() -> ustring().
get_renderer_name() ->
	Res = gl:getString( ?GL_RENDERER ),
	cond_utils:if_defined( myriad_check_opengl_support, check_error() ),
	Res.



% @doc Returns the version / release number of the currently used OpenGL
% implementation.
%
% Example: "4.6.0 FOOBAR 495.44".
%
-spec get_version() -> ustring().
get_version() ->
	Res = gl:getString( ?GL_VERSION ),
	cond_utils:if_defined( myriad_check_opengl_support, check_error() ),
	Res.


% @doc Returns the version /release number of the currently used OpenGL
% shading language.
%
% Example: "4.60 FOOBAR".
%
-spec get_shading_language_version() -> ustring().
get_shading_language_version() ->
	Res = gl:getString( ?GL_SHADING_LANGUAGE_VERSION ),
	cond_utils:if_defined( myriad_check_opengl_support, check_error() ),
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
	%cond_utils:if_defined( myriad_check_opengl_support, check_error() ),

	Res.



% @doc Returns the OpenGL context obtained from the specified canvas.
-spec create_context( canvas() ) -> context().
create_context( Canvas ) ->
	Res = wxGLContext:new( Canvas ),

	% Commented-out, as an OpenGL context may not already exist:
	%cond_utils:if_defined( myriad_check_opengl_support, check_error() ),

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
	cond_utils:if_defined( myriad_check_opengl_support, check_error() ).



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
	cond_utils:if_defined( myriad_check_opengl_support, check_error() ).



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
