% Copyright (C) 2003-2021 Olivier Boudeville
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


% @doc Testing the <b>OpenGL support</b>.
%
% See the gui_opengl.erl tested module.
%
-module(gui_opengl_test).


% Implementation notes:
%
% Inspired from:
% - wx:demo/0: lib/wx/examples/demo/ex_gl.erl
% - test suite: lib/wx/test/wx_opengl_SUITE.erl

% TO REMOVE:
%-include_lib("wx/include/wx.hrl").

% For GL/GLU defines:
-include("gui_opengl.hrl").
% For user code: -include_lib("myriad/include/gui_opengl.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").


% For re-use in other tests:
-export([ get_test_cube_info/0, get_test_cube_mesh/0,

		  get_test_cube_vertices/0, get_test_cube_faces/0,
		  get_test_cube_normals/0, get_test_cube_colors/0,

		  get_test_image_path/0 ]).


% The duration, in milliseconds, between two updates of the OpenGL rendering:
-define( interframe_duration, 20 ).


% Test-specific GUI state:
-record( my_test_state, {

	main_frame :: gui:frame(),

	canvas :: gui:opengl_canvas(),

	context :: gui:opengl_context(),

	image :: gui:image(),

	% The OpenGL information kept by this test once initialised:
	opengl_state :: maybe( my_opengl_test_state() )

 } ).

-type my_test_state() :: #my_test_state{}.


% OpenGL-specific GUI test state:
-record( my_opengl_test_state, {


} ).

-type my_opengl_test_state() :: #my_opengl_test_state{}.



% Shorthands:

-type file_path() :: file_utils:file_path().

-type vertex3() :: point3:vertex3().
-type unit_normal3() :: vector3:unit_normal3().

-type mesh() :: mesh:mesh().
-type face() :: mesh:face().

-type dimensions() :: gui:dimensions().
-type window() :: gui:window().
-type image() :: gui:image().

-type render_rgb_color() :: gui_color:render_rgb_color().



% @doc Returns the information needed in order to define a simple test cube.
-spec get_test_cube_info() -> { [ vertex3() ], [ face() ], [ unit_normal3() ],
								[ render_rgb_color() ] }.
get_test_cube_info() ->
	% No texture coordinates used:
	{ get_test_cube_vertices(), get_test_cube_faces(),
	  get_test_cube_normals(), get_test_cube_colors() }.



% @doc Returns a mesh coresponding to the test cube.
-spec get_test_cube_mesh() -> mesh().
get_test_cube_mesh() ->
	{ Vertices, Faces, Normals, Colors } = get_test_cube_info(),
	RenderingInfo = { color, per_vertex, Colors },
	mesh:create_mesh( Vertices, Faces, _NormalType=per_face, Normals,
					  RenderingInfo ).



% @doc Returns the (8) vertices of the test cube.
-spec get_test_cube_vertices() -> [ vertex3() ].
get_test_cube_vertices() ->
	[ _V1={ -0.5, -0.5, -0.5 },
	  _V2={  0.5, -0.5, -0.5 },
	  _V3={  0.5,  0.5, -0.5 },
	  _V4={ -0.5,  0.5, -0.5 },
	  _V5={ -0.5,  0.5,  0.5 },
	  _V6={  0.5,  0.5,  0.5 },
	  _V7={  0.5, -0.5,  0.5 },
	  _V8={ -0.5, -0.5,  0.5 } ].



% @doc Returns the (6) faces of the test cube.
-spec get_test_cube_faces() -> [ face() ].
get_test_cube_faces() ->
	[ _F1=[ 1, 2, 3, 4 ],
	  _F2=[ 8, 1, 4, 5 ],
	  _F3=[ 2, 7, 6, 3 ],
	  _F4=[ 7, 8, 5, 6 ],
	  _F5=[ 4, 3, 6, 5 ],
	  _F6=[ 1, 2, 7, 8 ] ].



% @doc Returns the (6) per-face unit normals of the test cube.
-spec get_test_cube_normals() -> [ unit_normal3() ].
get_test_cube_normals() ->
	[ _NF1=[ 0.0, 0.0,-1.0 ],
	  _NF2=[-1.0, 0.0, 0.0 ],
	  _NF3=[ 1.0, 0.0, 0.0 ],
	  _NF4=[ 0.0, 0.0, 1.0 ],
	  _NF5=[ 0.0, 1.0, 0.0 ],
	  _NF6=[ 0.0,-1.0, 0.0 ] ].


% @doc Returns the (6) per-face colors of the test cube.
-spec get_test_cube_colors( ) -> [ render_rgb_color() ].
get_test_cube_colors() ->
	[ _CF1={ 0.0, 0.0, 0.0 },
	  _CF2={ 1.0, 0.0, 0.0 },
	  _CF3={ 1.0, 1.0, 0.0 },
	  _CF4={ 0.0, 1.0, 0.0 },
	  _CF5={ 0.0, 1.0, 1.0 },
	  _CF6={ 1.0, 1.0, 1.0 },
	  _CF7={ 1.0, 0.0, 1.0 },
	  _CF8={ 0.0, 0.0, 1.0 } ].



% @doc Returns the path to a test image.
-spec get_test_image_path() -> file_path().
get_test_image_path() ->
	% Relative to this test directory:
	file_utils:join( [ "..", "..", "..", "doc",
					   "myriad-space-time-referential.png" ] ).



% @doc Runs the OpenGL test if possible.
-spec run_test_opengl() -> void().
run_test_opengl() ->

	test_facilities:display( "~nStarting the test of OpenGL support." ),

	case gui_opengl:get_glxinfo_strings() of

		undefined ->
			test_facilities:display( "No proper OpenGL support detected on host"
				" (no GLX visual reported), thus no test performed." );

		GlxInfoStr ->
			test_facilities:display( "Checking whether OpenGL hardware "
				"acceleration is available: ~ts; glxinfo report is: ~ts",
				[ gui_opengl:is_hardware_accelerated( GlxInfoStr ),
				  text_utils:strings_to_string( GlxInfoStr ) ] ),
			run_actual_test()

	end.


% Creates the initial test GUI.
init_test_gui() ->

	MainFrame = gui:create_frame( "MyriadGUI OpenGL Test" ),

	Panel = gui:create_panel( MainFrame ),

	% At least this number of bits per RGB component:
	MinSize = 8,

	GLAttributes = [ rgba, double_buffer, { min_red_size, MinSize },
					 { min_green_size, MinSize }, { min_blue_size, MinSize },
					 { depth_buffer_size, 24 } ],

	Canvas = gui_opengl:create_canvas( _Parent=Panel,
		_Opts=[ { style, full_repaint_on_resize },
				{ gl_attributes, GLAttributes } ] ),

	Context = gui_opengl:create_context( Canvas ),

	gui:subscribe_to_events( { [ onShown, onWindowClosed ], MainFrame } ),

	% (on Apple's Cocoa, subscribing to onRepaintNeeded might be required)
	gui:subscribe_to_events( { onResized, Canvas } ),

	StatusBar = gui:create_status_bar( MainFrame ),

	gui:push_status_text( "Testing OpenGL now.", StatusBar ),

	Image = gui_image:create_from_file( get_test_image_path() ),

	gui_image:scale( Image, _ScWidth=128, _ScHeight=128 ),

	% No OpenGL state yet:
	#my_test_state{ main_frame=MainFrame,
					canvas=Canvas,
					context=Context,
					image=Image }.



% @doc Runs the actual test.
-spec run_actual_test() -> void().
run_actual_test() ->

	gui:start(),

	%gui:set_debug_level( [ calls, life_cycle ] ),

	% Ignore first events to accelerate setup:
	InitialGUIState = gui:batch( fun() -> init_test_gui() end ),

	MainFrame = InitialGUIState#my_test_state.main_frame,

	gui:show( MainFrame ),

	% Uncomment to check that a no_gl_context error report is triggered indeed,
	% as expected:
	%
	%gl:viewport( 0, 0, 50, 50 ),

	gui_main_loop( InitialGUIState ),

	gui:stop().



% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages.
%
-spec gui_main_loop( my_test_state() ) -> void().
gui_main_loop( GUIState=#my_test_state{ main_frame=MainFrame } ) ->

	receive

		% Just to showcase that this event can be intercepted:
		{ onShown, [ MainFrame, _Content ] } ->
			trace_utils:debug( "Test main frame shown." ),

			% OpenGLcontext available by design, as a onResized event should
			% have already been received and processed:
			%
			test_facilities:display( "Reported OpenGL settings: "
				"vendor is '~ts', renderer is '~ts'; "
				"OpenGL version is '~ts', and the one of the "
				"shading language is '~ts'.",
				[ gui_opengl:get_vendor_name(), gui_opengl:get_renderer_name(),
				  gui_opengl:get_version(),
				  gui_opengl:get_shading_language_version() ] ),

			gui_main_loop( GUIState );


		{ onResized, [ Canvas, NewSize, _Context ] } ->
			trace_utils:debug_fmt( "Resized to ~w.", [ NewSize ] ),

			% The first resizing (an event received even before onShow) is a
			% relevant moment in order to setup OpenGL:

			NewGUIState = case GUIState#my_test_state.opengl_state of

				undefined ->
					gui_opengl:set_context( Canvas,
											GUIState#my_test_state.context ),

					InitOpenGLState = setup_opengl( Canvas,
						GUIState#my_test_state.image ),

					GUIState#my_test_state{ opengl_state=InitOpenGLState };

				_ ->
					reset_opengl_on_resize( NewSize ),
					GUIState
			end,

			gui_main_loop( NewGUIState );


		{ onWindowClosed, [ MainFrame, _Context ] } ->
			trace_utils:info( "Main frame closed, test success." ),
			gui:destruct_window( MainFrame );

		OtherEvent ->
			trace_utils:warning_fmt( "Test ignored following event:~n ~p",
									 [ OtherEvent ] ),

			gui_main_loop( GUIState )

	after ?interframe_duration ->

		update_rendering( GUIState#my_test_state.opengl_state )

	end.



% @doc Sets up OpenGL, once a proper context has been set.
-spec setup_opengl( window(), image() ) -> void().
setup_opengl( Window, _Image ) ->

	Size = { _Width, _Height } = gui:get_client_size( Window ),
	reset_opengl_on_resize( Size ),

	gl:enable( ?GL_DEPTH_TEST ),
	gl:depthFunc( ?GL_LESS ),

	% Solid white:
	gl:clearColor( 1.0, 1.0, 1.0, 1.0 ),

	%MatTexture = load_texture_by_image(Image),
	%ImgTexture = load_texture_by_image(
	%       wxImage:new("erlang.png")),
	%Font = wxFont:new(32, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
	%Brush = wxBrush:new({0,0,0}),
	%StrTexture = load_texture_by_string(Font, Brush, {40,40,40}, "Text from wxFont", true),
	%{_, {Hour,Min,Sec}} = calendar:local_time(),
	%%Clock = load_texture_by_string(Font, Brush, {40, 40, 40},
	%               [?PAD(Hour), $:, ?PAD(Min), $:, ?PAD(Sec)], false),
	%Sphere = glu:newQuadric(),
	%gl:enable(?GL_TEXTURE_2D),
ok.
	%#gl{win=Win,data={?FACES,?VS,?COLORS},deg=0.0,
	%mat=MatTexture, alpha=ImgTexture, text=StrTexture, font = Font,
	%brush = Brush, clock = Clock, sphere = Sphere}.



% @doc Resets the OpenGL state after a resize of its window.
-spec reset_opengl_on_resize( dimensions() ) -> void().
reset_opengl_on_resize( _NewSize={ Width, Height } ) ->

	gl:viewport( 0, 0, Width, Height ),

	gl:matrixMode( ?GL_PROJECTION ),

	gl:loadIdentity(),

	Left = -2.0,
	Bottom = -2.0 * Height / Width,
	Near = -20.00,
	gl:ortho( Left, _Right=-Left, Bottom, _Top=-Bottom, Near, _Far=-Near ),

	gl:matrixMode( ?GL_MODELVIEW ),
	gl:loadIdentity(),

	cond_utils:if_defined( myriad_check_opengl_support,
						   gui_opengl:check_error() ).


update_rendering( _ ) ->
	ok.


% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
			  "(not running the OpenGL test, being in batch mode)" );

		false ->
			run_test_opengl()

	end,

	test_facilities:stop().
