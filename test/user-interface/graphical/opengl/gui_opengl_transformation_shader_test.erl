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
% Creation date: Sunday, April 9, 2023.


% @doc Minimal testing of <b>shader-based transformation rendering</b>: applies
% a transformation matrix created from the application and displays a textured
% square based on it that can be moved with the keyboard to test translations
% (and directions thereof) in the current referential.
%
% This test relies on shaders and thus on modern versions of OpenGL (e.g. 3.3),
% as opposed to the compatibility mode for OpenGL 1.x.
%
-module(gui_opengl_transformation_shader_test).


% Implementation notes:
%
% Inspired from https://learnopengl.com/Getting-started/Transformations.
%
% Here, as we used NDC (normalised coordinates), not need to render based on the
% dimensions of the canvas.
%
% Refer to https://myriad.esperide.org/#geometric-conventions to better
% understand the referential and transformations involved.


% For GL/GLU defines; the sole include that MyriadGUI user code shall reference:
-include_lib("myriad/include/myriad_gui.hrl").



% For run/0 export and al:
-include("test_facilities.hrl").



% Test-specific overall GUI state:
%
% (no OpenGL-specific state to store, like vertices, textures or alike)
%
-record( my_gui_state, {

	% The main frame of this test:
	parent :: frame(),

	% The OpenGL canvas on which rendering will be done:
	canvas :: gl_canvas(),

	% The OpenGL context being used:
	context :: gl_context(),

	% The image as loaded from file, to be transformed in a texture:
	image :: image(),

	% The 3D position of the center of the textured square:
	center_pos :: point3(),

	% The model-view matrix for the square of interest:
	model_view :: matrix4(),

	% In more complex cases, would store the loaded textures, etc.:
	opengl_state :: maybe( my_opengl_state() ) } ).

-type my_gui_state() :: #my_gui_state{}.
% Test-specific overall GUI state.



-record( my_opengl_state, {

	% The identifier of our GLSL program:
	program_id :: program_id(),

	% Needs an OpenGL context:
	texture :: maybe( texture() ),

	% The identifier of the uniform matrix:
	model_view_id :: uniform_id(),

	% For the square, which has indexed coordinates:

	square_vao_id :: vao_id(),

	square_vertex_count :: vertex_count(),


	% Stored only so that they can be deallocated once the test is over:

	% The VBO concentrating vertices and texture coordinates:
	square_merged_vbo_id :: vbo_id(),

	% Indices for the vertex:
	square_ebo_id :: ebo_id() } ).

-type my_opengl_state() :: #my_opengl_state{}.
% Test-specific overall OpenGL state.
%
% Storing VBOs and EBOs is probably only of use in order to deallocate them
% properly once not needed anymore.


% Key bindings (Z-up conventions):

% X (abscissa) is controlled by left-right arrows:
-define( increase_x_scan_code, ?MYR_SCANCODE_KP_6 ).
-define( decrease_x_scan_code, ?MYR_SCANCODE_KP_4 ).

% Y (depth)
-define( increase_y_scan_code, ?MYR_SCANCODE_KP_9 ).
-define( decrease_y_scan_code, ?MYR_SCANCODE_KP_3 ).

% Z (ordinate)
-define( increase_z_scan_code, ?MYR_SCANCODE_KP_8 ).
-define( decrease_z_scan_code, ?MYR_SCANCODE_KP_2 ).

-define( reset_scan_code, ?MYR_SCANCODE_KP_5 ).

-define( quit_scan_code, ?MYR_SCANCODE_ESCAPE ).


% An increment on a given dimension:
-define ( delta_coord, 0.1 ).



% Shorthands:

-type point3() :: point3:point3().

-type matrix4() :: matrix4:matrix4().

-type frame() :: gui:frame().

-type image() :: gui_image:image().

-type scancode() :: gui_keyboard:scancode().

-type gl_canvas() :: gui:opengl_canvas().
-type gl_context() :: gui:opengl_context().

-type texture() :: gui_texture:texture().

-type program_id() :: gui_shader:program_id().
-type vao_id() :: gui_shader:vao_id().
-type vbo_id() :: gui_shader:vbo_id().
-type ebo_id() :: gui_shader:ebo_id().
-type uniform_id() :: gui_shader:uniform_id().

-type vertex_count() :: mesh:vertex_count().




% The attribute in the vertex stream that will be passed to the our (vertex)
% shader for the vertices; attribute 0 was chosen, yet no particular reason for
% this index, it just must match the layout (cf. 'location = 0') in the shader.
%
-define( my_vertex_attribute_index, 0 ).

% The attribute in the vertex stream that will be passed to the our (vertex)
% shader for the texture coordinates.
%
-define( my_texture_coords_attribute_index, 1 ).


% @doc Prepares all information needed to render the square, and returns them.
%
% Here a single VBO is used, merging the vertices and the texture coordinates;
% additionally an EBO is used.
%
-spec prepare_square( texture() ) -> { vao_id(), vbo_id(), ebo_id() }.
prepare_square( Texture ) ->

	SquareVAOId = gui_shader:set_new_vao(),

	% Half edge length:
	H = 0.5,

	Z = 0.0,

	% Square defined as [vertex3()], directly in normalized device coordinates
	% here; CCW order (bottom left, bottom right, top right, top left)::
	%
	%         S3--S2
	%         |    |
	%         S0--S1
	%
	SquareVertices = [ _SV2={  H,  H, Z }, _SV1={  H, -H, Z },
					   _SV0={ -H, -H, Z }, _SV3={ -H,  H, Z } ],

	O = 1.0,

	OrigSquareTexCoords = [ _STC2={ O, O }, _STC1={ O, Z },
							_STC0={ Z, Z }, _STC3={ Z, O } ],

	ActualSquareTexCoords = gui_texture:recalibrate_coordinates_for(
		OrigSquareTexCoords, Texture ),

	SquareAttrSeries= [ SquareVertices, ActualSquareTexCoords ],

	% We start at vertex attribute index #0 in this VAO; as there are two
	% series, the vertex attribute indices will be 0 and 1:
	%
	SquareMergedVBOId = gui_shader:assign_new_vbo_from_attribute_series(
		SquareAttrSeries ),

	% We describe now our square as two triangles in CCW order; the first,
	% S0-S1-S3 on the bottom left, the second, S1-S2-S3 on the top right; we
	% have just a list of indices (not for example a list of triplets of
	% indices):
	%
	SquareIndices = [ 0, 1, 3,   % As the first  triangle is S0-S1-S3
					  1, 2, 3 ], % As the second triangle is S1-S2-S3

	SquareEBOId = gui_shader:assign_indices_to_new_ebo( SquareIndices ),


	% As the (single, here) VBO and the EBO were created whereas this VAO was
	% active, they are tracked by this VAO, which will rebind them automatically
	% the next time it will be itself bound:
	%
	gui_shader:unset_current_vao(),

	{ SquareVAOId, SquareMergedVBOId, SquareEBOId }.




% @doc Runs the OpenGL test if possible.
-spec run_opengl_test() -> void().
run_opengl_test() ->

	test_facilities:display(
		"~nStarting the test of transformation support with OpenGL shaders." ),

	case gui_opengl:get_glxinfo_strings() of

		undefined ->
			test_facilities:display( "No proper OpenGL support detected on host"
				" (no GLX visual reported), thus no test performed." );

		GlxInfoStr ->
			test_facilities:display( "Checking whether OpenGL hardware "
				"acceleration is available: ~ts.",
				[ gui_opengl:is_hardware_accelerated( GlxInfoStr ) ] ),
			run_actual_test()

	end.



% @doc Runs the actual test.
-spec run_actual_test() -> void().
run_actual_test() ->

	test_facilities:display( "This test will display a textured square "
		"that can be translated by hitting keys:~n"
		"  - for its position on the X axis: hit the 9 key to increase it, "
		"7 to decrease it, and 8 to reset it to zero~n"
		"  - for its position on the Y axis: hit the 6 key to increase it, "
		"4 to decrease it, and 5 to reset it to zero~n"
		"  - for its position on the Z axis: hit the 3 key to increase it, "
		"1 to decrease it, and 2 to reset it to zero~n" ),

	gui:start(),

	% Could be batched (see gui:batch/1) to be more effective:
	InitialGUIState = init_test_gui(),

	gui:show( InitialGUIState#my_gui_state.parent ),

	% OpenGL will be initialised only when the corresponding frame will be ready
	% (that is once first reported as resized):
	%
	gui_main_loop( InitialGUIState ),

	gui:stop().



% @doc Creates the initial test GUI: a main frame containing an OpenGL canvas to
% which an OpenGL context is associated.
%
% Once the rendering is done, the buffers are swapped, and the content is
% displayed.
%
-spec init_test_gui() -> my_gui_state().
init_test_gui() ->

	MainFrame = gui:create_frame(
		"MyriadGUI OpenGL Shader-based Transformation Test",
		_Size={ 1024, 768 } ),

	% Using mostly default GL attributes:
	GLCanvasAttrs =
		[ use_core_profile | gui_opengl:get_default_canvas_attributes() ],

	GLCanvas = gui_opengl:create_canvas( _Parent=MainFrame,
										 [ { gl_attributes, GLCanvasAttrs } ] ),


	% Created, yet not bound yet (must wait for the main frame to be shown):
	GLContext = gui_opengl:create_context( GLCanvas ),

	gui:subscribe_to_events( { [ onResized, onShown, onWindowClosed ],
							   MainFrame } ),

	% Needed, otherwise if that frame is moved out of the screen or if another
	% windows overlaps, the OpenGL canvas gets garbled and thus must be redrawn:
	%
	% (key events collected at the canvas-level, as frames do not handle them)
	%
	gui:subscribe_to_events( { [ onRepaintNeeded, onKeyPressed ], GLCanvas } ),

	% Would be too early for gui_texture:load_from_file (no GL context yet):
	TestImage = gui_image:load_from_file(
		gui_opengl_texture_test:get_test_texture_path() ),

	% No OpenGL state yet (GL context cannot be set as current yet), actual
	% OpenGL initialisation to happen when available, i.e. when the main frame
	% is shown:
	%
	#my_gui_state{ parent=MainFrame, canvas=GLCanvas, context=GLContext,
				   image=TestImage, center_pos=point3:null(),
				   model_view=matrix4:identity() }.



% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages.
%
-spec gui_main_loop( my_gui_state() ) -> void().
gui_main_loop( GUIState ) ->

	%trace_utils:debug( "Main loop." ),

	% Matching the least-often received messages last:
	receive


		{ onKeyPressed, [ GLCanvas, _GLCanvasId, Context ] } ->
			% Using here scancodes, not to depend on any keyboard layout or
			% modifier:
			%
			Scancode = gui_keyboard:event_context_to_scancode( Context ),

			%trace_utils:debug_fmt( "Scan code pressed: ~B on ~w.",
			%                       [ Scancode, GLCanvas ] ),

			case update_scene( Scancode, GUIState ) of

				{ NewGUIState, _DoQuit=true } ->
					terminate( NewGUIState ),
					trace_utils:info( "Requested to quit, test success." );

				{ NewGUIState, _DoQuit } ->
					% Supposing the OpenGL state is already available:
					render( GUIState#my_gui_state.opengl_state ),
					gui_opengl:swap_buffers( GLCanvas ),
					gui_main_loop( NewGUIState )

			end;

		{ onRepaintNeeded, [ GLCanvas, _GLCanvasId, _EventContext ] } ->

			%trace_utils:debug_fmt( "Repaint needed for OpenGL canvas ~w.",
			%                       [ GLCanvas ] ),

			RepaintedGUIState = case GUIState#my_gui_state.opengl_state of

				% Not ready yet:
				undefined ->
					trace_utils:debug(
						"To be repainted, yet no OpenGL state yet." ),
					GUIState;

				GLState ->
					gui:enable_repaint( GLCanvas ),
					render( GLState ),
					gui_opengl:swap_buffers( GLCanvas ),
					GUIState

			end,
			gui_main_loop( RepaintedGUIState );


		% For a window, the first resizing event happens immediately before its
		% onShown one:
		%
		{ onResized, [ _ParentFrame, _ParentFrameId, _NewParentSize,
					   _EventContext ] } ->

			%trace_utils:debug_fmt( "Resizing of the parent window "
			%   "(main frame) to ~w detected.", [ NewParentSize ] ),

			ResizedGUIState = case GUIState#my_gui_state.opengl_state of

				% Not ready yet:
				undefined ->
					trace_utils:debug( "Resized, yet no OpenGL state yet." ),
					GUIState;

				_ ->
					on_main_frame_resized( GUIState )

			end,

			gui_main_loop( ResizedGUIState );


		% Less frequent messages looked up last:

		% The most suitable first location to initialise OpenGL, as making a GL
		% context current requires a shown window:
		%
		{ onShown, [ ParentFrame, _ParentFrameId, _EventContext ] } ->

			trace_utils:debug_fmt( "Parent window (main frame) just shown "
				"(initial size of ~w).", [ gui:get_size( ParentFrame ) ] ),

			% Optional yet better:
			gui:unsubscribe_from_events( { onShown, ParentFrame } ),

			% Done once for all:
			InitGUIState = initialise_opengl( GUIState ),

			% A onRepaintNeeded event message expected just afterwards.

			gui_main_loop( InitGUIState );


		{ onWindowClosed, [ _ParentFrame, _ParentFrameId, _EventContext ] } ->
			terminate( GUIState ),
			trace_utils:info( "Main frame closed, test success." );


		OtherEvent ->
			trace_utils:warning_fmt( "Test ignored following event:~n ~p",
									 [ OtherEvent ] ),

			gui_main_loop( GUIState )

	% No 'after': no spontaneous action taken here, in the absence of events.

	end.



% @doc Sets up OpenGL, once for all (regardless of next resizings), once a
% proper OpenGL context is available.
%
-spec initialise_opengl( my_gui_state() ) -> my_gui_state().
initialise_opengl( GUIState=#my_gui_state{ canvas=GLCanvas,
										   context=GLContext,
										   image=Image,
										   model_view=ModelViewMat4,
										   % Check:
										   opengl_state=undefined } ) ->

	% Initial size of canvas is typically 20x20 pixels:
	trace_utils:debug_fmt( "Initialising OpenGL (whereas canvas is of initial "
						   "size ~w).", [ gui:get_size( GLCanvas ) ] ),

	% So done only once, with appropriate measures for a first setting:
	gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

	% First possible moment:
	test_facilities:display( "Description of the current OpenGL support: ~ts",
							 [ gui_opengl:get_support_description() ] ),

	% These test shaders are in 3.3 core (cf. their '#version 330 core'):
	MinOpenGLVersion = { 3, 3 },

	% Not found available at least in some configurations:
	%TargetProfile = core,

	TargetProfile = compatibility,
	%TargetProfile = non_existing_profile,


	gui_opengl:check_requirements( MinOpenGLVersion, TargetProfile ),


	% These settings will not change afterwards here (hence set once for all):

	% Clears in white (otherwise black background):
	gl:clearColor( _R=1.0, _G=1.0, _B=1.0, ?alpha_fully_opaque ),

	% Specifies the location of the vertex attributes, so that the vertex shader
	% will be able to match its input variables with the vertex attributes of
	% the application:
	%
	UserVertexAttrs = [
		{ "my_input_vertex",    ?my_vertex_attribute_index },
		{ "my_input_tex_coord", ?my_texture_coords_attribute_index } ],

	% Creates, compiles and links our GLSL program from the two specified
	% shaders, that are, in the same movement, automatically attached and
	% linked, then detached and deleted:
	%
	ProgramId = gui_shader:generate_program_from(
		"gui_opengl_transformation_shader.vertex.glsl",
		"gui_opengl_transformation_shader.fragment.glsl", UserVertexAttrs ),

	% Uniform locations can be fetched as soon as the program is linked:

	% Refer to the vertex shader:
	ModelViewMatUnifId = gui_shader:get_uniform_id(
		_MVUnifName="my_model_view_matrix", ProgramId ),

	% Refer to the fragment shader:
	SamplerUnifId = gui_shader:get_uniform_id(
		_SamplerUnifName="my_texture_sampler", ProgramId ),

	Texture = gui_texture:create_from_image( Image ),

	% To showcase that we can use other texture units (locations) than the
	% default ?GL_TEXTURE0 one:
	%
	gui_texture:set_current_texture_unit( ?GL_TEXTURE2 ),

	% Thus associated to the previous texture unit:
	gui_texture:set_as_current( Texture ),

	trace_utils:debug_fmt( "Prepared ~ts.",
						   [ gui_texture:to_string( Texture ) ] ),

	% Rely on our shaders; can be used from now:
	gui_shader:install_program( ProgramId ),

	% Uniforms can be set as soon as the GLSL program is installed:

	% Initial setting of the model-view matrix:
	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, ModelViewMat4 ),

	% Set the texture location of the sampler uniform:
	gui_shader:set_uniform_i( SamplerUnifId, _TextureUnit=2 ),


	% Prepare the textured square, whose vertices are specified through indices.
	%
	% We also have here to manage texture coordinates in addition to vertices,
	% so we merge them in a single VBO (that will be accessed thanks to an EBO):
	%
	{ SquareVAOId, SquareMergedVBOId, SquareEBOId } = prepare_square( Texture ),

	InitOpenGLState = #my_opengl_state{
		program_id=ProgramId,

		texture=Texture,
		model_view_id=ModelViewMatUnifId,

		square_vao_id=SquareVAOId,
		% Two basic triangles referenced in the associated VBO:
		square_vertex_count=6,
		square_merged_vbo_id=SquareMergedVBOId,
		square_ebo_id=SquareEBOId
						},

	%trace_utils:debug_fmt( "Managing a resize of the main frame to ~w.",
	%                       [ gui:get_size( MainFrame ) ] ),

	InitGUIState = GUIState#my_gui_state{
		% Start at the origin:
		opengl_state=InitOpenGLState },

	% As the initial onResized was triggered whereas no OpenGL state was
	% already available:
	%
	on_main_frame_resized( InitGUIState ).



% @doc Cleans up OpenGL.
-spec cleanup_opengl( my_gui_state() ) -> void().
cleanup_opengl( #my_gui_state{ opengl_state=undefined } ) ->
	ok;

cleanup_opengl( #my_gui_state{ opengl_state=#my_opengl_state{
		program_id=ProgramId,
		square_vao_id=SquareVAOId,
		square_merged_vbo_id=SquareMergedVBOId,
		square_ebo_id=SquareEBOId } } ) ->

	trace_utils:debug( "Cleaning up OpenGL." ),

	gui_shader:delete_vbo( SquareMergedVBOId ),

	gui_shader:delete_vao( SquareVAOId ),

	gui_shader:delete_ebo( SquareEBOId ),

	gui_shader:delete_program( ProgramId ).



% @doc Managing a resizing of the main frame.
%
% OpenGL context expected here to have already been set.
%
-spec on_main_frame_resized( my_gui_state() ) -> my_gui_state().
on_main_frame_resized( GUIState=#my_gui_state{ canvas=GLCanvas,
											   opengl_state=GLState } ) ->

	% Maximises the canvas in the main frame:
	{ CanvasWidth, CanvasHeight } = gui:maximise_in_parent( GLCanvas ),

	%trace_utils:debug_fmt( "New client canvas size: {~B,~B}.",
	%                       [ CanvasWidth, CanvasHeight ] ),

	% Lower-left corner and size of the viewport in the current window:
	gl:viewport( 0, 0, CanvasWidth, CanvasHeight ),

	% Apparently, at least on a test setting, a race condition (discovered
	% thanks to the commenting-out of a debug trace) seems to exist between the
	% moment when the canvas is resized and the one when a new OpenGL rendering
	% is triggered afterwards; the cause is probably that maximising involves an
	% (Erlang) asynchronous message to be sent from this user process and to be
	% received and applied by the process of the target window, whereas a GL
	% (NIF-based) operation is immediate; without a sufficient delay, the
	% rendering will thus take place according to the former (e.g. minimised)
	% canvas size, not according to the one that was expected to be already
	% resized.
	%
	gui:sync( GLCanvas ),

	% No specific projection settings enforced.

	% Any OpenGL reset to be done because of the resizing should take place
	% here.
	%
	% Using here normalised coordinates (in [0.0,1.0]), so no need to update the
	% orthographic projection.

	render( GLState ),

	% Includes a gl:flush/0:
	gui_opengl:swap_buffers( GLCanvas ),

	% Const here:
	GUIState.



% @doc Performs a (pure OpenGL) rendering.
-spec render( my_opengl_state() ) -> void().
render( #my_opengl_state{
			square_vao_id=SquareVAOId,
			square_vertex_count=SquareVCount,
			square_merged_vbo_id=_SquareMergedVBOId,
			square_ebo_id=_SquareEBOId } ) ->

	%trace_utils:debug_fmt( "Rendering now for size {~B,~B}.",
	%                       [ Width, Height ] ),

	gl:clear( ?GL_COLOR_BUFFER_BIT ),

	% We already use (enabled) our shader program.

	PrimType = ?GL_TRIANGLES,

	% From now, all operations must be performed at each rendering; rendering
	% the square:

	% Sets the vertex attribute; binds at well the square EBO, as it was still
	% tracked by the VAO when this VAO was unset:
	%
	gui_shader:set_current_vao_from_id( SquareVAOId ),

	% No offset:
	gui_shader:render_from_enabled_ebo( PrimType, SquareVCount ),

	gui_shader:unset_current_vao(),

	% Not swapping buffers here, as would involve GLCanvas, whereas this
	% function is meant to remain pure OpenGL.
	%
	% gl:flush/0 done when swapping buffers.

	ok.


% @doc Terminates the test.
-spec terminate( my_gui_state() ) -> void().
terminate( GUIState=#my_gui_state{ parent=MainFrame } ) ->

	cleanup_opengl( GUIState ),
	trace_utils:info( "Terminating test." ),

	% Very final check, while there is still an OpenGL context:
	gui_opengl:check_error(),

	% No more recursing:
	gui:destruct_frame( MainFrame ).


% @doc Updates the scene based on the specified user-entered scan code.
-spec update_scene( scancode(), my_gui_state() ) ->
						{ my_gui_state(), DoQuit :: boolean() }.
update_scene( _Scancode=?increase_x_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = ?delta_coord,
	% Translation on the X axis:
	VT = [ Inc, 0.0, 0.0 ],
	NewModelViewMat4 = matrix4:translate( ModelViewMat4, VT ),

	trace_utils:debug_fmt( "Increasing X of ~f, resulting in:~ts",
						   [ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };

update_scene( _Scancode=?decrease_x_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = ?delta_coord,
	% Translation on the X axis:
	VT = [ -Inc, 0.0, 0.0 ],
	NewModelViewMat4 = matrix4:translate( ModelViewMat4, VT ),

	trace_utils:debug_fmt( "Decreasing X of ~f, resulting in:~ts",
						   [ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?increase_y_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = ?delta_coord,
	% Translation on the Y axis:
	VT = [ 0.0, Inc, 0.0 ],
	NewModelViewMat4 = matrix4:translate( ModelViewMat4, VT ),

	trace_utils:debug_fmt( "Increasing Y of ~f, resulting in:~ts",
						   [ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };

update_scene( _Scancode=?decrease_y_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = ?delta_coord,
	% Translation on the Y axis:
	VT = [ 0.0, -Inc, 0.0 ],
	NewModelViewMat4 = matrix4:translate( ModelViewMat4, VT ),

	trace_utils:debug_fmt( "Decreasing Y of ~f, resulting in:~ts",
						   [ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?increase_z_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = ?delta_coord,
	% Translation on the Z axis:
	VT = [ 0.0, 0.0, Inc ],
	NewModelViewMat4 = matrix4:translate( ModelViewMat4, VT ),

	trace_utils:debug_fmt( "Increasing Z of ~f, resulting in:~ts",
						   [ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?decrease_z_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = ?delta_coord,
	% Translation on the Z axis:
	VT = [ 0.0, 0.0, -Inc ],
	NewModelViewMat4 = matrix4:translate( ModelViewMat4, VT ),

	trace_utils:debug_fmt( "Decreasing Z of ~f, resulting in:~ts",
						   [ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?reset_scan_code,
			  GUIState=#my_gui_state{
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->
	trace_utils:debug( "Resetting modelview matrix." ),
	NewModelViewMat4 = identity_4,

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?quit_scan_code, GUIState ) ->
	trace_utils:debug( "Requested to quit." ),
	{ GUIState, _DoQuit=true };

update_scene( _Scancode, GUIState ) ->
	%trace_utils:debug_fmt( "(scancode ~B ignored)", [ Scancode ] ),
	{ GUIState, _DoQuit=false }.



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running this OpenGL test, being in batch mode)" );

		false ->
			run_opengl_test()

	end,

	test_facilities:stop().