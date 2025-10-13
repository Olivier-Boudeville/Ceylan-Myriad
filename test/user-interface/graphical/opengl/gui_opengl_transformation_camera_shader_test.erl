% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Wednesday, October 9, 2024.

-module(gui_opengl_transformation_camera_shader_test).

-moduledoc """
Minimal testing of **shader-based transformation rendering with a camera**: like
gui_opengl_transformation_world_space_shader_test.erl, but managing a camera.
""".


% Implementation notes:
%
% Inspired from https://learnopengl.com/Getting-started/Camera
%
% We use here usual (non-NDC) world coordinates and a camera.
%
% The final MVP matrix (Model/View/Projection) is computed here, on the host (on
% the CPU) rather than by the vertex shader (hence on the GPU), as it should be
% considerably cheaper in most circumstances (this minimises data transfers on
% the CPU/GPU bus; moreover the MVP matrix is then computed once, not as many
% times as there are vertices - even if it would be then done more
% efficiently). At this regard, the shader presented in
% https://learnopengl.com/Getting-started/Coordinate-Systems is suboptimal, it
% should take a MVP matrix rather than computing it from M, V and P.
%
% This is why our gui_opengl_transformation_shader.vertex.glsl shader was
% replaced with gui_opengl_integrated_shader.vertex.glsl.



% For GL/GLU defines; the sole include that MyriadGUI user code shall reference:
-include_lib("myriad/include/myriad_gui.hrl").

-include_lib("myriad/include/matrix4.hrl").
-include_lib("myriad/include/projection.hrl").
-include_lib("myriad/include/camera.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").


-doc "A transformation mode.".
-type transformation_mode() :: 'translation' | 'rotation' | 'scaling'.


% Test-specific overall GUI state:
%
% (no OpenGL-specific state to store, like vertices, textures or alike)
%
-record( my_gui_state, {

    % The main frame of this test:
    main_frame :: frame(),

    % The OpenGL canvas on which rendering will be done:
    canvas :: gl_canvas(),

    % The aspect ratio of that canvas:
    aspect_ratio :: option( aspect_ratio() ),

    % The OpenGL context being used:
    context :: gl_context(),

    % The image as loaded from file, to be transformed in a texture:
    image :: image(),

    % Currently, we directly update (translate, rotate, etc.) the next
    % model-view matrix based on the requested changes; this is prone to the
    % accumulation of rounding errors, hence a better practice would be to
    % recompute the model-view matrix from the next higher-level parameters:

    % Model-level settings:

    % The 3D position of the center of the model (textured square) in the world
    % coordinate system:
    %
    %center_pos :: point3(),

    % The angle of the model (textured square) along the X axis of the world
    % coordinate system:
    %
    %x_angle :: radians(),

    % The angle of the model (textured square) along the Y axis of the world
    % coordinate system:
    %
    %y_angle :: radians(),

    % The angle of the model (textured square) along the Z axis of the world
    % coordinate system:
    %
    %z_angle :: radians(),


    % The model matrix for the square of interest:
    model_mat4 :: matrix4(),

    % Camera-level settings:
    camera :: camera(),

    % The view matrix corresponding to the camera:
    view_mat4 :: matrix4(),

    % The current projection settings that apply:
    projection_settings :: projection_settings(),

    % The corresponding projection matrix of interest:
    proj_mat4 :: matrix4(),

    % No need to store the resulting mvp_mat4 matrix.

    % The currently active transformation mode (translation, rotation,
    % or scaling):
    %
    transformation_mode :: transformation_mode(),

    % In more complex cases, would store the loaded textures, etc.:
    opengl_state :: option( my_opengl_state() ) } ).


-doc "Test-specific overall GUI state.".
-type my_gui_state() :: #my_gui_state{}.



-record( my_opengl_state, {

    % The identifier of our GLSL program:
    program_id :: program_id(),

    % Needs an OpenGL context:
    texture :: option( texture() ),

    % The identifier of the Model/View/Projection uniform matrix:
    mvp_mat4_id :: uniform_id(),

    % For the square, which has indexed coordinates:

    square_vao_id :: vao_id(),

    square_vertex_count :: vertex_count(),


    % Stored only so that they can be deallocated once the test is over:

    % The VBO concentrating vertices and texture coordinates:
    square_merged_vbo_id :: vbo_id(),

    % Indices of the vertices:
    square_ebo_id :: ebo_id() } ).


-doc """
Test-specific overall OpenGL state.

Designed so that the render functions rely only on this state.

Storing VBOs and EBOs is probably only of use in order to deallocate them
properly once not needed anymore.
""".
-type my_opengl_state() :: #my_opengl_state{}.



% For the shared navigation defines:
-include("gui_opengl_test_defines.hrl").


% Test-specific defines:

-if( ?has_keypad =:= true ).

% Re-centers all:
-define( square_reset_scan_code, ?MYR_SCANCODE_KP_5 ).

% Switches to the next transformation mode:
-define( square_mode_switch_scan_code, ?MYR_SCANCODE_KP_ENTER ).


-else. % Not using keypad here:


-define( square_reset_scan_code, ?MYR_SCANCODE_SPACE ).

-define( square_mode_switch_scan_code, ?MYR_SCANCODE_RETURN ).


-endif. % has_keypad



% Type shorthands:

-type ustring() :: text_utils:ustring().

-type matrix4() :: matrix4:matrix4().
-type projection_settings() :: projection:projection_settings().

-type frame() :: gui_frame:frame().
-type aspect_ratio() :: gui:aspect_ratio().

-type image() :: gui_image:image().

-type scancode() :: gui_keyboard:scancode().

-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().

-type texture() :: gui_texture:texture().

-type program_id() :: gui_shader:program_id().
-type vao_id() :: gui_shader:vao_id().
-type vbo_id() :: gui_shader:vbo_id().
-type ebo_id() :: gui_shader:ebo_id().
-type uniform_id() :: gui_shader:uniform_id().

-type vertex_count() :: mesh:vertex_count().

-type camera() :: camera:camera().



% The attribute in the vertex stream that will be passed to our (vertex) shader
% for the vertices; attribute 0 was chosen, yet no particular reason for this
% index, it just must match the layout (cf. 'location = 0') in the shader.
%
-define( my_vertex_attribute_index, 0 ).


% The attribute in the vertex stream that will be passed to our (vertex) shader
% for the texture coordinates.
%
-define( my_texture_coords_attribute_index, 1 ).



-doc """
Prepares all information needed to render the square, and returns them.

Here a single VBO is used, merging the vertices and the texture coordinates;
additionally an EBO is used.
""".
-spec prepare_square( texture() ) -> { vao_id(), vbo_id(), ebo_id() }.
prepare_square( Texture ) ->

    % Creates the VAO context we need for the upcoming VBO (vertices and texture
    % coordinates) and EBO (for indices in the VBO):
    %
    SquareVAOId = gui_shader:set_new_vao(),

    % We consider here a square whose edge length is (in units):
    E = 5,

    % Half edge length:
    H = E / 2,

    % Depth (i.e. Z coordinate) of the square.
    %
    % Note that modifying depth does not change at all the rendering if in
    % orthographic projection mode - provided that the points remain in the unit
    % orthographic cube (so for example the square will disappear in
    % orthographic mode if D < -1.0)
    %
    % So that is well visible in the (initial) perspective mode:
    D = -15.0,


    % Square defined as [vertex3()], in world coordinates here, in the XY plane
    % (Z=0); CCW order (bottom left, bottom right, top right, top left):
    %
    %         S3--S2
    %         |    |
    %         S0--S1
    %
    SquareVertices = [ _SV2={  H,  H, D }, _SV1={  H, -H, D },
                       _SV0={ -H, -H, D }, _SV3={ -H,  H, D } ],

    % Zero:
    Z = 0.0,

    % One:
    O = 1.0,

    OrigSquareTexCoords = [ _STC2={ O, O }, _STC1={ O, Z },
                            _STC0={ Z, Z }, _STC3={ Z, O } ],

    % To have correct texture coordinates in spite of padding:
    ActualSquareTexCoords = gui_texture:recalibrate_coordinates_for(
        OrigSquareTexCoords, Texture ),

    SquareAttrSeries= [ SquareVertices, ActualSquareTexCoords ],

    % Creates a VBO from these two series.
    %
    % We start at vertex attribute index #0 in this VAO; as there are two
    % series, the vertex attribute indices will be 0 and 1:
    %
    SquareMergedVBOId = gui_shader:assign_new_vbo_from_attribute_series(
        SquareAttrSeries ),

    % We describe now our square as two triangles in CCW order; the first,
    % S0-S1-S3 on the bottom left, the second, S1-S2-S3 on the top right; we
    % have just a plain list of indices (not for example a list of triplets of
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



-spec get_help_text() -> ustring().
get_help_text() ->

    % Only true if keypad is enabled

    % Using a Myriad 3D coordinate system here with Z-up, where the camera is
    % fixed at the origin, pointing to the -Z axis, with its up direction being
    % the +Y axis; so:
    % - X increases from, onscreen, left to right
    % - Y increases from bottom of screen to top
    % - Z increases as getting from farther to nearer the observer (when Z<0)


    text_utils:format( "This test displays a square textured with a Myriad image, whose center is at the origin, which is belonging to the Z=0 plane (using Z-up conventions), and that can be moved by hitting keys on the numerical keypad (while the rendering window has the focus):~n"
        "  - to translate it of ~f units along (if in translation mode):~n"
        "    * the X (abscissa) axis: hit '4' to move it, on the left, '6' on the right (otherwise, if not compiled for keypad support, respectively the left and right arrow keys)~n"
        "    * the Y (ordinate) axis: hit '2' to move it down, '8' up (otherwise the down and up arrow keys)~n"
        "    * the Z (depth/altitude) axis: hit '3' to move it further/downward, '9' nearer/upward (otherwise the page-down and page-up arrow keys)~n"
        "  - to rotate of ~f degrees around (if in rotation mode):~n"
        "    * the X axis: hit '4' to turn it clockwise (CW), '6' counter-clockwise (CCW)~n"
        "    * the Y axis: hit '2' to turn it CW, '8' CCW~n"
        "    * the Z axis: hit '3' to turn it CW, '9' CCW~n"
        "  - to scale it of a ~f factor along (if in scaling mode):~n"
        "    * the X axis: hit '4' to scale it down, '6' up~n"
        "    * the Y axis: hit '2' to scale it down, '8' up~n"
        "    * the Z axis: hit '3' to scale it down, '9' up~n~n"
        " Hit '5' (otherwise: the spacebar) to reset its position and direction, 'Enter' on the keypad (otherwise the main 'Return' key)"
        "to switch to the next transformation mode (cycling between translation, rotation, scaling), 'p' to toggle the projection mode (cycling between perspective and orthographic), 'h' to display this help and 'Escape' to quit.~n~n"
        "Hints:~n"
        " - with the (default) perspective projection, the square will appear iff its Z is below -0.1 (as ZNear=0.1), and will then progressively shrink when progressing along the -Z axis; as a result, from the default position, to make the square appear, first make it go further/downward ~n"
        " - with the orthographic projection mode, the square will remain the same for any Z in [-1.0, 1.0] (no perspective division) and, out of this range (past either the near or far clipping plane), it will fully disappear~n",
        [ ?delta_coord, ?delta_angle, ?delta_scale ] ).



-doc "Runs the actual test.".
-spec run_actual_test() -> void().
run_actual_test() ->

    test_facilities:display( get_help_text() ),

    gui:start(),

    % Could be batched (see gui:batch/1) to be more effective:
    InitialGUIState = init_test_gui(),

    gui_frame:show( InitialGUIState#my_gui_state.main_frame ),

    % OpenGL will be initialised only when the corresponding frame will be ready
    % (that is once first reported as resized):
    %
    gui_main_loop( InitialGUIState ),

    gui:stop().



-doc """
Creates the initial test GUI: a main frame containing an OpenGL canvas to which
an OpenGL context is associated.

Once the rendering is done, the buffers are swapped, and the content is
displayed.
""".
-spec init_test_gui() -> my_gui_state().
init_test_gui() ->

    MainFrame = gui_frame:create(
        "MyriadGUI OpenGL Shader-based Camera Transformation Test",

        % Preferring a square frame/viewport, otherwise due to aspect ratio the
        % square will be a rectangle:
        _Size={ 800, 800 } ),

    % Better:
    gui_frame:center_on_screen( MainFrame ),

    % Using mostly default GL attributes:
    GLCanvasAttrs =
        [ use_core_profile | gui_opengl:get_default_canvas_attributes() ],

    GLCanvas = gui_opengl:create_canvas(
        _CanvasOpts=[ { gl_attributes, GLCanvasAttrs } ], _Parent=MainFrame ),

    % Created, yet not bound yet (must wait for the main frame to be shown):
    GLContext = gui_opengl:create_context( GLCanvas ),

    gui:subscribe_to_events( { [ onResized, onShown, onWindowClosed ],
                               MainFrame } ),

    % onRepaintNeeded needed, otherwise if that frame is moved out of the screen
    % or if another window overlaps, the OpenGL canvas gets garbled - and thus
    % must be redrawn:
    %
    % (key events collected at the canvas-level, as frames do not handle them)
    %
    gui:subscribe_to_events( { [ onRepaintNeeded, onKeyPressed ], GLCanvas } ),

    % Would be too early for gui_texture:load_from_file (no GL context yet):
    TestImage = gui_image:load_from_file(
        gui_opengl_texture_test:get_test_texture_path() ),

    Camera = camera:create( _Position={ 0.0, 0.0, 0.0 }, % Origin-centered
                            _TargetPoint={ 0.0, 0.0, -1.0 }, % Still towards -Z
                            _UpDir=[ 0.0, 1.0, 0.0 ] ),

    trace_utils:debug_fmt( "Initial ~ts",
                           [ camera:to_string( Camera, _Verbose=true ) ] ),

    ViewMat4 = camera:get_view_matrix( Camera ),

    ProjSettings = projection:get_base_perspective_settings(
        _InitialAspectRatio=1.0 ),

    % No OpenGL state yet (GL context cannot be set as current yet), actual
    % OpenGL initialisation to happen when available, i.e. when the main frame
    % is shown:
    %
    #my_gui_state{ main_frame=MainFrame,
                   canvas=GLCanvas,
                   context=GLContext,
                   image=TestImage,
                   %center_pos=point3:null(),
                   %x_angle=Zero,
                   %y_angle=Zero,
                   %z_angle=Zero,
                   model_mat4=identity_4,
                   camera=Camera,
                   view_mat4=ViewMat4,
                   projection_settings=ProjSettings,
                   proj_mat4=projection:projection( ProjSettings ),
                   transformation_mode=translation }.



-doc """
The main loop of this test, driven by the receiving of MyriadGUI messages.
""".
-spec gui_main_loop( my_gui_state() ) -> void().
gui_main_loop( GUIState ) ->

    %trace_utils:debug( "Main loop." ),

    % Matching the least-often received messages last:
    receive


        { onKeyPressed, [ GLCanvas, _GLCanvasId, EventContext ] } ->
            % Using here scancodes, not to depend on any keyboard layout or
            % modifier:
            %
            Scancode = gui_keyboard:event_context_to_scancode( EventContext ),

            %trace_utils:debug_fmt( "Scan code pressed: ~B on ~w.",
            %                       [ Scancode, GLCanvas ] ),

            case update_scene_on_key_pressed( Scancode, GUIState ) of

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
                    gui_widget:enable_repaint( GLCanvas ),
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

        % This is the most suitable first location to initialise OpenGL, as
        % making a GL context current requires a shown window:
        %
        { onShown, [ ParentFrame, _ParentFrameId, _EventContext ] } ->

            trace_utils:debug_fmt( "Parent window (main frame) just shown "
                "(initial size of ~w).",
                [ gui_widget:get_size( ParentFrame ) ] ),

            % Optional yet better:
            gui:unsubscribe_from_events( { onShown, ParentFrame } ),

            % Done once for all:
            InitGUIState = initialise_opengl( GUIState ),

            % A onRepaintNeeded event message is expected just afterwards.

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



-doc """
Sets up OpenGL, once for all (regardless of next resizings), once a proper
OpenGL context is available.
""".
-spec initialise_opengl( my_gui_state() ) -> my_gui_state().
initialise_opengl( GUIState=#my_gui_state{ canvas=GLCanvas,
                                           context=GLContext,
                                           image=Image,
                                           model_mat4=ModelMat4,
                                           view_mat4=ViewMat4,
                                           proj_mat4=ProjMat4,
                                           % Check:
                                           opengl_state=undefined } ) ->

    % Initial size of canvas is typically 20x20 pixels:
    trace_utils:debug_fmt( "Initialising OpenGL (whereas canvas is of initial "
                           "size ~w).", [ gui_widget:get_size( GLCanvas ) ] ),

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
        % See implementation notes regarding the use of such as shader:
        "gui_opengl_mvp_shader.vertex.glsl",
        "gui_opengl_transformation_shader.fragment.glsl", UserVertexAttrs,
        _ExtraGLSLSearchPaths=[ "." ] ),

    % Uniform locations can be fetched as soon as the program is linked:

    % For Model/View/Projection (no more "my_model_mat4_matrix" and
    % "my_model_mat4_matrix"):
    %
    MVPMatUnifId = gui_shader:get_uniform_id(
        _MVPUnifName="my_mvp_matrix", ProgramId ),

    % Refer to the fragment shader:
    SamplerUnifId = gui_shader:get_uniform_id(
        _SamplerUnifName="my_texture_sampler", ProgramId ),

    Texture = gui_texture:create_from_image( Image ),

    % To showcase that we can use other texture units (locations) than the
    % default 0 (translating to ?GL_TEXTURE0) one; designating the third unit
    % here:
    %
    gui_texture:set_current_texture_unit( 2 ),

    % Thus associated to the previous texture unit:
    gui_texture:set_as_current( Texture ),

    trace_utils:debug_fmt( "Prepared ~ts.",
                           [ gui_texture:to_string( Texture ) ] ),

    % Rely on our shaders; can be used from now:
    gui_shader:install_program( ProgramId ),

    % Uniforms can be set as soon as the GLSL program is installed:

    % Initial setting of the Model/View/Projection matrix:
    update_mvp_matrix( ModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

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
        mvp_mat4_id=MVPMatUnifId,

        square_vao_id=SquareVAOId,

        % Two basic triangles referenced in the associated VBO:
        square_vertex_count=6,

        square_merged_vbo_id=SquareMergedVBOId,
        square_ebo_id=SquareEBOId },

    % Note that the default projection is orthographic; as a result, moving the
    % square along the Z (depth) will not change anything (until going out of
    % the NDC [-1.0, 1.0] range and then having the square disappear).

    trace_utils:debug( "Starting with an orthographic projection "
                       "with Z-up conventions, and in translation mode." ),

    InitGUIState = GUIState#my_gui_state{
        % Start at the origin:
        opengl_state=InitOpenGLState },

    % As the initial onResized was triggered whereas no OpenGL state was
    % already available:
    %
    on_main_frame_resized( InitGUIState ).



-doc "Cleans up OpenGL.".
-spec cleanup_opengl( my_gui_state() ) -> void().
cleanup_opengl( #my_gui_state{ opengl_state=undefined } ) ->
    ok;

cleanup_opengl( #my_gui_state{ opengl_state=#my_opengl_state{
        program_id=ProgramId,
        square_vao_id=SquareVAOId,
        square_merged_vbo_id=SquareMergedVBOId,
        square_ebo_id=SquareEBOId } } ) ->

    trace_utils:debug( "Cleaning up OpenGL." ),

    % Deleting the VAO does not delete the VBO or the EBO (that are just
    % referenced); deleting first the VAO is preferred:

    gui_shader:delete_vao( SquareVAOId ),

    gui_shader:delete_vbo( SquareMergedVBOId ),

    gui_shader:delete_ebo( SquareEBOId ),

    gui_shader:delete_program( ProgramId ).



-doc """
Managing a resizing of the main frame.

OpenGL context expected here to have already been set.
""".
-spec on_main_frame_resized( my_gui_state() ) -> my_gui_state().
on_main_frame_resized( GUIState=#my_gui_state{ canvas=GLCanvas,
                                               opengl_state=GLState } ) ->

    % Maximises the canvas in the main frame:
    { CanvasWidth, CanvasHeight } = gui_widget:maximise_in_parent( GLCanvas ),

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
    % canvas size - not according to the one that was expected to be already
    % resized.
    %
    gui_widget:sync( GLCanvas ),

    % No specific projection settings enforced.

    % Any OpenGL reset to be done because of the resizing should take place
    % here.
    %
    % Using here normalised coordinates (in [0.0,1.0]), so no need to update the
    % projection.

    render( GLState ),

    % Includes a gl:flush/0:
    gui_opengl:swap_buffers( GLCanvas ),

    % No null canvas height expected:
    GUIState#my_gui_state{ aspect_ratio=CanvasWidth/CanvasHeight }.



-doc "Performs a (pure OpenGL; no gui_* involved) rendering.".
-spec render( my_opengl_state() ) -> void().
render( #my_opengl_state{
            square_vao_id=SquareVAOId,
            square_vertex_count=SquareVCount

            % Both bound thanks to the VAO:
            %square_merged_vbo_id=SquareMergedVBOId,
            %square_ebo_id=SquareEBOId

          } ) ->

    %trace_utils:debug_fmt( "Rendering now for size {~B,~B}.",
    %                       [ Width, Height ] ),

    gl:clear( ?GL_COLOR_BUFFER_BIT ),

    % We already use (enabled) our shader program.

    PrimType = ?GL_TRIANGLES,

    % From now, all operations must be performed at each rendering; displaying
    % the square:

    % Sets the vertex attribute; this binds as well the square EBO (and the
    % VBO), as they were still tracked by the VAO when this VAO was unset:
    %
    gui_shader:set_current_vao_from_id( SquareVAOId ),

    % No offset in the start index needed:
    gui_shader:render_from_enabled_ebo( PrimType, SquareVCount ),

    gui_shader:unset_current_vao(),

    % Not swapping buffers here, as would involve GLCanvas, whereas this
    % function is meant to remain pure OpenGL.
    %
    % gl:flush/0 done when swapping buffers.

    ok.



-doc "Terminates the test.".
-spec terminate( my_gui_state() ) -> void().
terminate( GUIState=#my_gui_state{ main_frame=MainFrame } ) ->

    cleanup_opengl( GUIState ),
    trace_utils:info( "Terminating test." ),

    % Very final check, while there is still an OpenGL context:
    gui_opengl:check_error(),

    % No more recursing:
    gui_frame:destruct( MainFrame ).



-doc """
Updates the scene, based on the specified user-entered (keyboard) scan code.
""".
% First section: regarding the movement of the square.
%
% First managing translations:
-spec update_scene_on_key_pressed( scancode(), my_gui_state() ) ->
                        { my_gui_state(), DoQuit :: boolean() }.
update_scene_on_key_pressed( _Scancode=?square_increase_x_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=translation,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = ?delta_coord,

    % Translation on the X axis:
    VT = [ Inc, 0.0, 0.0 ],

    NewModelMat4 = matrix4:translate_homogeneous_right( ModelMat4, VT ),

    trace_utils:debug_fmt( "Translating square X of ~f, "
        "resulting in: Mm = ~ts~ts",
        [ Inc, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_decrease_x_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=translation,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = ?delta_coord,

    % Translation on the X axis:
    VT = [ -Inc, 0.0, 0.0 ],

    NewModelMat4 = matrix4:translate_homogeneous_right( ModelMat4, VT ),

    trace_utils:debug_fmt( "Translating square X of -~f, "
        "resulting in: Mm = ~ts~ts",
        [ Inc, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_increase_y_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=translation,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = ?delta_coord,

    % Translation on the Y axis:
    VT = [ 0.0, Inc, 0.0 ],
    NewModelMat4 = matrix4:translate_homogeneous_right( ModelMat4, VT ),

    trace_utils:debug_fmt( "Translating square Y of ~f, "
        "resulting in: Mm= ~ts~ts",
        [ Inc, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_decrease_y_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=translation,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = ?delta_coord,

    % Translation on the Y axis:
    VT = [ 0.0, -Inc, 0.0 ],
    NewModelMat4 = matrix4:translate_homogeneous_right( ModelMat4, VT ),

    trace_utils:debug_fmt( "Translating square Y of -~f, "
        "resulting in: Mm = ~ts~ts",
        [ Inc, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };



% Note that moving along the Z axis whereas the projection is orthographic will
% show no difference:
%
update_scene_on_key_pressed( _Scancode=?square_increase_z_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=translation,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = ?delta_coord,

    % Translation on the Z axis:
    VT = [ 0.0, 0.0, Inc ],
    NewModelMat4 = matrix4:translate_homogeneous_right( ModelMat4, VT ),

    trace_utils:debug_fmt( "Translating square Z of ~f, "
        "resulting in: Mm = ~ts~ts",
        [ Inc, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_decrease_z_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=translation,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = ?delta_coord,

    % Translation on the Z axis:
    VT = [ 0.0, 0.0, -Inc ],
    NewModelMat4 = matrix4:translate_homogeneous_right( ModelMat4, VT ),

    trace_utils:debug_fmt( "Translating square Z of -~f, "
        "resulting in: Mm = ~ts~ts",
        [ Inc, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };



% Secondly managing rotations:

update_scene_on_key_pressed( _Scancode=?square_increase_x_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=rotation,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    % Rotation around the X axis:
    RotAxis = vector3:x_axis(),

    Angle = math_utils:degrees_to_radians( ?delta_angle ),

    NewModelMat4 =
        matrix4:rotate_homogeneous_right( ModelMat4, RotAxis, Angle ),

    trace_utils:debug_fmt( "Rotating square around the X axis "
        "of an angle of ~f radians, resulting in: Mm = ~ts~ts",
        [ Angle, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_decrease_x_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=rotation,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    % Rotation around the X axis:
    RotAxis = vector3:x_axis(),

    Angle = - math_utils:degrees_to_radians( ?delta_angle ),

    NewModelMat4 =
        matrix4:rotate_homogeneous_right( ModelMat4, RotAxis, Angle ),

    trace_utils:debug_fmt( "Rotating square around the X axis "
        "of an angle of ~f radians, resulting in: Mm = ~ts~ts",
        [ Angle, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_increase_y_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=rotation,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    % Rotation around the Y axis:
    RotAxis = vector3:y_axis(),

    Angle = math_utils:degrees_to_radians( ?delta_angle ),

    NewModelMat4 =
        matrix4:rotate_homogeneous_right( ModelMat4, RotAxis, Angle ),

    trace_utils:debug_fmt( "Rotating square around the Y axis "
        "of an angle of ~f radians, resulting in: Mm = ~ts~ts",
        [ Angle, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_decrease_y_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=rotation,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    % Rotation around the Y axis:
    RotAxis = vector3:y_axis(),

    Angle = - math_utils:degrees_to_radians( ?delta_angle ),

    NewModelMat4 =
        matrix4:rotate_homogeneous_right( ModelMat4, RotAxis, Angle ),

    trace_utils:debug_fmt( "Rotating square around the Y axis "
        "of an angle of ~f radians, resulting in: Mm = ~ts~ts",
        [ Angle, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_increase_z_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=rotation,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    % Rotation around the Z axis:
    RotAxis = vector3:z_axis(),

    Angle = math_utils:degrees_to_radians( ?delta_angle ),

    NewModelMat4 =
        matrix4:rotate_homogeneous_right( ModelMat4, RotAxis, Angle ),

    trace_utils:debug_fmt( "Rotating square around the Z axis "
        "of an angle of ~f radians, resulting in: Mm = ~ts~ts",
        [ Angle, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_decrease_z_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=rotation,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    % Rotation around the Z axis:
    RotAxis = vector3:z_axis(),

    Angle = - math_utils:degrees_to_radians( ?delta_angle ),

    NewModelMat4 =
        matrix4:rotate_homogeneous_right( ModelMat4, RotAxis, Angle ),

    trace_utils:debug_fmt( "Rotating square around the Z axis "
        "of an angle of ~f radians, resulting in: Mm = ~ts~ts",
        [ Angle, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


% Thirdly managing scalings:
update_scene_on_key_pressed( _Scancode=?square_increase_x_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=scaling,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = 1.0 + ?delta_coord,

    NewModelMat4 = matrix4:scale_homogeneous_x( ModelMat4, Inc ),

    trace_utils:debug_fmt( "Scaling square on the X axis of a factor ~f, "
        "resulting in: Mm = ~ts~ts",
        [ Inc, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_decrease_x_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=scaling,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = 1.0 - ?delta_coord,

    NewModelMat4 = matrix4:scale_homogeneous_x( ModelMat4, Inc ),

    trace_utils:debug_fmt( "Scaling square on the X axis of a factor ~f, "
        "resulting in: Mm = ~ts~ts",
        [ Inc, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_increase_y_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=scaling,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = 1.0 + ?delta_coord,

    NewModelMat4 = matrix4:scale_homogeneous_y( ModelMat4, Inc ),

    trace_utils:debug_fmt( "Scaling square on the Y axis of a factor ~f, "
        "resulting in: Mm = ~ts~ts",
        [ Inc, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_decrease_y_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=scaling,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = 1.0 - ?delta_coord,

    NewModelMat4 = matrix4:scale_homogeneous_y( ModelMat4, Inc ),

    trace_utils:debug_fmt( "Scaling square on the Y axis of a factor ~f, "
        "resulting in: Mm = ~ts~ts",
        [ Inc, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_increase_z_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=scaling,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = 1.0 + ?delta_coord,

    NewModelMat4 = matrix4:scale_homogeneous_z( ModelMat4, Inc ),

    trace_utils:debug_fmt( "Scaling square on the Z axis of a factor ~f, "
        "resulting in: Mm = ~ts~ts",
        [ Inc, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_decrease_z_scan_code,
              GUIState=#my_gui_state{
                model_mat4=ModelMat4,
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                transformation_mode=scaling,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = 1.0 - ?delta_coord,

    NewModelMat4 = matrix4:scale_homogeneous_z( ModelMat4, Inc ),

    trace_utils:debug_fmt( "Scaling square on the Z axis of a factor ~f, "
        "resulting in: Mm = ~ts~ts",
        [ Inc, matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };



update_scene_on_key_pressed( _Scancode=?square_reset_scan_code,
              GUIState=#my_gui_state{
                view_mat4=ViewMat4,
                proj_mat4=ProjMat4,
                opengl_state=#my_opengl_state{
                    mvp_mat4_id=MVPMatUnifId } } ) ->

    NewModelMat4 = identity_4,

    trace_utils:debug_fmt(
        "Resetting the model matrix, resulting in: Mm = ~ts~ts",
        [ matrix4:to_string( NewModelMat4 ),
          describe_square_origin( NewModelMat4 ) ] ),

    update_mvp_matrix( NewModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ model_mat4=NewModelMat4 }, _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?square_mode_switch_scan_code,
              GUIState=#my_gui_state{ transformation_mode=TransfoMode } ) ->

    NewTransfoMode = case TransfoMode of

        translation ->
            rotation;

        rotation ->
            scaling;

        scaling ->
            translation

    end,

    trace_utils:debug_fmt( "Switching transformation mode from ~ts to ~ts.",
                           [ TransfoMode, NewTransfoMode ] ),

    { GUIState#my_gui_state{ transformation_mode=NewTransfoMode },
      _DoQuit=false };



% Second section: regarding the movement of the camera.

update_scene_on_key_pressed( _Scancode=?camera_increase_x_scan_code,
              GUIState=#my_gui_state{ model_mat4=ModelMat4,
                                      camera=Camera,
                                      proj_mat4=ProjMat4,
                                      opengl_state=#my_opengl_state{
                                        mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = ?delta_coord,

    % Translation on the X axis:
    VT = [ Inc, 0.0, 0.0 ],

    NewPos = point3:translate( Camera#camera.position, VT ),

    NewCamera = camera:set_position( NewPos, Camera ),

    NewViewMat4 = camera:get_view_matrix( NewCamera ),

    trace_utils:debug_fmt( "Translating camera X of ~f, "
        "resulting in: Mv = ~ts~ts",
        [ Inc, matrix4:to_string( NewViewMat4 ),
          describe_camera_origin( NewViewMat4 ) ] ),

    % Allows to check positioning:
    %trace_utils:debug_fmt( "New camera position: ~p", [
    %   matrix4:get_translation(
    %       camera:get_view_matrix_inverse( NewCamera ) ) ] ),

    update_mvp_matrix( ModelMat4, NewViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ camera=NewCamera, view_mat4=NewViewMat4 },
      _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?camera_decrease_x_scan_code,
              GUIState=#my_gui_state{ model_mat4=ModelMat4,
                                      camera=Camera,
                                      proj_mat4=ProjMat4,
                                      opengl_state=#my_opengl_state{
                                        mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = ?delta_coord,

    % Translation on the X axis:
    VT = [ -Inc, 0.0, 0.0 ],

    NewPos = point3:translate( Camera#camera.position, VT ),
    NewCamera = camera:set_position( NewPos, Camera ),

    NewViewMat4 = camera:get_view_matrix( NewCamera ),

    trace_utils:debug_fmt( "Translating camera X of -~f, "
        "resulting in: Mv = ~ts~ts",
        [ Inc, matrix4:to_string( NewViewMat4 ),
          describe_camera_origin( NewViewMat4 ) ] ),

    % Allows to check positioning:
    %trace_utils:debug_fmt( "New camera position: ~p", [
    %   matrix4:get_translation(
    %       camera:get_view_matrix_inverse( NewCamera ) ) ] ),

    update_mvp_matrix( ModelMat4, NewViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ camera=NewCamera, view_mat4=NewViewMat4 },
      _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?camera_increase_y_scan_code,
              GUIState=#my_gui_state{ model_mat4=ModelMat4,
                                      camera=Camera,
                                      proj_mat4=ProjMat4,
                                      opengl_state=#my_opengl_state{
                                        mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = ?delta_coord,

    % Translation on the Y axis:
    VT = [ 0.0, Inc, 0.0 ],

    NewPos = point3:translate( Camera#camera.position, VT ),

    NewCamera = camera:set_position( NewPos, Camera ),

    NewViewMat4 = camera:get_view_matrix( NewCamera ),

    trace_utils:debug_fmt( "Translating camera Y of ~f, "
        "resulting in: Mv = ~ts~ts",
        [ Inc, matrix4:to_string( NewViewMat4 ),
          describe_camera_origin( NewViewMat4 ) ] ),

    % Allows to check positioning:
    trace_utils:debug_fmt( "New camera position: ~p", [
       matrix4:get_translation(
           camera:get_view_matrix_inverse( NewCamera ) ) ] ),

    update_mvp_matrix( ModelMat4, NewViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ camera=NewCamera, view_mat4=NewViewMat4 },
      _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?camera_decrease_y_scan_code,
              GUIState=#my_gui_state{ model_mat4=ModelMat4,
                                      camera=Camera,
                                      proj_mat4=ProjMat4,
                                      opengl_state=#my_opengl_state{
                                        mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = ?delta_coord,

    % Translation on the Y axis:
    VT = [ 0.0, -Inc, 0.0 ],

    NewPos = point3:translate( Camera#camera.position, VT ),
    NewCamera = camera:set_position( NewPos, Camera ),

    NewViewMat4 = camera:get_view_matrix( NewCamera ),

    trace_utils:debug_fmt( "Translating camera Y of -~f, "
        "resulting in: Mv = ~ts~ts",
        [ Inc, matrix4:to_string( NewViewMat4 ),
          describe_camera_origin( NewViewMat4 ) ] ),

    % Allows to check positioning:
    %trace_utils:debug_fmt( "New camera position: ~p", [
    %   matrix4:get_translation(
    %       camera:get_view_matrix_inverse( NewCamera ) ) ] ),

    update_mvp_matrix( ModelMat4, NewViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ camera=NewCamera, view_mat4=NewViewMat4 },
      _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?camera_increase_z_scan_code,
              GUIState=#my_gui_state{ model_mat4=ModelMat4,
                                      camera=Camera,
                                      proj_mat4=ProjMat4,
                                      opengl_state=#my_opengl_state{
                                        mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = ?delta_coord,

    % Translation on the Z axis:
    VT = [ 0.0, 0.0, Inc ],

    NewPos = point3:translate( Camera#camera.position, VT ),

    NewCamera = camera:set_position( NewPos, Camera ),

    NewViewMat4 = camera:get_view_matrix( NewCamera ),

    trace_utils:debug_fmt( "Translating camera Z of ~f, "
        "resulting in: Mv = ~ts~ts",
        [ Inc, matrix4:to_string( NewViewMat4 ),
          describe_camera_origin( NewViewMat4 ) ] ),

    % Allows to check positioning:
    trace_utils:debug_fmt( "New camera position: ~p", [
       matrix4:get_translation(
           camera:get_view_matrix_inverse( NewCamera ) ) ] ),

    update_mvp_matrix( ModelMat4, NewViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ camera=NewCamera, view_mat4=NewViewMat4 },
      _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?camera_decrease_z_scan_code,
              GUIState=#my_gui_state{ model_mat4=ModelMat4,
                                      camera=Camera,
                                      proj_mat4=ProjMat4,
                                      opengl_state=#my_opengl_state{
                                        mvp_mat4_id=MVPMatUnifId } } ) ->

    Inc = ?delta_coord,

    % Translation on the Z axis:
    VT = [ 0.0, 0.0, -Inc ],

    NewPos = point3:translate( Camera#camera.position, VT ),
    NewCamera = camera:set_position( NewPos, Camera ),

    NewViewMat4 = camera:get_view_matrix( NewCamera ),

    trace_utils:debug_fmt( "Translating camera Z of -~f, "
        "resulting in: Mv = ~ts~ts",
        [ Inc, matrix4:to_string( NewViewMat4 ),
          describe_camera_origin( NewViewMat4 ) ] ),

    % Allows to check positioning:
    %trace_utils:debug_fmt( "New camera position: ~p", [
    %   matrix4:get_translation(
    %       camera:get_view_matrix_inverse( NewCamera ) ) ] ),

    update_mvp_matrix( ModelMat4, NewViewMat4, ProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ camera=NewCamera, view_mat4=NewViewMat4 },
      _DoQuit=false };



% Third section: regarding the projection.

update_scene_on_key_pressed( _Scancode=?projection_mode_scan_code,
              GUIState=#my_gui_state{ aspect_ratio=AspectRatio,
                                      model_mat4=ModelMat4,
                                      view_mat4=ViewMat4,
                                      projection_settings=ProjSettings,
                                      opengl_state=#my_opengl_state{
                                        mvp_mat4_id=MVPMatUnifId } } ) ->
    % Swapping the projection type:
    { NewProjSettings, NewProjMat4 } =
            case type_utils:get_record_tag( ProjSettings ) of

        orthographic_settings ->
            % No typo here, 'perspective' wanted:
            PerspSettings =
                projection:get_base_perspective_settings( AspectRatio ),
            { PerspSettings, projection:perspective( PerspSettings ) };

        perspective_settings ->
            % No typo here either, 'orthographic' wanted:
            OrthoSettings = projection:get_base_orthographic_settings(),
            { OrthoSettings, projection:orthographic( OrthoSettings ) }

    end,

    trace_utils:debug_fmt( "Switching to ~ts, the corresponding projection "
        "matrix being: Mp = ~ts.",
        [ projection:settings_to_string( NewProjSettings ),
          matrix4:to_string( NewProjMat4 ) ] ),

    update_mvp_matrix( ModelMat4, ViewMat4, NewProjMat4, MVPMatUnifId ),

    { GUIState#my_gui_state{ projection_settings=NewProjSettings,
                             proj_mat4=NewProjMat4 },
      _DoQuit=false };


update_scene_on_key_pressed( _Scancode=?quit_scan_code, GUIState ) ->
    trace_utils:debug( "Requested to quit." ),
    { GUIState, _DoQuit=true };

update_scene_on_key_pressed( _Scancode=?help_scan_code, GUIState ) ->
    trace_utils:debug( get_help_text() ),
    { GUIState, _DoQuit=false };

update_scene_on_key_pressed( _Scancode, GUIState ) ->
    %trace_utils:debug_fmt( "(scancode ~B ignored)", [ Scancode ] ),
    { GUIState, _DoQuit=false }.



-doc """
Returns a description of the local origin of the square, in the global (world)
coordinate system.
""".
-spec describe_square_origin( matrix4() ) -> ustring().
describe_square_origin( ModelMat4 ) ->

    % For some reason, initially an inversion was done:

    % Using a transformation would eliminate the need of this inversion:
    %% case matrix4:inverse( ModelMat4 ) of

    %   undefined ->
    %       text_utils:format( "~nThe local origin of the square coordinate "
    %           "system in the global coordinate system cannot be determined "
    %           "(singular matrix); too much downscaling attempted?~n"
    %           "Model-view matrix is ~ts",
    %           [ matrix4:to_string( ModelMat4 ) ] );

    %   InvMat4 ->
    %       LocalOrigin = matrix4:get_translation( InvMat4 ),

    %       text_utils:format( "~nIn the global coordinate system, "
    %           "the local origin of the square coordinate system is now: ~ts",
    %           [ point3:to_string( LocalOrigin ) ] )

    %% end.

    LocalOrigin = matrix4:get_translation( ModelMat4 ),

    text_utils:format( "~nIn the global coordinate system, "
        "the local origin of the square coordinate system is now: ~ts",
        [ point3:to_string( LocalOrigin ) ] ).



-doc """
Returns a description of the local origin of the camera, in the global (world)
coordinate system.
""".
-spec describe_camera_origin( matrix4() ) -> ustring().
describe_camera_origin( ViewMat4 ) ->

    LocalOrigin = matrix4:get_translation( ViewMat4 ),

    text_utils:format( "~nIn the global coordinate system, "
        "the local origin of the camera coordinate system is now: ~ts",
        [ point3:to_string( LocalOrigin ) ] ).



-doc "Recomputes the MVP uniform matrix.".
-spec update_mvp_matrix( matrix4(), matrix4(), matrix4(), uniform_id() ) ->
                                                void().
update_mvp_matrix( ModelMat4, ViewMat4, ProjMat4, MVPMatUnifId ) ->

    % Of course order matters:
    MVPMat4 = matrix4:mult( [ ProjMat4, ViewMat4, ModelMat4 ] ),

    gui_shader:set_uniform_matrix4( MVPMatUnifId, MVPMat4 ).



-doc "Runs the test.".
-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    gui_opengl_for_testing:can_be_run(
            "the test of transformation support with OpenGL shaders" ) =:= yes
        andalso run_actual_test(),

    test_facilities:stop().
