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
% Creation date: Wednesday, December 6, 2023.


% @doc Support for <b>splash screens</b>.
%
% Basic splash screens just display images, whereas more complex ones are
% dynamically built from elements that can include versions, release dates, etc.
%
% In all cases they are to be displayed as soon as possible (when created, which
% returns the PID of their controller process), and will be dismissed and fully
% deallocated/terminated as soon as that process will receive a removal message.
%
% Refer to the gui_splash_test module for an example of use.
%
-module(gui_splash).


-type splash_panel() :: panel().
% A panel used by a splash screen.


-record( basic_splash_info, {

	splash_frame :: frame(),

	% The splash panel (in the splash frame), used here as a canvas:
	% (useful to keep for proper pattern-matching afterwards)
	%
	splash_panel :: splash_panel(),

	% The off-screen bitmaps where all renderings take place:
	backbuffer :: bitmap(),

	% The ready-to-use in-memory data corresponding to an image to be displayed:
	image_bitmap :: bitmap() } ).


-type basic_splash_info() :: #basic_splash_info{}.
% Information to be kept by an application using a basic splash screen.



-record( dynamic_splash_info, {

	splash_frame :: frame(),

	% The splash panel (in the splash frame), used here as a canvas:
	% (useful to keep for proper pattern-matching afterwards)
	%
	splash_panel :: splash_panel(),

	% The off-screen bitmaps where all renderings take place:
	%backbuffer :: bitmap(),

	% The ready-to-use in-memory data corresponding to the icon of interest:
	icon_bitmap :: bitmap(),

	% The ready-to-use in-memory data corresponding to the main image (logo) of
	% interest:
	%
	main_bitmap :: bitmap() } ).


-type dynamic_splash_info() :: #dynamic_splash_info{}.
% Information to be kept by an application using a dynamic splash screen.




-type splash_info() :: basic_splash_info().
% Information regarding a splash screen so that it can be properly managed.


-type removal_message() :: 'removeSplash'.
% The atom to be received by a controller to dismiss its splash screen and
% terminate.


-export_type([ splash_panel/0, basic_splash_info/0, splash_info/0,
			   removal_message/0 ]).



% Implementation notes:
%
% For splashed content, we favor mostly the PNG and JPEG formats.
%
% Here, rather than using our canvas, we directly paint on the panel defined
% within the splash frame.
%
% While the splash screen is active, any kind of event can happen, leading to
% resizing and repainting to be needed, which is thus to be managed in the event
% loop of the overall application.


-export([ create_basic/2, create_basic/3, create_dynamic/10,
		  show/1,
		  on_repaint_needed/2, on_resized/3, remove/1,
		  get_panel/1, get_frame/1,
		  destruct/1, update_panel/2 ]).


% Shorthands:

-type any_string() :: text_utils:any_string().

-type scale_factor() :: math_utils:scale_factor().

-type size() :: gui:size().
-type parent() :: gui:parent().

-type color() :: gui_color:color().

-type any_image_path() :: gui_image:any_image_path().

-type panel() :: gui_panel:panel().
-type frame() :: gui_frame:frame().
-type bitmap() :: gui_bitmap:bitmap().



% @doc Creates a basic splash screen that will display the specified image on a
% minimalist frame created on top of the specified parent (typically a frame,
% probably the main one), and returns the associated splash information, for
% future use (event management).
%
% The splash screen will be dismissed when the application will call the
% remove/1 function, typically once fully initialised and ready.
%
% See the gui_splash_test module for an usage example.
%
-spec create_basic( any_image_path(), parent() ) -> basic_splash_info().
create_basic( ImgPath, Parent ) ->
	create_basic( ImgPath, _ScaleFactor=1, Parent ).




% @doc Creates a basic splash screen that will display the specified image
% once scaled (uniformously) at the specified factor (typically in ]0,1[ to
% shrink a too large image) on a minimalist frame on top of the specified parent
% (typically a frame, probably the main one), and returns the associated splash
% information, for future use (event management).
%
% The splash screen will be dismissed when the application will call the
% remove/1 function, typically once fully initialised and ready.
%
% See the gui_splash_test module for an usage example.
%
-spec create_basic( any_image_path(), scale_factor(), parent() ) ->
											basic_splash_info().
create_basic( ImgPath, ScaleF, Parent ) ->

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt( "Creating a basic splash screen, "
			"to display '~ts', with scale ~w, and parent ~w.",
			[ ImgPath, ScaleF, Parent ] ) ),

	% Needing first to establish the size of the splash frame:
	Img = gui_image:load_from_file( ImgPath ),

	NativeImgSize = { NativeImgWidth, NativeImgHeight } =
		gui_image:get_size( Img ),

	% ScaleF is integer or float:
	ScaleF == 1 orelse
		begin
			ScaledImgWidth  = round( NativeImgWidth  * ScaleF ),
			ScaledImgHeight = round( NativeImgHeight * ScaleF ),

			% Image rescaled in-place:
			gui_image:scale( Img, ScaledImgWidth, ScaledImgHeight )
		end,

	Pos = auto,

	% The image bitmap, kept once for all to regenerate the backbuffer as
	% needed:
	%
	ImgBitmap = gui_image:to_bitmap( Img ),

	ImgBitmapSize = gui_bitmap:get_size( ImgBitmap ),

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt( "Image original size: ~w pixels, "
			"scaled down for bitmap: ~w.", [ NativeImgSize, ImgBitmapSize ] ) ),

	SplashFrame = create_splash_frame( Parent ),

	% (should no size be specified, would be 20x20 initially)
	SplashPanel = gui_panel:create( Pos, ImgBitmapSize, SplashFrame ),

	% To be compared afterwards with bitmap size; even if starting with a
	% correct, sufficient size, may by default be immediately shrunk vertically
	% afterwards:
	%
	SplashPanelSize = gui_panel:get_size( SplashPanel ),

	% Panel size should be of the exact same size as the latest actual image
	% one:
	%
	cond_utils:if_defined( myriad_debug_gui_splash, trace_utils:debug_fmt(
		"Initial splash panel size: ~w.", [ SplashPanelSize ] ) ),

	% Force correct size, no shrinking wanted:
	gui_widget:set_client_size( SplashFrame, SplashPanelSize ),

	% The backbuffer on which the panel content will be drawn:
	BackbufferBitmap = gui_bitmap:create_empty_for( SplashPanel ),

	% Initialisation:
	render_basic_splash( BackbufferBitmap, ImgBitmap ),

	% Both can happen, separately (just a repaint, e.g. after having been masked
	% by another window) or not (resize then repaint):
	%
	gui:subscribe_to_events( [ { onResized,       SplashPanel },
							   { onRepaintNeeded, SplashPanel } ] ),

	% Could/would be too early (main frame shall be shown first):
	%gui_frame:show( SplashFrame ),

	#basic_splash_info{ splash_frame=SplashFrame,
						splash_panel=SplashPanel,
						backbuffer=BackbufferBitmap,
						image_bitmap=ImgBitmap }.


% @doc Blits the specified bitmap to the specified panel, once cleared.
%
% For example so that a splash panel is updated based on an already-available
% backbuffer bitmap.
%
-spec update_panel( panel(), bitmap() ) -> void().
update_panel( TargetPanel, SourceBitmap ) ->

	trace_utils:debug_fmt( "Updating panel ~w from bitmap ~w.",
						   [ TargetPanel, SourceBitmap ] ),

	% Locks the source surface (device context):
	SourceBitmapDC = gui_bitmap:lock( SourceBitmap ),

	% Then blits this updated backbuffer to the panel:
	TopLeftPos = {0,0},

	% Locks the target surface (device context):
	TargetPanelDC = gui_widget:lock( TargetPanel ),

	gui_render:blit( _From=SourceBitmapDC, _FromPos=TopLeftPos,
		_BlitArea=gui_bitmap:get_size( SourceBitmap ),
		_To=TargetPanelDC, _ToPos=TopLeftPos ),

	gui_widget:unlock( TargetPanelDC ),

	gui_bitmap:unlock( SourceBitmapDC ),

	trace_utils:debug( "Updated panel." ).


% @doc Creates a dynamic splash screen displaying the specified information on a
% minimalist frame on top of the specified parent (typically a frame, probably
% the main one), and returns the associated splash information, for future use
% (event management).
%
% The resulting splash screen (heavily inspired from the one of Wings3D) will be
% rendered on a panel whose background color will be the specified one, and will
% be organised based on three rows (each described from left to right):
%
%  1. a top row (of the same background color), including:
%     * an icon-like image of the project
%     * then, just afterwards, two lines:
%       - top one: with a large font, the title of the project (e.g. "Foobar")
%       followed on its right, with a smaller font, by a version string
%       (e.g. "v1.0.17")
%       - bottom one (finer font): a description of the project (e.g. "Foobar is
%       a Frobnicator with twin acceleration beams")
%     * right-justified: the project URL (e.g. "www.foobar.org")
%
%  2. a middle row displaying (without horizontal margins) the project's main
%  representation, as an image, at full size
%
%  3. a bottom row, made of two text displays, on both sides of a spacer:
%
%     * general information (e.g. terms of use) on the left, e.g. "Foobar comes
%     with absolutely no warranty, but is completly free for any kind of use
%     (including commercial)." or "Published by Foobar
%     Software\nhttp://foobar.com"
%
%     * copyright information on the right, i.e. "Copyright (C) 2022-2023 John
%     Doe, James Bond and Others" or "Copyright" ++ [$\s,169] ++ " 2022-2023
%     John Doe\nAll rights reserved"
%
% The overall width is solely determined by the one of the image in the middle
% row.
%
% The overall height is the sum of the one of the three rows/panels:
%  1. the height of the first row is solely determined by the one of the icon
%  image
%  2. the height of the second row is solely determined by the one of its (main)
%  image
%  3. the height of the third row is solely determined by the maximum one of the
%  two text displays
%
% The splash screen will be dismissed when the application will call the
% remove/1 function.
%
-spec create_dynamic( IconImgPath :: any_image_path(), TitleStr :: any_string(),
	VersionStr :: any_string(), DescStr :: any_string(), URLStr :: any_string(),
	BackgroundColor :: color(),
	MainImgPath :: any_image_path(), GeneralInfoStr :: any_string(),
	CopyrightStr :: any_string(), parent() ) -> dynamic_splash_info().
create_dynamic( IconImgPath, TitleStr, VersionStr, DescStr, URLStr,
		BackgroundColor, MainImgPath, GeneralInfoStr, CopyrightStr, Parent ) ->

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt( "Creating a dynamic splash screen, "
			"to display '~ts' and '~ts', with parent ~w.",
			[ IconImgPath, MainImgPath, Parent ] ) ),

	SplashFrame = create_splash_frame( Parent ),

	% No position, no size:
	SplashPanel = gui_panel:create( _Par=SplashFrame ),

	IconImg = gui_image:load_from_file( IconImgPath ),
	IconBitmap = gui_image:to_bitmap( IconImg ),

	MainImg = gui_image:load_from_file( MainImgPath ),
	MainBitmap = gui_image:to_bitmap( MainImg ),

	% Initialisation:
	{ _MainSizer, _TopPanel, _MainPanel, _MainStaticBtmpDisp,
	  _LeftTextDisplay, _RightTextDisplay } =
		render_dynamic_splash( SplashPanel,
		IconBitmap, TitleStr, VersionStr, DescStr, URLStr, BackgroundColor,
		MainBitmap, GeneralInfoStr, CopyrightStr ),

	#dynamic_splash_info{
		splash_frame=SplashFrame,
		splash_panel=SplashPanel,
		icon_bitmap=IconBitmap,
		main_bitmap=MainBitmap }.



% @doc Creates the necessary widgets in order to fill the specified splash panel
% with splash content.
%
% Defined for re-use.
%
render_dynamic_splash( SplashPanel, IconBitmap, _TitleStr, _VersionStr,
		_DescStr, _URLStr, BackgroundColor, MainBitmap, GeneralInfoStr,
		CopyrightStr ) ->

	gui_widget:set_background_color( SplashPanel, BackgroundColor ),

	% Let's proceed row per row, stacked vertically thanks to:
	MainSizer = gui_sizer:create_with_box( _Orientation=vertical ),

	IconHeight = gui_bitmap:get_height( IconBitmap ),

	% First row: setting a sufficient initial height of the top panel in order
	% to contain all elements (so that the icon image and bottom text fit); it
	% will be fixed, whereas its width will expand with the sizer:
	%
	% (we cannot render the header text yet, as this top panel/static bitmap
	% display has not its final size)
	%
	TopPanel = gui_panel:create( { size, { _W=0, _H=IconHeight } },
								 SplashPanel ),

	gui_widget:set_background_color( TopPanel, BackgroundColor ),

	gui_sizer:add_element( MainSizer, TopPanel,
		[ { proportion, _VerticallyFixed=0 }, expand_fully ] ),


	% Second row: the splash (main) image.

	MainDims = gui_bitmap:get_size( MainBitmap ),

	MainPanel = gui_panel:create(
		[ { size, MainDims }, { style, no_border } ], SplashPanel ),

	MainStaticBtmpDisp =
		gui_bitmap:create_static_display( MainBitmap, _Par=SplashPanel ),

	gui_sizer:add_element( MainSizer, SplashPanel,
		[ { proportion, 0 }, { border, 5 }, all_borders, align_center ] ),


	% Taking care of the bottom part (third row) now:

	BottomFont = gui_font:create( _PointSize=10 ),

	gui_widget:set_font( SplashPanel, BottomFont, _Textcolor=black,
						 _DestructFont=true ),

	BottomSizer = gui_sizer:create( _Orient=horizontal ),

	LeftTextDisplay = gui_text:create_static_display( GeneralInfoStr,
													  SplashPanel ),

	gui_sizer:add_element( BottomSizer, LeftTextDisplay,
						   [ { proportion, 0 }, { border, 5 }, left_border ] ),

	% In-between:
	gui_sizer:add_spacer( BottomSizer, _Width=0, _Height=0,
						  [ { proportion, 1 }, expand_fully] ),

	RightTextDisplay = gui_text:create_static_display( CopyrightStr,
		{ style, [ align_right ] }, SplashPanel ),

	gui_sizer:add_element( BottomSizer, RightTextDisplay,
						   [ { proportion, 0 }, { border, 5 }, right_border ] ),

	gui_sizer:add_element( MainSizer, BottomSizer,
		[ { proportion, 1 }, { border, 8 }, all_borders, expand_fully ] ),

	gui_widget:set_sizer( SplashPanel, MainSizer ),

	gui_widget:fit_to_sizer( SplashPanel, MainSizer ),

	{ MainSizer, TopPanel, MainPanel, MainStaticBtmpDisp,
	  LeftTextDisplay, RightTextDisplay }.



% @doc Returns a frame suitable to hosting any kind of splash screen.
-spec create_splash_frame( parent() ) -> frame().
create_splash_frame( Parent ) ->

	% The splash frame will directly have its client area set accordingly:
	%SplashFrameSize = { ScaledImgWidth + 15, ScaledImgHeight + 15 },
	SplashFrameSize = auto,

	% A mere window would not be sufficient: frame needed; title useless:
	gui_frame:create( _STitle="Myriad Splash Screen", _Pos=auto,
		SplashFrameSize,
		_SplashStyles=[ no_border, no_taskbar, float_on_parent ],
		_NoId=undefined, Parent ).



% @doc Shows the corresponding splash screen.
%
% Must be called only after that the parent frame is shown, otherwise the splash
% screen will not be centered in the parent, but on the whole screen, which
% generally is not desirable.
%
-spec show( splash_info() ) -> void().
show( #basic_splash_info{ splash_frame=SplashFrame } ) ->
	gui_frame:show( SplashFrame );

show( #dynamic_splash_info{ splash_frame=SplashFrame } ) ->
	gui_frame:show( SplashFrame ).


% @doc Callback to be triggered whenever the application catches a
% onRepaintNeeded event for the splash panel.
%
-spec on_repaint_needed( panel(), splash_info() ) -> void().
on_repaint_needed( SplashPanel, _SplashInfo=#basic_splash_info{
		backbuffer=BackbufferBitmap } ) ->
		%image_bitmap=ImgBitmap } ) ->

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt(
			"Basic splash panel '~ts' needs to be repainted (from ~w).",
			[ gui:object_to_string( SplashPanel ), BackbufferBitmap ] ) ),

	% No size change, so backbuffer still legit:
	%render_basic_splash( SplashPanel, BackbufferBitmap, ImgBitmap ).
	update_panel( SplashPanel, BackbufferBitmap );

on_repaint_needed( SplashPanel, _SplashInfo=#dynamic_splash_info{} ) ->

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt(
			"Dynamic splash panel '~ts' needs to be repainted.",
			[ gui:object_to_string( SplashPanel ) ] ) ),

	% Nothing done.

	ok.


% @doc Callback to be triggered whenever the application catches a onResized
% event for the splash panel.
%
-spec on_resized( panel(), size(), splash_info() ) -> splash_info().
on_resized( SplashPanel, NewSize, SplashInfo=#basic_splash_info{
		backbuffer=BackbufferBitmap,
		image_bitmap=ImgBitmap } ) ->

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt( "Splash panel '~ts' resized to ~p.",
			[ gui:object_to_string( SplashPanel ), NewSize ] ) ),

	% We have to resize the framebuffer first:
	NewBackbufferBitmap = gui_bitmap:create_empty( NewSize ),

	% A new backbuffer is used, the image must be copied on it (update_panel/2
	% would not suffice):
	%
	render_basic_splash( NewBackbufferBitmap, ImgBitmap ),

	gui_bitmap:destruct( BackbufferBitmap ),

	%trace_utils:debug( "Splash panel resized (render)." ),

	SplashInfo#basic_splash_info{ backbuffer=NewBackbufferBitmap }.




% @doc Callback to be triggered whenever the application chooses to remove the
% corresponding splash screen.
%
-spec remove( splash_info() ) -> void().
remove( SplashInfo ) ->
	destruct( SplashInfo ).



% @doc Returns the splash panel, as stored in the specified splash information.
-spec get_panel( splash_info() ) -> panel().
get_panel( #basic_splash_info{ splash_panel=SplashPanel } ) ->
	SplashPanel;

get_panel( #dynamic_splash_info{ splash_panel=SplashPanel } ) ->
	SplashPanel.


% @doc Returns the splash frame, as stored in the specified splash information.
-spec get_frame( splash_info() ) -> frame().
get_frame( #basic_splash_info{ splash_frame=SplashFrame } ) ->
	SplashFrame;

get_frame( #dynamic_splash_info{ splash_frame=SplashFrame } ) ->
	SplashFrame.



% @doc Destructs the splash screen, based on the specified splash information.
-spec destruct( splash_info() ) -> void().
destruct( #basic_splash_info{ splash_frame=SplashFrame,
							  backbuffer=BackBufferBitmap,
							  image_bitmap=ImgBitmap } ) ->
	% Implies panel:
	gui_frame:destruct( SplashFrame ),

	[ gui_bitmap:destruct( B ) || B <- [ BackBufferBitmap, ImgBitmap ] ];

destruct( #dynamic_splash_info{ splash_frame=SplashFrame,
								icon_bitmap=IconBitmap,
								main_bitmap=MainBitmap } ) ->

	% Implies panel and all:
	gui_frame:destruct( SplashFrame ),

	[ gui_bitmap:destruct( B ) || B <- [ IconBitmap, MainBitmap ] ].




% Internal helpers.


% @doc Renders the splash screen ("once for all"): updates, from the specified
% image bitmap, the (bitmap) backbuffer (but does not blit it to any target
% panel).
%
-spec render_basic_splash( Target :: bitmap(), Source :: bitmap() ) -> void().
render_basic_splash( BackbufferBitmap, ImageBitmap ) ->

	% Note: the image could be copied directly onto the panel, yet preparing a
	% backbuffer will allow preparing to easily and efficiently blit this
	% panel-size backbuffer afterwards to said panel.

	% Updates the backbuffer with the stored image, drawn from its top-left
	% position:
	%
	TopLeftPos = {0,0},

	% Locks the target surface (device context):
	BackbufferDC = gui_bitmap:lock( BackbufferBitmap ),

	gui_render:clear_device_context( BackbufferDC ),

	% Just copies the image bitmap onto the backbuffer from said corner:
	gui_bitmap:draw( _Source=ImageBitmap, _Target=BackbufferDC,
					 _PosInTarget=TopLeftPos ).
