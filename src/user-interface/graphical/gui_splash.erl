% Copyright (C) 2023-2024 Olivier Boudeville
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

	% The ready-to-use in-memory data corresponding to the small image
	% (icon-like) of interest representing the application:
	%
	symbol_bitmap :: bitmap(),

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


-export([ create_basic/2, create_basic/3, create_dynamic/11,
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



% Section for basic splash screens.


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
			"scaled down for bitmap: ~w.", [ NativeImgSize, ImgBitmapSize ] ),
		basic_utils:ignore_unused( [ NativeImgSize, ImgBitmapSize ] ) ),

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



% Section for dynamically-created splash screens.


% @doc Creates a dynamic splash screen displaying the specified information on a
% minimalist frame on top of the specified parent (typically a frame, probably
% the main one), and returns the associated splash information, for future use
% (event management).
%
% The resulting splash screen (heavily inspired from the one of Wings3D) will be
% rendered with the specified overall background color, and will be organised
% based on three rows (each described from left to right):
%
%  1. a top row (of the same background color), including (left-to-right):
%     * an icon-like image symbolising the project
%     * then, just afterwards, two lines, on the specified title background
%     color:
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
%  3. a bottom row, made of two text displays, on either sides of a spacer:
%
%     * general information (e.g. terms of use) on the left, e.g. "Foobar comes
%     with absolutely no warranty, but is completly free for any kind of use
%     (including commercial).", or "Published by Foobar
%     Software\nhttp://foobar.com"
%
%     * copyright information on the right, i.e. "Copyright (C) 2022-2023 John
%     Doe, James Bond and Others", or "Copyright" ++ [$\s,169] ++ " 2022-2023
%     John Doe\nAll rights reserved"
%
% The overall width is solely determined by the one of the image in the middle
% row. Ensure that all other elements are not too wide (e.g. that texts are not
% too long, knowing that they can be multi-lines).
%
% The overall height is the sum of the one of the three rows/panels:
%  1. the height of the first row is solely determined by the one of the symbol
%  image
%  2. the height of the second row is solely determined by the one of its (main)
%  image
%  3. the height of the third row is solely determined by the maximum one of the
%  two text displays
%
% The splash screen will be dismissed when the application will call the
% remove/1 function.
%
-spec create_dynamic( SymbolImgPath :: any_image_path(),
	TitleStr :: any_string(), VersionStr :: any_string(),
	DescStr :: any_string(), UrlStr :: any_string(),
	TitleBackgroundColor :: color(), OverallBackgroundColor :: color(),
	MainImgPath :: any_image_path(), GeneralInfoStr :: any_string(),
	CopyrightStr :: any_string(), parent() ) -> dynamic_splash_info().
create_dynamic( SymbolImgPath, TitleStr, VersionStr, DescStr, UrlStr,
		TitleBackgroundColor, OverallBackgroundColor, MainImgPath,
		GeneralInfoStr, CopyrightStr, Parent ) ->

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt( "Creating a dynamic splash screen, "
			"to display '~ts' and '~ts', with parent ~w.",
			[ SymbolImgPath, MainImgPath, Parent ] ) ),

	SplashFrame = create_splash_frame( Parent ),

	SymbolBitmap = gui_image:create_bitmap( SymbolImgPath ),
	MainBitmap = gui_image:create_bitmap( MainImgPath ),

	% Initialisation:
	{ _MainSizer, _TopPanel, _MainPanel, _MainStaticBtmpDisp,
	  _LeftTextDisplay, _RightTextDisplay } =
		render_dynamic_splash( SplashFrame, SymbolBitmap, TitleStr, VersionStr,
			DescStr, UrlStr, TitleBackgroundColor, OverallBackgroundColor,
			MainBitmap, GeneralInfoStr, CopyrightStr ),

	#dynamic_splash_info{ splash_frame=SplashFrame,
						  %splash_panel=SplashPanel,
						  symbol_bitmap=SymbolBitmap,
						  main_bitmap=MainBitmap }.



% @doc Creates the necessary widgets in order to fill the specified frame with
% splash content.
%
% Defined for re-use.
%
render_dynamic_splash( TargetFrame, SymbolBitmap, TitleStr, VersionStr,
		DescStr, UrlStr, TitleBackgroundColor, OverallBackgroundColor,
		MainBitmap, GeneralInfoStr, CopyrightStr ) ->

	trace_utils:debug_fmt( "Rendering dynamic splash on frame ~w.",
						   [ TargetFrame ] ),

	gui_widget:set_background_color( TargetFrame, OverallBackgroundColor ),

	% Let's proceed row per row, stacked vertically thanks to:
	MainSizer = gui_sizer:create( _Orientation=vertical ),


	trace_utils:debug( "Operating on first row: symbol and texts." ),

	% First row: setting a sufficient initial height of the top panel in order
	% to contain all elements (so that the symbol image and bottom text fit); it
	% will be fixed, whereas its width will expand with the sizer:
	%
	% (we cannot render the header text yet, as this top panel/static bitmap
	% display has not its final size)
	%
	TopSizer = gui_sizer:create( _Orient=horizontal ),

	SymbolBmpDisplay =
		gui_bitmap:create_static_display( SymbolBitmap, TargetFrame ),


	gui_sizer:add_element( TopSizer, SymbolBmpDisplay,
		[ { proportion, 0 }, { border, 5 }, right_border, align_left ] ),

	% To set exactly the height of the information panel:
	SymbolHeight = gui_bitmap:get_height( SymbolBitmap ),

	MainBitmapWidth = gui_bitmap:get_width( MainBitmap ),

	% A zero width could have sufficed, yet afterwards for the URL display we
	% have to rely on a proper evaluation of the panel width in order to
	% right-align it; so:
	%
	InfoPanel = gui_panel:create(
		{ size, { _Width=MainBitmapWidth, _Height=SymbolHeight } }, TargetFrame ),

	%trace_utils:debug_fmt( "Size of info panel: ~p.",
	%                       [ gui_widget:get_size( InfoPanel ) ] ),

	gui_widget:set_background_color( InfoPanel, TitleBackgroundColor ),

	TitleFontSize = 12,
	TitleFontFamily = decorative, % teletype,
	TitleFontStyle = slant, % normal,

	TitleFont = gui_font:create( TitleFontSize, TitleFontFamily,
								 TitleFontStyle ),

	DescFontSize = 9,

	% Title left-aligned, centered vertically if single-line:
	%TitlePos = { TitleX=5, TitleY=( SymbolHeight - TitleFontSize ) div 2 },

	TitleX = 5,

	% Between the two levels of title/desc:
	YLvlMargin = 2,

	% Between the bottom of the description and the one of the panel:
	YBottomMargin = 3,

	TitleY = ( SymbolHeight - YBottomMargin
			   - ( TitleFontSize + YLvlMargin + DescFontSize ) ) div 2,

	TitlePos = { TitleX, TitleY },

	TitleOpt = { position, TitlePos },

	{ _TitleDisplay,
	  _TitleDispSize={ TitleDW, TitleDH, TitleDescent, _TitleExtLead } } =
		gui_text:create_presized_static_display( TitleStr, TitleOpt,
												 TitleFont, InfoPanel ),

	gui_font:destruct( TitleFont ),

	%gui_widget:set_background_color( TitleDisplay, yellow ),

	DescFontFamily = TitleFontFamily,
	DescFontStyle = normal,

	DescFont = gui_font:create( DescFontSize, DescFontFamily,
								DescFontStyle ),


	DescPos = { TitleX, SymbolHeight - DescFontSize - YBottomMargin },

	DescOpt = { position, DescPos },

	gui_text:create_presized_static_display( DescStr, DescOpt, DescFont,
											 InfoPanel ),


	VersionFontSize = 10,
	VersionFontFamily = TitleFontFamily,
	VersionFontStyle = normal,

	XMargin = 5,

	VersionPos = { TitleX + TitleDW + XMargin,
				   TitleY + TitleDH - TitleDescent - VersionFontSize },

	VersionFont = gui_font:create( VersionFontSize, VersionFontFamily,
								   VersionFontStyle ),


	VersionOpt = { position, VersionPos },

	{ _VersionDisplay, _VersionDispSize } =
		gui_text:create_presized_static_display( VersionStr, VersionOpt,
												 VersionFont, InfoPanel ),

	%gui_widget:set_background_color( VersionDisplay, brown ),


	UrlFont = VersionFont,

	UrlSize = { UrlW, UrlH } = gui_font:get_text_extent( UrlStr, UrlFont ),

	_InfoPanelSize = { IPW, _IPH } = gui_widget:get_size( InfoPanel ),

	UrlPos = { IPW - UrlW - XMargin, ( SymbolHeight - UrlH ) div 2 },

	UrlDisplay = gui_text:create_static_display( UrlStr,
		_Opts=[ { position, UrlPos }, { size, UrlSize } ], InfoPanel ),

	gui_widget:set_font( UrlDisplay, UrlFont ),

	gui_font:destruct( VersionFont ),

	gui_sizer:add_element( TopSizer, InfoPanel,
		[ { proportion, 1 }, expand_fully ] ),

	gui_sizer:add_element( MainSizer, TopSizer,
		[ { proportion, 0 }, { border, 8 }, all_borders, expand_fully ] ),


	MainDims = gui_bitmap:get_size( MainBitmap ),

	trace_utils:debug_fmt( "Operating on second row: the splash (main) image, "
		"whose dimensions are ~w.", [ MainDims ] ),

	MainImgPanel = gui_panel:create(
		[ { size, MainDims }, { style, no_border } ], TargetFrame ),

	MainStaticBtmpDisp =
		gui_bitmap:create_static_display( MainBitmap, _Par=MainImgPanel ),

	gui_sizer:add_element( MainSizer, MainImgPanel,
		[ { proportion, 0 }, { border, 5 }, all_borders, align_center ] ),



	% Taking care of the bottom part now:
	trace_utils:debug( "Operating on third row: texts on the side." ),

	BottomFont = gui_font:create( _PointSize=10 ),

	gui_widget:set_font( TargetFrame, BottomFont, _Textcolor=black,
						 _DestructFont=true ),

	BottomSizer = gui_sizer:create( _HOrient=horizontal ),

	LeftTextDisplay =
		gui_text:create_static_display( _Lbel=GeneralInfoStr, _Pr=TargetFrame ),

	gui_sizer:add_element( BottomSizer, LeftTextDisplay,
						   [ { proportion, 0 }, { border, 5 }, left_border ] ),

	% In-between side texts:
	gui_sizer:add_spacer( BottomSizer, _Wdth=0, _Hght=0,
						  [ { proportion, 1 }, expand_fully ] ),

	RightTextDisplay = gui_text:create_static_display( _L=CopyrightStr,
		{ style, [ align_right ] }, TargetFrame ),

	gui_sizer:add_element( BottomSizer, RightTextDisplay,
						   [ { proportion, 0 }, { border, 5 }, right_border ] ),

	gui_sizer:add_element( MainSizer, BottomSizer,
		[ { proportion, 1 }, { border, 2 }, all_borders, expand_fully ] ),

	% Fitting is necessary to adopt a proper, sufficient frame size:
	gui_widget:set_and_fit_to_sizer( TargetFrame, MainSizer ),

	{ MainSizer, InfoPanel, MainImgPanel, MainStaticBtmpDisp,
	  LeftTextDisplay, RightTextDisplay }.



% Section for facilities common to all types of splash screens.


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

	gui_bitmap:unlock( SourceBitmapDC ).



% @doc Callback to be triggered whenever the application catches a
% onRepaintNeeded event for the splash panel.
%
-spec on_repaint_needed( panel(), splash_info() ) -> void().
on_repaint_needed( SplashPanel, _SplashInfo=#basic_splash_info{
		backbuffer=BackbufferBitmap } ) ->
		%image_bitmap=ImgBitmap } ) ->

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt(
			"Basic splash panel '~ts' needs to be repainted (based on ~w).",
			[ gui:object_to_string( SplashPanel ), BackbufferBitmap ] ) ),

	% No size change, so backbuffer still legit:
	%render_basic_splash( SplashPanel, BackbufferBitmap, ImgBitmap ).
	update_panel( SplashPanel, BackbufferBitmap );

on_repaint_needed( SplashPanel, _SplashInfo=#dynamic_splash_info{} ) ->

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt(
			"Dynamic splash panel '~ts' needs to be repainted.",
			[ gui:object_to_string( SplashPanel ) ] ),
		basic_utils:ignore_unused( [ SplashPanel ] ) ),

	% Nothing done:
	ok.


% @doc Callback to be triggered whenever the application catches a onResized
% event for the splash panel.
%
-spec on_resized( panel(), size(), splash_info() ) -> splash_info().
on_resized( SplashPanel, NewSize, SplashInfo=#basic_splash_info{
		backbuffer=BackbufferBitmap,
		image_bitmap=ImgBitmap } ) ->

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt( "Basic splash panel '~ts' resized to ~p.",
			[ gui:object_to_string( SplashPanel ), NewSize ] ),
		basic_utils:ignore_unused( [ SplashPanel ] ) ),

	% We have to resize the framebuffer first:
	NewBackbufferBitmap = gui_bitmap:create_empty( NewSize ),

	% A new backbuffer is used, the image must be copied on it (update_panel/2
	% would not suffice):
	%
	render_basic_splash( NewBackbufferBitmap, ImgBitmap ),

	gui_bitmap:destruct( BackbufferBitmap ),

	%trace_utils:debug( "Splash panel resized (render)." ),

	SplashInfo#basic_splash_info{ backbuffer=NewBackbufferBitmap };


on_resized( SplashPanel, NewSize, SplashInfo=#dynamic_splash_info{} ) ->

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt( "Dynamic splash panel '~ts' resized to ~p.",
			[ gui:object_to_string( SplashPanel ), NewSize ] ),
		basic_utils:ignore_unused( [ SplashPanel, NewSize ] ) ),

	SplashInfo.



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
								symbol_bitmap=SymbolBitmap,
								main_bitmap=MainBitmap } ) ->

	% Implies panel and all:
	gui_frame:destruct( SplashFrame ),

	[ gui_bitmap:destruct( B ) || B <- [ SymbolBitmap, MainBitmap ] ].
