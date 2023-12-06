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


-export([ create_basic/2, create_basic/3,
		  show/1,
		  on_repaint_needed/2, on_resized/3, remove/1,
		  get_panel/1, get_frame/1,
		  destruct/1 ]).



% Shorthands:

-type scale_factor() :: math_utils:scale_factor().

-type any_image_path() :: gui_image:any_image_path().

-type size() :: gui:size().
-type parent() :: gui:parent().

-type panel() :: gui_panel:panel().
-type frame() :: gui_frame:frame().
-type bitmap() :: gui_bitmap:bitmap().



% @doc Creates and shows a basic splash screen displaying the specified image on
% a minimalist frame created on top of the specified parent (typically a frame,
% probably the main one), and returns the associated splash information, for
% future use (event management).
%
% The splash screen will be dismissed when the application will call the
% remove/1 function.
%
-spec create_basic( any_image_path(), parent() ) -> basic_splash_info().
create_basic( ImgPath, Parent ) ->
	create_basic( ImgPath, _ScaleFactor=1, Parent ).




% @doc Creates and shows a basic splash screen displaying the specified image
% once scaled (uniformously) at the specified factor (typically in ]0,1[ to
% shrink a too large image) on a minimalist frame on top of the specified parent
% (typically a frame, probably the main one), and returns the associated splash
% information, for future use (event management).
%
% The splash screen will be dismissed when the application will call the
% remove/1 function.
%
-spec create_basic( any_image_path(), scale_factor(), parent() ) ->
											basic_splash_info().
create_basic( ImgPath, ScaleF, Parent ) ->

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt( "Creating a splash screen, to display '~ts', "
			"with scale ~w, and parent ~w.", [ ImgPath, ScaleF, Parent ] ) ),

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
	NoId = undefined,

	% The image bitmap, kept once for all to regenerate the backbuffer as
	% needed:
	%
	ImgBitmap = gui_image:to_bitmap( Img ),

	ImgBitmapSize = gui_bitmap:get_size( ImgBitmap ),

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt( "Image original size: ~w pixels, "
			"scaled down for bitmap: ~w.", [ NativeImgSize, ImgBitmapSize ] ) ),

	% The splash frame will directly have its client area set accordingly:
	%SplashFrameSize = { ScaledImgWidth + 15, ScaledImgHeight + 15 },
	SplashFrameSize = auto,

	% As minimal as possible:
	SplashStyles = [ no_border, no_taskbar, float_on_parent ],

	% A mere window would not be sufficient; title useless:
	SplashFrame = gui_frame:create( _STitle="Myriad Splash Screen", Pos,
		SplashFrameSize, SplashStyles, NoId, Parent ),

	% (should no size be specified, would be 20x20 initially)
	SplashPanel = gui_panel:create( Pos, ImgBitmapSize, SplashFrame ),

	% To be compared afterwards with bitmap size; even if starting with a
	% correct, sufficient size, may by default be immediately shrunk vertically
	% afterwards:
	%
	SplashPanelSize = gui_panel:get_size( SplashPanel ),

	cond_utils:if_defined( myriad_debug_gui_splash, trace_utils:debug_fmt(
		"Initial splash panel size: ~w.", [ SplashPanelSize ] ) ),

	% Force correct size, no shrinking wanted:
	gui_widget:set_client_size( SplashFrame, SplashPanelSize ),

	% The backbuffer on which the panel content will be drawn:
	BackbufferBitmap = gui_bitmap:create_empty_for( SplashPanel ),

	% Initialisation:
	render_splash( SplashPanel, BackbufferBitmap, ImgBitmap ),

	% Both can happen, separately (just a repaint, e.g. after having been masked
	% by another window) or not (resize+repaint):
	%
	gui:subscribe_to_events( [ { onResized, SplashPanel },
							   { onRepaintNeeded, SplashPanel } ] ),

	% Would be too early (main frame shall be shown first):
	%gui_frame:show( SplashFrame ),

	#basic_splash_info{ splash_frame=SplashFrame,
						splash_panel=SplashPanel,
						backbuffer=BackbufferBitmap,
						image_bitmap=ImgBitmap }.


% @doc Shows the corresponding splash screen.
%
% Must be called only after that the parent frame is shown, otherwise the splash
% screen will not be centered in the parent, but on the whole screen, which
% generally is not desirable.
%
-spec show( splash_info() ) -> void().
show( #basic_splash_info{ splash_frame=SplashFrame } ) ->
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
			"Splash panel '~ts' needs to be repainted.",
			[ gui:object_to_string( SplashPanel ) ] ) ),

	% No size change, backbuffer still legit:
	%render_splash( SplashPanel, BackbufferBitmap, ImgBitmap ).
	update_panel( SplashPanel, BackbufferBitmap ).



% @doc Callback to be triggered whenever the application catches a onResized
% event for the splash panel.
%
-spec on_resized( panel(), size(), splash_info() ) -> splash_info().
on_resized( SplashPanel, NewSize, SplashInfo=#basic_splash_info{
		backbuffer=BackbufferBitmap, % } ) ->
		image_bitmap=ImgBitmap } ) ->

	cond_utils:if_defined( myriad_debug_gui_splash,
		trace_utils:debug_fmt( "Splash panel '~ts' resized to ~p.",
			[ gui:object_to_string( SplashPanel ), NewSize ] ) ),

	% We have to resize the framebuffer first:
	NewBackbufferBitmap = gui_bitmap:create_empty( NewSize ),

	% A new backbuffer is used, the image must be copied on it (update_panel/2
	% would not suffice):
	%
	render_splash( SplashPanel, NewBackbufferBitmap, ImgBitmap ),

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
	SplashPanel.


% @doc Returns the splash frame, as stored in the specified splash information.
-spec get_frame( splash_info() ) -> frame().
get_frame( #basic_splash_info{ splash_frame=SplashFrame } ) ->
	SplashFrame.


% @doc Destructs the splash screen, based on the specified splash information.
-spec destruct( splash_info() ) -> void().
destruct( #basic_splash_info{ splash_frame=SplashFrame,
							  %splash_panel=SplashPanel,
							  backbuffer=BackBufferBitmap,
							  image_bitmap=ImgBitmap } ) ->
	% Implies panel:
	gui_frame:destruct( SplashFrame ),

	[ gui_bitmap:destruct( B ) || B <- [ BackBufferBitmap, ImgBitmap ] ].



% Internal helpers.


% @doc Renders the splash screen: updates the (bitmap) backbuffer accordingly,
% and blits it to the specified panel.
%
-spec render_splash( splash_panel(), bitmap(), bitmap() ) -> void().
render_splash( TargetPanel, BackbufferBitmap, ImageBitmap ) ->

	% Note: the image could be copied directly onto the panel, yet preparing a
	% backbuffer will allow to easily and efficiently blit it afterwards to said
	% panel.

	% Updates the backbuffer with the stored image:

	% Locks the target surface (device context):
	BackbufferDC = gui_bitmap:lock( BackbufferBitmap ),

	gui_render:clear_device_context( BackbufferDC ),

	TopLeftPos = {0,0},

	% Just copies the image bitmap onto the backbuffer:
	gui_bitmap:draw( _Source=ImageBitmap, _Target=BackbufferDC,
					 _PosInTarget=TopLeftPos ),

	% Then blits this updated backbuffer to the panel:

	TargetPanelDC = gui_widget:lock( TargetPanel ),

	gui_render:blit( _From=BackbufferDC, _FromPos=TopLeftPos,
		_BlitArea=gui_bitmap:get_size( BackbufferBitmap ),
		_To=TargetPanelDC, _ToPos=TopLeftPos ),

	gui_widget:unlock( TargetPanelDC ),

	gui_bitmap:unlock( BackbufferDC ).


% @doc Blits the current backbuffer bitmap to the specified panel, once cleared.
-spec update_panel( splash_panel(), bitmap() ) -> void().
update_panel( TargetPanel, BackbufferBitmap ) ->

	% No need to update the update the framebuffer.

	% Locks the target surface (device context):
	BackbufferDC = gui_bitmap:lock( BackbufferBitmap ),

	% Then blits this updated backbuffer to the panel:
	TopLeftPos = {0,0},

	TargetPanelDC = gui_widget:lock( TargetPanel ),

	gui_render:blit( _From=BackbufferDC, _FromPos=TopLeftPos,
		_BlitArea=gui_bitmap:get_size( BackbufferBitmap ),
		_To=TargetPanelDC, _ToPos=TopLeftPos ),

	gui_widget:unlock( TargetPanelDC ),

	gui_bitmap:unlock( BackbufferDC ).
