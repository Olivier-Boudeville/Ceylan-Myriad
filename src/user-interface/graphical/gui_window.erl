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
% Creation date: Thursday, August 31, 2023.


% @doc Gathering of various facilities for <b>windows</b>.
%
% A window may be a top-level of not, a frame, a splitter window, etc.
%
% A window is a special case of widget, which is the most general form of
% graphical component.
%
% See also the gui_window_manager module regarding the insertion of windows in
% their environment.
%
-module(gui_window).




-type window() :: gui_widget:widget().
% Any kind of window.
%
% Base, most general class for all windows (for example, a frame is a window
% whose size and position can usually be changed by the user).
%
% This corresponds to "real" windows - not to any widget, as wx/WxWidgets call
% windows.

-type window_option() :: { 'position', point() }
					   | { 'size', size() }
					   | { 'style', [ window_style() ] }.
% Window-specific options (quite common).


-type window_style() ::
	'default_border'
  | 'simple_border'
  | 'sunken_border'
  | 'raised_border'
  | 'static_border'
  | 'theme_border'
  | 'no_border'
  | 'double_border'
  | 'transparent' % Windows-only
  | 'tab_traversable'
  | 'grab_all_keys'
  | 'with_vertical_scrollbar'
  | 'with_horizontal_scrollbar'
  | 'never_hide_scrollbars'
  | 'clip_children'
  | 'full_repaint_on_resize'.
% A style element of a window.
%
% See also [http://docs.wxwidgets.org/stable/classwx_window.html]


-export_type([ window/0, window_option/0, window_style/0 ]).



-type icon_name_id() :: standard_icon_name_id() | id().
% The identifier of an icon.


-type standard_icon_name_id() ::
	'asterisk_icon' | 'stop_icon' | 'information_icon'
  | 'question_icon' | 'error_icon' | 'warning_icon' | 'hand_icon'
  | 'exclamation_icon'.
% The name identifiers of the standard icons.


-export_type([ icon_name_id/0, standard_icon_name_id/0 ]).



-type top_level_window() :: wxTopLevelWindow:wxTopLevelWindow().
% A top-level (application-wide) window.
%
% The top-level window is a base class common to frames and dialogs; so such a
% window is typically a frame (including any main one) or a dialog.


-export_type([ top_level_window/0 ]).


% At least for the splitter record:
-include("gui_base.hrl").

-type splitter() :: #splitter{}.
% Represents a window able to be split into two panes.
%
% Information regarding the (fixed, static) horizontal or vertical splitting of
% a window into two ones.


-type splitter_window() :: wxSplitterWindow:wxSplitterWindow().
% A window able to be split into two panes; it may thus manage up to two
% subwindows.


-type sash_gravity() :: number().
% Tells how much the first pane of a splitter window is to grow while resizing
% it:
%  - 0.0: only the bottom/right window is automatically resized
%  - 0.5: both windows grow by equal size
%  - 1.0: only left/top window grows
%
% Gravity should be a value between 0.0 and 1.0; its default value is 0.0.


-export_type([ splitter/0, splitter_window/0, sash_gravity/0 ]).



-opaque frame() :: wxFrame:wxFrame().
% A frame is a window whose size and position can usually be changed by the
% user.
%
% Note that a frame is a top_level_window(), a window() and an event_handler(),
% and thus can use their methods.
%
% At least on some platforms, while initialising a frame, an older graphical
% content can be seen for a short while, before the first repaint. No amount of
% wxWindow:{clearBackground,refresh,update}/1 or buffer swapping was able to
% hide it.


-type top_level_frame() :: frame().
% A top-level (application-wide) frame.


% 'iconized' not kept (duplicate of 'minimize').
-type frame_style() ::

	% Corresponds to the following options: minimize_icon, maximize_icon,
	% resize_border, system_menu, caption, close_icon and clip_children.
	%
	'default'

	% Displays a caption on the title bar of this frame (needed for icons).
  | 'caption'

	% Displays a minimize icon on the title bar of this frame.
  | 'minimize_icon'

	% Displays a maximize icon on the title bar of this frame.
  | 'maximize_icon'

	% Displays a close icon on the title bar of this frame.
  | 'close_icon'

	% Stays on top of all other windows.
  | 'stay_on_top'

	% Displays a system menu containing the list of various windows commands
	% in the window title bar.
	%
  | 'system_menu'

	% Displays a resizable border around the window.
  | 'resize_border'

	% This frame will have a small title bar:
  | 'tool_window'

	% Requests that this frame does not appear in the taskbar:
  | 'no_taskbar'

	% Stays on top of (only) its parent:
  | 'float_on_parent'

	% Allows this frame to have its shape changed:
  | 'shaped'.
% A style element for frames.
%
% Note that specifying an empty option list does not enable any option,; one may
% use 'default' instead.
%
% See also [http://docs.wxwidgets.org/stable/classwx_frame.html].


-export_type([ frame/0, top_level_frame/0, frame_style/0 ]).



% For standard, basic windows:
-export([ create/0, create/1, create/2, create/5,
		  destruct/1 ]).


% For splitter windows:
-export([ create_splitter/4, create_splitter/5, set_unique_pane/2 ]).


% For any kind of window:
-export([ show/1, hide/1 ]).


% Implementation notes:
%
% The wx backend names the concept of widget "window", which is a bit
% restrictive (a window is seen at least here in MyriadGUI as a special case of
% widget).


% For ?gui_any_id:
-include("gui_internal_defines.hrl").


% Shorthands:

-type any_file_path() :: file_utils:any_file_path().

-type point() :: gui:point().
-type size() :: gui:size().
-type position() :: gui:position().
-type orientation() :: orientation(). 
-type parent() :: gui:parent().
-type title() :: gui:title().

-type id() :: gui_id:id().



% Subsection for standard, basic windows.


% @doc Creates a basic window.
-spec create() -> window().
create() ->
	wxWindow:new().



% @doc Creates a basic window having the specified identifier.
%
% @hidden (internal use only)
%
-spec create( id(), parent() ) -> window().
create( Id, Parent ) ->

	ActualId = gui_id:declare_any_id( Id ),

	% Should not be 'undefined', otherwise: "wxWidgets Assert failure:
	% ./src/gtk/window.cpp(2586): \"parent\" in PreCreation() : Must have
	% non-NULL parent"}
	%
	ActualParent = gui_wx_backend:to_wx_parent( Parent ),

	wxWindow:new( ActualParent, ActualId ).



% @doc Creates a basic window of the specified size.
-spec create( size() ) -> window().
create( Size ) ->

	ActualId = gui_id:declare_any_id( undefined ),
	ActualParent = gui_wx_backend:to_wx_parent( undefined ),

	WxOpts = [ gui_wx_backend:to_wx_size( Size ) ],

	wxWindow:new( ActualParent, ActualId, WxOpts ).



% @doc Creates a basic window from the specified settings.
%
% @hidden (internal use only)
%
-spec create( position(), size(), window_style(), id(), parent() ) ->
											window().
create( Position, Size, Style, Id, Parent ) ->

	WxOpts = [ gui_wx_backend:to_wx_position( Position ),
			   gui_wx_backend:to_wx_size( Size ),
			   { style, gui_wx_backend:window_style_to_bitmask( Style ) } ],

	ActualId = gui_id:declare_any_id( Id ),
	ActualParent = gui_wx_backend:to_wx_parent( Parent ),

	wxWindow:new( ActualParent, ActualId, WxOpts ).


% @doc Destructs the specified window.
-spec destruct( window() ) -> void().
destruct( Window ) ->
	wxWindow:destroy( Window ).



% @doc Shows (renders) the specified window (or subclass thereof).
%
% Returns whether anything had to be done.
%
% This is the place where all widgets resolve their positions, sizes and
% contents.
%
-spec show( window() | [ window() ] ) -> boolean().
show( Windows ) when is_list( Windows )->

	% Note: onShown used to be sent to the MyriadGUI loop, as some widgets had
	% to be adjusted then, but it is no longer useful.

	%trace_utils:debug_fmt( "Showing windows ~p.", [ Windows ] ),
	Res = show_helper( Windows, _Acc=false ),
	%get_main_loop_pid() ! { onShown, [ Windows ] },

	show_fix(),

	Res;

show( Window ) ->
	%trace_utils:debug_fmt( "Showing window ~p.", [ Window ] ),
	Res = wxWindow:show( Window ),
	%get_main_loop_pid() ! { onShown, [ [ Window ] ] },
	show_fix(),

	Res.


% (helper)
show_helper( _Windows=[], Acc ) ->
	show_fix(),
	Acc;

show_helper( _Windows=[ W | T ], Acc ) ->
	NewAcc = wxWindow:show( W ) orelse Acc,
	show_helper( T, NewAcc ).


% This is certainly a strange fix. It was observed with gui_image_test.erl that
% the image was initially displayed iff such a sleep was following
% 'gui:show(MainFrame)' - although there was no next rendering operation (a
% receive blocking until the user closes the window). Otherwise the panel
% remained blank until the frame was redrawn for any reason (e.g. resize).
%
show_fix() ->
	timer:sleep( 10 ).
	%ok.


% @doc Hides the specified window.
%
% Returns whether anything had to be done.
%
-spec hide( window() ) -> boolean().
hide( Window ) ->
	wxWindow:show( Window, [ { show, false } ] ).




% Top-level window subsection.


% @doc Sets the title of the specified top-level window.
-spec set_title( top_level_window(), title() ) -> void().
set_title( TopLevelWindow, Title ) ->
	wxTopLevelWindow:setTitle( TopLevelWindow, Title ).


% @doc Returns the title of the specified top-level window.
-spec get_title( top_level_window() ) -> title().
get_title( TopLevelWindow ) ->
	wxTopLevelWindow:getTitle( TopLevelWindow ).


% @doc Sets the icon of the specified top-level window.
-spec set_icon( top_level_window(), any_file_path() ) -> void().
set_icon( TopLvlWin, IconPath ) ->

	% Supported image formats documented as being only BMP by default, yet test
	% on PNG succeeded.

	% Current no wx_image:initAllImageHandlers/* (for other formats than BMP),
	% just wx_image:initStandardHandlers/0.

	% Apparently 'Icon = wxIcon:new(IconPath),' could have sufficed:
	Img = wxImage:new( IconPath ),
	Bitmap = wxBitmap:new( Img ),
	Icon = wxIcon:new(),
	wxIcon:copyFromBitmap( Icon, Bitmap ),

	wxTopLevelWindow:setIcon( TopLvlWin, Icon ).


% @doc Centers the specified top-level window on screen.
-spec center_on_screen( top_level_window() ) -> void().
center_on_screen( TopLvlWin ) ->
	wxTopLevelWindow:centerOnScreen( TopLvlWin ).


% @doc Centers the specified top-level window on screen, along the specified
% orientation(s).
%
-spec center_on_screen( top_level_window(), orientation() ) -> void().
center_on_screen( TopLvlWin, Orientation ) ->
	wxTopLevelWindow:centerOnScreen( TopLvlWin,
		gui_wx_backend:to_wx_orientation( Orientation ) ).


% @doc Tells whether the specified top-level window is maximised.
-spec is_maximised( top_level_window() ) -> boolean().
is_maximised( TopLevelWindow ) ->
	wxTopLevelWindow:isMaximized( TopLevelWindow ).


% @doc Maximises the specified top-level window.
-spec maximize( top_level_window() ) -> void().
maximize( TopLevelWindow ) ->
	wxTopLevelWindow:maximize( TopLevelWindow ).



% @doc Returns whether the specified top-level window is fullscreen.
-spec is_fullscreen( top_level_window() ) -> boolean().
is_fullscreen( TopLvlWin ) ->
	wxTopLevelWindow:isFullScreen( TopLvlWin ).


% @doc Shows the specified top-level window to fullscreen (if true) or restores
% it to its normal state (if false).
%
% Showing a window full screen also actually shows the window if it is not
% already shown. Any menu bar is then hidden.
%
% Returns (supposedly) whether the operation succeeded.
%
-spec set_fullscreen( top_level_window(), boolean() ) -> void().
set_fullscreen( TopLvlWin, ForceFullscreen ) ->
	wxTopLevelWindow:showFullScreen( TopLvlWin, ForceFullscreen ).


% @doc Returns whether the specified top-level window is currently active, that
% is if the user is currently interacting with it.
%
-spec is_active( top_level_window() ) -> boolean().
is_active( TopLvlWin ) ->
	wxTopLevelWindow:isActive( TopLvlWin ).




% Frame subsection.
%
% A frame is a window whose size and position can (usually) be changed by the
% user. It usually has thick borders and a title bar, and can optionally contain
% a menu bar, toolbar and status bar. A frame can contain any window that is not
% a frame or a dialog.
%
% Source: http://docs.wxwidgets.org/stable/classwx_frame.html

% An application has generally exactly one top-level frame. Creating such kind
% of frame allows to record it, and then the window management services are able
% to tell whether for example the application as a whole shall be considered as
% maximised.



% @doc Creates a top-level frame, with default position, size, style and
% identifier.
%
-spec create_top_level_frame( title() ) -> frame().
create_top_level_frame( Title ) ->
	Frame = create_frame( Title ),
	record_top_level_window( Frame ),
	Frame.



% @doc Creates a top-level frame, with the specified size, and a default
% identifier.
%
-spec create_top_level_frame( title(), size() ) -> frame().
create_top_level_frame( Title, Size ) ->
	Frame = create_frame( Title, Size ),
	record_top_level_window( Frame ),
	Frame.



% @doc Creates a top-level frame, with the specified title, position, size and
% style.
%
-spec create_top_level_frame( title(), position(), size(), frame_style() ) ->
												frame().
create_top_level_frame( Title, Position, Size, Style ) ->
	Frame = create_frame( Title, Position, Size, Style ),
	record_top_level_window( Frame ),
	Frame.



% @doc Creates a frame, with default title, identifier, parent, position, size
% and style.
%
% Note: this version apparently does not correctly initialise the frame;
% following error is indeed reported:
% "wxWidgets Assert failure: ./src/gtk/toplevel.cpp(988): \"m_widget\" in Show()
% : invalid frame".
%
-spec create_frame() -> frame().
create_frame() ->
	% We could see a case where a call to wxFrame:new/0 issued by an helper
	% spawned process (having set its environment) would trigger a segmentation
	% fault, whereas wxFrame:new(wx:null(), ?wxID_ANY, "Hello") worked
	% flawlessly:
	%
	wxFrame:new().


% @doc Creates a titled frame, with default position, size, style, identifier
% and parent.
%
-spec create_frame( title() ) -> frame().
create_frame( Title ) ->
	wxFrame:new( gui_wx_backend:to_wx_parent( undefined ), ?gui_any_id, Title ).



% @doc Creates a frame, with the specified title and size, and default
% identifier and parent.
%
-spec create_frame( title(), size() ) -> frame().
create_frame( Title, Size ) ->

	WxOpts = [ gui_wx_backend:to_wx_size( Size ) ],

	%trace_utils:debug_fmt( "create_frame options: ~p.", [ WxOpts ] ),

	wxFrame:new( gui_wx_backend:to_wx_parent( undefined ),
				 gui_id:declare_any_id( undefined ), Title, WxOpts ).



% @doc Creates a frame, with default position, size and style.
%
% (internal use only)
%
-spec create_frame( title(), id(), maybe( parent() ) ) -> frame().
create_frame( Title, Id, Parent ) ->
	wxFrame:new( gui_wx_backend:to_wx_parent( Parent ),
				 gui_id:declare_any_id( Id ), Title ).



% @doc Creates a frame, with the specified title, position, size and style, and
% with a default parent.
%
-spec create_frame( title(), position(), size(), frame_style() ) -> frame().
create_frame( Title, Position, Size, Style ) ->

	WxOpts = [ gui_wx_backend:to_wx_position( Position ),
				gui_wx_backend:to_wx_size( Size ),
				{ style, frame_style_to_bitmask( Style ) } ],

	%trace_utils:debug_fmt( "create_frame options: ~p.", [ WxOpts ] ),

	wxFrame:new( gui_wx_backend:to_wx_parent( undefined ),
				 gui_id:declare_any_id( undefined ), Title, WxOpts ).



% @doc Creates a frame, with the specified title, position, size and style, and
% with a default parent.
%
% (internal use only: wx exposed)
%
-spec create_frame( title(), position(), size(), frame_style(), id(),
					parent() ) -> frame().
create_frame( Title, Position, Size, Style, Id, Parent ) ->

	WxOpts = [ gui_wx_backend:to_wx_position( Position ),
				gui_wx_backend:to_wx_size( Size ),
				{ style, frame_style_to_bitmask( Style ) } ],

	ActualId = gui_id:declare_any_id( Id ),

	ActualParent = gui_wx_backend:to_wx_parent( Parent ),

	wxFrame:new( ActualParent, ActualId, Title, WxOpts ).



% @doc Destructs the specified frame.
-spec destruct_frame( frame() ) -> void().
destruct_frame( Frame  ) ->
	wxFrame:destroy( Frame ).



% Records the specified window as the application top-level one, in the
% MyriadGUI environment.
%
% (helper)
%
-spec record_top_level_window( top_level_window() ) -> void().
record_top_level_window( TopLvlWindow ) ->
	environment:set( _K=top_level_window, _V=TopLvlWindow,
					 _Designator=?gui_env_reg_name ).




% Splitter subsection.
%
% Note that these functions handle splitter() instances - not any form of
% splitter_window().


% @doc Creates a splitter of the specified orientation, in the specified window,
% based on the specified sash gravity and pane size, returning a corresponding
% splitter record so that the up to two subwindows can be declared afterwards.
%
-spec create_splitter( window(), orientation(), sash_gravity(), size() ) ->
								splitter().
create_splitter( ParentWindow, Orientation, SashGravity, PaneSize ) ->
	create_splitter( ParentWindow, Orientation, SashGravity, PaneSize,
					 system_utils:get_operating_system_type() ).


% @doc Creates a splitter of the specified orientation, in the specified window,
% based on the specified sash gravity and pane size, according to the specified
% OS, returning a corresponding splitter record so that the up to two subwindows
% can be declared afterwards.
%
-spec create_splitter( window(), orientation(), sash_gravity(), size(),
					   os_type() ) -> splitter().
create_splitter( ParentWindow, Orientation, SashGravity, PaneSize, OSType ) ->

	WxStyle = case OSType of

		{ _OSFamily=unix, _OSName=darwin } ->
			?wxSP_LIVE_UPDATE bor ?wxSP_3DSASH;

		{ win32, _ } ->
			?wxSP_LIVE_UPDATE bor ?wxSP_BORDER;

		_ ->
			?wxSP_LIVE_UPDATE bor ?wxSP_3D

		end,

	SplitterWin = wxSplitterWindow:new( ParentWindow, [ { style, WxStyle } ] ),

	wxSplitterWindow:setSashGravity( SplitterWin, SashGravity ),

	wxSplitterWindow:setMinimumPaneSize( SplitterWin, PaneSize ),

	#splitter{ splitter_window=SplitterWin,
			   orientation=gui:check_orientation( Orientation ) }.


% @doc Sets the specified splitter in a single pane configuration, using for
% that the specified window.
%
-spec set_unique_pane( splitter(), parent() ) -> void().
set_unique_pane( #splitter{ splitter_window=SplitterWin }, WindowPane ) ->
	wxSplitterWindow:initialize( SplitterWin, WindowPane ).



% Helper section.


% Records, in the MyriadGUI environment, the specified window (typically a
% frame) as the application top-level window.
%
% (helper)
%
-spec record_as_top_level( window() ) -> void().
record_as_top_level( Window ) ->
	environment:set( _K=top_level_window, _V=Window,
					 _Designator=?gui_env_reg_name ).
