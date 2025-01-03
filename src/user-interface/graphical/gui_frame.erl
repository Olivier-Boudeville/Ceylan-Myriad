% Copyright (C) 2023-2025 Olivier Boudeville
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
% Creation date: Sunday, September 10, 2023.

-module(gui_frame).

-moduledoc """
Gathering of various facilities for **frames**, which are windows whose size and
position can usually be changed by the user.

See also the gui_window module.
""".


% Usage notes:
%
% A frame is a window whose size and position can (usually) be changed by the
% user. It usually has thick borders and a title bar, and can optionally contain
% a menu bar, toolbar and status bar. A frame can contain any window that is not
% a frame or a dialog.
%
% Source: http://docs.wxwidgets.org/stable/classwx_frame.html
%
% An application has generally exactly one top-level frame. Creating such kind
% of frame allows to record it, and then the window management services are able
% to tell whether for example the application as a whole shall be considered as
% maximised.



-doc """
A frame is a window whose size and position can usually be changed by the user.

Note that a frame is a top_level_window(), a window() and an event_handler(),
and thus can use their methods.

At least on some platforms, while initialising a frame, an older graphical
content can be seen for a short while, before the first repaint. No amount of
wxWindow:{clearBackground,refresh,update}/1 or buffer swapping was able to hide
it.
""".
-opaque frame() :: wxFrame:wxFrame().



-doc "A top-level (application-wide) frame.".
-opaque top_level_frame() :: frame().



% 'iconized' not kept (duplicate of 'minimize').
-doc """
A style element for frames.

Note that specifying an empty option list does not enable any option; one may
specify 'default' instead.

See also <http://docs.wxwidgets.org/stable/classwx_frame.html>.
""".
-type frame_style() ::

	window_style()

	% Corresponds to the following options: minimize_icon, maximize_icon,
	% resize_border, system_menu, caption, close_icon and clip_children:
	%
  | 'default'

	% Displays a caption on the title bar of this frame (needed for icons):
  | 'caption'

	% Displays a minimize icon on the title bar of this frame:
  | 'minimize_icon'

	% Displays a maximize icon on the title bar of this frame:
  | 'maximize_icon'

	% Displays a close icon on the title bar of this frame:
  | 'close_icon'

	% Stays on top of all other windows (see also 'float_on_parent'):
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

	% Stays on top of (only) its parent (see also 'stay_on_top'):
  | 'float_on_parent'

	% Allows this frame to have its shape changed:
  | 'shaped'.


-export_type([ frame/0, top_level_frame/0, frame_style/0 ]).


% For frames:
-export([ create/0, create/1, create/2, create/3, create/4, create/6,
		  destruct/1,
		  set_menu_bar/2, show/1 ]).


% For top-level frames:
-export([ create_top_level/1, create_top_level/2, create_top_level/4,
		  set_icon/2, center_on_screen/1, center_on_screen/2,
		  is_maximised/1, maximize/1,
		  is_fullscreen/1, set_fullscreen/2 ]).




% For ?gui_any_id:
-include("gui_internal_defines.hrl").


% Type shorthands:

-type bit_mask() :: type_utils:bit_mask().

-type any_file_path() :: file_utils:any_file_path().

-type size() :: gui:size().
-type sizing() :: gui:sizing().
-type position() :: gui:position().
-type orientation() :: gui:orientation().
-type parent() :: gui:parent().
-type title() :: gui:title().

-type window_style() :: gui_window:window_style().
-type menu_bar() :: gui_menu:menu_bar().

-type id() :: gui_id:id().



% Section for all kinds of frames.


-doc """
Creates a frame, with default title, identifier, parent, position, size and
style.

Note: this version apparently does not correctly initialise the frame; following
error is indeed reported: "wxWidgets Assert failure:
./src/gtk/toplevel.cpp(988): \"m_widget\" in Show() : invalid frame".
""".
-spec create() -> frame().
create() ->
	% We could see a case where a call to wxFrame:new/0 issued by an helper
	% spawned process (having set its environment) would trigger a segmentation
	% fault, whereas wxFrame:new(wx:null(), ?wxID_ANY, "Hello") worked
	% flawlessly:
	%
	wxFrame:new().



-doc """
Creates a titled frame, with default position, size, style, identifier and
parent.
""".
-spec create( title() ) -> frame().
create( Title ) ->
	wxFrame:new( gui_wx_backend:to_wx_parent( undefined ), ?gui_any_id, Title ).



-doc """
Creates a frame, with the specified title and size, and default identifier and
parent.
""".
-spec create( title(), size() ) -> frame().
create( Title, Size ) ->

	WxOpts = [ gui_wx_backend:to_wx_size( Size ) ],

	%trace_utils:debug_fmt( "create_frame options: ~p.", [ WxOpts ] ),

	wxFrame:new( gui_wx_backend:to_wx_parent( undefined ),
				 gui_id:declare_any_id( undefined ), Title, WxOpts ).



-doc "Creates a frame, with default position, size and styles.".
-spec create( title(), id(), option( parent() ) ) -> frame().
create( Title, Id, MaybeParent ) ->
	wxFrame:new( gui_wx_backend:to_wx_parent( MaybeParent ),
				 gui_id:declare_any_id( Id ), Title ).



-doc """
Creates a frame, with the specified title, position, size and styles, and with a
default parent.
""".
-spec create( title(), position(), sizing(), [ frame_style() ] ) -> frame().
create( Title, Position, Sizing, Styles ) ->

	WxOpts = [ gui_wx_backend:to_wx_position( Position ),
				gui_wx_backend:to_wx_size( Sizing ),
				{ style, frame_styles_to_bitmask( Styles ) } ],

	%trace_utils:debug_fmt( "Creating a frame with WxOpts = ~w "
	%                       "(styles: ~w).", [ WxOpts, Styles ] ),

	wxFrame:new( gui_wx_backend:to_wx_parent( undefined ),
				 gui_id:declare_any_id( undefined ), Title, WxOpts ).



-doc """
Creates a frame, with the specified title, position, size and styles, and with
the specified parent.
""".
-spec create( title(), position(), sizing(), [ frame_style() ], id(),
			  option( parent() ) ) -> frame().
create( Title, Position, Sizing, Styles, Id, MaybeParent ) ->

	WxOpts = [ gui_wx_backend:to_wx_position( Position ),
			   gui_wx_backend:to_wx_size( Sizing ),
			   { style, frame_styles_to_bitmask( Styles ) } ],

	%trace_utils:debug_fmt( "Creating a frame with WxOpts = ~w "
	%                       "(styles: ~w).", [ WxOpts, Styles ] ),

	ActualId = gui_id:declare_any_id( Id ),

	ActualParent = gui_wx_backend:to_wx_parent( MaybeParent ),

	wxFrame:new( ActualParent, ActualId, Title, WxOpts ).



-doc "Destructs the specified frame.".
-spec destruct( frame() ) -> void().
destruct( Frame  ) ->
	wxFrame:destroy( Frame ).



-doc "Assigns the specified menu bar to the specified frame.".
-spec set_menu_bar( frame(), menu_bar() ) -> void().
set_menu_bar( Frame, MenuBar ) ->
	wxFrame:setMenuBar( Frame, MenuBar ).



-doc """
Shows (renders) the specified frame.

Returns whether anything had to be done.

This is the place where all widgets resolve their positions, sizes and contents.
""".
-spec show( frame() | [ frame() ] ) -> boolean().
show( FrameMaybeList ) ->
	gui_window:show( FrameMaybeList ).



-doc """
Converts the specified MyriadGUI frame style into the appropriate wx-specific
bit mask.

(helper)
""".
-spec frame_styles_to_bitmask( [ frame_style() ] ) -> bit_mask().
frame_styles_to_bitmask( Styles ) when is_list( Styles ) ->
	% 'bor ?wxWANTS_CHARS' not desirable a priori:
	lists:foldl( fun( S, Acc ) ->
					gui_generated:get_second_for_frame_style( S ) bor Acc
				 end,
				 _InitialAcc=0,
				 _List=Styles ).



% Top-level frame section.


-doc """
Creates a top-level frame, with default position, size, style and identifier.
""".
-spec create_top_level( title() ) -> top_level_frame().
create_top_level( Title ) ->
	Frame = create( Title ),
	gui_window:record_as_top_level( Frame ),
	Frame.



-doc """
Creates a top-level frame, with the specified size, and a default identifier.
""".
-spec create_top_level( title(), size() ) -> top_level_frame().
create_top_level( Title, Size ) ->
	Frame = create( Title, Size ),
	gui_window:record_as_top_level( Frame ),
	Frame.



-doc """
Creates a top-level frame, with the specified title, position, size and style.
""".
-spec create_top_level( title(), position(), size(), frame_style() ) ->
												top_level_frame().
create_top_level( Title, Position, Size, Style ) ->
	Frame = create( Title, Position, Size, Style ),
	gui_window:record_as_top_level( Frame ),
	Frame.



-doc "Sets the icon of the specified top-level frame.".
-spec set_icon( top_level_frame(), any_file_path() ) -> void().
set_icon( TopLevelFrame, IconPath ) ->
	gui_window:set_icon( TopLevelFrame, IconPath ).



-doc "Centers the specified top-level frame on screen.".
-spec center_on_screen( top_level_frame() ) -> void().
center_on_screen( TopLevelFrame ) ->
	gui_window:center_on_screen( TopLevelFrame ).



-doc """
Centers the specified top-level frame on screen, along the specified
orientation(s).
""".
-spec center_on_screen( top_level_frame(), orientation() ) -> void().
center_on_screen( TopLevelFrame, Orientation ) ->
	gui_window:center_on_screen( TopLevelFrame, Orientation ).



-doc "Tells whether the specified top-level frame is maximised.".
-spec is_maximised( top_level_frame() ) -> boolean().
is_maximised( TopLevelFrame ) ->
	gui_window:is_maximised( TopLevelFrame ).



-doc "Maximises the specified top-level frame.".
-spec maximize( top_level_frame() ) -> void().
maximize( TopLevelFrame ) ->
	gui_window:maximize( TopLevelFrame ).



-doc "Returns whether the specified top-level frame is fullscreen.".
-spec is_fullscreen( top_level_frame() ) -> boolean().
is_fullscreen( TopLvlFrame ) ->
	wxTopLevelWindow:isFullScreen( TopLvlFrame ).



-doc """
Shows the specified top-level frame to fullscreen (if true) or restores it to
its normal state (if false).

Showing a frame full screen also actually shows the frame if it is not already
shown. Any menu bar is then hidden.

Returns (supposedly) whether the operation succeeded.
""".
-spec set_fullscreen( top_level_frame(), boolean() ) -> void().
set_fullscreen( TopLvlFrame, ForceFullscreen ) ->
	wxTopLevelWindow:showFullScreen( TopLvlFrame, ForceFullscreen ).
