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
% Creation date: Saturday, September 2, 2023.


% @doc Gathering of various facilities for <b>toolbar management</b>. A tool bar
% is a bar of buttons and/or other controls usually placed below the menu bar in
% a frame.
%
% Use our gui_frame_bars_test.erl test in order to display all known tools.
%
-module(gui_toolbar).


-opaque toolbar() :: wxToolBar:wxToolBar().
% A bar of image-based buttons and/or other controls usually placed below the
% menu bar of a frame.
%
% A toolbar emits menu commands in the same way that a frame menubar does.


-type toolbar_style() ::
	'top'
  | 'bottom'
  | 'left'
  | 'right'
  | 'flat'
  | 'dockable'
  | 'no_icons'
  | 'text'
  | 'no_divider'
  | 'no_align'
  | 'horizontal_layout'
  | 'no_tooltips'
  | 'default'.
% A style element of a toolbar.


-type tool() :: wx_object().
% A tool, as an element of a toolbar.


% Corresponds to menu items:
-type tool_kind() :: menu_item_kind().
% The kind of a tool in a toolbar.


-export_type([ toolbar/0, toolbar_style/0, tool/0, tool_kind/0 ]).


-export([]).


% Shorthands:

-type wx_object() :: gui:wx_object().




% @doc Creates a toolbar in the specified frame.
-spec create_toolbar( frame() ) -> toolbar().
create_toolbar( Frame ) ->
	wxFrame:createToolBar( Frame ).


% @doc Creates a toolbar in the specified frame, with the specified identifier
% (if any) and style.
%
% Apparently up to one toolbar can be associated to a frame (e.g. no top and
% left toolbars allowed simultaneously).
%
-spec create_toolbar( frame(), id(), [ toolbar_style() ] ) -> toolbar().
create_toolbar( Frame, Id, MaybeToolbarStyles ) ->
	wxFrame:createToolBar( Frame, [ { id, gui_id:declare_any_id( Id ) },
		{ style, gui_wx_backend:to_wx_toolbar_style( MaybeToolbarStyles ) } ] ).



% @doc Sets the specified toolbar in the specified frame.
-spec set_toolbar( frame(), toolbar() ) -> void().
set_toolbar( Frame, Toolbar ) ->
	wxFrame:setToolBar( Frame, Toolbar ).



% @doc Adds the specified control to the specified toolbar.
-spec add_control( toolbar(), control() ) -> void().
add_control( Toolbar, Control ) ->
	wxToolBar:addControl( Toolbar, Control ).


% For add_separator/1, refer to the menu section.


% @doc Adds the specified tool, represented by the specified bitmap, with the
% specified identifier (if any) and short help, to the specified toolbar.
%
% update_tools/1 should be called once additions have been done, so that they
% are taken into account.
%
-spec add_tool( toolbar(), id(), label(), bitmap(), help_info() ) -> void().
add_tool( Toolbar, Id, Label, Bitmap, ShortHelp ) ->

	Opts = case ShortHelp of

		undefined ->
			[];

		_ ->
			[ { shortHelp, ShortHelp } ]

	end,

	wxToolBar:addTool( Toolbar, gui_id:declare_any_id( Id ), Label, Bitmap,
					   Opts ).


% @doc Adds the specified tool, represented by the specified enabled/disabled
% bitmaps, with the specified identifier (if any) and short/long helps, to the
% specified toolbar.
%
% update_tools/1 should be called once additions have been done, so that they
% are taken into account.
%
-spec add_tool( toolbar(), id(), label(), bitmap(), bitmap(), help_info(),
				help_info() ) -> void().
add_tool( Toolbar, Id, Label, BitmapIfEnabled, BitmapIfDisabled,
		  ShortHelp, LongHelp ) ->

	Opts = case ShortHelp of
		undefined -> [];
		_ -> [ { shortHelp, ShortHelp } ]
	end ++ case LongHelp  of
		undefined -> [];
		_ -> [ { longHelp, LongHelp } ]
	end,

	wxToolBar:addTool( Toolbar, gui_id:declare_any_id( Id ), Label,
					   BitmapIfEnabled, BitmapIfDisabled, Opts ).


% @doc Updates the specified toolbar so that it takes into account any new
% tools; returns whether an update had to be done.
%
-spec update_tools( toolbar() ) -> boolean().
update_tools( Toolbar ) ->
	wxToolBar:realize( Toolbar ).


% @doc Adds a separator to the specified toolbar, and returns that separator.
-spec add_separator( menu() | toolbar() ) -> menu_item().
add_separator( Toolbar ) ->
	wxToolBar:addSeparator( Toolbar ).
