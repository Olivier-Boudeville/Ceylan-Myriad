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
% Creation date: Saturday, August 26, 2023.


% @doc Gathering of various facilities for <b>button management</b>.
-module(gui_button).


% Note that:
%
%  - the parent of a button must be a widget (not a sizer for example)
%
%  - for a corresponding stock image/icon to be displayed within a button, its
%  stock identifier shall of course be specified (e.g. 'about_button'), but,if
%  specified, its label must also match (e.g. "About" or "&About") - otherwise
%  no image will be added; specifying an empty label will set the stock one



-type button() :: wxButton:wxButton().
% Designates an actual button instance.


-type button_ref() :: button() | button_id().
% Any kind of reference onto a button.


-type toggle_button() :: wxToggleButton:wxToggleButton().
% Designates a toggle button, a button that stays pressed when clicked by the
% user.
%
% It is therefore similar to a checkbox in functionality, but looks like a
% button.


-type bitmap_button() :: wxBitmapButton:wxBitmapButton().
% Designates an actual button instance displaying a bitmap.


-export_type([ button/0, button_ref/0, toggle_button/0, bitmap_button/0 ]).


-type button_opt() ::
	{ 'label', label() }
  | { 'style', button_style() }
  | { 'position', point() }
  | { 'size', dimensions() }.
% An option for a message dialog.


-type button_style() :: [ button_style_opt() ].
% The style of a button.

-type button_style_opt() ::
	'default'
  | 'left_justified'
  | 'right_justified'
  | 'top_justified'
  | 'bottom_justified'
  | 'exact_fit'
  | 'flat'.
% Options for button style, see
% [http://docs.wxwidgets.org/stable/classwx_button.html].


-export_type([ button_opt/0, button_style_opt/0 ]).


-export([ create/2, create/3, create/4, create/6,
		  create_multiple/2,
		  create_toggle/3, create_toggle/4,
		  create_bitmap/3,
		  set_label/2, destruct/1 ]).


% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").


% Shorthands:

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type parent() :: gui:parent().
-type label() :: gui:label().
-type point() :: gui:point().
-type position() :: gui:position().
-type dimensions() :: gui:dimensions().
-type size() :: gui:size().

-type id() :: gui_id:id().
-type button_id() :: gui_id:button_id().

-type bitmap() :: gui_image:bitmap().



% @doc Creates a (labelled) button, with the specified parent.
-spec create( label(), parent() ) -> button().
create( Label, Parent ) ->

	Id = ?gui_any_id,

	Options = [ { label, Label } ],

	%trace_utils:info_fmt( "Button options (for any ID): ~p.",
	%                      [ Id, Options ] ),

	wxButton:new( Parent, Id, Options ).



% @doc Creates a (labelled) button, with the specified identifier and parent.
-spec create( label(), id(), parent() ) -> button().
create( Label, Id, Parent ) ->

	Options = [ { label, Label } ],

	BackendId = gui_id:declare_any_id( Id ),

	%trace_utils:debug_fmt( "Button options for ~ts (backend ~ts): ~p.",
	% [ gui_id:id_to_string( Id ), gui_id:id_to_string( BackendId ),
	%   Options ] ),

	wxButton:new( Parent, BackendId, Options ).



% @doc Creates a (labelled) button, with the specified identifier, option(s) and
% parent.
%
% No 'label' option is to be specified among the options.
%
-spec create( label(), id(), maybe_list( button_opt() ), parent() ) -> button().
create( Label, Id, Opts, Parent ) ->

	WxOpts = [ { label, Label } | gui_wx_backend:to_wx_button_opts( Opts ) ],

	wxButton:new( Parent, gui_id:declare_any_id( Id ), WxOpts ).



% @doc Creates a button, with parent and most settings specified.
%
% (internal use only)
%
-spec create( label(), position(), size(), button_style(), id(),
			  parent() ) -> button().
create( Label, Position, Size, Style, Id, Parent ) ->

	Options = [ { label, Label }, gui_wx_backend:to_wx_position( Position ),
				gui_wx_backend:to_wx_size( Size ),
				{ style, gui_wx_backend:button_style_to_bitmask( Style ) } ],

	BackendId = gui_id:declare_any_id( Id ),

	%trace_utils:debug_fmt( "For button '~ts' (~ts), got ~ts. "
	%   "Options: ~n ~p.",
	%   [ Label, gui_id:id_to_string( Id ), gui_id:id_to_string( BackendId ),
	%     Options ] ),

	wxButton:new( Parent, BackendId, Options ).




% @doc Creates (labelled) buttons, with their (single, common) parent specified.
-spec create_multiple( [ label() ], parent() ) -> [ button() ].
% Not merged with create/2, as would not be clear enough.
create_multiple( Labels, Parent ) ->
	create_multiple_helper( Labels, Parent, _Acc=[] ).


% (helper)
create_multiple_helper( _Labels=[], _Parent, Acc ) ->
	lists:reverse( Acc );

create_multiple_helper( [ Label | T ], Parent, Acc ) ->
	NewButton = create( Label, Parent ),
	create_multiple_helper( T, Parent, [ NewButton | Acc ] ).



% @doc Creates a (labelled) toggle button with the specified identifier and
% parent.
%
-spec create_toggle( label(), id(), parent() ) -> toggle_button().
create_toggle( Label, Id, Parent ) ->
	wxToggleButton:new( Parent, gui_id:declare_any_id( Id ), Label ).


% @doc Creates a (labelled) toggle button, with the specified identifier,
% option(s) and parent.
%
% No 'label' option is to be specified among the options.
%
-spec create_toggle( label(), id(), maybe_list( button_opt() ), parent() ) ->
											toggle_button().
create_toggle( Label, Id, Opts, Parent ) ->
	wxToggleButton:new( Parent, gui_id:declare_any_id( Id ), Label,
						gui_wx_backend:to_wx_button_opts( Opts ) ).



% @doc Creates a button with the specified identifier, and which displays the
% specified bitmap.
%
-spec create_bitmap( bitmap(), id(), parent() ) -> bitmap_button().
create_bitmap( Bitmap, Id, Parent ) ->
	wxBitmapButton:new( Parent, gui_id:declare_any_id( Id ), Bitmap ).


% @doc Sets the label of the specified button.
-spec set_label( button(), label() ) -> void().
set_label( Button, Label ) ->
	wxButton:setLabel( Button, Label ).


% @doc Destructs the specified button.
-spec destruct( button() ) -> void().
destruct( Button ) ->
	wxButton:destroy( Button ).
