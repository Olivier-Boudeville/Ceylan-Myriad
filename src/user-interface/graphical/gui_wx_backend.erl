% Copyright (C) 2017-2023 Olivier Boudeville
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
% Creation date: Wednesday, October 4, 2017.


% @doc Gathers all elements relative to the (Erlang) <b>wx backend</b> version
% 2.1 (itself based on [wxWidgets](https://www.wxwidgets.org/)).
%
% Now uses the gui_generated module, which is generated based on the
% gui_constants one (see gui:generate_support_modules/0) in order to be able to
% convert the identifiers / defines / constants between MyriadGUI and their
% backend (wx) counterparts.
%
-module(gui_wx_backend).



% Usually a class of wxWidgets is represented as a module in Erlang.
%
% GUI objects (e.g. widgets) correspond to (Erlang) processes. They are
% designated here with gui_object() values, which are references, either to wx
% objects or, in some cases, to MyriadGUI-defined ones.

% In wx, the user code handles wx:wx_object() instances, which are actually just
% references onto the actual instances that are stored internally by wx.
%
% We replicate this behaviour with the myriad_object_ref() type, referencing a
% MyriadGUI-object (most probably made from wx ones, like in the case of the
% canvas).
%
% MyriadGUI objects are created with {new,create}/*, and, when appropriate,
% deleted with destruct/1.
%
% For information regarding events, refer to gui_event.erl.



% Understanding the wx base widgets hierarchy:
%
% (offsets meaning "inheriting from", corresponding local types specified
% between brackets; a hierarchy is a truncated view as a wx class may inherit
% from more than one parent one):
%
% .
% ├── wxObject
% │   ├── wxEvtHandler
% │   │   └── wxWindow
% │   │       ├── wxControl
% │   │       │   └── wxAnyButton
% │   │       │       └── wxButton
% │   │       ├── wxPanel
% │   │       ├── wxStatusBar
% │   │       └── wxTopLevelWindow
% │   │           ├── wxDialog
% │   │           └── wxFrame
% │   └── wxSizer
% └── wxTrackable


% In a more detailed view, with the direct parent classes listed between
% parentheses, and with the corresponding MyriadGUI types being specified
% between brackets:
%
% - wxObject() [gui_object]: root class of all wxWidgets classes
%
%   - wxEvtHandler(wxObject) [event_handler]: a class that can handle events
%   from the windowing system
%
%      - wxWindow(wxEvtHandler) [window]: the base class for all windows and
%      represents any visible object on screen For colors, refer to the color
%      module
%
%        - wxControl(wxWindow): base class for a control or
%        "widget"; generally a small window which processes user input and/or
%        displays one or more item of data
%
%          - wxButton(wxControl): a control that contains a text string, and is
%          one of the most common elements of a GUI; may be placed on any
%          window, notably dialog boxes or panels
%
%        - wxPanel(wxWindow) [panel]: a window on which controls are placed;
%        usually placed within a frame
%
%        - wxTopLevelWindow(wxWindow): abstract base class for wxDialog and
%        wxFrame
%
%           - wxDialog(wxTopLevelWindow): a window with a title bar and
%           sometimes a system menu, which can be moved around the screen; often
%           used to allow the user to make some choice or to answer a question
%
%           - wxFrame(wxTopLevelWindow) [frame]: a window whose size and
%           position can (usually) be changed by the user. It usually has thick
%           borders and a title bar, and can optionally contain a menu bar,
%           toolbar and status bar. A frame can contain any window that is not a
%           frame or dialog
%
%      - wxStatusBar(wxEvtHandler): narrow window that can be placed along the
%      bottom of a frame
%
%   - wxSizer(wxClientDataContainer): abstract base class used for laying out
%   subwindows in a window


% See also all the classes of wxwidgets:
% https://docs.wxwidgets.org/3.0/page_class_cat.html and
% https://docs.wxwidgets.org/3.0/classes.html


% Additional widgets
%
% Some types of widgets seem to be lacking to wxWidgets, such as canvases that
% would be first-level citizens (e.g. able to emit and receive events, when
% needing repaint or being resized).
%
% To support them, we defined the myriad_object_ref record to complement the
% wx_object() type (all widgets are thus gui_object(), meaning either
% wx:wx_object() or myriad_object_ref()), and we maintain our own instance table
% of the instances of the additional gui_object types we defined.
%
% For the actual mode of operation, we mimic the mode of operation of wx; to
% find the GUI server, no naming service is used, instead the process dictionary
% stores a (MyriadGUI) environment (like the wx one).



% Function export section.


-export([ get_wx_version/0 ]).


% Conversions between MyriadGUI and backend (wx) are now mostly done thanks to
% the gui_generated module:
%
-export([ to_wx_object_type/1,
		  to_wx_connect_options/3,
		  to_wx_debug_level/1,

		  window_style_to_bitmask/1, get_window_options/1,
		  frame_style_to_bitmask/1,
		  get_panel_options/1,
		  button_style_to_bitmask/1,

		  to_wx_sizer_options/1, sizer_flags_to_bitmask/1,

		  to_wx_menu_options/1,
		  to_new_wx_menu_item_id/1,
		  to_wx_menu_item_id/1,

		  %to_wx_menu_item_options/1,

		  to_wx_bitmap_id/1, to_wx_icon_id/1,

		  to_wx_status_bar_style/1,
		  to_wx_toolbar_style/1,
		  to_wx_tool_kind/1,

		  to_wx_message_dialog_opts/1, to_wx_message_dialog_opt/1,
		  to_wx_single_choice_dialog_opts/1, to_wx_single_choice_dialog_opt/1,
		  to_wx_multi_choice_dialog_opts/1, to_wx_multi_choice_dialog_opt/1,
		  to_wx_text_entry_dialog_opts/1, to_wx_text_entry_dialog_opt/1,

		  to_wx_event_type/1, from_wx_event_type/1,

		  to_wx_id/1, to_wx_parent/1, to_wx_position/1, to_wx_size/1,
		  to_wx_direction/1, to_wx_orientation/1,
		  wx_id_to_window/1, wx_id_to_string/1,

		  to_wx_device_context_attributes/1,

		  connect/2, connect/3, connect/4, disconnect/1, disconnect/2 ]).


% Conversions from wx to MyriadGUI:
-export([ from_wx_object_type/1 ]).


% For the wx defines:
-include("gui_internal_defines.hrl").


% Type section.


% Default position, chosen by either the windowing system or wxWidgets,
% depending on platform:
%
-define( wx_default_position, { -1, -1 } ).

-type wx_position() :: { 'pos', gui:point() }.


% Default size, chosen by either the windowing system or wxWidgets,
% depending on platform:
%
-define( wx_default_size, { -1, -1 } ).

-type wx_size() :: { 'size', gui:size() }.

-type wx_direction() :: ?wxVERTICAL | ?wxHORIZONTAL.
-type wx_orientation() :: wx_direction() | ?wxBOTH.


-type wx_id() :: maybe( integer() ).
% The identifier (ID) of a wx element is an integer (positive or not).
%
% This identifier (e.g. 63) is relative to a given type, like in:
% {wx_ref,63,wxFrame,[]}.
%
% This type allows to specify a 'void' (null) ID of a GUI element.
%
% Sometimes the ID may be directly provided by the user or have a predefined
% value, such as wxID_OPEN; see
% [http://docs.wxwidgets.org/2.8.12/wx_stockitems.html#stockitems] for a list
% thereof.
%
% Often, however, the value of the ID is unimportant and in this case it is
% enough to use wxID_ANY as the ID of an object which tells wxWidgets to assign
% an ID automatically.
%
% All such automatically-assigned IDs are negative, so the IDs predefined in the
% user code should always be positive to avoid clashes with them.
%
% More generally, wx identifiers (rather than wx references) should be used only
% internally.
%
% Note: this type is defined and exported, yet reported unknown by Dialyzer.


% See any_id, no_parent, etc. as defined in gui_base.hrl.



-type wx_native_object_type() :: atom().
% Native wx object types (e.g. 'wxFrame').
%
% No enumeration like 'wxWindow' | 'wxFrame' | ... found in wx.



-type wx_window_option() :: term().

-type wx_event_handler_option() :: { 'id', integer() }
								 | { 'lastId', integer() }
								 | { 'skip', boolean() }
								 | 'callback'
								 | {'callback', function() }
								 | {'userData', term() }.
% Refer to https://erlang.org/doc/man/wxEvtHandler.html.
% See the corresponding gui:event_subscription_option().


-type wx_panel_option() :: wx_window_option() | wx_event_handler_option().

% Precisely:
%    {id, integer()} |
%    {pos, {X :: integer(), Y :: integer()}} |
%    {size, {W :: integer(), H :: integer()}} |
%    {style, integer()} |
%    {name, unicode:chardata()} |
%    {palette, wxPalette:wxPalette()}
%
-type other_wx_device_context_attribute() :: atom_entry().


-type wx_device_context_attribute() ::
		{ 'attribList', integer() } | other_wx_device_context_attribute().
% Refer to wxGLCanvas: https://www.erlang.org/doc/man/wxglcanvas#new-2.


-type wx_enum() :: wx:wx_enum().
% A wxWidgets enumerated value.


-export_type([ wx_native_object_type/0, wx_window_option/0,
			   wx_event_handler_option/0, wx_panel_option/0,
			   other_wx_device_context_attribute/0,
			   wx_device_context_attribute/0, wx_enum/0,
			   wx_direction/0, wx_orientation/0 ]).

-type wx_art_id() :: unicode:chardata().
% For example "wxART_NEW".


% Preferably no '-export_type' here to avoid leakage of backend conventions.


% Shorthands:

-type bit_mask() :: basic_utils:bit_mask().

-type maybe_list(T) :: list_utils:maybe_list( T ).

-type atom_entry() :: hashtable:atom_entry().

-type ustring() :: text_utils:ustring().

-type wx_object_type() :: gui:wx_object_type().
-type myriad_object_type() :: gui:myriad_object_type().

-type gui_object() :: gui:gui_object().

-type window() :: gui:window().

-type window_style() :: gui:window_style().
-type window_style_opt() :: gui:window_style_opt().

-type window_option() :: gui:window_option().

-type frame_style() :: gui:frame_style().
-type frame_style_opt() :: gui:frame_style_opt().

-type panel_option() :: gui:panel_option().

-type button_style() :: gui:button_style().
-type button_style_opt() :: gui:button_style_opt().


-type bitmap_name_id() :: gui:bitmap_name_id().
-type icon_name_id() :: gui:icon_name_id().

-type sizer_options() :: gui:sizer_options().
-type sizer_flags() :: gui:sizer_flags().
-type sizer_flag_opt() :: gui:sizer_flag_opt().

-type menu_option() :: gui:menu_option().
-type menu_item_id() :: gui:menu_item_id().
-type menu_item_kind() :: gui:menu_item_kind().

-type status_bar_style() :: gui:status_bar_style().
-type status_bar_style_opt() :: gui:status_bar_style_opt().

-type toolbar_style() :: gui:toolbar_style().
-type toolbar_style_opt() :: gui:toolbar_style_opt().

-type tool_kind() :: gui:tool_kind().

-type message_dialog_opt() :: gui:message_dialog_opt().
-type single_choice_dialog_opt() :: gui:single_choice_dialog_opt().
-type multi_choice_dialog_opt() :: gui:multi_choice_dialog_opt().
-type text_entry_dialog_opt() :: gui:text_entry_dialog_opt().

-type position() :: gui:position().
-type size() :: gui:size().
-type direction() :: gui:direction().
-type orientation() :: gui:orientation().


-type myriad_instance_id() :: gui_id:myriad_instance_id().

-type event_subscription_option() :: gui_event:event_subscription_option().
-type wx_event_type() :: gui_event:wx_event_type().
-type event_type() :: gui_event:event_type().
-type event_source() :: gui_event:event_source().
-type trap_set() :: gui_event:trap_set().

-type device_context_attribute() :: gui_opengl:device_context_attribute().

% To improve:
-type wx_sizer_options() :: sizer_options().


% To avoid unused warnings:
-export_type([ wx_id/0 ]).


% For canvas_state():
-include("gui_canvas.hrl").



% Implementation section.
%
% Conversions, once listed by themes, used to be done thanks to explicitly
% defined function pairs: in each theme first from MyriadGUI to wx
% (to_wx_*), then from wx to MyriadGUI (from_wx_*), to facilitate the
% consistency of the two-way definitions.
%
% Now these bijective MyriadGUI/wx conversions are more efficiently managed as
% (generated) code (shared directly, with neither double definition nor runtime
% term-passing) through the gui_generated module (refer to
% gui:generate_support_modules/0 and the resulting gui_constants module).




% @doc Returns the build-time version of wx (wxWidgets).
-spec get_wx_version() -> basic_utils:four_digit_version().
get_wx_version() ->
	{ ?wxMAJOR_VERSION, ?wxMINOR_VERSION, ?wxRELEASE_NUMBER,
	  ?wxSUBRELEASE_NUMBER }.



% Object type section.


% @doc Converts a MyriadGUI type of object into a wx one.
-spec to_wx_object_type( myriad_object_type() ) -> wx_object_type().
to_wx_object_type( MyrObjType ) ->
	gui_generated:get_second_for_object_type( MyrObjType ).


% @doc Converts a wx type of object into a MyriadGUI one.
-spec from_wx_object_type( wx_object_type() ) -> myriad_object_type().
from_wx_object_type( WxObjectType ) ->
	gui_generated:get_first_for_object_type( WxObjectType ).



% Event type section.


% @doc Converts a MyriadGUI type of event into a wx one.
-spec to_wx_event_type( event_type() ) -> wx_event_type().
to_wx_event_type( EventType ) ->
	gui_generated:get_second_for_event_type( EventType ).


% @doc Converts a wx type of event into a MyriadGUI one.
-spec from_wx_event_type( wx_event_type() ) -> event_type().
from_wx_event_type( WxEventType ) ->
	gui_generated:get_first_for_event_type( WxEventType ).



% Debug section.


% @doc Converts the debug level from MyriadGUI to the one of wx.
%
% (helper)
%
to_wx_debug_level( _DebugLevel=none ) ->
	none;

to_wx_debug_level( _DebugLevel=calls ) ->
	trace;

to_wx_debug_level( _DebugLevel=life_cycle ) ->
	driver.




% Windows section.


% @doc Converts the specified MyriadGUI window style into the appropriate
% wx-specific bit mask.
%
% (helper)
%
-spec window_style_to_bitmask( window_style() | window_style_opt() ) ->
										bit_mask().
window_style_to_bitmask( StyleOpts ) when is_list( StyleOpts ) ->
	lists:foldl( fun( S, Acc ) ->
					gui_generated:get_second_for_window_style( S ) bor Acc
				 end,
				 _InitialAcc=0,
				 _List=StyleOpts );

window_style_to_bitmask( StyleOpt ) ->
	gui_generated:get_second_for_window_style( StyleOpt ).



% @doc Converts the specified MyriadGUI window option(s) into the appropriate
% wx-specific options.
%
% (exported helper)
%
-spec get_window_options( maybe_list( window_option() ) ) ->
								[ wx_window_option() ].
get_window_options( Options ) when is_list( Options ) ->
	get_window_options( Options, _Acc=[] );

get_window_options( Option ) ->
	get_window_options( [ Option ] ).



get_window_options( _Options=[], Acc ) ->
	Acc;

get_window_options( _Options=[ { style, Style } | T ], Acc ) ->
	get_window_options( T,
		[ { style, window_style_to_bitmask( Style ) } | Acc ] );

% Unchanged:
get_window_options( _Options=[ H | T ], Acc ) ->
	get_window_options( T, [ H | Acc ] ).



% Frames section.


% @doc Converts the specified MyriadGUI frame style into the appropriate
% wx-specific bit mask.
%
% (helper)
%
-spec frame_style_to_bitmask( frame_style() | frame_style_opt() ) -> bit_mask().
frame_style_to_bitmask( StyleOpts ) when is_list( StyleOpts ) ->
	% 'bor ?wxWANTS_CHARS' not desirable a priori:
	lists:foldl( fun( S, Acc ) ->
					gui_generated:get_second_for_frame_style( S ) bor Acc
				 end,
				 _InitialAcc=0,
				 _List=StyleOpts );

frame_style_to_bitmask( StyleOpt ) ->
	gui_generated:get_second_for_frame_style( StyleOpt ).





% Panels section.


% @doc Converts the specified MyriadGUI panel option(s) into the appropriate
% wx-specific options.
%
% (exported helper)
%
-spec get_panel_options( maybe_list( panel_option() ) ) ->
											[ wx_panel_option() ].
get_panel_options( Options ) ->
	get_window_options( Options ).




% Buttons section.


% @doc Converts the specified MyriadGUI button style into the appropriate
% wx-specific bit mask.
%
% (helper)
%
-spec button_style_to_bitmask( button_style() | button_style_opt() ) ->
										bit_mask().
button_style_to_bitmask( StyleOpts ) when is_list( StyleOpts ) ->
	lists:foldl( fun( S, Acc ) ->
					gui_generated:get_second_for_button_style( S ) bor Acc
				 end,
				 _InitialAcc=0,
				 _List=StyleOpts );

button_style_to_bitmask( StyleOpt ) ->
	gui_generated:get_second_for_button_style( StyleOpt ).




% Sizers section.



% @doc Converts the specified sizer options into wx-specific ones.
%
% (helper)
%
-spec to_wx_sizer_options( maybe_list( sizer_options() ) ) ->
											wx_sizer_options().
to_wx_sizer_options( Options ) when is_list( Options ) ->
	to_wx_sizer_options( Options, _AccOpts=[], _AccFlags=[] );

to_wx_sizer_options( Option ) ->
	to_wx_sizer_options( [ Option ] ).



% (helper)
to_wx_sizer_options( _Options=[], AccOpts, _AccFlags=[]  ) ->
	AccOpts;

to_wx_sizer_options( _Options=[], AccOpts, AccFlags ) ->
	[ { flag, sizer_flags_to_bitmask( AccFlags ) } | AccOpts ];

to_wx_sizer_options( _Options=[ { userData, Data } | T ], AccOpts, AccFlags ) ->
	to_wx_sizer_options( T, [ { user_data, Data } | AccOpts ], AccFlags );

% Letting {proportion, integer()} and {border, integer()} go through:
to_wx_sizer_options( _Options=[ P | T ], AccOpts, AccFlags )
											when is_tuple( P ) ->
	to_wx_sizer_options( T, [ P | AccOpts ], AccFlags );

to_wx_sizer_options( _Options=[ F | T ], AccOpts, AccFlags ) ->
	to_wx_sizer_options( T, AccOpts, [ F | AccFlags ] ).



% @doc Converts the specified MyriadGUI sizer flag options into the appropriate
% wx-specific bit mask.
%
% (helper)
%
-spec sizer_flags_to_bitmask( sizer_flags() | sizer_flag_opt() ) -> bit_mask().
sizer_flags_to_bitmask( FlagOpts ) when is_list( FlagOpts ) ->
	lists:foldl( fun( F, Acc ) ->
					gui_generated:get_second_for_sizer_flag( F ) bor Acc end,
				 _InitialAcc=0,
				 _List=FlagOpts );

sizer_flags_to_bitmask( FlagOpt ) ->
	gui_generated:get_second_for_sizer_flag( FlagOpt ).


% @doc Converts specified menu option(s) into wx-specific ones.
-spec to_wx_menu_options( maybe_list( menu_option() ) ) -> list().
to_wx_menu_options( Options ) when is_list( Options ) ->
	[ to_wx_menu_option( O ) || O <- Options ];

to_wx_menu_options( Opt ) ->
	to_wx_menu_options( [ Opt ] ).


% (helper)
to_wx_menu_option( _Opt=detachable ) ->
	{ style, ?wxMENU_TEAROFF }.



% @doc Converts the specified menu item identifier (for an already-existing menu
% item) into a wx-specific one.
%
-spec to_wx_menu_item_id( menu_item_id() ) -> wx_id().
to_wx_menu_item_id( MenuItemId ) ->

	%trace_utils:debug_fmt( "Testing availability: ~ts",
	%   [ code_utils:study_function_availability( gui_generated,
	%       get_maybe_second_for_menu_item_id, 1 ) ] ),

	case gui_generated:get_maybe_second_for_menu_item_id( MenuItemId ) of

		undefined ->
			gui_id:resolve_id( MenuItemId );

		WxId ->
			WxId

	end.


% @doc Converts the specified menu identifier for a new menu item into a
% wx-specific one.
%
-spec to_new_wx_menu_item_id( menu_item_id() ) -> wx_id().
to_new_wx_menu_item_id( MenuItemId ) ->
	case gui_generated:get_maybe_second_for_menu_item_id( MenuItemId ) of

		undefined ->
			gui_id:declare_id( MenuItemId );

		WxId ->
			WxId

	end.


% @doc Converts the specified kind of menu identifier into a wx-specific one.
-spec to_wx_menu_item_kind( menu_item_kind() ) -> wx_enum().
to_wx_menu_item_kind( Kind ) ->
	% Same:
	gui_generated:get_second_for_menu_item_kind( Kind ).



% @doc Converts the specified bitmap identifier (for an already-existing menu
% item) into a wx-specific one.
%
-spec to_wx_bitmap_id( bitmap_name_id() ) -> wx_art_id().
to_wx_bitmap_id( BitmapId ) ->
	case gui_generated:get_maybe_second_for_bitmap_id( BitmapId ) of

		undefined ->
			throw( { unknown_bitmap_id, BitmapId } );

		WxArtId ->
			WxArtId

	end.




% @doc Converts the specified icon identifier into a wx-specific one.
-spec to_wx_icon_id( icon_name_id() ) -> wx_art_id().
to_wx_icon_id( IconId ) ->
	case gui_generated:get_maybe_second_for_icon_name_id( IconId ) of

		undefined ->
			throw( { unknown_icon_id, IconId } );

		WxIconId ->
			WxIconId

	end.


% @doc Converts the specified style of status bar into a wx-specific one.
-spec to_wx_status_bar_style( status_bar_style() | status_bar_style_opt() ) ->
										wx_enum().
to_wx_status_bar_style( StyleOpts ) when is_list( StyleOpts ) ->
	lists:foldl( fun( S, Acc ) ->
					gui_generated:get_second_for_status_bar_style( S ) bor Acc
				 end,
				 _InitialAcc=0,
				 _List=StyleOpts );

to_wx_status_bar_style( StyleOpt ) ->
	gui_generated:get_second_for_status_bar_style( StyleOpt ).




% @doc Converts the specified style of toolbar into a wx-specific one.
-spec to_wx_toolbar_style( toolbar_style() | toolbar_style_opt() ) -> wx_enum().
to_wx_toolbar_style( StyleOpts ) when is_list( StyleOpts ) ->
	lists:foldl( fun( S, Acc ) ->
					gui_generated:get_second_for_toolbar_style( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleOpts );

to_wx_toolbar_style( StyleOpt ) ->
	gui_generated:get_second_for_toolbar_style( StyleOpt ).



% @doc Converts the specified options for message dialogs into wx-specific ones.
-spec to_wx_message_dialog_opts( maybe_list( message_dialog_opt() ) ) -> list().
to_wx_message_dialog_opts( MsgOpts ) when is_list( MsgOpts ) ->
	Res = [ to_wx_message_dialog_opt( MO ) || MO <- MsgOpts ],
	%trace_utils:debug_fmt( "Wx message dialog options: ~p", [ Res ] ),
	Res;

to_wx_message_dialog_opts( MsgOpt ) ->
	to_wx_message_dialog_opts( [ MsgOpt ] ).



% @doc Converts the specified option for message dialogs into a wx-specific one.
-spec to_wx_message_dialog_opt( message_dialog_opt() ) -> tuple().
to_wx_message_dialog_opt( MsgOpt={ caption, _CaptionStr } ) ->
	MsgOpt;

to_wx_message_dialog_opt( _MsgOpt={ style, Styles } ) ->
	WxStyle = lists:foldl( fun( S, Acc ) ->
		gui_generated:get_second_for_message_dialog_style( S ) bor Acc end,
		_InitialAcc=0,
		_List=Styles ),

	{ style, WxStyle };

to_wx_message_dialog_opt( MsgOpt={ pos, _Pos } ) ->
	MsgOpt.



% @doc Converts the specified options for single-choice dialogs into wx-specific
% ones.
%
-spec to_wx_single_choice_dialog_opts(
						maybe_list( single_choice_dialog_opt() ) ) -> list().
to_wx_single_choice_dialog_opts( ScdOpts ) when is_list( ScdOpts ) ->
	Res = [ to_wx_single_choice_dialog_opt( SO ) || SO <- ScdOpts ],
	%trace_utils:debug_fmt( "Wx single-choice dialog options: ~p", [ Res ] ),
	Res;

to_wx_single_choice_dialog_opts( ScdOpt ) ->
	to_wx_single_choice_dialog_opts( [ ScdOpt ] ).



% @doc Converts the specified option for single-choice dialogs into a
% wx-specific one.
%
-spec to_wx_single_choice_dialog_opt( single_choice_dialog_opt() ) -> tuple().
to_wx_single_choice_dialog_opt( _ScdOpt={ style, Styles } ) ->
	WxStyle = lists:foldl( fun( S, Acc ) ->
		% More general table used:
		gui_generated:get_second_for_message_dialog_style( S ) bor Acc end,
		_InitialAcc=0,
		_List=Styles ),

	{ style, WxStyle };

to_wx_single_choice_dialog_opt( ScdOpt={ pos, _Pos } ) ->
	ScdOpt.



% @doc Converts the specified options for multiple-choice dialogs into
% wx-specific ones.
%
-spec to_wx_multi_choice_dialog_opts(
						maybe_list( multi_choice_dialog_opt() ) ) -> list().
to_wx_multi_choice_dialog_opts( MultOpts ) ->
	% Same:
	to_wx_single_choice_dialog_opts( MultOpts ).


% @doc Converts the specified option for multi-choice dialogs into a
% wx-specific one.
%
-spec to_wx_multi_choice_dialog_opt( multi_choice_dialog_opt() ) -> tuple().
to_wx_multi_choice_dialog_opt( MultOpt ) ->
	to_wx_single_choice_dialog_opt( MultOpt ).




% @doc Converts the specified options for text-entry dialogs into wx-specific
% ones.
%
-spec to_wx_text_entry_dialog_opts( maybe_list( text_entry_dialog_opt() ) ) ->
											 list().
to_wx_text_entry_dialog_opts( TextEntryOpts ) when is_list( TextEntryOpts ) ->
	[ to_wx_text_entry_dialog_opt( TEO ) || TEO <- TextEntryOpts ];

to_wx_text_entry_dialog_opts( TextEntryOpt ) ->
	to_wx_text_entry_dialog_opts( [ TextEntryOpt ] ).


% @doc Converts the specified option for text-entry dialogs into a wx-specific
% one.
%
-spec to_wx_text_entry_dialog_opt( text_entry_dialog_opt() ) -> tuple().
to_wx_text_entry_dialog_opt( TextEntryOpt={ caption, _CaptionStr } ) ->
	TextEntryOpt;

to_wx_text_entry_dialog_opt( _TextEntryOpt={ style, Styles } ) ->
	WxStyle = lists:foldl( fun( S, Acc ) ->
		gui_generated:get_second_for_message_dialog_style( S ) bor Acc end,
		_InitialAcc=0,
		_List=Styles ),

	{ style, WxStyle };

to_wx_text_entry_dialog_opt( TextEntryOpt={ pos, _Pos } ) ->
	TextEntryOpt;

to_wx_text_entry_dialog_opt( _TextEntryOpt={ initial_text, InitialText } ) ->
	{ value, InitialText }.



% @doc Converts the specified kind of tool into a wx-specific one.
-spec to_wx_tool_kind( tool_kind() ) -> wx_enum().
to_wx_tool_kind( ToolKind ) ->
	to_wx_menu_item_kind( ToolKind ).



% @doc Converts specified MyriadGUI identifier in a wx-specific widget
% identifier.
%
% (helper)
%
-spec to_wx_id( maybe( myriad_instance_id() ) ) -> wx_id().
to_wx_id( undefined ) ->
	?any_id;

to_wx_id( Other ) ->
	Other.



% @doc Converts the specified MyriadGUI identifier into a wx-specific parent
% widget identifier.
%
% (helper)
%
-spec to_wx_parent( maybe( gui_object() ) ) -> gui_object().
to_wx_parent( undefined ) ->
	?no_parent;

to_wx_parent( Other ) ->
	Other.



% @doc Converts specified MyriadGUI position in a wx-specific position (with
% defaults).
%
% (helper)
%
-spec to_wx_position( position() ) -> wx_position().
to_wx_position( _Position=auto ) ->
	{ pos, ?wx_default_position };

to_wx_position( Position ) ->
	{ pos, Position }.



% @doc Converts specified MyriadGUI size in a wx-specific size (with defaults).
%
% (helper)
%
-spec to_wx_size( size() ) -> wx_size().
to_wx_size( _Size=auto ) ->
	{ size, ?wx_default_size };

%to_wx_size( Size={ _X, _Y } ) ->
to_wx_size( Size ) ->
	{ size, Size }.



% @doc Converts to back-end direction.
%
% (helper)
%
-spec to_wx_direction( direction() ) -> wx_direction().
to_wx_direction( Direction ) ->
	gui_generated:get_second_for_direction( Direction ).


% @doc Converts to back-end orientation.
%
% (helper)
%
-spec to_wx_orientation( orientation() ) -> wx_orientation().
to_wx_orientation( Orientation ) ->
	gui_generated:get_second_for_orientation( Orientation ).



% @doc Converts the specified MyriadGUI device context attributes to wx
% conventions.
%
-spec to_wx_device_context_attributes( [ device_context_attribute() ] ) ->
											[ wx_device_context_attribute() ].
to_wx_device_context_attributes( Attrs ) ->
	to_wx_device_context_attributes( Attrs, _Acc=[] ).


% (helper)
%
% Adding in a reverse form:
to_wx_device_context_attributes( _Attrs=[], Acc ) ->
	lists:reverse( [ 0 | Acc ] );

to_wx_device_context_attributes( _Attrs=[ rgba | T ], Acc ) ->
	to_wx_device_context_attributes( T, [ ?WX_GL_RGBA | Acc ] );

% Not existing:
%to_wx_device_context_attributes( _Attrs=[ bgra | T ], Acc ) ->
%   to_wx_device_context_attributes( T, [ ?WX_GL_BGRA | Acc ] );

to_wx_device_context_attributes( _Attrs=[ double_buffer | T ], Acc ) ->
	to_wx_device_context_attributes( T, [ ?WX_GL_DOUBLEBUFFER | Acc ] );

to_wx_device_context_attributes( _Attrs=[ { min_red_size, S } | T ], Acc ) ->
	to_wx_device_context_attributes( T, [ S, ?WX_GL_MIN_RED | Acc ] );

to_wx_device_context_attributes( _Attrs=[ { min_green_size, S } | T ],
								 Acc ) ->
	to_wx_device_context_attributes( T, [ S, ?WX_GL_MIN_GREEN | Acc ] );

to_wx_device_context_attributes( _Attrs=[ { min_blue_size, S } | T ], Acc ) ->
	to_wx_device_context_attributes( T, [ S, ?WX_GL_MIN_BLUE | Acc ] );

to_wx_device_context_attributes( _Attrs=[ { depth_buffer_size, S } | T ],
								 Acc ) ->
	to_wx_device_context_attributes( T, [ S, ?WX_GL_DEPTH_SIZE | Acc ] );

to_wx_device_context_attributes( _Attrs=[ use_core_profile | T ], Acc ) ->
	to_wx_device_context_attributes( T, [ ?WX_GL_CORE_PROFILE | Acc ] );

to_wx_device_context_attributes( _Attrs=[ debug_context | T ], Acc ) ->
	to_wx_device_context_attributes( T, [ ?WX_GL_DEBUG | Acc ] );

to_wx_device_context_attributes( _Attrs=[ Other | _T ], _Acc ) ->
	throw( { unsupported_device_context_attribute, Other } ).



%
% Section for the conversions from wx to MyriadGUI:
%




%
% Section for wx-related facilities.
%


% @doc Returns the widget corresponding to the specified wx identifier.
%
% (internal use only)
%
-spec wx_id_to_window( wx_id() ) -> window().
wx_id_to_window( Id ) ->
	wxWindow:findWindowById( Id ).


% @doc Returns a textual representation of the specified GUI object wx
% identifier.
%
-spec wx_id_to_string( wx_id() ) -> ustring().
wx_id_to_string( _Id=undefined ) ->
	"no id defined";

wx_id_to_string( _Id=?any_id ) ->
	"'any id' defined";

wx_id_to_string( Id ) ->
	text_utils:format( "ID #~B", [ Id ] ).






% Connection-related section.


% @doc Subscribes the current process to the specified type(s) of events
% regarding the specified object (receiving for that a message).
%
% Only useful for context-less calls; the versions of that function specifying a
% "trap set" parameter shall be preferred, as they are more efficient.
%
-spec connect( event_source(), maybe_list( event_type() ) ) -> void().
connect( EventSource, EventTypeOrTypes ) ->

	% Here no trap set specified, trying to secure it:
	GUIEnvPid = gui:get_environment_server(),

	TrapSet = environment:get( trap_set, GUIEnvPid ),

	connect( EventSource, EventTypeOrTypes, TrapSet ).



% @doc Subscribes the current process to the specified type(s) of events
% regarding the specified object (receiving for that a message).
%
% Said otherwise: requests the specified widget to send to the current process a
% message-based event when the specified kind of event happens, knowing that by
% default, depending on its type, this event may also be propagated upward in
% the widget hierarchy, through the corresponding event handlers (the trap_event
% option allows not to propagate this event).
%
% Note:
%  - apparently registering more than once a given type has no effect (not N
%  messages of that type sent afterwards)
%  - only useful internally or when bypassing the default main loop
%
-spec connect( event_source(), maybe_list( event_type() ), trap_set() ) ->
						void().
connect( EventSource, EventTypeOrTypes, TrapSet ) ->
	connect( EventSource, EventTypeOrTypes, _Options=[], TrapSet ).



% @doc Subscribes the current process to the specified type(s) of events
% regarding the specified object, with the specified options; this process will
% thus receive a gui_event() message whenever a corresponding event occurs.
%
% The {trap,propagate}_event options (or the corresponding
% {trap,propagate}_event/1 functions) can override these defaults.
%
% Refer to connect/3 for all details.
%
-spec connect( event_source(), maybe_list( event_type() ),
		[ event_subscription_option() ], trap_set() ) -> void().
% Was not used apparently:
%connect( #canvas_state{ panel=Panel }, EventTypeOrTypes, Options, TrapSet ) ->
%   connect( Panel, EventTypeOrTypes, Options, TrapSet );

connect( SourceGUIObject, EventTypes, Options, TrapSet )
						when is_list( EventTypes ) ->

	%trace_utils:debug_fmt( "Connecting ~p for event types ~w with options ~p.",
	%                       [ SourceGUIObject, EventTypes, Options ] ),

	[ connect( SourceGUIObject, ET, Options, TrapSet ) || ET <- EventTypes ];

connect( SourceGUIObject, EventType, Options, TrapSet ) ->

	% Events to be processed through messages, not callbacks:
	WxEventType = to_wx_event_type( EventType ),

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( " - connecting event source '~ts' to ~w "
			"for ~p (i.e. ~p), with options ~p.",
			[ gui:object_to_string( SourceGUIObject ), self(), EventType,
			  WxEventType, Options ] ) ),

	WxConnOpts = to_wx_connect_options( Options, EventType, TrapSet ),

	wxEvtHandler:connect( SourceGUIObject, WxEventType, WxConnOpts ).



% @doc Converts MyriadGUI connect options into wx ones.
%
% The corresponding event type must be specified in order to apply per-type
% defaults.
%
-spec to_wx_connect_options( [ event_subscription_option() ], event_type(),
							 trap_set() ) -> [ wx_event_handler_option() ].
to_wx_connect_options( Opts, EventType, TrapSet ) ->
	to_wx_connect_options( Opts, EventType, TrapSet,
						   _PropagationSetting=undefined, _Acc=[] ).


% (helper)
-spec to_wx_connect_options( [ event_subscription_option() ], event_type(),
	trap_set(), maybe( 'propagate' | 'trap' ), event_type() ) ->
		[ wx_event_handler_option() ].
% End of recursion, propagation explicitly requested by the user:
to_wx_connect_options( _Opts=[], _EventType, _TrapSet,
					   _PropagationSetting=propagate, Acc ) ->
	[ _Propagate={ skip, true } | Acc ];

% End of recursion, propagation explicitly denied by the user:
to_wx_connect_options( _Opts=[], _EventType, _TrapSet,
					   _PropagationSetting=trap, Acc ) ->
	% As skip=False is the default:
	%[ _Trap={ skip, false } | Acc ];
	Acc;

% End of recursion, no user-defined propagation setting, applying thus per-type
% defaults:
%
to_wx_connect_options( _Opts=[], EventType, TrapSet,
					   _PropagationSetting=undefined, Acc ) ->

	case set_utils:member( EventType, TrapSet ) of

		true ->
			% As skip=False is the default:
			%[ _Trap={ skip, false } | Acc ];
			Acc;

		false ->
			[ _Propagate={ skip, true } | Acc ]

	end;

to_wx_connect_options( _Opts=[ P={ id, _I } | T ], EventType, TrapSet,
					   PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet, PropagationSetting,
						   [ P | Acc ] );

to_wx_connect_options( _Opts=[ { last_id, I } | T ], EventType, TrapSet,
					   PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet, PropagationSetting,
						   [ { lastId, I } | Acc ] );

to_wx_connect_options( _Opts=[ trap_event | T ], EventType, TrapSet,
					   _PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet,
						   _ForcedPropagationSetting=trap, Acc );

to_wx_connect_options( _Opts=[ propagate_event | T ], EventType, TrapSet,
					   _PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet,
						   _ForcedPropagationSetting=propagate, Acc );

to_wx_connect_options( _Opts=[ callback | T ], EventType, TrapSet,
					   PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet, PropagationSetting,
						   [ callback | Acc ] );

to_wx_connect_options( _Opts=[ P={ callback, _F } | T ], EventType, TrapSet,
					   PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet, PropagationSetting,
						   [ P | Acc ] );

to_wx_connect_options( _Opts=[ { user_data, D } | T ], EventType, TrapSet,
					   PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet, PropagationSetting,
						   [ { userData, D } | Acc ] );

to_wx_connect_options( _Opts=[ Other | _T ], _EventType, _TrapSet,
					   _PropagationSetting, _Acc ) ->
	throw( { invalid_event_subscription_option, Other } ).



% @doc Unsubscribes the current process from the specified object, for all event
% types.
%
% The meaning of the returned boolean is not specified, presumably whether the
% operation went well.
%
-spec disconnect( event_source() ) -> boolean().
disconnect( _SourceObject=#canvas_state{ panel=Panel } ) ->
	disconnect( Panel );

disconnect( SourceObject ) ->
	wxEvtHandler:disconnect( SourceObject ).



% @doc Unsubscribes the current process from the specified object, for the
% specified event type(s).
%
% The meaning of the returned boolean is not specified, presumably whether the
% operation went well.
%
-spec disconnect( event_source(), maybe_list( event_type() ) ) -> boolean().
disconnect( SourceObject, EventTypes ) when is_list( EventTypes ) ->
	[ disconnect( SourceObject, ET ) || ET <- EventTypes ];

% Single event type now:
disconnect( #canvas_state{ panel=Panel }, EventType ) ->
	disconnect( Panel, EventType );

disconnect( SourceObject, EventType ) ->

	WxEventType = to_wx_event_type( EventType ),

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( " - disconnecting event source '~ts' from ~w "
			"for ~p (i.e. ~p).",
			[ gui:object_to_string( SourceObject ), self(), EventType,
			  WxEventType ] ) ),

	wxEvtHandler:disconnect( SourceObject, WxEventType ).
