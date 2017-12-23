% Copyright (C) 2010-2017 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Monday, February 15, 2010.


% Gathering of various facilities for Graphical User Interfaces.
%
% We name this library MyriadGUI (shortened here in 'gui' whenever it is not
% ambiguous).
%
% The purpose of MyriadGUI is to wrap, complement and improve what we consider
% the best (set of) gui backend available (previously: gs; now: wx+esdl, with
% OpenGL), for classical applications and multimedia ones (ex: games).
%
% See gui_test.erl for the corresponding test.
%
-module(gui).


% For the canvas_state record:
-include("gui_canvas.hrl").


-include("polygon.hrl").




% Rendering of GUI elements.
%
% Formerly based on the gs backend, now on the wx one.
%
% Providing improved and enriched APIs for all kinds of GUI.
%
% Relying optionally on:
%
% - OpenGL, for efficient 3D rendering
%
% - esdl, for adequate lower-level primitives (ex: management of input devices)



% Implementation notes:

% This module used to rely on the gs module, whose API was quite simple and
% elegant.
%
% As 'gs' was replaced (quite quickly, unfortunately) by 'wx' (an Erlang binding
% to wxWidgets), now we rely on the latter.
%
% The general convention is still to put as first argument the object on which
% the operation is to be applied (ex: the window).
%
% See also: gui_canvas.erl for all canvas-related operations.


% Usually a class of wxWidgets is represented as a module in Erlang.
%
% GUI objects (e.g. widgets) correspond to (Erlang) processes. In wx the user
% code handles gui_object() instances, which are actually just references onto
% the actual instances that are stored internally by wx.
%
% GUI objects are created with new/*, and deleted with destroy/1.
%
% Events can be managed as messages or callbacks. We generally prefer the former
% (messages can be selectively received, any context can be kept, no temporary
% process created, no wx include needed, etc.).
%
% Whether an event shall be also dispatched to subsequent handlers may be
% decided by using propagate_event/1.
%
% Event messages are internally converted, in order to hide the wx backend,
% augment it with other primitives (ex: canvas widget) and make them compliant
% with the MyriadGUI conventions, as seen by the user code (hint: these are the
% WOOPER ones).
%
% Regarding events, see also:
% https://wiki.wxwidgets.org/Events#Event.Skip_and_Event.Veto

% Please refer to wx.pdf for more architecture/implementation details of wx.


% Understanding wx base widgets hierarchy (offsets meaning "inheriting from",
% corresponding local types specified between brackets):
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


% In a more detailed view, the corresponding local types specified between
% brackets:
%
% - wxObject [gui_object]: root class of all wxWidgets classes
%
%   - wxEvtHandler [event_handler]: a class that can handle events from the
%   windowing system
%
%      - wxWindow [window]: the base class for all windows and represents any
%      visible object on screen For colors, refer to the color module
%
%        - wxControl: base class for a control or "widget"; generally a small
%        window which processes user input and/or displays one or more item of
%        data
%
%          - wxButton: a control that contains a text string, and is one of the
%          most common elements of a GUI; may be placed on any window, notably
%          dialog boxes or panels
%
%        - wxPanel [panel]: a window on which controls are placed; usually
%        placed within a frame
%
%        - wxTopLevelWindow: abstract base class for wxDialog and wxFrame
%
%           - wxDialog: a window with a title bar and sometimes a system menu,
%           which can be moved around the screen; often used to allow the user
%           to make some choice or to answer a question
%
%           - wxFrame [frame]: a window whose size and position can (usually) be
%           changed by the user. It usually has thick borders and a title bar,
%           and can optionally contain a menu bar, toolbar and status bar. A
%           frame can contain any window that is not a frame or dialog
%
%        - wxStatusBar: narrow window that can be placed along the bottom of a
%        frame
%
%   - wxSizer (also derives from wxClientDataContainer): abstract base class
%   used for laying out subwindows in a window


% Additional widgets
%
% Some types of widgets seem to be lacking to WxWidgets, such as canvases that
% would be first-level citizens (ex: able to emit and receive events, when
% needing repaint or being resized).
%
% To support them, we defined gui_object_ref() to complement wx_object() (all
% widgets are thus gui_object_ref()), and we maintain our own instance table.
%
% For that we mimic the mode of operation of wx; no naming service is used,
% instead the process dictionary stores a (MyriadGUI) environment (like the wx
% one).



% Event loops.
%
% There is generally two loops involved here:

% - a mandatory, generic, MyriadGUI-internal one, looping over main_loop/4 with
% an (opaque) gui:loop_state(), running on a dedicated process spawned by
% gui:receive_events/2
%
% - an application-specific one, fed by the former loop (ex: with onWindowClosed
% messages), possibly using any client-side state of interest (whose definition
% is fully free)




% Environments.



-record( gui_env, {

	% Identifier of the current top-level wx server:
	server_id = undefined :: server_id(),

	% PID of the main loop (if any):
	loop_pid = undefined :: pid()

}).


% Stores the current, user-side (client) state (merely references) of the GUI.
%
% Like wx:wx_env(); kept in the process dictionary for easier sharing that if
% using a naming service or having to keep around a bound variable.
%
-type gui_env() :: #gui_env{}.



% Event messages.
%
% Lower-level, backend-specific events are translated in MyriadGUI event
% messages, to be received by their respective event subscribers.
%
% An event message is a pair whose first element is the event type, as an atom
% (ex: onWindowClosed), and whose second element is a list, whose first element
% is the GUI object that generated that event (the closed window, here), and
% whose last element is the event context:
%
% { event_type(), [ gui_object(), ..., event_context() ] }
%
% Ex: { onWindowClosed, [ Window, CloseContext ] }.
%
% Note: these messages respect the WOOPER conventions, and this is done on
% purpose, to facilitate any integration with upper layers.
%
-type event_message() :: { event_type(), [ any() ] }.



% Basic GUI operations.
%
-export([ start/0, start/1, set_debug_level/1, stop/0 ]).



% Event-related operations.
%
-export([ receive_events/1, receive_events/2, propagate_event/1 ]).



% Stringification section.
%
% (mostly internal purpose)
%
-export([ object_to_string/1, context_to_string/1, event_table_to_string/1 ]).


% Temporary exports:
%
% (to avoid warnings about unused functions)
%
-export([ to_wx_object_type/1 ]).



% Widget-related section.


% General-purpose:
%
-export([ id_to_window/1, connect/2, connect/3, disconnect/1, set_tooltip/2 ]).




% Windows:
%
-export([ create_window/0, create_window/1, create_window/2, create_window/5,
		  set_background_color/2, set_sizer/2, show/1, hide/1, get_size/1,
		  destruct_window/1 ]).


% Frames:
%
% Note that a frame is a top_level_window(), a window() and an event_handler(),
% and thus can use their methods.
%
-export([ create_frame/0, create_frame/1, create_frame/2, create_frame/3,
		  create_frame/4, create_frame/6 ]).


% Panels:
%
-export([ create_panel/0, create_panel/1, create_panel/2, create_panel/4,
		  create_panel/5, create_panel/6 ]).


% Buttons:
%
-export([ create_button/2, create_button/6, create_buttons/2 ]).


% Sizers:
%
-export([ create_sizer/1, create_sizer_with_box/2,
		  create_sizer_with_labelled_box/3, add_to_sizer/2, add_to_sizer/3,
		  clear_sizer/1, clear_sizer/2 ]).


% Status bars:
%
-export([ create_status_bar/1, push_status_text/2 ]).


% Canvas support (forwarded to gui_canvas).
%
-export([ create_canvas/1 ]).



% For related, public defines:
-include("gui.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").




% Type declarations:

-type length() :: linear:distance().
-type coordinate() :: linear:integer_coordinate().


% linear_2D:point() would allow for floating-point coordinates:
-type point() :: linear_2D:integer_point().

-type position() :: point() | 'auto'.

% Default position, chosen by either the windowing system or wxWidgets,
% depending on platform:
%
-define( wx_default_position, { -1, -1 } ).



-type size() :: { linear:integer_distance(), linear:integer_distance() }
			  | 'auto'.


% Default size, chosen by either the windowing system or wxWidgets,
% depending on platform:
%
-define( wx_default_size, { -1, -1 } ).



% The identifier of a wx element is an integer (positive or not).



% Allows to specify a 'void' (null) ID of a GUI element.
%
% Sometimes the ID may be directly provided by the user or have a predefined
% value, such as wxID_OPEN; see
% http://docs.wxwidgets.org/2.8.12/wx_stockitems.html#stockitems for a list
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
% (note: this type is defined and exported, yet reported unknown by Dialyzer)
%
-type wx_id() :: 'undefined' | integer().


% See any_id, no_parent, etc. as defined in gui.hrl.


% A vertical orientation means piling elements top to bottom for example, while
% an horizontal means left to right, for example.
%
-type orientation() :: 'vertical' | 'horizontal'.



% Widget types.

% Native wx object types.
%
% No enumeration like 'wxWindow' | 'wxFrame' | ... found in wx, so:
%
-type wx_object_type() :: atom().


% Internal types for all GUI objects:
%
-type object_type() :: wx_object_type() | myriad_object_type().


% MyriadGUI-translated version of a native wx type, i.e. of the wx_object_type()
%
% (ex: 'window', instead of 'wxWindow'):
%
-type wx_object_type() :: 'object'
						| 'event_handler'
						| 'window'
						| 'control'
						| 'button'
						| 'panel'
						| 'status_bar'
						| 'top_level_window'
						| 'dialog'
						| 'frame'
						| 'sizer'.


% Additional widget types introduced by MyriadGUI:
%
-type myriad_object_type() :: 'canvas'.


% Records the actual state of a MyriadGUI object:
%
-type myriad_object_state() :: canvas_state().



% The construction parameters of a MyriadGUI object:
%
-type construction_parameters() :: any().


% Instance identifier (null not allowed):
-type myriad_instance_id() :: basic_utils:non_null_count()



% A reference onto a GUI object, for the widgets that MyriadGUI added to the
% backend at hand.
%
% Results in terms such as: { gui_object_ref, canvas, 12 }.
%
-record( gui_object_ref, {

		% The type of GUI object referred to (ex: 'canvas'):
		object_type :: myriad_object_type(),

		% The identifier of this referenced instance:
		myriad_instance_id :: myriad_instance_id()

}).



-type gui_object_ref() :: #gui_object_ref{}.


% Reference to a GUI object (often designated as "widget" here), somewhat akin
% to a PID.
%
% (ex: {wx_ref,35,wxFrame,[]})
%
-type gui_object() :: wx:wx_object() | gui_object_ref().


-type window() :: 'undefined' | wxWindow:wxWindow() | gui_canvas:canvas().

-type frame() :: wxFrame:wxFrame().

-type panel() :: wxPanel:wxPanel().

-type button() :: wxButton:wxButton().

-type sizer() :: wxSizer:wxSizer().

% Elements that can be included in a sizer:
-type sizer_child() :: window() | sizer().

-type sizer_item() :: wxSizerItem:wxSizerItem().

-type status_bar() :: wxStatusBar:wxStatusBar().

-type bitmap() :: wxBitmap:wxBitmap().

-type back_buffer() :: wxMemoryDC:wxMemoryDC().



% Shorthands:

-type void() :: basic_utils:void().
-type text() :: text_utils:unicode_string().


% Aliases:

-type title() :: text().
-type label() :: text().


% User data, as specified in the connect call:
-type user_data() :: any().




% Event management.
%
% In general we promote managing events thanks to messages rather than thanks to
% callbacks, as the former can access easily to the full context of the program
% whereas the latter is only executed into a transient process.


-type event_handler() :: wxEvtHandler:wxEvtHandler().

-type event_source() :: event_handler() | gui_canvas:canvas().


% Using the wx-event type, leaked by wx.hrl:
%
% (add them whenever needed)
%
-type wx_event_type() :: wx_close_event_type().


% Associated to wxCloseEvent:
-type wx_close_event_type() :: 'close_window' | 'end_session'
							 | 'query_end_session'.


% Our own event types, independent from any backend:
%
-type event_type() :: 'onWindowClosed'
					  .




% The PID of an event subscriber:
-type event_subscriber_pid() :: pid().


% So that user process(es) can subscribe to GUI events:
%
-type event_subscription() ::
		{ list_utils:maybe_list( event_type() ),
		  list_utils:maybe_list( gui_object() ),
		  list_utils:maybe_list( event_subscriber_pid() ) }.


% Specifies, for any combination of type of events and GUI objects, the event
% listener (by default: the calling process) that subscribes to the
% corresponding actual events.
%
% Note: no use case for more than one subscriber found.
%
-type event_subscription_spec() :: list_utils:maybe_list(
									 event_subscription() ).


% An indirection table dispatching events according to subscription
% specifications.
%
% For an incoming event, we see it (virtually) as a table(
% { gui_object(), event_type() }, set_utils:set( event_subscriber_pid() ) ):
%
% - the first key is the GUI object (e.g. widget) from which it emanates (ex: a
% frame)
%
% - the second key is its corresponding (internal) event type (ex:
% 'onWindowClosed')
%
% - the associated value is a list/set of the PID of the subscribers regarding
% this (object,event) combination
%
% Note: two nested tables (one table(), one list_table()) are used also in
% order to ensure that there is up to one entry per GUI object and per event
% type stored.
%
-type event_table() :: table:table( gui_object(), event_dispatch_table() ).


% Tells, for a given event type (e.g. in the context of a specific GUI object),
% to which event subscribers the corresponding GUI messages shall be sent.
%
-type event_dispatch_table() :: list_table:table( event_type(),
												  [ event_subscriber_pid() ] ).


% Identifier of a (generally, the) overall wx server:
-type server_id() :: gui_object().



% To replace source events (objects) by others:
-type reassign_table() :: table:table( gui_object(), gui_object() ).


% To store our own instances (sorted by types) and manage them like wx native
% objects.
%
% Keys are like 'canvas'.
%
-type gui_type_table() :: table:table( myriad_object_type(),
									   gui_instance_table() ).


% To store, for a given internal type, a table whose keys are the identifiers of
% the GUIMyriad objects of that type, and whose values are the actual state of
% these instances.
%
-type gui_instance_table() :: table:table( myriad_instance_id(),
										   myriad_object_state() ).





% Stores the current, internal side state of the GUI, as managed by the main
% loop.
%
-record( loop_state, {

		   % Identifier (if any) of the current top-level wx server:
		   server_id ::  gui_object(),

		   % To dispatch appropriately the backend events:
		   event_table :: event_table(),

		   % Allows to replace an event source by another.
		   %
		   % For example useful when having defined a canvas (which thus embeds
		   % a wx panel): when the internal event loop receives a 'paint' wx
		   % event for that wx panel, the actual object referred to by the GUI
		   % message that we will send to the user code shall not be that panel,
		   % but the canvas that owns it (for example so that other elements of
		   % that canvas can then be used when the user code processes this
		   % event - like the bitmap or the back-buffer of this canvas).
		   %
		   reassign_table :: reassign_table(),


		   % Stores the widget instances that have been introduced to complement
		   % the backend (ex: canvas instances).
		   %
		   gui_instance_table :: gui_instance_table()

}).

-type loop_state() :: #loop_state{}.



% A #wx event record comprises:
%
% - (the 'wx' record tag, if seen as a tuple)
%
% - id :: wx_id() the (integer) identifier of the object (e.g. widget) that
% received the event (event source)
%
% - obj :: gui_object() is the reference of the object that was specified in the
% connect/n call, i.e. on which connect/n was called (ex:
% {wx_ref,35,wxFrame,[]})
%
% - userData :: any() is the user-specified data that was specified in the
% connect/n call (typically [])
%
% - event :: event() is the information about the event itself, i.e. the
% corresponding record (ex: {wxClose,close_window}), whose first element is the
% type of the event (ex: close_window)


% Current backend is wx (WxWidgets).
%
-type backend_event() :: wx_event().


% The actual event source can be found either directly (through its reference)
% or from its ID (see id_to_window/1). Best option seems to be the first one, in
% the general case.
%
% As always, same as: -record( wx,...
%
% id: the (integer) identifier of the widget
%
-type wx_event() :: { 'wx', wx_id(), gui_object(), user_data(),
					  wx_event_type() }.


% Options for windows, see:
% http://docs.wxwidgets.org/stable/wx_wxwindow.html and
% http://docs.wxwidgets.org/stable/wx_windowstyles.html#windowstyles

-type window_style_opt() ::  'default'
						   | 'simple_border'
						   | 'double_border'
						   | 'sunken_border'
						   | 'raised_border'
						   | 'static_border'
						   | 'theme_border'
						   | 'no_border'
						   | 'transparent'
						   | 'tab_traversable'
						   | 'grab_all_keys'
						   | 'with_vertical_scrollbar'
						   | 'with_horizontal_scrollbar'
						   | 'never_hide_scrollbars'
						   | 'clip_children'
						   | 'grab_all_keys'
						   | 'full_repaint_on_resize'.


-type window_style() :: window_style_opt() | [ window_style_opt() ].

-type window_option() :: { pos, point() }
					   | { size, size() }
					   | { style, [ window_style_opt() ] }.

% Unused: -type window_options() :: [ window_option() ].



% Options for frames, see:
% http://docs.wxwidgets.org/stable/wx_wxframe.html#wxframewxframe
%
-type frame_style_opt() ::   'default'
						   | 'caption'
						   | 'minimize'
						   | 'minimize_box'
						   | 'maximize'
						   | 'maximize_box'
						   | 'close_box'
						   | 'stay_on_top'
						   | 'system_menu'
						   | 'resize_border'
						   | 'tool_window'
						   | 'no_taskbar'.


-type frame_style() :: frame_style_opt() | [ frame_style_opt() ].


% Options for panels, see:
% http://docs.wxwidgets.org/stable/wx_wxpanel.html#wxpanelwxpanel
%
-type panel_option() :: window_option().

-type panel_options() :: [ panel_option() ].



% Options for button style, see:
% http://docs.wxwidgets.org/stable/wx_wxbutton.html#wxbuttonwxbutton
%
-type button_style_opt() ::  'default'
						   | 'left_justified'
						   | 'right_justified'
						   | 'top_justified'
						   | 'bottom_justified'
						   | 'exact_fit'
						   | 'flat'.


-type button_style() :: button_style_opt() | [ button_style_opt() ].



% Options for sizers, see:
% http://docs.wxwidgets.org/stable/wx_wxsizer.html
%
-type sizer_flag_opt() :: 'default'
						| 'top_border'
						| 'bottom_border'
						| 'left_border'
						| 'right_border'
						| 'all_borders'
						| 'expand_fully'
						| 'expand_shaped'
						| 'fixed_size'
						| 'counted_even_if_hidden'
						| 'align_center'
						| 'align_left'
						| 'align_right'
						| 'align_top'
						| 'align_bottom'
						| 'align_center_vertical'
						| 'align_center_horizontal'.


-type sizer_flag() :: sizer_flag_opt() | [ sizer_flag_opt() ].

-type sizer_option() :: { 'proportion', integer() }
					  | { 'flag', sizer_flag() }
					  | { 'border', integer() }
					  | { 'userData', gui_object() }.


-type sizer_options() :: [ sizer_option() ].


% Options for event management connections:
%
-type connect_opt() ::   { 'id', integer() }
					   | { lastId, integer() }
					   | { skip, boolean() }
					   |   callback
					   | { callback, function() }
					   | { userData, term() }.


-type connect_options() :: connect_opt() | [ connect_opt() ].


% Mapped internally to 'none', 'verbose', 'trace', etc.; see
% convert_debug_level/1:
%
-type debug_level_opt() :: 'none' | 'calls' | 'life_cycle'.


-type debug_level() :: debug_level_opt() | [ debug_level_opt() ].

-type error_message() :: term().



-export_type ([ length/0, coordinate/0, point/0, id/0,
				orientation/0,
				window/0, frame/0, panel/0, button/0, sizer/0, status_bar/0,
				bitmap/0, back_buffer/0,
				event_message/0, backend_event/0,
				window_style/0, frame_style/0, button_style/0,
				sizer_flag/0,
				error_message/0 ]).



% GUI-specific defines:
-define( gui_env_process_key, myriad_gui_env ).



% Section for basic GUI overall operations.



% Starts the MyriadGUI subsystem.
%
-spec start() -> void().
start() ->

	% Initialises the wx backend (no option relevant here):
	WxServerId = wx:new(),

	% The wx environment will be exported to the internal main loop process, so
	% that both the user code and that loop can make use of wx:
	%
	WxEnv = wx:get_env(),

	% To identify the default event subscriber, the calling user process:
	Self = self(),

	% The event table must be initialised in the spawned process, so that
	% connect/n can use the right actual, first-level subscriber PID: the
	% internal main loop.

	LoopPid = spawn_link( fun() ->
							main_event_loop( WxServerId, WxEnv, Self )
						  end ),

	trace_utils:trace_fmt( "Main loop running on ~w (created from ~w).",
						   [ LoopPid, self() ] ),

	GUIEnv = #gui_env{ server_id=WxServerId, loop_pid=LoopPid },

	% Stored in the process dictionary of the user process:
	put( ?gui_env_process_key, GUIEnv ).



% Starts the GUI subsystem, with specified debug level.
%
-spec start( debug_level() ) -> void().
start( DebugLevel ) ->
	start(),
	set_debug_level( DebugLevel ).



% Sets the debug level(s) of the GUI.
%
-spec set_debug_level( debug_level() ) -> void().
set_debug_level( DebugLevels ) when is_list( DebugLevels ) ->
	wx:debug( [ convert_debug_level( L ) || L <- DebugLevels ] );

set_debug_level( DebugLevel ) ->
	set_debug_level( [ DebugLevel ] ).



% Creates a new process in charge of managing the internal event loop of the
% GUI, and of dispatching upcoming events according to the user-defined
% subscriptions.
%
% Events received will result in the callback messages defined here (ex:
% onWindowClosed) to be sent to their respective subscribers.
%
% By default the corresponding event will not be transmitted upward in the
% widget hierarchy (as this event will be expected to be processed for good by
% the subscriber(s) it has been dispatched to), unless the propagate_event/1
% function is called from one of them.
%
-spec receive_events( event_subscription_spec() ) -> void().
receive_events( SubscribedEvents ) ->
	receive_events( SubscribedEvents, #gui_env{} ).



% Creates a new process in charge of managing the internal event loop of the
% GUI, and of dispatching upcoming events according to the user-defined
% subscriptions.
%
% Events received will result in the callback messages defined here (ex:
% onWindowClosed) to be sent to their respective subscribers.
%
% By default the corresponding event will not be transmitted upward in the
% widget hierarchy (as this event will be expected to be processed for good by
% the subscriber(s) it has been dispatched to), unless the propagate_event/1
% function is called from one of them.
%
-spec receive_events( event_subscription_spec(),
					 basic_utils:maybe( gui_env() ) ) ->
						   basic_utils:maybe( gui_env() ).
receive_events( SubscribedEvents, GUIState=#gui_env{ server_id=ServerId } ) ->

	trace_utils:trace_fmt( "Entering main loop, with following event "
						   "subscription:~n~p", [ SubscribedEvents ] ),

	GUIState#gui_env{ loop_pid=LoopPid }.




% Creates a new process in charge of managing the internal, main event loop of
% MyriadGUI.
%
% Events received will result in callbacks to be triggered on their respective
% subscribers.
%
% The goal is to devise a generic event loop, while still being able to be
% notified of all relevant information (and only them).
%
-spec main_event_loop( server_id(), wx_env(), pid() ) -> no_return().
main_event_loop( ServerId, WxEnv, CallerPid ) ->

	% To be done first, so that we are able to use wx from that process from now
	% on:
	%
	wx:set_env( WxEnv ),

	EmptyTable = table:new(),

	InitialLoopState = #loop_state{ server_id=ServerId,
									event_table=EmptyTable,
									reassign_table=EmptyTable },

	%trace_utils:debug_fmt( "Starting main MyriadGUI loop." ] ),

	process_event_messages( InitialLoopState ).



% Receives and process all wx-originating messages, on behalf of the main event
% loop.
%
process_event_messages( LoopState ) ->

	%trace_utils:trace( "Waiting for event messages..." ),

	% Event types roughly sorted by decreasing frequency of appearance:
	%
	% (defined in lib/wx/include/wx.hrl)
	%
	NewLoopState = receive

		% wx event received here:
		%
		% Structure: { wx, Id, Obj, UserData, Event } with event:
		% { WxEventName, Type, ...}
		%
		% Ex: { wx, -2006, {wx_ref,35,wxFrame,[]}, [], {wxClose,close_window} }.
		%
		Event=#wx{ id=Id, obj=GUIObject, userData=UserData, event=WxEvent } ->
			process_wx_event( Id, GUIObject, UserData, WxEvent, Event,
							  LoopState );


		% MyriadGUI user request (ex: from gui:create_canvas/1):
		{ createInstance, [ ObjectType, ConstructionParams ], CallerPid } ->
			process_myriad_creation( ObjectType, ConstructionParams,
									 CallerPid, LoopState );


		UnmatchedEvent ->
			trace_utils:warning_fmt( "Ignored following unmatched event "
									 "message:~n~p", [ UnmatchedEvent ] ),
			LoopState

	end,

	process_event_messages( NewLoopState ).




% Processes specified wx event message.
%
-spec process_wx_event( wx_id(), wx_object_type(), user_data(), wx_event(),
						loop_state() ) -> loop_state().
process_wx_event( Id, GUIObject, UserData, WxEvent, LoopState ) ->

			trace_utils:trace_fmt( "Event received about '~s':~n~p.",
								   [ object_to_string( GUIObject ), Event ] ),

			ActualGUIObject = case table:lookupEntry( GUIObject,
													  ReassignTable ) of

				key_not_found ->
					trace_utils:trace_fmt( "Event received about '~s':~n~p.",
								  [ object_to_string( GUIObject ), Event ] ),
					GUIObject;


				{ value, TargetGUIObject } ->
					trace_utils:trace_fmt( "Event received about '~s', "
						   "reassigned to '~s':~n~p.",
						   [ object_to_string( GUIObject ),
							 object_to_string( TargetGUIObject ), Event ] ),

					TargetGUIObject

			end,

			case table:lookupEntry( ActualGUIObject, EventTable ) of

				key_not_found ->
					trace_utils:debug_fmt( "No event subscription for GUI "
					  "object '~s'.", [ object_to_string( ActualGUIObject ) ] ),
					process_event_messages( State );


				{ value, DispatchTable } ->

					% Example: close_window
					WxEventType = element( 2, WxEvent ),

					EventType = from_wx_event_type( WxEventType ),

					case list_table:lookupEntry( EventType, DispatchTable ) of

						{ value, Subscribers } ->

							trace_utils:debug_fmt( "Sending ~p event to "
							  "subscriber ~w.", [ EventType, Subscribers ] ),

							send_event( Subscribers, Event, EventType, Id,
										ActualGUIObject, UserData, WxEvent,
										ReassignTable );

						key_not_found ->
							trace_utils:error_fmt( "For GUI object '~s', event "
								"type '~s' not registered whereas notified "
								"(abnormal).",
								[ object_to_string( ActualGUIObject ),
								  EventType ] )

					end,



% Processes specified MyriadGUI event message.
%
-spec process_myriad_creation( myriad_object_type(), construction_parameters(),
							   pid(), loop_state() ) -> loop_state().
process_myriad_creation( ObjectType, ConstructionParams, CallerPid,
						 LoopState ) ->

			create_instance( ObjectType, ConstructionParams,
			trace_utils:debug_fmt( "Instance creation request received from "
								   "~w, for type ~s, with construction "
								   "parameters ~p.", [ CallerPid, ObjectType,
													   ConstructionParams ] ),

			{ ObjectInitialState, NewReassignTable } = case ObjectType of

				canvas ->
					{ CanvasInitialState, PanelRef } =
							gui_canvas:create_instance( ConstructionParams ),

					{ CanvasRef, NewInstanceTable } = register_instance(
						  ObjectType, CanvasInitialState,  InstanceTable ),

					CanvReassignTable = table:addNewEntry( PanelRef, CanvasRef,
														  ReassignTable ),
					{ CanvasInitialState, CanvReassignTable }

			end,

			{ ObjectRef, NewInstanceTable } = register_instance( ObjectType,
									ObjectInitialState, InstanceTable ),

			CallerPid ! { instance_created, ObjectType, ObjectRef },





% (helper)
-spec send_event( [ event_subscriber_pid() ], event(), event_type(), wx_id(),
					gui_object(), user_data(), wx_event(), reassign_table() ) ->
						void().
send_event( Subscribers, Event, EventType, Id, GUIObject, UserData, WxEvent,
			ReassignTable ) ->

	Context = #gui_event_context{ id=Id, user_data=UserData,
								  backend_event=WxEvent },

	Msg = { EventType, [ GUIObject, Context ] },

	[ SubPid ! Msg || SubPid <- Subscribers ].



% Propagates the event designated by the specified context upward in the widget
% hierarchy (instead of the default, which is considering that it has been
% processed once for all, and thus shall not be propagated further).
%
% Events are handled in order, from bottom to top in the widgets hierarchy, by
% the last subscribed handler first. Most of the events have default event
% handler(s) set.
%
% As a result, calling this function results in having the corresponding event
% handled by the other handler(s) afterwards.
%
% In general, it is recommended to propagate all non-command events to allow the
% default handling to take place. The command events are, however, normally not
% propagated as usually a single command such as a button click or menu item
% selection must only be processed by one handler.
%
% Note: to be called from an event handler, i.e. at least from a process which
% set the wx environment.
%
-spec propagate_event( gui_event_context() ) -> void().
propagate_event( #gui_event_context{ backend_event=WxEvent } ) ->

	% Honestly the skip semantics looks a bit unclear.
	% 'skip' is here a synonymous of 'propagate'.

	% Default is having skip=true, so same as:
	% wxEvent:skip( WxEvent, _Opts=[ { skip, true } ] ):
	wxEvent:skip( WxEvent ).



% Stops the GUI subsystem.
%
-spec stop() -> void().
stop() ->

	% No server_id needed:
	ok = wx:destroy(),

	% Leaves the process dictionary:
	set( ?gui_env_process_key, _Value=undefined ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Event section.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Registers specified event subscriptions.
%
% (helper)
%
-spec register_event_subscriptions( event_subscription_spec(), pid(),
									server_id() ) -> loop_state().
register_event_subscriptions( SubscribedEvents, CallerPid, ServerId ) ->

	EmptyTable = table:new(),

	BlankLoopState = #loop_state{ server_id=ServerId,
								  event_table=EmptyTable,
								  reassign_table=EmptyTable },

	% The caller is the default susbscriber process:
	LoopState = update_event_loop_tables( SubscribedEvents, CallerPid,
										  BlankLoopState ),

	trace_utils:debug_fmt( "Initial tables:~n - ~s~n - ~s~n",
		[ event_table_to_string( LoopState#loop_state.event_table ),
		  reassign_table_to_string( LoopState#loop_state.reassign_table ) ] ),

	LoopState.




% Enriches the specified event table with specified subscription information.
%
% (helper)
%
-spec update_event_loop_tables( event_subscription_spec(),
		event_subscriber_pid(), loop_state() ) -> loop_state().
update_event_loop_tables( _SubscribedEvents=[], _DefaultSubscriberPid,
						  LoopState ) ->
	LoopState;

update_event_loop_tables( _SubscribedEvents=[
	   { EventTypeMaybeList, GUIObjectMaybeList, SubscriberMaybeList } | T ],
	   DefaultSubscriberPid, LoopState ) ->

	EventTypeList = list_utils:ensure_list_of_atoms( EventTypeMaybeList ),
	GUIObjectList = list_utils:ensure_list_of_tuples( GUIObjectMaybeList ),
	SubscriberList= list_utils:ensure_list_of_pids( SubscriberMaybeList ),

	NewLoopState = lists:foldl( fun( Obj, AccState ) ->
								   register_event_types_for( Obj, EventTypeList,
											SubscriberList, AccState )
							end,
							_Acc0=LoopState,
							_List=GUIObjectList ),

	update_event_loop_tables( T, DefaultSubscriberPid, NewLoopState );

update_event_loop_tables( _SubscribedEvents=[
	   { EventTypeMaybeList, GUIObjectMaybeList } | T ],
	   DefaultSubscriberPid, LoopState ) ->
	update_event_loop_tables( [ { EventTypeMaybeList, GUIObjectMaybeList,
							  [ DefaultSubscriberPid ] } | T ],
							DefaultSubscriberPid, LoopState ).


% (helper)
%
-spec register_event_types_for( gui_object(), [ event_type() ],
				[ event_subscriber_pid() ], loop_state() ) -> loop_state().
register_event_types_for( Canvas=#canvas{ panel=Panel }, EventTypes,
						  Subscribers, LoopState=#loop_state{
											event_table=EventTable,
											reassign_table=ReassignTable } ) ->

	trace_utils:debug_fmt( "Registering subscribers ~w for event types ~p "
						   "regarding canvas '~s'.", [ Subscribers, EventTypes,
							 object_to_string( Canvas ) ] ),

	% A canvas is registered in wx as a panel (as wx will send events about it)
	% that will be reassigned as a canvas:

	NewEventTable = record_subscriptions( Canvas, EventTypes, Subscribers,
										  EventTable ),

	% Will defer all events (paint, size) of the underlying panel to the canvas:

	[ connect( Panel, EvType ) || EvType <- EventTypes ],

	NewReassignTable = table:addNewEntry( Panel, Canvas, ReassignTable ),

	LoopState#loop_state{ event_table=NewEventTable,
						  reassign_table=NewReassignTable };


register_event_types_for( GUIObject, EventTypes, Subscribers,
						  LoopState=#loop_state{ event_table=EventTable } ) ->

	trace_utils:debug_fmt( "Registering subscribers ~w for event types ~p "
						   "regarding object '~s'.", [ Subscribers, EventTypes,
							 object_to_string( GUIObject ) ] ),

	% Auto-connection to the current PID (i.e. the one of the internal, main
	% event loop), so that it receives these events for their upcoming
	% dispatching to the actual subscribers:
	%
	[ connect( GUIObject, EvType ) || EvType <- EventTypes ],

	% Now prepare the upcoming routing to the right subscriber:
	%
	NewEventTable = record_subscriptions( GUIObject, EventTypes, Subscribers,
										  EventTable ),

	LoopState#loop_state{ event_table=NewEventTable }.



% (helper)
%
-spec record_subscriptions( gui_object(), [ event_type() ],
			[ event_subscriber_pid() ], event_table() ) -> event_table().
record_subscriptions( GUIObject, EventTypes, Subscribers, EventTable ) ->

	NewDispatchTable= case table:lookupEntry( GUIObject, EventTable ) of

		key_not_found ->
			UniqueSubscribers = list_utils:uniquify( Subscribers ),
			Entries = [ { EvType, UniqueSubscribers } || EvType <- EventTypes ],
			list_table:new( Entries );

		{ value, DispatchTable } ->
			update_event_table( EventTypes, Subscribers, DispatchTable )

	end,

	table:addEntry( GUIObject, NewDispatchTable, EventTable ),


% Returns an event dispatch table recording specified event type / subscriber
% associations.
%
-spec update_event_table( [ event_type() ], [ event_subscriber_pid() ],
						  event_dispatch_table() ) -> event_dispatch_table().
update_event_table( _EventTypes=[], _Subscribers, DispatchTable ) ->
	DispatchTable;

update_event_table( _EventTypes=[ EventType | T ], Subscribers,
					DispatchTable ) ->

	NewSubscribers = case list_table:lookupEntry( EventType,
												  DispatchTable ) of

		key_not_found ->
			list_utils:uniquify( Subscribers );

		{ value, CurrentSubscribers } ->
			list_utils:union( CurrentSubscribers, Subscribers )

	end,

	NewDispatchTable = list_table:addEntry( EventType, NewSubscribers,
											DispatchTable ),

	update_event_table( T, Subscribers, NewDispatchTable ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Widget section.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Common section.


% Returns the widget corresponding to the specified identifier.
%
% (internal use only)
%
-spec id_to_window( wx_id() ) -> window().
id_to_window( Id ) ->
	wxWindow:findWindowById( Id ).



% Subscribes the current process to the specified type of event of the specified
% object (receiving for that a message).
%
% Said otherwise: requests the specified widget to send a message-based event
% when the specified kind of event happens, overriding its default behaviour.
%
% Note: only useful internally or when bypasssing the default main loop.
%
-spec connect( event_source(), event_type() ) -> void().
connect( #canvas{ panel=Panel }, EventType ) ->
	connect( Panel, EventType );

connect( EventSource, EventType ) ->
	connect( EventSource, EventType, _Options=[] ).



% Subscribes the current process to the specified type of event of the specified
% object (receiving for that a message).
%
% Said otherwise: requests the specified widget to send a message-based event
% when the specified kind of event happens, overriding its default behaviour
% based on specified options.
%
% Note: only useful internally or when bypasssing the default main loop.
%
-spec connect( event_source(), event_type(), connect_options() ) -> void().
connect( #canvas{ panel=Panel }, EventType, Options ) ->
	connect( Panel, EventType, Options );

connect( SourceObject, EventType, Options ) ->

	% Events to be processed through messages, not callbacks:
	WxEventType = to_wx_event_type( EventType ),

	trace_utils:debug_fmt( "Connecting event source '~s' to ~w for ~p.",
						   [ object_to_string( SourceObject ), self(),
							 EventType ] ),

	wxEvtHandler:connect( SourceObject, WxEventType, Options ).



% Unsubscribes the event source, for all event types.
%
-spec disconnect( event_source() ) -> boolean().
disconnect( #canvas{ panel=Panel } ) ->
	disconnect( Panel );

disconnect( EventSource ) ->
		wxEvtHandler:disconnect( EventSource ).



% Attaches a tooltip to specified widget.
%
-spec set_tooltip( window(), label() ) -> void().
set_tooltip( #canvas{ panel=Panel }, Label ) ->
	set_tooltip( Panel, Label );

set_tooltip( Window, Label ) ->

	%trace_utils:debug_fmt( "Setting tooltip '~s' to ~s.",
	%					   [ Label, object_to_string( Window ) ] ),

	% For an unknown reason, works on panels but never on buttons:
	wxWindow:setToolTip( Window, Label ).



% Window section.
%
% Base class for all windows and represents any visible object on screen. All
% controls, top level windows and so on are windows. Sizers and device contexts
% are not, however, as they do not appear on screen themselves.


% Creates a new window.
%
-spec create_window() -> window().
create_window() ->
	wxWindow:new().


% (internal use only)
%
-spec create_window( wx_id(), window() ) -> window().
create_window( Id, Parent ) ->

	ActualId = get_id( Id ),
	ActualParent = get_parent( Parent ),

	wxWindow:new( ActualParent, ActualId ).


-spec create_window( size() ) -> window().
create_window( Size ) ->

	ActualId = get_id( undefined ),
	ActualParent = get_parent( undefined ),

	Options =  [ get_wx_size( Size ) ],

	wxWindow:new( ActualParent, ActualId, Options ).



% (internal use only)
%
-spec create_window( position(), size(), window_style(), wx_id(), window() ) ->
						   window().
create_window( Position, Size, Style, Id, Parent ) ->

	Options = [ get_wx_position( Position ), get_wx_size( Size ),
				 { style, window_style_to_bitmask( Style ) } ],

	ActualId = get_id( Id ),
	ActualParent = get_parent( Parent ),

	wxWindow:new( ActualParent, ActualId, Options ).



% Sets the background color of the specified window.
%
-spec set_background_color( window(), gui_color:color() ) -> void().
set_background_color( #canvas{ back_buffer=BackBuffer }, Color ) ->

	% Must not be used, other double-deallocation core dump:
	%_PreviousBrush = wxMemoryDC:getBrush( BackBuffer ),
	%wxBrush:destroy( PreviousBrush ),

	ActualColor = gui_color:get_color( Color ),

	NewBrush = wxBrush:new( ActualColor ),

	wxMemoryDC:setBackground( BackBuffer, NewBrush );


set_background_color( Window, Color ) ->

	ActualColor = gui_color:get_color( Color ),

	wxWindow:setBackgroundColour( Window, ActualColor ).



% Associates specified sizer to specified window.
%
-spec set_sizer( window(), sizer() ) -> void().
set_sizer( #canvas{ panel=Panel }, Sizer ) ->
	set_sizer( Panel, Sizer );

set_sizer( Window, Sizer ) ->
	wxWindow:setSizer( Window, Sizer ).



% Shows (renders) specified window (or subclass thereof).
%
% Returns whether anything had to be done.
%
% This is the place where all widgets resolve their positions, sizes and
% contents.
%
-spec show( window() | [ window() ] ) -> boolean().
show( Windows ) when is_list( Windows )->
	show_helper( Windows, _Acc=false );

show( Window ) ->
	wxWindow:show( Window ).


show_helper( _Windows=[], Acc ) ->
	Acc;

show_helper( _Windows=[ W | T ], Acc ) ->
	NewAcc = show( W ) orelse Acc,
	show_helper( T, NewAcc ).



% Hides specified window.
%
% Returns whether anything had to be done.
%
-spec hide( window() ) -> boolean().
hide( Window ) ->
	wxWindow:show( Window, [ { show, false } ] ).



% Returns the size (as a 2D vector, i.e. {Width,Height}) of specified window.
%
-spec get_size( window() ) -> linear_2D:vector().
get_size( Window ) ->
	wxWindow:getSize( Window ).


% Destructs specified window.
%
-spec destruct_window( window() ) -> void().
destruct_window( Window ) ->
	wxWindow:destroy( Window ).



% Frame section.
%
% A frame is a window whose size and position can (usually) be changed by the
% user. It usually has thick borders and a title bar, and can optionally contain
% a menu bar, toolbar and status bar. A frame can contain any window that is not
% a frame or dialog.
%
% Source: http://docs.wxwidgets.org/stable/wx_wxframe.html#wxframewxframe


% Creates a new frame, with default title, ID, parent, position, size and style.
%
% Note: this version apparently does not correctly initialise the frame;
% following error is indeed reported:
% "wxWidgets Assert failure: ./src/gtk/toplevel.cpp(988): \"m_widget\" in Show()
% : invalid frame".
%
-spec create_frame() -> frame().
create_frame() ->
	wxFrame:new().


% Creates a new frame, with default position, size, style, ID and parent.
%
-spec create_frame( title() ) -> frame().
create_frame( Title ) ->
	wxFrame:new( get_parent( undefined ), get_id( undefined ), Title ).



% Creates a new frame, with specified size, and default ID and parent.
%
-spec create_frame( title(), size() ) -> frame().
create_frame( Title, Size ) ->

	Options =  [ get_wx_size( Size ) ],

	%trace_utils:debug_fmt( "create_frame options: ~p.", [ Options ] ),

	wxFrame:new( get_parent( undefined ), get_id( undefined ), Title, Options ).



% Creates a new frame, with default position, size and style.
%
% (internal use only)
%
-spec create_frame( title(), wx_id(), window() ) -> frame().
create_frame( Title, Id, Parent ) ->
	wxFrame:new( get_parent( Parent ), get_id( Id ), Title ).


% Creates a new frame, with default parent.
%
-spec create_frame( title(), position(), size(), frame_style() ) -> frame().
create_frame( Title, Position, Size, Style ) ->

	Options =  [ get_wx_position( Position ), get_wx_size( Size ),
				 { style, frame_style_to_bitmask( Style ) } ],

	%trace_utils:debug_fmt( "create_frame options: ~p.", [ Options ] ),

	wxFrame:new( get_parent( undefined ), get_id( undefined ), Title, Options ).



% Creates a new frame.
%
% (internal use only)
%
-spec create_frame( title(), position(), size(), frame_style(), id(),
					window() ) -> frame().
create_frame( Title, Position, Size, Style, Id, Parent ) ->

	Options =  [ get_wx_position( Position ), get_wx_size( Size ),
				 { style, frame_style_to_bitmask( Style ) } ],

	ActualId = get_id( Id ),

	ActualParent = get_parent( Parent ),

	wxFrame:new( ActualParent, ActualId, Title, Options ).




% Panel section.


% Creates a new panel.
%
-spec create_panel() -> panel().
create_panel() ->
	wxPanel:new().



% Creates a new panel, associated to specified parent.
%
-spec create_panel( window() ) -> panel().
create_panel( Parent ) ->
	wxPanel:new( Parent ).


% Creates a new panel, associated to specified parent and with specified
% options.
%
-spec create_panel( window(), panel_options() ) -> panel().
create_panel( Parent, Options ) ->

	ActualOptions = get_panel_options( Options ),

	wxPanel:new( Parent, ActualOptions ).



% Creates a new panel, associated to specified parent and with specified
% position and dimensions.
%
-spec create_panel( window(), coordinate(), coordinate(),
					length(), length() ) -> panel().
create_panel( Parent, X, Y, Width, Height ) ->
	create_panel( Parent, _Pos={ X, Y }, _Size={ Width, Height } ).



% Creates a new panel, associated to specified parent and with specified
% position and dimensions.
%
-spec create_panel( window(), position(), size() ) -> panel().
create_panel( Parent, Position, Size ) ->
	wxPanel:new( Parent, _Opts=[ { pos, get_wx_position( Position ) },
								 { size, get_wx_size( Size ) } ] ).


% Creates a new panel, associated to specified parent and with specified
% position and dimensions.
%
-spec create_panel( window(), position(), size(), panel_options() ) -> panel().
create_panel( Parent, Position, Size, Options ) ->

	FullOptions = get_panel_options( Options )
		++ [ get_wx_position( Position ), get_wx_size( Size ) ],

	%trace_utils:debug_fmt( "Creating panel: parent: ~w, position: ~w, "
	%					   "size: ~w, options: ~w, full options: ~w.",
	%					   [ Parent, Position, Size, Options, FullOptions ] ),

	wxPanel:new( Parent, FullOptions ).



% Creates a new panel, associated to specified parent, with specified
% position, dimensions and options.
%
-spec create_panel( window(), coordinate(), coordinate(),
					length(), length(), panel_options() ) -> panel().
create_panel( Parent, X, Y, Width, Height, Options ) ->

	ActualOptions = get_panel_options( Options ),

	wxPanel:new( Parent, X, Y, Width, Height, ActualOptions ).




% Button section.



% Creates a new (labelled) button, with parent specified.
%
-spec create_button( label(), window() ) -> button().
create_button( Label, Parent ) ->

	Id = ?wxID_ANY,

	Options = [ { label, Label } ],

	%trace_utils:trace_fmt( "Button options (for any ID): ~p.",
	%                       [ Id, Options ] ),

	wxButton:new( Parent, Id, Options ).



% Creates new (labelled) buttons, with their (single, common) parent specified.
%
-spec create_buttons( [ label() ], window() ) -> [ button() ].
create_buttons( Labels, Parent ) ->
	create_buttons_helper( Labels, Parent, _Acc=[] ).


create_buttons_helper( _Labels=[], _Parent, Acc ) ->
	lists:reverse( Acc );

create_buttons_helper( [ Label | T ], Parent, Acc ) ->
	NewButton = create_button( Label, Parent ),
	create_buttons_helper( T, Parent, [ NewButton | Acc ] ).




% Creates a new button, with parent and most settings specified.
%
% (internal use only)
%
-spec create_button( label(), position(), size(), button_style(), wx_id(),
					 window() ) -> button().
create_button( Label, Position, Size, Style, Id, Parent ) ->

	Options = [ { label, Label }, get_wx_position( Position ),
				get_wx_size( Size ),
				{ style, button_style_to_bitmask( Style ) } ],

	%trace_utils:trace_fmt( "Button options for ID #~B: ~p.", [ Id, Options ] ),

	wxButton:new( Parent, Id, Options ).




% Sizer section.
%
% Sizers correspond actually to wxBoxSizer (wxSizer is an abstract class).


% Creates a sizer operating on specified orientation.
%
-spec create_sizer( orientation() ) -> sizer().
create_sizer( Orientation ) ->
	ActualOrientation = get_orientation( Orientation ),
	wxBoxSizer:new( ActualOrientation ).



% Creates a sizer operating on specified orientation, within specified parent,
% with a box drawn around.
%
-spec create_sizer_with_box( orientation(), window() ) -> sizer().
create_sizer_with_box( Orientation, Parent ) ->

	ActualOrientation = get_orientation( Orientation ),

	wxStaticBoxSizer:new( ActualOrientation, Parent ).



% Creates a sizer operating on specified orientation, within specified parent,
% with a box drawn around bearing specified label.
%
-spec create_sizer_with_labelled_box( orientation(), window(), label() ) ->
											sizer().
create_sizer_with_labelled_box( Orientation, Parent, Label ) ->

	ActualOrientation = get_orientation( Orientation ),

	wxStaticBoxSizer:new( ActualOrientation, Parent, [ { label, Label } ] ).



% Adds specified element, or elements with options, to the specified sizer.
%
-spec add_to_sizer( sizer(), sizer_child() ) -> sizer_item();
				  (  sizer(), [ { sizer_child(), sizer_options() } ] ) ->
						  void().
add_to_sizer( Sizer, _Element=#canvas{ panel=Panel } ) ->
	add_to_sizer( Sizer, Panel );

% List version:
add_to_sizer( _Sizer, _Elements=[] ) ->
	ok;

add_to_sizer( Sizer, _Elements=[ { Elem, Opts } | T ] ) ->
	add_to_sizer( Sizer, Elem, Opts ),
	add_to_sizer( Sizer, T );

add_to_sizer( Sizer, Element ) ->
	wxSizer:add( Sizer, Element ).



% Adds specified element (or elements), with (common) options, to the specified
% sizer.
%
-spec add_to_sizer( sizer(), sizer_child(), sizer_options() ) ->
						  sizer_item();
				  ( sizer(), [ sizer_child() ], sizer_options() ) ->
						  void().
add_to_sizer( Sizer, _Element=#canvas{ panel=Panel }, Options ) ->
	add_to_sizer( Sizer, Panel, Options );

add_to_sizer( _Sizer, _Elements=[], _Options ) ->
	ok;

add_to_sizer( Sizer, _Elements=[ Elem | T ], Options ) ->
	add_to_sizer( Sizer, Elem, Options ),
	add_to_sizer( Sizer, T, Options );

add_to_sizer( Sizer, Element, Options ) ->

	ActualOptions = get_sizer_options( Options ),

	wxSizer:add( Sizer, Element, ActualOptions ).


% Clears specified sizer, detaching and deleting all its child windows.
%
-spec clear_sizer( sizer() ) -> void().
clear_sizer( Sizer ) ->
	clear_sizer( Sizer, _DeleteWindows=true ).



% Clears specified sizer, detaching all its child windows, and deleting them iff
% requested.
%
-spec clear_sizer( sizer(), boolean() ) -> void().
clear_sizer( Sizer, DeleteWindows ) ->
	wxSizer:clear( Sizer, [ { delete_windows, DeleteWindows } ] ).



% Status bar section.
%
% Status bars are very useful to trace events.


% Creates and attaches a status bar to the specified frame.
%
-spec create_status_bar( frame() ) -> status_bar().
create_status_bar( Frame ) ->
	% No interesting option:
	wxFrame:createStatusBar( Frame ).



% Pushes specified text in the specified status bar.
%
-spec push_status_text( text(), status_bar() ) -> void().
push_status_text( Text, StatusBar ) ->
	wxStatusBar:pushStatusText( StatusBar, Text ).



% Creates a canvas, attached to specified parent window.
%
-spec create_canvas( window() ) -> canvas().
create_canvas( Parent ) ->

	% Returns the corresponding gui_object_ref:
	execute_instance_creation( canvas, Parent ).


	gui_canvas:create( Parent, GUIEnv ).




% General MyriadGUI helpers.


% Requests the creating of specified instance (done from the MyriadGUI main
% loop), and returns the corresponding GUI object reference.
%
-spec execute_instance_creation( myriad_object_type(), any() ) ->
									   gui_object_ref().
execute_instance_creation( ObjectType, ConstructionParams ) ->

	LoopPid = get_main_loop_pid(),

	LoopPid ! { createInstance, [ ObjectType, ConstructionParams ], self() },

	receive

		{ instance_created, ObjectType, ObjectRef } ->
			ObjectRef

	end.



% Fetches (from the MyriadGUI environment) the PID of the process in charge of
% running the main GUI loop.
%
-spec get_main_loop_pid() -> pid().
get_main_loop_pid() ->

	GUIEnv = get_gui_env(),

	GUIEnv#gui_env.loop_pid.



% Fetches (from the process dictionary) the MyriadGUI environment.
%
-spec get_gui_env() -> gui_env().
get_gui_env() ->

	case get( ?gui_env_process_key ) of

		undefined ->
			trace_utils:error_fmt( "No MyriadGUI environment available for "
								   "process ~w.", [ self() ] ),
			throw( { no_myriad_gui_env, self() } );

		Env ->
			Env

	end.




% Section for backend specific helpers.



% Object type section.


% Converts a wx type of object into an internal one.
%
-spec from_wx_object_type( wx_object_type() ) -> object_type().
from_wx_object_type( wxObject ) ->
	object;

from_wx_object_type( wxEvtHandler ) ->
	event_handler;

from_wx_object_type( wxWindow ) ->
	window;

from_wx_object_type( wxControl ) ->
	control;

from_wx_object_type( wxButton ) ->
	button;

from_wx_object_type( wxPanel ) ->
	panel;

from_wx_object_type( wxStatusBar ) ->
	status_bar;

from_wx_object_type( wxTopLevelWindow ) ->
	top_level_window;

from_wx_object_type( wxDialog ) ->
	dialog;

from_wx_object_type( wxFrame ) ->
	frame;

from_wx_object_type( wxSizer ) ->
	sizer;

from_wx_object_type( wxBitmap ) ->
	bitmap;

from_wx_object_type( wxMemoryDC ) ->
	memory_device_context;

from_wx_object_type( Other ) ->
	throw( { unsupported_wx_object_type, Other } ).




% Converts an internal type of object into a wx one.
%
-spec to_wx_object_type( object_type() ) -> wx_object_type().
to_wx_object_type( object ) ->
	wxObject;

to_wx_object_type( event_handler ) ->
	wxEvtHandler;

to_wx_object_type( window ) ->
	wxWindow;

to_wx_object_type( control ) ->
	wxControl;

to_wx_object_type( button ) ->
	wxButton;

to_wx_object_type( panel ) ->
	wxPanel;

to_wx_object_type( status_bar ) ->
	wxStatusBar;

to_wx_object_type( top_level_window ) ->
	wxTopLevelWindow;

to_wx_object_type( dialog ) ->
	wxDialog;

to_wx_object_type( frame ) ->
	wxFrame;

to_wx_object_type( sizer ) ->
	wxSizer;

to_wx_object_type( bitmap ) ->
	wxBitmap;

to_wx_object_type( memory_device_context ) ->
	wxMemoryDC;

to_wx_object_type( Other ) ->
	throw( { unsupported_object_type, Other } ).



% Event type section.


% Converts a wx type of event into an internal one.
%
-spec from_wx_event_type( wx_event_type() ) -> event_type().
from_wx_event_type( close_window ) ->
	onWindowClosed;

from_wx_event_type( command_button_clicked ) ->
	onButtonClicked;

from_wx_event_type( paint ) ->
	onRepaintNeeded;

from_wx_event_type( size ) ->
	onResized.



% Converts an internal type of event into a wx one.
%
-spec to_wx_event_type( event_type() ) -> wx_event_type().
to_wx_event_type( onWindowClosed ) ->
	close_window;

to_wx_event_type( onButtonClicked ) ->
	command_button_clicked;

to_wx_event_type( onRepaintNeeded ) ->
	paint;

to_wx_event_type( onResized ) ->
	size.





% Debug section.


% Converts from our API to the one of the current GUI back-end.
%
% (helper)
%
convert_debug_level( _DebugLevel=none ) ->
	none;

convert_debug_level( _DebugLevel=calls ) ->
	trace;

convert_debug_level( _DebugLevel=life_cycle ) ->
	driver.



% Windows section.


% Converts specified window style into the appropriate back-end specific bit
% mask.
%
% (helper)
%
-spec window_style_to_bitmask( window_style() ) -> basic_utils:bit_mask().
window_style_to_bitmask( StyleList ) when is_list( StyleList ) ->

	lists:foldl( fun( S, Acc ) -> window_style_to_bitmask( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleList );

window_style_to_bitmask( _Style=default ) ->
	?wxBORDER_SIMPLE;

window_style_to_bitmask( _Style=simple_border ) ->
	?wxBORDER_SIMPLE;

window_style_to_bitmask( _Style=double_border ) ->
	?wxBORDER_DOUBLE;

window_style_to_bitmask( _Style=sunken_border ) ->
	?wxBORDER_SUNKEN;

window_style_to_bitmask( _Style=raised_border ) ->
	?wxBORDER_RAISED;

window_style_to_bitmask( _Style=static_border ) ->
	?wxSTATIC_BORDER;

window_style_to_bitmask( _Style=theme_border ) ->
	?wxBORDER_THEME;

window_style_to_bitmask( _Style=no_border ) ->
	?wxBORDER_NONE;

window_style_to_bitmask( _Style=transparent ) ->
	?wxTRANSPARENT_WINDOW;

window_style_to_bitmask( _Style=tab_traversable ) ->
	?wxTAB_TRAVERSAL;

window_style_to_bitmask( _Style=grab_all_keys ) ->
	?wxWANTS_CHARS;

window_style_to_bitmask( _Style=with_vertical_scrollbar ) ->
	?wxVSCROLL;

window_style_to_bitmask( _Style=with_horizontal_scrollbar ) ->
	?wxHSCROLL;

window_style_to_bitmask( _Style=never_hide_scrollbars ) ->
	?wxALWAYS_SHOW_SB;

window_style_to_bitmask( _Style=clip_children ) ->
	?wxCLIP_CHILDREN;

window_style_to_bitmask( _Style=full_repaint_on_resize ) ->
	?wxFULL_REPAINT_ON_RESIZE.



% Converts specified window options into the appropriate back-end specific
% options.
%
% (helper)
%
get_window_options( Options ) ->
	get_window_options( Options, _Acc=[] ).


get_window_options( _Options=[], Acc ) ->
	Acc;

get_window_options( _Options=[ { style, Style } | T ], Acc ) ->
	get_window_options( T,
					[ { style, window_style_to_bitmask( Style ) } | Acc ] );

get_window_options( _Options=[ H | T ], Acc ) ->
	get_window_options( T, [ H | Acc ] ).




% Frames section.


% Converts specified frame style into the appropriate back-end specific bit
% mask.
%
% (helper)
%
-spec frame_style_to_bitmask( frame_style() ) -> basic_utils:bit_mask().
frame_style_to_bitmask( StyleList ) when is_list( StyleList ) ->

	lists:foldl( fun( S, Acc ) -> frame_style_to_bitmask( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleList );

frame_style_to_bitmask( _Style=default ) ->
	?wxDEFAULT_FRAME_STYLE;

frame_style_to_bitmask( _Style=caption ) ->
	?wxCAPTION;

frame_style_to_bitmask( _Style=minimize ) ->
	?wxMINIMIZE;

frame_style_to_bitmask( _Style=minimize_box ) ->
	?wxMINIMIZE_BOX;

frame_style_to_bitmask( _Style=maximize ) ->
	?wxMAXIMIZE;

frame_style_to_bitmask( _Style=maximize_box ) ->
	?wxMAXIMIZE_BOX;

frame_style_to_bitmask( _Style=close_box ) ->
	?wxCLOSE_BOX;

frame_style_to_bitmask( _Style=stay_on_top ) ->
	?wxSTAY_ON_TOP;

frame_style_to_bitmask( _Style=system_menu ) ->
	?wxSYSTEM_MENU;

frame_style_to_bitmask( _Style=resize_border ) ->
	?wxRESIZE_BORDER;

frame_style_to_bitmask( _Style=tool_window ) ->
	?wxFRAME_TOOL_WINDOW;

frame_style_to_bitmask( _Style=no_taskbar ) ->
	?wxFRAME_NO_TASKBAR.



% Panels section.


% Converts specified panel options into the appropriate back-end specific
% options.
%
% (helper)
%
get_panel_options( Options ) ->
	get_window_options( Options ).




% Buttons section.


% Converts specified button style into the appropriate back-end specific bit
% mask.
%
% (helper)
%
-spec button_style_to_bitmask( button_style() ) -> basic_utils:bit_mask().
button_style_to_bitmask( StyleList ) when is_list( StyleList ) ->

	lists:foldl( fun( S, Acc ) -> button_style_to_bitmask( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleList );

button_style_to_bitmask( _Style=default ) ->
	0;

button_style_to_bitmask( _Style=left_justified ) ->
	?wxBU_LEFT;

button_style_to_bitmask( _Style=right_justified ) ->
	?wxBU_RIGHT;

button_style_to_bitmask( _Style=top_justified ) ->
	?wxBU_TOP;

button_style_to_bitmask( _Style=bottom_justified ) ->
	?wxBU_BOTTOM;

button_style_to_bitmask( _Style=exact_fit ) ->
	?wxBU_EXACTFIT;

button_style_to_bitmask( _Style=flat ) ->
	throw( not_implemented ).




% Sizers section.


% Converts specified sizer flag into the appropriate back-end specific bit
% mask.
%
% (helper)
%
-spec sizer_flag_to_bitmask( sizer_flag() ) -> basic_utils:bit_mask().
sizer_flag_to_bitmask( FlagList ) when is_list( FlagList ) ->

	lists:foldl( fun( F, Acc ) -> sizer_flag_to_bitmask( F ) bor Acc end,
				 _InitialAcc=0,
				 _List=FlagList );

sizer_flag_to_bitmask( _Flag=default ) ->
	0;

sizer_flag_to_bitmask( _Flag=top_border ) ->
	?wxTOP;

sizer_flag_to_bitmask( _Flag=bottom_border ) ->
	?wxBOTTOM;

sizer_flag_to_bitmask( _Flag=left_border ) ->
	?wxLEFT;

sizer_flag_to_bitmask( _Flag=right_border ) ->
	?wxRIGHT;

sizer_flag_to_bitmask( _Flag=all_borders ) ->
	?wxALL;

sizer_flag_to_bitmask( _Flag=expand_fully ) ->
	?wxEXPAND;

sizer_flag_to_bitmask( _Flag=expand_shaped ) ->
	?wxSHAPED;

sizer_flag_to_bitmask( _Flag=fixed_size ) ->
	?wxFIXED_MINSIZE;

sizer_flag_to_bitmask( _Flag=counted_even_if_hidden ) ->
	?wxRESERVE_SPACE_EVEN_IF_HIDDEN;

sizer_flag_to_bitmask( _Flag=align_center ) ->
	?wxALIGN_CENTER;

sizer_flag_to_bitmask( _Flag=align_left ) ->
	?wxALIGN_LEFT;

sizer_flag_to_bitmask( _Flag=align_right ) ->
	?wxALIGN_RIGHT;

sizer_flag_to_bitmask( _Flag=align_top ) ->
	?wxALIGN_TOP;

sizer_flag_to_bitmask( _Flag=align_bottom ) ->
	?wxALIGN_BOTTOM;

sizer_flag_to_bitmask( _Flag=align_center_vertical ) ->
	?wxALIGN_CENTER_VERTICAL;

sizer_flag_to_bitmask( _Flag=align_center_horizontal ) ->
	?wxALIGN_CENTER_HORIZONTAL.



% Converts to backend sizer options.
%
% (helper)
%
get_sizer_options( Options ) ->
	get_sizer_options( Options, _Acc=[] ).

get_sizer_options( _Options=[], Acc ) ->
	Acc;

get_sizer_options( _Options=[ { flag, Flag } | T ], Acc ) ->
	get_sizer_options( T, [ { flag, sizer_flag_to_bitmask( Flag ) } | Acc ] );

get_sizer_options(_Options=[ H | T ], Acc ) ->
	get_sizer_options( T, [ H | Acc ] ).



%
% General-purpose section.
%



% Returns a textual representation of the specified GUI object.
%
-spec object_to_string( gui_object() ) -> string().
object_to_string( #gui_object_ref{ object_type=Type,
								   myriad_instance_id=InstanceRef } ) ->
	text_utils:format( "~s-~B", [ ObjectType, InstanceRef ] );

object_to_string( { wx_ref, InstanceRef, WxObjectType, _State=[] } ) ->
	% Ex: {wx_ref,35,wxFrame,[]}
	ObjectType = from_wx_object_type( WxObjectType ),
	text_utils:format( "~s-~B", [ ObjectType, InstanceRef ] );

object_to_string( { wx_ref, InstanceRef, WxObjectType, State } ) ->
	ObjectType = from_wx_object_type( WxObjectType ),
	text_utils:format( "~s-~B whose state is ~p",
					   [ ObjectType, InstanceRef, State ] ).


% Returns a textual representation of the specified GUI event context.
%
-spec context_to_string( gui_event_context() ) -> string().
context_to_string( #gui_event_context{ id=Id, user_data=UserData,
									   backend_event=WxEvent } ) ->

	IdString = id_to_string( Id ),

	UserDataString = case UserData of

		[] ->
			"no user data";

		_ ->
			text_utils:format( "following user data: ~p", [ UserData ] )

	end,

	EventString = text_utils:format( "~p", [ WxEvent ] ),

	text_utils:format( "context for event ~s: ~s and ~s",
					   [ EventString, IdString, UserDataString ] ).



% Returns a textual representation of the specified GUI object wx identifier.
%
-spec id_to_string( wx_id() ) -> string().
id_to_string( _Id=undefined ) ->
	"no id defined";

id_to_string( _Id=?any_id ) ->
	"'any id' defined";

id_to_string( Id ) ->
	text_utils:format( "ID #~B", [ Id ] ).



% Returns a textual representation of specified event table.
%
-spec event_table_to_string( event_table() ) -> string().
event_table_to_string( EventTable ) ->

	case table:enumerate( EventTable ) of

		[] ->
			"empty event table";

		DispatchPairs ->

			DispatchStrings = [ dispatch_table_to_string( Object, Table )
								|| { Object, Table } <- DispatchPairs ],

			DispatchString = text_utils:strings_to_string( DispatchStrings ),

			text_utils:format( "event table with ~B GUI objects registered:~s",
							   [ length( DispatchPairs ), DispatchString ] )

	end.


% (helper)
-spec dispatch_table_to_string( gui_object(), event_dispatch_table() ) ->
									  string().
dispatch_table_to_string( GUIObject, DispatchTable ) ->

	EventPairs = list_table:enumerate( DispatchTable ),

	EventStrings = [ text_utils:format( "subscribers for event '~s': ~w",
										[ EvType, EvSubscribers ] )
					 || { EvType, EvSubscribers } <- EventPairs ],

	EventString = text_utils:strings_to_string( EventStrings,
												_IndentationLevel=1 ),

	text_utils:format( "for GUI object '~s':~s",
					   [ object_to_string( GUIObject ), EventString ] ).



% Returns a textual representation of specified reassign table.
%
-spec reassign_table_to_string( reassign_table() ) -> string().
reassign_table_to_string( ReassignTable ) ->

	case table:enumerate( ReassignTable ) of

		[] ->
			"no GUI object reassignment defined";

		ObjectPairs ->
			Strings = [ text_utils:format( "events sent to '~s' will be "
										   "reassigned to '~s'",
										   [ object_to_string( From ),
											 object_to_string( To ) ] )
						|| { From, To } <- ObjectPairs ],
			text_utils:format( "~B GUI object reassignments defined:~s",
							   [ length( Strings),
								 text_utils:strings_to_string( Strings ) ] )

	end.



% Section for backend conversions.


% Converts to back-end widget identifier.
%
% (helper)
%
get_id( undefined ) ->
	?any_id;

get_id( Other ) ->
	Other.



% Converts to back-end parent window.
%
% (helper)
%
get_parent( undefined ) ->
	?no_parent;

get_parent( Other ) ->
	Other.



% Converts to back-end position (with defaults).
%
% (helper)
%
-spec get_wx_position( position() ) -> linear_2D:point().
get_wx_position( _Position=auto ) ->
	{ pos, ?wx_default_position };

get_wx_position( Position ) ->
	{ pos, Position }.



% Converts to back-end size (with defaults).
%
% (helper)
%
-spec get_wx_size( size() ) -> linear_2D:vector().
get_wx_size( _Size=auto ) ->
	{ size, ?wx_default_size };

%get_wx_size( Size={ _X, _Y } ) ->
get_wx_size( Size ) ->
	{ size, Size }.



% Converts to back-end orientation.
%
% (helper)
%
get_orientation( vertical ) ->
	?wxVERTICAL;

get_orientation( horizontal ) ->
	?wxHORIZONTAL.
