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



% Gathers all elements relative to the MyriadGUI events, including the event
% loop.
%
-module(gui_event).


% Implementation notes.


% Events can be managed as messages or callbacks. We generally prefer the former
% (as messages can be selectively received, any context can be kept in the
% receive loop, no temporary process is created, no wx include is needed hence
% the backend can be well encapsulated, etc.).
%
% Whether an event shall be also dispatched to subsequent handlers may be
% decided by using propagate_event/1.
%
% Event messages are internally converted, in order to hide the wx backend,
% augment it with other primitives (ex: canvas widget) and make them compliant
% with the MyriadGUI conventions, as seen by the user code (hint: these
% conventions comply with the WOOPER ones, should the GUI be used in an OOP
% context).
%
% Regarding events, see also:
% https://wiki.wxwidgets.org/Events#Event.Skip_and_Event.Veto

% Please refer to wx.pdf (available on http://www.erlang.org/doc/apps/wx/wx.pdf)
% for more architecture/implementation details about wx.


% Function export section.


% Main event primitives:
-export([ start_main_event_loop/2, propagate_event/1 ]).


% Stringification:
-export([ event_table_to_string/1 ]).


% To silence unused warnings:
-export([ reassign_table_to_string/1 ]).



% Type section.


% Event messages.
%
% Lower-level, backend-specific events are translated into MyriadGUI event
% messages, to be received by their respective event subscribers.



% An event message is a pair whose first element is the event type, as an atom
% (ex: 'onWindowClosed'), and whose second element is a list, whose first
% element is the GUI object that generated that event (the closed window, here),
% and whose last element is the event context (intermediary elements carrying
% event-specific information):
%
% { event_type(), [ gui_object(), ..., event_context() ] }
%
% Ex: { onWindowClosed, [ Window, CloseContext ] }.
%
% Note: these messages respect the WOOPER conventions, and this is done on
% purpose, to facilitate any integration with upper layers.
%
-type event_message() :: { event_type(), [ any() ] }.



% A count of instances of a given object type:
-type instance_count() :: basic_utils:count().


% Event management.
%
% In general we promote managing events thanks to messages rather than thanks to
% callbacks, as the former can access easily to the full context of the program
% whereas the latter is only executed into a transient process (less convenient,
% probably less efficient).

-type event_source() :: wx_event_handler() | myriad_event_handler().


-export_type([ event_message/0, event_source/0 ]).


-type wx_event_handler() :: wxEvtHandler:wxEvtHandler().

% Only one currently:
-type myriad_event_handler() :: gui_canvas:canvas().



% Using the wx-event type, leaked by wx.hrl:
%
% (enrich this union whenever needed)
%
-type wx_event_type() :: wx_close_event_type(). % | ...


% Associated to wxCloseEvent:
-type wx_close_event_type() :: 'close_window' | 'end_session'
							 | 'query_end_session'.


% Our own event types, independent from any backend:
%
-type event_type() :: 'onWindowClosed'
					  .


% The PID of a user calling process:
-type user_pid() :: pid().

% The PID of an event subscriber:
-type event_subscriber_pid() :: pid().


% So that user process(es) can subscribe to GUI events:
%
-type event_subscription() ::
		{ list_utils:maybe_list( event_type() ),
		  list_utils:maybe_list( gui_object() ),
		  list_utils:maybe_list( event_subscriber_pid() ) }.



% Specifies, for an event subscriber (by default: the calling process), any
% combination of type of events and GUI objects that shall be listened to.
%
-type event_subscription_spec() :: list_utils:maybe_list(
									 event_subscription() ).



% An indirection table dispatching events according to subscription
% specifications.
%
% For an incoming event, we see this type (virtually, logically) as:
% table( { gui_object(), event_type() },
%            set_utils:set( event_subscriber_pid() ) ):
%
% - the first key is the GUI object (e.g. widget) from which the event emanates
% (ex: a frame)
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


% To replace source events objects (ex: a panel) by others (ex: its associated
% canvas, if any):
%
-type reassign_table() :: table:table( gui_object(), gui_object() ).


% To store the MyriadGUI instances (sorted by types) and manage them like wx
% native objects.
%
% Keys are like 'canvas'.
%
-type myriad_type_table() :: table:table( gui:myriad_object_type(),
										  instance_referential() ).


% To store, for a given MyriadGUI type (ex: 'canvas'), all information about all
% instances.
%
% - a total count of the instances already created for that type
%
% - a table whose keys are the identifiers of the objects of that type, and
% whose values are the actual state of these instances.
%
% Note: the total count is not the same as the size of the table, as instances
% may be deleted.
%
-record( instance_referential, {

		% Total count of the instances already created for that type:
		instance_count :: instance_count(),

		instance_table :: table:table( gui:myriad_instance_id(),
									   gui:myriad_object_state() )

}).

-type instance_referential() :: #instance_referential{}.



% Stores the current MyriadGUI state, as managed by its main event loop.
%
-record( loop_state, {


		   % Identifier of the current top-level wx server:
		   wx_server :: gui:wx_server(),


		   % To dispatch appropriately the backend-originating events:
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


		   % Stores, by types, the current widget instances that have been
		   % introduced by MyriadGUI to complement the backend (ex: canvas
		   % instances).
		   %
		   type_table :: myriad_type_table()

}).

-type loop_state() :: #loop_state{}.


% A #wx event record comprises:
%
% - (the 'wx' record tag, if the record instance is seen as a tuple)
%
% - id :: wx_id() the (integer) identifier of the object (e.g. widget) that
% received the event (event source)
%
% - obj :: wx:wx_object() is the reference of the wx object that was specified
% in the connect/n call, i.e. on which connect/n was called (ex:
% {wx_ref,35,wxFrame,[]})
%
% - userData :: user_data() is the user-specified data that was specified in the
% connect/n call (typically [], as not very useful)
%
% - event :: wx_event_info() is the description of the event itself
%
% As always, same as: -record( wx,...
%
-type wx_event() :: { 'wx', wx_id(),  wx:wx_object(), gui:user_data(),
					  wx_event_info() }.


% A wx-defined record describing an actual event.
%
% WxFoobar record whose first field is 'type', and which may have other
% fields.
%
% Examples of descriptions, as tuples: {wxClose,close_window}, or
% {wxCommand,command_button_clicked,CmdString,CmdInt,...}
%
-type wx_event_info() :: tuple().


% To silence unused warnings:
-export_type([ wx_event_type/0 ]).


% Shorthands:

-type gui_object() :: gui:gui_object().
-type wx_id() :: gui_wx_backend:wx_id().




% For gui_event_context():
-include("gui.hrl").

% For canvas_state():
-include("gui_canvas.hrl").


% For wx headers:
-include("gui_internal_defines.hrl").




% Implementation section.



% Starts the internal, main event loop of MyriadGUI.
%
% The backend events received will result in callbacks to be triggered on their
% respective subscribers.
%
% The goal is to devise a generic event loop, while still being able to be
% notified of all relevant information (and only them).
%
-spec start_main_event_loop( gui:wx_server(), wx:wx_env() ) -> no_return().
start_main_event_loop( WxServer, WxEnv ) ->

	% To be done first, so that we are able to use wx from that process from now
	% on:
	%
	wx:set_env( WxEnv ),

	EmptyTable = table:new(),

	InitialLoopState = #loop_state{ wx_server=WxServer,
									event_table=EmptyTable,
									reassign_table=EmptyTable,
									type_table=EmptyTable },

	%trace_utils:debug_fmt( "Starting main MyriadGUI loop." ] ),

	% Enter the infinite event loop:
	process_event_messages( InitialLoopState ).



% Receives and process all messages (actual main event loop), coming:
%
% - either from controlling processes (typically from application processes
% subscribing to some events)
%
% - or from the backend, that notifies this loop of the actual, lower-level
% events
%
-spec process_event_messages( loop_state() ) -> no_return().
process_event_messages( LoopState ) ->

	%trace_utils:trace( "Waiting for event messages..." ),

	% Event types roughly sorted by decreasing frequency of appearance:
	%
	% (defined in lib/wx/include/wx.hrl)
	%
	NewLoopState = receive

		% A wx event has been received here:
		%
		% Structure: { wx, Id, Obj, UserData, EventInfo }, with EventInfo:
		% { WxEventName, EventType, ...}
		%
		% Ex: { wx, -2006, {wx_ref,35,wxFrame,[]}, [], {wxClose,close_window} }.
		%
		WxEvent=#wx{ id=Id, obj=GUIObject, userData=UserData,
					 event=WxEventInfo } ->
			process_wx_event( Id, GUIObject, UserData, WxEventInfo, WxEvent,
							  LoopState );


		% MyriadGUI user request (ex: emaning from gui:create_canvas/1):
		{ createInstance, [ ObjectType, ConstructionParams ], CallerPid } ->
			process_myriad_creation( ObjectType, ConstructionParams,
									 CallerPid, LoopState );


		{ subscribeToEvents, [ SubscribedEvents, SubscriberPid ] } ->
			update_event_loop_tables( SubscribedEvents, SubscriberPid, 
									  LoopState );


		UnmatchedEvent ->
			trace_utils:warning_fmt( "Ignored following unmatched event "
									 "message:~n~p", [ UnmatchedEvent ] ),
			LoopState

	end,

	process_event_messages( NewLoopState ).




% Processes specified wx event message.
%
-spec process_wx_event( wx_id(), gui:wx_object(),
						gui:user_data(), wx_event_info(), wx_event(),
						loop_state() ) -> loop_state().
process_wx_event( Id, GUIObject, UserData, WxEventInfo, WxEvent,
				  LoopState=#loop_state{ event_table=EventTable,
										 reassign_table=ReassignTable } ) ->

	trace_utils:trace_fmt( "Wx event received about '~s':~n~p.",
						   [ gui:object_to_string( GUIObject ), WxEventInfo ] ),

	ActualGUIObject = case table:lookupEntry( GUIObject, ReassignTable ) of

		key_not_found ->
			trace_utils:trace_fmt( "Wx event received about '~s':~n~p.",
						   [ gui:object_to_string( GUIObject ), WxEventInfo ] ),
			GUIObject;


		{ value, TargetGUIObject } ->
			trace_utils:trace_fmt( "Wx event received about '~s', "
								   "reassigned to '~s':~n~p.",
								   [ gui:object_to_string( GUIObject ),
									 gui:object_to_string( TargetGUIObject ),
									 WxEventInfo ] ),

			TargetGUIObject

	end,

	% Here some events shall be intercepted and specific processing actions
	% shall be triggered: if a canvas-owned panel is resized, then that canvas
	% shall itself by resized, see gui_canvas:update/2.

	% Then notify the subscribers:
	case table:lookupEntry( ActualGUIObject, EventTable ) of

		key_not_found ->
			% At least one subscriber would be expected:
			trace_utils:warning_fmt( "No event subscription for GUI "
							 "object '~s', hence ignoring event.",
							 [ gui:object_to_string( ActualGUIObject ) ] ),
			LoopState;

		{ value, DispatchTable } ->

			% Example: WxEventType = close_window (first element is the record
			% name, such as wxClose).
			%
			WxEventType = element( 2, WxEventInfo ),

			EventType = gui_wx_backend:from_wx_event_type( WxEventType ),

			case list_table:lookupEntry( EventType, DispatchTable ) of

				{ value, _Subscribers=[] } ->
					trace_utils:error_fmt( "For GUI object '~s', event type "
					   "'~s' not registered whereas notified (abnormal).",
					   [ gui:object_to_string( ActualGUIObject ), EventType ] );

				{ value, Subscribers } ->

					trace_utils:debug_fmt( "Sending ~p event to subscribers "
										   "~w.", [ EventType, Subscribers ] ),

					send_event( Subscribers, EventType, Id, ActualGUIObject,
								UserData, WxEvent )

			end,

			LoopState

	end.



% Creates specified MyriadGUI object.
%
-spec process_myriad_creation( gui:myriad_object_type(),
							   gui:construction_parameters(), user_pid(),
							   loop_state() ) -> loop_state().
process_myriad_creation( ObjectType, ConstructionParams, CallerPid,
						 LoopState=#loop_state{ reassign_table=ReassignTable,
												type_table=TypeTable } ) ->

	trace_utils:debug_fmt( "Myriad instance creation request received from ~w, "
						   "for type ~s, with construction parameters ~p.",
						   [ CallerPid, ObjectType, ConstructionParams ] ),

	case ObjectType of

		canvas ->

			{ CanvasInitialState, PanelRef } = gui_canvas:create_instance(
												 ConstructionParams ),

			{ CanvasRef, NewTypeTable } = register_instance( ObjectType,
										   CanvasInitialState, TypeTable ),

			CallerPid ! { instance_created, ObjectType, CanvasRef },

			NewReassignTable = table:addNewEntry( PanelRef, CanvasRef,
												  ReassignTable ),

			LoopState#loop_state{ reassign_table=NewReassignTable,
								  type_table=NewTypeTable };


		UnexpectedType ->
			trace_utils:error_fmt( "'~s' is not a known MyriadGUI type.",
								   [ UnexpectedType ] ),

			throw( { unexpected_myriad_type, UnexpectedType } )


	end.


% Registers the creation of a MyriadGUI instance of specified type and initial
% state, in specified instance table.
%
-spec register_instance( gui:myriad_object_type(), gui:myriad_object_state(),
		 myriad_type_table() ) -> { myriad_object_ref(), myriad_type_table() }.
register_instance( ObjectType, ObjectInitialState, TypeTable ) ->

	trace_utils:trace_fmt( "Registering a MyriadGUI instance of type '~s' and "
						   "of initial state ~p.",
						   [ ObjectType, ObjectInitialState ] ),

	{ NewInstanceId, NewInstanceReferential } = case table:lookupEntry(
													ObjectType, TypeTable ) of

		key_not_found ->

			% First instance of its type:
			FirstInstanceId = 1,

			FirstInstanceTable = table:new(
								  [ { FirstInstanceId, ObjectInitialState } ] ),

			FirstInstanceReferential = #instance_referential{ instance_count=1,
											instance_table=FirstInstanceTable },

			{ FirstInstanceId, FirstInstanceReferential };


		{ value, InstanceReferential=#instance_referential{
				   instance_count=InstanceCount,
				   instance_table=InstanceTable } } ->

			NextInstanceId = InstanceCount + 1,

			NextInstanceTable = table:addEntry( NextInstanceId,
									 ObjectInitialState, InstanceTable ),

			NextInstanceReferential = InstanceReferential#instance_referential{
										instance_count=NextInstanceId,
										instance_table=NextInstanceTable },

			{ NextInstanceId, NextInstanceReferential }


	end,

	MyriadRef = #myriad_object_ref{ object_type=ObjectType,
									myriad_instance_id=NewInstanceId },

	NewTypeTable = table:addEntry( ObjectType, NewInstanceReferential,
								   TypeTable ),

	{ MyriadRef, NewTypeTable }.




% Sends the specified translation of a wx events to the relevant subscribers.
%
% (helper)
%
-spec send_event( [ event_subscriber_pid() ], event_type(), wx_id(),
				  gui_object(), gui:user_data(), wx_event() ) ->
						basic_utils:void().
send_event( Subscribers, EventType, Id, GUIObject, UserData, Event ) ->

	Context = #gui_event_context{ id=Id, user_data=UserData,
								  backend_event=Event },

	Msg = { EventType, [ GUIObject, Context ] },

	[ SubPid ! Msg || SubPid <- Subscribers ].



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
register_event_types_for( Canvas=#canvas_state{ panel=Panel }, EventTypes,
						  Subscribers, LoopState=#loop_state{
											event_table=EventTable,
											reassign_table=ReassignTable } ) ->

	trace_utils:debug_fmt( "Registering subscribers ~w for event types ~p "
						   "regarding canvas '~s'.", [ Subscribers, EventTypes,
							   gui:object_to_string( Canvas ) ] ),

	% A canvas is registered in wx as a panel (as wx will send events about it)
	% that will be reassigned as a canvas:

	NewEventTable = record_subscriptions( Canvas, EventTypes, Subscribers,
										  EventTable ),

	% Will defer all events (paint, size) of the underlying panel to the canvas:

	[ gui_wx_backend:connect( Panel, EvType ) || EvType <- EventTypes ],

	NewReassignTable = table:addNewEntry( Panel, Canvas, ReassignTable ),

	LoopState#loop_state{ event_table=NewEventTable,
						  reassign_table=NewReassignTable };


register_event_types_for( GUIObject, EventTypes, Subscribers,
						  LoopState=#loop_state{ event_table=EventTable } ) ->

	trace_utils:debug_fmt( "Registering subscribers ~w for event types ~p "
						   "regarding object '~s'.", [ Subscribers, EventTypes,
							 gui:object_to_string( GUIObject ) ] ),

	% Auto-connection to the current PID (i.e. the one of the internal, main
	% event loop), so that it receives these events for their upcoming
	% dispatching to the actual subscribers:
	%
	[ gui_wx_backend:connect( GUIObject, EvType ) || EvType <- EventTypes ],

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

	table:addEntry( GUIObject, NewDispatchTable, EventTable ).



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
-spec propagate_event( gui_event_context() ) -> basic_utils:void().
propagate_event( #gui_event_context{ backend_event=WxEvent } ) ->

	% Honestly the skip semantics looks a bit unclear.
	% 'skip' is here a synonymous of 'propagate'.

	% Default is having skip=true, so same as:
	% wxEvent:skip( WxEvent, _Opts=[ { skip, true } ] ):
	wxEvent:skip( WxEvent ).





% Helper section.


% Stringification subsection.



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
					   [ gui:object_to_string( GUIObject ), EventString ] ).



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
										   [ gui:object_to_string( From ),
											 gui:object_to_string( To ) ] )
						|| { From, To } <- ObjectPairs ],
			text_utils:format( "~B GUI object reassignments defined:~s",
							   [ length( Strings),
								 text_utils:strings_to_string( Strings ) ] )

	end.
