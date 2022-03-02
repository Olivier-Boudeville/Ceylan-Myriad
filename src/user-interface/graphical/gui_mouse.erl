% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Saturday, February 26, 2022.


% @doc Gathering of various facilities for <b>mouse management</b>.
-module(gui_mouse).


% Implementation notes:
%
% The goal is to offer a stable interface, as independent as possible from any
% backend.
%
% Currently the actual backend is wx; in the future it may be any other (ex:
% esdl2).

% Largely inspired from: Wings3D, SDL and esdl2.

% Custom cursors may be defined, see
% https://docs.wxwidgets.org/3.1/classwx_cursor.html.


-include_lib("wx/include/wx.hrl").


-type cursor_type() ::
		'default'
	  | 'none'
	  | 'blank' % An invisible cursor
	  | 'stop' % "no entry", typically to denote a disallowed operation
	  | 'arrow' % Typically to designate an element
	  | 'right_arrow'
	  | 'question_arrow'
	  | 'wait_arrow'
	  | 'wait' % Hourglass, tTypically to denote an ongoing operation
	  | 'watch'
	  | 'hand' % "pointing hand"
	  | 'closed_hand' % "bull's eye"
	  | 'point_left'
	  | 'point_right'
	  | 'char'
	  | 'cross'
	  | 'ibeam'
	  | 'left_button'
	  | 'middle_button'
	  | 'right_button'
	  | 'magnifier' % Typically to zoom
	  | 'paintbrush'
	  | 'pencil'
	  | 'spraycan'
	  %| 'eyedropper' % Typically to pick a color
	  | 'size_ne_sw'
	  | 'size_ns'
	  | 'size_nw_se'
	  | 'size_we'
	  | 'sizing'.
% A cursor is a small bitmap usually used for denoting where the mouse pointer
% is, with a picture that might indicate the interpretation of a mouse click.

-type cursor_table() :: table( cursor_type(), wx_cursor_type() ).


-export_type([ cursor_type/0, cursor_table/0 ]).


-export([ init_pref_env_server/1, init_pref_env_server/2,
		  terminate_pref_env_server/1,
		  list_cursor_types/0, set_cursor/1 ]).



% Internals:

-type wx_cursor_type() :: non_neg_integer().
% See include/wx.hrl.

-export([ cursor_type_to_wx/1 ]).


% Shorthands:

-type preferences_pid() ::preferences:preferences_pid().


% @doc Initialises, mouse-wise, the preferences/environment server, preparing
% all standard mouse cursors.
%
-spec init_pref_env_server( preferences_pid() ) -> void().
init_pref_env_server( PrefEnvPid ) ->
	AllCursorTypes = list_cursor_types(),
	init_pref_env_server( PrefEnvPid, AllCursorTypes ).


% @doc Initialises, mouse-wise, the preferences/environment server, preparing
% the specified standard mouse cursors.
%
-spec init_pref_env_server( [ cursor_type() ], preferences_pid() ) -> void().
init_pref_env_server( CursorTypes, PrefEnvPid ) ->
	CursorEntries = [ { CT, wxCursor:new( cursor_type_to_wx( CT ) ) }
										|| CT <- CursorTypes ],
	CursorTable = table:new( CursorEntries ),
	preferences:set( _K=cursor_table, _Value=CursorTable, PrefEnvPid ).


% @doc Terminates (de-inits) the specified preferences/environment server.
-spec terminate_pref_env_server( preferences_pid() ) -> void().
terminate_pref_env_server( PrefEnvPid ) ->
	CursorTable = preferences:get( _K=cursor_table, PrefEnvPid ),
	[ wx:destroy( WxCT ) || WxCT <- table:values( CursorTable ) ].



% @doc Returns a list of all the standard mouse cursor types.
-spec list_cursor_types() -> [ cursor_type() ].
list_cursor_types() ->
	[ 'default', 'none', 'blank', 'stop', 'arrow', 'right_arrow',
	  'question_arrow', 'wait_arrow', 'wait', 'watch', 'hand',
	  'closed_hand', 'point_left', 'point_right', 'char', 'cross', 'ibeam',
	  'left_button', 'middle_button', 'right_button', 'magnifier', 'paintbrush',
	  'pencil', 'spraycan', 'size_ne_sw', 'size_ns', 'size_nw_se', 'size_we',
	  'sizing' ].


% @doc Sets the current mouse cursor.
-spec set_cursor( cursor_type() ) -> void().
set_cursor( CursorType ) ->




% Helpers related to wx:


% @doc Returns the wx cursor type corresponding to the MyriadGUI specified one.
-spec cursor_type_to_wx( cursor_type() ) -> wx_cursor_type().
% From wx.hrl:
cursor_type_to_wx( default ) -> ?wxCURSOR_DEFAULT;
cursor_type_to_wx( none ) -> ?wxCURSOR_NONE;
cursor_type_to_wx( blank ) -> ?wxCURSOR_BLANK;

cursor_type_to_wx( stop ) -> ?wxCURSOR_NO_ENTRY;
cursor_type_to_wx( arrow ) -> ?wxCURSOR_ARROW;
cursor_type_to_wx( right_arrow ) -> ?wxCURSOR_RIGHT_ARROW;
cursor_type_to_wx( question_arrow ) -> ?wxCURSOR_QUESTION_ARROW;
cursor_type_to_wx( wait_arrow ) -> ?wxCURSOR_ARROWWAIT;
cursor_type_to_wx( wait ) -> ?wxCURSOR_WAIT;
cursor_type_to_wx( watch ) -> ?wxCURSOR_WATCH;
cursor_type_to_wx( hand ) -> ?wxCURSOR_HAND;
cursor_type_to_wx( closed_hand ) -> ?wxCURSOR_BULLSEYE;

cursor_type_to_wx( point_left ) -> ?wxCURSOR_POINT_LEFT;
cursor_type_to_wx( point_right ) -> ?wxCURSOR_POINT_RIGHT;

cursor_type_to_wx( char ) -> ?wxCURSOR_CHAR;
cursor_type_to_wx( cross ) -> ?wxCURSOR_CROSS;
cursor_type_to_wx( ibeam ) -> ?wxCURSOR_IBEAM;

cursor_type_to_wx( left_button ) -> ?wxCURSOR_LEFT_BUTTON;
cursor_type_to_wx( middle_button ) -> ?wxCURSOR_MIDDLE_BUTTON;
cursor_type_to_wx( right_button ) -> ?wxCURSOR_RIGHT_BUTTON;

cursor_type_to_wx( magnifier ) -> ?wxCURSOR_MAGNIFIER;
cursor_type_to_wx( paintbrush ) -> ?wxCURSOR_PAINT_BRUSH;
cursor_type_to_wx( pencil ) -> ?wxCURSOR_PENCIL;
cursor_type_to_wx( spraycan ) -> ?wxCURSOR_SPRAYCAN;
% Non-existing: cursor_type_to_wx( eyedropper ) -> ?wxCURSOR_;

cursor_type_to_wx( size_ne_sw ) -> ?wxCURSOR_SIZENESW;
cursor_type_to_wx( size_ns ) -> ?wxCURSOR_SIZENS;
cursor_type_to_wx( size_nw_se ) -> ?wxCURSOR_SIZENWSE;
cursor_type_to_wx( size_we ) -> ?wxCURSOR_SIZEWE;
cursor_type_to_wx( sizing ) -> ?wxCURSOR_SIZING.
