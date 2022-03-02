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


% @doc Gathering of various facilities for <b>keyboard management</b>.
-module(gui_keyboard).


% Implementation notes:
%
% The goal is to offer a stable interface, as independent as possible from any
% backend.
%
% Currently the actual backend is wx; in the future it may be any other (ex:
% esdl2).

% Largely inspired from: Wings3D, SDL and esdl2.


% There are two ways to consider a keyboard:
%  - a 101-button joystick, for which only the button's locations and
%  press/release statuses matter
%  - a device that produces text (Unicode) inputs
%
% In the first case, we are dealing with scancodes ("button codes"), whereas in
% the second one we are dealing with keycodes ("character codes"). Both are
% 32-bit values, yet have different semantics.

% Scancodes relate only to actual button locations, hence are meant not to
% depend on any current specific keyboard layout; think of this as "the user
% pressed the Q key as it would be on a US QWERTY keyboard" regardless of
% whether this is actually a European keyboard or a Dvorak keyboard or
% whatever. They correspond to the gui_keyboard:scancode/0 type.
%
% So the scancode is always the same key position, it basically designates a
% button at a given location of the aforementioned 101-button joystick.

% As for keycodes, they are meant to be layout-dependent (and
% location-independent). Think of this as "the user pressed the key that is
% labelled 'Q' on a specific keyboard, wherever it is.". They correspond to the
% gui_keyboard:keycode/0 type.


% For the key defines:
-include_lib("wx/include/wx.hrl").


% For the actual scancode definitions:
-include("ui_keyboard_scancodes.hrl").

% For the actual keycode definitions:
-include("ui_keyboard_keycodes.hrl").


-type scancode() :: uint32().
% Designates a button at a given location of the keyboard when it is considered
% as a 101-button joystick.
%
% A scancode is a "button code", it designates a location of a button on any
% keyboard, regardless of the character displayed on the user's actual keyboard.
%
% These locations are designated according the characters that would be printed
% on a virtual, canonical US QWERTY keyboard, taken as a reference.
%
% Refer to the corresponding MYR_SCANCODE_* defines.



-type keycode() :: uint32().
% Designates a layout-dependent (Unicode) character code, i.e. the key
% corresponding to a given character, wherever that key may be on the user's
% actual keyboard (which notably depends on its layout of choice).
%
% Refer to the corresponding MYR_K_* defines.



-type key_transition() :: 'key_down'
						| 'key_up'.
% Corresponds to a (punctual) state transition of a key.


-type key_status() :: 'pressed'
					| 'released'.
% Corresponds to the (potentially durable) status of a key.


-export_type([ scancode/0, keycode/0,
			   key_transition/0, key_status/0
			 ]).



% Internals:

-export([ wx_keycode_to_myr/1, myr_keycode_to_wx/1 ]).


% Shorthands:

-type uint32() :: type_utils:uint32().

-type wx_keycode() :: integer().


% Helpers related to wx:


% @doc Returns the MyriadGUI scancode corresponding to the specified wx one.
%-spec wx_scancode_to_myr( wx_scancode() ) -> scancode().


% @doc Returns the wx scancode corresponding to the specified MyriadGUI one.
%-spec myr_scancode_to_wx( scancode() ) -> wx_scancode().
%myr_scancode_to_wx( MyrScanCode ) ->



% @doc Returns the MyriadGUI keycode corresponding to the specified wx one.
-spec wx_keycode_to_myr( wx_keycode() ) -> maybe( keycode() ).
% Directly obtained from wings_io_wx:wx_key_map/1:
% (could be substituted with a corresponding edsl2 NIF)
wx_keycode_to_myr( ?WXK_SHIFT )   -> ?MYR_K_LSHIFT;
wx_keycode_to_myr( ?WXK_ALT )     -> ?MYR_K_LALT;
wx_keycode_to_myr( ?WXK_CONTROL ) -> ?MYR_K_LCTRL;

wx_keycode_to_myr( ?WXK_F1 )  -> ?MYR_K_F1;
wx_keycode_to_myr( ?WXK_F2 )  -> ?MYR_K_F2;
wx_keycode_to_myr( ?WXK_F3 )  -> ?MYR_K_F3;
wx_keycode_to_myr( ?WXK_F4 )  -> ?MYR_K_F4;
wx_keycode_to_myr( ?WXK_F5 )  -> ?MYR_K_F5;
wx_keycode_to_myr( ?WXK_F6 )  -> ?MYR_K_F6;
wx_keycode_to_myr( ?WXK_F7 )  -> ?MYR_K_F7;
wx_keycode_to_myr( ?WXK_F8 )  -> ?MYR_K_F8;
wx_keycode_to_myr( ?WXK_F9 )  -> ?MYR_K_F9;
wx_keycode_to_myr( ?WXK_F10 ) -> ?MYR_K_F10;
wx_keycode_to_myr( ?WXK_F11 ) -> ?MYR_K_F11;
wx_keycode_to_myr( ?WXK_F12 ) -> ?MYR_K_F12;
wx_keycode_to_myr( ?WXK_F13 ) -> ?MYR_K_F13;
wx_keycode_to_myr( ?WXK_F14 ) -> ?MYR_K_F14;
wx_keycode_to_myr( ?WXK_F15 ) -> ?MYR_K_F15;

wx_keycode_to_myr( ?WXK_UP )    -> ?MYR_K_UP;
wx_keycode_to_myr( ?WXK_LEFT )  -> ?MYR_K_LEFT;
wx_keycode_to_myr( ?WXK_DOWN )  -> ?MYR_K_DOWN;
wx_keycode_to_myr( ?WXK_RIGHT ) -> ?MYR_K_RIGHT;

wx_keycode_to_myr( ?WXK_DELETE )   -> ?MYR_K_DELETE; %% same
wx_keycode_to_myr( ?WXK_INSERT )   -> ?MYR_K_INSERT;
wx_keycode_to_myr( ?WXK_END )      -> ?MYR_K_END;
wx_keycode_to_myr( ?WXK_HOME )     -> ?MYR_K_HOME;
wx_keycode_to_myr( ?WXK_PAGEUP )   -> ?MYR_K_PAGEUP;
wx_keycode_to_myr( ?WXK_PAGEDOWN ) -> ?MYR_K_PAGEDOWN;

% In SDL2, '_KPn' became '_KP_n':
wx_keycode_to_myr( ?WXK_NUMPAD0 ) -> ?MYR_K_KP_0;
wx_keycode_to_myr( ?WXK_NUMPAD1 ) -> ?MYR_K_KP_1;
wx_keycode_to_myr( ?WXK_NUMPAD2 ) -> ?MYR_K_KP_2;
wx_keycode_to_myr( ?WXK_NUMPAD3 ) -> ?MYR_K_KP_3;
wx_keycode_to_myr( ?WXK_NUMPAD4 ) -> ?MYR_K_KP_4;
wx_keycode_to_myr( ?WXK_NUMPAD5 ) -> ?MYR_K_KP_5;
wx_keycode_to_myr( ?WXK_NUMPAD6 ) -> ?MYR_K_KP_6;
wx_keycode_to_myr( ?WXK_NUMPAD7 ) -> ?MYR_K_KP_7;
wx_keycode_to_myr( ?WXK_NUMPAD8 ) -> ?MYR_K_KP_8;
wx_keycode_to_myr( ?WXK_NUMPAD9 ) -> ?MYR_K_KP_9;

wx_keycode_to_myr( ?WXK_NUMPAD_MULTIPLY ) -> ?MYR_K_KP_MULTIPLY;
wx_keycode_to_myr( ?WXK_NUMPAD_ADD )      -> ?MYR_K_KP_PLUS;
wx_keycode_to_myr( ?WXK_NUMPAD_SUBTRACT ) -> ?MYR_K_KP_MINUS;
wx_keycode_to_myr( ?WXK_NUMPAD_DECIMAL )  -> ?MYR_K_KP_PERIOD;
wx_keycode_to_myr( ?WXK_NUMPAD_DIVIDE )   -> ?MYR_K_KP_DIVIDE;
wx_keycode_to_myr( ?WXK_NUMPAD_ENTER )    -> ?MYR_K_KP_ENTER;

% In SDL2, '_{L,R}LSUPER' became '_{L,R}GUI', we prefer the former convention:
wx_keycode_to_myr( ?WXK_WINDOWS_LEFT )  -> ?MYR_K_LSUPER;
wx_keycode_to_myr( ?WXK_WINDOWS_RIGHT ) -> ?MYR_K_RSUPER;

wx_keycode_to_myr( WxKeycode ) ->
	trace_utils:warning_fmt( "Unknown wx keycode: '~ts' (i.e. ~B).",
							 [ [ WxKeycode ], WxKeycode ] ),
	undefined.


% @doc Returns the wx keycode corresponding to the specified MyriadGUI one.
-spec myr_keycode_to_wx( keycode() ) -> wx_keycode().
% Directly obtained from wings_io_wx:sdl_key_map/1:
% ( could be substituted with a corresponding edsl2 NIF)
myr_keycode_to_wx( ?MYR_K_LSHIFT ) -> ?WXK_SHIFT;
myr_keycode_to_wx( ?MYR_K_LALT )   -> ?WXK_ALT;
myr_keycode_to_wx( ?MYR_K_LCTRL )  -> ?WXK_CONTROL;

myr_keycode_to_wx( ?MYR_K_F1 )   -> ?WXK_F1;
myr_keycode_to_wx( ?MYR_K_F2 )   -> ?WXK_F2;
myr_keycode_to_wx( ?MYR_K_F3 )   -> ?WXK_F3;
myr_keycode_to_wx( ?MYR_K_F4 )   -> ?WXK_F4;
myr_keycode_to_wx( ?MYR_K_F5 )   -> ?WXK_F5;
myr_keycode_to_wx( ?MYR_K_F6 )   -> ?WXK_F6;
myr_keycode_to_wx( ?MYR_K_F7 )   -> ?WXK_F7;
myr_keycode_to_wx( ?MYR_K_F8 )   -> ?WXK_F8;
myr_keycode_to_wx( ?MYR_K_F9 )   -> ?WXK_F9;
myr_keycode_to_wx( ?MYR_K_F10 )  -> ?WXK_F10;
myr_keycode_to_wx( ?MYR_K_F11 )  -> ?WXK_F11;
myr_keycode_to_wx( ?MYR_K_F12 )  -> ?WXK_F12;
myr_keycode_to_wx( ?MYR_K_F13 )  -> ?WXK_F13;
myr_keycode_to_wx( ?MYR_K_F14 )  -> ?WXK_F14;
myr_keycode_to_wx( ?MYR_K_F15 )  -> ?WXK_F15;

myr_keycode_to_wx( ?MYR_K_UP )    -> ?WXK_UP;
myr_keycode_to_wx( ?MYR_K_LEFT )  -> ?WXK_LEFT;
myr_keycode_to_wx( ?MYR_K_DOWN )  -> ?WXK_DOWN;
myr_keycode_to_wx( ?MYR_K_RIGHT ) -> ?WXK_RIGHT;

myr_keycode_to_wx( ?MYR_K_DELETE )   -> ?WXK_DELETE; %% same
myr_keycode_to_wx( ?MYR_K_INSERT )   -> ?WXK_INSERT;
myr_keycode_to_wx( ?MYR_K_END )      -> ?WXK_END;
myr_keycode_to_wx( ?MYR_K_HOME )     -> ?WXK_HOME;
myr_keycode_to_wx( ?MYR_K_PAGEUP )   -> ?WXK_PAGEUP;
myr_keycode_to_wx( ?MYR_K_PAGEDOWN ) -> ?WXK_PAGEDOWN;

% In SDL2, '_KPn' became '_KP_n':
myr_keycode_to_wx( ?MYR_K_KP_0 ) -> ?WXK_NUMPAD0;
myr_keycode_to_wx( ?MYR_K_KP_1 ) -> ?WXK_NUMPAD1;
myr_keycode_to_wx( ?MYR_K_KP_2 ) -> ?WXK_NUMPAD2;
myr_keycode_to_wx( ?MYR_K_KP_3 ) -> ?WXK_NUMPAD3;
myr_keycode_to_wx( ?MYR_K_KP_4 ) -> ?WXK_NUMPAD4;
myr_keycode_to_wx( ?MYR_K_KP_5 ) -> ?WXK_NUMPAD5;
myr_keycode_to_wx( ?MYR_K_KP_6 ) -> ?WXK_NUMPAD6;
myr_keycode_to_wx( ?MYR_K_KP_7 ) -> ?WXK_NUMPAD7;
myr_keycode_to_wx( ?MYR_K_KP_8 ) -> ?WXK_NUMPAD8;
myr_keycode_to_wx( ?MYR_K_KP_9 ) -> ?WXK_NUMPAD9;

myr_keycode_to_wx( ?MYR_K_KP_MULTIPLY ) -> ?WXK_NUMPAD_MULTIPLY;
myr_keycode_to_wx( ?MYR_K_KP_PLUS )     -> ?WXK_NUMPAD_ADD;
myr_keycode_to_wx( ?MYR_K_KP_MINUS )    -> ?WXK_NUMPAD_SUBTRACT;
myr_keycode_to_wx( ?MYR_K_KP_PERIOD )   -> ?WXK_NUMPAD_DECIMAL;
myr_keycode_to_wx( ?MYR_K_KP_DIVIDE )   -> ?WXK_NUMPAD_DIVIDE;
myr_keycode_to_wx( ?MYR_K_KP_ENTER )    -> ?WXK_NUMPAD_ENTER;

% In SDL2, '_{L,R}LSUPER' became '_{L,R}GUI', we prefer the former convention:
myr_keycode_to_wx( ?MYR_K_LSUPER ) -> ?WXK_WINDOWS_LEFT;
myr_keycode_to_wx( ?MYR_K_RSUPER ) -> ?WXK_WINDOWS_RIGHT;

myr_keycode_to_wx( MyrKeycode ) ->
	trace_utils:warning_fmt( "MyriadGUI keycode '~ts' (i.e. ~B) passed "
		"verbatim to wx.", [ [ MyrKeycode ], MyrKeycode ] ),
	MyrKeycode.
