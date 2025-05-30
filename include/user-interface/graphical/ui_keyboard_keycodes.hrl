% Copyright (C) 2022-2025 Olivier Boudeville
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
% Creation date: Wednesday, February 23, 2022.


% These keyboard keycodes (i.e. gui_keyboard:keycode()) are directly coming from
% Loic Hoguin's esdl2 (see [1]:
% https://github.com/ninenines/esdl2/blob/master/include/sdl_keycode.hrl), which
% are themselves deriving in turn from Sam Lantinga's libSDL2 (see [2]:
% https://github.com/libsdl-org/SDL/blob/main/src/events/SDL_keyboard.c).
%
% Many thanks to both projects.
%
% However our tests showed (one may use the 'Keyboard Layouts' applet to switch
% layout) that, for some reasons, such keycodes do not match the actual
% ones. For example, hitting 'a' produces keycode 65 (which is actually $A), not
% keycode 97 (which is $a); 'b' produces keycode 66 ($B), not keycode 98, etc.

% So finally we defined our own keycodes from experience; for example (noting
% 'generated key: scancode/keycode'):
%
% Keyboard layout: \ Label of the key being pressed:
%            A           Q          B
%  US    Q: 24/81    A: 38/65   B: 56/66
%  FR    A: 24/65    Q: 38/81   B: 56/66
%
% We can see that scancodes are stable whereas keycodes depend on the current
% keyboard layout.
%
% Depending on the layout, some characters cannot be obtained from a single
% keypress, and the check is made more problematic.
%
% Note that, when a given key is hit (e.g. the key labelled 'A' on a French
% keyboard), the keycodes returned for (1) onKeyPressed/onKeyReleased or (2)
% onCharEntered will differ.
%
% Indeed:
% - (1) will return keycode 65 ("A"), as it depends only on the physical key
% being pressed, independently of the state of the modifier keys
% - whereas (2) will return 97 ("a"), that is the key that would correspond to
% the character that would appear in e.g. a text zone if the user pressed the
% key in it.
%
% Refer to https://www.erlang.org/doc/apps/wx/wxkeyevent for further details.


% So, with onCharEntered, pressing the key labelled 'A' on a French keyboard (to
% write a "a", not a "A") results in 24/97, whereas pressing 'Ctrl-a' results in
% 24/1 (and suitable modifiers). 'B' is 56/98 and 'Ctrl-b' is 56/2 (and suitable
% modifiers).
%

% Refer to the implementation notes in gui_keyboard.erl for all related
% information, notably for the differences between scancodes and keycodes.


% We prefer exposing uniform prefixes, and not pieces of some backends; so below
% is just [1], with the 'SDL' prefix replaced with the 'MYR_' one (for
% MyriadGUI); adding then 'K' for Keycode.

% Note that, with SDL, 'GUI' designates the former "Windows" key, we prefer
% naming it 'SUPER' (see https://en.wikipedia.org/wiki/Windows_key), also rather
% than WINDOWS, META, etc.


% Preferably not to be included directly; include at least the more general
% 'myriad_ui.hrl' instead.


% Include guard:
-ifndef(__MYRIAD_UI_KEYBOARD_KEYCODES_HRL__).
-define(__MYRIAD_UI_KEYBOARD_KEYCODES_HRL__, 1).


% Warning: we are not sure at all that with wx some keycodes can be deduced from
% scancodes.


% Structure of a keycode define, using 'MYR_K_q' as an example:
% - 'MYR' for Myriad, to avoid any define clash with third-party
% - 'K' for keycode (as opposed to scancode)
% - 'q' for the 'q' key


% Mask of 0b100-0000-0000-0000-0000-0000-0000-0000:
-define(MYR_K_SCANCODE_MASK, (1 bsl 30)).

% From button to character (i.e. ensuring that bit 30 is set to 1):
-define(MYR_SCANCODE_TO_KEYCODE(SC), (SC bor ?MYR_K_SCANCODE_MASK)).


% Definitions of all known keycodes (a.k.a. on-keyboard characters that can be
% selected):

-define(MYR_K_RETURN, $\r).      % 13
-define(MYR_K_ESCAPE, $\033).    % 27
-define(MYR_K_BACKSPACE, $\b).   % 22
-define(MYR_K_TAB, $\t).         % 23
-define(MYR_K_SPACE, $ ).        % 32
-define(MYR_K_EXCLAIM, $!).      % 33

% Double-quote:
-define(MYR_K_QUOTEDBL, $").     % 34

-define(MYR_K_HASH, $#).         % 35
-define(MYR_K_PERCENT, $%).      % 37
-define(MYR_K_DOLLAR, $$).       % 36
-define(MYR_K_AMPERSAND, $&).    % 38
-define(MYR_K_QUOTE, $').        % 39
-define(MYR_K_LEFTPAREN, $().    % 40
-define(MYR_K_RIGHTPAREN, $)).   % 41
-define(MYR_K_ASTERISK, $*).     % 42
-define(MYR_K_PLUS, $+).         % 43
-define(MYR_K_COMMA, $,).        % 44
-define(MYR_K_MINUS, $-).        % 45
-define(MYR_K_PERIOD, $.).       % 46
-define(MYR_K_SLASH, $/).        % 47
-define(MYR_K_0, $0).            % 48
-define(MYR_K_1, $1).            % 49
-define(MYR_K_2, $2).            % 50
-define(MYR_K_3, $3).            % 51
-define(MYR_K_4, $4).            % 52
-define(MYR_K_5, $5).            % 53
-define(MYR_K_6, $6).            % 54
-define(MYR_K_7, $7).            % 55
-define(MYR_K_8, $8).            % 56
-define(MYR_K_9, $9).            % 57
-define(MYR_K_COLON, $:).        % 58
-define(MYR_K_SEMICOLON, $;).    % 59
-define(MYR_K_LESS, $<).         % 60
-define(MYR_K_EQUALS, $=).       % 61
-define(MYR_K_GREATER, $>).      % 62
-define(MYR_K_QUESTION, $?).     % 63
-define(MYR_K_AT, $@).           % 64
-define(MYR_K_LEFTBRACKET, $[).  % 91
-define(MYR_K_BACKSLASH, $\\).   % 92
-define(MYR_K_RIGHTBRACKET, $]). % 93
-define(MYR_K_CARET, $^).        % 94
-define(MYR_K_UNDERSCORE, $_).   % 95
-define(MYR_K_BACKQUOTE, $`).    % 96

-define(MYR_K_a, 97). % For 'a': neither 65 (keycode for 'A') nor 24 (scancode
					  % of the key labelled 'A' on a French keyboard)
-define(MYR_K_b, 98). % Neither 66 nor 56
-define(MYR_K_c, 54). % Not 67 nor 99.
-define(MYR_K_d, 40). % Not 68.
-define(MYR_K_e, 26). % Not 69.
-define(MYR_K_f, 41). % Not 70.
-define(MYR_K_g, 42). % Not 71.
-define(MYR_K_h, 43). % Not 72.
-define(MYR_K_i, 31). % Not 73.
-define(MYR_K_j, 44). % Not 74.
-define(MYR_K_k, 45). % Not 75.
-define(MYR_K_l, 46). % Not 76.
-define(MYR_K_m, 47). % Not 77.
-define(MYR_K_n, 57). % Not 78.
-define(MYR_K_o, 32). % Not 79.
-define(MYR_K_p, 33). % Not 80.
-define(MYR_K_q, 38). % Not 81.
-define(MYR_K_r, 27). % Not 82.
-define(MYR_K_s, 39). % Not 83.
-define(MYR_K_t, 28). % Not 84.
-define(MYR_K_u, 30). % Not 85.
-define(MYR_K_v, 55). % Not 86.
-define(MYR_K_w, 52). % Not 87.
-define(MYR_K_x, 53). % Not 88.
-define(MYR_K_y, 29). % Not 89.
-define(MYR_K_z, 25). % Not 90.


-define(MYR_K_CAPSLOCK, 66 ). % ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_CAPSLOCK)).
-define(MYR_K_F1, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F1)).
-define(MYR_K_F2, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F2)).
-define(MYR_K_F3, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F3)).
-define(MYR_K_F4, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F4)).
-define(MYR_K_F5, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F5)).
-define(MYR_K_F6, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F6)).
-define(MYR_K_F7, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F7)).
-define(MYR_K_F8, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F8)).
-define(MYR_K_F9, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F9)).
-define(MYR_K_F10, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F10)).
-define(MYR_K_F11, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F11)).
-define(MYR_K_F12, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F12)).
-define(MYR_K_PRINTSCREEN, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_PRINTSCREEN)).
-define(MYR_K_SCROLLLOCK, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_SCROLLLOCK)).
-define(MYR_K_PAUSE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_PAUSE)).
-define(MYR_K_INSERT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_INSERT)).
-define(MYR_K_HOME, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_HOME)).
-define(MYR_K_PAGEUP, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_PAGEUP)).
-define(MYR_K_DELETE, $\177).
-define(MYR_K_END, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_END)).
-define(MYR_K_PAGEDOWN, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_PAGEDOWN)).
-define(MYR_K_RIGHT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_RIGHT)).
-define(MYR_K_LEFT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_LEFT)).
-define(MYR_K_DOWN, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_DOWN)).
-define(MYR_K_UP, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_UP)).
-define(MYR_K_NUMLOCKCLEAR, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_NUMLOCKCLEAR)).
-define(MYR_K_KP_DIVIDE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_DIVIDE)).
-define(MYR_K_KP_MULTIPLY, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_MULTIPLY)).
-define(MYR_K_KP_MINUS, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_MINUS)).
-define(MYR_K_KP_PLUS, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_PLUS)).
-define(MYR_K_KP_ENTER, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_ENTER)).
-define(MYR_K_KP_1, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_1)).
-define(MYR_K_KP_2, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_2)).
-define(MYR_K_KP_3, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_3)).
-define(MYR_K_KP_4, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_4)).
-define(MYR_K_KP_5, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_5)).
-define(MYR_K_KP_6, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_6)).
-define(MYR_K_KP_7, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_7)).
-define(MYR_K_KP_8, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_8)).
-define(MYR_K_KP_9, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_9)).
-define(MYR_K_KP_0, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_0)).
-define(MYR_K_KP_PERIOD, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_PERIOD)).

% No MYR_SCANCODE_APPLICATION set:
%-define(MYR_K_APPLICATION, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_APPLICATION)).

-define(MYR_K_POWER, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_POWER)).
-define(MYR_K_KP_EQUALS, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_EQUALS)).
-define(MYR_K_F13, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F13)).
-define(MYR_K_F14, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F14)).
-define(MYR_K_F15, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F15)).
-define(MYR_K_F16, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F16)).
-define(MYR_K_F17, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F17)).
-define(MYR_K_F18, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F18)).
-define(MYR_K_F19, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F19)).
-define(MYR_K_F20, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F20)).
-define(MYR_K_F21, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F21)).
-define(MYR_K_F22, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F22)).
-define(MYR_K_F23, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F23)).
-define(MYR_K_F24, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_F24)).
-define(MYR_K_EXECUTE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_EXECUTE)).
-define(MYR_K_HELP, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_HELP)).
-define(MYR_K_MENU, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_MENU)).
-define(MYR_K_SELECT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_SELECT)).
-define(MYR_K_STOP, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_STOP)).
-define(MYR_K_AGAIN, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AGAIN)).
-define(MYR_K_UNDO, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_UNDO)).
-define(MYR_K_CUT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_CUT)).
-define(MYR_K_COPY, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_COPY)).
-define(MYR_K_PASTE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_PASTE)).
-define(MYR_K_FIND, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_FIND)).
-define(MYR_K_MUTE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_MUTE)).
-define(MYR_K_VOLUMEUP, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_VOLUMEUP)).
-define(MYR_K_VOLUMEDOWN, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_VOLUMEDOWN)).
-define(MYR_K_KP_COMMA, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_COMMA)).
-define(MYR_K_KP_EQUALSAS400, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_EQUALSAS400)).
-define(MYR_K_ALTERASE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_ALTERASE)).
-define(MYR_K_SYSREQ, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_SYSREQ)).
-define(MYR_K_CANCEL, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_CANCEL)).
-define(MYR_K_CLEAR, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_CLEAR)).
-define(MYR_K_PRIOR, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_PRIOR)).
-define(MYR_K_RETURN2, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_RETURN2)).
-define(MYR_K_SEPARATOR, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_SEPARATOR)).
-define(MYR_K_OUT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_OUT)).
-define(MYR_K_OPER, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_OPER)).
-define(MYR_K_CLEARAGAIN, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_CLEARAGAIN)).
-define(MYR_K_CRSEL, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_CRSEL)).
-define(MYR_K_EXSEL, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_EXSEL)).
-define(MYR_K_KP_00, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_00)).
-define(MYR_K_KP_000, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_000)).
-define(MYR_K_THOUSANDSSEPARATOR, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_THOUSANDSSEPARATOR)).
-define(MYR_K_DECIMALSEPARATOR, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_DECIMALSEPARATOR)).
-define(MYR_K_CURRENCYUNIT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_CURRENCYUNIT)).
-define(MYR_K_CURRENCYSUBUNIT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_CURRENCYSUBUNIT)).
-define(MYR_K_KP_LEFTPAREN, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_LEFTPAREN)).
-define(MYR_K_KP_RIGHTPAREN, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_RIGHTPAREN)).
-define(MYR_K_KP_LEFTBRACE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_LEFTBRACE)).
-define(MYR_K_KP_RIGHTBRACE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_RIGHTBRACE)).
-define(MYR_K_KP_TAB, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_TAB)).
-define(MYR_K_KP_BACKSPACE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_BACKSPACE)).
-define(MYR_K_KP_A, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_A)).
-define(MYR_K_KP_B, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_B)).
-define(MYR_K_KP_C, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_C)).
-define(MYR_K_KP_D, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_D)).
-define(MYR_K_KP_E, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_E)).
-define(MYR_K_KP_F, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_F)).
-define(MYR_K_KP_XOR, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_XOR)).
-define(MYR_K_KP_POWER, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_POWER)).
-define(MYR_K_KP_PERCENT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_PERCENT)).
-define(MYR_K_KP_LESS, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_LESS)).
-define(MYR_K_KP_GREATER, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_GREATER)).
-define(MYR_K_KP_AMPERSAND, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_AMPERSAND)).
-define(MYR_K_KP_DBLAMPERSAND, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_DBLAMPERSAND)).
-define(MYR_K_KP_VERTICALBAR, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_VERTICALBAR)).
-define(MYR_K_KP_DBLVERTICALBAR, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_DBLVERTICALBAR)).
-define(MYR_K_KP_COLON, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_COLON)).
-define(MYR_K_KP_HASH, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_HASH)).
-define(MYR_K_KP_SPACE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_SPACE)).
-define(MYR_K_KP_AT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_AT)).
-define(MYR_K_KP_EXCLAM, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_EXCLAM)).
-define(MYR_K_KP_MEMSTORE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_MEMSTORE)).
-define(MYR_K_KP_MEMRECALL, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_MEMRECALL)).
-define(MYR_K_KP_MEMCLEAR, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_MEMCLEAR)).
-define(MYR_K_KP_MEMADD, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_MEMADD)).
-define(MYR_K_KP_MEMSUBTRACT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_MEMSUBTRACT)).
-define(MYR_K_KP_MEMMULTIPLY, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_MEMMULTIPLY)).
-define(MYR_K_KP_MEMDIVIDE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_MEMDIVIDE)).
-define(MYR_K_KP_PLUSMINUS, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_PLUSMINUS)).
-define(MYR_K_KP_CLEAR, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_CLEAR)).
-define(MYR_K_KP_CLEARENTRY, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_CLEARENTRY)).
-define(MYR_K_KP_BINARY, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_BINARY)).
-define(MYR_K_KP_OCTAL, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_OCTAL)).
-define(MYR_K_KP_DECIMAL, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_DECIMAL)).
-define(MYR_K_KP_HEXADECIMAL, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KP_HEXADECIMAL)).
-define(MYR_K_LCTRL, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_LCTRL)).
-define(MYR_K_LSHIFT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_LSHIFT)).
-define(MYR_K_LALT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_LALT)).

% Preferred to LGUI:
-define(MYR_K_LSUPER, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_LSUPER)).

-define(MYR_K_RCTRL, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_RCTRL)).
-define(MYR_K_RSHIFT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_RSHIFT)).

% Any shift:
-define(MYR_K_ANY_SHIFT, (?MYR_K_LSHIFT bor ?MYR_K_RSHIFT)).

-define(MYR_K_RALT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_RALT)).



% Preferred to RGUI:
-define(MYR_K_RSUPER, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_RSUPER)).

% "Alt Gr" key:
-define(MYR_K_MODE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_MODE)).

-define(MYR_K_AUDIONEXT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AUDIONEXT)).
-define(MYR_K_AUDIOPREV, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AUDIOPREV)).
-define(MYR_K_AUDIOSTOP, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AUDIOSTOP)).
-define(MYR_K_AUDIOPLAY, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AUDIOPLAY)).
-define(MYR_K_AUDIOMUTE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AUDIOMUTE)).
-define(MYR_K_MEDIASELECT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_MEDIASELECT)).
-define(MYR_K_WWW, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_WWW)).
-define(MYR_K_MAIL, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_MAIL)).
-define(MYR_K_CALCULATOR, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_CALCULATOR)).
-define(MYR_K_COMPUTER, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_COMPUTER)).
-define(MYR_K_AC_SEARCH, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AC_SEARCH)).
-define(MYR_K_AC_HOME, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AC_HOME)).
-define(MYR_K_AC_BACK, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AC_BACK)).
-define(MYR_K_AC_FORWARD, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AC_FORWARD)).
-define(MYR_K_AC_STOP, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AC_STOP)).
-define(MYR_K_AC_REFRESH, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AC_REFRESH)).
-define(MYR_K_AC_BOOKMARKS, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AC_BOOKMARKS)).
-define(MYR_K_BRIGHTNESSDOWN, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_BRIGHTNESSDOWN)).
-define(MYR_K_BRIGHTNESSUP, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_BRIGHTNESSUP)).
-define(MYR_K_DISPLAYSWITCH, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_DISPLAYSWITCH)).
-define(MYR_K_KBDILLUMTOGGLE, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KBDILLUMTOGGLE)).
-define(MYR_K_KBDILLUMDOWN, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KBDILLUMDOWN)).
-define(MYR_K_KBDILLUMUP, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_KBDILLUMUP)).
-define(MYR_K_EJECT, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_EJECT)).
-define(MYR_K_SLEEP, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_SLEEP)).
-define(MYR_K_APP1, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_APP1)).
-define(MYR_K_APP2, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_APP2)).
-define(MYR_K_AUDIOREWIND, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AUDIOREWIND)).
-define(MYR_K_AUDIOFASTFORWARD, ?MYR_SCANCODE_TO_KEYCODE(?MYR_SCANCODE_AUDIOFASTFORWARD)).


% See the notes at the top for purposes of the next defines, to be used with the
% onCharEntered events:
%
% (these are independent from keyboard layouts; for example one should find the
% same keycode on a French keyboard, when, with the French layout, hitting the
% key labelled 'A' or, this time with the English layout, hitting the key
% labelled 'Q' in the same keyboard)
%
-define(MYR_K_CTRL_A, 1).
-define(MYR_K_CTRL_B, 2).
-define(MYR_K_CTRL_C, 3).
-define(MYR_K_CTRL_D, 4).
-define(MYR_K_CTRL_E, 5).
-define(MYR_K_CTRL_F, 6).
-define(MYR_K_CTRL_G, 7).
-define(MYR_K_CTRL_H, 8).
-define(MYR_K_CTRL_I, 9).
-define(MYR_K_CTRL_J, 10).
-define(MYR_K_CTRL_K, 11).
-define(MYR_K_CTRL_L, 12).
-define(MYR_K_CTRL_M, 13).
-define(MYR_K_CTRL_N, 14).
-define(MYR_K_CTRL_O, 15).
-define(MYR_K_CTRL_P, 16).
-define(MYR_K_CTRL_Q, 17).
-define(MYR_K_CTRL_R, 18).
-define(MYR_K_CTRL_S, 19).
-define(MYR_K_CTRL_T, 20).
-define(MYR_K_CTRL_U, 21).
-define(MYR_K_CTRL_V, 22).
-define(MYR_K_CTRL_W, 23).
-define(MYR_K_CTRL_X, 24).
-define(MYR_K_CTRL_Y, 25).
-define(MYR_K_CTRL_Z, 26).


-endif. % __MYRIAD_UI_KEYBOARD_KEYCODES_HRL__
