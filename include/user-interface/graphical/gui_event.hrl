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
% Creation date: Sunday, June 18, 2023.


% A registry defined to abstract-out the various ways for the user to generate
% application-level events (e.g. based on remapped keys, mouse actions, etc.).
%
% Aggregates tables translating user events into higher-level
% application events.
%
-record( user_event_registry, {

	% For all sort of basic, atom-based user-level events (like
	% 'window_closed'):
	%
	basic_event_table :: gui_event:basic_event_table(),

	% For buttons being clicked:
	button_table :: gui_event:button_table(),

	% For keys-as-scancode being pressed:
	scancode_table :: gui_event:scancode_table(),

	% For keys-as-keycode being pressed:
	keycode_table :: gui_event:keycode_table() } ).
