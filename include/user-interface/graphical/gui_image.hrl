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
% Creation date: Thursday, April 7, 2022.


% As, sometimes, defines in header files make sense:
-ifndef(myriad_gui_image_hrl_guard).
-define(myriad_gui_image_hrl_guard,).


% A record describing a raw, ready-to-use, bitmap, as a term, possibly loaded
% from file, or generated, etc., as opposed to an image (which respects a format
% like PNG or JPEG, has metadata, etc.).
%
-record( raw_bitmap, {

	dimensions :: gui:dimensions(),
	% The dimensions of that raw bitmap.

	pixels :: binary()
	% The actual content of that raw bitmap.

} ).


-endif. % myriad_gui_image_hrl_guard
