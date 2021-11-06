% Copyright (C) 2021-2021 Olivier Boudeville
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
% Creation date: Friday, November 5, 2021.


% @doc Support for the <b>Protocol Buffer</b> facilities, a.k.a. Protobuf.
%
% See protobuf_support_test.erl for the corresponding test, and
% http://myriad.esperide.org/#about-protobuf for further information.
%
-module(protobuf_support).

-export_type([ ]).

-export([ ]).


% Implementation notes:
%
% The role of this module is to provide any higher-level service needed, and to
% shelter the user code from any Protobuf backend.
%
% The currently-used Protobuf backend is gpb
% (https://github.com/tomas-abrahamsson/gpb).
