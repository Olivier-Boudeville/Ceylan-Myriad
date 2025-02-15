% Copyright (C) 2017-2025 Olivier Boudeville
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
% Creation date: 2017.

-module(bin_utils_test).

-moduledoc """
Unit tests for the management of units.

See the unit_utils.erl tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing CRC support." ),

	CRCTable = bin_utils:get_crc8_table(),

	%test_facilities:display( "CRC table: ~p", [ CRCTable ] ),

	256 = size( CRCTable ),

	Binary = << 0, 1, 2, 3, 4, 5, 6 >>,

	CRC = bin_utils:compute_crc8_checksum( Binary ),

	test_facilities:display( "CRC for ~p is ~p.", [ Binary, CRC ] ),

	test_facilities:stop().
