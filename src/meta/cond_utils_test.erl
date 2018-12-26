% Copyright (C) 2014-2018 Olivier Boudeville
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
% Creation date: Tuesday, December 25, 2018


% Unit tests for the cond_utils services.
%
% See the cond_utils.erl tested module.
%
-module(cond_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	Token = debug_mode,
	%Token = non_existing_token,

	test_facilities:display(
	  "Testing code whose execution is conditioned by token '~s'.", [ Token ] ),

	A = 1,
	B = 2,

     A = 1,
									io:format( "Conditional code executed!~n" ),
									B = A + 1,

	test_facilities:stop().
