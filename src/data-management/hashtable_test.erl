% Copyright (C) 2003-2013 Olivier Boudeville
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


% Unit tests for the generic hash table implementation.
% See the hashtable.erl tested module.

-module(hashtable_test).
% Directly depends on the hashtable module.


-define(Tested_modules,[hashtable]).

% For test_finished/0 and al:
-include("test_facilities.hrl").


run() ->

	io:format( "--> Testing module ~p.~n", [ ?Tested_modules ] ),

	MyH1 = hashtable:new(0),
	hashtable:display(MyH1),

	MyH2 = hashtable:new(4),

	MyH3 = hashtable:addEntry( "MyFirstKey", "MyFirstValue", MyH2 ),

	MyH4 = hashtable:addEntry( "AnotherKey", [1,2,3], MyH3 ),
	hashtable:display(MyH4),

	io:format( "   Looking up for ~s: ~p~n", [ "MyFirstKey",
		hashtable:lookupEntry("MyFirstKey",MyH4)]),

	{value,"MyFirstValue"} = hashtable:lookupEntry("MyFirstKey",MyH4),

	io:format( "   Removing that entry.~n" ),
	MyH5 = hashtable:removeEntry("MyFirstKey",MyH4),

	io:format( "   Looking up for ~s: ~p~n", [ "MyFirstKey",
		hashtable:lookupEntry("MyFirstKey",MyH5)]),

	{hashtable_key_not_found,"MyFirstKey"} =
		hashtable:lookupEntry("MyFirstKey",MyH5),

	% removeEntry can also be used if the specified key is not here, will return
	% an identical table.
	hashtable:display(MyH5),

	io:format( "   Testing double key registering.~n" ),

	MyH6 = hashtable:addEntry("AnotherKey",anything,MyH5),
	hashtable:display(MyH6),

	io:format( "   Enumerating the hash table: ~p~n",
		[hashtable:enumerate(MyH4)]),

	io:format( "   Listing the hash table keys: ~p~n",
		[hashtable:keys(MyH4)]),
	["MyFirstKey","AnotherKey"] = hashtable:keys(MyH4),

	MyH7 = hashtable:addEntry("Third key",3,MyH6),

	% MyH8 should have {AnotherKey,[1,2,3]} and {"Third key",3}:
	MyH8 = hashtable:merge(MyH4,MyH7),

	io:format( "   Merged table: ~s~n", [hashtable:toString(MyH8)]),

	test_finished().
