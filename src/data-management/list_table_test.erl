% Copyright (C) 2014-2017 Olivier Boudeville
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

% Creation date: Tuesday, December 23, 2014

% Author: Olivier Boudeville (olivier.boudeville@esperide.com)



% Unit tests for the list-based table implementation.
%
% See the list_table.erl tested module.
%
-module(list_table_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-define(MyFirstKey,  'MyFirstKey').
-define(MySecondKey, 'MySecondKey').
-define(MyThirdKey,  'MyThirdKey').
-define(MyFourthKey, 'MyFourthKey').



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	MyH1 = list_table:new( 10 ),

	true = list_table:isEmpty( MyH1 ),

	list_table:display( "Vanilla map table", MyH1 ),

	%list_table:display( MyH1 ),
	test_facilities:display( "Adding entries in map table." ),
	MyH2 = list_table:new( 4 ),

	MyFirstValue = "MyFirstValue",

	MyH3 = list_table:addEntry( ?MyFirstKey, MyFirstValue, MyH2 ),
	false = list_table:isEmpty( MyH3 ),

	MySecondValue = [ 1, 2, 3 ],
	MyH4 = list_table:addEntry( ?MySecondKey, MySecondValue, MyH3 ),
	false = list_table:isEmpty( MyH4 ),

	list_table:display( "The map table", MyH4 ),

	MyH4Size = list_table:size( MyH4 ),
	test_facilities:display( "Size of table '~s': ~B entries",
							 [ list_table:toString( MyH4 ), MyH4Size ] ),

	test_facilities:display( "Looking up for ~s: ~p", [ ?MyFirstKey,
			list_table:lookupEntry( ?MyFirstKey, MyH4 ) ] ),

	{ value, MyFirstValue } = list_table:lookupEntry( ?MyFirstKey, MyH4 ),

	test_facilities:display( "Removing that entry." ),
	MyH5 = list_table:removeEntry( ?MyFirstKey, MyH4 ),
	false = list_table:isEmpty( MyH5 ),

	test_facilities:display( "Extracting the same entry from "
							 "the same initial table." ),
	{ MyFirstValue, MyH5 } = list_table:extractEntry( ?MyFirstKey, MyH4 ),

	test_facilities:display( "Looking up for ~s: ~p", [ ?MyFirstKey,
		list_table:lookupEntry( ?MyFirstKey, MyH5 ) ] ),

	key_not_found = list_table:lookupEntry( ?MyFirstKey, MyH5 ),

	[ MySecondValue, MyFirstValue ] = list_table:getAllValues(
										[ ?MySecondKey, ?MyFirstKey ], MyH4 ),

	% removeEntry can also be used if the specified key is not here, will return
	% an identical table.
	list_table:display( MyH5 ),
	test_facilities:display( "Testing double key registering." ),

	MyH6 = list_table:addEntry( ?MySecondKey, anything, MyH5 ),
	list_table:display( MyH6 ),

	test_facilities:display( "Enumerating the table: ~p.",
		[ list_table:enumerate( MyH4 ) ] ),

	test_facilities:display( "Listing the table keys: ~p.",
		[ list_table:keys( MyH4 ) ] ),

	test_facilities:display( "Listing the table values: ~p",
		[ list_table:values( MyH4 ) ] ),


	test_facilities:display( "Appending values to elements" ),


	MyH7 = list_table:appendToEntry( ?MyFourthKey, first_element, MyH5 ),

	MyH8 = list_table:appendToExistingEntry( ?MyFourthKey, second_element,
											 MyH7 ),

	MyH9 = list_table:appendListToExistingEntry( ?MyFourthKey,
								 [ third_element, fourth_element ], MyH8 ),

	list_table:display( MyH9 ),



	test_facilities:display( "Applying a fun to all values of "
							 "previous table" ),

	FunValue = fun( V ) ->
				io:format( " - hello value '~p'!~n", [ V ] ),
				% Unchanged here:
				V
	end,

	list_table:mapOnValues( FunValue, MyH4 ),


	test_facilities:display( "Applying a fun to all entries of "
							 "previous table:" ),

	FunEntry = fun( E={ K, V } ) ->
				io:format( " - hello, key '~p' associated to value '~p'!~n",
						   [ K, V ] ),
				% Unchanged here:
				E
	end,

	list_table:mapOnEntries( FunEntry, MyH4 ),


	test_facilities:display( "Folding on the same initial table to "
							 "count the number of entries." ),

	FunCount = fun( _Entry, AccCount ) ->
					   AccCount + 1
			   end,

	2 = list_table:foldOnEntries( FunCount, _InitialCount=0, MyH4 ),

	0 = list_table:foldOnEntries( FunCount, _InitialCount=0, MyH1 ),


	true = list_utils:unordered_compare( [ ?MyFirstKey, ?MySecondKey ],
										 list_table:keys( MyH4 ) ),

	MyH10 = list_table:addEntry( ?MyThirdKey, 3, MyH6 ),

	% MyH8 should have { AnotherKey, [1,2,3] } and { ?MyThirdKey, 3 }:
	MyH11 = list_table:merge( MyH4, MyH10 ),

	% Any optimisation would be automatic:
	test_facilities:display( "Merged table: ~s.",
							 [ list_table:toString( MyH11 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys ~p:~n ~p",
					[ Keys, list_table:selectEntries( Keys, MyH11 ) ] ),

	test_facilities:stop().
