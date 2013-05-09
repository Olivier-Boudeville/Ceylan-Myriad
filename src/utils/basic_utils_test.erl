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


% Unit tests for the basic utils toolbox.
% See the basic_utils.erl tested module.
-module(basic_utils_test).


-export([ run/0 ]).


-define( Tested_module, basic_utils ).


check_process_specific_values( Min, Max ) ->
	Self = self(),
	F = fun() -> Self ! basic_utils:get_process_specific_value(Min,Max) end,

	[ spawn(F) || _X <- lists:seq(1,10) ],

	G = fun() -> receive V -> V end end,
	[ io:format( "   Generating a process-specific value in [~B;~B[: ~w.~n",
				[ Min, Max, G() ] ) || _Y <- lists:seq(1,10) ].




run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),

	InitialTimestamp = basic_utils:get_timestamp(),
	InitialPreciseTimestamp = basic_utils:get_precise_timestamp(),

	io:format( "   Timestamp is ~s.~n", [
		basic_utils:get_textual_timestamp(InitialTimestamp) ] ),

	io:format( "   Timestamp for path is ~s.~n", [
		basic_utils:get_textual_timestamp_for_path(InitialTimestamp) ] ),

	basic_utils:checkpoint(1),

	basic_utils:start_random_source( default_seed ),

	RandomList = [ basic_utils:get_random_value(5) || _X <- lists:seq(1,15) ],

	basic_utils:stop_random_source(),

	basic_utils:checkpoint(2),

	io:format( "   Current module being used as random source: ~w.~n",
		[basic_utils:get_random_module_name()] ),

	io:format( "   A list of integer random values between 1 and 5 "
		"(both included): ~w.~n", [RandomList] ),


	% Testing list management:
	L = [ 12, 4, 13, 2, 56, 0 ],

	GetIndex = 3,

	GetValue = basic_utils:get_element_at(L,GetIndex),
	io:format( "   Getting item #~B of list ~w: ~B.~n", [GetIndex,L,GetValue] ),

	13 = GetValue,


	%OutOfBoundsIndex = 0,
	%OutOfBoundsIndex = 100,
	%io:format( "   Getting item #~B of list ~w: ~B.~n", [OutOfBoundsIndex,L,
	%	basic_utils:get_element_at(L,OutOfBoundsIndex) ] ),

	RemoveIndex = 3,

	ShortenList = basic_utils:remove_element_at( L, RemoveIndex ),

	io:format( "   List obtained after having removed item #~B of list ~w: "
		" ~w.~n", [RemoveIndex,L,ShortenList] ),

	% Hardcoded for checking:
	CorrectShortenList = [ 12, 4, 2, 56, 0 ],

	ShortenList = CorrectShortenList,


	%OutOfBoundsIndex = 0,
	%OutOfBoundsIndex = 100,
	%io:format( "   List obtained after having removed item #~B of list ~w: "
	%	" ~w.~n", [OutOfBoundsIndex,L,
	%	basic_utils:remove_element_at( L, OutOfBoundsIndex )] ),

	io:format( "   List obtained after having uniformly permuted list ~w: "
		" ~w.~n", [L,basic_utils:random_permute(L)] ),

	io:format( "   List obtained after having uniformly permuted list ~w "
		"(again): ~w.~n", [L,basic_utils:random_permute(L)] ),

	L1 = [1,2,3,4,2],

	L2 = [2,3],

	Subtracted = basic_utils:subtract_all_duplicates( L1, L2 ),

	io:format( "   Displaying the subtraction with duplicates removal "
		"of ~w by ~w: ~w.~n", [ L1, L2, Subtracted ] ),

	[1,4] = Subtracted,

	Uniquified = basic_utils:uniquify( L1 ),

	io:format( "   Displaying a uniquified version of ~w: ~w.~n",
			   [ L1, Uniquified ] ),

	% Supposedly the order will be consistent, although this is not requested:
	[3,2,1,4] = Uniquified,

	UnregisteredName = test_non_registered,
	try basic_utils:get_registered_pid_for( UnregisteredName ) of

		_Anything ->
			throw( test_should_have_failed )

	catch

		{neither_registered_locally_nor_globally,UnregisteredName} ->
			ok

	end,

	RegisteredName = test_registered,
	PidToRegister = self(),
	basic_utils:register_as( PidToRegister, RegisteredName, global_only ),

	try basic_utils:get_registered_pid_for( RegisteredName ) of

		PidToRegister ->
			ok

	catch

		Exception ->
			throw( {test_should_have_succeeded,Exception} )

	end,


	FirstVersion  = {0,0,0},
	SecondVersion = {0,0,1},
	ThirdVersion  = {0,1,0},
	FourthVersion = {1,0,0},
	FifthVersion  = {1,1,1},

	first_bigger = basic_utils:compare_versions( SecondVersion, FirstVersion),
	first_bigger = basic_utils:compare_versions( ThirdVersion, SecondVersion),
	first_bigger = basic_utils:compare_versions( FifthVersion, FirstVersion),

	second_bigger = basic_utils:compare_versions( FirstVersion, FourthVersion),
	second_bigger = basic_utils:compare_versions( ThirdVersion, FourthVersion),
	second_bigger = basic_utils:compare_versions( SecondVersion, ThirdVersion),

	equal = basic_utils:compare_versions( FirstVersion, FirstVersion ),
	equal = basic_utils:compare_versions( ThirdVersion, ThirdVersion ),
	equal = basic_utils:compare_versions( FifthVersion, FifthVersion ),

	io:format( "   Comparisons of versions like ~s succeeded.~n",
		[ text_utils:version_to_string(ThirdVersion) ] ),

	DrawList = [ {first,1}, {second,2}, {third,1} ],

	io:format( "   Drawing an element from ~w, got: '~w'.~n",
		[ DrawList, basic_utils:draw_element( DrawList ) ] ),

	io:format( "   Drawing an element from ~w, got: '~w'.~n",
		[ DrawList, basic_utils:draw_element( DrawList ) ] ),

	io:format( "   Drawing an element from ~w, got: '~w'.~n",
		[ DrawList, basic_utils:draw_element( DrawList ) ] ),

	io:format( "   Generating a new UUID:"
		" '~s'.~n", [ basic_utils:generate_uuid() ] ),

	io:format( "   Generating a process-specific value: ~w.~n",
			  [ basic_utils:get_process_specific_value() ] ),

	{Min,Max} = {3,16},
	check_process_specific_values( Min, Max ),

	FinalPreciseTimestamp = basic_utils:get_precise_timestamp(),

	io:format( "   Precise duration in test is ~p ms.~n", [
		basic_utils:get_precise_duration(InitialPreciseTimestamp,
										 FinalPreciseTimestamp) ] ),

	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().
