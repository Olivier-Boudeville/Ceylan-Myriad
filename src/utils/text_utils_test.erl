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


% Unit tests for the text utils toolbox.
% See the text_utils.erl tested module.
-module(text_utils_test).


-export([ run/0 ]).


-define( Tested_module, text_utils ).


% For pretty-printing test:
%-record( my_test_record, {

%	first_field,
%	second_field = 1,
%	third_file = "This is a test"

%} ).



print_title( Title, Level ) ->
	io:format( "Title level ~B:~n~s~n", [ Level,
		text_utils:generate_title(Title,Level) ] ).



run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),


	io:format( "   Converting an integer to a string: ~s.~n",
		[ text_utils:integer_to_string(3245) ] ),


	%MyTestRecord = #my_test_record{},

	%io:format( "   Converting a record instance to a string: ~s.~n",
	%	[ text_utils:record_to_string(MyTestRecord) ] ),


	io:format( "   Output with term_toString : ~s, ~s and ~s.~n",
		[ text_utils:term_toString(an_atom), text_utils:term_toString([1,2]),
			text_utils:term_toString("A string")	]),


	ListOfStrings = [ "Hello", "World", "Vampire" ],

	io:format( "   Displaying list ~p as a string:~n~s~n",
		[ ListOfStrings, text_utils:string_list_to_string(ListOfStrings) ] ),


	LongLine = "This is a long line to test the paragraph formatting.",

	% So that "formatting." has a chance to fit:
	TargetWidth = 10,

	io:format( "   Displaying text '~s' once formatted "
		"for a width of ~B:~n~p~n",
		[LongLine,TargetWidth,
			text_utils:format_text_for_width(LongLine,TargetWidth) ] ),


	JustWideEnoughLine = "<0.33.0>",

	% So that "formatting." has a chance to fit:
	NewTargetWidth = 8,

	io:format( "   Displaying text '~s' once formatted "
		"for a width of ~B:~n~p~n",
		[JustWideEnoughLine,NewTargetWidth,
			text_utils:format_text_for_width( JustWideEnoughLine,
				NewTargetWidth) ] ),


	io:format( "   Displaying atom list obtained from string list ~p: ~p.~n",
		[ ListOfStrings, text_utils:string_list_to_atom_list(ListOfStrings) ]),

	FirstTestString = "Hello world!",

	io:format( "   Determining whether '~p' is a string: ~w.~n",
		[ FirstTestString, text_utils:is_string(FirstTestString) ] ),
	true = text_utils:is_string( FirstTestString ),

	SecondTestString = [ $o, [ $s, $d ], $l ],

	io:format( "   Determining whether '~p' is a string: ~w.~n",
		[ SecondTestString, text_utils:is_string(SecondTestString) ] ),
	false = text_utils:is_string( SecondTestString ),

	ThirdTestString = [ $e, 1, 2, $r ],

	io:format( "   Determining whether '~p' is a string: ~w.~n",
		[ ThirdTestString, text_utils:is_string(ThirdTestString) ] ),
	true = text_utils:is_string(ThirdTestString),

	FirstList = [],
	io:format( "   Determining whether '~p' is a list of strings: ~w.~n",
		[ FirstList, text_utils:is_list_of_strings(FirstList) ] ),
	true = text_utils:is_list_of_strings(FirstList),

	SecondList = [FirstTestString],
	io:format( "   Determining whether '~p' is a list of strings: ~w.~n",
		[ SecondList, text_utils:is_list_of_strings(SecondList) ] ),
	true = text_utils:is_list_of_strings(SecondList),

	ThirdList = [FirstTestString,ThirdTestString],
	io:format( "   Determining whether '~p' is a list of strings: ~w.~n",
		[ ThirdList, text_utils:is_list_of_strings(ThirdList) ] ),
	true = text_utils:is_list_of_strings(ThirdList),

	FourthList = [FirstTestString,SecondTestString],
	io:format( "   Determining whether '~p' is a list of strings: ~w.~n~n",
		[ FourthList, text_utils:is_list_of_strings(FourthList) ] ),
	false = text_utils:is_list_of_strings(FourthList),


	Title = "Alien creatures invaded Ireland!",

	[ print_title( Title, Level ) || Level <- lists:seq( 1, 9 ) ],

	Percent = 0.1234,

	io:format( "    Displaying ~p as a percentage: ~s.~n",
			  [ Percent, text_utils:percent_to_string( Percent ) ] ),


	io:format( "    Checking string/binary conversions.~n" ),

	"hello" = text_utils:binary_to_string( <<"hello">> ),
	 <<"hello">> = text_utils:string_to_binary( "hello" ),

	StringList = [ "hello", "world" ],
	BinList = [ <<"hello">>, <<"world">> ],

	% Order matters:
	BinList = text_utils:strings_to_binaries( StringList ),
	StringList = text_utils:binaries_to_strings( BinList ),

	10.0 = text_utils:string_to_float( "10" ),
	10.0 = text_utils:string_to_float( "10.0" ),
	try
		text_utils:string_to_float( "Not a float" )

	catch

		throw:_ ->
			ok

	end,

	io:format( "~n		Testing the textual conversion of distances:~n" ),

	% In millimeters:
	Distances = [ -1001.5, -1001.0, -1000.5, -1000.0, -999.5, -999.0,
				 -1001, -1000, -999, -1.6, -1.4, -1.0, -0.9, -1, 0,
				 1, 0.9, 2, 999, 1000, 1001, 999999, 1000000, 1000001 ],

	[ io:format( " - an integer distance of ~w millimeters is ~s, "
				"and roughly ~s~n", [ D, text_utils:distance_to_string(D),
								text_utils:distance_to_short_string(D) ] )
	 || D <- Distances ],


	io:format( "~n		Testing the textual conversion of durations:~n" ),

	% In milliseconds:

	Durations = [ -100000, -1000, -1, 0 , 1, 2, 10, 3000, 3599,
				 3600, 3601, 36000, 59000, 60000, 61000, 100000,
				 12345678, 1234567890123 ],

	[ io:format( " - an integer duration of ~w milliseconds is ~s~n",
				[ D, text_utils:duration_to_string(D) ] )
	 || D <- Durations ],


	io:format( "~n		Testing the upper-casing of first letter:~n" ),

	[ io:format( " - '~s' becomes '~s'~n",
				[ T, text_utils:uppercase_initial_letter(T) ] )
	 || T <- [ [], "a", "A", "Hello", "hello" ] ],

	io:format( "~n--> End of test for module ~s.~n", [ ?Tested_module ] ),

	erlang:halt().
