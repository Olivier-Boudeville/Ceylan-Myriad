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
% Creation date: July 1, 2007.


% Gathering of various convenient facilities.
% See text_utils_test.erl for the corresponding test.
-module(text_utils).



% Note that string:tokens can be used to split strings.



% String management functions.
-export([ term_toString/1, term_to_string/1, integer_to_string/1,
		 record_to_string/1,
		 string_list_to_string/1, string_list_to_atom_list/1,
		 version_to_string/1,
		 string_to_binary/1, binary_to_string/1,
		 strings_to_binaries/1, binaries_to_strings/1,
		 string_to_float/1,
		 percent_to_string/1, percent_to_string/2,
		 distance_to_string/1, distance_to_short_string/1,
		 duration_to_string/1,
		 uppercase_initial_letter/1,
		 join/2, remove_ending_carriage_return/1, format_text_for_width/2,
		 pad_string/2, is_string/1, is_list_of_strings/1 ]).


% Restructured-Text (RST) related functions.
-export([ generate_title/2 ]).


% Miscellaneous functions.
-export([ generate_text_name_from/1 ]).







% String management functions.


% Returns a human-readable string describing specified term.
term_toString(Term) ->
	case io_lib:printable_list(Term) of

		true ->
			io_lib:format("~s",[Term]);

		_    ->
			io_lib:format("~p",[Term])

	end.



term_to_string(Term) ->
	term_toString(Term).



% Avoids to have to use lists:flatten when converting an integer to a string.
% Useless when using functions like io:format, that accept iolists as
% parameters.
integer_to_string(IntegerValue) ->
	hd( io_lib:format( "~B", [IntegerValue] ) ).



% Returns a string describing the specified record.
% Hugely inspired from a Ulf Wiger's snippet. described in
% http://erlang.org/pipermail/erlang-questions/2006-September/023181.html
% Apparently, as records are compile-time structures only, there is no simple
% way of determining the name of their fields at runtime.
record_to_string(_Record) ->
	throw( {not_implemented,record_to_string} ).
	%RF = fun(R,L) when R == element(1,Record) ->
	%	% Needs apparently a parse transform:
	%   Fields = '#info-'(Record),
	%	true = (L == length(Fields)),
	%	Fields
	%end,
	%
	%io_lib_pretty:print(Record, RF).



% Returns a string which pretty-prints specified list of strings, with bullets.
string_list_to_string( ListOfStrings ) ->
	io_lib:format( "~n~s", [ string_list_to_string( ListOfStrings, [] ) ] ).


string_list_to_string( [], Acc ) ->
	 Acc;

string_list_to_string( [H|T], Acc ) when is_list(H) ->
	string_list_to_string( T, Acc ++ io_lib:format( " + ~s~n", [H] ) ).



% Returns a list whose elements are atoms corresponding to the strings
% supposedly composing the specified list.
% Ex: string_list_to_atom_list( ["abc", "def"] ) should return [abc,def].
string_list_to_atom_list( StringList ) when is_list(StringList) ->
	[ list_to_atom( X ) || X <- StringList ].




% Returns a string describing the specified three-element version.
version_to_string( {V1,V2,V3} ) ->
	io_lib:format( "~B.~B.~B", [V1,V2,V3] ).



% Returns a textual description of the specified percentage, expected to be a
% float in [0,1], with the default number of digits after the decimal point.
percent_to_string( Value ) ->
	percent_to_string( Value, _DefaultPrecision = 1 ).


% Returns a textual description of the specified percentage, expected to be a
% float in [0,1], with the specified number of digits after the decimal point.
percent_to_string( Value, Precision ) ->
	% Awful format string to determine:
	io_lib:format( "~.*f%", [ Precision, Value * 100 ] ).



% Returns an exact rounded textual description of the specified distance,
% expected to be expressed as a floating-point number of millimeters, which will
% be first rounded to nearest integer.
% Ex: for a distance of 1001.5 millimeters, returns "1m and 2mm".
distance_to_string( Millimeters ) when is_float(Millimeters) ->
	distance_to_string( round(Millimeters) );

% Returns an exact textual description of the specified distance, expected to be
% expressed as an integer number of millimeters.
% Ex: for an integer distance of 1000001 millimeters, returns "1km and 1mm".
distance_to_string( Millimeters ) ->

	Centimeters = 10,
	Meters = 100 * Centimeters,
	Km = Meters*Meters,

	ListWithKm = case Millimeters div Km of

					 0 ->
						 [];

					 KmNonNull->
						 [io_lib:format( "~Bkm", [KmNonNull] )]

				   end,
	DistAfterKm = Millimeters rem Km,
	%io:format( "DistAfterKm = ~B.~n", [DistAfterKm] ),

	ListWithMeters = case DistAfterKm div Meters of

					 0 ->
						 ListWithKm;

					 MetersNonNull->
						 [io_lib:format( "~Bm", [MetersNonNull] )|ListWithKm]

				   end,
	DistAfterMeters = DistAfterKm rem Meters,
	%io:format( "DistAfterMeters = ~B.~n", [DistAfterMeters] ),

	ListWithCentimeters = case DistAfterMeters div Centimeters of

					 0 ->
						 ListWithMeters;

					 CentNonNull->
						 [io_lib:format( "~Bcm", [CentNonNull])|ListWithMeters]

				   end,
	DistAfterCentimeters = DistAfterMeters rem Centimeters,
	%io:format( "DistAfterCentimeters = ~B.~n", [DistAfterCentimeters] ),

	ListWithMillimeters = case DistAfterCentimeters of

					 0 ->
						 ListWithCentimeters;

					 AtLeastOneMillimeter ->
						 [ io_lib:format( "~Bmm", [AtLeastOneMillimeter] )
						   | ListWithCentimeters ]

				   end,

	%io:format( "Unit list is: ~w.~n", [ListWithMillimeters] ),

	% Preparing for final display:
	case ListWithMillimeters of

		[] ->
			"0mm";

		[OneElement] ->
			OneElement;

		[Smaller|Bigger] ->
			join( ", ", lists:reverse(Bigger) ) ++ " and " ++ Smaller

	end.



% Returns an approximate textual description of the specified distance, expected
% to be expressed as a floating-point number of millimeters, which will be first
% rounded to nearest integer.
%
% Only one unit, the most appropriate one, will be used, with up to 1 figure
% after the comma.
%
% Ex: for a distance of 1000.5 millimeters, returns "1.0m".
distance_to_short_string( Millimeters ) when is_float(Millimeters) ->
	distance_to_short_string( round(Millimeters) );

% Returns an approximate textual description of the specified distance, expected
% to be expressed as an integer number of millimeters.
%
% Only one unit, the most appropriate one, will be used, with up to 1 figure
% after the comma.
%
% Ex: for a distance of 1000001 millimeters, returns "1.0km".
distance_to_short_string( Millimeters ) ->

	% Note: very specific limit distances could be better managed.
	% Ex: 999999 millimeters is 999m, 99cm and 9mm, and "1000.0m" due to
	% rounding, whereas we would have preferred "1km".

	Centimeters = 10,
	Meters = 100 * Centimeters,
	Km = Meters*Meters,

	% First, guess the most suitable unit, then use it:

	case Millimeters div Km of

		0 ->
			% Kilometers are too big:
			case Millimeters div Meters of

				0 ->
					% Meters are too big:
					case Millimeters div Centimeters of

						0 ->
							% Centimeters are too big, stick to mm:
							io_lib:format( "~Bmm", [Millimeters] );

						_CmNonNull ->
							io_lib:format( "~.1fcm", [Millimeters/Centimeters] )

					end;

				 _MetersNonNull ->
					io_lib:format( "~.1fm", [Millimeters/Meters] )

			end;

		_KmNonNull->
			io_lib:format( "~.1fkm", [Millimeters/Km] )

	end.



% Returns an approximate textual description of the specified duration, expected
% to be expressed as an integer number of milliseconds.
%
% Ex: for a duration of ms, returns "".
%
% See also: basic_utils:get_textual_duration/2.
duration_to_string( Milliseconds ) ->

	FullSeconds = Milliseconds div 1000,
	{Days,{Hours,Minutes,Seconds}} = calendar:seconds_to_daystime(FullSeconds),

	ListWithDays = case Days of

					   0 ->
						   [];

					   1 ->
						   [ "1 day" ];

					   _ ->
						   [ io_lib:format("~B days",[Days]) ]

				   end,

	ListWithHours = case Hours of

						0 ->
							ListWithDays;

						1 ->
							[ "1 hour" |ListWithDays];

						_ ->
							[ io_lib:format("~B hours",[Hours]) |ListWithDays]

					end,

	ListWithMinutes = case Minutes of

						  0 ->
							  ListWithHours;

						  1 ->
							  [ "1 minute" |ListWithHours];

						  _ ->
							  [ io_lib:format("~B minutes",[Minutes])
							   |ListWithHours]

					  end,

	ListWithSeconds = case Seconds of

						  0 ->
							  ListWithMinutes;

						  1 ->
							  [ "1 second" |ListWithMinutes];

						  _ ->
							  [ io_lib:format("~B seconds",[Seconds]) |
							   ListWithMinutes]

					  end,

	ActualMilliseconds = Milliseconds rem 1000,

	ListWithMilliseconds = case ActualMilliseconds of

							   0 ->
								   ListWithSeconds;

							   1 ->
								   [ "1 millisecond" |ListWithSeconds];

							   _ ->
								   [ io_lib:format("~B milliseconds",
												   [ActualMilliseconds])
									|ListWithSeconds]

						   end,

	% Preparing for final display:
	case ListWithMilliseconds of

		[] ->
			"0 millisecond";

		[OneElement] ->
			OneElement;

		[Smaller|Bigger] ->
			join( ", ", lists:reverse(Bigger) ) ++ " and " ++ Smaller

	end.



% Converts a plain (list-based) string into a binary.
string_to_binary( String ) ->
	erlang:list_to_binary(String).


% Converts a binary into a plain (list-based)string.
binary_to_string( Binary ) ->
	erlang:binary_to_list(Binary).


% Converts a list of plain (list-based) strings into a list of binaries.
% Order of items remains unaffected.
strings_to_binaries( StringList ) ->
	% Order must be preserved:
	[ erlang:list_to_binary(S) || S <- StringList ].


% Converts a list of binaries into list of plain (list-based) string.
% Order of items remains unaffected.
binaries_to_strings( BinaryList ) ->
	% Order must be preserved:
	[ erlang:binary_to_list(B) || B <- BinaryList ].


% Returns a float which corresponds to the specified text, not depending on its
% being defined as an integer or as a float.
% Throws an exception if the conversion failed.
string_to_float( String ) ->

	try list_to_float( String ) of

		F ->
			F

	catch

		error:badarg ->
			try list_to_integer( String ) of

				I ->
					float(I)
			catch

				error:badarg ->
					throw( {float_conversion_failed,String} )

			end

	end.



% Returns the specified string, ensuring that its first letter is a majuscule,
% uppercasing it if necessary.
uppercase_initial_letter( [] ) ->
	[];

uppercase_initial_letter( [First|Others] ) ->
	[string:to_upper(First)|Others].



% join(Separator,ListToJoin), ex: join( '-', [ "Barbara", "Ann" ] ).
%
% Python-like 'join', combines items in a list into a string using a separator
% between each item representation.
%
% Inspired from http://www.trapexit.org/String_join_with.
%
% For file-related paths, you are expected to use portable standard
% filename:join functions instead.
%
% Note: use string:tokens to split the string.
join(_Separator,[]) ->
	"";

join(Separator,ListToJoin) ->
	lists:flatten( lists:reverse( join(Separator, ListToJoin, []) ) ).


join(_Separator,[],Acc) ->
	Acc;

join(_Separator,[H| [] ],Acc) ->
	[H|Acc];

join(Separator,[H|T],Acc) ->
	join(Separator, T, [Separator,H|Acc]).



% Removes the ending "\n" character(s) of specified string.
remove_ending_carriage_return(String) when is_list(String) ->
	% 'Res ++ "\n" = String,Res' will not work:
	string:strip( String, right, $\n ).




% Formats specified text according to specified width, expressed in characters.
% Returns a line of strings, each of which having Width characters.
format_text_for_width( Text, Width ) ->
	% Whitespaces converted to spaces:
	CleanedTest = re:replace( lists:flatten(Text), "\\s+", " ",
		[global,{return, list}] ),
	WordList = string:tokens( CleanedTest, " " ),
	%io:format( "Formatting ~p.~n", [WordList] ),
	join_words( WordList, Width ).



% Joins words from the list, line by line.
join_words( WordList, Width ) ->
	join_words( WordList, Width, _Lines = [], _CurrentLine = "",
		_CurrentLineLen = 0 ).


join_words( [], _Width, AccLines, _CurrentLine, _CurrentLineLen = 0 ) ->
	% Ended with a full line:
	lists:reverse(AccLines);

join_words( [], Width, AccLines, CurrentLine, _CurrentLineLen ) ->
	% Ended with a partial line:
	lists:reverse( [pad_string(CurrentLine,Width)|AccLines] );

join_words( [Word|RemainingWords], Width, AccLines, CurrentLine,
		CurrentLineLen ) ->

	%io:format( "Managing word '~s' (len=~B), current line is '~s' (len=~B), "
	%	"width = ~B.~n", [Word,length(Word),CurrentLine,CurrentLineLen,Width] ),

	% Length should be incremented, as a space must be inserted before that
	% word, however we want to accept words whose width would be exactly equal
	% to the line width:
	ActualLength = case CurrentLine of

		"" ->
			length(Word);

		_NonEmpty ->
			% Already at least a letter, we therefore must add a space before
			% the new word:
			length(Word) + 1

	end,
	case ActualLength of

		CompatibleWidth when CompatibleWidth =< Width ->
			% Word width is manageable.
			% Will this word fit on the current line?
			% It depends on the
			case CurrentLineLen + CompatibleWidth of

				FittingLen when FittingLen =< Width ->
					% Yes, this word fits on the current line.
					% Avoids adding a space at the beginning of a new line:
					{NewCurrentLine,NewLineLen} = case CurrentLineLen of

						0 ->
							{Word,CompatibleWidth};

						Len ->
							{CurrentLine ++ " " ++ Word,Len+CompatibleWidth+1}

					end,
					%io:format("Current line is now '~s'.~n", [NewCurrentLine]),
					join_words( RemainingWords, Width, AccLines, NewCurrentLine,
						NewLineLen );

				_ExceedingLen ->
					% No, with this word the current line would be too wide,
					% inserting it on new line instead:
					PaddedCurrentLine = pad_string( CurrentLine, Width ),
					%io:format( "Inserting line '~s'.~n", [PaddedCurrentLine] ),
					join_words( RemainingWords, Width,
						[PaddedCurrentLine|AccLines], Word, CompatibleWidth )

			end;

		_TooLargeWidth ->
			% Will break words as many times as needed:
			%io:format( "Word '~s' is too large (len=~B), breaking it.~n",
			%	[Word,length(Word)] ),
			Subwords = break_word(Word,Width),
			PaddedCurrentLine = pad_string( CurrentLine, Width ),
			join_words( Subwords ++ RemainingWords, Width,
				[PaddedCurrentLine|AccLines], "", 0 )

	end.



% Returns the specified string, padded with spaces to specified width,
% left-justified.
pad_string( String, Width ) when length(String) =< Width ->
	lists:flatten( io_lib:format( "~*.s", [ -Width, String ] ) ).



% Returns true iff the parameter is a (non-nested) string (actually a plain list
% of integers).
%
% Taken from http://lethain.com
% (see distinguishing-strings-from-lists-in-erlang)
% Note: something like [ $e, 1, 2, $r ] is deemed to be a string.
is_string( [] ) ->
	true;

is_string( [H|_] ) when not is_integer(H) ->
	false;

is_string( [_|T] ) ->
	is_string(T);

is_string( _Other ) ->
	false.



% Returns true iff the specified parameter is a list whose all elements are
% strings.
is_list_of_strings( [] ) ->
	true;

is_list_of_strings( [H|T] ) ->

	case is_string(H) of

		true ->
			is_list_of_strings( T );

		false ->
			false

	end.



% Returns a list of words obtained from the breaking of specified word,
% according to specified maximum width.
% Parts of that word will use a separating dash.
% Ex: break_word("simulator",5) returns ["simu-","lator"]
break_word(Word,Width) ->
	%io:format( "Breaking '~s' for width ~B.~n", [Word,Width] ),
	%timer:sleep(500),
	% We do not want to have underscores in the word, as if the word happens
	% to be broken just after an underscore, RST will interpret it as a link.
	% Therefore we escape underscores:
	% Used to cut into halves, then preferring truncating a first full-length
	% chunk, finally directly cutting the word into appropriate pieces:
	%CutIndex = length(Word) div 2,
	%CutIndex = Width-1,
	cut_into_chunks( Word, Width, [] ).



% Cuts specified string into pieces, each of them having to fit in specified
% width.
cut_into_chunks( _String = [], _ChunkSize, Acc ) ->
	%io:format( "cut_into_chunks return ~p.", [lists:reverse(Acc)]),
	lists:reverse(Acc);

% Last word may take the full width (no dash to add):
cut_into_chunks( String, ChunkSize, Acc ) when length(String) =< ChunkSize ->
	cut_into_chunks( [], ChunkSize, [String|Acc] );

% Here we have to cut the string anyway:
cut_into_chunks( String, ChunkSize, Acc ) ->
	% Rule is to add (and convert) characters until the end of line:
	% (ChunkSize decremented as "-" will be added)
	{FirstPart,Remaining} = aggregate_word( String, ChunkSize-1, [] ),
	% Each underscore will result into another character (\) being added:
	%io:format( "FirstPart = '~s' (~B), Remaining = '~s'.~n",
	%	[FirstPart,length(FirstPart),Remaining] ),
	cut_into_chunks( Remaining, ChunkSize, [ FirstPart ++ "-" |Acc] ).



aggregate_word( String, 0, Acc ) ->
	{lists:reverse(Acc),String};


% An underscore once escaped would not fit, as it would result into two
% characters ('\_'):
aggregate_word( String = [$_|_T], 1, Acc ) ->
	aggregate_word( String, 0, Acc );

% An escaped underscore will fit:
aggregate_word( [$_|T], Count, Acc ) ->
	% Adding '_\' as it will reversed (into the expected '\_'):
	aggregate_word( T, Count-2, [$\_,$\\|Acc] );

aggregate_word( [H|T], Count, Acc ) ->
	aggregate_word( T, Count-1, [H|Acc] ).




% Restructured-Text (RST) related functions.


% Generates a RST-compatible standard title, with the proper ASCII art.
% Follows our general conventions regarding title level, from H1 to Hn.
generate_title( Title, Level ) ->
	{Char,Layout} = get_title_rendering_for( Level ),
	TitleLine = get_line_of( Char, length(Title) ) ++ "\n",
	case Layout of

		only_below ->
			Title ++ "\n" ++ TitleLine ++ "\n";

		below_and_on_top ->
			TitleLine ++ Title ++ "\n" ++ TitleLine ++ "\n"

	end.



% Returns how a title with specified level can be rendered.
% See demo-for-css-testing.rst for the convention.
get_title_rendering_for( 1 ) ->
	{ $=, below_and_on_top };

get_title_rendering_for( 2 ) ->
	{ $-, below_and_on_top };

get_title_rendering_for( 3 ) ->
	{ $=, only_below };

get_title_rendering_for( 4 ) ->
	{ $-, only_below };

get_title_rendering_for( 5 ) ->
	{ $., only_below };

get_title_rendering_for( 6 ) ->
	{ $_, only_below };

get_title_rendering_for( 7 ) ->
	{ $*, only_below };

get_title_rendering_for( 8 ) ->
	{ $:, only_below };

get_title_rendering_for( 9 ) ->
	{ $+, only_below }.




% Returns a line made of Lenght characters "Character".
% Ex: get_line_of( $+, 5 ) = "+++++".
get_line_of( Character, Length ) ->
	lists:flatten( [ Character || _X <- lists:seq( 1, Length ) ] ).


% Miscellaneous functions.


% Tries to return a string adequate to form a simple name (mostly alphanumerical
% with underscores) from specified term.
%
% See also: file_utils:convert_to_filename/1.
generate_text_name_from(Term) ->
	String = term_to_string(Term),
	fix_characters(String).




% Non-exported helper functions.

fix_characters(String) ->
	lists:reverse( fix_characters(lists:flatten(String),[]) ).


fix_characters([],Acc) ->
	Acc;

% 32 corresponds to space ('$ '):
fix_characters([32|T],Acc) ->
	fix_characters(T,["_"|Acc]);

fix_characters([$'|T],Acc) ->
	fix_characters(T,["_"|Acc]);

fix_characters([H|T],Acc) ->
	fix_characters(T,[H|Acc]).
