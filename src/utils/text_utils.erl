% Copyright (C) 2003-2020 Olivier Boudeville
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
% Creation date: July 1, 2007.


% Gathering of various convenient facilities.
%
% See text_utils_test.erl for the corresponding test.
%
-module(text_utils).



% Note that string:tokens/1 can be used to split strings.



% String management functions.


% Conversions between terms and strings (both ways).
-export([ term_to_string/1, term_to_string/2, term_to_string/3,
		  term_to_binary/1,
		  integer_to_string/1, atom_to_string/1, pid_to_string/1,
		  record_to_string/1,
		  strings_to_string/1, strings_to_sorted_string/1,
		  strings_to_string/2, strings_to_sorted_string/2,
		  strings_to_enumerated_string/1, strings_to_enumerated_string/2,
		  strings_to_listed_string/1,
		  binaries_to_string/1, binaries_to_string/2,
		  binaries_to_sorted_string/1, binaries_to_listed_string/1,
		  atoms_to_string/1, atoms_to_sorted_string/1, atoms_to_listed_string/1,
		  proplist_to_string/1, version_to_string/1,
		  atom_to_binary/1,
		  string_to_binary/1, binary_to_string/1,
		  strings_to_binaries/1, binaries_to_strings/1,
		  string_to_integer/1, try_string_to_integer/1, try_string_to_integer/2,
		  string_to_float/1, try_string_to_float/1,
		  string_to_atom/1, strings_to_atoms/1,
		  terms_to_string/1, terms_to_enumerated_string/1,
		  terms_to_listed_string/1,
		  binary_to_atom/1, float_to_string/1,
		  percent_to_string/1, percent_to_string/2,
		  distance_to_string/1, distance_to_short_string/1,
		  duration_to_string/1,
		  format/2, bin_format/2, format/3,
		  ensure_string/1, ensure_binary/1 ]).



% Other string operations:
-export([ get_lexicographic_distance/2, get_longest_common_prefix/1,
		  uppercase_initial_letter/1,
		  to_lowercase/1, to_uppercase/1,
		  join/2,
		  split/2, split_at_whitespaces/1, split_at_first/2, split_camel_case/1,
		  tokenizable_to_camel_case/2,

		  substitute/3, filter/2, split_after_prefix/2,
		  update_with_keywords/2,

		  list_whitespaces/0,

		  escape_single_quotes/1, escape_double_quotes/1,
		  escape_all_quotes/1,

		  is_uppercase/1, is_figure/1,
		  remove_ending_carriage_return/1, remove_last_characters/2,
		  remove_whitespaces/1,

		  trim_whitespaces/1, trim_leading_whitespaces/1,
		  trim_trailing_whitespaces/1,

		  get_default_bullet/0, get_bullet_for_level/1,
		  format_text_for_width/2,
		  pad_string/2, pad_string_left/2, pad_string_right/2,
		  is_string/1, is_non_empty_string/1, is_list_of_strings/1,
		  is_bin_string/1,
		  is_list_of_binaries/1 ]).


% Restructured-Text (RST) related functions.
-export([ generate_title/2 ]).


% HTTP-related operations.
-export([ encode_as_url/1, encode_element_as_url/1, escape/1 ]).



% Miscellaneous functions.
-export([ generate_text_name_from/1 ]).


% This module being a bootstrap one, the 'table' pseudo-module is not available
% (as this module is by design not processed by the 'Myriad' parse transform):
%
-define( table, map_hashtable ).


% Type section.

% These strings are supposed to contain Erlang-fashioned format characters, like
% in "hello ~p!":
%
-type format_string() :: string().


% Lists of terms corresponding to a format string:
-type format_values() :: [ term() ].


% These strings are supposed to contain Regular Expressions, like in:
% "*-emitter-(first|second)-*".
%
% Patterns are to be expressed according to the “Perl Compatible Regular
% Expressions” conventions, or PCRE for short.
% For more information, see following cheat sheet:
% http://www.bitcetera.com/page_attachments/0000/0030/regex_in_a_nutshell.pdf
%
% See also: http://erlang.org/doc/man/re.html
%
-type regex_string() :: string().


% A string that describes a title:
-type title() :: string().


% A string that describes a label:
-type label() :: string().


% A binary corresponding to a string:
-type bin_string() :: binary().


% Any kind of string (a.k.a chardata() :: charlist() | unicode_binary()):
-type any_string() :: string() | bin_string().



% A Unicode string.
%
% This is our new default.
%
-type unicode_string() :: unicode:chardata().


% A Unicode codepoint for a character.
%
% (unfortunately we cannot define a text_utils:char/0 type, as "type char()
% is a builtin type; it cannot be redefined").
%
-type uchar() :: integer().


% Now our default:
%
% (unfortunately we cannot define a text_utils:string/0 type, as "type string()
% is a builtin type; it cannot be redefined").
%
-type ustring() :: unicode_string().


% Any kind of terms that can be directly mapped to a string (typically accepted
% by ~s in format strings):
%
-type string_like() :: string() | unicode_string() | bin_string() | atom().


% Like io_list(), with possibly binaries:
-type string_list() :: [ string_like() ].

% To convert keywords:
-type translation_table() :: ?table:?table( any_string(), any_string() ).


% The level of indentation (starts at zero, and the higher, the most nested).
-type indentation_level() :: basic_utils:level().


% A bullet, to denote the elements of a list.
-type bullet() :: ustring().


% Either an indentation level, or directly a bullet:
-type indentation_level_or_bullet() :: indentation_level() | bullet().


% Lexicographic (Levenshtein) distance, i.e. minimum number of single-character
% edits (i.e. insertions, deletions or substitutions) required to change one
% string into the other:
%
-type distance() :: non_neg_integer().


-export_type([ format_string/0, format_values/0,
			   regex_string/0, title/0, label/0,
			   bin_string/0, any_string/0, unicode_string/0, uchar/0, ustring/0,
			   string_like/0, string_list/0,
			   translation_table/0, indentation_level/0, distance/0 ]).




% Maybe at least format/2 would be better inlined, however it is no cross-module
% inlining (just inside this module), so a parse-transform may be used in order
% to transform text_utils:format/2 into io_lib:format/2 or into its actual,
% safer code.
%
-compile( { inline, [ format/2 ] } ).



% String management functions.


% Returns a human-readable string describing specified term.
-spec term_to_string( term() ) -> string().
term_to_string( _Term=[] ) ->
	% Otherwise would be an empty string:
	"[]";

term_to_string( Term ) ->

	case io_lib:printable_list( Term ) of

		true ->
			io_lib:format( "~ts", [ Term ] );

		_    ->
			io_lib:format( "~p", [ Term ] )

	end.



% Returns a human-readable binary string describing specified term.
-spec term_to_binary( term() ) -> bin_string().
term_to_binary( Term ) ->
	String = term_to_string( Term ),
	string_to_binary( String ).



% Returns a human-readable string describing specified term, up to the specified
% nesting depth.
%
-spec term_to_string( term(), basic_utils:count() ) -> string().
term_to_string( _Term=[], _MaxDepthCount ) ->
	% Otherwise would be an empty string:
	"[]";

term_to_string( Term, MaxDepthCount ) ->

	case io_lib:printable_list( Term ) of

		true ->
			io_lib:format( "~ts", [ Term ] );

		_    ->
			io_lib:format( "~P", [ Term, MaxDepthCount ] )

	end.



% Returns a human-readable string describing specified term, up to the specified
% nesting depth, and up to specified string length (at least 3, so that the
% "..." marker can be inserted.
%
-spec term_to_string( term(), basic_utils:count(), basic_utils:count() )->
							string().
term_to_string( _Term=[], _MaxDepthCount, _MaxLength ) ->
	% Otherwise would be an empty string:
	"[]";

term_to_string( Term, MaxDepthCount, MaxLength ) when MaxLength >= 3 ->

	% First limit the depth (beware of IO-lists!):
	FullString = case io_lib:printable_list( Term ) of

		true ->
			% The '*' character in the format string is not suitable here:
			lists:flatten( io_lib:format( "~ts", [ Term ] ) );

		_ ->
			lists:flatten( io_lib:format( "~P", [ Term, MaxDepthCount ] ) )

	end,

	% Then limit the length:
	case length( FullString ) of

		L when L > MaxLength ->
			% We have to truncate here, length( "..." ) = 3
			% MaxLength - 3 = 0 is allowed there:
			string:sub_string( FullString, 1, MaxLength - 3 ) ++ " ..";

		_ ->
			FullString

	end.



% Avoids to have to use lists:flatten when converting an integer to a string.
% Useless when using functions like io:format, that accept iolists as
% parameters.
%
-spec integer_to_string( integer() ) -> string().
integer_to_string( IntegerValue ) ->
	% Nonsensical: hd( io_lib:format( "~B", [ IntegerValue ] ) ).
	%io_lib:format( "~B", [ IntegerValue ] ).
	erlang:integer_to_list( IntegerValue ).


% Returns a plain string corresponding to the specified atom.
-spec atom_to_string( atom() ) -> string().
atom_to_string( Atom ) ->
	atom_to_list( Atom ).



% Returns a plain string corresponding to the specified PID.
-spec pid_to_string( pid() ) -> string().
pid_to_string( Pid ) ->

	% PID are akin to <X.Y.Z>.

	PidAsText = lists:flatten( io_lib:format( "~w", [ Pid ] ) ),

	%io:format( "PID: ~w.~n", [ self() ] ) ,
	% Ex: ["<0","33","0>"]:
	[ $< | Rest ] = PidAsText,

	% Returns "X.Y.Z":
	list_utils:remove_last_element( Rest ).

	% Automatic truncating if defaults currently deactivated:
	% ActualFirst = case First of

	%		"0" ->
	%			[];

	%		_ ->
	%			First ++ "."

	% end,

	% ActualThird = case Third of

	%		"0" ->
	%			[];

	%		_ ->
	%			"." ++ Third

	% end,

	% ActualFirst ++ Second ++ ActualThird.



% Returns a string describing the specified record.
%
% Hugely inspired from a Ulf Wiger's snippet. described in
% http://erlang.org/pipermail/erlang-questions/2006-September/023181.html
%
% Apparently, as records are compile-time structures only, there is no simple
% way of determining the name of their fields at runtime.
%
-spec record_to_string( _ ) -> none().
record_to_string( _Record ) -> % No 'when is_record( Record, Tag ) ->' here.

	throw( { not_implemented, record_to_string } ).

	%RF = fun(R,L) when R == element(1,Record) ->
	%	% Needs apparently a parse transform:
	%   Fields = '#info-'(Record),
	%	true = (L == length(Fields)),
	%	Fields
	%end,
	%
	%io_lib_pretty:print( Record, RF ).



% Returns the default bullet to be used for top-level lists.
-spec get_default_bullet() -> ustring().
get_default_bullet() ->
	get_bullet_for_level( 0 ).



% Returns the bullet to be used for specified indentation level.
-spec get_bullet_for_level( indentation_level() ) -> bullet().
get_bullet_for_level( 0 ) ->
	" + ";

get_bullet_for_level( 1 ) ->
	"   - ";

get_bullet_for_level( 2 ) ->
	"     * ";

get_bullet_for_level( N ) when is_integer( N ) andalso N > 0 ->
	Base = get_bullet_for_level( N rem 3 ),
	string:copies( "   ", N div 3 ) ++ Base.



% Returns the indentation offset to be used for specified indentation level of
% enumerated lists.
%
-spec get_indentation_offset_for_level( indentation_level() ) ->  ustring().
get_indentation_offset_for_level( N ) ->
	string:copies( _BaseString=" ", _Count=N+1 ).



% (helper)
%
% Note: the caller should have already vetted the specified arguments.
%
strings_to_string( _ListOfStrings=[], Acc, _Bullet ) ->
	 Acc;

% We do not want an extra newline at the end:
strings_to_string( _ListOfStrings=[ LastString ], Acc, Bullet )
  when is_list( LastString ) ->
	%Pattern="~ts~n",
	% Added back, as makes sense?
	% Nope:
	Pattern="~ts",
	Acc ++ Bullet ++ io_lib:format( Pattern, [ LastString ] );

strings_to_string( _ListOfStrings=[ H | T ], Acc, Bullet )
  when is_list( H ) ->
	% Byproduct of the trailing newline: an empty line at the end if nested.
	strings_to_string( T, Acc ++ Bullet ++ io_lib:format( "~ts~n", [ H ] ),
					   Bullet );

strings_to_string( _ListOfStrings=[ H | _T ], _Acc, _Bullet ) ->
	throw( { not_a_string, H } ).




% Returns a string that pretty-prints specified list of strings, with
% enumerated (i.e. 1, 2, 3) bullets.
%
-spec strings_to_enumerated_string( [ ustring() ] ) -> ustring().
strings_to_enumerated_string( ListOfStrings ) ->
	strings_to_enumerated_string( ListOfStrings, _DefaultIndentationLevel=0 ).


-spec strings_to_enumerated_string( [ ustring() ], indentation_level() ) ->
										  ustring().
strings_to_enumerated_string( ListOfStrings, IndentationLevel ) ->

	Prefix = get_indentation_offset_for_level( IndentationLevel ),

	{ _FinalCount, ReversedStrings } = lists:foldl(
				 fun( String, _Acc={ Count, Strings } ) ->

					 NewStrings = [ format( "~s~B. ~ts~n",
									   [ Prefix, Count, String ] ) | Strings ],
					 { Count + 1, NewStrings }

				 end,
				 _Acc0={ 1, "" },
				 _List=ListOfStrings ),

	OrderedStrings = lists:reverse( ReversedStrings ),

	format( "~n~ts", [ lists:flatten( OrderedStrings ) ] ).



% Returns a string that pretty-prints specified list of strings (actually, any
% element that can be processed with ~s will do; ex: atoms), with default
% bullets.
%
-spec strings_to_string( [ ustring() ] ) -> ustring().
strings_to_string( [] ) ->
	"(empty list)";

strings_to_string( L=[ SingleString ] ) when is_list( SingleString ) ->

	% Not retained, as the single string may itself correspond to a full, nested
	% list and no dangling final quote is desirable:
	%io_lib:format( " '~ts'", L );

	% No leading space, the caller is expected to have it specified by himself,
	% like in: "foo: ~s", not as "foo:~s":

	%io_lib:format( " ~ts", L );
	io_lib:format( "~ts", L );

strings_to_string( ListOfStrings ) when is_list( ListOfStrings ) ->

	%trace_utils:debug_fmt( "Stringifying ~p.", [ ListOfStrings ] ),

	% Leading '~n' had been removed for some unknown reason:
	io_lib:format( "~n~ts~n", [ strings_to_string(
					   ListOfStrings, _Acc=[], get_default_bullet() ) ] );

strings_to_string( ErrorTerm ) ->
	throw( { not_a_list, ErrorTerm } ).



% Returns a string that pretty-prints specified list of strings (actually, any
% element that can be processed with ~s will do; ex: atoms) once reordered (and
% with default bullets).
%
-spec strings_to_sorted_string( [ ustring() ] ) -> ustring().
strings_to_sorted_string( ListOfStrings ) when is_list( ListOfStrings ) ->
	strings_to_string( lists:sort( ListOfStrings ) );

strings_to_sorted_string( ErrorTerm ) ->
	throw( { not_a_list, ErrorTerm } ).



% Returns a string that pretty-prints specified list of strings (actually, any
% element that can be processed with ~s will do; ex: atoms), with user-specified
% bullets.
%
% This can be a solution to nest bullet lists, by specifying a bullet with an
% offset, such as " * ".
%
-spec strings_to_string( [ ustring() ], indentation_level_or_bullet() ) ->
							   ustring().
strings_to_string( _ListOfStrings=[ SingleString ], _LevelOrBullet )
  when is_list( SingleString ) ->
	% For a single string, no need for leading and trailing newlines, but it
	% used to be separated (with single quotes) from the surrounding text
	% (not done anymore, as the single may be itself a bullet list)
	io_lib:format( "~ts", [ SingleString ]);

strings_to_string( ListOfStrings, IndentationLevel )
  when is_integer( IndentationLevel ) ->
	Bullet = get_bullet_for_level( IndentationLevel ),
	strings_to_string( ListOfStrings, Bullet );

strings_to_string( ListOfStrings, Bullet ) when is_list( ListOfStrings )
												andalso is_list( Bullet ) ->
	% Leading '~n' had been removed for some unknown reason:

	% Trailing '~n' was removed (as was inducing a too large final blank space),
	% yet proved necessary (otherwise text may continue just at the right of the
	% last bullet; only drawback: indeed, many intermediary and final blank
	% lines inserted when nesting lists):
	%
	% Finally we were not able to reproduce the continuing text on a simple
	% test, so:
	%Pattern = "~n~ts~n",
	Pattern = "~n~ts",

	io_lib:format( Pattern, [ strings_to_string( ListOfStrings, _Acc=[],
												 Bullet ) ] );

strings_to_string( ListOfStrings, Bullet ) when is_list( Bullet ) ->
	throw( { not_a_list, ListOfStrings } );

strings_to_string( _ListOfStrings, ErrorTerm ) ->
	throw( { bullet_not_a_string, ErrorTerm } ).



% Returns a string that pretty-prints specified list of strings (actually, any
% element that can be processed with ~s will do; ex: atoms) once reordered, with
% user-specified indentation level or bullet.
%
-spec strings_to_sorted_string( [ ustring() ],
								indentation_level_or_bullet() ) -> ustring().
strings_to_sorted_string( ListOfStrings, IndentationOrBullet )
  when is_list( ListOfStrings ) ->
	strings_to_string( lists:sort( ListOfStrings ), IndentationOrBullet );

strings_to_sorted_string( ErrorTerm, _IndentationOrBullet ) ->
	throw( { not_a_list, ErrorTerm } ).



% Returns a string that pretty-prints specified list of binary strings, with
% default bullets.
%
-spec binaries_to_string( [ binary() ] ) -> ustring().
binaries_to_string( ListOfBinaries ) ->
	binaries_to_string( ListOfBinaries, _IndentationLevel=0 ).



% Returns a string that pretty-prints specified list of binary strings, with
% specified indentation level or bullet.
%
-spec binaries_to_string( [ binary() ], indentation_level_or_bullet() ) ->
								ustring().
binaries_to_string( ListOfBinaries, IndentationOrBullet ) ->
	Strings = binaries_to_strings( ListOfBinaries ),
	strings_to_string( Strings, IndentationOrBullet ).



% Returns a string that pretty-prints specified list of sorted binary strings,
% with default bullets.
%
-spec binaries_to_sorted_string( [ binary() ] ) -> ustring().
binaries_to_sorted_string( ListOfBinaries ) ->
	Strings = binaries_to_strings( ListOfBinaries ),
	strings_to_string( lists:sort( Strings ) ).



% Returns a string that pretty-prints the specified list of binary strings,
% listed directly along the text (not one item per line).
%
% Ex: binaries_to_listed_string( [ <<"red">>, <<"blue">>, <<"green">> ] )
% returns "red, blue and green".
%
-spec binaries_to_listed_string( [ binary() ] ) -> ustring().
binaries_to_listed_string( ListOfBinaries ) ->
	strings_to_listed_string( [ binary_to_string( B )
								|| B <- ListOfBinaries ] ).


% Returns a string that pretty-prints specified list of atoms, with default
% bullets.
%
-spec atoms_to_string( [ atom() ] ) -> ustring().
atoms_to_string( ListOfAtoms ) ->
	io_lib:format( "~n~ts", [ atoms_to_string( ListOfAtoms, [] ) ] ).


atoms_to_string( [], Acc ) ->
	 Acc;

atoms_to_string( [ H | T ], Acc ) when is_atom( H )  ->
	atoms_to_string( T, Acc ++ get_default_bullet()
						 ++ io_lib:format(  "~ts~n", [ H ] ) ).



% Returns a string that pretty-prints the specified list of atoms once ordered,
% with default bullets.
%
-spec atoms_to_sorted_string( [ atom() ] ) -> ustring().
atoms_to_sorted_string( ListOfAtoms ) ->
	atoms_to_string( lists:sort( ListOfAtoms ) ).



% Returns a string that pretty-prints the specified list of atoms, listed
% directly in the text.
%
% Ex: atoms_to_listed_string( [ red, blue, green ] ) returns "red, blue and
% green".
%
-spec atoms_to_listed_string( [ atom() ] ) -> ustring().
atoms_to_listed_string( ListOfAtoms ) ->
	ListOfStrings = [ atom_to_string( A ) || A <- ListOfAtoms ],
	strings_to_listed_string( ListOfStrings ).



% Returns a string that pretty-prints the specified list of strings, listed
% directly along the text (not one item per line).
%
% Ex: strings_to_listed_string( [ "red", "blue", "green" ] ) returns "red, blue
% and green".
%
%strings_to_listed_string( _ListOfStrings=[] ) ->
%	throw( empty_list_of_strings_to_list );
% Probably more relevant:
strings_to_listed_string( _ListOfStrings=[] ) ->
	"";

strings_to_listed_string( _ListOfStrings=[ SingleString ] ) ->
	SingleString;

strings_to_listed_string( ListOfStrings ) ->

	% Here all strings shall be separated with commas, except the last, starting
	% with "and":

	% We do not want here a dependency onto list_utils, which is not
	% bootstrapped, as this current function might be called from the Myriad
	% parse transform.

	%{ LastString, OtherStrings } = list_utils:extract_last_element(
	%								 ListOfStrings ),

	% A somewhat inlined version of it:
	[ LastString | RevOtherStrings ] = lists:reverse( ListOfStrings ),

	OtherStrings = lists:reverse( RevOtherStrings ),

	OtherStringsString = join( ", ", OtherStrings ),

	format( "~ts and ~ts", [ OtherStringsString, LastString ] ).



% Returns a list whose elements are atoms corresponding to the plain strings
% supposedly composing the specified list.
%
% Ex: strings_to_atoms( ["abc","def"] ) should return [ abc, def ].
%
% Note that only a bounded number of atoms should be created that way, lest the
% atom table gets saturated.
%
-spec strings_to_atoms( [ ustring() ] ) -> [ atom() ].
strings_to_atoms( StringList ) when is_list( StringList ) ->
	[ list_to_atom( X ) || X <- StringList ].



% Returns a string that pretty-prints specified list of key (as binary, string
% or atom) / value pairs, with bullets, after having been sorted.
%
% Ex: proplist_to_string( [ { ccc, 42 }, { "beta", 1.0 } ] ) returns a bullet
% list like:
%
%  + beta: 1.0
%  + ccc: 42
%
-spec proplist_to_string( list_table:list_table() ) -> ustring().
proplist_to_string( Proplist ) ->

	% In this context, key and value known to be strings or atoms:
	Strings = [ io_lib:format( "~ts: ~ts", [ K, V ] )
				|| { K, V } <- lists:sort( Proplist ) ],

	strings_to_string( Strings ).



% Returns a string describing the specified three-element version.
-spec version_to_string( basic_utils:version() ) -> string().
version_to_string( { V1, V2, V3 } ) ->
	io_lib:format( "~B.~B.~B", [ V1, V2, V3 ] ).



% Returns a binary string corresponding to the specified atom.
-spec atom_to_binary( atom() ) -> bin_string().
atom_to_binary( Atom ) ->
	string_to_binary( atom_to_string( Atom ) ).



% Returns a textual description of the specified percentage, expected to be a
% float in [0,1], with the default number of digits after the decimal point.
%
-spec percent_to_string( math_utils:percent() ) -> string().
percent_to_string( Value ) ->
	percent_to_string( Value, _DefaultPrecision=1 ).


% Returns a textual description of the specified percentage, expected to be a
% float in [0,1], with the specified number of digits after the decimal point.
%
-spec percent_to_string( math_utils:percent(), integer() ) -> string().
percent_to_string( Value, Precision ) ->
	% Awful format string to determine:
	io_lib:format( "~.*f%", [ Precision, Value * 100 ] ).



% Returns a textual description of the specified (dot-based, not comma-based)
% float.
%
-spec float_to_string( float() ) -> string().
float_to_string( Float ) ->
	erlang:float_to_list( Float ).


% Returns an exact rounded textual description of the specified distance,
% expected to be expressed as a floating-point number of millimeters, which will
% be first rounded to nearest integer.
%
% Ex: for a distance of 1001.5 millimeters, returns "1m and 2mm".
%
-spec distance_to_string( unit_utils:millimeters()
						 | unit_utils:int_millimeters() ) -> string().
distance_to_string( Millimeters ) when is_float( Millimeters ) ->
	distance_to_string( round( Millimeters ) );

% Returns an exact textual description of the specified distance, expected to be
% expressed as an integer number of millimeters.
%
% Ex: for an integer distance of 1000001 millimeters, returns "1km and 1mm".
%
distance_to_string( Millimeters ) ->

	Centimeters = 10,
	Meters = 100 * Centimeters,
	Km = Meters*Meters,

	ListWithKm = case Millimeters div Km of

		0 ->
			[];

		KmNonNull->
			[ io_lib:format( "~Bkm", [ KmNonNull ] ) ]

   end,

	DistAfterKm = Millimeters rem Km,
	%io:format( "DistAfterKm = ~B.~n", [ DistAfterKm ] ),

	ListWithMeters = case DistAfterKm div Meters of

		0 ->
			ListWithKm;

		MetersNonNull->
			[ io_lib:format( "~Bm", [ MetersNonNull ] ) | ListWithKm ]

	end,

	DistAfterMeters = DistAfterKm rem Meters,
	%io:format( "DistAfterMeters = ~B.~n", [ DistAfterMeters ] ),

	ListWithCentimeters = case DistAfterMeters div Centimeters of

		0 ->
			ListWithMeters;

		CentNonNull->
			[ io_lib:format( "~Bcm", [ CentNonNull ] ) | ListWithMeters ]

   end,

	DistAfterCentimeters = DistAfterMeters rem Centimeters,
	%io:format( "DistAfterCentimeters = ~B.~n", [ DistAfterCentimeters ] ),

	ListWithMillimeters = case DistAfterCentimeters of

		0 ->
			ListWithCentimeters;

		AtLeastOneMillimeter ->
			 [ io_lib:format( "~Bmm", [ AtLeastOneMillimeter ] )
			   | ListWithCentimeters ]

	end,

	%io:format( "Unit list is: ~w.~n", [ ListWithMillimeters ] ),

	% Preparing for final display:
	case ListWithMillimeters of

		[] ->
			"0mm";

		[ OneElement ] ->
			OneElement;

		[ Smaller | Bigger ] ->
			join( ", ", lists:reverse( Bigger ) ) ++ " and " ++ Smaller

	end.



% Returns an approximate textual description of the specified distance, expected
% to be expressed as a floating-point number of millimeters, which will be first
% rounded to nearest integer.
%
% Only one unit, the most appropriate one, will be used, with up to 1 figure
% after the comma.
%
% Ex: for a distance of 1000.5 millimeters, returns "1.0m".
%
-spec distance_to_short_string( unit_utils:millimeters()
							   | unit_utils:int_millimeters() ) -> string().
distance_to_short_string( Millimeters ) when is_float( Millimeters ) ->
	distance_to_short_string( round( Millimeters ) );

% Returns an approximate textual description of the specified distance, expected
% to be expressed as an integer number of millimeters.
%
% Only one unit, the most appropriate one, will be used, with up to 1 figure
% after the comma.
%
% Ex: for a distance of 1000001 millimeters, returns "1.0km".
%
distance_to_short_string( Millimeters ) ->

	% Note: very specific limit distances could be better managed.
	% Ex: 999999 millimeters is 999m, 99cm and 9mm, and "1000.0m" due to
	% rounding, whereas we would have preferred "1km".

	Centimeters = 10,
	Meters = 100 * Centimeters,
	Km = Meters * Meters,

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
							io_lib:format( "~Bmm", [ Millimeters ] );

						_CmNonNull ->
							io_lib:format( "~.1fcm",
										   [ Millimeters / Centimeters ] )

					end;

				 _MetersNonNull ->
					io_lib:format( "~.1fm", [ Millimeters / Meters ] )

			end;

		_KmNonNull->
			io_lib:format( "~.1fkm", [ Millimeters / Km ] )

	end.



% Returns an approximate textual description of the specified duration, expected
% to be expressed as a number of milliseconds (integer otherwise, if being
% floating-point, it will be rounded), or as the 'infinity' atom.
%
% Ex: for a duration of 150012 ms, returns:
% "2 minutes, 30 seconds and 12 milliseconds".
%
% See also: basic_utils:get_textual_duration/2.
%
-spec duration_to_string( unit_utils:milliseconds() | float() | 'infinity' ) ->
								string().
duration_to_string( Duration ) ->
	time_utils:duration_to_string( Duration ).



% Formats specified string as io_lib:format/2 would do, except it returns a
% flattened version of it and cannot fail (so that for example a badly formatted
% log cannot crash anymore its emitter process).
%
% Tries to never crash.
%
% Note: rely preferably on '~ts' rather than on '~s', to avoid unexpected
% Unicode inputs resulting on crashes afterwards.
%
-spec format( format_string(), format_values() ) -> ustring().

-ifdef(exec_target_is_production).

format( FormatString, Values ) ->

	String =
		try

			io_lib:format( FormatString, Values )

		catch

			_:_ ->

				% Useful to obtain the stacktrace of a culprit or to check for
				% silent errors:
				%
				%throw( { badly_formatted, FormatString, Values } )

				Msg = io_lib:format( "[error: badly formatted string output] "
						"Format string was '~p', values were '~p'.",
						[ FormatString, Values ] ),

				% Not wanting to be extra verbose in this mode:
				%io:format( Msg ++ "~n", [] ),

				Msg

		end,

	% Using 'flatten' allows for example to have clearer string outputs in case
	% of error (at a rather low cost):
	%
	lists:flatten( String ).


-else. % exec_target_is_production


format( FormatString, Values ) ->

	String =
		try

			io_lib:format( FormatString, Values )

		catch

			_:_ ->

				% Useful to obtain the stacktrace of a culprit or to check for
				% silent errors:
				%
				%throw( { badly_formatted, FormatString, Values } )

				Msg = io_lib:format( "[error: badly formatted string output] "
						"Format string was '~p', values were '~p'.",
						[ FormatString, Values ] ),

				% If wanting to be extra verbose:
				io:format( Msg ++ "~n", [] ),

				Msg

		end,

	% Using 'flatten' allows for example to have clearer string outputs in case
	% of error (at a rather low cost):
	%
	lists:flatten( String ).

-endif. % exec_target_is_production




% Formats specified binary string as io_lib:format/2 would do, except it returns
% a flattened version of it and cannot fail (so that for example a badly
% formatted log cannot crash anymore its emitter process).
%
% Note: rely preferably on '~ts' rather than on '~s', to avoid unexpected
% Unicode inputs resulting on crashes afterwards.
%
-spec bin_format( format_string(), [ term() ] ) -> ustring().


-ifdef(exec_target_is_production).


bin_format( FormatString, Values ) ->

	String =
		try

			io_lib:format( FormatString, Values )

		catch

		_:_ ->

				% Useful to obtain the stacktrace of a culprit or to check for
				% silent errors:
				%
				%throw( { badly_formatted, FormatString, Values } )

				Msg = io_lib:format( "[error: badly formatted string output] "
									 "Format: '~p', values: '~p'",
									 [ FormatString, Values ] ),

				% If wanting to be extra verbose:
				%io:format( Msg ++ "~n", [] ),

				Msg

	end,

	% No flattening needed here:
	erlang:list_to_binary( String ).


-else. % exec_target_is_production


bin_format( FormatString, Values ) ->

	String =
		try

			io_lib:format( FormatString, Values )

		catch

		_:_ ->

				% Useful to obtain the stacktrace of a culprit or to check for
				% silent errors:
				%
				%throw( { badly_formatted, FormatString, Values } )

				Msg = io_lib:format( "[error: badly formatted string output] "
									 "Format: '~p', values: '~p'",
									 [ FormatString, Values ] ),

				% If wanting to be extra verbose:
				io:format( Msg ++ "~n", [] ),

				Msg

	end,

	% No flattening needed here:
	erlang:list_to_binary( String ).

-endif. % exec_target_is_production



% Useful to catch silly mistakes involving an extra comma in a format string:
-spec format( term(), term(), term() ) -> no_return().
format( A, B, C ) ->
	throw( { faulty_format_call, { A, B, C } } ).



% Note: we deemed safer to consider for ensure_*/1 that atoms shall not be
% directly seen as possible inputs.



% Returns a string version of the specified text-like parameter.
%
% Note: using such functions may be a bad practice, as it may lead to losing the
% awareness of the types of the variables that are handled. We may even decide
% in the future to output warning traces whenever the specified element happens
% not to be a string.
%
-spec ensure_string( term() ) -> string().
ensure_string( String ) when is_list( String ) ->
	String;

ensure_string( BinString ) when is_binary( BinString ) ->
	binary_to_string( BinString );

ensure_string( Int ) when is_integer( Int ) ->
	integer_to_list( Int );

ensure_string( F ) when is_float( F ) ->
	float_to_list( F );

ensure_string( U ) ->
	throw( { invalid_value, U } ).



% Returns a binary string version of the specified text-like parameter.
%
% Note: using such functions may be a bad practice, as it may lead to losing the
% awareness of the types of the variables that are handled. We may even decide
% in the future to output warning traces whenever the specified element happens
% not to be a binary.
%
-spec ensure_binary( any_string() ) -> binary().
ensure_binary( BinString ) when is_binary( BinString ) ->
	BinString;

ensure_binary( String ) when is_list( String ) ->
	string_to_binary( String );

ensure_binary( String ) ->
	throw( { invalid_value, String } ).


% Returns the lexicographic distance between the two specified strings, i.e. the
% minimal number of single-character changes in order to transform one string
% into the other one.
%
% The strings are equal iff returns zero.
%
% Directly inspired from
% https://rosettacode.org/wiki/Levenshtein_distance#Erlang and, on
% https://en.wikibooks.org,
% wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Erlang.
%
% See also: https://en.wikipedia.org/wiki/Levenshtein_distance
%
%-spec get_lexicographic_distance_variant( string(), string() ) -> distance().

% This basic implementation is correct, yet way too inefficient:
%get_lexicographic_distance_variant( FirstString, _SecondString=[] ) ->
%	length( FirstString );

%get_lexicographic_distance_variant( _FirstString=[], SecondString ) ->
%	length( SecondString );

%get_lexicographic_distance_variant( _FirstString=[ H | T1 ],
%									_SecondString=[ H | T2 ] ) ->
%	get_lexicographic_distance_variant( T1, T2 );

%get_lexicographic_distance_variant( FirstString=[ _H1 | T1 ],
%									SecondString=[ _H2 | T2 ] ) ->
%	1 + lists:min( [ get_lexicographic_distance_variant( FirstString, T2 ),
%					 get_lexicographic_distance_variant( T1, SecondString ),
%					 get_lexicographic_distance_variant( T1, T2 ) ] ).


% Significantly more efficient version, using memoization:
-spec get_lexicographic_distance( string(), string() ) -> distance().
get_lexicographic_distance( FirstString, SecondString ) ->
	{ Distance, _NewAccTable } = get_lexicographic_distance( FirstString,
										 SecondString, _AccTable=?table:new() ),
	Distance.


% Actual helper:
get_lexicographic_distance( _FirstString=[], SecondString, AccTable ) ->
	Len = length( SecondString ),
	NewTable = ?table:add_entry( _K={ [], SecondString }, _V=Len, AccTable ),
	{ Len, NewTable };

get_lexicographic_distance( FirstString, _SecondString=[], AccTable ) ->
	Len = length( FirstString ),
	NewTable = ?table:add_entry( _K={ FirstString, [] }, _V=Len, AccTable ),
	{ Len, NewTable };

get_lexicographic_distance( _FirstString=[ H | T1 ], _SecondString=[ H | T2 ],
							AccTable ) ->
	get_lexicographic_distance( T1, T2 , AccTable );

get_lexicographic_distance( FirstString=[ _H1 | T1 ], SecondString=[ _H2 | T2 ],
							AccTable ) ->
	Key = { FirstString, SecondString },
	case ?table:lookup_entry( Key, AccTable ) of

		{ value, Distance } ->
			{ Distance, AccTable };

		key_not_found ->
			{ Len1, Table1 } = get_lexicographic_distance( FirstString, T2,
														   AccTable ),
			{ Len2, Table2 } = get_lexicographic_distance( T1, SecondString,
														   Table1 ),
			{ Len3, Table3 } = get_lexicographic_distance( T1, T2, Table2 ),
			Len = 1 + lists:min( [ Len1, Len2, Len3 ] ),
			{ Len, ?table:add_entry( Key, Len, Table3 ) }

	end.


% Returns the longest prefix that is common to all of the specified strings, and
% a list of the specified strings with this prefix removed, in the same order.
%
% See also: file_utils:get_longest_common_path/1.
%
-spec get_longest_common_prefix( [ string() ] ) -> { string(), [ string() ] }.
get_longest_common_prefix( _Strings=[] ) ->
	throw( empty_string_list );

get_longest_common_prefix( _Strings=[ S ] ) ->
	{ S, [ "" ] };

get_longest_common_prefix( _Strings=[ S | T ] ) ->
	% If having more than one string, take the first as the reference:
	get_prefix_helper( T, _RefString=S, _AccPrefix=[] ).


% (helper)
get_prefix_helper( Strings, _RefString=[], AccPrefix ) ->
	% Characters of the reference exhausted, it is the prefix as a whole:
	{ lists:reverse( AccPrefix ), [ "" | Strings ] };


get_prefix_helper( Strings, RefString=[ C | T ], AccPrefix ) ->

	case are_all_starting_with( C, Strings ) of

		{ true, NewStrings } ->
			get_prefix_helper( NewStrings, T, [ C | AccPrefix ] );

		false ->
			% Do not forget the reference one:
			{ lists:reverse( AccPrefix ), [ RefString | Strings ] }

	end.



% (helper)
are_all_starting_with( C, Strings ) ->
	are_all_starting_with( C, Strings, _Acc=[] ).


are_all_starting_with( _C, _Strings=[], Acc ) ->
	% String order does not matter:
	{ true, Acc };

% This string matches:
are_all_starting_with( C, _Strings=[ [ C | Rest ] | T ], Acc ) ->
	are_all_starting_with( C, T, [ Rest | Acc ] );

% Either _Strings=[ [] | T ] or _Strings=[ [ NonC | Rest ] | T ]:
are_all_starting_with( _C, _Strings, _Acc ) ->
	false.



% Converts a plain (list-based) string into a binary.
-spec string_to_binary( ustring() ) -> binary().
string_to_binary( String ) when is_list( String ) ->
	try

		erlang:list_to_binary( String )

	catch Class:Exception ->

		% Ex: might be triggered if String=[8364] ('euro' character), possibly
		% if being fed with Unicode string.
		%
		throw( { invalid_string, String, Class, Exception } )

	end;

string_to_binary( Other ) ->
	throw( { not_a_string, Other } ).



% Converts a binary into a plain (list-based) string.
-spec binary_to_string( binary() ) -> ustring().
binary_to_string( Binary ) when is_binary( Binary ) ->
	erlang:binary_to_list( Binary );

binary_to_string( Other ) ->
	throw( { not_a_binary_string, Other } ).





% Converts a list of plain (list-based) strings into a list of binaries.
%
% Order of items remains unaffected.
%
-spec strings_to_binaries( [ ustring() ] ) -> [ binary() ].
strings_to_binaries( StringList ) ->
	% Order must be preserved:
	[ string_to_binary( S ) || S <- StringList ].



% Converts a list of binaries into list of plain (list-based) strings.
%
% Order of items remains unaffected.
%
-spec binaries_to_strings( [ bin_string() ] ) -> [ ustring() ].
binaries_to_strings( BinaryList ) ->

	%trace_utils:debug_fmt( "binaries_to_strings: ~p", [ BinaryList ] ),

	% Order must be preserved:
	%[ erlang:binary_to_list( B ) || B <- BinaryList ].
	[ try

		  erlang:binary_to_list( B )

	  catch _:E ->

		  throw( { binary_conversion_failed, E, B } )

	  end || B <- BinaryList ].



% Returns an integer that corresponds to the specified text.
%
% Throws an exception if the conversion failed.
%
-spec string_to_integer( ustring() ) -> integer().
string_to_integer( String ) ->

	try list_to_integer( String ) of

		I ->
			I

	catch

		error:badarg ->
			throw( { integer_conversion_failed , String } )

	end.



% Returns an integer that corresponds to the specified text (expected to rely on
% our usual base 10).
%
% Returns the 'undefined' atom if the conversion failed.
%
-spec try_string_to_integer( ustring() ) -> basic_utils:maybe( integer() ).
try_string_to_integer( String ) ->
	try_string_to_integer( String, _Base=10 ).



% Returns an integer that corresponds to the specified text, expected to rely on
% the specified base.
%
% Returns the 'undefined' atom if the conversion failed.
%
-spec try_string_to_integer( ustring(), 2..36 ) ->
								   basic_utils:maybe( integer() ).
try_string_to_integer( String, Base ) ->
	try list_to_integer( String, Base ) of

		I ->
			I

	catch

		error:badarg ->
			undefined

	end.



% Returns a float that corresponds to the specified text, not depending on its
% being defined as an integer or as a float.
%
% Throws an exception if the conversion failed.
%
-spec string_to_float( ustring() ) -> float().
string_to_float( String ) ->

	case try_string_to_float( String ) of

		undefined ->
			throw( { float_conversion_failed, String } );

		F ->
			F

	end.



% Returns a float that corresponds to the specified text, not depending on its
% being defined as an integer or as a float.
%
% Returns the 'undefined' atom if the conversion failed.
%
-spec try_string_to_float( ustring() ) -> basic_utils:maybe( float() ).
try_string_to_float( String ) ->

	% Erlang is very picky (too much?) when interpreting floats-as-a-string: if
	% there is an exponent, it shall be 'e' (preferably that 'E' which is
	% nevertheless tolerated), and the mantissa must be a floating-point number
	% (hence with a point, such as 3.0e2, not 3e2) and at least one figure must
	% exist after the point (ex: 1.0e2 is accepted, 1.e2 not). Moreover the
	% decimal mark must be '.' (ex: not ',').

	% We overcome all these limitations here, so that for example -1,2E-4, 40E2
	% and 1,E3 are accepted and interpreted correctly.

	% Indeed, 'list_to_float("1e-4")' will raise badarg, whereas
	% 'list_to_float("1.0e-4")' will be accepted.
	%
	% So: if there is no dot on the left of a 'e' or a 'E', add ".0".
	% Moreover, "1.E-4" is also rejected, it must be fixed as well.

	% First, normalise the string, by transforming any 'E' into 'e', and by
	% converting any comma-based decimal mark into a dot:
	%
	LowerString = substitute( _SourceChar=$E, _TargetChar=$e, String ),

	DotString = substitute( $,, $., LowerString ),

	CandidateString = case split_at_first( $e, DotString ) of

		none_found ->
			% There was no exponent here:
			String;

		{ Left, Right } ->
			NewLeft = case split_at_first( $., Left ) of

				none_found ->
					Left ++ ".0";

				% Here there is a dot, yet there is no number afterward (ex:
				% 1.E2), we fix it (to have 1.0E2):
				%
				{ DotLeft, _DotRight="" } ->
					DotLeft ++ ".0";

				{ _DotLeft, _DotRight } ->
					% Already a dot, continue as is:
					Left

			end,
			NewLeft ++ "e" ++ Right

	end,

	try list_to_float( CandidateString ) of

		F ->
			F

	catch

		error:badarg ->

			try list_to_integer( String ) of

				I ->
					float( I )

			catch

				error:badarg ->
					undefined

			end

	end.




% Converts specified plain string into an atom.
%
% Note that only a bounded number of atoms should be created that way, lest the
% atom table gets saturated.
%
-spec string_to_atom( ustring() ) -> atom().
string_to_atom( String ) ->
	try

		erlang:list_to_atom( String )

	catch

		error:badarg ->
			throw( { not_string, String } )

	end.



% Returns a textual representation of the specified terms, as a list of their
% user-friendly (i.e. based on ~p) default representation.
%
-spec terms_to_string( [ term() ] ) -> string().
terms_to_string( Terms ) ->
	strings_to_string( [ format( "~p", [ T ] ) || T <- Terms ] ).



% Returns a textual representation of the specified terms, as an enumerated list
% of their user-friendly (i.e. based on ~p) default representation.
%
-spec terms_to_enumerated_string( [ term() ] ) -> string().
terms_to_enumerated_string( Terms ) ->
	strings_to_enumerated_string( [ format( "~p", [ T ] ) || T <- Terms ] ).



% Returns a textual representation of the specified terms, as a listed
% representation of their user-friendly (i.e. based on ~p) default
% representation.
%
-spec terms_to_listed_string( [ term() ] ) -> string().
terms_to_listed_string( Terms ) ->
	strings_to_listed_string( [ format( "~p", [ T ] ) || T <- Terms ] ).



% Converts specified list of plain strings into a corresponding list of atoms.
%
% Note that a bounded number of atoms should be created that way, lest the atom
% table gets saturated.
%
-spec binary_to_atom( binary() ) -> atom().
binary_to_atom( Binary ) ->
	String = binary_to_string( Binary ),
	string_to_atom( String ).



% Returns the specified string, ensuring that its first letter is a majuscule,
% uppercasing it if necessary.
%
-spec uppercase_initial_letter( ustring() ) -> ustring().
uppercase_initial_letter( _Letters=[] ) ->
	[];

uppercase_initial_letter( _Letters=[ First | Others ] ) ->
	[ string:to_upper( First ) | Others ].



% Sets the specified string to lowercase, i.e. downcase it (as a whole).
-spec to_lowercase( string() ) -> string().
to_lowercase( String ) ->
	string:to_lower( String ).


% Sets the specified string to uppercase.
-spec to_uppercase( string() ) -> string().
to_uppercase( String ) ->
	string:to_upper( String ).



% join( Separator, StringsToJoin ), to be used like in:
%   join( $-, [ "Barbara", "Ann" ] ) = "Barbara-Ann".
%
% Separator can be a character, like $a, or a string, like ", ".
%
% Python-like 'join', combines items in a list into a string using a separator
% between each item representation.
%
% Inspired from http://www.trapexit.org/String_join_with.
%
% For file-related paths, you are expected to use portable standard
% filename:join/{1,2} functions instead.
%
% Note: conversely, use split/2 to split the string.
%
-spec join( ustring() | uchar(), [ ustring() ] ) -> ustring().
join( _Separator, _ListToJoin=[] ) ->
	"";

join( Separator, ListToJoin ) ->
	%io:format( "ListToJoin = ~p~n", [ ListToJoin ] ),
	lists:flatten( lists:reverse( join( Separator, ListToJoin, _Acc=[] ) ) ).


% Helper:
join( _Separator, _ListToJoin=[], Acc) ->
	Acc;

join( _Separator, _ListToJoin=[ H | [] ], Acc ) ->
	[ H | Acc ];

join( Separator, _ListToJoin=[ H | T ], Acc ) ->
	join( Separator, T, [ Separator, H | Acc ] ).




% Splits the specified string into a list of strings, based on the list of
% specified characters to be interpreted as delimiters.
%
% Defined here not to chase anymore after string:tokens/2.
%
% See also: split_at_whitespaces/0.
%
-spec split( ustring(), [ uchar() ] ) -> [ ustring() ].
split( String, Delimiters ) ->

	% Note: string:tokens/2 is now deprecated in favor of string:lexemes/2, and
	% and anyway both treat two or more adjacent separator graphemes clusters as
	% only one, which is generally not what we want; so we now use:

	%string:lexemes( String, Delimiters ).
	string:split( String, Delimiters, _Where=all ).



% Splits the specified string into a list of strings, using whitespaces as
% delimiters.
%
-spec split_at_whitespaces( ustring() ) -> [ ustring() ].
split_at_whitespaces( String ) ->
	split( String, list_whitespaces() ).



% Splits the specified string according to the first occurrence of specified
% character: returns a pair of two strings, containing respectively all
% characters strictly before and strictly after the first occurrence of the
% marker (which thus is not kept).
%
% Ex: split_at_first( $x, "  aaaxbbbxccc" ) shall return { "  aaa", "bbbxccc" }.
%
-spec split_at_first( uchar(), ustring() ) ->
							'none_found' | { ustring(), ustring() }.
split_at_first( Marker, String ) ->
	split_at_first( Marker, String, _Acc=[] ).


% Helper:
split_at_first( _Marker, _ToRead=[], _Read ) ->
	none_found;

split_at_first( Marker, _ToRead=[ Marker | T ], Read ) ->
	{ lists:reverse( Read ), T };

split_at_first( Marker, _ToRead=[ Other | T ], Read ) ->
	split_at_first( Marker, T, [ Other | Read ] ).




% Splits the specified string, expected to be containing a word in CamelCase,
% into a list of strings, based on the internal words (delimited by uppercases,
% knowing a series of uppercase letters, except the last one, is considered as
% an acronym, hence as a single word), in their original order.
%
% Ex: split_camel_case( "IndustrialWasteSource" ) shall return [ "Industrial",
% "Waste", "Source" ], while split_camel_case( "TheySaidNYCWasGreat" ) shall
% return [ "They", "Said", "NYC", "Was", "Great" ].
%
-spec split_camel_case( ustring() ) -> [ ustring() ].
split_camel_case( String ) ->

	case is_uppercase( hd( String ) ) of

		true ->
			split_camel_case( String, [] );

		false ->
			throw( { not_camel_case_string, String } )

	end.


% (helper)
split_camel_case( _String=[], Acc ) ->
	lists:reverse( Acc );

split_camel_case( _String=[ HeadChar | MoreChars ], Acc ) ->

	case is_uppercase( HeadChar ) of

		true ->

			% is_uppercase rertuns 'true' if a char is unchanged by 'to_upper',
			% hence non-letter characters will be let in the second string:
			IsLowercase = fun( C ) ->
							  not is_uppercase( C )
						  end,

			{ TailOfWord, MoreWords } =
				lists:splitwith( IsLowercase, MoreChars ),

			NewWord = [ HeadChar | TailOfWord ],

			split_camel_case( MoreWords, [ NewWord | Acc ] );

		false ->

			% Discards the non-letter characters:
			split_camel_case( MoreChars, Acc )

	end.



% Splits the specified string into a list of strings, based on the list of
% separating characters provided in SeparatorsList, then turns these resulting
% strings in the Capitalized Case (all lower-case except for the first letter)
% and finally joins them to get a long CamelCased string.
%
% Ex: tokenizable_to_camel_case( "industrial_WASTE_sOuRCe", "_" ) shall return
% "IndustrialWasteSource", while tokenizable_to_camel_case( "ME HAZ READ J.R.R",
% ". " ) shall return "MeHazReadJRR".
%
-spec tokenizable_to_camel_case( ustring(), ustring() ) -> ustring().
tokenizable_to_camel_case( String, SeparatorsList ) ->

	% Separates the tokens:
	Tokens = string:tokens( String, SeparatorsList ),

	% Makes all the tokens lower-case if needed:
	LowerCaseTokens = [ string:to_lower( Str ) || Str <- Tokens ],

	% Capitalizes all lower-cased tokens:
	CamelCaseTokens = [ uppercase_initial_letter( Str )
						|| Str <- LowerCaseTokens ],

	% Concatenates the capitalized tokens:
	lists:concat( CamelCaseTokens ).




% Substitutes in specified string the source character with the target one (all
% occurrences thereof).
%
% Note: simpler and probably more efficient that a regular expression.
%
-spec substitute( uchar(), uchar(), ustring() | bin_string() ) -> ustring().
substitute( SourceChar, TargetChar, BinString ) when is_binary( BinString ) ->
	substitute( SourceChar, TargetChar, binary_to_string( BinString ) );

substitute( SourceChar, TargetChar, String ) ->
	substitute( SourceChar, TargetChar, String, _Acc=[] ).


substitute( _SourceChar, _TargetChar, _String=[], Acc ) ->
	lists:reverse( Acc );

substitute( SourceChar, TargetChar, _String=[ SourceChar | T ], Acc ) ->
	substitute( SourceChar, TargetChar, T, [ TargetChar | Acc ] );

substitute( SourceChar, TargetChar, _String=[ OtherChar | T ], Acc ) ->
	substitute( SourceChar, TargetChar, T, [ OtherChar | Acc ] ).



% Filters out in specified string the specified character, so that it does not
% occur anymore on the returned string.
%
% Note: simpler and probably more efficient that a regular expression.
%
-spec filter( uchar(), ustring() ) -> ustring().
filter( CharToRemove, String ) ->
	filter( CharToRemove, String, _Acc=[] ).


filter( _CharToRemove, _String=[], Acc ) ->
	lists:reverse( Acc );

filter( CharToRemove, _String=[ CharToRemove | T ], Acc ) ->
	% Just drop that character:
	filter( CharToRemove, T, Acc );

filter( CharToRemove, _String=[ OtherChar | T ], Acc ) ->
	filter( CharToRemove, T, [ OtherChar | Acc ] ).



% Splits the specified string after specified prefix and returns the remaining
% part, otherwise returns that the prefix was not found.
%
% Ex: split_after_prefix( "Foo", "Foobar is baz." ) returns "bar is baz.";
% split_after_prefix( "ABC", "Foobar is baz." ) return 'no_prefix'.
%
-spec split_after_prefix( string(), string() ) -> string() | 'no_prefix'.
split_after_prefix( _Prefix=[], String ) ->
	String;

split_after_prefix( _Prefix=[ C | T ], _String=[ C | StringT ] ) ->
	split_after_prefix( T, StringT );

split_after_prefix( _Prefix, _String ) ->
	no_prefix.



% Updates specified file with specified keywords, i.e. copies the original file
% into a target, updated one (supposedly non-already existing) in which all the
% specified keywords (the keys of the translation table) are replaced with their
% associated value (the corresponding value in table).
%
% Ex: file_utils:update_with_keywords( "original.txt", "updated.txt", table:new(
%  [ { "hello", "goodbye" }, { "Blue", "Red" } ] ).
%
% See also: file_utils:update_with_keywords/3.
%
-spec update_with_keywords( any_string(), translation_table() ) ->
								  string_list().
update_with_keywords( Content, TranslationTable ) ->

	TransPairs = ?table:enumerate( TranslationTable ),

	% As many passes as keyword pairs:
	lists:foldl(

		fun( { SearchP, Replacement }, Acc ) ->
			string:replace( Acc, SearchP, Replacement, _Where=all )
		end,
		_Acc0=Content,
		_List=TransPairs ).



% Returns a list of all known whitespaces.
-spec list_whitespaces() -> [ char() ].
list_whitespaces() ->
	" \t\n".



% Returns specified text, in which single quotes have been escaped (i.e. "'" has
% been replaced with "\'" - ignore the double quotes in this example).
%
-spec escape_single_quotes( string() ) -> string().
escape_single_quotes( Text ) ->
	escape_single_quotes_helper( Text, _Acc=[] ).


escape_single_quotes_helper( _Text=[], Acc ) ->
	lists:reverse( Acc );

escape_single_quotes_helper( _Text=[ $' | T ], Acc ) ->
	% As will be reversed:
	escape_single_quotes_helper( T, "'\\" ++ Acc );

escape_single_quotes_helper( _Text=[ C | T ], Acc ) ->
	escape_single_quotes_helper( T, [ C | Acc ] ).



% Returns specified text, in which double quotes have been escaped (i.e. '"' has
% been replaced with '\"' - ignore the single quotes in this example).
%
-spec escape_double_quotes( string() ) -> string().
escape_double_quotes( Text ) ->
	escape_double_quotes_helper( Text, _Acc=[] ).


escape_double_quotes_helper( _Text=[], Acc ) ->
	lists:reverse( Acc );

escape_double_quotes_helper( _Text=[ $" | T ], Acc ) ->
	% As will be reversed:
	escape_double_quotes_helper( T, "\"\\" ++ Acc );

escape_double_quotes_helper( _Text=[ C | T ], Acc ) ->
	escape_double_quotes_helper( T, [ C | Acc ] ).



% Returns specified text, in which all quotes have been escaped (i.e. ' and "
% have been replaced respectively with \' and \").
%
-spec escape_all_quotes( string() ) -> string().
escape_all_quotes( Text ) ->
	escape_all_quotes_helper( Text, _Acc=[] ).


escape_all_quotes_helper( _Text=[], Acc ) ->
	lists:reverse( Acc );

escape_all_quotes_helper( _Text=[ $' | T ], Acc ) ->
	% As will be reversed:
	escape_all_quotes_helper( T, "'\\" ++ Acc );

escape_all_quotes_helper( _Text=[ $" | T ], Acc ) ->
	% As will be reversed:
	escape_all_quotes_helper( T, "\"\\" ++ Acc );

escape_all_quotes_helper( _Text=[ C | T ], Acc ) ->
	escape_all_quotes_helper( T, [ C | Acc ] ).



% Tells whether specified character is an uppercase one.
-spec is_uppercase( uchar() ) -> boolean().
is_uppercase( Char ) ->

	% Simplistic but working:

	OneCharacterString = [ Char ],

	case string:to_upper( OneCharacterString ) of

		OneCharacterString ->
			true;

		_ ->
			false

	end.



% Tells whether specified character is a figure (in 0..9).
-spec is_figure( char() ) -> boolean().
is_figure( Char ) when is_integer( Char ) andalso Char >= $0
					   andalso Char =< $9 ->
	true;

is_figure( Char ) when is_integer( Char ) ->
	false.



% Removes the ending "\n" character(s) of specified string.
-spec remove_ending_carriage_return( ustring() ) -> ustring().
remove_ending_carriage_return( String ) when is_list( String ) ->

	% See also: list_utils:remove_last_element/1.

	% 'Res ++ "\n" = String,Res' will not work:
	string:strip( String, right, $\n ).



% Removes the last Count characters from specified string, and returns the
% result.
%
-spec remove_last_characters( ustring(), basic_utils:count() ) -> ustring().
remove_last_characters( String, Count ) ->

	% Not necessarily the most efficient, but at least it is not an illegal
	% pattern:
	%
	case length( String ) of

		C when C >= Count ->
			string:substr( String, 1, C - Count );

		_->
			throw( { removal_failed, String, Count } )

	end.


% Removes all whitespaces from specified string, and returns the result.
-spec remove_whitespaces( ustring() ) -> ustring().
remove_whitespaces( String ) ->
	re:replace( String, "\s", "", [ global, unicode, { return, list } ] ).


% Removes all leading and trailing whitespaces from specified string, and
% returns the result.
%
-spec trim_whitespaces( ustring() ) -> ustring().
trim_whitespaces( String ) ->

	% Should be done in one pass:
	trim_leading_whitespaces( trim_trailing_whitespaces( String ) ).



% Removes all leading whitespaces from specified string, and returns the result.
-spec trim_leading_whitespaces( ustring() ) -> ustring().
trim_leading_whitespaces( String ) ->

	% Largely inspired from http://www.trapexit.org/Trimming_Blanks_from_String:
	re:replace( String, "^\\s*", "", [ unicode, { return, list } ] ).



% Removes all trailing whitespaces from specified string, and returns the
% result.
%
-spec trim_trailing_whitespaces( ustring() ) -> ustring().
trim_trailing_whitespaces( String ) ->

	% The $ confuses some syntax highlighting systems (like the one of some
	% emacs):
	%
	re:replace( String, "\\s*$", "", [ unicode, { return, list } ] ).





% Formats specified text according to specified width, expressed in characters.
%
% Returns a list of strings, each of which having Width characters.
%
-spec format_text_for_width( ustring(), pos_integer() ) -> [ ustring() ].
format_text_for_width( Text, Width ) ->

	% Whitespaces converted to spaces:
	CleanedTest = re:replace( lists:flatten( Text ), "\\s+", " ",
							  [ global, { return, list } ] ),

	WordList = string:tokens( CleanedTest, " " ),

	%io:format( "Formatting ~p.~n", [ WordList ] ),
	join_words( WordList, Width ).



% Joins words from the list, line by line.
join_words( WordList, Width ) ->
	join_words( WordList, Width, _Lines=[], _CurrentLine="",
				_CurrentLineLen=0 ).


join_words( [], _Width, AccLines, _CurrentLine, _CurrentLineLen=0 ) ->
	% Ended with a full line:
	lists:reverse( AccLines );

join_words( [], Width, AccLines, CurrentLine, _CurrentLineLen ) ->
	% Ended with a partial line:
	lists:reverse( [ pad_string( CurrentLine, Width ) | AccLines ] );

join_words( [ Word | RemainingWords ], Width, AccLines, CurrentLine,
			CurrentLineLen ) ->

	%io:format( "Managing word '~s' (len=~B), current line is '~s' (len=~B), "
	%	"width = ~B.~n", [ Word, length( Word ), CurrentLine, CurrentLineLen,
	% Width ] ),

	% Length should be incremented, as a space must be inserted before that
	% word, however we want to accept words whose width would be exactly equal
	% to the line width:
	%
	ActualLength = case CurrentLine of

		"" ->
			length( Word );

		_NonEmpty ->
			% Already at least a letter, we therefore must add a space before
			% the new word:
			length( Word ) + 1

	end,

	case ActualLength of

		CompatibleWidth when CompatibleWidth =< Width ->
			% Word width is manageable.
			% Will this word fit on the current line?
			%
			case CurrentLineLen + CompatibleWidth of

				FittingLen when FittingLen =< Width ->
					% Yes, this word fits on the current line.
					% Avoids adding a space at the beginning of a new line:
					{ NewCurrentLine, NewLineLen } = case CurrentLineLen of

						0 ->
							{ Word, CompatibleWidth };

						Len ->
							{ CurrentLine ++ " " ++ Word,
							  Len + CompatibleWidth + 1 }

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
								[ PaddedCurrentLine | AccLines ], Word,
								CompatibleWidth )

			end;


		_TooLargeWidth ->

			% Will break words as many times as needed:
			%io:format( "Word '~s' is too large (len=~B), breaking it.~n",
			%	[ Word, length( Word ) ] ),
			Subwords = break_word( Word, Width ),

			PaddedCurrentLine = pad_string( CurrentLine, Width ),

			join_words( Subwords ++ RemainingWords, Width,
						[ PaddedCurrentLine | AccLines ], "", 0 )

	end.



% Returns the specified string, padded with spaces to specified width,
% left-justified (i.e. with spaces added to the right).
%
-spec pad_string( ustring(), integer() ) -> ustring().
pad_string( String, Width ) when length( String ) =< Width ->
	pad_string_left( String, Width ).



% Returns the specified string, padded with spaces to specified width,
% left-justified (i.e. with spaces added to the right).
%
-spec pad_string_left( ustring(), integer() ) -> ustring().
pad_string_left( String, Width ) when length( String ) =< Width ->

	% Note that the settings listed in
	% http://erlang.org/doc/apps/stdlib/unicode_usage.html shall be enforced so
	% that character encoding is properly supported (with Unicode), otherwise
	% characters such as "e" with an accent are considered as two characters
	% instead of one, leading to incorrect (insufficient) padding:
	%
	lists:flatten( io_lib:format( "~*.ts", [ -Width, String ] ) ).


% Returns the specified string, padded with spaces to specified width,
% right-justified (i.e. with spaces added to the left).
%
-spec pad_string_right( ustring(), integer() ) -> ustring().
pad_string_right( String, Width ) when length( String ) =< Width ->
	lists:flatten( io_lib:format( "~*.ts", [ Width, String ] ) ).


% Returns true iff the parameter is a (non-nested) string (actually a plain list
% of integers).
%
% Taken from http://lethain.com
% (see distinguishing-strings-from-lists-in-erlang)
%
% Note: something like [ $e, 1, 2, $r ] is deemed to be a string.
%
-spec is_string( term() ) -> boolean().
is_string( [] ) ->
	true;

is_string( [ H | _ ] ) when not is_integer( H ) ->
	false;

is_string( [ _ | T ] ) ->
	is_string( T );

is_string( _Other ) ->
	false.

% Alternate, less efficient version:
%is_string( Term ) when is_list( Term ) ->
%			lists:all( fun erlang:is_integer/1, Term );
%
%is_string( _Term ) -> false.


% Returns true iif the parameter is a (non-nested) non-empty string (actually a
% plain list of at least one integer).
%
-spec is_non_empty_string( term() ) -> boolean().
is_non_empty_string( [] ) ->
	% Shall be not empty:
	false;

is_non_empty_string( [ H ] ) when is_integer( H ) ->
	true;

is_non_empty_string( [ H | T ] ) when is_integer( H ) ->
	is_non_empty_string( T );

is_non_empty_string( _Other ) ->
	false.



% Returns true iff the specified parameter is a list whose all elements are
% strings.
%
% Note: especially useful knowing that a string is itself a list, hence a string
% can easily be mistaken for a list of strings, in which case each of these
% strings would actually be found being an integer instead (corresponding to
% each of the characters of the overall string).
%
-spec is_list_of_strings( list() ) -> boolean().
is_list_of_strings( [] ) ->
	true;

is_list_of_strings( [ H | T ] ) ->

	case is_string( H ) of

		true ->
			is_list_of_strings( T );

		false ->
			false

	end;

is_list_of_strings( _Other ) ->
	false.




% Returns true iff the specified parameter is a binary string.
-spec is_bin_string( term() ) -> boolean().
is_bin_string( Term ) when is_binary( Term ) ->
	%is_string( binary_to_list( Term ) );
	true;

is_bin_string( _Term ) ->
	false.



% Tells whether specified term is a list of binaries.
-spec is_list_of_binaries( term() ) -> boolean().
is_list_of_binaries( List ) when is_list( List ) ->
	lists:all( fun is_bin_string/1, List );

is_list_of_binaries( _NotList ) ->
	false.




% Returns a list of words obtained from the breaking of specified word,
% according to specified maximum width.
%
% Parts of that word will use a separating dash.
%
% Ex: break_word( "simulator", 5 ) returns [ "simu-", "lator" ].
%
break_word( Word, Width ) ->

	% We do not want to have underscores in the word, as if the word happens
	% to be broken just after an underscore, RST will interpret it as a link.
	% Therefore we escape underscores:
	%
	% Used to cut into halves, then preferring truncating a first full-length
	% chunk, finally directly cutting the word into appropriate pieces:
	% CutIndex = length(Word) div 2,
	% CutIndex = Width-1,
	cut_into_chunks( Word, Width, _Acc=[] ).



% Cuts specified string into pieces, each of them having to fit in specified
% width.
%
cut_into_chunks( _String=[], _ChunkSize, Acc ) ->
	%io:format( "cut_into_chunks return ~p.", [ lists:reverse( Acc ) ] ),
	lists:reverse( Acc );

% Last word may take the full width (no dash to add):
cut_into_chunks( String, ChunkSize, Acc ) when length( String ) =< ChunkSize ->
	cut_into_chunks( [], ChunkSize, [ String | Acc ] );

% Here we have to cut the string anyway:
cut_into_chunks( String, ChunkSize, Acc ) ->

	% Rule is to add (and convert) characters until the end of line:
	% (ChunkSize decremented as "-" will be added)

	{ FirstPart, Remaining } = aggregate_word( String, ChunkSize-1, [] ),

	% Each underscore will result into another character (\) being added:
	%io:format( "FirstPart = '~s' (~B), Remaining = '~s'.~n",
	%	[ FirstPart, length( FirstPart ), Remaining ] ),
	cut_into_chunks( Remaining, ChunkSize, [ FirstPart ++ "-" | Acc ] ).



aggregate_word( String, 0, Acc ) ->
	{ lists:reverse( Acc ), String };


% An underscore once escaped would not fit, as it would result into two
% characters ('\_'):
%
aggregate_word( String=[ $_ | _T ], 1, Acc ) ->
	aggregate_word( String, 0, Acc );

% An escaped underscore will fit:
aggregate_word( [ $_ | T ], Count, Acc ) ->
	% Adding '_\' as it will reversed (into the expected '\_'):
	aggregate_word( T, Count-2, [ $\_, $\\ | Acc ] );

aggregate_word( [ H | T ], Count, Acc ) ->
	aggregate_word( T, Count-1, [ H | Acc ] ).




% Restructured-Text (RST) related functions.


% Generates a RST-compatible standard title, with the proper ASCII art.
% Follows our general conventions regarding title level, from H1 to Hn.
%
-spec generate_title( ustring(), 1..9 ) -> ustring().
generate_title( Title, Level ) ->

	{ Char, Layout } = get_title_rendering_for( Level ),

	TitleLine = get_line_of( Char, length( Title ) ) ++ "\n",

	case Layout of

		only_below ->
			Title ++ "\n" ++ TitleLine ++ "\n";

		below_and_on_top ->
			TitleLine ++ Title ++ "\n" ++ TitleLine ++ "\n"

	end.



% Returns how a title with specified level can be rendered.
% See demo-for-css-testing.rst for the convention.
%
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




% Returns a line made of Length characters "Character".
% Ex: get_line_of( $+, 5 ) = "+++++".
%
get_line_of( Character, Length ) ->
	lists:flatten( [ Character || _X <- lists:seq( 1, Length ) ] ).



% HTTP-related operations.


% About encoding.

% The character "è" (e with a grave accent, hex code: xE8) might be for example
% either translated as "%C3%A8" or as "%E8". It is apparently the difference
% between encodeURI(chr) and escape(chr) in Javascript.
%
% The most adequate encoding in general seems the first (in which case
% encode_as_url/1 and encode_element_as_url/1 shall be used), however some
% webservers seem to insist on having the second (in which case
% escape/1 and escape_element/1 shall be used).
%
% See also: http://www.javascripter.net/faq/accentedcharacters.htm



% Encodes specified list of {Key,Value} pairs so that it can used into an URL.
%
% Full example:
%
% inets:start(),
% httpc:request( post, { "http://localhost:3000/foo", [],
%  "application/x-www-form-urlencoded",
%  encode_as_url( [ {"username", "bob"}, {"password", "123456"} ] ) }, [], [] ).
%
% Directly inspired from:
% http://stackoverflow.com/questions/114196/url-encode-in-erlang
%
-spec encode_as_url( option_list:option_list() ) -> ustring().
encode_as_url( OptionList ) ->
   encode_as_url( OptionList, _Acc=[] ).

encode_as_url( _OptionList=[], Acc ) ->
	Acc;

% First entry:
encode_as_url( [ { Key, Value } | T ], _Acc=[] ) ->
	encode_as_url( T, encode_element_as_url( Key ) ++ "="
				   ++ encode_element_as_url( Value ) );

encode_as_url( [ { Key, Value } | T ], Acc ) ->
	encode_as_url( T, Acc ++ "&" ++ encode_element_as_url( Key ) ++ "="
				   ++ encode_element_as_url( Value ) ).


% Encodes specified element so that it can be used in an URL.
-spec encode_element_as_url( ustring() ) -> ustring().
encode_element_as_url( E ) ->
	% They seem to produce quite similar results in our few test cases:
	edoc_lib:escape_uri( E ).
	%encode_uri_rfc3986:encode( E ).



% Escapes specified list of {Key,Value} pairs so that it can used into some URL.
-spec escape( option_list:option_list() ) -> ustring().
escape( OptionList ) ->
	%io:format( "~n~nEscaping '~p'.~n", [ OptionList ] ),
	escape( OptionList, _Acc=[] ).

escape( _OptionList=[], Acc ) ->
	Acc;

% First entry:
escape( [ { Key, Value } | T ], _Acc=[] ) ->
	escape( T, escape_key( Key ) ++ "=" ++ escape_value( Value ) );

escape( [ { Key, Value } | T ], Acc ) ->
	escape( T, Acc ++ "&" ++ escape_key( Key ) ++ "="
			++ escape_value( Value ) ).



% Escapes specified element so that it can be used in some URL.
-spec escape_key( option_list:key() ) -> ustring().
escape_key( Key ) when is_atom( Key ) ->
	atom_to_string( Key ).


-spec escape_value( ustring() ) -> ustring().
escape_value( String ) ->
	R = lists:flatten( [ escape_char( C ) || C <- String ] ),
	%io:format( "'~s' became '~s'.~n", [ String, R ] ),
	R.



% Escapes specified character.
%
% Alphanumerical characters left as are:
escape_char( C ) when C >= 48 andalso C =< 57 ->
	% 0..9 kept as is:
	C;

escape_char( C ) when C >= 65 andalso C =< 90 ->
	% A..Z kept as is:
	C;

escape_char( C ) when C >= 97 andalso C =< 122 ->
	% a..z kept as is:
	C;

escape_char( C ) ->
	% Everything else is blindly encoded:
	io_lib:format( "%~s", [ integer_to_list( C, _HexBase=16 ) ] ).





% Miscellaneous functions.


% Tries to return a string adequate to form a simple name (mostly alphanumerical
% with underscores) from specified term.
%
% See also: file_utils:convert_to_filename/1.
%
-spec generate_text_name_from( term() ) -> ustring().
generate_text_name_from( Term ) ->
	String = term_to_string( Term ),
	fix_characters( String ).




% Non-exported helper functions.

fix_characters( String ) ->
	lists:reverse( fix_characters( lists:flatten( String ), [] ) ).


fix_characters( [], Acc ) ->
	Acc;

% 32 corresponds to space ('$ '):
fix_characters( [ 32 | T ], Acc ) ->
	fix_characters( T, [ "_" | Acc ] );

fix_characters( [ $' | T ], Acc ) ->
	fix_characters( T, [ "_" | Acc ] );

fix_characters( [ H | T ], Acc ) ->
	fix_characters( T, [ H | Acc ] ).
