% Copyright (C) 2007-2025 Olivier Boudeville
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
% Creation date: July, 2007.

-module(text_utils).

-moduledoc """
Gathering of various facilities to manage **textual content**.

See text_utils_test.erl for the corresponding test.
""".



% Note: this a bootstrap module, so its build is only to be triggered from the
% root of Myriad, and it should not depend at runtime on non-bootstrapped
% modules.



% String management functions.


% Conversions between terms and strings (both ways).
-export([ term_to_string/1, term_to_string/2, term_to_string/3,
		  term_to_bounded_string/1, term_to_bounded_string/2,
		  term_to_binary/1,

		  integer_to_string/1, integer_to_binary/1,

		  integer_to_hexastring/1, integer_to_hexastring/2,
		  integer_to_hexabinstring/1, integer_to_hexabinstring/2,


		  hexastring_to_integer/1, hexastring_to_integer/2,

		  hexabinstring_to_binary/1, hexastring_to_binary/1,
		  binary_to_hexastring/1, binary_to_hexastring/2,

		  integer_to_bits/1, integer_to_bits/2,

		  atom_to_string/1,

		  pid_to_string/1, pids_to_string/1,
		  pid_to_short_string/1, pids_to_short_string/1,
		  pid_to_core_string/1, pid_to_filename/1,

		  record_to_string/1,

		  strings_to_string/1, strings_to_string/2,
		  strings_to_spaced_string/1, strings_to_spaced_string/2,
		  strings_to_sorted_string/1, strings_to_sorted_string/2,

		  strings_to_enumerated_string/1, strings_to_enumerated_string/2,
		  strings_to_enumerated_string/3,

		  strings_to_enumerated_comment/1, strings_to_enumerated_comment/2,

		  strings_to_listed_string/1, strings_to_listed_string/2,
		  maybe_strings_to_listed_string/1,

		  binaries_to_string/1, binaries_to_string/2,
		  binaries_to_sorted_string/1, binaries_to_listed_string/1,
		  binaries_to_binary/1, binaries_to_binary/2,

		  buffer_to_string/1, buffer_to_binstring/1,

		  atoms_to_string/1, atoms_to_sorted_string/1, atoms_to_listed_string/1,
		  atoms_to_quoted_listed_string/1,
		  integers_to_listed_string/1, integer_ids_to_listed_string/1,
		  proplist_to_string/1, version_to_string/1,
		  atom_to_binary/1,

		  string_to_binary/1, string_to_binary/2, maybe_string_to_binary/1,
		  binary_to_string/1, binary_to_string/2,
		  strings_to_binaries/1, binaries_to_strings/1,
		  string_to_integer/1, try_string_to_integer/1, try_string_to_integer/2,
		  string_to_float/1, try_string_to_float/1,
		  string_to_atom/1, strings_to_atoms/1,
		  terms_to_string/1, terms_to_string/2,
		  terms_to_enumerated_string/1, terms_to_listed_string/1,
		  binary_to_atom/1, binary_to_integer/1, binary_to_float/1,
		  float_to_string/1, float_to_string/2, number_to_string/1,
		  percent_to_string/1, percent_to_string/2,
		  distance_to_string/1, distance_to_short_string/1,
		  repetition_to_string/1, table_to_string/2,

		  string_like_to_atom/1, string_like_to_bin_string/1,

		  format/2, format_failsafe/1, bin_format/2, atom_format/2, format/3,
		  format_ellipsed/2, format_ellipsed/3,
		  format_as_comment/1, format_as_comment/2, format_as_comment/3,
		  format_as_comment/4,
		  scan_format_string/1, interpret_faulty_format/2,

		  ensure_string/1, ensure_string/2,
		  ensure_strings/1, ensure_strings/2,

		  ensure_binary/1, ensure_binary/2, ensure_maybe_binary/1,
		  ensure_binaries/1, ensure_binaries/2 ]).



% Other string operations:
-export([ get_lexicographic_distance/2, get_longest_common_prefix/1,
		  get_unique_string/2,
		  safe_length/1, length/1,
		  uppercase_initial_letter/1, to_lowercase/1, to_uppercase/1,
		  flatten/1, io_to_binary/1,
		  join/2, bin_join/2,

		  split/2,
		  split_lines/1, unsplit_lines/1, bin_unsplit_lines/1,
		  split_per_element/2, split_parsed/2,
		  split_at_whitespaces/1,
		  split_at_first/2, split_camel_case/1, split_every/2,

		  tokenizable_to_camel_case/2,
		  duplicate/2,
		  concatenate/1, concatenate/2,
		  bin_concatenate/1, bin_concatenate/2,
		  remove_empty_lines/1,

		  find_substring_index/2, find_substring_index/3,

		  substitute/3, filter/2, split_after_prefix/2, split_before_suffix/2,
		  update_with_keywords/2,

		  list_whitespaces/0,

		  single_quote_string/1, double_quote_string/1,
		  single_quote_strings/1, double_quote_strings/1,
		  escape_single_quotes/1, escape_double_quotes/1,
		  escape_all_quotes/1, escape_with/3,
		  remove_newlines/1,

		  parse_quoted/1, parse_quoted/3,

		  is_uppercase/1, is_figure/1,
		  remove_ending_carriage_return/1, remove_last_characters/2,
		  remove_whitespaces/1,

		  trim_whitespaces/1, trim_leading_whitespaces/1,
		  trim_trailing_whitespaces/1,

		  ellipse/1, ellipse/2, ellipse_fmt/2, ellipse_fmt/3,
		  tail/1, tail/2,

		  get_default_bullet/0, get_bullet_for_level/1,
		  format_text_for_width/2, format_text_for_width/3,

		  pad_string/2,
		  pad_string_left/2, pad_string_left/3,
		  pad_string_right/2, pad_string_right/3,
		  center_string/2, center_string/3,

		  is_char/1,
		  is_string/1, is_bin_string/1, is_any_string/1, is_non_empty_string/1,
		  is_string_like/1,
		  are_strings/1, are_binaries/1, are_of_same_string_type/2,
		  try_convert_to_unicode_list/1, to_unicode_list/1, to_unicode_list/2,
		  try_convert_to_unicode_binary/1, to_unicode_binary/1,
		  to_unicode_binary/2 ]).


% Restructured-Text (RST) related functions.
-export([ generate_title/2 ]).


% To report properly (i.e. with a location) at runtime type errors:
-export([ report_not_a_string/1, report_not_a_binary_string/1,
		  report_not_a_list/1, report_not_a_number/1 ]).


% Miscellaneous functions.
-export([ generate_text_name_from/1, match_types/3 ]).


% This module being a bootstrap one, the 'table' pseudo-module is not available
% (as this module is by design not processed by the 'Myriad' parse transform):
%
-define( table, map_hashtable ).


% Prefix for text corresponding to hexadecimal values:
-define( hexa_prefix, "0x" ).



% Type section.


-doc """
These strings are supposed to contain Erlang-fashioned format characters, like
in "hello ~p!".
""".
-type format_string() :: ustring().



-doc """
These strings are supposed to contain Erlang-fashioned format characters, like
in `<<"hello ~p!">>`.
""".
-type format_bin_string() :: bin_string().



-doc """
In a format string (e.g. `"~n"`).
""".
-type control_sequence() :: ustring().



-doc """
Lists of terms corresponding to values to be referenced from a format string.
""".
-type format_values() :: [ term() ].



-doc "A level of verbosity (typically for the `to_string/2` functions).".
-type verbosity_level() :: 'low' | 'high'.



-doc """
These strings are supposed to contain Regular Expressions, like in:
`"*-emitter-(first|second)-*"`.

Patterns shall be expressed according to the "Perl Compatible Regular
Expressions" conventions, or PCRE for short.

For more information, see
[https://en.wikipedia.org/wiki/Perl_Compatible_Regular_Expressions].

See also [http://erlang.org/doc/man/re.html].
""".
-type regex_string() :: ustring().



-doc "A string that describes a title.".
-type title() :: ustring().



-doc "A binary string that describes a title.".
-type bin_title() :: bin_string().



-doc "Any string that describes a title.".
-type any_title() :: title() | bin_title().



-doc "A string that describes a label.".
-type label() :: ustring().



-doc "A binary string that describes a label.".
-type bin_label() :: ustring().



-doc "Any string that describes a label.".
-type any_label() :: label() | bin_label().



-doc "A binary corresponding to a string.".
-type bin_string() :: binary().



-doc """
Any kind of string (a.k.a `chardata() :: charlist() | unicode_string()`).
""".
-type any_string() :: ustring() | bin_string().



-doc """
A string containing hexadecimal values (possibly with a `"0x"` prefix).

We prefer hexadecimal (letter) characters to be in lower case.

For example: `"0x44e390a3"` or `"44e390a3"`.
""".
-type hexastring() :: ustring().



-doc """
A binary string containing hexadecimal values (possibly with a "0x" prefix).  We
prefer hexadecimal (letter) characters to be in lower case.

For example: `<<"0x44e390a3">>` or `<<"44e390a3">>`.
""".
-type hexabinstring() :: bin_string().



-doc """
A Unicode (plain) string.

This is our new default, and corresponds to `charlist() | unicode_binary()`, so
basically any non-nested string.

We mostly mean by that [char()], where char() is an integer corresponding to an
ASCII character or to a unicode codepoint, expected to be in `[0..16#10ffff]`.

See also [https://www.erlang.org/doc/man/unicode.html#type-charlist].
""".
-type unicode_string() :: chardata().



-doc "Any Unicode data.".
-type unicode_data() :: unicode:latin1_chardata()
					  | chardata() | unicode:external_chardata().



-doc """
A Unicode codepoint for a character.

(unfortunately we cannot define a `text_utils:char/0` type, as `"type char() is
a builtin type; it cannot be redefined"`).
""".
-type uchar() :: integer().



-doc """
Index in a Unicode string, in terms of grapheme clusters (e.g. not codepoints,
not bytes).
""".
-type gc_index() :: non_neg_integer().



-doc "A direction, typically in a string.".
-type direction() :: 'leading' | 'trailing'.



-doc "A plain (Unicode) string.".
-type plain_string() :: [ uchar() ].



-doc """
Now is our default type of (plain) string.

(unfortunately we cannot define a `text_utils:string/0` type, as `"type string()
is a builtin type; it cannot be redefined"`).
""".
-type ustring() :: unicode_string().


-doc """
A nested string, that is a possibly deep list containing only `char()` elements.
""".
-type chars() :: io_lib:chars(). % Thus [char() | chars()].



-doc """
Any kind of terms that can be directly mapped to a string (typically accepted by
`~ts` in format strings).
""".
-type string_like() :: ustring() | unicode_string() | bin_string() | atom().



-doc "The specific type of iolist resulting from a parsing.".
-type parse_string() :: [ uchar() | plain_string() ].



-doc """
A list whose elements are either integers (characters), binaries or other
iolists.

Most Erlang standard functions, like `file:write_file/2` and `gen_tcp:send/2`,
accept them, so converting an iolist to a binary is generally at least useless.

For example the `["foo", $b, $a, $r, <<"baz">>]` iolist represents the
`"foobarbaz"` string.

Type redefined exactly as the standard one, almost verbatim (with a name
including an underscore to avoid colliding with the builtin type) for easier
reference.

No such type as `iostring()` or `io_string()`.

See
[https://www.erlang.org/doc/reference_manual/typespec.html#types-and-their-syntax]
for more details.
""".
-type io_list() ::
		maybe_improper_list( byte() | binary() | iolist(), binary() | [] ).



-doc """
Either an iolist or a (direct, top-level) binary.

Type redefined exactly as the standard one, almost verbatim (with a name
including an underscore to avoid colliding with the builtin type) for easier
reference.

See
[https://www.erlang.org/doc/reference_manual/typespec.html#types-and-their-syntax]
for more details.
""".
-type io_data() :: iolist() | binary().



-doc "To convert strings (e.g. keywords) into others.".
-type translation_table() :: ?table( any_string(), any_string() ).



-doc """
The length of a string, typically in terms of number of characters / grapheme
clusters.
""".
-type length() :: pos_integer().



-doc """
A width, typically in terms of number of characters / grapheme clusters.
""".
-type width() :: pos_integer().



-doc """
The level of indentation (starts at zero, and the higher, the most nested).
""".
-type indentation_level() :: basic_utils:level().



-doc "A (nesting) depth, typically to keep track of indentation levels.".
-type depth() :: pos_integer().



-doc "A bullet, to denote the elements of a list.".
-type bullet() :: ustring().



-doc "Either an indentation level, or directly a bullet.".
-type indentation_level_or_bullet() :: indentation_level() | bullet().



-doc """
Lexicographic (Levenshtein) distance, i.e. minimum number of single-character
edits (i.e. insertions, deletions or substitutions) required to change one
string into the other.
""".
-type distance() :: non_neg_integer().



-doc """
See [https://erlang.org/doc/man/erlang.html#float_to_list-2] for more
information.
""".
-type float_option() ::

	% At most Decimals number of digits past the decimal point:
	{ 'decimals', 0..253 }

	% Scientific notation with Decimals digits of precision:
  | { 'scientific', 0..249 }

	% Trailing zeros at the end of the list are truncated (if using 'decimals'):
  | 'compact'.



-type format_parsing_error() ::
	{ 'format_parsing_failed', ReasonStr :: ustring() }.

% Expected types, based on a format string:
-type scan_format_outcome() :: [ value_description() ] | format_parsing_error().


-export_type([ format_string/0, format_bin_string/0, control_sequence/0,
			   format_values/0,
			   verbosity_level/0,
			   regex_string/0,
			   title/0, bin_title/0, any_title/0,
			   label/0, bin_label/0, any_label/0,
			   bin_string/0, any_string/0,
			   unicode_string/0, unicode_data/0,
			   uchar/0, plain_string/0, ustring/0, string_like/0,
			   parse_string/0, io_list/0, io_data/0,
			   translation_table/0, length/0, width/0, depth/0,
			   indentation_level/0, distance/0,

			   format_parsing_error/0, scan_format_outcome/0 ]).




% Type shorthands:

% A user-perceived character, consisting of one or more (Unicode) codepoints.
-type grapheme_cluster() :: string:grapheme_cluster().

-type chardata() :: unicode:chardata().


% As these pioneer modules are not parse-transformed:

-type void() :: basic_utils:void().
-type count() :: basic_utils:count().

-type option( T ) :: type_utils:option( T ).
-type value_description() :: type_utils:value_description().

-type ?table() :: ?table:?table().
-type ?table( K, V ) :: ?table:?table( K, V ).

-type integer_id() :: id_utils:integer_id().

-type any_millimeters() :: unit_utils:any_millimeters().



% Maybe at least format/2 would be better inlined, however it is no cross-module
% inlining (just inside this module), so a parse-transform may be used in order
% to transform text_utils:format/2 into io_lib:format/2 or into its actual,
% safer code.
%
-compile( { inline, [ format/2 ] } ).

% Defining here length/1, so having to prefix the otherwise auto-imported
% length/1 with its 'erlang' module.

 -compile([ {nowarn_unused_function, [ local_display/1, local_display/2 ]} ]).



% String management functions.



-doc "Returns a human-readable string describing the specified term.".
-spec term_to_string( term() ) -> ustring().
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



-doc """
Returns a human-readable string describing the specified term, within a bounded,
default length.
""".
-spec term_to_bounded_string( term() ) -> ustring().
% Does not happen, as empty set is actually {0,nil}:
%term_to_bounded_string( _AttrValue=[] ) ->
%   % To avoid being it interpreted as a set:
%   "(empty list or set)";
term_to_bounded_string( Term ) ->
	term_to_bounded_string( Term, _MaxLen=2000 ).



-doc """
Returns a human-readable string describing the specified term, within the
specified length.

See also `term_to_string/3`.
""".
-spec term_to_bounded_string( term(), length() | 'unlimited' ) -> ustring().
term_to_bounded_string( Term, _MaxLen=unlimited ) ->
	Term;

term_to_bounded_string( Term, MaxLen ) ->

	FullString = case set_utils:is_set( Term ) of

		true ->
			format( "[as set] ~p", [ set_utils:to_list( Term ) ] );

		false ->
			% Might not respect Unicode encoding:
			case is_any_string( Term ) of

				true ->
					format( "~ts", [ Term ] );

				false ->
					format( "~p", [ Term ] )

			end

	end,

	% To avoid that gigantic terms saturate the outputs:
	ellipse( FullString, MaxLen ).



-doc "Returns a human-readable binary string describing the specified term.".
-spec term_to_binary( term() ) -> bin_string().
term_to_binary( Term ) ->
	String = term_to_string( Term ),
	string_to_binary( String ).



-doc """
Returns a human-readable string describing the specified term, up to the
specified nesting depth.
""".
-spec term_to_string( term(), depth() ) -> ustring().
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



-doc """
Returns a human-readable string describing the specified term, up to the
specified nesting depth, and up to the specified string length (at least 3, so
that the `"..."` marker can be inserted).

A plain string is returned (not an `iolist/0` for example).

See also `term_to_bounded_string/{1,2}`.
""".
-spec term_to_string( term(), depth(), count() )-> ustring().
term_to_string( _Term=[], _MaxDepthCount, _MaxLength ) ->
	% Otherwise would be an empty string:
	"[]";

term_to_string( Term, MaxDepthCount, MaxLength ) when MaxLength >= 3 ->

	% First limit the depth (beware of iolists!):
	FullString = case io_lib:printable_list( Term ) of

		true ->
			% The '*' character in the format string is not suitable here:
			lists:flatten( io_lib:format( "~ts", [ Term ] ) );

		_ ->
			lists:flatten( io_lib:format( "~P", [ Term, MaxDepthCount ] ) )

	end,

	% Then limit the length:
	case erlang:length( FullString ) of

		L when L > MaxLength ->
			% We have to truncate here, length( "..." ) = 3
			%
			% MaxLength - 3 = 0 is allowed there:
			string:sub_string( FullString, 1, MaxLength - 3 ) ++ " ..";

		_ ->
			FullString

	end.



-doc """
Converts the specified integer into a plain string.

Avoids to have to use `lists:flatten/1` when converting an integer to a
string. Useless when using functions like `io:format/2`, that accept iolists as
parameters.
""".
-spec integer_to_string( integer() ) -> ustring().
integer_to_string( IntegerValue ) ->
	% Nonsensical: hd( io_lib:format( "~B", [ IntegerValue ] ) ).
	%io_lib:format( "~B", [ IntegerValue ] ).
	erlang:integer_to_list( IntegerValue ).



-doc """
Converts the specified integer into a binary string.

Avoids to have to use `lists:flatten/1` when converting an integer to a
string. Useless when using functions like `io:format/2`, that accept iolists as
parameters.
""".
-spec integer_to_binary( integer() ) -> bin_string().
integer_to_binary( IntegerValue ) ->
	erlang:integer_to_binary( IntegerValue ).



% Hexadecimal notes:

% Regarding zero-padding:
%
% None is done (no zeros added on the left of the resulting hexastring), as the
% expected size of the corresponding value type cannot be determined; for
% example integer_to_hexastring(16#f) returns "f" - but "0f", "00f", etc. would
% be equally true. It is up to the caller, if appropriate, to pad the returned
% hexastring with zeros, possibly with: pad_string_right(HexaStr,_Width=3, $0)
% in order to obtain, once flattened, "00f" instead of "f".

% Regarding the "0x" prefix:
%
% We consider the "0x" hexadecimal prefix as fully optional: now, by default,
% none is expected, none is added.



-doc """
Returns a plain string corresponding to the specified integer, in hexadecimal
form (with no `"0x"` prefix).

For example: `integer_to_hexastring(3432) = "d68"`.

Refer to the `Hexadecimal notes` section above, regarding zero-padding and
`"0x"` prefixing.
""".
-spec integer_to_hexastring( integer() ) -> hexastring().
integer_to_hexastring( IntegerValue ) ->
	integer_to_hexastring( IntegerValue, _AddPrefix=false ).



-doc """
Returns a plain string corresponding to the specified integer, in hexadecimal
form, with a `"0x"` prefix if requested.

For example: `integer_to_hexastring(3432, _AddPrefix=true) = "0xd68"`.

Refer to the `Hexadecimal notes` section above, regarding zero-padding and
`"0x"` prefixing.
""".
-spec integer_to_hexastring( integer(), boolean() ) -> hexastring().
integer_to_hexastring( IntegerValue, _AddPrefix=true ) ->
	?hexa_prefix ++ integer_to_hexastring( IntegerValue, _Prefix=false );

integer_to_hexastring( IntegerValue, _AddPrefix=false ) ->
	to_lowercase( erlang:integer_to_list( IntegerValue, _Base=16 ) ).



-doc """
Returns a binary string corresponding to the specified integer, in hexadecimal
form (with no `"0x"` prefix).

For example: `integer_to_hexabinstring(3432) = <<"d68">>`.

Refer to the `Hexadecimal notes` section above, regarding zero-padding and
`"0x"` prefixing.
""".
-spec integer_to_hexabinstring( integer() ) -> hexastring().
integer_to_hexabinstring( IntegerValue ) ->
	string_to_binary( integer_to_hexastring( IntegerValue ) ).



-doc """
Returns a binary string corresponding to the specified integer, in hexadecimal
form, with a `"0x"` prefix if requested.

For example: `integer_to_hexabinstring(3432, _AddPrefix=true) = <<"0xd68">>`.

Refer to the 'Hexadecimal notes' section above, regarding zero-padding and
`"0x"` prefixing.
""".
-spec integer_to_hexabinstring( integer(), boolean() ) -> hexastring().
integer_to_hexabinstring( IntegerValue, AddPrefix ) ->
	string_to_binary( integer_to_hexastring( IntegerValue, AddPrefix ) ).



-doc """
Returns an integer corresponding to the specified string containing a (single)
hexadecimal number as a text (not expected to start with a `"0x"` prefix).

Note: both uppercase and lowercase letters are supported.

For example: `hexastring_to_integer("d68") = 3432`.
""".
-spec hexastring_to_integer( hexastring() ) -> integer().
hexastring_to_integer( HexaString ) ->
	hexastring_to_integer( HexaString, _ExpectPrefix=false ).



-doc """
Returns an integer corresponding to the specified string containing a (single)
hexadecimal number as a text, expected to start with a `"0x"` prefix if
specified.

Note: both uppercase and lowercase letters are supported.

For example: `hexastring_to_integer("0xd68", _ExpectPrefix=true) = 3432`.
""".
-spec hexastring_to_integer( hexastring(), boolean() ) -> integer().
hexastring_to_integer( ?hexa_prefix ++ HexaString, _ExpectPrefix=true ) ->
	hexastring_to_integer( HexaString, _HasPrefix=false );

hexastring_to_integer( Other, _ExpectPrefix=true ) ->
	throw( { invalid_hexastring, Other, { lacking_prefix, ?hexa_prefix } } );

hexastring_to_integer( HexaString, _ExpectPrefix=false ) ->
	list_to_integer( HexaString, _Base=16 ).



-doc """
Returns a plain string corresponding to the specified binary, in hexadecimal
form (with no `"0x"` prefix).

For example: `binary_to_hexastring(<<"hello">>) = "68656c6c6f"`.
""".
-spec binary_to_hexastring( binary() ) -> hexastring().
binary_to_hexastring( Bin ) ->
	binary_to_hexastring( Bin, _AddPrefix=false ).



-doc """
Returns a plain string corresponding to the specified binary, in hexadecimal
form, with a `"0x"` prefix if requested.

For example:
`binary_to_hexastring(<<"hello">>, _AddPrefix=true) = "0x68656c6c6f"`.
""".
-spec binary_to_hexastring( binary(), boolean() ) -> hexastring().
binary_to_hexastring( Bin, _AddPrefix=true ) ->
	?hexa_prefix ++ binary_to_hexastring( Bin, _Prefix=false );

binary_to_hexastring( Bin, _AddPrefix=false ) ->
	% Binary comprehension; left-padding with a zero, as
	% erlang:integer_to_list(I) for I in [0,10] results in "I", not "0I":
	%
	to_lowercase( flatten( [
		case erlang:integer_to_list( Int, _Base=16 ) of

			[ SingleChar ] ->
				[ $0, SingleChar ];

			TwoChars ->
				TwoChars

		end || <<Int>> <= Bin ] ) ).



-doc """
Returns the binary corresponding to the specified binary string that contains a
series of hexadecimal values.

No `"0x"` prefix is expected.

For example: `hexabinstring_to_binary(<<"ffac01">>) = <<255,172,1>>`.
""".
-spec hexabinstring_to_binary( hexabinstring() ) -> binary().
hexabinstring_to_binary( HexaBinStr ) ->
	hexastring_to_binary( binary_to_string( HexaBinStr ) ).



-doc """
Returns the binary corresponding to the specified string that contains a series
of hexadecimal values.

No `"0x"` prefix is expected.

For example: `hexastring_to_binary("ffac01") = <<255,172,1>>`.
""".
-spec hexastring_to_binary( hexastring() ) -> binary().
hexastring_to_binary( HexaStr ) ->
	hexastring_to_binary( HexaStr, _BinAcc= <<>> ).


% (helper)
hexastring_to_binary( _HexaStr=[], BinAcc ) ->
	% No reversing:
	BinAcc;

% Two hexadecimal characters account for one byte:
hexastring_to_binary( _HexaStr=[ Hex1, Hex2 | T ], BinAcc ) ->
	% For example: "3c".
	TwoCharStr = [ Hex1, Hex2 ],
	Int = list_to_integer( TwoCharStr, _Base=16 ),
	NewBinAcc = <<BinAcc/binary,Int/integer>>,
	hexastring_to_binary( T, NewBinAcc );

% Odd number of hexadecimal characters, at least currently not knowing the
% corresponding byte to insert:
%
hexastring_to_binary( _HexaStr=[ SingleHex ], BinAcc ) ->
	throw( { single_hex_remaining, SingleHex, BinAcc } ).



-doc """
Returns a plain string corresponding to the specified integer once translated to
a series of bits, listed per groups of 4, not padded.

Example: `"0b100-0000-0011" = integer_to_bits(1024+2+1)`.
""".
-spec integer_to_bits( integer() ) -> ustring().
integer_to_bits( I ) ->
	AllBits = io_lib:format( "~.2B", [ I ] ),

	% We want to group bits, but from right to left:
	RevAllBits = lists:reverse( AllBits ),
	RevPacketRevStrs = split_every( _GroupCount=4, RevAllBits ),
	RevPacketStrs = [ lists:reverse( S ) || S <- RevPacketRevStrs ],
	"0b" ++ join( _Sep=$-, lists:reverse( RevPacketStrs ) ).



-doc """
Returns a plain string corresponding to the specified integer once translated to
a series of bits, listed per groups of 4, possibly padded with zeros on the left
to reach the specified number of bits.

Example: `"0b0000-0100-0000-0011" = integer_to_bits(1024+2+1, 16)`.
""".
-spec integer_to_bits( integer(), width() ) -> ustring().
integer_to_bits( I, PadWidth ) ->
	AllBits = io_lib:format( "~.2B", [ I ] ),
	AllBitsPadded = list_utils:flatten_once(
		pad_string_right( AllBits, PadWidth, _PadChar=$0 ) ),

	% We want to group bits, but from right to left:
	RevAllBits = lists:reverse( AllBitsPadded ),
	RevPacketRevStrs = split_every( _GroupCount=4, RevAllBits ),
	RevPacketStrs = [ lists:reverse( S ) || S <- RevPacketRevStrs ],
	"0b" ++ join( _Sep=$-, lists:reverse( RevPacketStrs ) ).



-doc "Returns a plain string corresponding to the specified atom.".
-spec atom_to_string( atom() ) -> ustring().
atom_to_string( Atom ) ->
	atom_to_list( Atom ).



-doc """
Returns nested characters corresponding to the specified PID.

For example `["<0.84.0>"]`.
""".
-spec pid_to_string( pid() ) -> chars().
pid_to_string( Pid ) ->
	io_lib:format( "~w", [ Pid ] ).



-doc """
Returns nested characters corresponding to the specified list of PIDs.

For example `[[91,["<0.84.0>",44,"<0.84.0>"],93]]`.
""".
-spec pids_to_string( [ pid() ] ) -> chars().
pids_to_string( PidList ) ->
	io_lib:format( "~w", [ PidList ] ).



-doc """
Returns a short, plain string corresponding to the specified PID.

For example, `<0.33.0>` returned as `"|33|"` (half size); `<1.44.0>` returned as
`"|1.44|"`, `<1.55.7>` as `"|1.55.7|"` (same size).

Note though that the pipe character may be better avoided on some systems
(e.g. Ceylan-Traces ones, at least for the name of trace emitters).
""".
-spec pid_to_short_string( pid() ) -> ustring().
pid_to_short_string( Pid ) ->
	% concatenate/1 could be used:
	%[ $< | pid_to_core_string( Pid ) ] ++ ">".
	[ $| | pid_to_core_string( Pid ) ] ++ "|".



-doc """
Returns a short, plain string corresponding to the specified PIDs.

For example, `[<0.33.0>,<0.35.0>]` is returned as `"|33,35|"` (7 characters
instead of 19, almost one-third).
""".
-spec pids_to_short_string( [ pid() ] ) -> ustring().
pids_to_short_string( PidList ) ->
	% concatenate/1 could be used:

	% Preferring an extra character, as better allowing to break longer lines:
	%Sep = ",",
	Sep = ", ",

	[ $| | join( Sep, [ pid_to_core_string( P ) || P <- PidList ] ) ] ++ "|".



-doc """
Returns a very short plain string corresponding to the specified PID.

For example, for:
 - `<0.33.0>` will return `"33"`
 - `<1.44.0>` will return `"1.44"`
 - `<1.55.7>` will return `"1.55.7"`
""".
-spec pid_to_core_string( pid() ) -> ustring().
pid_to_core_string( Pid ) ->

	% A PID is akin to <X.Y.Z>.

	% Needed otherwise returns ["<0.78.0>"], not "<0.78.0>":
	PidAsText = hd( io_lib:format( "~w", [ Pid ] ) ),

	%trace_utils:debug_fmt( "PidAsText = '~p'.", [ PidAsText ] ),

	[ $< | Rest ] = PidAsText,

	% PidCore is thus "X.Y.Z":
	PidCore = list_utils:remove_last_element( Rest ),

	%trace_utils:debug_fmt( "PidCore = '~w'.", [ PidCore ] ),

	% For example: ["0","33","0"]:
	[ First, Second, Third ] = split( PidCore, [ _Sep=$. ] ),

	% Automatic truncating if defaults:
	ActualFirst = case First of

		"0" ->
			[];

		_ ->
			First ++ "."

	 end,

	 ActualThird = case Third of

		"0" ->
			[];

		_ ->
			"." ++ Third

	end,

	% For example: "33", "1.33", or "1.33.2":
	ActualFirst ++ Second ++ ActualThird.



-doc """
Returns a plain string corresponding to the specified PID and that is suitable
to be (at least part of) a filename.

For example, for `<0.84.0>`, returns `"0.84.0"`.
""".
-spec pid_to_filename( pid() ) -> ustring().
pid_to_filename( Pid ) ->

	% A PID is akin to <X.Y.Z>.

	% Needed otherwise returns ["<0.78.0>"], not "<0.78.0>":
	PidAsText = hd( io_lib:format( "~w", [ Pid ] ) ),

	%trace_utils:debug_fmt( "PidAsText = '~p'.", [ PidAsText ] ),

	[ $< | Rest ] = PidAsText,

	% Thus "X.Y.Z":
	list_utils:remove_last_element( Rest ).



-doc """
Returns a string describing the specified record.

Hugely inspired from a Ulf Wiger's snippet described in
[http://erlang.org/pipermail/erlang-questions/2006-September/023181.html].

As records are compile-time structures only, there is no simple way of
determining the name of their fields at runtime.
""".
-spec record_to_string( _ ) -> ustring().
record_to_string( _Record ) -> % No 'when is_record( Record, Tag ) ->' here.

	throw( { not_implemented, record_to_string } ).

	%RF = fun(R,L) when R == element(1,Record) ->
	%   % Needs apparently a parse transform:
	%   Fields = '#info-'(Record),
	%   true = (L == length(Fields)),
	%   Fields
	%end,
	%
	%io_lib_pretty:print( Record, RF ).



-doc "Returns the default bullet to be used for top-level lists.".
-spec get_default_bullet() -> ustring().
get_default_bullet() ->
	get_bullet_for_level( 0 ).



-doc "Returns the bullet to be used for the specified indentation level.".
-spec get_bullet_for_level( indentation_level() ) -> bullet().
get_bullet_for_level( 0 ) ->
	" + ";

get_bullet_for_level( 1 ) ->
	"    - ";

get_bullet_for_level( 2 ) ->
	"       * ";

get_bullet_for_level( N ) when is_integer( N ), N > 0 ->
	Base = get_bullet_for_level( N rem 3 ),
	string:copies( "   ", ( N div 3 ) + 1 ) ++ Base.



-doc """
Returns the indentation offset to be used for the specified indentation level of
enumerated lists.
""".
-spec get_indentation_offset_for_level( indentation_level() ) ->  ustring().
get_indentation_offset_for_level( N ) ->
	string:copies( _BaseString="   ", _Count=N+1 ).


% (helper)
%
% Note:
% - the caller should have already vetted the specified arguments
% - binaries are welcome as well
%
strings_to_string_helper( _Strings=[], Acc, _Bullet ) ->
	Acc;

% We do not want an extra newline at the end:
strings_to_string_helper( _Strings=[ LastString ], Acc, Bullet )
                when is_list( LastString ); is_binary( LastString ) ->
	%Pattern = "~ts~n",
	% Added back, as makes sense?
	% Nope:
	Pattern = "~ts",
	Acc ++ Bullet ++ io_lib:format( Pattern, [ LastString ] );

% We allow also for bin_string():
strings_to_string_helper( _Strings=[ H | T ], Acc, Bullet )
                when is_list( H ); is_binary( H ) ->
	% Byproduct of the trailing newline: an empty line at the end if nested.
	strings_to_string_helper( T,
		Acc ++ Bullet ++ io_lib:format( "~ts~n", [ H ] ), Bullet );

strings_to_string_helper( _Strings=[ H | _T ], _Acc, _Bullet ) ->
	report_not_a_string( H ).



-doc """
Returns a string that pretty-prints the specified list of strings, with
enumerated (that is 1, 2, 3) bullets, not specifically indented as a whole.
""".
-spec strings_to_enumerated_string( [ string_like() ] ) -> ustring().
strings_to_enumerated_string( Strings ) ->
	strings_to_enumerated_string( Strings, _DefaultIndentationLevel=0 ).



-doc """
Returns a string that pretty-prints the specified list of strings, with
enumerated (that is 1, 2, 3) bullets, for the specified indentation and not
prefixed.
""".
-spec strings_to_enumerated_string( [ string_like() ], indentation_level() ) ->
											ustring().
strings_to_enumerated_string( Strings, IndentationLevel ) ->
	strings_to_enumerated_string( Strings, IndentationLevel, _Prefix="" ).



-doc """
Returns a string that pretty-prints the specified list of strings, with
enumerated (that is 1, 2, 3) bullets, indented after the specified prefix.
""".
-spec strings_to_enumerated_string( [ string_like() ], indentation_level(),
									ustring() ) -> ustring().
strings_to_enumerated_string( _Strings=[ Str ], _IndentationLevel, _Prefix ) ->
	Str;

strings_to_enumerated_string( Strings, IndentationLevel, Prefix ) ->

	IndentStr = get_indentation_offset_for_level( IndentationLevel ),

	{ _FinalCount, ReversedStrings } = lists:foldl(
		fun( String, _Acc={ Count, Strs } ) ->

			NewStrs = [ format( "~n~ts~ts~B. ~ts",
								[ Prefix, IndentStr, Count, String ] ) | Strs ],

			{ Count+1, NewStrs }

		end,
		_Acc0={ 1, [] },
		_List=Strings ),

	OrderedStrings = lists:reverse( ReversedStrings ),

	format( "~ts~n", [ lists:flatten( OrderedStrings ) ] ).



-doc """
Returns a (Erlang) comment string (a series of lines starting with '%')
that pretty-prints the specified list of strings, with enumerated (that is 1,
2, 3) bullets, not specifically indented.
""".
-spec strings_to_enumerated_comment( [ string_like() ] ) -> ustring().
strings_to_enumerated_comment( Strings ) ->
	strings_to_enumerated_comment( Strings, _IndentationLevel=0 ).



-doc """
Returns a (Erlang) comment string (a series of lines starting with '%') that
pretty-prints the specified list of strings, with enumerated (that is 1, 2, 3)
bullets, with the specified indentation at each beginning of comment line.
""".
-spec strings_to_enumerated_comment( [ string_like() ], indentation_level() ) ->
															ustring().
strings_to_enumerated_comment( Strings, IndentationLevel ) ->
	strings_to_enumerated_string( Strings, IndentationLevel, _Prefix="% " ).



-doc """
Returns a plain string that pretty-prints the specified list of strings
(actually the list may contain also binary strings), with default bullets.
""".
-spec strings_to_string( [ string_like() ] ) -> ustring().
strings_to_string( _Strings=[] ) ->
	"(empty list)";

strings_to_string( Strings=[ SingleString ] )
		when is_list( SingleString ); is_binary( SingleString ) ->

	% Not retained, as the single string may itself correspond to a full, nested
	% list and no dangling final quote is desirable:
	%io_lib:format( " '~ts'", Strings );

	% No leading space, the caller is expected to have it specified by himself,
	% like in: "foo: ~ts", not as "foo:~ts":

	% To force a plain string:
	%io_lib:format( " ~ts", Strings );
	io_lib:format( "~ts", Strings );

strings_to_string( Strings ) when is_list( Strings ) ->

	%trace_utils:debug_fmt( "Stringifying ~p.", [ Strings ] ),

	% Leading '~n' had been removed for some unknown reason:
	io_lib:format( "~n~ts~n", [ strings_to_string_helper( Strings,
		_Acc=[], get_default_bullet() ) ] );

strings_to_string( ErrorTerm ) ->
	report_not_a_list( ErrorTerm ).



-doc """
Returns a string that pretty-prints the specified list of strings (actually, any
element that can be processed with `~ts` will do; e.g. atoms) once reordered
(and with default bullets).
""".
-spec strings_to_sorted_string( [ string_like() ] ) -> ustring().
strings_to_sorted_string( Strings ) when is_list( Strings ) ->
	strings_to_string( lists:sort( Strings ) );

strings_to_sorted_string( ErrorTerm ) ->
	report_not_a_list( ErrorTerm ).



-doc """
Returns a string that pretty-prints the specified list of strings (actually, any
element that can be processed with ~ts will do; e.g. atoms), with user-specified
bullets or indentation level.

This can be a solution to nest bullet lists, by specifying a bullet with an
offset, such as `" * "`.
""".
-spec strings_to_string( [ string_like() ], indentation_level_or_bullet() ) ->
								ustring().
strings_to_string( _Strings=[], _IndentationOrBullet ) ->
	"(empty list)";

strings_to_string( _Strings=[ SingleString ], _IndentationOrBullet )
								when is_list( SingleString ) ->
	% For a single string, no need for leading and trailing newlines, but it
	% used to be separated (with single quotes) from the surrounding text
	% (not done anymore, as this single element may be itself a bullet list)
	%
	SingleString;

strings_to_string( Strings, IndentationLevel )
								when is_integer( IndentationLevel ) ->
	Bullet = get_bullet_for_level( IndentationLevel ),
	strings_to_string( Strings, Bullet );

strings_to_string( Strings, Bullet )
                                when is_list( Strings ), is_list( Bullet ) ->

	%trace_utils:debug_fmt( "strings_to_string/2 for '~p' : bullet is '~ts'.",
	%                       [ Strings, Bullet ] ),

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

	io_lib:format( Pattern,
		[ strings_to_string_helper( Strings, _Acc=[], Bullet ) ] );

strings_to_string( Strings, Bullet ) when is_list( Bullet ) ->
	report_not_a_list( Strings );

strings_to_string( _Strings, IncorrectBullet ) ->
	throw( { bullet_not_a_string, IncorrectBullet } ).



-doc """
Returns a plain string that pretty-prints the specified list of strings
(actually the list may contain also binary strings), with default bullets and a
blank line before each top-level entry in order to better space them, for an
increased readability.
""".
-spec strings_to_spaced_string( [ any_string() ] ) -> ustring().
strings_to_spaced_string( _Strings=[] ) ->
	"(empty list)";

strings_to_spaced_string( Strings=[ SingleString ] )
		when is_list( SingleString ); is_binary( SingleString ) ->

	% Not retained, as the single string may itself correspond to a full, nested
	% list and no dangling final quote is desirable:
	%io_lib:format( " '~ts'", Strings );

	% No leading space, the caller is expected to have it specified by himself,
	% like in: "foo: ~ts", not as "foo:~ts":

	% To force a plain string:
	%io_lib:format( " ~ts", Strings );
	io_lib:format( "~ts", Strings );

strings_to_spaced_string( Strings ) when is_list( Strings ) ->

	%trace_utils:debug_fmt( "Stringifying ~p.", [ Strings ] ),

	SpacedBullet = [ $\n | get_default_bullet() ],

	io_lib:format( "~n~ts~n",
		[ strings_to_string_helper( Strings, _Acc=[], SpacedBullet ) ] );

strings_to_spaced_string( ErrorTerm ) ->
	report_not_a_list( ErrorTerm ).



-doc """
Returns a string that pretty-prints the specified list of strings (actually, any
element that can be processed with ~ts will do; e.g. atoms), with user-specified
bullets or indentation level, and a blank line before each top-level entry in
order to better space them, for an increased readability.

This can be a solution to nest bullet lists, by specifying a bullet with an
offset, such as `" * "`.
""".
-spec strings_to_spaced_string( [ ustring() ],
								indentation_level_or_bullet() ) -> ustring().
strings_to_spaced_string( _Strings=[], _IndentationOrBullet ) ->
	"(empty list)";

strings_to_spaced_string( _Strings=[ SingleString ], _IndentationOrBullet )
									when is_list( SingleString ) ->
	% For a single string, no need for leading and trailing newlines, but it
	% used to be separated (with single quotes) from the surrounding text
	% (not done anymore, as this single element may be itself a bullet list)
	%
	SingleString;

strings_to_spaced_string( Strings, IndentationLevel )
									when is_integer( IndentationLevel ) ->
	Bullet = get_bullet_for_level( IndentationLevel ),
	strings_to_spaced_string( Strings, Bullet );

strings_to_spaced_string( Strings, Bullet )
			when is_list( Strings ), is_list( Bullet ) ->

	%trace_utils:debug_fmt( "strings_to_spaced_string/2 for '~p' : "
	%    "bullet is '~ts'.", [ Strings, Bullet ] ),

	Pattern = "~n~ts",

	SpacedBullet = [ $\n | Bullet ],

	io_lib:format( Pattern,
		[ strings_to_string_helper( Strings, _Acc=[], SpacedBullet ) ] );

strings_to_spaced_string( Strings, Bullet ) when is_list( Bullet ) ->
	report_not_a_list( Strings );

strings_to_spaced_string( _Strings, IncorrectBullet ) ->
	throw( { bullet_not_a_string, IncorrectBullet } ).



-doc """
Returns a string that pretty-prints the specified list of strings (actually, any
element that can be processed with `~ts` will do; e.g. atoms) once reordered,
with user-specified indentation level or bullet.
""".
-spec strings_to_sorted_string( [ string_like() ],
								indentation_level_or_bullet() ) -> ustring().
strings_to_sorted_string( Strings, IndentationOrBullet )
											when is_list( Strings ) ->
	strings_to_string( lists:sort( Strings ), IndentationOrBullet );

strings_to_sorted_string( ErrorTerm, _IndentationOrBullet ) ->
	report_not_a_list( ErrorTerm ).



-doc """
Returns a plain string that pretty-prints the specified list of binary strings,
with default bullets.
""".
-spec binaries_to_string( [ bin_string() ] ) -> ustring().
binaries_to_string( Binaries ) ->
	binaries_to_string( Binaries, _IndentationLevel=0 ).



-doc """
Returns a binary string that pretty-prints the specified list of binary strings,
with the specified indentation level or bullet.
""".
-spec binaries_to_string( [ bin_string() ], indentation_level_or_bullet() ) ->
								ustring().
% See strings_to_string/2 for a counterpart implementation.
%
% A conversion to strings followed by the use of strings_to_string/2 is not the
% way to go as some binary strings (e.g. "raw filenames") cannot be converted to
% plain strings, due to a mismatching encoding. strings_to_string/2 cannot be
% used directly either, because of its guards (which should be kept, as it is
% not supposed to support binaries). So we have to mimic it here.
%
binaries_to_string( _Binaries=[ SingleBinString ],
					_IndentationOrBullet ) when is_binary( SingleBinString ) ->
	%binary_to_string( SingleBinString );
	io_lib:format( "~ts", [ SingleBinString ] );

binaries_to_string( Binaries, IndentationLevel )
								when is_integer( IndentationLevel ) ->
	Bullet = get_bullet_for_level( IndentationLevel ),
	binaries_to_string( Binaries, Bullet );

binaries_to_string( Binaries, Bullet )
                                when is_list( Binaries ), is_list( Bullet ) ->
	Pattern = "~n~ts~n",
	% Actually no need for a dedicated binaries_to_string_helper/3:
	io_lib:format( Pattern,
		[ strings_to_string_helper( Binaries, _Acc=[], Bullet ) ] );

binaries_to_string( Binaries, Bullet ) when is_list( Bullet ) ->
	report_not_a_list( Binaries );

binaries_to_string( _Binaries, IncorrectBullet ) ->
	throw( { bullet_not_a_string, IncorrectBullet } ).



-doc """
Returns a string that pretty-prints the specified list of sorted binary strings,
with default bullets.
""".
-spec binaries_to_sorted_string( [ bin_string() ] ) -> ustring().
binaries_to_sorted_string( Binaries ) ->
	Strings = binaries_to_strings( Binaries ),
	strings_to_string( lists:sort( Strings ) ).



-doc """
Returns a string that pretty-prints the specified list of binary strings, listed
directly along the text (not one item per line).

For example: `binaries_to_listed_string([<<"red">>, <<"blue">>, <<"green">>])`
returns `"red, blue and green"`.
""".
-spec binaries_to_listed_string( [ bin_string() ] ) -> ustring().
binaries_to_listed_string( Binaries ) ->
	strings_to_listed_string( [ binary_to_string( B ) || B <- Binaries ] ).



-doc """
Returns a binary string that pretty-prints the specified list of binary strings,
with default bullets.
""".
-spec binaries_to_binary( [ bin_string() ] ) -> bin_string().
binaries_to_binary( Binaries ) ->
	binaries_to_binary( Binaries, get_default_bullet() ).



-doc """
Returns a binary string that pretty-prints the specified list of binary strings,
with user-specified bullets or indentation level.
""".
-spec binaries_to_binary( [ bin_string() ], indentation_level_or_bullet() ) ->
								bin_string().
% Not wanting to ever convert to plain strings (to avoid any encoding mismatch):
binaries_to_binary( _Binaries=[], _Bullet ) ->
	<<"(empty list)">>;

% Hopefully a binary:
binaries_to_binary( _Binaries=[ SingleBin ], _Bullet ) ->
	SingleBin;

binaries_to_binary( Binaries, IndentationLevel )
							when is_integer( IndentationLevel ) ->
	Bullet = get_bullet_for_level( IndentationLevel ),
	binaries_to_binary( Binaries, Bullet );

binaries_to_binary( Binaries, Bullet )
							when is_list( Binaries ), is_list( Bullet ) ->

	%trace_utils:debug_fmt( "Binaries: ~p, Bullet: '~p'.",
	%                       [ Binaries, Bullet ] ),

	% Operating first on a list of binaries:
	BinNewline = <<"\n">>,
	Inter = [ BinNewline, Bullet ],
	L = [ [ Inter, Bin ] || Bin <- Binaries ],
	Res = erlang:list_to_binary( L ++ [ BinNewline ] ),
	%trace_utils:debug_fmt( "Returned binary: ~p", [ Res ] ),
	Res;

binaries_to_binary( Binaries, Bullet ) when is_list( Bullet ) ->
	report_not_a_list( Binaries );

binaries_to_binary( _Binaries, IncorrectBullet ) ->
	throw( { bullet_not_a_string, IncorrectBullet } ).



-doc """
Returns a (plain) string corresponding to the specified (byte) buffer, expected
to contain a 8 bit ASCII null-terminated string.
""".
-spec buffer_to_string( binary() ) -> ustring().
buffer_to_string( Bin ) ->
	buffer_to_string( Bin, _Acc=[], Bin ).


% (helper)
buffer_to_string( _Bin= <<>>, _Acc, OriginalBin ) ->
	throw( { not_null_terminated, OriginalBin } );

% End of string found:
buffer_to_string( _Bin= <<0,_T/binary>>, Acc, _OriginalBin ) ->
	lists:reverse( Acc );

buffer_to_string( _Bin= <<H,T/binary>>, Acc, OriginalBin ) ->
	buffer_to_string( T, [ H | Acc ], OriginalBin ).



-doc """
Returns a binary string corresponding to the specified (byte) buffer, expected
to contain a 8 bit ASCII null-terminated string.
""".
-spec buffer_to_binstring( binary() ) -> bin_string().
buffer_to_binstring( Bin ) ->
	%string_to_binary( buffer_to_string( Bin ) ).
	% Possibly more efficient:
	case binary:split( Bin, _Null= <<0>> ) of

		[ _SingleElem ] ->
			throw( { not_null_terminated, Bin } );

		% Never empty by design:
		[ FirstElemBin | _T ] ->
			FirstElemBin

	end.



-doc """
Returns a string that pretty-prints the specified list of atoms, with default
bullets.
""".
-spec atoms_to_string( [ atom() ] ) -> ustring().
atoms_to_string( Atoms ) ->
	io_lib:format( "~n~ts", [ atoms_to_string( Atoms, _Acc="" ) ] ).


% (helper)
atoms_to_string( _Atoms=[], Acc ) ->
	Acc;

atoms_to_string( _Atoms=[ A | T ], Acc ) when is_atom( A )  ->
	atoms_to_string( T,
		Acc ++ get_default_bullet() ++ io_lib:format(  "~ts~n", [ A ] ) );

atoms_to_string( _Atoms=[ H | _T ], _Acc ) ->
	throw( { not_atom, H } );

atoms_to_string( Term, _Acc ) ->
	throw( { not_list, Term } ).



-doc """
Returns a string that pretty-prints the specified list of atoms once ordered,
with default bullets.
""".
-spec atoms_to_sorted_string( [ atom() ] ) -> ustring().
atoms_to_sorted_string( Atoms ) ->
	atoms_to_string( lists:sort( Atoms ) ).



-doc """
Returns a string that pretty-prints the specified list of atoms, listed directly
(in an unquoted form) in the returned text.

For example: `atoms_to_listed_string([red, blue, green])` returns `"red, blue
and green"`.
""".
-spec atoms_to_listed_string( [ atom() ] ) -> ustring().
atoms_to_listed_string( Atoms ) ->
	Strings = [ atom_to_string( A ) || A <- Atoms ],
	strings_to_listed_string( Strings ).



-doc """
Returns a string that pretty-prints the specified list of atoms, listed
directly, in a quoted form, in the returned text.

For example: `atoms_to_quoted_listed_string([red, blue, green])` returns
`"'red', 'blue' and 'green'"`.
""".
-spec atoms_to_quoted_listed_string( [ atom() ] ) -> ustring().
atoms_to_quoted_listed_string( Atoms ) ->
	Strings = [ text_utils:format("'~ts'", [ A ] ) || A <- Atoms ],
	strings_to_listed_string( Strings ).



-doc """
Returns a string that pretty-prints the specified list of integers, listed
directly in the returned text.

For example: `integers_to_listed_string([1, 13, 8])` returns `"1, 13 and 8"`.
""".
-spec integers_to_listed_string( [ integer() ] ) -> ustring().
integers_to_listed_string( ListOfIntegers ) ->
	Strings = [ integer_to_string( I ) || I <- ListOfIntegers ],
	strings_to_listed_string( Strings ).



-doc """
Returns a string that pretty-prints the specified list of integer identifiers,
listed directly in the returned text.

For example: `integer_ids_to_listed_string([1, 13, 8])` returns `"#1, #13 and
#8"`.
""".
-spec integer_ids_to_listed_string( [ integer_id() ] ) -> ustring().
integer_ids_to_listed_string( IntegerIds ) ->
	Strings = [ text_utils:format( "#~B", [ I ] ) || I <- IntegerIds ],
	strings_to_listed_string( Strings ).



-doc """
Returns a string that pretty-prints the specified list of strings, listed
directly along the text (not one item per line).

For example: `strings_to_listed_string([ "red", "blue", "green"])` returns
`"red, blue and green"`.
""".
%strings_to_listed_string( _Strings=[] ) ->
%   throw( empty_list_of_strings_to_list );
% Probably more relevant:
-spec strings_to_listed_string( [ ustring() ] ) -> ustring().
strings_to_listed_string( Strings ) ->
	strings_to_listed_string( Strings, _Lang=english ).



-doc """
Returns a string that pretty-prints the specified list of strings, listed
directly along the text (not one item per line), according to the specified
(human) language.

For example: `strings_to_listed_string(["red", "blue", "green"])` returns `"red,
blue and green"`.
""".
%strings_to_listed_string( _Strings=[] ) ->
%   throw( empty_list_of_strings_to_list );
% Probably more relevant:
-spec strings_to_listed_string( [ ustring() ],
								language_utils:human_language() ) -> ustring().
strings_to_listed_string( _Strings=[], _Lang ) ->
	"";

strings_to_listed_string( _Strings=[ SingleString ], _Lang ) ->
	SingleString;

strings_to_listed_string( Strings, Lang ) ->

	% Here all strings shall be separated with commas, except the last, starting
	% with "and":

	% We do not want here a dependency onto list_utils, which is not
	% bootstrapped, as this current function might be called from the Myriad
	% parse transform.

	%{ LastString, OtherStrings } =
	%   list_utils:extract_last_element( Strings ),

	% A somewhat inlined version of it:
	[ LastString | RevOtherStrings ] = lists:reverse( Strings ),

	OtherStrings = lists:reverse( RevOtherStrings ),

	OtherStringsString = join( ", ", OtherStrings ),

	case Lang of

		french ->
			format( "~ts et ~ts", [ OtherStringsString, LastString ] );

		english ->
			format( "~ts and ~ts", [ OtherStringsString, LastString ] )

	end.



-doc """
Returns a string that pretty-prints the specified list of maybe-strings
(ignoring undefined ones), listed directly along the text (not one item per
line).

For example: `maybe_strings_to_listed_string(["red", "blue", undefined, "green",
undefined])` returns `"red, blue and green"`.
""".
-spec maybe_strings_to_listed_string( [ option( ustring() ) ] ) -> ustring().
maybe_strings_to_listed_string( Strings ) ->
	strings_to_listed_string( [ S || S <- Strings, S =/= undefined ] ).



-doc """
Returns a list whose elements are atoms corresponding to the plain strings
supposedly composing the specified list.

For example: `strings_to_atoms(["abc","def"])` should return `[abc, def]`.

Note that only a bounded number of atoms should be created that way, lest the
atom table gets saturated.
""".
-spec strings_to_atoms( [ ustring() ] ) -> [ atom() ].
strings_to_atoms( StringList ) when is_list( StringList ) ->
	[ list_to_atom( X ) || X <- StringList ].



-doc """
Returns a string that pretty-prints the specified list of key (as binary, string
or atom) / value pairs, with bullets, after having been sorted.

For example: `proplist_to_string([{ccc, 42}, {"beta", 1.0}])` returns a bullet
list like:
```
 + beta: 1.0
 + ccc: 42
```
""".
-spec proplist_to_string( list_table:list_table() ) -> ustring().
proplist_to_string( Proplist ) ->

	% In this context, key and value known to be strings or atoms:
	Strings = [ io_lib:format( "~ts: ~ts", [ K, V ] )
						|| { K, V } <- lists:sort( Proplist ) ],

	strings_to_string( Strings ).



-doc "Returns a string describing the specified version.".
-spec version_to_string( basic_utils:any_version() ) -> ustring().
version_to_string( VersionTuple ) ->
	Elems = tuple_to_list( VersionTuple ),
	ElemCount = erlang:length( Elems ),
	ControlSeq = list_utils:duplicate( "~B", ElemCount ),
	FormatStr = flatten( list_utils:intercalate( $., ControlSeq ) ),
	io_lib:format( FormatStr, Elems ).



-doc "Returns a binary string corresponding to the specified atom.".
-spec atom_to_binary( atom() ) -> bin_string().
atom_to_binary( Atom ) ->
	% Note: options may apply, like in: erlang:atom_to_binary(X, utf8).
	string_to_binary( atom_to_string( Atom ) ).



-doc """
Returns a textual description of the specified percentage, expected to be a
float in [0,1], with the default number of digits after the decimal point.
""".
-spec percent_to_string( math_utils:percent() ) -> ustring().
percent_to_string( Value ) ->
	percent_to_string( Value, _DefaultPrecision=1 ).



-doc """
Returns a textual description of the specified percentage, expected to be a
float in `[0,1]`, with the specified number of digits after the decimal point.
""".
-spec percent_to_string( math_utils:percent(), integer() ) -> ustring().
percent_to_string( Value, Precision ) ->
	% Awful format string to determine:
	io_lib:format( "~.*f%", [ Precision, Value * 100 ] ).



-doc """
Returns a textual description of the specified (dot-based, not comma-based)
float.
""".
-spec float_to_string( float() ) -> ustring().
float_to_string( Float ) ->
	erlang:float_to_list( Float ).



-doc """
Returns a textual description of the specified (dot-based, not comma-based)
float.
""".
-spec float_to_string( float(), [ float_option() ] ) -> ustring().
float_to_string( Float, Options ) ->
	erlang:float_to_list( Float, Options ).



-doc """
Returns a textual description of the specified (dot-based, not comma-based)
number.
""".
-spec number_to_string( number() ) -> ustring().
number_to_string( I ) when is_integer( I ) ->
	erlang:integer_to_list( I );

number_to_string( F ) when is_float( F ) ->
	erlang:float_to_list( F );

number_to_string( Other ) ->
	report_not_a_number( Other ).



-doc """
Returns an exact rounded textual description of the specified distance, expected
to be expressed as a floating-point number of millimeters, which will be first
rounded to the nearest integer.

For example: for a distance of 1001.5 millimeters, returns `"1 m and 2 mm"`; for
1 000 001 millimeters, returns `"1 km and 1 mm"`.

See also `unit_utils:meters_to_string/1` for larger lengths/distances.
""".
-spec distance_to_string( any_millimeters() ) -> ustring().
distance_to_string( Millimeters ) when is_float( Millimeters ) ->
	distance_to_string( round( Millimeters ) );

distance_to_string( Millimeters ) ->

	% In millimeters:
	Centimeters = 10,
	Meters = 100 * Centimeters,
	Km = 1000*Meters,

	ListWithKm = case Millimeters div Km of

		0 ->
			[];

		KmNonNull->
			[ io_lib:format( "~B km", [ KmNonNull ] ) ]

   end,

	DistAfterKm = Millimeters rem Km,
	%io:format( "DistAfterKm = ~B.~n", [ DistAfterKm ] ),

	ListWithMeters = case DistAfterKm div Meters of

		0 ->
			ListWithKm;

		MetersNonNull->
			[ io_lib:format( "~B m", [ MetersNonNull ] ) | ListWithKm ]

	end,

	DistAfterMeters = DistAfterKm rem Meters,
	%io:format( "DistAfterMeters = ~B.~n", [ DistAfterMeters ] ),

	ListWithCentimeters = case DistAfterMeters div Centimeters of

		0 ->
			ListWithMeters;

		CentNonNull->
			[ io_lib:format( "~B cm", [ CentNonNull ] ) | ListWithMeters ]

   end,

	DistAfterCentimeters = DistAfterMeters rem Centimeters,
	%io:format( "DistAfterCentimeters = ~B.~n", [ DistAfterCentimeters ] ),

	ListWithMillimeters = case DistAfterCentimeters of

		0 ->
			ListWithCentimeters;

		AtLeastOneMillimeter ->
			 [ io_lib:format( "~B mm", [ AtLeastOneMillimeter ] )
								| ListWithCentimeters ]

	end,

	%io:format( "Unit list is: ~w.~n", [ ListWithMillimeters ] ),

	% Preparing for final display:
	case ListWithMillimeters of

		[] ->
			"0 mm";

		[ OneElement ] ->
			OneElement;

		[ Smaller | Bigger ] ->
			join( ", ", lists:reverse( Bigger ) ) ++ " and " ++ Smaller

	end.



-doc """
Returns an approximate textual description of the specified distance, expected
to be expressed as a floating-point number of millimeters, which will be first
rounded to nearest integer.

Only one unit, the most appropriate one, will be used, with up to 1 figure after
the comma.

For example: for a distance of 1000.5 millimeters, returns `"1.0m"`.
""".
-spec distance_to_short_string( any_millimeters() ) -> ustring().
distance_to_short_string( Millimeters ) when is_float( Millimeters ) ->
	distance_to_short_string( round( Millimeters ) );

% Returns an approximate textual description of the specified distance, expected
% to be expressed as an integer number of millimeters.
%
% Only one unit, the most appropriate one, will be used, with up to 1 figure
% after the comma.
%
% For example: for a distance of 1000001 millimeters, returns "1.0 km".
%
distance_to_short_string( Millimeters ) ->

	% Note: very specific limit distances could be better managed.
	%
	% For example: "999999 millimeters" is 999 m, 99 cm and 9 mm, and "1000.0
	% m", whereas we would have preferred "1 km".

	% In millimeters:
	Centimeters = 10,
	Meters = 100 * Centimeters,
	Km = 1000*Meters,

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
							io_lib:format( "~B mm", [ Millimeters ] );

						_CmNonNull ->
							io_lib:format( "~.1f cm",
										   [ Millimeters / Centimeters ] )

					end;

				 _MetersNonNull ->
					io_lib:format( "~.1f m", [ Millimeters / Meters ] )

			end;

		_KmNonNull->
			io_lib:format( "~.1f km", [ Millimeters / Km ] )

	end.



-doc """
Returns a textual description of the specified number of repetitions
(occurrences, number of times).
""".
-spec repetition_to_string( count() ) -> ustring().
repetition_to_string( _RepetitionCount=0 ) ->
	"never";

repetition_to_string( _RepetitionCount=1 ) ->
	"once";

repetition_to_string( _RepetitionCount=2 ) ->
	"twice";

repetition_to_string( _RepetitionCount=3 ) ->
	"thrice";

repetition_to_string( RepetitionCount ) ->
	text_utils:format( "~B times", [ RepetitionCount ] ).



-doc """
Returns a very synthetic textual description of the specified table, holding the
specified type of described entries.

For example: `table_to_string(MyPathTable, _EntryType="path")` may return `"no
path"`, `"a single path"`, or - for example - `"3 paths"`.
""".
-spec table_to_string( ?table(), ustring() ) -> ustring().
table_to_string( Table, EntryDesc ) ->
	case ?table:size( Table ) of

		0 ->
			"no " ++ EntryDesc;

		1 ->
			"a single " ++ EntryDesc;

		S ->
			% Only a very basic plural mark:
			text_utils:format( "~B ~tss", [ S, EntryDesc ] )

	end.



-doc "Returns the atom corresponding to the specified string-like.".
-spec string_like_to_atom( string_like() ) -> atom().
string_like_to_atom( Str ) when is_list( Str ) ->
	erlang:list_to_atom( Str );

string_like_to_atom( BinStr ) when is_binary( BinStr ) ->
	erlang:binary_to_atom( BinStr );

string_like_to_atom( AtomStr ) when is_atom( AtomStr ) ->
	AtomStr.



-doc "Returns the binary string corresponding to the specified string-like.".
string_like_to_bin_string( Str ) when is_list( Str ) ->
	erlang:list_to_binary( Str );

string_like_to_bin_string( AtomStr ) when is_atom( AtomStr ) ->
	erlang:atom_to_binary( AtomStr );

string_like_to_bin_string( BinStr ) when is_binary( BinStr ) ->
	BinStr.



-doc """
Formats the specified string as `io_lib:format/2` would do, except it returns a
flattened version of it and cannot fail (so that for example a badly formatted
log cannot crash anymore its emitter process).

Note: rely preferably on `~ts` rather than on `~s`, to avoid unexpected Unicode
inputs resulting on crashes afterwards.
""".
-spec format( format_string(), format_values() ) -> ustring().

-ifdef(exec_target_is_production).

format( FormatString, Values ) ->

	String =
		try

			io_lib:format( FormatString, Values )

		catch

			_:_ ->

				Msg = io_lib:format( "[error: badly formatted string output] "
					"Format string was '~p', values were '~ts'.~n",
					[ FormatString, basic_utils:describe_term( Values ) ] ),

				% Not wanting to be extra verbose in this mode:
				%io:format( Msg ++ "~n", [] ),

				% Useful to obtain the stacktrace of a culprit or to check for
				% silent errors:
				%
				% (note: we are in production mode here)
				%
				%throw( { badly_formatted, FormatString, Values } ),

				% Not ellipsing by default anymore (the caller thus may wrap
				% with a call to *_to_bounded_string/1 instead):
				%
				%ellipse( Msg, _HighThreshold=5000 )
				Msg

		end,

	% Using flatten/1 allows for example to have clearer string outputs in case
	% of error (at a rather low cost):
	%
	lists:flatten( String ).


-else. % exec_target_is_production


% In development mode here:
format( FormatString, Values ) ->

	%io:format( "FormatString='~ts', Values='~p'.~n",
	%           [ FormatString, Values ] ),

	String =
		try

			io_lib:format( FormatString, Values )

		catch

			_:_ ->

				VString = basic_utils:describe_term( Values ),

				Msg = "[error: badly formatted string output] "
								++ case is_string( FormatString ) of

					true ->
						case is_list( Values ) of

							true ->
								io_lib:format( "format specified as '~ts', "
									"values as ~ts~ts", [ FormatString, VString,
									interpret_faulty_format( FormatString,
															 Values ) ] );

							false ->
								io_lib:format(
									"values were not specified as a list "
									"(i.e. incorrectly as '~ts'; "
									"format was '~ts')",
									[ VString, FormatString ] )

						end;

					false ->
						io_lib:format( "format was not specified as a string "
							"(i.e. incorrectly as '~p'; values were '~ts').",
							[ FormatString, VString ] )

				end,

				% Not ellipsing by default anymore (the caller thus may wrap
				% with a call to *_to_bounded_string/1 instead):
				%
				%EllipsedMsg = ellipse( Msg, _MaxLen=2500 ),

				% If wanting to be extra verbose, duplicating message on the
				% console:
				%
				%io:format( Msg ++ "~n~n", [] ),

				% Useful to obtain the stacktrace of a culprit or to check for
				% silent errors:
				%
				% (in development mode here)
				%
				%throw( { badly_formatted, FormatString, Values } ),
				Msg

	end,

	% Using flatten/1 allows for example to have clearer string outputs in case
	% of error (at an acceptable cost):
	%
	lists:flatten( String ).


-endif. % exec_target_is_production


-doc "Module-local version of `(io_lib/text_utils):format/2`.".
-spec local_format( format_string(), format_values() ) -> ustring().
local_format( FormatString, Values ) ->
	lists:flatten( io_lib:format( FormatString, Values ) ).


-doc "Module-local version of `io:format/1`.".
-spec local_display( ustring() ) -> void().
local_display( S ) ->
	% To avoid a wrong number of arguments being detected due to ~:
	EscapedS = string:replace( _In=S, _SearchPattern="~",
							   _Replacement="\~", _Where=all),

	io:format( "~ts~n", [ EscapedS ] ).


-doc "Module-local version of `io:format/2`.".
-spec local_display( format_string(), format_values() ) -> void().
local_display( FormatString, Values ) ->
	local_display( local_format( FormatString, Values ) ).



-doc """
Interprets a faulty format command, based on respectively a format string and a
supposedly-corresponding list of values.
""".
-spec interpret_faulty_format( format_string(), format_values() ) -> ustring().
interpret_faulty_format( FormatString, Values ) ->

	%trace_utils:debug_fmt( "FormatString: ~p~nValues: ~p.",
	%                       [ FormatString, Values ] ),

	% Preferring not using 'text_utils:format/2' here, but io_lib.

	ValueCount = erlang:length( Values ),

	Diagnosis = case scan_format_string( FormatString ) of

		{ format_parsing_failed, ReasonStr } ->
			local_format( " (the format string '~ts' is invalid: ~ts)",
						  [ FormatString, ReasonStr ] );

		% Not even one control sequence, a bit unusual:
		[] ->
			% Of course not 'text_utils', to avoid any infinite recursion:
			local_format( " (no control sequence detected in format "
						  "string '~ts')", [ FormatString ] );

		ValueDescs ->
			FmtValueCount = erlang:length( ValueDescs ),

			% Counting value-based control sequences:
			case ValueCount - FmtValueCount of

				0 ->
					"; apparently the correct number of values "
					"has been specified, so the types may not all match: "
					++ match_types( ValueDescs, Values, _VCount=1 ); % ++ ".";

				% Very common case:
				1 ->
					case FmtValueCount of

						1 ->
							" (expecting a single value, got two of them)";

						_ ->
							local_format( " (expecting ~B values, got ~B, "
								"hence an extra value has been specified)",
								[ FmtValueCount, ValueCount ] )

					end;

				TooMany when TooMany > 1 ->
					local_format( " (expecting ~B values, got ~B, hence ~B "
						"extra values have been specified)",
						[ FmtValueCount, ValueCount, TooMany ] );

				% Very common case:
				-1 ->
					local_format( " (expecting ~B values, got ~B, hence an "
						"additional value ought to have been specified)",
						[ FmtValueCount, ValueCount ] );

				TooFew when TooFew < 1 ->
					local_format( " (expecting ~B values, got ~B, hence ~B "
						"additional values ought to have been specified)",
						[ FmtValueCount, ValueCount, -TooFew ] )

			end

	end,

	% To track origin (not always obvious):
	Diagnosis ++ "; corresponding stack trace was: "
		++ code_utils:interpret_shortened_stacktrace( _SkipLastElemCount=2 ).





-doc """
Scans the specified format string, returning, in case of success, a list of the
corresponding value descriptions that are expected, otherwise returns the
detected parsing error.
""".

% See also io_lib_format:collect/2 (in lib/stdlib/src/io_lib_format.erl) and
% erl_lint:check_format_string/1 (in lib/stdlib/src/erl_lint.erl).
%
-spec scan_format_string( format_string() ) -> scan_format_outcome().
scan_format_string( FormatString ) ->
	%local_display( "### Scanning format string '~ts'...", [ FormatString ] ),

	case scan_format_string( FormatString, _ValueDescs=[] ) of

		P={ format_parsing_failed, _Reason } ->
			P;

		FinalValueDescs ->
			InOrderDescs = lists:reverse( FinalValueDescs ),

			%local_display( "### ... format string '~ts' interpreted as ~w.~n",
			%               [ FormatString, InOrderDescs ] ),

			InOrderDescs

	end.



% (helper)
-spec scan_format_string( format_string(), [ value_description() ] ) ->
												scan_format_outcome().
%scan_format_string( _FormatString=[], ValueDescs ) ->
%	lists:reverse( ValueDescs );

scan_format_string( FormatStr, ValueDescs ) ->

	% A strict left-to-right parsing would be difficult, due to the F.P.PadModC
	% general form and the optional fields (refer to
	% https://www.erlang.org/doc/apps/stdlib/io#fwrite/3 for all formatting
	% options).
	%
	% So we follow here the approach of locating C first (the actual control
	% sequence, the most explicative field, to which most others are relative).
	%
	% Then we could go backward from C, yet this would be a poor choice, as then
	% a left-to-right parsing is more natural: for example, in "~.*f", "*"
	% refers to the precision (for example: 'io:format("~.*f",
	% [_DigitCountAfterComma=4, 1/3]' displays "0.3333"), not padding or
	% modifier: a format is to be understood from left to right, as using
	% directly for the first fields the values found. So, for example,
	% specifying a precision atually *requires* a prior dot, and F.P.PadModC
	% shall be interpreted as: (F)(.P)((.Pad)Mod)C (and we could have gone
	% forward-only in one pass, as io_lib_format:collect/2).

	case locate_control_seq( FormatStr ) of

		not_found ->
			% Reversed only ultimately:
			ValueDescs;

		{ CtrlSeqChar, PrevChars, NextStr } ->

			%local_display( "Control sequence: ~tc, with previous "
			%   "characters: '~ts', and next: '~ts'.",
			%   [ CtrlSeqChar, PrevChars, NextStr ] ),

			case parse_to_control_seq( CtrlSeqChar, PrevChars ) of

				P={ format_parsing_failed, _Reason } ->
					P;

				ExtraValueDescs ->
					%local_display( "Obtained for sequence '~tc': ~w.",
					%               [ CtrlSeqChar, ExtraValueDescs ] ),

					NewValueDescs = ExtraValueDescs ++ ValueDescs,
					scan_format_string( NextStr, NewValueDescs )

			end;

		P={ format_parsing_failed, _Reason } ->
			P

	end.




-doc """
Locates the C part (control sequence), in `"xx~F.P.PadModCyy"`.
""".
-spec locate_control_seq( format_string() ) ->
	'not_found' | { char(), [ char() ], ustring() } | format_parsing_error() .
locate_control_seq( FormatStr ) ->

	AllCtrlSeqChars = [ $~, $c, $f, $e, $g, $s, $w, $p, $W, $P, $B, $X, $#,
						$b, $x, $+, $n, $i ],

	locate_control_seq( FormatStr, AllCtrlSeqChars ).


% (helper)
locate_control_seq( _FormatStr=[], _AllCtrlSeqChars ) ->
	not_found;

% "~~" is not a control sequence per se:
%locate_control_seq( _FormatStr=[ $~, $~ | T ], AllCtrlSeqChars ) ->
%	locate_control_seq( T, AllCtrlSeqChars );

locate_control_seq( _FormatStr=[ $~ | T ], AllCtrlSeqChars ) ->
	% Now, in-sequence, extracting "F.P.PadModC":
	extract_control_seq( T, AllCtrlSeqChars );

% Not reached yet ( still in "xx"), hence character dropped:
locate_control_seq( _FormatStr=[ _Char | T ], AllCtrlSeqChars ) ->
	locate_control_seq( T, AllCtrlSeqChars ).



-doc """
Extracts the (post-tilde) content of the corresponding control sequence, by
accumulating all characters before the first control sequence one found (if
any).
""".
-spec extract_control_seq( [ char() ], [ char() ] ) ->
	'not_found' | { char(), [ char() ], ustring() } | format_parsing_error() .
extract_control_seq( Chars, AllCtrlSeqChars ) ->
	extract_control_seq( Chars, AllCtrlSeqChars, _Acc=[] ).


% (helper)
extract_control_seq( _Chars=[], _AllCtrlSeqChars, _Acc ) ->
	{ format_parsing_failed, "no ending control sequence found." };

extract_control_seq( _Chars=[ Char |  T ], AllCtrlSeqChars, Acc ) ->

	case lists:member( Char, AllCtrlSeqChars ) of

		true ->
			%local_display( "locate_c: returning sequence ~tc.", [ Char ] ),
			{ Char, lists:reverse( Acc ), T };

		false ->
			extract_control_seq( T, AllCtrlSeqChars, [ Char | Acc ] )

	end.



-doc """
Parses the specified characters that were after the initial tilde and before the
specified control sequence one.
""".
-spec parse_to_control_seq( char(), [ char() ] ) -> scan_format_outcome().
parse_to_control_seq( _CtrlSeqChar=$n, _PrevChars=[] ) ->
	[];

parse_to_control_seq( _CtrlSeqChar=$~, _PrevChars=[] ) ->
	[];

parse_to_control_seq( _CtrlSeqChar=$~, PrevChars ) ->
	{ format_parsing_failed, local_format( "only direct double-tilde allowed "
		"(whereas here '~ts' was found in-between); incorrect format string.",
		[ PrevChars ] ) };

parse_to_control_seq( CtrlSeqChar, PrevChars ) ->

	% First, let's track the expected type of values corresponding to the
	% specified control sequence (i.e. for C=CtrlSeqChar):
	%
	case integrate_control_sequence( CtrlSeqChar ) of

		P={ format_parsing_failed, _ReasonStr } ->
			P;

		ValueDescs ->
			% Now, knowing C=CtrlSeqChar, considering "F.P.PadMod" (or
			% ".P.PadMod", or "..PadMod", or "Mod", or "", etc.), remembering
			% the left-to-right parsing logic, i.e. for example that in "~.*f",
			% "*" is for the precision; so we have to count the "macro-fields"
			% (F, P or PadMod), whose number is the number of $. plus one:
			%
			case split( PrevChars, _Separator=$. ) of

				[ FStr, PStr, PadModStr ] ->
					case integrate_width_field( FStr, CtrlSeqChar,
												ValueDescs ) of

						P={ format_parsing_failed, _ReasonStr } ->
							P;

						FValueDescs ->
							case integrate_precision_field( PStr, CtrlSeqChar,
															FValueDescs ) of

								P={ format_parsing_failed, _ReasonStr } ->
									P;

								PValueDescs ->
									integrate_padmod_field( PadModStr,
										CtrlSeqChar, PValueDescs )

							end

					end;

				[ PStr, PadModStr ] ->
					case integrate_precision_field( PStr, CtrlSeqChar,
													ValueDescs ) of

						P={ format_parsing_failed, _ReasonStr } ->
							P;

						PValueDescs ->
							integrate_padmod_field( PadModStr, CtrlSeqChar,
													PValueDescs )

					end;

				[ ModStr ] ->
					% Not calling integrate_padmod_field/3, as we believe no
					% padding character can be specified if there was no dot
					% just before:
					%
					integrate_modifiers( ModStr, CtrlSeqChar, ValueDescs )

			end

	end.



-doc """
Returns the value descriptions implied by the specified control sequence (`$n`
and `$~` already managed).
""".
-spec integrate_control_sequence( char() ) -> scan_format_outcome().
integrate_control_sequence( _CtrlSeqChar=$c ) ->
	% A	number that is interpreted as an ASCII code:
	[ char ];

integrate_control_sequence( _CtrlSeqChar=$f ) ->
	[ float ];

integrate_control_sequence( _CtrlSeqChar=$e ) ->
	[ float ];

integrate_control_sequence( _CtrlSeqChar=$g ) ->
	[ float ];

integrate_control_sequence( _CtrlSeqChar=$s ) ->
	[ string_like ];

integrate_control_sequence( _CtrlSeqChar=$w ) ->
	[ term ];

integrate_control_sequence( _CtrlSeqChar=$p ) ->
	[ term ];

integrate_control_sequence( _CtrlSeqChar=$W ) ->
	[ pos_integer, term ];

integrate_control_sequence( _CtrlSeqChar=$P ) ->
	[ pos_integer, term ];

integrate_control_sequence( _CtrlSeqChar=$B ) ->
	[ integer ];

integrate_control_sequence( _CtrlSeqChar=$X ) ->
	[ string_like, integer ];

integrate_control_sequence( _CtrlSeqChar=$# ) ->
	[ integer ];

integrate_control_sequence( _CtrlSeqChar=$b ) ->
	[ integer ];

integrate_control_sequence( _CtrlSeqChar=$x ) ->
	[ string_like, integer ];


integrate_control_sequence( _CtrlSeqChar=$+ ) ->
	[ integer ];

integrate_control_sequence( _CtrlSeqChar=$i ) ->
	[ term ].



-doc """
Integrates any field width (the `F` field) for the specified control sequence.
""".
-spec integrate_width_field( [ char() ], char(), [ value_description() ] ) ->
									scan_format_outcome().
% we expect a (signed) integer, or $*, or nothing.
%
% No width here, that was just "~.xx", hence default width:
integrate_width_field( _FStr=[], _CtrlSeqChar, ValueDescs ) ->
	ValueDescs;

% Wildcard width:
integrate_width_field( _FStr=[ $* ], _CtrlSeqChar, ValueDescs ) ->
	[ integer | ValueDescs ];

% Width directly set:
integrate_width_field( FStr, CtrlSeqChar, ValueDescs ) ->

	%local_display( "Integrating width field '~ts'.", [ FStr ] ),

	% Negative widths allowed:
	case skip_signed_integer( FStr ) of

		P={ format_parsing_failed, _Reason } ->
			P;

		% Finished:
		_NextStr=[] ->
			ValueDescs;

		_Other ->
			{ format_parsing_failed, local_format( "invalid field width "
				"specification for control character ~tc: '~ts'.",
				[ CtrlSeqChar, FStr ] ) }

	end.



-doc """
Integrates any precision (the `P` field) for the specified control sequence.
""".
-spec integrate_precision_field( [ char() ], char(),
            [ value_description() ] ) -> scan_format_outcome().
% Meaning of 'within' unclear; so we expect a non-negative integer, or $*, or
% nothing.
%
% No precision here, that was just "~xx..yy", hence default precision:
integrate_precision_field( _PStr=[], _CtrlSeqChar, ValueDescs ) ->
	ValueDescs;

% Wildcard precision:
integrate_precision_field( _PStr=[ $* ], _CtrlSeqChar, ValueDescs ) ->
	[ integer | ValueDescs ];

% Precision directly set:
integrate_precision_field( PStr, CtrlSeqChar, ValueDescs ) ->

	case skip_non_negative_integer( PStr ) of

		P={ format_parsing_failed, _Reason } ->
			P;

		% Finished:
		_NextStr=[] ->
			ValueDescs;

		_Other ->
			{ format_parsing_failed, local_format( "invalid field precision "
				"specification for control character ~tc: '~ts'.",
				[ CtrlSeqChar, PStr ] ) }

	end.




-doc """
Integrates any padding character and/or modifiers (the `PadMod` field) for the
specified control sequence.
""".
-spec integrate_padmod_field( [ char() ], char(), [ value_description() ] ) ->
									scan_format_outcome().
% In ~F.P.PadModC, the PadMod field can be: "", Pad, Mod, PadMod with multiple
% modifiers.
%
% As shown by io:format("xx~10..ts", ["hello"]) displaying "xxttttthello", $t is
% interpreted here as a padding character - rather than as a modifier;
% nevertheless io:format("xx~..ts", ["hâte"]) displays "hâte" whereas
% io:format("xx~..s", ["hâte"]) triggers bad_arg: $t is apparently interpreted
% there as a modifier - but io:format("xx~10..ts", ["hâte"]) shows
% "xxtttttthâte", so $t must be here a padding character, a side-effect being to
% switch to Unicode (as if it was also a modifier).
%
% Looking at io_lib_format:pad_char/2, the rule seems simple: any second
% $. means that the next character (possibly $*) designates the padding one.
%
% Previously we thought that a second $. did not necessarily implied a pad was
% set; in that case that pad would have to be prioritary in all cases over mod
% (otherwise the rule would be dangerous knowing that modifiers may be added in
% the future), so the default pad ($ , i.e. "space") must be specified if
% wanting a modifier to be taken into account then.
%
% Anyway:
integrate_padmod_field( _PadModStr=[], _CtrlSeqChar, ValueDescs ) ->
	% None of them:
	ValueDescs;

% Hence a padding character is expected, here as a wildcard::
integrate_padmod_field( _PadModStr=[ _Pad=$* | Mods ], CtrlSeqChar,
						ValueDescs ) ->
	integrate_modifiers( Mods, CtrlSeqChar, [ char | ValueDescs ] );

% A direct padding character, which we drop:
integrate_padmod_field( _PadModStr=[ _Pad | Mods ], CtrlSeqChar, ValueDescs ) ->
	integrate_modifiers( Mods, CtrlSeqChar, ValueDescs ).



-doc """
Takes into account the modifiers (`"Mod"`).
""".
-spec integrate_modifiers( [ char() ], char(), [ value_description() ] ) ->
									scan_format_outcome().
integrate_modifiers( _Mods=[], _CtrlSeqChar, ValueDescs ) ->
	ValueDescs;

integrate_modifiers( _Mods=[ Mod | T ], CtrlSeqChar, ValueDescs ) ->
	case interpret_as_modifier( Mod, CtrlSeqChar ) of

		P= { format_parsing_failed, _Reason } ->
			P;

		% No value to add:
		undefined ->
			integrate_modifiers( T, CtrlSeqChar, ValueDescs );

		VDesc ->
			integrate_modifiers( T, CtrlSeqChar, [ VDesc | ValueDescs ] )

	end.




-doc """
Interprets the specified character as a possible modifier, in the context of
specified control sequence character.
""".
-spec interpret_as_modifier( char(), char() ) -> scan_format_outcome().
% Apparently $t may apply to all control sequence chars (s, c, etc.):
interpret_as_modifier( _Mod=$t, _CtrlSeqChar ) ->
	% No impact on value count / types:
	undefined;

interpret_as_modifier( _Mod=$l, CtrlSeqChar )
					   when CtrlSeqChar =:= $p; CtrlSeqChar =:= $P ->
	undefined;

interpret_as_modifier( _Mod=$k, CtrlSeqChar ) ->
	case lists:member( CtrlSeqChar, [ $p, $P, $w, $W ] ) of

		true ->
			undefined;

		false ->
			{ format_parsing_failed, local_format( "modifier 'k' does not "
				"apply to control sequence '~tc'.", [ CtrlSeqChar ] ) }

	end;

interpret_as_modifier( _Mod=$K, CtrlSeqChar ) ->
	case lists:member( CtrlSeqChar, [ $p, $P, $w, $W ] ) of

		true ->
			% Expects another specific value, see
			% https://www.erlang.org/doc/apps/stdlib/maps#t:iterator_order/0:
			%
			atom_or_function;

		false ->
			{ format_parsing_failed, local_format( "modifier 'K' does not "
				"apply to control sequence '~tc'.", [ CtrlSeqChar ] ) }

	end;

interpret_as_modifier( Mod, CtrlSeqChar ) ->
	{ format_parsing_failed, local_format( "invalid modifier '~tc' "
		"(found in the context of control sequence '~tc').",
		[ Mod, CtrlSeqChar ] ) }.



-doc """
Skips a non-negative integer.

For example `"31xx"`, returning then `"xx"`.
""".
-spec skip_non_negative_integer( [ char() ] ) ->
									[ char() ] | format_parsing_error().
% At least one number expected:
skip_non_negative_integer( [ C | T ] ) when C >= $0, C =< $9 ->
	skip_all_numbers( T );

% Either not a number or no character (empty string):
skip_non_negative_integer( Other ) ->
	{ format_parsing_failed, local_format( "Failed to skip non-negative "
		"integer from '~ts'.", [ Other ] ) }.


-doc "Skipping all numbers, if any (and only them).".
-spec skip_all_numbers( [ char() ] ) -> [ char() ].
skip_all_numbers( [ C | T ] ) when C >= $0, C =< $9 ->
	skip_all_numbers( T );

skip_all_numbers( NextStr ) ->
	NextStr.


-doc """
Skips a (signed) integer.

For example `"31xx"` or `"-144xx"`, returning then, in both cases, `"xx"`.
""".
-spec skip_signed_integer( [ char() ] ) -> [ char() ] | format_parsing_error().
% At least one number expected, possibly after a $-:
skip_signed_integer( _RevPrevChars=[ $-, C | T ] ) when C >= $0, C =< $9 ->
	skip_all_numbers( T );

skip_signed_integer( _RevPrevChars=[ C | T ] ) when C >= $0, C =< $9 ->
	skip_all_numbers( T );

% Either not a number or no character (empty string):
skip_signed_integer( Other ) ->
	{ format_parsing_failed, local_format( "Failed to skip signed "
		"integer from '~ts'.", [ Other ] ) }.




-doc """
Formats the specified values in a fail-safe manner; returns a string meant to
correspond as much as possible to these values (rather than diagnosing any
problem detected as `format/2`); cannot fail (so that for example a badly
formatted log cannot crash anymore its emitter process).

Typically useful as a failsafe solution, should a previous format string be
detected as faulty (e.g. containing `~s` where `~ts` should have been used).
""".
-spec format_failsafe( format_values() ) -> ustring().
format_failsafe( Values ) ->
	format_failsafe( Values, Values, _AccFmtStr=[] ).


% (helper)
format_failsafe( _Vs=[], Values, AccFmtStr ) ->
	FmtStr = lists:flatten( lists:reverse( AccFmtStr ) ),
	io_lib:format( FmtStr, Values );

format_failsafe( _Vs=[ V | T ], Values, AccFmtStr ) ->
	VFmt = case is_string( V ) of

		true ->
			"~ts";

		false ->
			"~p"

	end,
	format_failsafe( T, Values, [ VFmt | AccFmtStr ] ).



-doc """
Formats the specified string as `io_lib:format/2` would do, except it returns a
flattened, ellipsed version of it and cannot fail (so that for example a badly
formatted log cannot crash anymore its emitter process).

Tries to never crash.

Note: rely preferably on `~ts` rather than on `~s`, to avoid unexpected Unicode
inputs resulting on crashes afterwards.
""".
-spec format_ellipsed( format_string(), format_values() ) -> ustring().
format_ellipsed( FormatString, Values ) ->
	ellipse( format( FormatString, Values ), _MaxLen=500 ).



-doc """
Formats the specified string as `io_lib:format/2` would do, except it returns a
flattened, ellipsed (based on the specified length) version of it, and cannot
fail (so that for example a badly formatted log cannot crash anymore its emitter
process).

Tries to never crash.

Note: rely preferably on `~ts` rather than on `~s`, to avoid unexpected Unicode
inputs resulting on crashes afterwards.
""".
-spec format_ellipsed( format_string(), format_values(), length() ) ->
							ustring().
format_ellipsed( FormatString, Values, MaxLen ) ->
	ellipse( format( FormatString, Values ), MaxLen ).



-doc """
Compares the value types (typically emanating from a format string based on
control sequences) to the types of the specified, numbered values (expected to
correspond), and detects mismatches.

Note: beware to the output error messages comprising `~XXX` not being afterwards
interpreted as control sequences; we finally gave up including a `~` character
in the output sequence, as it has to be escaped a number of times that depended
on how many `io*:format/*` it was to go through (fragile at best).
""".
-spec match_types( [ value_description() ], format_values(), count() ) ->
													ustring().
match_types( _TypeDescs=[], _Values=[], _Count ) ->
	"yet no mismatch detected";

match_types( _TypeDescs=[ TypeDesc | Ts ], _Values=[ V | Tv ], VCount ) ->

	case type_utils:is_value_matching( TypeDesc, V ) of

		true ->
			% No: trace_utils:debug_fmt
			%io:format( "[debug] The type of value #~B (i.e. '~ts'), which "
			% "may be described as ~ts, matches the one specified by the "
			% "control sequence, ~ts.~n",
			% [ VCount, basic_utils:describe_term( V ),
			%   type_utils:get_type_of( V ), TypeDesc ] ),
			match_types( Ts, Tv, VCount+1 );

		false ->
			io_lib:format( "type mismatch for value #~B (i.e. '~ts'), "
				"which may be described as ~ts and does match "
				"the one specified by the control sequence, ~ts",
				[ VCount, basic_utils:describe_term( V ),
				  type_utils:get_type_of( V ), TypeDesc  ] )

	end.



-doc """
Formats the specified text as a comment, based on the default character denoting
comments (namely `%`), and for a line width of 80 characters.
""".
-spec format_as_comment( ustring() ) -> ustring().
format_as_comment( Text ) ->
	format_as_comment( Text, _CommentChar=$% ).



-doc """
Formats the specified format string with values as a comment, based on the
default character denoting comments (namely `%`), and for a line width of 80
characters.
""".
-spec format_as_comment( format_string(), format_values() ) -> ustring();
					   ( ustring(), char() ) -> ustring().
format_as_comment( FormatString, Values ) when is_list( Values ) ->
	Text = format( FormatString, Values ),
	format_as_comment( Text );

% Formats the specified text as a comment, based on the specified character
% denoting comments, and for a line width of 80 characters.
%
format_as_comment( Text, CommentChar ) ->
	format_as_comment( Text, CommentChar, _LineWidth=80 ).



-doc """
Formats the specified text as a comment, based on the specified character
denoting comments, and for the specified line width (in characters).
""".
-spec format_as_comment( any_string(), char(), width() ) -> ustring().
format_as_comment( Text, CommentChar, LineWidth ) when is_binary( Text ) ->
	format_as_comment( binary_to_string( Text ), CommentChar, LineWidth );

format_as_comment( Text, CommentChar, LineWidth ) when is_list( Text ) ->

	% To account for the (for example) "% " prefix:
	RemainWidth = LineWidth - 2,

	Elems = split_at_whitespaces( Text ),

	format_as_comment_helper( Elems, CommentChar, RemainWidth, _AccLines=[],
							  _AccLine=[], RemainWidth ).



-doc """
Formats the specified format string with values as a comment, based on the
specified character denoting comments, and for the specified line width (in
characters).
""".
-spec format_as_comment( format_string(), format_values(), char(), width() ) ->
															ustring().
format_as_comment( FormatString, Values, CommentChar, LineWidth ) ->
	Text = format( FormatString, Values ),
	format_as_comment( Text, CommentChar, LineWidth ).


% (helper)
format_as_comment_helper( _Text=[], CommentChar, _LineWidth, AccLines, AccLine,
						  _RemainWidth ) ->
	join( _Separator=$\n, lists:reverse(
			[ get_formatted_line( CommentChar, AccLine ) | AccLines ] ) );

format_as_comment_helper( _Text=[ Word | T ], CommentChar, LineWidth, AccLines,
						  AccLine, RemainWidth ) ->

	WordWidth = erlang:length( Word ),

	case WordWidth >= RemainWidth of

		true ->
			%trace_utils:debug_fmt( "Word '~ts' too long, hence to be put on "
			%                       "next line.", [ Word ] ),
			NewAccLines =
				[ get_formatted_line( CommentChar, AccLine ) | AccLines ],

			format_as_comment_helper( T, CommentChar, LineWidth,
				NewAccLines, _AccLine=[ Word ],
				_RemainWidth=LineWidth-WordWidth );

		false ->
			%trace_utils:debug_fmt( "Word '~ts' still fits on the current "
			%   "line.", [ Word ] ),
			format_as_comment_helper( T, CommentChar, LineWidth,
				% Decremented width to account for the space *before* this word:
				AccLines, [ Word | AccLine ], RemainWidth - WordWidth - 1 )

	end.


% (helper)
get_formatted_line( CommentChar, Line ) ->
	[ CommentChar, $ ] ++ join( _Separator=$ , lists:reverse( Line ) ).



-doc """
Formats the specified string as a (flattened) binary, as `io_lib:format/2` would
do, except it cannot fail (so that for example a badly formatted log cannot
crash anymore its emitter process).

Note: rely preferably on `~ts` rather than on `~s`, to avoid unexpected Unicode
inputs resulting on crashes afterwards.
""".
-spec bin_format( format_string(), format_values() ) -> bin_string().
bin_format( FormatString, Values ) ->

	String = format( FormatString, Values ),

	% No flattening needed here:
	%erlang:list_to_binary( String ).
	to_unicode_binary( String ).



-doc """
Formats the specified string as an atom; cannot fail (so that for example a
badly formatted log cannot crash anymore its emitter process).

Note: rely preferably on `~ts` rather than on `~s`, to avoid unexpected Unicode
inputs resulting on crashes afterwards.
""".
-spec atom_format( format_string(), format_values() ) -> atom().
atom_format( FormatStr, FormatValues ) ->
	string_to_atom( format( FormatStr, FormatValues ) ).



-doc """
Useful to catch silly mistakes involving an extra comma in a format string.
""".
-spec format( term(), term(), term() ) -> no_return().
format( A, B, C ) ->

	trace_utils:error_fmt( "Call to non-existing function text_utils:format/3; "
		"extra comma in format string? Parameters were: ~ts",
		[ strings_to_enumerated_string( [
			basic_utils:describe_term( T ) || T <- [ A, B, C ] ] ) ] ),

	throw( { faulty_format_call, { A, B, C } } ).



% Note: we deemed safer to consider for ensure_*/1 that atoms shall not be
% directly seen as possible inputs.



-doc """
Returns a (plain) string version of the specified text-like parameter.

Never fails because of any transcoding involved.

Note: using such functions may be a bad practice, as it may lead to losing the
awareness of the types of the variables that are handled. We now output warning
traces whenever the specified element happens not to be a string-like
element. It is however convenient to define functions whose string parameters
may be of any possible type (plain or binary).
""".
-spec ensure_string( any_string() ) -> ustring().
ensure_string( String ) ->
	ensure_string( String, _CanFailDueToTranscoding=false ).



-doc """
Returns a (plain) string version of the specified text-like parameter.

CanFailDueToTranscoding tells whether, should a transcoding fail, this function
is allowed to fail in turn.

Note: using such functions may be a bad practice, as it may lead to losing the
awareness of the types of the variables that are handled. We now output warning
traces whenever the specified element happens not to be a string-like
element. It is however convenient to define functions whose string parameters
may be of any possible type (plain or binary).
""".
-spec ensure_string( any_string(), boolean() ) -> ustring().
ensure_string( String, _CanFailDueToTranscoding ) when is_list( String ) ->
	String;

ensure_string( BinString, CanFailDueToTranscoding )
											when is_binary( BinString ) ->
	binary_to_string( BinString, CanFailDueToTranscoding );

%ensure_string( Int, _CanFailDueToTranscoding ) when is_integer( Int ) ->
%   trace_utils:warning_fmt( "Implicit conversion of integer (here '~B') "
%       "to plain string is now discouraged. "
%       "Use text_utils:integer_to_string/1 instead.", [ Int ] ),
%   integer_to_list( Int );

%ensure_string( F, _CanFailDueToTranscoding ) when is_float( F ) ->
%   trace_utils:warning_fmt( "Implicit conversion of float (here '~f') "
%       "to plain string is now discouraged. "
%       "Use text_utils:float_to_string/1 instead.", [ F ] ),
%   float_to_list( F );

ensure_string( U, _CanFailDueToTranscoding ) ->
	throw( { invalid_value, U } ).



-doc """
Returns a list of (plain) string versions of the string-like elements of the
specified list.

Never fails because of any transcoding involved.

Note: using such functions may be a bad practice, as it may lead to losing the
awareness of the types of the variables that are handled. We now output warning
traces whenever the specified element happens not to be a string-like
element. It is however convenient to define functions whose string parameters
may be of any possible type (plain or binary).
""".
-spec ensure_strings( [ term() ] ) -> [ ustring() ].
ensure_strings( Elems ) ->
	ensure_strings( Elems, _CanFailDueToTranscoding=false ).



-doc """
Returns a list of (plain) string versions of the string-like elements of the
specified list.

`CanFailDueToTranscoding` tells whether, should a transcoding fail, this
function is allowed to fail in turn.

Note: using such functions may be a bad practice, as it may lead to losing the
awareness of the types of the variables that are handled. We now output warning
traces whenever the specified element happens not to be a string-like
element. It is however convenient to define functions whose string parameters
may be of any possible type (plain or binary).
""".
-spec ensure_strings( [ term() ], boolean() ) -> [ ustring() ].
ensure_strings( Elems, CanFailDueToTranscoding ) ->
	[ ensure_string( E, CanFailDueToTranscoding ) || E <- Elems ].



-doc """
Returns a binary string version of the specified text-like parameter (binary or
plain string).

Never fails because of any transcoding involved.

Note: using such functions may be a bad practice, as it may lead to losing the
awareness of the types of the variables that are handled. It is however
convenient to define functions whose string parameters may be of any possible
type (plain or binary).
""".
-spec ensure_binary( any_string() ) -> bin_string().
ensure_binary( AnyString ) ->
	ensure_binary( AnyString, _CanFailDueToTranscoding=false ).



-doc """
Returns a binary string version of the specified text-like parameter (binary or
plain string).

`CanFailDueToTranscoding` tells whether, should a transcoding fail, this
function is allowed to fail in turn.

Note: using such functions may be a bad practice, as it may lead to losing the
awareness of the types of the variables that are handled. It is however
convenient to define functions whose string parameters may be of any possible
type (plain or binary).
""".
-spec ensure_binary( any_string(), boolean() ) -> bin_string().
ensure_binary( BinString, _CanFailDueToTranscoding )
										when is_binary( BinString ) ->
	BinString;

ensure_binary( String, CanFailDueToTranscoding ) when is_list( String ) ->
	string_to_binary( String, CanFailDueToTranscoding );

ensure_binary( String, _CanFailDueToTranscoding ) ->
	throw( { invalid_value, String } ).



-doc """
Returns a binary string version of the specified text-like parameter (binary or
plain string), if any (otherwise leave it to `undefined`).
""".
-spec ensure_maybe_binary( option( any_string() ) ) -> option( bin_string() ).
ensure_maybe_binary( undefined ) ->
	undefined;

ensure_maybe_binary( AnyString ) ->
	ensure_binary( AnyString ).



-doc """
Returns a list of binary string versions of the string-like elements of the
specified list.

Never fails because of any transcoding involved.

Note: using such functions may be a bad practice, as it may lead to losing the
awareness of the types of the variables that are handled. It is however
convenient to define functions whose string parameters may be of any possible
type (plain or binary).
""".
-spec ensure_binaries( [ term() ] ) -> [ bin_string() ].
ensure_binaries( Elems ) ->
	ensure_binaries( Elems, _CanFailDueToTranscoding=false ).



-doc """
Returns a list of binary string versions of the string-like elements of the
specified list.

`CanFailDueToTranscoding` tells whether, should a transcoding fail, this
function is allowed to fail in turn.

Note: using such functions may be a bad practice, as it may lead to losing the
awareness of the types of the variables that are handled. It is however
convenient to define functions whose string parameters may be of any possible
type (plain or binary).
""".
-spec ensure_binaries( [ term() ], boolean() ) -> [ bin_string() ].
ensure_binaries( Elems, CanFailDueToTranscoding ) ->
	[ ensure_binary( E, CanFailDueToTranscoding ) || E <- Elems ].



-doc """
Returns the lexicographic distance between the two specified strings, that is
the minimal number of single-character changes in order to transform one string
into the other one.

The strings are equal iff returns zero.

Directly inspired from
[https://rosettacode.org/wiki/Levenshtein_distance#Erlang] and
[https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Erlang].

See also: <https://en.wikipedia.org/wiki/Levenshtein_distance
""".
%-spec get_lexicographic_distance_variant( ustring(), ustring() ) -> distance().

% This basic implementation is correct, yet way too inefficient:
%get_lexicographic_distance_variant( FirstString, _SecondString=[] ) ->
%   erlang:length( FirstString );

%get_lexicographic_distance_variant( _FirstString=[], SecondString ) ->
%   erlang:length( SecondString );

%get_lexicographic_distance_variant( _FirstString=[ H | T1 ],
%                                    _SecondString=[ H | T2 ] ) ->
%   get_lexicographic_distance_variant( T1, T2 );

%get_lexicographic_distance_variant( FirstString=[ _H1 | T1 ],
%                                    SecondString=[ _H2 | T2 ] ) ->
%   1 + lists:min( [ get_lexicographic_distance_variant( FirstString, T2 ),
%                    get_lexicographic_distance_variant( T1, SecondString ),
%                    get_lexicographic_distance_variant( T1, T2 ) ] ).
%
% Significantly more efficient version, using memoization:
-spec get_lexicographic_distance( ustring(), ustring() ) -> distance().
get_lexicographic_distance( FirstString, SecondString ) ->

	{ Distance, _NewAccTable } = get_lexicographic_distance( FirstString,
		SecondString, _AccTable=?table:new() ),

	Distance.


% Actual helper:
get_lexicographic_distance( _FirstString=[], SecondString, AccTable ) ->
	Len = erlang:length( SecondString ),
	NewTable = ?table:add_entry( _K={ [], SecondString }, _V=Len, AccTable ),
	{ Len, NewTable };

get_lexicographic_distance( FirstString, _SecondString=[], AccTable ) ->
	Len = erlang:length( FirstString ),
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



-doc """
Returns the longest prefix that is common to all of the specified strings, and a
list of the specified strings with this prefix removed, in the same order.

See also `file_utils:get_longest_common_path/1`.
""".
-spec get_longest_common_prefix( [ ustring() ] ) ->
										{ ustring(), [ ustring() ] }.
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



-doc """
Returns a string, based on the specified one and guaranteed to be different from
all the other specified ones.

For example useful to generate non-clashing names, like in:
```
"Hello" = text_utils:get_unique_string( "Hello", [] ),
"Hello2" = text_utils:get_unique_string( "Hello", ["Hello","Goodbye"] )
```
""".
-spec get_unique_string( ustring(), [ ustring() ] ) -> ustring().
get_unique_string( BaseStr, AllStrs ) ->
	case lists:member( BaseStr, AllStrs ) of

		false ->
			BaseStr;

		true ->
			get_uniq_helper( lists:reverse( BaseStr ), AllStrs )

	end.


% Skip first any already trailing final numbers of the original string,
% provided a prefix remains:
%
get_uniq_helper( _RevBaseStr=[ C | T ], AllStrs )
								when $0 =< C, C =< $9, T /= [] ->
	get_uniq_helper( T, AllStrs );

% Prefix is BaseStr without any number-based suffix:
get_uniq_helper( RevPrefix, AllStrs ) ->
	Prefix = [ FirstChar | _ ] = lists:reverse( RevPrefix ),
	SameStartStrs = [ S || S <- AllStrs, hd( S ) =:= FirstChar ],
	% Add a trailing space if inner spaces are already used:
	SpacedPrefix = case lists:member( $ , Prefix )
					   andalso hd( RevPrefix ) =/= $ of

		true ->
			Prefix ++ " ";

		false ->
			Prefix

	end,
	suffix_uniq_helper( SpacedPrefix, _Count=2,
						set_utils:new( SameStartStrs ) ).


% Find the first relevant number for uniqueness:
suffix_uniq_helper( Prefix, Count, Strs ) ->
	CandidateStr = Prefix ++ integer_to_string( Count ),
	case set_utils:member( CandidateStr, Strs ) of

		true ->
			suffix_uniq_helper( Prefix, Count+1, Strs );

		false ->
			CandidateStr

	end.



% For plain string, just use the length/1 built-in function.



-doc """
Returns, if possible, the length of the specified string-like argument,
otherwise returns `undefined`.

Never fails, but thus may report only indicative lengths (where
`string:length/1` would have thrown a `badarg` exception, typically because of
an inconsistent encoding).
""".
-spec safe_length( unicode_data() ) -> option( length() ).
safe_length( PseudoStr ) ->
	try string:length( PseudoStr ) of

		L ->
			L

	catch _:_ ->
		undefined

	end.



-doc """
Returns the length (precisely, the number of grapheme clusters) of the specified
any-string.
""".
-spec length( any_string() ) -> length().
length( AnyString ) ->
	string:length( AnyString ).



-doc """
Converts a plain (list-based) string into a binary.

Never fails because of any transcoding involved.
""".
-spec string_to_binary( ustring() ) -> bin_string().
string_to_binary( String ) ->
	string_to_binary( String, _CanFailDueToTranscoding=false ).



-doc """
Converts a plain (list-based) string into a binary.

`CanFailDueToTranscoding` tells whether, should a transcoding fail, this
function is allowed to fail in turn.
""".
-spec string_to_binary( ustring(), boolean() ) -> bin_string().
string_to_binary( String, CanFailDueToTranscoding ) when is_list( String ) ->

	%try
	%
	%   % No specific encoding needed:
	%   %Bin = erlang:list_to_binary( String ),
	%
	%   %io:format( "String '~ts' converted to binary '~ts'.",
	%   %           [ String, Bin ] ),
	%
	%   Bin
	%
	%catch Class:Exception ->
	%
	%   % For example: might be triggered if String=[8364] ('euro' character),
	%   % possibly % if being fed with Unicode string.
	%   %

	%   throw( { invalid_string, String, Class, Exception } )
	%
	%end;

	% Yes, encodings must be managed:
	to_unicode_binary( String, CanFailDueToTranscoding );

string_to_binary( Other, _CanFailDueToTranscoding ) ->
	report_not_a_string( Other ).



-doc """
Converts a plain (list-based) maybe-string into a binary. Returns `undefined` if
the argument string is itself undefined.

`CanFailDueToTranscoding` tells whether, should a transcoding fail, this
function is allowed to fail in turn.
""".
-spec maybe_string_to_binary( option( ustring() ) ) -> option( bin_string() ).
maybe_string_to_binary( _MaybeString=undefined ) ->
	undefined;

maybe_string_to_binary( MaybeString ) ->
	string_to_binary( MaybeString ).



-doc """
Converts a Unicode binary into a plain (list-based) string.

Use `binary_to_list/1` for other encodings like ISO-8859.

Never fails because of any transcoding involved.
""".
-spec binary_to_string( bin_string() ) -> ustring().
binary_to_string( Binary ) when is_binary( Binary ) ->
	binary_to_string( Binary, _CanFailDueToTranscoding=false ).



-doc """
Converts a Unicode binary into a plain (list-based) string.

Use `binary_to_list/1` for other encodings like ISO-8859.

`CanFailDueToTranscoding` tells whether, should a transcoding fail, this
function is allowed to fail in turn.
""".
binary_to_string( Binary, CanFailDueToTranscoding ) when is_binary( Binary ) ->
	%erlang:binary_to_list( Binary );
	to_unicode_list( Binary, CanFailDueToTranscoding );

binary_to_string( Other, _CanFailDueToTranscoding ) ->
	report_not_a_binary_string( Other ).



-doc """
Converts a list of plain (list-based) strings into a list of binaries.

Order of items remains unaffected.
""".
-spec strings_to_binaries( [ ustring() ] ) -> [ bin_string() ].
strings_to_binaries( StringList ) ->
	% Order must be preserved:
	[ string_to_binary( S ) || S <- StringList ].



-doc """
Converts a list of binaries into list of plain (list-based) strings.

Order of items remains unaffected.
""".
-spec binaries_to_strings( [ bin_string() ] ) -> [ ustring() ].
binaries_to_strings( BinaryList ) ->

	%trace_utils:debug_fmt( "binaries_to_strings: ~p", [ BinaryList ] ),

	% Order must be preserved:
	%[ erlang:binary_to_list( B ) || B <- BinaryList ].
	[ %try
	  %
	  %    erlang:binary_to_list( B )
	  %
	  %catch _:E ->
	  %
	  %   throw( { binary_conversion_failed, E, B } )
	  %
	  %end

	  to_unicode_list( B ) || B <- BinaryList ].
	  %lists:flatten( io_lib:format( "~ts", [ BinStr ] ) )
	  %    || BinStr <- BinaryList ].



-doc """
Returns an integer that corresponds to the specified text.

Throws an exception if the conversion failed.
""".
-spec string_to_integer( ustring() ) -> integer().
string_to_integer( String ) ->

	try list_to_integer( String ) of

		I ->
			I

	catch

		error:badarg ->
			throw( { integer_conversion_failed, String } )

	end.



-doc """
Returns an integer that corresponds to the specified text (expected to rely on
our usual base 10).

Returns the `undefined` atom if the conversion failed.
""".
-spec try_string_to_integer( ustring() ) -> option( integer() ).
try_string_to_integer( String ) ->
	try_string_to_integer( String, _Base=10 ).



-doc """
Returns an integer that corresponds to the specified text, expected to rely on
the specified base.

Returns the `undefined` atom if the conversion failed.
""".
-spec try_string_to_integer( ustring(), 2..36 ) -> option( integer() ).
try_string_to_integer( String, Base ) when is_list( String ) ->
	try list_to_integer( String, Base ) of

		I ->
			I

	catch

		error:badarg ->
			undefined

	end;

try_string_to_integer( Other, _Base ) ->
	report_not_a_string( Other ).



-doc """
Returns a float that corresponds to the specified text, not depending on its
being defined as an integer or as a float.

Throws an exception if the conversion failed.
""".
-spec string_to_float( ustring() ) -> float().
string_to_float( String ) ->

	case try_string_to_float( String ) of

		undefined ->
			throw( { float_conversion_failed, String } );

		F ->
			F

	end.



-doc """
Returns a float that corresponds to the specified text, not depending on its
being defined as an integer or as a float.

Returns the `undefined` atom if the conversion failed.
""".
-spec try_string_to_float( ustring() ) -> option( float() ).
try_string_to_float( String ) when is_list( String ) ->

	% Erlang is very picky (too much?) when interpreting floats-as-a-string: if
	% there is an exponent, it shall be 'e' (preferably that 'E' which is
	% nevertheless tolerated), and the mantissa must be a floating-point number
	% (hence with a point, such as 3.0e2, not 3e2) and at least one figure must
	% exist after the point (e.g. 1.0e2 is accepted, 1.e2 not). Moreover the
	% decimal mark must be '.' (e.g. not ',').

	% We overcome all these limitations here, so that for example -1,2E-4, 40E2
	% and 1,E3 are accepted and interpreted correctly.

	% Indeed, 'list_to_float("1e-4")' will raise badarg, whereas
	% 'list_to_float("1.0e-4")' will be accepted.
	%
	% So: if there is no dot on the left of a 'e' or a 'E', add ".0".
	% Moreover, "1.E-4" is also rejected, it must be fixed as well.

	% See also: wings_util:string_to_float/1.

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

				% Here there is a dot, yet there is no number afterward (e.g.
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

	end;

% An error (not 'undefined'):
try_string_to_float( Other ) ->
	report_not_a_string( Other ).



-doc """
Converts the specified plain string into an atom.

Note that only a bounded number of atoms should be created that way, lest the
atom table gets saturated.
""".
-spec string_to_atom( ustring() ) -> atom().
string_to_atom( String ) ->
	try

		erlang:list_to_atom( String )

	catch

		error:badarg ->
			report_not_a_string( String )

	end.



-doc """
Returns a textual representation of the specified terms, as a list of their
user-friendly (that is based on `~p`) default representation.
""".
-spec terms_to_string( [ term() ] ) -> ustring().
terms_to_string( Terms ) ->
	strings_to_string( [ format( "~p", [ T ] ) || T <- Terms ] ).



-doc """
Returns a textual representation of the specified terms, as a list of their
user-friendly (that is based on `~p`) default representation, for the specified
indentation.
""".
-spec terms_to_string( [ term() ], indentation_level_or_bullet()  ) ->
											ustring().
terms_to_string( Terms, IndentationOrBullet ) ->
	strings_to_string( [ format( "~p", [ T ] ) || T <- Terms ],
					   IndentationOrBullet ).



-doc """
Returns a textual representation of the specified terms, as an enumerated list
of their user-friendly (that is based on `~p`) default representation.
""".
-spec terms_to_enumerated_string( [ term() ] ) -> ustring().
terms_to_enumerated_string( Terms ) ->
	strings_to_enumerated_string( [ format( "~p", [ T ] ) || T <- Terms ] ).



-doc """
Returns a textual representation of the specified terms, as a listed
representation of their user-friendly (that is based on `~p`) default
representation.
""".
-spec terms_to_listed_string( [ term() ] ) -> ustring().
terms_to_listed_string( Terms ) ->
	strings_to_listed_string( [ format( "~p", [ T ] ) || T <- Terms ] ).



-doc """
Converts the specified binary string into a corresponding atom.

Note that a bounded number of atoms should be created that way, lest the atom
table gets saturated.
""".
-spec binary_to_atom( bin_string() ) -> atom().
binary_to_atom( BinString ) ->
	String = binary_to_string( BinString ),
	string_to_atom( String ).



-doc "Converts the specified binary string into a corresponding integer.".
-spec binary_to_integer( bin_string() ) -> integer().
binary_to_integer( BinString ) ->
	String = binary_to_string( BinString ),
	string_to_integer( String ).



-doc "Converts the specified binary string into a corresponding float.".
-spec binary_to_float( bin_string() ) -> float().
binary_to_float( BinString ) ->
	String = binary_to_string( BinString ),
	string_to_float( String ).



-doc """
Capitalises the specified string, ensuring that its first letter is a capital
one, uppercasing it if necessary.
""".
-spec uppercase_initial_letter( ustring() ) -> ustring().
uppercase_initial_letter( _Letters=[] ) ->
	[];

uppercase_initial_letter( _Letters=[ First | Others ] ) ->
	% More reliable to use First-$a+$A if $a =< First, First =< $z:
	[ string:to_upper( First ) | Others ].



-doc """
Sets the specified string to lowercase, that is downcase it (as a whole).
""".
-spec to_lowercase( ustring() ) -> ustring();
				  ( bin_string() ) -> bin_string().
to_lowercase( String ) when is_list( String ) ->
	string:to_lower( String );

to_lowercase( BinString ) when is_binary( BinString ) ->
	Str = binary_to_string( BinString ),
	LowStr = string:to_lower( Str ),
	string_to_binary( LowStr ).



-doc "Sets the specified string to uppercase.".
-spec to_uppercase( ustring() ) -> ustring();
				  ( bin_string() ) -> bin_string().
to_uppercase( String ) when is_list( String ) ->
	string:to_upper( String );

to_uppercase( BinString ) when is_binary( BinString ) ->
	Str = binary_to_string( BinString ),
	UpStr = string:to_upper( Str ),
	string_to_binary( UpStr ).



-doc """
Flattens the specified IOList, that is returns a plain (non-nested) string out
of it.

Note that usually a good practice is to rely on IOLists as much as possible, as
most standard functions can deal with them.
""".
-spec flatten( io_list() ) -> ustring().
flatten( IOList ) ->
	lists:flatten( IOList ).



-doc """
Returns a binary string corresponding to the specified io_data (i.e. already a
binary, or an `iolist()`).
""".
-spec io_to_binary( io_data() ) -> bin_string().
io_to_binary( IOData ) ->
	iolist_to_binary( IOData ).




-doc """
Joins, with the specified separator, the specified (plain) strings, and
returns another plain string.

So `join(Separator, StringsToJoin)` is to be used like in: `join($-, ["Barbara",
"Ann"]) = "Barbara-Ann"`.

Separator can be a character, like `$a`, or a string, like `", "`.

Python-like `join`, combines items in a list into a string using a separator
between each item representation.

Inspired from [http://www.trapexit.org/String_join_with].

For file-related paths, the `file_utils:join/{1,2}` functions should be used
instead.

Note: conversely, use `split/2` to split the string.
""".
-spec join( ustring() | uchar(), [ ustring() ] ) -> ustring().
join( _Separator, _ListToJoin=[] ) ->
	"";

join( Separator, ListToJoin ) ->

	%io:format( "ListToJoin = ~p~n", [ ListToJoin ] ),

	IntercalList =
		list_utils:intercalate( _Elem=Separator, _TargetList=ListToJoin ),

	lists:flatten( IntercalList ).



-doc """
Joins, with the specified separator, the specified strings (of any type), and
returns a corresponding binary string.

So `bin_join(Separator, BinStringsToJoin)` is to be used like in: `join($-,
[<<"Barbara">>, <<"Ann">>]) = <<"Barbara-Ann">>`.

Separator can be a character, like `$a`, or any string, like `", "` or
`<<"hello">>`.

For file-related paths, the file_utils:bin_join/{1,2} functions should be used
instead..

Note: conversely, use `split/2` to split the string.

See also `bin_concatenate/2`.
""".
-spec bin_join( any_string() | uchar(), [ any_string() ] ) -> bin_string().
bin_join( Separator, ListToJoin ) ->

	IntercalList =
		list_utils:intercalate( _Elem=Separator, _TargetList=ListToJoin ),

	erlang:iolist_to_binary( IntercalList ).



-doc """
Splits the specified string into a list of strings (of the same type as the
input one), based on the single specified character to be interpreted as a
separator, or on the list thereof.

Note that a series of contiguous separators (e.g. two spaces in a row) will
result in inserting empty strings (i.e. `[]`) in the returned list. Use
`split_per_element/2` if wanting to handle series of delimeters as if there was
only one of them (i.e. if not wanting the returned list to include empty
strings).

Defined here not to chase anymore after `string:tokens/2` and friends.

See also: `split_at_whitespaces/0`.
""".
-spec split( ustring(), [ uchar() ] | uchar() ) -> [ ustring() ];
		   ( bin_string(), [ uchar() ] | uchar() ) -> [ bin_string() ].
% Special-cased (clearer, more direct):
split( AnyString, Separator ) when is_integer( Separator ) ->
				   % As not a BIF: is_char( Separator ) ->
	string:split( AnyString, _SearchPattern=[ Separator ], _Where=all );

% List (of separators) expected:
split( AnyString, Separators ) ->

	%trace_utils:debug_fmt( "Splitting '~ts' with '~ts'.",
	%                       [ AnyString, Separators ] ),

	% Note: string:tokens/2 is now deprecated in favor of string:lexemes/2, and
	% and anyway both treat two or more adjacent separator graphemes clusters as
	% only one, which is generally not what we want; so we now use our own
	% function.

	% Would be quite different, as Separators here would be understood as a
	% search pattern (i.e. a "word" as a whole) instead of a list of separators:
	%
	%string:split( AnyString, _SearchPattern=Separators, _Where=all ).

	% Would lead to a breach of contract (no empty string ever inserted):
	%string:lexemes( AnyString, Separators ).

	% So we go for a not-so-expensive multi-pass splitting (one pass per
	% separator):
	%
	split_helper( Separators, _Acc=[ AnyString ] ).



% (helper)
split_helper( _Separators=[], Acc ) ->
	Acc;

split_helper( _Separators=[ D | T ], Acc ) ->

	SplitStrs = [ string:split( S, _SearchPattern=[ D ], _Where=all )
											|| S <- Acc ],

	NewAcc = concatenate( SplitStrs ),
	split_helper( T, NewAcc ).



-doc "Splits a string according to the newlines (`~n`) that it contains.".
-spec split_lines( ustring() ) -> [ ustring() ];
				 ( bin_string() ) -> [ bin_string() ].
split_lines( AnyString ) ->
	split( AnyString, "\n" ). % i.e. [ $\n ]



-doc """
Unsplits the specified lines: returns a plain string aggregating the specified
strings, once separated by newlines.
""".
-spec unsplit_lines( [ any_string() ] ) -> ustring().
unsplit_lines( AnyStrings ) ->
	%trace_utils:debug_fmt( "Lines to unsplit: '~p'.", [ AnyStrings ] ),
	join( _Sep=$\n, AnyStrings ).



-doc """
Unsplits the specified lines: returns a binary string aggregating the specified
strings, once separated by newlines.
""".
-spec bin_unsplit_lines( [ any_string() ] ) -> bin_string().
bin_unsplit_lines( AnyStrings ) ->
	bin_join( _Sep=$\n, AnyStrings ).



-doc """
Splits the specified string into a list of strings, based on the list of
specified characters to be interpreted as separators.

Note that a series of contiguous separators (e.g. two spaces in a row) will be
handled as if there was only one of them (i.e. if the returned list should not
include empty strings).

See also: `split/2`.
""".
-spec split_per_element( ustring(), [ uchar() ] ) -> [ ustring() ].
split_per_element( String, Separators ) ->
	%[ Elem || Elem <- split( String, Separators ), Elem =/= [] ].
	string:lexemes( String, Separators ).



-doc """
Splits the specified parse string (typically returned by `parse_quoted/{1,3}`)
into a list of plain strings, based on the list of specified characters to be
interpreted as separators.

Note: implemented in an ad hoc way, so that any plain string found in the
input character stream is properly handled (i.e. not searched for any
separator).

In this example, parsing is needed so that the comma just after the first
`"Bond"` is not considered as a separator (since it is in a quoted context):
```
ParsedString = text_utils:parse_quoted( "Hello,'Mr Bond,James Bond',MI6",
	_QuotingChars=[ $' ], _EscapingChars=[] ),

ParsedString = "Hello," ++ ["Mr Bond,James Bond"] ++ ",MI6",

text_utils:split_parsed(ParsedString, [ $, ]) =
	["Hello", "Mr Bond, James Bond", "MI6"]
```

This allows extracting here three comma-separated fields, while taking into
account any quoting involved.

See also: `split/2`, `split_per_element/2`.
""".
-spec split_parsed( parse_string(), [ uchar() ] ) -> [ ustring() ].
split_parsed( ParseString, Separators ) ->

	%trace_utils:debug_fmt( "Splitting '~p' with separators '~p'...",
	%                       [ ParseString, Separators ] ),

	Res = split_parsed( ParseString, Separators, _AccElem=[], _AccStrs=[] ),

	%trace_utils:debug_fmt( "... returned: ~p.", [ Res ] ),

	Res.



-doc """
Collects chars in elements (`AccElem`), then elements in the overall accumulator
(`AccStrs`).

We used to avoid adding any empty element, yet this may happen (typically in CSV
files), hence re-enabled (previous version left commented).

(helper)
""".
%split_parsed( _ParseString=[], _Separators, _AccElem=[], AccStrs ) ->
%   lists:reverse( AccStrs );

split_parsed( _ParseString=[], _Separators, AccElem, AccStrs ) ->
	lists:reverse( [ lists:reverse( AccElem ) | AccStrs ] );

split_parsed( _ParseString=[ C | T ], Separators, AccElem, AccStrs )
												when is_integer( C ) ->
	case lists:member( C, Separators ) of

		true ->
			split_parsed( T, Separators, _AccElem=[],
						  [ lists:reverse( AccElem ) | AccStrs ] );

			%case AccElem of
			%
			%   [] ->
			%		split_parsed( T, Separators, _AccElem=[], AccStrs );
			%
			%   _ ->
			%       split_parsed( T, Separators, _AccElem=[],
			%                     [ lists:reverse( AccElem ) | AccStrs ] )
			%
			%end;

		false ->
			split_parsed( T, Separators, [ C | AccElem ], AccStrs )

	end;

split_parsed( _ParseString=[ Str | T ], Separators, AccElem, AccStrs )
														when is_list( Str ) ->
	split_parsed( T, Separators, lists:reverse( Str ) ++ AccElem, AccStrs );

split_parsed( _ParseString=[ Other | _T ], _Separators, _AccElem, _AccStrs ) ->
	throw( { unexpected_parsed_element, Other } ).



-doc """
Splits the specified string into a list of strings, using whitespaces as
separators.

For example: `split_at_whitespaces("  aaa  bbb  ccc  ") =
	[[], [], "aaa", [], "bbb", [], "ccc", [], []]`.
""".
-spec split_at_whitespaces( ustring() ) -> [ ustring() ].
split_at_whitespaces( String ) ->
	split( String, list_whitespaces() ).



-doc """
Splits the specified string according to the first occurrence (if any) of the
specified character, then returns a pair of two strings, containing respectively
all characters strictly before and strictly after the first occurrence of the
marker (which is thus not kept); otherwise returns `none_found`.

For example: `split_at_first($x, " aaaxbbbxccc")` shall return `{" aaa",
"bbbxccc"}`.
""".
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



-doc """
Splits the specified string, expected to be containing a word in CamelCase, into
a list of strings, based on the internal words (delimited by uppercases, knowing
a series of uppercase letters, except the last one, is considered as an acronym,
hence as a single word), in their original order.

For example: `split_camel_case("IndustrialWasteSource")` shall return
`["Industrial", "Waste", "Source"]`, while
`split_camel_case("TheySaidNYCWasGreat")` shall return `["They", "Said", "NYC",
"Was", "Great"]`.
""".
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
			%
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



-doc """
Splits the specified string into a list of strings, based on the list of
separating characters provided in `SeparatorsList`, then turns these resulting
strings in the "Capitalized Case" (all lower-case except for the first letter)
and finally joins them to get a long CamelCased string.

For example: `tokenizable_to_camel_case("industrial_WASTE_sOuRCe", "_")` shall
return `"IndustrialWasteSource"`, while `tokenizable_to_camel_case("ME HAZ READ
J.R.R", ". ")` shall return `"MeHazReadJRR"`.
""".
-spec tokenizable_to_camel_case( ustring(), ustring() ) -> ustring().
tokenizable_to_camel_case( String, SeparatorsList ) ->

	% Separates the tokens:
	Tokens = string:tokens( String, SeparatorsList ),

	% Makes all the tokens lower-case if needed:
	LowerCaseTokens = [ string:to_lower( Str ) || Str <- Tokens ],

	% Capitalizes all lower-cased tokens:
	CamelCaseTokens =
		[ uppercase_initial_letter( Str ) || Str <- LowerCaseTokens ],

	% Concatenates the capitalized tokens:
	lists:concat( CamelCaseTokens ).



-doc """
Splits the specified string every Count characters.

The last string may have less than Count characters.

For example: `["AB", "CD", "E"] = split_every( "ABCDE", _Count=2 )`.
""".
-spec split_every( count(), ustring() ) -> [ ustring() ].
split_every( Count, Str ) ->
	list_utils:group_by( Count, Str ).



-doc """
Duplicates the specified string as many times as specified; returns a plain
(flattened-once) string, not an `iolist`.

For example: `duplicate(3, "abc") = "abcabcabc"`.

Use directly `lists:duplicate/2` if wanting for example `["abc", "abc", "abc"]`.
""".
-spec duplicate( count(), ustring() ) -> ustring().
duplicate( Count, Str ) ->
	concatenate( lists:duplicate( Count, Str ) ).



-doc """
Concatenates all elements (string-like ones or numbers) in the specified list
into a single (plain) string.

More general and convenient defined here rather than only in
`list_utils:flatten_once/1`.
""".
-spec concatenate( [ string() | atom() | number() ] ) -> ustring().
concatenate( Elements ) ->
	%trace_utils:debug_fmt( "Concatenating ~p.", [ Elements ] ),
	lists:concat( Elements ).


-doc """
Concatenates the two specified strings (supposed of the same type) into the
returned one, of the same type.
""".
-spec concatenate( any_string(), any_string() ) -> any_string().
concatenate( FirstBinStr, SecondBinStr ) when is_binary( FirstBinStr ) ->
	bin_concatenate( FirstBinStr, SecondBinStr );

% concatenate/2 on plain strings is just '++'.
concatenate( FirstStr, SecondStr ) ->
	% Implicit encoding: unicode.
	unicode:characters_to_list( _Data=[ FirstStr, SecondStr ] ).



-doc "Concatenates the two specified binary strings into the returned one.".
-spec bin_concatenate( bin_string(), bin_string() ) -> bin_string().
bin_concatenate( FirstBinStr, SecondBinStr ) ->

	%trace_utils:debug_fmt( "Concatenating '~p' with '~p'.",
	%                       [ FirstBinStr, SecondBinStr ] ),

	% Presumably better than bin_format("~ts~ts", [FirstBinStr, SecondBinStr]),
	% mostly the same as erlang:iolist_to_binary([FirstBinStr, SecondBinStr]):
	%
	<<FirstBinStr/binary, SecondBinStr/binary>>.



-doc "Concatenates the specified binary strings into the returned one.".
-spec bin_concatenate( [ bin_string() ] ) -> bin_string().
bin_concatenate( BinStrs ) ->
	bin_utils:concatenate( BinStrs ).



-doc """
Returns in-order the specified list of strings once all empty ones have been
removed.
""".
-spec remove_empty_lines( [ ustring() ] ) -> [ ustring() ].
remove_empty_lines( Strs ) ->
	[ S || S <- Strs, S =/= "" ].



-doc """
Substitutes in the specified string the specified source character with the
target one: replaces all occurrences thereof; returns a string of the same type
as the specified one.

Note: simpler and probably more efficient than a regular expression.

Use `string:replace/3` for string-based substitutions, like for example:
```
EscapedArgStr = string:replace(_In=ArgStr, _SearchPattern="~",
							   _Replacement="\~", _Where=all)
```
""".
-spec substitute( uchar(), uchar(), ustring() | bin_string() ) -> ustring().
substitute( SourceChar, TargetChar, BinString ) when is_binary( BinString ) ->
	substitute( SourceChar, TargetChar, binary_to_string( BinString ) );

substitute( SourceChar, TargetChar, String ) ->
	substitute( SourceChar, TargetChar, String, _Acc=[] ).


% (helper)
substitute( _SourceChar, _TargetChar, _String=[], Acc ) ->
	lists:reverse( Acc );

substitute( SourceChar, TargetChar, _String=[ SourceChar | T ], Acc ) ->
	substitute( SourceChar, TargetChar, T, [ TargetChar | Acc ] );

substitute( SourceChar, TargetChar, _String=[ OtherChar | T ], Acc ) ->
	substitute( SourceChar, TargetChar, T, [ OtherChar | Acc ] ).



-doc """
Returns the index, in terms of grapheme clusters, of the first occurrence of the
specified pattern substring (if any) in the specified string.

An (attempt of) Unicode-aware replacement of `string:str/2` and `string:rstr/2`.
""".
-spec find_substring_index( chardata(), chardata() ) -> gc_index() | 'nomatch'.
find_substring_index( String, SearchPattern ) ->
	find_substring_index( String, SearchPattern, _Direction=leading ).



-doc """
Returns the index, in terms of grapheme clusters, of the first or last
occurrence (depending on the specified direction) of the specified pattern
substring (if any) in the specified string.

An (attempt of) Unicode-aware replacement of `string:str/2` and `string:rstr/2`.
""".
-spec find_substring_index( chardata(), chardata(), direction() ) ->
									gc_index() | 'nomatch'.
find_substring_index( String, SearchPattern, Direction ) ->
	GCString = string:to_graphemes( String ),
	GCSearchPattern = string:to_graphemes( SearchPattern ),
	PseudoIndex = case Direction of

		leading ->
			string:str( GCString, GCSearchPattern );

		trailing ->
			string:rstr( GCString, GCSearchPattern )

	end,

	case PseudoIndex of

		0 ->
			nomatch;

		% Indexes of grapheme clusters are to start at 0, not 1:
		I ->
			I-1

	end.



-doc """
Filters out in the specified string the specified character, so that it does not
occur anymore on the returned string.

Note: simpler and probably more efficient that a regular expression.
""".
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



-doc """
Splits the specified string after the specified prefix and returns the remaining
part, otherwise returns that the prefix was not found.

For example: `split_after_prefix("Foo", "Foobar is baz.")` returns `"bar is
baz."`; `split_after_prefix("ABC", "Foobar is baz.")` returns `'no_prefix'`.
""".
-spec split_after_prefix( ustring(), ustring() ) -> ustring() | 'no_prefix'.
split_after_prefix( _Prefix=[], String ) ->
	String;

split_after_prefix( _Prefix=[ C | T ], _String=[ C | StringT ] ) ->
	split_after_prefix( T, StringT );

split_after_prefix( _Prefix, _String ) ->
	no_prefix.



-doc """
Splits the specified string before the specified suffix, and returns the leading
part, otherwise returns that the suffix was not found.

For example: `split_before_suffix("baz.", "Foobar is baz.")` returns `"Foobar is
"`; `split_before_suffix("ABC", "Foobar is baz.")` returns `'no_suffix'`.
""".
-spec split_before_suffix( ustring(), ustring() ) -> ustring() | 'no_suffix'.
split_before_suffix( Suffix, String ) ->
	case split_after_prefix( lists:reverse( Suffix ),
							 lists:reverse( String ) ) of

		no_prefix ->
			no_suffix;

		RevTrailing ->
			lists:reverse( RevTrailing )

	end.



-doc """
Updates the specified text with the specified keywords, returning a version of
which where all the specified keywords (the keys of the translation table) have
been replaced by their associated value (that is the value in table
corresponding to that key).

For example: `text_utils:update_with_keywords("Hello word!", table:new([{"foo",
"bar"}, {"ord", "orld"}]))`.

See also: `file_utils:update_with_keywords/3`.
""".
-spec update_with_keywords( any_string(), translation_table() ) ->
											[ string_like() ].
update_with_keywords( Content, TranslationTable ) ->

	TransPairs = ?table:enumerate( TranslationTable ),

	% As many passes as keyword pairs:
	lists:foldl(
		fun( { SearchP, Replacement }, ContentAcc ) ->
			%trace_utils:debug_fmt( "Replacing '~ts' with '~ts' in:~n  ~p",
			%                       [ SearchP, Replacement, ContentAcc ] ),
			string:replace( ContentAcc, SearchP, Replacement, _Where=all )
		end,
		_Acc0=Content,
		_List=TransPairs ).



-doc "Returns a list of all known whitespaces.".
-spec list_whitespaces() -> [ char() ].
list_whitespaces() ->
	" \t\n".



-doc """
Single-quotes the specified string, that is returns it once single-quoted.
""".
-spec single_quote_string( any_string() ) -> ustring().
single_quote_string( AnyStr ) ->
	format( "'~ts'", [ AnyStr ] ).



-doc """
Double-quotes the specified string, that is returns it once double-quoted.
""".
-spec double_quote_string( any_string() ) -> ustring().
double_quote_string( AnyStr ) ->
	format( "\"~ts\"", [ AnyStr ] ).



-doc """
Single-quotes each string in the specified list, that is returns them (in-order)
once single-quoted.
""".
-spec single_quote_strings( [ any_string() ] ) -> [ ustring() ].
single_quote_strings( AnyStrs ) ->
	[ single_quote_string( S ) || S <- AnyStrs ].



-doc """
Double-quotes each string in the specified list, that is returns them (in-order)
once double-quoted.
""".
-spec double_quote_strings( [ any_string() ] ) -> [ ustring() ].
double_quote_strings( AnyStrs ) ->
	[ double_quote_string( S ) || S <- AnyStrs ].



-doc """
Returns the specified text, in which single quotes have been escaped (that is:
all `'` characters have been replaced with `\'` ones).
""".
-spec escape_single_quotes( ustring() ) -> ustring().
escape_single_quotes( Text ) ->
	escape_single_quotes_helper( Text, _Acc=[] ).


escape_single_quotes_helper( _Text=[], Acc ) ->
	lists:reverse( Acc );

escape_single_quotes_helper( _Text=[ $' | T ], Acc ) ->
	% As will be reversed:
	escape_single_quotes_helper( T, "'\\" ++ Acc );

escape_single_quotes_helper( _Text=[ C | T ], Acc ) ->
	escape_single_quotes_helper( T, [ C | Acc ] ).



-doc """
Returns the specified text, in which double quotes have been escaped (that is:
all `"` characters have been replaced with `\"` ones).
""".
-spec escape_double_quotes( ustring() ) -> ustring().
escape_double_quotes( Text ) ->
	escape_double_quotes_helper( Text, _Acc=[] ).


escape_double_quotes_helper( _Text=[], Acc ) ->
	lists:reverse( Acc );

escape_double_quotes_helper( _Text=[ $" | T ], Acc ) ->
	% As will be reversed:
	escape_double_quotes_helper( T, "\"\\" ++ Acc );

escape_double_quotes_helper( _Text=[ C | T ], Acc ) ->
	escape_double_quotes_helper( T, [ C | Acc ] ).



-doc """
Returns the specified text, in which all quotes have been escaped (that is
characters `'` and `"` have been replaced respectively with `\'` and `\"`).
""".
-spec escape_all_quotes( ustring() ) -> ustring().
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



-doc """
Escapes, in the specified text, all characters in the specified list, with the
specified escaping char.

For example:
```
"baz\.foobar\.org" =
	text_utils:escape_with("baz.foobar.org", [ $. ], $\\).
```
""".
-spec escape_with( ustring(), [ char() ], char() ) -> ustring().
escape_with( Text, CharsToEscape, EscapingChar ) ->
	escape_with( Text, CharsToEscape, EscapingChar, _Acc=[] ).


% (helper)
escape_with( _Text=[], _CharsToEscape, _EscapingChar, Acc ) ->
	lists:reverse( Acc );

escape_with( _Text=[ C | T ], CharsToEscape, EscapingChar, Acc ) ->
	NewAcc = case lists:member( C, CharsToEscape ) of

		true ->
			% As will be ultimately reversed:
			[ C, EscapingChar | Acc ];

		false ->
			[ C | Acc ]

	end,

	escape_with( T, CharsToEscape, EscapingChar, NewAcc ).



-doc "Removes all newlines from the specified string.".
-spec remove_newlines( ustring() ) -> ustring().
remove_newlines( String ) ->
	lists:flatten( string:replace( String, "\n", "", all ) ).



-doc """
Parses the specified plain (non-`iolist`) string (that is a mere list of
characters), based on two quoting characters (single and double quotes) and one
escaping character (backslash), returning a specific kind of iolist containing
either characters or plain strings, the latter corresponding to the found quoted
texts, provided that they were not escaped.

For example, let's consider an input string such as (using, from now, `§` to
delimit strings):

`§This is an "example \" 'convoluted" string' with various 'quoting elements'.§`

Once parsed with this function, it shall be translated to a list containing the
following series of characters:

`§This is an §`, then: `§example " 'convoluted§`, then the series of characters
corresponding to: `§ string' with various 'quoting elements'.§`, i.e.: `"This is
an " ++ ["example \" 'convoluted" | "string' with various 'quoting elements']`.

Note: any escaping character is to escape any of the quoting characters, and
only them, if being in an unquoted context (i.e. otherwise both will be added
verbatim in the resulting string).

See `text_utils_test.erl` for a full example with additional explanations.
""".
-spec parse_quoted( plain_string() ) -> parse_string().
parse_quoted( InputStr ) ->
	parse_quoted( InputStr, _QuotingChars=[ $', $" ], _EscapingChars=[ $\\ ] ).



-doc """
Parses the specified plain (non-`iolist`) string (that is a mere list of
characters), based on the specified quoting characters and escaping characters,
returning a specific kind of iolist containing either individual characters or
plain strings, the latter corresponding to the found quoted texts, provided that
they were not escaped.

See `parse_quoted/1` regarding parsing/escaping rules, and `text_utils_test.erl`
for a full example with additional explanations.
""".
-spec parse_quoted( plain_string(), [ uchar() ], [ uchar() ] ) ->
														parse_string().
parse_quoted( InputStr, QuotingChars, EscapingChars ) ->

	%trace_utils:debug_fmt( "Parsing §~ts§, with quoting §~ts§ and "
	%   "escaping §~ts§:", [ InputStr, QuotingChars, EscapingChars ] ),

	Res = parse_helper( InputStr, QuotingChars, EscapingChars,
		_CurrentQuoteChar=undefined, _CurrentQuotedText=undefined,
		_PreviousChar=undefined, _Acc=[] ),

	%trace_utils:debug_fmt( "Parsed string is:~n ~p", [ Res ] ),

	Res.



% In examples below, double quotes are a quoting character, and backslash an
% escaping one.
%
% The general principal here is to read one character ahead and include the one
% just before it iff relevant.

% Normal endings:

% Not in quoted:
parse_helper( _InputStr=[], _QuotingChars, _EscapingChars, CurrentQuoteChar,
		_CurrentQuotedText=undefined, _PreviousChar=CurrentQuoteChar, Acc ) ->
	% Closing for good then:
	lists:reverse( Acc );

% Ending while a quoting sequence is still open, but here the last (previous)
% character was a closing quoting one:
%
parse_helper( _InputStr=[], _QuotingChars, _EscapingChars, CurrentQuoteChar,
			  CurrentQuotedText, _PreviousChar=CurrentQuoteChar, Acc ) ->
	% Closing for good then:
	RevQuoted = lists:reverse( CurrentQuotedText ),
	lists:reverse( [ RevQuoted | Acc ] );

% Never add 'undefined' chars:
parse_helper( _InputStr=[], _QuotingChars, _EscapingChars,
			  _CurrentQuoteChar=undefined, _CurrentQuotedText=undefined,
			  _PreviousChar=undefined, Acc ) ->
	lists:reverse( Acc );

% Most usual (normal) ending (not in a quoted context):
% (this clause just has PreviousChar =/= undefined)
%
parse_helper( _InputStr=[], _QuotingChars, _EscapingChars,
			  _CurrentQuoteChar=undefined, _CurrentQuotedText=undefined,
			  PreviousChar, Acc ) ->
	lists:reverse( [ PreviousChar | Acc ] );


parse_helper( _InputStr=[], _QuotingChars, _EscapingChars, CurrentQuoteChar,
			  CurrentQuotedText, PreviousChar, Acc ) ->

	RevQuoted = case PreviousChar of

		undefined ->
			lists:reverse( CurrentQuotedText );

		_ ->
			lists:reverse( [ PreviousChar | CurrentQuotedText ] )

	end,

	CurrentStr = lists:reverse( [ RevQuoted | Acc ] ),

	throw( { unmatched_quoting_char, CurrentQuoteChar,
				{ still_in, RevQuoted }, lists:flatten( CurrentStr ) } );


% Still iterating below:

% While not being in a quoted context and reading a character, possibly a
% quoting one:
%
parse_helper( _InputStr=[ C | T ], QuotingChars, EscapingChars,
			  CurrentQuoteChar=undefined, CurrentQuotedText=undefined,
			  _PreviousChar=PrevC, Acc ) ->

	%trace_utils:debug_fmt( "Out of quoted context, read §~ts§ "
	%    "(previous: §~p§), while current, reversed accumulator is:~n  §~p§.",
	%    [ [ C ], [ PrevC ], lists:reverse( Acc ) ] ),

	% lists:member/2 not a valid guard, so:
	%
	% (note that having PreviousChar=undefined is nicely handled as well by this
	% code)
	%
	case lists:member( C, QuotingChars ) of

		true ->
			% Read char is a quoting one (while not in a quoted text), so:
			case lists:member( PrevC, EscapingChars ) of

				% The quoting char is escaped, keep it (and only it).
				%
				% For example: found §\"§; then just retaining §"§ verbatim (we
				% used to drop PrevC=§\§ but it should not):
				%
				true ->

					%trace_utils:debug_fmt( "Out of quoted context, read "
					%   "quoting char §~ts§ while previous was an escaping "
					%    "one (§~p§), while current, reversed accumulator "
					%    "is:~n  §~p§.",
					%    [ [C], [PrevC], lists:reverse( Acc ) ] ),

					parse_helper( T, QuotingChars, EscapingChars,
						CurrentQuoteChar, CurrentQuotedText, _PrevChar=C,
						%Acc );
						[ PrevC | Acc ] );

				% Here, unescaped quoting char while not in quoted text, thus
				% entering a quoting section:
				%
				% (PrevC possibly equal to 'undefined' here)
				%
				false ->
					NewAcc = case PrevC of

						undefined ->
							Acc;

						_ ->
							[ PrevC | Acc ]

					end,

					%trace_utils:debug_fmt( "Entering a quoting section with "
					%   "§~ts§, while current, reversed accumulator is:~n  "
					%   "§~p§", [ [C], lists:reverse( NewAcc ) ] ),

					parse_helper( T, QuotingChars, EscapingChars,
						_CurrentQuoteChar=C, _CurrentQuotedText=[],
						_PrevChar=undefined, NewAcc )

			end;

		% The just-read char (C) is not a quoting one, still out of quoted
		% context then:
		%
		false ->
			case PrevC of

				undefined ->
					parse_helper( T, QuotingChars, EscapingChars,
						CurrentQuoteChar, CurrentQuotedText, _PrevChar=C, Acc );

				_ ->
					parse_helper( T, QuotingChars, EscapingChars,
						CurrentQuoteChar, CurrentQuotedText, _PrevChar=C,
						[ PrevC | Acc ] )

			end

	end;


% Here, we are already in a quoted context, and we found a matching quoting
% char:
%
parse_helper( _InputStr=[ C | T ], QuotingChars, EscapingChars,
		CurrentQuoteChar=C, CurrentQuotedText, _PreviousChar=PrevC, Acc ) ->

	%trace_utils:debug_fmt( "In quoted context, read §~ts§ (previous: §~p§) "
	%   "while current quoted text is §~ts§",
	%   [ [C], [PrevC], CurrentQuotedText ] ),

	% Maybe found a closing quoting char - unless it is escaped:
	case lists:member( C, QuotingChars ) of

		true ->
			case lists:member( PrevC, EscapingChars ) of

				% For example §\"§.
				%
				% This quoting char is escaped, thus not counting as such:
				% (quoting C kept in previous, escaping PrevC used to be
				% dropped but should not)
				true ->

					%trace_utils:debug_fmt( "Adding quoting character '~ts' as "
					%   "such, as was escaped (by '~ts').", [ [C], [PrevC] ] ),

					parse_helper( T, QuotingChars, EscapingChars,
						%CurrentQuoteChar, CurrentQuotedText,
						CurrentQuoteChar, [ PrevC | CurrentQuotedText ],
						_PrevChar=C, Acc );

				% For example §A"§.
				% Here, unescaped quoting char while in quoted text, thus
				% closing a quoting section:
				%
				% (PrevC possibly equal to 'undefined' here)
				%
				false ->
					Quoted = case PrevC of

						undefined ->
							lists:reverse( CurrentQuotedText );

						_ ->
							lists:reverse( [ PrevC | CurrentQuotedText ] )

					end,

					%trace_utils:debug_fmt( "Closing a quoting section "
					%   "(result:'~ts') with '~ts', while reversed accumulator "
					%   "is:~n~p", [ Quoted, [C], lists:reverse( Acc ) ] ),

					parse_helper( T, QuotingChars, EscapingChars,
						_CurrentQuoteChar=undefined,
						_CurrentQuotedText=undefined,
						_PrevChar=undefined, [ Quoted | Acc ] )

			end;

		% Is not a quoting char here, thus continuing in quoted:
		false ->
			case PrevC of

				undefined ->
					parse_helper( T, QuotingChars, EscapingChars,
						CurrentQuoteChar, CurrentQuotedText, _PrevChar=C, Acc );

				_ ->
					parse_helper( T, QuotingChars, EscapingChars,
						CurrentQuoteChar, [ PrevC | CurrentQuotedText ],
						_PrevChar=C, Acc )

			end

	end;


% In quoted context, read char not being a matching quoting char, and not having
% a previous char:
%
parse_helper( _InputStr=[ C | T ], QuotingChars, EscapingChars,
		CurrentQuoteChar, CurrentQuotedText, _PreviousChar=undefined, Acc ) ->

	%trace_utils:debug_fmt( "Just recording, in quoted context, "
	%   "current char: §~ts§", [ [ C ] ] ),

	parse_helper( T, QuotingChars, EscapingChars, CurrentQuoteChar,
				  CurrentQuotedText, _PrevChar=C, Acc );

% Same but with a previous (non-undefined) char:
parse_helper( _InputStr=[ C | T ], QuotingChars, EscapingChars,
			  CurrentQuoteChar, CurrentQuotedText, PreviousChar, Acc ) ->

	%trace_utils:debug_fmt( "Recording, in quoted context, "
	%   "current char: §~ts§", [ [ C ] ] ),

	parse_helper( T, QuotingChars, EscapingChars, CurrentQuoteChar,
				  [ PreviousChar | CurrentQuotedText ], _PrevChar=C, Acc ).



-doc "Tells whether the specified character is an uppercase one.".
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



-doc "Tells whether the specified character is a figure (in `0..9`).".
-spec is_figure( char() ) -> boolean().
is_figure( Char ) when is_integer( Char ), Char >= $0, Char =< $9 ->
	true;

is_figure( Char ) when is_integer( Char ) ->
	false.



-doc """
Removes any ending `\n` character(s) - zero or more thereof - from the specified
string.
""".
-spec remove_ending_carriage_return( ustring() ) -> ustring().
remove_ending_carriage_return( String ) when is_list( String ) ->

	% See also: list_utils:remove_last_element/1.

	% 'Res ++ "\n" = String,Res' will not work:
	string:strip( String, right, $\n ).



-doc """
Removes the last `Count` characters from the specified string, and returns the
result.
""".
-spec remove_last_characters( ustring(), count() ) -> ustring().
remove_last_characters( String, Count ) ->

	% Not necessarily the most efficient, but at least it is not an illegal
	% pattern:
	%
	case erlang:length( String ) of

		C when C >= Count ->
			string:substr( String, 1, C - Count );

		_->
			throw( { removal_failed, String, Count } )

	end.



-doc """
Removes all whitespaces from the specified string, and returns the result.
""".
-spec remove_whitespaces( any_string() ) -> ustring().
remove_whitespaces( String ) ->
	re:replace( String, "\s", "", [ global, unicode, { return, list } ] ).



-doc """
Removes all leading and trailing whitespaces from the specified string, and
returns the result.
""".
-spec trim_whitespaces( any_string() ) -> ustring().
trim_whitespaces( String ) ->
	% Should be done in one pass:
	trim_leading_whitespaces( trim_trailing_whitespaces( String ) ).



-doc """
Removes all leading whitespaces from the specified string, and returns the
result.
""".
-spec trim_leading_whitespaces( any_string() ) -> ustring().
trim_leading_whitespaces( String ) ->
	% Largely inspired from http://www.trapexit.org/Trimming_Blanks_from_String:
	re:replace( String, "^\\s*", "", [ unicode, { return, list } ] ).



-doc """
Removes all trailing whitespaces from the specified string, and returns the
result.
""".
-spec trim_trailing_whitespaces( any_string() ) -> ustring().
trim_trailing_whitespaces( String ) ->
	% The $ confuses some syntax highlighting systems (like the one of some
	% emacs):
	%
	re:replace( String, "\\s*$", "", [ unicode, { return, list } ] ).



-doc """
Ellipses (shortens by removing the end of) the specified string, so that its
total length remains up to the default (maximum length) threshold (including an
additional final `" [...]"` part if it was shortened).

Returns a string of the same type.

See also: `tail/1`.
""".
-spec ellipse( any_string() ) -> any_string().
ellipse( String ) ->
	ellipse( String, _DefaultMaxLen=800 ).



-doc """
Ellipses (shortens by removing the end of) the specified string, so that its
total length remains up to the specified (maximum length) threshold (including
an additional final `" [...]"` if it was shortened).

Returns a string of the same type.

See also: `tail/2`.
""".
-spec ellipse( any_string(), length() | 'unlimited' ) -> any_string().
ellipse( String, _MaxLen=unlimited ) ->
	String;

ellipse( String, MaxLen ) ->

	case string:length( String ) of

		L when L > MaxLen ->

			% To allow for a proper next concatenation:
			Suffix = case is_binary( String ) of

				true ->
					<<" [...]">>;

				false ->
					" [...]"

			end,

			% To avoid countless computations of a constant:
			SuffixLen = 6,

			TargetLen = MaxLen - SuffixLen,
			concatenate( string:slice( String, _Start=0, TargetLen ), Suffix );

		_ ->
			String

	end.



-doc """
Ellipses (shortens) the specified string to format, so that its total length
remains up to the default (maximum length) threshold (including an additional
final `" [...]"` if it was shortened).
""".
-spec ellipse_fmt( format_string(), format_values() ) -> ustring().
ellipse_fmt( FormatString, Values ) ->
	ellipse( format( FormatString, Values ) ).



-doc """
Ellipses (shortens) the specified string to format, so that its total length
remains up to the default (maximum length) threshold (including an additional
final `" [...]"` if it was shortened).
""".
-spec ellipse_fmt( format_string(), format_values(),
				   length() | 'unlimited' ) -> ustring().
ellipse_fmt( FormatString, Values, MaxLen ) ->
	ellipse( format( FormatString, Values ), MaxLen ).



-doc """
Tails (shortens by removing the beginning of) the specified string, so that its
total length remains up to the default (maximum length) threshold (including an
additional initial `" [...]"` if it was shortened).

See also: `ellipse/1`.
""".
-spec tail( ustring() ) -> ustring().
tail( String ) ->
	tail( String, _DefaultMaxLen=800 ).



-doc """
Tails (shortens by removing the beginning of) the specified string, so that its
total length remains up to the specified threshold.

Note: the specified threshold is expected to be equal at least to 6.

See also: `ellipse/2`.
""".
-spec tail( ustring(), length() | 'unlimited' ) -> ustring().
tail( String, _MaxLen=unlimited ) ->
	String;

tail( String, MaxLen ) ->

	Len = string:length( String ),

	ExtraCount = Len - MaxLen,

	case ExtraCount > 0 of

		true ->

			% To allow for a proper next concatenation:
			Prefix = case is_binary( String ) of

				true ->
					<<"[...] ">>;

				false ->
					"[...] "

			end,

			% To avoid countless computations of a constant:
			PrefixLen = 6,

			concatenate( Prefix,
				string:slice( String, _Start=ExtraCount + PrefixLen ) );

		_ ->
			String

	end.



-doc """
Formats (word-wraps) the specified text according to the specified line width,
expressed in characters, by maximising the number of words in each line and
afterwards padding it with spaces.

Returns a list of strings, each of which having exactly Width characters.
""".
-spec format_text_for_width( ustring(), width() ) -> [ ustring() ].
format_text_for_width( Text, Width ) ->
	format_text_for_width( Text, Width, _DoPad=true ).



-doc """
Formats (word-wraps) the specified text according to the specified line width,
expressed in characters, by maximising the number of words in each line and
afterwards padding it with spaces.

Returns a list of strings, each of which having exactly `Width` characters.
""".
-spec format_text_for_width( ustring(), width(), boolean() ) -> [ ustring() ].
format_text_for_width( Text, Width, DoPad ) ->

	% Whitespaces converted to spaces:
	CleanedTest = re:replace( lists:flatten( Text ), "\\s+", " ",
							  [ global, { return, list } ] ),

	Words = string:tokens( CleanedTest, _Sep=" " ),

	%io:format( "Formatting ~p.~n", [ Words ] ),
	R = join_words( Words, Width, DoPad ),

	%io:format( "Result: '~ts'.~n", [ R ] ),

	R.



-doc "Joins words from the specified list, line by line.".
join_words( Words, Width, DoPad ) ->
	join_words( Words, Width, DoPad, _AccLines=[], _CurrentLine="",
				_CurrentLineLen=0 ).


join_words( _Words=[], _Width, _DoPad, AccLines,
			_CurrentLine, _CurrentLineLen=0 ) ->
	% Just ended with a full line:
	R = lists:reverse( AccLines ),
	%io:format( "Returning R1='~w'.~n", [ R ] ),
	R;

join_words( _Words=[], _Width, _DoPad=false, AccLines,
			CurrentLine, _CurrentLineLen ) ->
	lists:reverse( [ CurrentLine | AccLines ] );

join_words( _Words=[], Width, _DoPad=true, AccLines,
			CurrentLine, _CurrentLineLen ) ->

	PadChar = $ ,
	%PadChar = $X,

	% Ended with a partial line (most likely):
	R = lists:reverse( [ pad_string_left( CurrentLine, Width, PadChar )
							| AccLines ] ),

	%io:format( "Returning R2='~w'.~n", [ R ] ),
	R;

join_words( [ Word | RemainingWords ], Width, DoPad, AccLines, CurrentLine,
			CurrentLineLen ) ->

	%io:format( "Managing word '~ts' (len=~B), current line is '~ts' (len=~B), "
	%   "width = ~B.~n", [ Word, erlang:length( Word ), CurrentLine,
	%   CurrentLineLen, Width ] ),

	PadChar = $ ,
	%PadChar = $X,

	% The length of the new word should be incremented, as a space must be
	% inserted before that word, however we want to accept also words whose
	% width would be exactly equal to the line width:

	WLen = erlang:length( Word ),

	case WLen of

		CompatibleWidth when CompatibleWidth =< Width ->
			% The length of this word is manageable.
			% Will this word fit on the current line?

			ActualWLen = case CurrentLineLen of

				0 ->
					WLen;

				_NonNullLen ->
					% Already at least a letter, we therefore must add a space
					% before this new word:
					%
					WLen + 1

			end,

			case CurrentLineLen + ActualWLen of

				FittingLen when FittingLen =< Width ->
					% Yes, this word fits on the current line.
					% Avoids adding a space at the beginning of a new line:
					%
					{ NewCurrentLine, NewLineLen } = case CurrentLineLen of

						0 ->
							{ Word, CompatibleWidth };

						Len ->
							{ CurrentLine ++ " " ++ Word,
							  Len + CompatibleWidth + 1 }

					end,

					%io:format( "Current line is now '~ts'.~n",
					%           [ NewCurrentLine ] ),
					join_words( RemainingWords, Width, DoPad, AccLines,
								NewCurrentLine, NewLineLen );

				_ExceedingLen ->

					% No, with this word the current line would be too wide,
					% inserting it on new line instead:
					%
					PaddedCurrentLine = case DoPad of

						true ->
							pad_string_left( CurrentLine, Width, PadChar );

						false ->
							CurrentLine

					end,

					%io:format( "Inserting line '~ts'.~n",
					%           [ PaddedCurrentLine ] ),

					join_words( RemainingWords, Width, DoPad,
						[ PaddedCurrentLine | AccLines ], Word,
						CompatibleWidth )

			end;


		_TooLargeWidth ->

			% Will break words as many times as needed:
			%io:format( "Word '~ts' is too large (len=~B), breaking it.~n",
			%           [ Word, erlang:length( Word ) ] ),

			Subwords = break_word( Word, Width ),

			PaddedCurrentLine = case DoPad of

				true ->
					pad_string_left( CurrentLine, Width, PadChar );

				false ->
					CurrentLine

			end,

			join_words( Subwords ++ RemainingWords, Width, DoPad,
						[ PaddedCurrentLine | AccLines ], "", 0 )

	end.



-doc """
Returns the specified string, once padded with spaces to the specified width,
left-justified (that is with spaces added to the right).

For example: `pad_string("hello", 8) = ["hello",32,32,32]`.

Note that the returned string is not flattened.
""".
-spec pad_string( ustring(), width() ) -> parse_string().
pad_string( String, Width ) ->
	pad_string_left( String, Width ).



-doc """
Returns the specified string, once padded with spaces to the specified width,
left-justified (that is with spaces added to the right).

For example: `pad_string_left("hello", 8) = ["hello",32,32,32]`.

Note that the returned string is not flattened.
""".
-spec pad_string_left( ustring(), width() ) -> parse_string().
pad_string_left( String, Width ) ->
	pad_string_left( String, Width, _PadChar=$\s ).



-doc """
Returns the specified string, once padded with spaces to the specified width,
left-justified (that is with spaces added to the right), with the specified
padding character.

For example: `pad_string_left("hello", 8, $*) = ["hello",42,42,42]`.

Note that the returned string is not flattened.
""".
-spec pad_string_left( ustring(), width(), grapheme_cluster() ) ->
												parse_string().
pad_string_left( String, Width, PadChar )
								when erlang:length( String ) =< Width ->

	% Note that the settings listed in
	% http://erlang.org/doc/apps/stdlib/unicode_usage.html shall be enforced so
	% that character encoding is properly supported (with Unicode), otherwise
	% characters such as "e" with an accent are considered as two characters
	% instead of one, leading to incorrect (insufficient) padding:
	%
	%lists:flatten( io_lib:format( "~*.ts", [ -Width, String ] ) );

	string:pad( String, Width, _Dir=trailing, PadChar );

pad_string_left( String, Width, PadChar ) ->

	Len = erlang:length( String ),

	trace_utils:error_fmt( "String '~ts' already too long (~B characters) "
		"to be padded (left) to width ~B (with '~ts').",
		[ String, Len, Width, case is_integer( PadChar ) of
			true -> [ PadChar ]; false -> PadChar end ] ),

	throw( { string_too_long_to_pad_left, String, Len, Width } ).



-doc """
Returns the specified string, once padded with spaces to the specified width,
right-justified (that is with spaces added to the left).

For example: `pad_string_right("hello", 8) = ["   ", "hello"]`.

Note that the returned string is not flattened.
""".
-spec pad_string_right( ustring(), width() ) -> parse_string().
pad_string_right( String, Width ) ->
	pad_string_right( String, Width, _PadChar=$\s ).



-doc """
Returns the specified string, once padded with spaces to the specified width,
right-justified (that is with spaces added to the left), with the specified
padding character.

For example: `pad_string_right("hello", 8, $*) = ["***", "hello"]`.

Note that the returned string is not flattened.
""".
-spec pad_string_right( ustring(), width(), grapheme_cluster() ) ->
														parse_string().
pad_string_right( String, Width, PadChar )
									when erlang:length( String ) =< Width ->
	%lists:flatten( io_lib:format( "~*.ts", [ Width, String ] ) );

	string:pad( String, Width, _Dir=leading, PadChar );

pad_string_right( String, Width, PadChar ) ->

	Len = erlang:length( String ),

	trace_utils:error_fmt( "String '~ts' already too long (~B characters) "
		"to be padded (right) to width ~B (with '~ts').",
		[ String, Len, Width, case is_integer( PadChar ) of
			true -> [ PadChar ]; false -> PadChar end ] ),

	throw( { string_too_long_to_pad_right, String, Len, Width } ).



-doc """
Returns the specified string once padded with spaces on its left and right, in
order that it is centered within the specified width (expected of course to be
larger than the length of the specified string).

For example: `center_string("hello",8) = [" ","hello"," ",32]`.
""".
-spec center_string( ustring(), width() ) -> any_string().
center_string( String, Width ) ->
	center_string( String, Width, _PaddingChar=$\s ).



-doc """
Returns the specified string once padded with the specified character on its
left and right, in order that it is centered within the specified width
(expected of course to be larger than the length of the specified string).

For example: `center_string("hello",8, $*) = ["*","hello","*",42]`.
""".
-spec center_string( ustring(), width(), grapheme_cluster() ) -> any_string().
center_string( String, Width, PaddingChar ) ->

	%case Width - erlang:length( String ) of

	%   Offset when Offset < 0 ->
	%       throw( { string_to_center_too_long, String, -Offset } );

	%  Offset ->
	%       BaseCount = Offset div 2,
	%       { LeftPadCount, RightPadCount } = case Offset rem 2 of

	%           0 ->
	%               { BaseCount, BaseCount };

	%           1 ->
	%               % When not able to center perfectly, we prefer here being
	%               % the string to be a little on the left rather than a litlle
	%               % on the right:
	%               %
	%               { BaseCount, BaseCount+1 }

	%       end,

	%       lists:flatten( lists:duplicate( LeftPadCount, PaddingChar )
	%           ++ String ++ lists:duplicate( RightPadCount, PaddingChar ) )

	%end.
	string:pad( String, Width, _Dir=both, PaddingChar ).



-doc """
Tells whether the specified term is a (possibly Unicode, UTF-8) character,
i.e. `char()`.
""".
-spec is_char( term() ) -> boolean().
is_char( I ) when is_integer( I ), I >= 0, I =< 16#10ffff ->
	true;

is_char( _Other ) ->
	false.



-doc """
Returns true iff the parameter is a (non-nested) plain string (actually a list
of characters).

Improved from [http://lethain.com#distinguishing-strings-from-lists-in-erlang].

Note: so something like `[$e, 1, 2, $r]` is deemed to be a string.
""".
-spec is_string( term() ) -> boolean().
is_string( [] ) ->
	true;

is_string( [ H | T ] ) ->
	case is_char( H ) of

		true ->
			is_string( T );

		false ->
			false

	end;

is_string( _Other ) ->
	false.



-doc "Returns true iff the specified parameter is a binary string.".
-spec is_bin_string( term() ) -> boolean().
is_bin_string( Term ) when is_binary( Term ) ->
	% Would probably be excessive:
	%is_string( binary_to_list( Term ) );
	true;

is_bin_string( _Term ) ->
	false.



-doc """
Returns true iff the parameter is any kind (plain or binary) of (non-nested)
string.

Note: something like `[$e, 1, 2, $r]` is deemed to be a string.
""".
-spec is_any_string( term() ) -> boolean().
is_any_string( Bin ) when is_binary( Bin ) ->
	is_bin_string( Bin );

is_any_string( Term ) ->
	is_string( Term ).



-doc """
Returns true iff the parameter is a (non-nested) non-empty string (actually a
plain list of at least one integer).
""".
-spec is_non_empty_string( term() ) -> boolean().
is_non_empty_string( [] ) ->
	% Shall be not empty:
	false;

is_non_empty_string( S ) ->
	is_string( S ).



-doc """
Returns true iff the parameter is a string-like.
""".
-spec is_string_like( term() ) -> boolean().
% Possibly to be further refined/fixed:
is_string_like( A ) when is_atom( A ) ->
	true;

is_string_like( BS ) when is_binary( BS ) ->
	is_bin_string( BS );

is_string_like( L ) when is_list( L ) ->
	% Traverse recursively:
	lists:all( [ is_char( E ) orelse is_string_like( E ) || E <- L ] );

is_string_like( _Other ) ->
	false.



-doc """
Returns true iff the specified parameter is a list whose all elements are (all)
plain strings.

Note: especially useful knowing that a string is itself a list, hence a string
can easily be mistaken for a list of strings, in which case each of these
strings would actually be found being an integer instead (corresponding to each
of the characters of the overall string).
""".
-spec are_strings( list() ) -> boolean().
are_strings( [] ) ->
	true;

are_strings( [ H | T ] ) ->

	case is_string( H ) of

		true ->
			are_strings( T );

		false ->
			false

	end;

are_strings( _Other ) ->
	false.



-doc "Tells whether the specified term is a list of binary strings.".
-spec are_binaries( term() ) -> boolean().
are_binaries( List ) when is_list( List ) ->
	lists:all( fun is_bin_string/1, List );

are_binaries( _NotList ) ->
	false.



-doc """
Returns whether the two specified strings are of the same type (both plain or
both binary ones).
""".
-spec are_of_same_string_type( any_string(), any_string() ) -> boolean().
are_of_same_string_type( S1, S2 ) when is_list( S1 ), is_list( S2 ) ->
	true;

are_of_same_string_type( S1, S2 )
				when is_binary( S1 ), is_binary( S2 ) ->
	true;

are_of_same_string_type( _S1, _S2 ) ->
	false.



-doc """
Returns a list of words obtained from the breaking of the specified word,
according to the specified maximum width.

Parts of that word will use a separating dash.

For example: `break_word("simulator", 5)` returns `["simu-", "lator"]`.
""".
break_word( Word, Width ) ->

	% We do not want to have underscores in the word, as if the word happens
	% to be broken just after an underscore, RST will interpret it as a link.
	% Therefore we escape underscores:
	%
	% Used to cut into halves, then preferring truncating a first full-length
	% chunk, finally directly cutting the word into appropriate pieces:
	% CutIndex = erlang:length(Word) div 2,
	% CutIndex = Width-1,
	cut_into_chunks( Word, Width, _Acc=[] ).



-doc """
Cuts the specified string into pieces, each of them having to fit in the
specified width.
""".
cut_into_chunks( _String=[], _ChunkSize, Acc ) ->
	%io:format( "cut_into_chunks return ~p.", [ lists:reverse( Acc ) ] ),
	lists:reverse( Acc );

% Last word may take the full width (no dash to add):
cut_into_chunks( String, ChunkSize, Acc )
								when erlang:length( String ) =< ChunkSize ->
	cut_into_chunks( [], ChunkSize, [ String | Acc ] );

% Here we have to cut the string anyway:
cut_into_chunks( String, ChunkSize, Acc ) ->

	% Rule is to add (and convert) characters until the end of line:
	% (ChunkSize decremented as "-" will be added)

	{ FirstPart, Remaining } = aggregate_word( String, ChunkSize-1, [] ),

	% Each underscore will result into another character (\) being added:
	%io:format( "FirstPart = '~ts' (~B), Remaining = '~ts'.~n",
	%   [ FirstPart, erlang:length( FirstPart ), Remaining ] ),
	cut_into_chunks( Remaining, ChunkSize, [ FirstPart ++ "-" | Acc ] ).


% Aggregates a word.
aggregate_word( String, _Count=0, Acc ) ->
	{ lists:reverse( Acc ), String };


% An underscore once escaped would not fit, as it would result into two
% characters ('\_'):
%
aggregate_word( String=[ $_ | _T ], _Count=1, Acc ) ->
	aggregate_word( String, 0, Acc );

% An escaped underscore will fit:
aggregate_word( _String=[ $_ | T ], Count, Acc ) ->
	% Adding '_\' as it will reversed (into the expected '\_'):
	aggregate_word( T, Count-2, [ $\_, $\\ | Acc ] );

aggregate_word( _String=[ H | T ], Count, Acc ) ->
	aggregate_word( T, Count-1, [ H | Acc ] ).



-doc """
Tries to convert the specified Unicode-related datastructure into a flat, plain
Unicode string.

(exported helper, for re-use)
""".
-spec try_convert_to_unicode_list( unicode_data() ) -> option( ustring() ).
try_convert_to_unicode_list( Data ) ->

	% A binary_to_list/1 would not be sufficient here.

	% It seems that using io_lib:format( "~ts", [ Data ] ) could still be an
	% option.

	% Possibly a deep list:
	case unicode:characters_to_list( Data ) of

		Str when is_list( Str ) ->
			Str;

		_ ->
			undefined

	end.



-doc """
Converts the specified Unicode-related datastructure into a flat, plain Unicode
string.

Never fails yet can return a bogus string.

(exported helper, for re-use)
""".
-spec to_unicode_list( unicode_data() ) -> ustring().
to_unicode_list( Data ) ->
	to_unicode_list( Data, _CanFail=false ).


-doc """
Converts the specified Unicode-related datastructure into a flat, plain Unicode
string.

If enabled, fails if the conversion cannot be properly done, otherwise can
return a bogus string.

Note that at least some ISO-8859 contents (thus non-Unicode) may not properly be
converted by this function; use `binary_to_list(Data)` (or `io_lib:format("~ts",
[Data])`) instead.

(exported helper, for re-use)
""".
-spec to_unicode_list( unicode_data(), boolean() ) -> ustring().
to_unicode_list( Data, CanFail ) ->

	% A binary_to_list/1 would not be sufficient here.

	% Possibly a deep list:
	case unicode:characters_to_list( Data ) of

		Str when is_list( Str ) ->
			Str;

		{ error, Prefix, Remaining } ->

			trace_bridge:error_fmt( "Cannot transform data '~p' into "
				"a proper Unicode string:~nafter prefix '~ts', "
				"cannot convert '~w'.~nStacktrace was: ~ts",
				[ Data, Prefix, Remaining,
				  code_utils:interpret_shortened_stacktrace( 1 ) ] ),

			case CanFail of

				true ->
					throw( { improper_data_for_string, Data, Prefix,
							 Remaining } );

				false ->
					% Best effort:
					io_lib:format( "~ts## SUFFIX COULD NOT BE CONVERTED",
								   [ Prefix ] )

			end;

		{ incomplete, Prefix, Bin } ->

			trace_bridge:error_fmt( "Cannot transform data '~p' into "
				"a proper Unicode string:~nafter prefix '~ts', "
				"'~p' is incomplete.", [ Data, Prefix, Bin ] ),

			case CanFail of

				true ->
					throw( { incomplete_data_for_string, Data, Prefix, Bin } );

				false ->
					% Best effort:
					io_lib:format( "~ts## A SUFFIX WAS LACKING", [ Prefix ] )

			end

	end.



-doc """
Tries to convert the specified Unicode-related datastructure into a Unicode
binary string.

(exported helper, for re-use)
""".
-spec try_convert_to_unicode_binary( unicode_data() ) -> option( bin_string() ).
try_convert_to_unicode_binary( Data ) ->

	% A list_to_binary/1 would not be sufficient here.

	% Possibly a deep list:
	case unicode:characters_to_binary( Data ) of

		Bin when is_binary( Bin ) ->
			Bin;

		_Other ->
			%trace_utils:debug_fmt( "For '~p', got:~n~p", [ Data, Other ] ),
			undefined

	end.



-doc """
Converts the specified Unicode-related datastructure into a flat, plain Unicode
binary string.

Never fails yet can return a bogus string.

(exported helper, for re-use)
""".
-spec to_unicode_binary( unicode_data() ) -> bin_string().
to_unicode_binary( Data ) ->
	to_unicode_binary( Data, _CanFail=false ).



-doc """
Converts the specified Unicode-related datastructure into a flat, plain Unicode
binary string.

If enabled, fails if the conversion cannot be properly done, otherwise can
return a bogus string.

(exported helper, for re-use)
""".
-spec to_unicode_binary( unicode_data(), boolean() ) -> bin_string().
to_unicode_binary( Data, CanFail ) ->

	% A list_to_binary/1 would not be sufficient here.

	% Possibly a deep list:
	case unicode:characters_to_binary( Data ) of

		Bin when is_binary( Bin ) ->
			Bin;

		{ error, Prefix, Remaining } ->

			trace_bridge:error_fmt( "Cannot transform data '~p' into "
				"a proper Unicode binary:~nafter prefix '~ts', "
				"cannot convert '~p'.", [ Data, Prefix, Remaining ] ),

			case CanFail of

				true ->
					throw( { improper_data_for_binary, Data, Prefix,
							 Remaining } );

				false ->
					% Best effort; hopefully relevant:
					list_to_binary( io_lib:format(
						"~ts## SUFFIX COULD NOT BE CONVERTED", [ Prefix ] ) )

			end;

		{ incomplete, Prefix, Bin } ->
			trace_bridge:error_fmt( "Cannot transform data '~p' into "
				"a proper Unicode binary:~nafter prefix '~ts', "
				"'~p' is incomplete.", [ Data, Prefix, Bin ] ),
			case CanFail of

				true ->
					throw( { incomplete_data_for_binary, Data, Prefix, Bin } );

				false ->
					% Best effort; hopefully relevant:
					list_to_binary( io_lib:format(
						"~ts## A SUFFIX WAS LACKING", [ Prefix ] ) )

			end

	end.



% Restructured-Text (RST) related functions.


-doc """
Generates a RST-compatible standard title, with the proper ASCII art.

Follows our general conventions regarding title level, from H1 to Hn.
""".
-spec generate_title( ustring(), 1..9 ) -> ustring().
generate_title( Title, Level ) ->

	{ Char, Layout } = get_title_rendering_for( Level ),

	TitleLine = get_line_of( Char, erlang:length( Title ) ) ++ "\n",

	case Layout of

		only_below ->
			Title ++ "\n" ++ TitleLine ++ "\n";

		below_and_on_top ->
			TitleLine ++ Title ++ "\n" ++ TitleLine ++ "\n"

	end.



-doc """
Returns how a title with the specified level can be rendered.

See `demo-for-css-testing.rst` for the convention.
""".
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



-doc """
Returns a line made of Length characters `Character`.

For example: `get_line_of($+, 5) = "+++++"`.
""".
get_line_of( Character, Length ) ->
	%lists:flatten( [ Character || _X <- lists:seq( 1, Length ) ] ).
	lists:duplicate( Length, Character ).




% Miscellaneous functions.


-doc """
Tries to return a string adequate to form a simple name (mostly alphanumerical
with underscores) from the specified term.

See also: `file_utils:convert_to_filename/1`.
""".
-spec generate_text_name_from( term() ) -> ustring().
generate_text_name_from( Term ) ->
	String = term_to_string( Term ),
	fix_characters( String ).




% Non-exported helper functions.

% Ensures that no undesirable character (space or single quote) remains in the
% returned string.
%
fix_characters( String ) ->
	lists:reverse( fix_characters( lists:flatten( String ), _Acc=[] ) ).


fix_characters( _S=[], Acc ) ->
	Acc;

% 32 corresponds to space ('$ '):
fix_characters( _S=[ 32 | T ], Acc ) ->
	fix_characters( T, [ "_" | Acc ] );

fix_characters( _S=[ $' | T ], Acc ) ->
	fix_characters( T, [ "_" | Acc ] );

fix_characters( _S=[ H | T ], Acc ) ->
	fix_characters( T, [ H | Acc ] ).




% As too often (e.g. with gen_statem) no relevant origin location is specified:


-doc "Reports that the specified term is not a plain string.".
-spec report_not_a_string( any() ) -> no_return().
report_not_a_string( Term ) ->
	report_wrong_type( not_a_string, Term ).



-doc "Reports that the specified term is not a binary string.".
-spec report_not_a_binary_string( any() ) -> no_return().
report_not_a_binary_string( Term ) ->
	report_wrong_type( not_a_binary_string, Term ).



-doc "Reports that the specified term is not a list.".
-spec report_not_a_list( any() ) -> no_return().
report_not_a_list( Term ) ->
	report_wrong_type( not_a_list, Term ).



-doc "Reports that the specified term is not a number.".
-spec report_not_a_number( any() ) -> no_return().
report_not_a_number( Term ) ->
	report_wrong_type( not_a_number, Term ).



-doc "Allows to report at runtime a wrong type, with or without a stacktrace.".
-spec report_wrong_type( atom(), term() ) -> no_return().

-ifdef(myriad_add_stacktraces).

report_wrong_type( NotThisType, Term ) ->

	% Not wanting the stacktrace to include these last error-reporting
	% functions:
	%
	Stacktrace = code_utils:get_stacktrace( _SkipLastElemCount=2 ),

	throw( { NotThisType, Term, { stacktrace, Stacktrace } } ).

-else. % myriad_add_stacktraces

report_wrong_type( NotThisType, Term ) ->
	throw( { NotThisType, Term } ).

-endif. % myriad_add_stacktraces
