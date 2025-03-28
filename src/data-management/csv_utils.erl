% Copyright (C) 2018-2025 Olivier Boudeville
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
% Creation date: Thursday, February 22, 2018.

-module(csv_utils).

-moduledoc """
Management of **CSV** (*Comma-Separated Values*) data.

Note that such a data format is not at the state of the art: it is very basic
if not rudimentary, not formalised and quite limited (no meta-data, escaping
being required, etc.).

See also <https://en.wikipedia.org/wiki/Comma-separated_values>.

See csv_utils_test.erl for the corresponding test.
""".


% Usual extension of CSV files, which we recommend:
-define( csv_extension, ".csv" ).



-doc """
The field separator (delimiter) used between the values in a row (often comma,
 sometimes semicolon or alike).
""".
-type separator() :: char().


% The default separator of CSV is, well, a comma (even with Excel):
-define( default_separator, $, ).


-doc """
The value V, in "CSV".
""".
-type value() :: any().



-doc """
A row of a CSV content, as a tuple of values.

Logically a tuple, often more convenient as a list, yet kept as a tuple as more
compact in memory.
""".
-type row() :: tuple().  % tuple( value() ).



-doc "A CSV content, as an (ordered) list of rows.".
-type content() :: [ row() ].



-doc "A count of rows.".
-type row_count() :: count().



-doc """
The number of a row in a row stream (akin to a line number in a file).

Starts at #1.
""".
-type row_number() :: count().



-doc """
A CSV content, as an (ordered) list of rows that match a row spec (e.g.
expected number of fields) or not (then represented as a "non-matching" triplet.
""".
-type mixed_content() :: [ row() | { 'non_matching', row_number(), row() } ].



-doc "A number of fields in a row.".
-type field_count() :: count().



-doc """
The result of the parsing of a CSV file, comprised of:
- the corresponding content, as an ordered list of rows
- the number of rows found
- the number of fields each row has
""".
-type reading_outcome() :: { Rows :: content(), RowCount :: row_count(),
							 FieldCount :: field_count() }.


-export_type([ separator/0, value/0, row/0, content/0,
			   row_count/0, row_number/0,
			   field_count/0,
			   reading_outcome/0 ]).



% The default read-ahead size for CSV files:
-define( ahead_size, 512*1024 ).


% For some reason, if relying on the '-noinput' option, using the following
% options used to result in {read_error,{no_translation,unicode,unicode},...},
% whereas using io:setopts/1 (e.g. possibly through
% system_utils:force_unicode_support/0) afterwards did not fail and allowed
% reads to return correctly-encoded lines:
%
% (additionally, even when forcing UTF8 encoding when exporting as CSV an Excel
% spreadsheet, the same ISO-8859 content will be obtained; )
%
%-define( read_options,
%   [ read, { read_ahead, ?ahead_size }, { encoding, utf8 } ] ).
%
% However, when not specifying { encoding, utf8 }, the fields read from a CSV
% file determined to be 'UTF-8 Unicode text' would be able to be matched with
% corresponding strings, despite the corresponding source file being itself
% 'UTF-8 Unicode text'.
%
% Starting a source file with '%% coding: utf-8' should not change anything, yet
% it does (strings are displayed differently).
%
% We also believe that, at least with older versions of LibreOffice (5.2.7.2),
% saving a spreadsheet as a CSV file with the UTF-8 character set may lead to
% garbled encodings (one may just try to do the same with Excel).
%
% This work-around is still necessary (still true with Erlang 23.0):
%-define( read_options, [ read, { read_ahead, ?ahead_size } ] ).
-define( read_options,
		 [ read, { read_ahead, ?ahead_size }, { encoding, utf8 } ] ).



% Defines what are the characters that denote a start/end of quoting in CVS
% files:
%
% (currently, only double quotes; single ones could be added)
%
-define( quoting_characters, [ $" ] ).


% Defines what are the characters that are used to escape quoting characters in
% CVS files:
%
% (currently, only backslash)
%
-define( escaping_characters, [ $\\ ] ).
%-define( escaping_characters, [] ).


% A CSV file is seen here as a series of rows ending with a newline, i.e. '\n'.
%
% Any empty row (either blank or containing only whitespaces) or corresponding
% to a comment (i.e. whose first non-blank character is '#') will be dropped
% (ignored); it is designated here as a dropping row.
%
% Each row contains a series of values (possibly non-defined, i.e. empty)
% delimited by a separator character (usually a comma, i.e. ',').
%
% Each value can be enclosed with single (') or double quote ("), and should not
% contain any newline.
%
% Any \r is ignored.
%
% All lines (rows) are expected to contain the same number of values (fields).



% Implementation notes:
%
% Our version of the CSV format supports:
%
% - non-comma separators
%
% - comments (lines starting with the # character, potentially with leading
% whitespaces are ignored)
%
% - blank lines
%
% - larger CSV files (they are not read as a whole first, they are parsed line
% by line)
%
% We used to represent rows as lists (a convenient datatype), now we prefer (at
% least for fixed-sized ones) representing them, more logically, as tuples.


% Future improvements:
%
% - performs an actual parsing, so that a field of type 'string' may contain the
% separator (e.g. if the separator is ',', a field could then be "hello, you!"),
% and be possibly defined over multiple lines
%
% - read from a compressed file
%
% - support character encoding


-export([

	% For regular, homogeneous CSV files, supposing the separator known a
	% priori, but not the number of fields to expect :
	%
	read_file/1, read_file/2,

	% For CSV files that shall be filtered before use (e.g. with some rows
	% obeying different rules):

	% If the separator and number of fields to expect are known a priori:
	interpret_file/3,

	% If the separator is known a priori, but not the number of fields to
	% expect:
	%
	interpret_file/2,

	% If neither the separator nor the number of fields to expect is known a
	% priori:
	%
	interpret_file/1,


	describe_non_matching_rows_in/1,

	% For CSV files that shall be filtered before use (e.g. with some rows
	% obeying different rules):
	%
	write_file/2, write_file/3,

	get_usual_separators/0,

	check_all_empty/1, are_all_empty/1,

	content_to_string/1 ]).



% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type line() :: ustring().

-type any_file_path() :: file_utils:any_file_path().
-type file() :: file_utils:file().



-doc """
Reads the specified file, expected to be in the CSV format and supposed to be
homogeneous (all non-dropping rows having the same number of fields - otherwise
an exception is raised), using the default separator.

Returns {Rows, RowCount, FieldCount}, see reading_outcome().
""".
-spec read_file( any_file_path() ) -> reading_outcome().
read_file( FilePath ) ->
	read_file( FilePath, ?default_separator ).



-doc """
Reads the specified file, expected to be in the CSV format and supposed to be
homogeneous (all non-dropped rows having the same number of fields - otherwise
an exception is raised), using the specified separator.

Returns {Rows, RowCount, FieldCount}, see reading_outcome().
""".
-spec read_file( any_file_path(), separator() ) -> reading_outcome().
read_file( FilePath, Separator ) when is_integer( Separator ) ->

	File = get_file_for_reading( FilePath ),

	Res = { _Content, _RowCount, _FieldCount } = read_rows( File, Separator ),

	%trace_utils:debug_fmt( "Read content (~B rows, each having ~p fields): "
	%   "~n ~p", [ RowCount, FieldCount, Content ] ),

	file_utils:close( File ),

	Res.



-doc """
Interprets the specified file, based on the specified separator and number of
fields per row.

Returns {MixedContent, MatchingCount, UnmatchCount, DropCount}, where:

- MixedContent is a list (respecting the in-file order), each element of which
being either a row (that is a tuple of the legit number of values), or a triplet
whose first element is the 'non_matching' atom, whose second element is the
in-file number of the corresponding row and whose third element is the
corresponding row (hence a tuple whose elements were obtained based on the same
separator - yet in a different number than the expected one)

- MatchingCount is the number of rows that match the row spec (i.e. the
specified number of fields), based on the specified separator

- UnmatchingCount is the number of rows that do not match said constraints

- DropCount is the number of rows that were dropped (typically because they were
either empty or containing a comment)
""".
-spec interpret_file( any_file_path(), separator(), field_count() ) ->
		{ MixedContent :: mixed_content(), MatchingCount :: row_count(),
		  UnmatchingCount :: row_count(), DropCount :: row_count() }.
interpret_file( FilePath, Separator, ExpectedFieldCount )
												when is_integer( Separator ) ->

	File = get_file_for_reading( FilePath ),

	%{ MixedContent, MatchingCount, UnmatchingCount, DropCount } =
	Res = interpret_rows( File, Separator, ExpectedFieldCount ),

	%trace_utils:debug_fmt( "Read mixed content (matching count: ~B, "
	%   "unmatching count: ~B, drop count: ~B):~n ~p",
	%   [ MatchingCount, UnmatchingCount, DropCount, MixedContent ] ),

	file_utils:close( File ),

	Res.



-doc """
Interprets the specified file, based on the specified separator, with no prior
knowledge about the number of fields per row.

Returns {FieldCount, MixedContent, MatchingCount, UnmatchCount, DropCount}
(i.e. the same as interpret_file/3, except that the detected field count is
prepended):

- FieldCount is the number of fields in each row, as determined thanks to the
first non-dropped row

- MixedContent: refer to interpret_file/3 for a detailed description

- MatchingCount is the number of rows that match the row spec (i.e. the
specified number of fields), based on the specified separator

- UnmatchingCount is the number of rows that do not match said constraints

- DropCount is the number of rows that were dropped (typically because they were
either empty or containing a comment)
""".
-spec interpret_file( any_file_path(), separator() ) ->
		{ FieldCount :: field_count(), MixedContent :: mixed_content(),
		  MatchingCount :: row_count(), UnmatchingCount :: row_count(),
		  DropCount :: row_count() }.
interpret_file( FilePath, Separator ) when is_integer( Separator ) ->

	File = get_file_for_reading( FilePath ),

	{ FirstRow, FieldCount, GuessDropCount } =
		guess_field_count( File, Separator, _InitialDropCount=0 ),

	% Branch to the helper with a correct initial state:
	{ MixedContent, MatchCount, UnmatchCount, DropCount } =
		interpret_rows( _Device=File, Separator, FieldCount,
			_MatchCount=1, _UnmatchCount=0, GuessDropCount, _RowCount=1,
			_Acc=[ FirstRow ] ),

	%trace_utils:debug_fmt( "Read mixed content with detected field count ~B "
	%   "(matching count: ~B, unmatching count: ~B, drop count: ~B):~n ~p",
	%   [ FieldCount, MatchCount, UnmatchCount, DropCount, MixedContent ] ),

	file_utils:close( File ),

	{ FieldCount, MixedContent, MatchCount, UnmatchCount, DropCount }.



-doc """
Interprets the specified file, with no prior knowledge about the separator or
the number of fields per row.

Returns {Separator, FieldCount, MixedContent, MatchingCount, UnmatchCount,
DropCount}, i.e. the same as interpret_file/4, except that the detected
separator is prepended):

- Separator is the most likely separator in use

- FieldCount is the number of fields in each row, as determined thanks to the
first non-dropped row

- MixedContent: refer to interpret_file/3 for a detailed description

- MatchingCount is the number of rows that match the row spec (i.e. the
specified number of fields), based on the detected separator

- UnmatchingCount is the number of rows that do not match said constraints

- DropCount is the number of rows that were dropped (typically because they were
either empty or containing a comment)
""".
-spec interpret_file( any_file_path() ) ->
		{ separator(), field_count(), mixed_content(), row_count(),
		  row_count(), row_count() }.
interpret_file( FilePath ) ->

	File = get_file_for_reading( FilePath ),

	{ FirstRow, Separator, FieldCount, FirstDropCount } =
		guess_separator_and_field_count( File, _InitialDropCount=0 ),

	cond_utils:if_defined( myriad_debug_csv_support,
		trace_utils:debug_fmt( "First row: '~p', separator: '~ts', "
			"field count: ~B, first drop count: ~B.",
			[ FirstRow, [ Separator ], FieldCount, FirstDropCount ] ) ),

	% Branch to the helper with a correct initial state:
	{ MixedContent, MatchCount, UnmatchCount, DropCount } =
		interpret_rows( _Device=File, Separator, FieldCount,
			_MatchCount=1, _UnmatchCount=0, FirstDropCount, _RowCount=1,
			_Acc=[ FirstRow ] ),

	% Full version with content:
	%cond_utils:if_defined( myriad_debug_csv_support,
	%	trace_utils:debug_fmt( "Read mixed content with detected separator "
	%		"'~ts' and field count ~B (matching count: ~B, unmatching "
	%		"count: ~B, drop count: ~B):~n ~p",
	%		[ [ Separator ], FieldCount, MatchCount, UnmatchCount, DropCount,
	%		  MixedContent ] ) ),

	% Summary:
	cond_utils:if_defined( myriad_debug_csv_support,
		trace_utils:debug_fmt( "Read mixed content with detected separator "
			"'~ts' and field count ~B (matching count: ~B, "
			"unmatching count: ~B, drop count: ~B).",
			[ [ Separator ], FieldCount, MatchCount, UnmatchCount,
			  DropCount ] ) ),

	file_utils:close( File ),

	{ Separator, FieldCount, MixedContent, MatchCount, UnmatchCount,
	  DropCount }.




% Helper section.


-doc "Returns the context read from specified device/file.".
-spec read_rows( file(), separator() ) ->
						{ content(), row_count(), field_count() }.
read_rows( File, Separator ) ->
	read_rows( _Device=File, Separator, _RowCount=0, _FieldCount=undefined,
			   _Acc=[] ).


% (helper)
read_rows( Device, Separator, RowCount, FieldCount, Acc ) ->

	case io:get_line( Device, _Prompt="" ) of

		eof  ->
			file_utils:close( Device ),
			Content = lists:reverse( Acc ),
			{ Content, RowCount, FieldCount };

		{ error, Error } ->
			throw( { read_error, Error } );

		Line ->

			case parse_row( Line, Separator ) of

				{ Values, ThisFieldCount } ->

					%trace_utils:debug_fmt( "For line '~ts', "
					%     "~B field(s) found.", [ Line, ThisFieldCount ] ),

					NewFieldCount = case FieldCount of

						undefined ->
							ThisFieldCount;

						ThisFieldCount ->
							ThisFieldCount;

						_OtherFieldCount ->
							trace_utils:error_fmt(
								"Non matching line (row #~B): '~ts'.",
								[ RowCount, Line ] ),

							throw( { non_uniform_field_count,
									 { FieldCount, ThisFieldCount } } )

					end,

					%trace_utils:debug_fmt( "Read ~B fields: ~p.",
					%                       [ ThisFieldCount, Values ] ),

					read_rows( Device, Separator, RowCount+1, NewFieldCount,
							   [ Values | Acc ] );

				dropped ->
					read_rows( Device, Separator, RowCount, FieldCount, Acc )


			end

	end.



-doc """
Guesses the number of fields per row, and returns also the first read line so
that the rest of the file can be read in the same movement.

(helper)
""".
-spec guess_field_count( file(), separator(), row_count() ) ->
								{ row(), field_count(), row_count() }.
guess_field_count( Device, Separator, DropCount ) ->

	case io:get_line( Device, _Prompt="" ) of

		eof ->
			% Nothing can be determined in this case:
			throw( empty_csv_file );

		{ error, Error } ->
			throw( { read_error, Error } );

		Line ->
			case parse_row( Line, Separator ) of

				dropped ->
					guess_field_count( Device, Separator, DropCount+1 );

				{ Values, FieldCount } ->
					{ Values, FieldCount, DropCount }

			end

	end.



-doc """
Guesses the separator and number of fields per row, and returns also the first
read line so that the rest of the file can be read in the same movement.

(helper)
""".

-spec guess_separator_and_field_count( file(), row_count() ) ->
						{ row(), separator(), field_count(), row_count() }.
guess_separator_and_field_count( Device, DropCount ) ->

	case io:get_line( Device, _Prompt="" ) of

		eof ->
			% Nothing can be determined in this case:
			throw( empty_csv_file );

		{ error, Error } ->
			throw( { read_error, Error } );

		Line ->
			%trace_utils:debug_fmt( "Read line '~ts'.", [ Line ] ),
			case parse_row_no_separator( Line ) of

				dropped ->
					guess_separator_and_field_count( Device, DropCount+1 );

				{ Values, Separator, FieldCount } ->
					{ Values, Separator, FieldCount, DropCount }

			end

	end.



-doc "Returns the context read from specified device/file.".
-spec interpret_rows( file(), separator(), field_count() ) ->
				{ mixed_content(), row_count(), row_count(), row_count() }.
interpret_rows( File, Separator, ExpectedFieldCount ) ->
	interpret_rows( _Device=File, Separator, ExpectedFieldCount, _MatchCount=0,
					_UnmatchCount=0, _DropCount=0, _RowNumber=1, _Acc=[] ).


% (helper)
interpret_rows( Device, Separator, ExpectedFieldCount, MatchCount, UnmatchCount,
				DropCount, RowNumber, Acc ) ->

	case io:get_line( Device, _Prompt="" ) of

		eof ->
			file_utils:close( Device ),
			MixedContent = lists:reverse( Acc ),
			{ MixedContent, MatchCount, UnmatchCount, DropCount };

		{ error, Error } ->
			throw( { read_error, Error } );

		Line ->

			%io:format( "Read line #~B: '~ts'.", [ RowCount, Line ] ),

			case parse_row( Line, Separator ) of

				% Matching:
				{ Values, ExpectedFieldCount } ->

					%trace_utils:debug_fmt( "Read matching row: ~p.",
					%                       [ Values ] ),

					interpret_rows( Device, Separator, ExpectedFieldCount,
						MatchCount+1, UnmatchCount, DropCount, RowNumber+1,
						[ Values | Acc ] );


				% Not matching:
				{ Values, OtherFieldCount } ->

					trace_utils:debug_fmt( "Read non-matching row #~B:~n~ts~n"
						"(i.e. '~p', which has ~B fields)",
						[ RowNumber, Line, Line, OtherFieldCount ] ),

					interpret_rows( Device, Separator, ExpectedFieldCount,
						MatchCount, UnmatchCount+1, DropCount, RowNumber+1,
						[ { non_matching, RowNumber, Values } | Acc ] );


				dropped ->
					interpret_rows( Device, Separator, ExpectedFieldCount,
						MatchCount, UnmatchCount, DropCount+1, RowNumber+1,
						Acc )

			end

	end.



-doc """
Parses the specified line into a proper row, guessing the most likely separator
(that is that not known).
""".
-spec parse_row_no_separator( line() ) ->
					'dropped' | { row(), separator(), field_count() }.
parse_row_no_separator( Line ) ->

	% Useful also to remove the ending newline:
	TrimmedLine = text_utils:trim_whitespaces( Line ),

	case TrimmedLine of

		[] ->
			%trace_utils:debug( "Dropped blank line" ),
			dropped;


		[ $# | _ ] ->
			%trace_utils:debug_fmt( "Dropped following comment: '~ts'.",
			%                       [ Line ] ),
			dropped;

		_ ->
			GuessedSep = guess_separator_from( TrimmedLine ),

			%trace_utils:debug_fmt( "Guessed separator: '~ts'.",
			%                       [ [GuessedSep] ] ),

			Values = parse_line( TrimmedLine, GuessedSep ),

			FieldCount = length( Values ),
			{ list_to_tuple( Values ), GuessedSep, FieldCount }

	end.



-doc """
Parses the specified line into a proper row, based on the specified separator.
""".
-spec parse_row( line(), separator() ) -> 'dropped' | { row(), field_count() }.
parse_row( Line, Separator ) ->

	% Useful also to remove at least the ending newline:
	case text_utils:trim_whitespaces( Line )  of

		[] ->
			%trace_utils:debug( "Dropped blank line" ),
			dropped;


		[ $# | _ ] ->
			%trace_utils:debug_fmt( "Dropped following comment: '~ts'.",
			%                       [ Line ] ),
			dropped;

		TrimmedLine ->
			Values = parse_line( TrimmedLine, Separator ),
			FieldCount = length( Values ),
			{ list_to_tuple( Values ), FieldCount }

	end.



-doc """
Parsing allows to see quoted sequences as a single, opaque element in which any
presence of the separator is ignored.
""".
-spec parse_line( line(), separator() ) -> [ value() ].
parse_line( Line, Separator ) ->

	% Allows not to consider as unmatched the lines that happen to have, in a
	% quote, the separator in use (which, in this case, shall be considered as
	% any other character):

	%trace_utils:debug_fmt( "A. Parsing line: '~ts'.~n", [ Line ] ),

	% First consider each quoted sequence as a single element:
	ParseLine = text_utils:parse_quoted( Line, ?quoting_characters,
										 ?escaping_characters ),

	%trace_utils:debug_fmt( "B. Parsed line elements:~n ~p.~n", [ ParseLine ] ),

	% Now split with the separator, respecting quoted elements:
	SplitStrs = text_utils:split_parsed( ParseLine, [ Separator ] ),

	%trace_utils:debug_fmt( "C. Split strings: ~ts",
	%                       [ text_utils:strings_to_string( SplitStrs ) ] ),

	SplitStrs.



-doc """
Checks that specified row or list of values (typically coming from a row of
unspecified field count) contains only empty values (empty strings).
""".
-spec check_all_empty( row() | [ value() ] ) -> void().
check_all_empty( Row ) when is_tuple( Row ) ->
	check_all_empty( tuple_to_list( Row ) );

check_all_empty( _List=[] ) ->
	ok;

check_all_empty( _List=[ "" | T ] ) ->
	check_all_empty( T );

check_all_empty( _List=[ H | _T ] ) ->
	throw( { non_empty_value, H } ).



-doc "Returns whether the specified list of values contains only empty ones.".
-spec are_all_empty( row() | [ value() ] ) -> boolean().
are_all_empty( Row ) when is_tuple( Row ) ->
	are_all_empty( tuple_to_list( Row ) );

are_all_empty( [] ) ->
	true;

are_all_empty( [ "" | T ] ) ->
	are_all_empty( T );

are_all_empty( [ _H | _T ] ) ->
	false.



-doc "Returns the most usual separators used in CSV files.".
-spec get_usual_separators() -> [ separator() ].
get_usual_separators() ->
	[ $,, $; ].



-doc "Determines the separator used in specified line.".
-spec guess_separator_from( line() ) -> separator().
guess_separator_from( Line ) ->
	%trace_utils:debug_fmt( "Guessing separator used in '~ts'...", [ Line ] ),
	SepPairs = gather_potential_separators( Line ),
	select_most_likely_separator( SepPairs ).


gather_potential_separators( Line ) ->
	evaluate_separators_on( _Seps=get_usual_separators(), Line, _SepAcc=[] ).


% (helper)
evaluate_separators_on( _Seps=[], _Line, SepAcc ) ->
	SepAcc;

evaluate_separators_on( _Seps=[ Sep | T ], Line, SepAcc ) ->
	SubStrings = text_utils:split( Line, _Delimiter=Sep ),
	NewSepAcc = [ { Sep, length( SubStrings ) - 1 } | SepAcc ],
	evaluate_separators_on( T, Line, NewSepAcc ).


% (helper)
select_most_likely_separator( SepPairs ) ->

	%trace_utils:debug_fmt( "Separator pairs: ~w.", [ SepPairs ] ),

	% Returns the separator having the higher number of occurrences:
	{ Sep, _Count } = list_utils:get_last_element(
		lists:keysort( _CountIndex=2, SepPairs ) ),

	Sep.



-doc """
Writes the specified content, a list of row tuples whose elements are serialised
according to the standard Erlang syntax (as ~w), in the specified CSV file,
using the default separator for that.

Rows may have a different number of elements.

In terms of file extension, we recommend the one specified through our
csv_extension define.
""".
-spec write_file( content(), any_file_path() ) -> void().
write_file( Content, TargetFilePath ) ->
	write_file( Content, TargetFilePath, ?default_separator ).



-doc """
Writes the specified content, a list of row tuples whose elements are serialised
according to the standard Erlang syntax (as ~w), in the specified CSV file,
using the specified separator for that.

Rows may have a different number of elements.

In terms of file extension, we recommend the one specified through our
csv_extension define.
""".
-spec write_file( content(), any_file_path(), separator() ) -> void().
write_file( Content, TargetFilePath, Separator ) ->

   file_utils:exists( TargetFilePath ) andalso
		throw( { already_existing_csv_file,
				 text_utils:ensure_string( TargetFilePath ) } ),

	WriteOpts = [ write, raw, delayed_write ],

	File = file_utils:open( TargetFilePath, WriteOpts ),

	% Add a space for readability:
	FullSeparator = [ Separator, $ ],

	% Specifically checking rows (in terms of datatypes, number of elements,
	% etc.) has little interest, as supporting heterogeneous rows may be a
	% feature; moreover the content data originates from the program and thus is
	% probably already correct by design.

	write_rows( Content, FullSeparator, File ),

	file_utils:close( File ).



% (helper)
write_rows( _Content=[], _Separator, _File ) ->
	ok;

write_rows( _Content=[ Row | T ], Separator, File ) ->

	Elems = tuple_to_list( Row ),

	CtrlSeqs = lists:duplicate( length( Elems ), "~w" ),

	FormatString = text_utils:join( Separator, CtrlSeqs ),

	Line = text_utils:format( FormatString, Elems ),

	file_utils:write_ustring( File, "~ts~n", [ Line ] ),

	write_rows( T, Separator, File ).



-doc "Returns a file handle to read specified file.".
-spec get_file_for_reading( any_file_path() ) -> file().
get_file_for_reading( FilePath ) ->

	%trace_utils:debug_fmt( "Opening '~ts' with options ~w.",
	%                       [ FilePath, ?read_options ] ),

	file_utils:is_existing_file_or_link( FilePath ) orelse
		throw( { csv_file_not_found, FilePath,
				 file_utils:get_current_directory() } ),

	File = file_utils:open( FilePath, ?read_options ),

	% Refer to the note in file_utils:open/2 for explanation:
	%
	% (possibly still needed, yet a test case with 25.3 could work without it,
	% if having {encoding, utf8} is the read options)
	%
	%system_utils:force_unicode_support(),

	File.



-doc "Returns a textual representation of the specified content.".
-spec content_to_string( content() ) -> ustring().
content_to_string( Content ) ->
	text_utils:format( "content of ~B rows: ~ts", [ length( Content ),
					   text_utils:terms_to_enumerated_string( Content ) ] ).


-doc """
Describes the non-matching triplets listed in the specified mixed content:
returns the number of non-matching rows, an overall description thereof, and an
ordered list of these rows.
""".
-spec describe_non_matching_rows_in( mixed_content() ) ->
								{ row_count(), ustring(), [ row() ] }.
describe_non_matching_rows_in( MixedContent ) ->
	% Preferring a single pass:
	describe_non_matching_rows_in( MixedContent, _AccDesc=[], _AccRows=[] ).


% (helper)
describe_non_matching_rows_in( _MixedContent=[], AccDesc, AccRows ) ->
	Count = length( AccRows ),
	Desc = text_utils:strings_to_string( lists:reverse( AccDesc ) ),
	Rows = lists:reverse( AccRows ),
	{ Count, Desc, Rows };

describe_non_matching_rows_in( _MixedContent=[
		{ non_matching, RowNumber, Row } | T ], AccDesc, AccRows ) ->

	NewAccDesc = [ text_utils:format( "row #~B had ~B field(s): ~p",
		[ RowNumber, size( Row ), Row ] ) | AccDesc ],

	describe_non_matching_rows_in( T, NewAccDesc, [ Row | AccRows ] );

describe_non_matching_rows_in( _MixedContent=[ _NormalRow | T ], AccDesc,
							   AccRows ) ->
	describe_non_matching_rows_in( T, AccDesc, AccRows ).
