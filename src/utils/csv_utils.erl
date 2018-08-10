% Copyright (C) 2018-2018 Olivier Boudeville
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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Creation date: Thursday, February 22, 2018.



% Management of CSV (Comma-Separated Values) data.
%
% Note that such a data format is not at the state of the art: it is very basic
% if not rudimentary, not formalised and quite limited (no meta-data, escaping
% being required, etc.).
%
% See also: https://en.wikipedia.org/wiki/Comma-separated_values.
%
% See csv_utils_test.erl for the corresponding test.
%
-module(csv_utils).


% Extension of CSV files:
-define( csv_extension, ".csv" ).


% The separator (delimiter) used between the values in a line (often comma,
% sometimes semicolon or alike):
%
-type separator() :: char().

% The default separator of CSV is, well, a comma:
-define( default_separator, $, ).


% The value V, in "CSV":
-type value() :: any().


% A row of a CSV content, as an (ordered) list of values:
-type row() :: [ value() ].

% A CSV content, as an (ordered) list of lines:
-type content() :: [ row() ].


% Number of rows:
-type row_count() :: basic_utils:count().

% Number of fields:
-type field_count() :: basic_utils:count().


-export_type([ separator/0, value/0, row/0, content/0,
			   row_count/0, field_count/0 ]).


% A CSV file is seen here as a series of lines ending with a newline, i.e. '\n'.
%
% Each line contains a series of values separated by separator (usually a comma,
% i.e. ',').
%
% Each value can be enclosed with single (') or double quote ("), and should not
% contain any newline.
%
% Each value can be empty.
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
% - comments (lines starting with the # character, potentially with leading whitespaces are ignored)
%
% - blank lines
%
% - larger CSV files (they are not read as a whole first, they are parsed line
% by line)



% Future improvements:
%
% - performs an actual parsing, so that a field of type 'string' may contain the
% separator (ex: if the separator is ',', a field could then be "hello, you!")
%
% - read from a compressed file
%
% - support character encoding


-export([ read_file/1, read_file/2,
		  %write_file/2, write_file/3
		  content_to_string/1 ]).



% Reads specified file (expected to be in CSV format; using the default
% separator), and returns the corresponding content.
%
-spec read_file( file_utils:filename() ) ->
					   { content(), row_count(), field_count() }.
read_file( Filename ) ->
	read_file( Filename, ?default_separator ).



% Reads specified file (expected to be in CSV format; using the specified
% separator), and returns the corresponding content.
%
-spec read_file( file_utils:filename(), separator() ) ->
					   { content(), row_count(), field_count() }.
read_file( Filename, Separator ) when is_list( Filename ) ->
									  %andalso is_char( Separator ) ->

	case file_utils:is_existing_file_or_link( Filename ) of

		true ->
			ok;

		false ->
			throw( { csv_file_not_found, Filename,
					 file_utils:get_current_directory() } )

	end,

	AheadSize = 512*1024,

	File = file_utils:open( Filename,
							_Options=[ read, { read_ahead, AheadSize } ] ),

	Res = { _Content, _RowCount, _FieldCount } = read_rows( File, Separator ),

	%trace_utils:debug_fmt( "Read content (~B rows, each having ~p fields):~n~p",
	%					   [ RowCount, FieldCount, Content ] ),

	file_utils:close( File ),

	Res.



% Returns the context read from specified device/file.
%
-spec read_rows( file_utils:file(), separator() ) ->
						{ content(), row_count(), field_count() }.
read_rows( File, Separator ) ->
	read_rows( _Device=File, Separator, _RowCount=0, _FieldCount=undefined,
			   _Acc=[] ).



% (helper)
%
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

					NewFieldCount = case FieldCount of

						undefined ->
							ThisFieldCount;

						ThisFieldCount ->
							ThisFieldCount;

						_OtherFieldCount ->
							trace_utils:error_fmt( "Non matching line: ~s",
												   [ Line ] ),

							throw( { non_uniform_field_count,
									 { FieldCount, ThisFieldCount } } )

					end,

					%trace_utils:debug_fmt( "Read ~B fields: ~p.",
					%					   [ ThisFieldCount, Values ] ),

					read_rows( Device, Separator, RowCount+1, NewFieldCount,
							   [ Values | Acc ] );

				dropped ->
					read_rows( Device, Separator, RowCount, FieldCount, Acc )


			end

	end.



% Parses specified line into a proper row.
%
-spec parse_row( text_utils:ustring(), char() ) ->
					   basic_utils:maybe( { row(), field_count() } ).
parse_row( Line, Separator ) ->

	% Useful also to remove ending newline:
	TrimmedLine = text_utils:trim_whitespaces( Line ),

	case TrimmedLine of

		[ $# | _ ] ->
			%trace_utils:debug_fmt( "Dropped following comment: '~s'.",
			%					   [ Line ] ),
			dropped;

		_ ->
			Values = text_utils:split( TrimmedLine, [ Separator ] ),
			FieldCount = length( Values ),
			{ Values, FieldCount }

	end.



% Returns a textual representation of specified content.
%
-spec content_to_string( content() ) -> text_utils:ustring().
content_to_string( Content ) ->
	text_utils:format( "content of ~B rows:~s", [ length( Content ),
					   text_utils:terms_to_enumerated_string( Content ) ] ).
