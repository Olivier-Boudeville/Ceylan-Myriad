% Copyright (C) 2019-2020 Olivier Boudeville
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
% Creation date: Tuesday, June 25, 2019.



% Gathering of services related to web content, notably for HTML generation.
%
% See web_utils_test.erl for the corresponding test.
%
-module(web_utils).


% Ex: "<p>Hello!</p>":
-type html_element() :: text_utils:any_string().


-export_type([ html_element/0 ]).


% HTTP-related section:

% URL subsection:
-export([ encode_as_url/1, encode_element_as_url/1, escape_as_url/1 ]).


% HTML-related section:
-export([ get_ordered_list/1, get_unordered_list/1,
		  escape_as_html_content/1, escape_term_as_html_content/1 ]).


% Shorthands:
-type ustring() :: text_utils:ustring().



% HTTP-related operations.


% About encoding.

% The character "è" (e with a grave accent, hex code: xE8) might be for example
% either translated as "%C3%A8" or as "%E8". It is apparently the difference
% between encodeURI(chr) and escape(chr) in Javascript.
%
% The most adequate encoding in general seems the first (in which case
% encode_as_url/1 and encode_element_as_url/1 shall be used), however some
% webservers seem to insist on having the second (in which case escape/1 and
% escape_element/1 shall be used).
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
% See also escape_as_url/1 for some more specific uses.
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
%
% Note: apparently useful only for quite specific websites; encode_as_url/1
% should be preferred in most cases.
%
-spec escape_as_url( option_list:option_list() ) -> ustring().
escape_as_url( OptionList ) ->
	%trace_utils:debug_fmt( "~n~nEscaping '~p'.", [ OptionList ] ),
	escape_as_url( OptionList, _Acc=[] ).

escape_as_url( _OptionList=[], Acc ) ->
	Acc;

% First entry:
escape_as_url( [ { Key, Value } | T ], _Acc=[] ) ->
	escape_as_url( T, escape_key( Key ) ++ "=" ++ escape_value( Value ) );

escape_as_url( [ { Key, Value } | T ], Acc ) ->
	escape_as_url( T, Acc ++ "&" ++ escape_key( Key ) ++ "="
			++ escape_value( Value ) ).



% Escapes specified element so that it can be used in some URL.
-spec escape_key( option_list:key() ) -> ustring().
escape_key( Key ) when is_atom( Key ) ->
	text_utils:atom_to_string( Key ).


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



% Returns the HTML code of an ordered (numbered bullets) list corresponding to
% specified list of elements.
%
-spec get_ordered_list( [ html_element() ] ) -> html_element().
get_ordered_list( Elements ) ->

	HTMLElems = [ text_utils:format( "    <li>~s</li>~n", [ E ] )
				  || E <- Elements ],

	text_utils:format( "  <ol>~n~s  </ol>~n", [ lists:flatten( HTMLElems ) ] ).



% Returns the HTML code of an unordered list corresponding to specified list of
% elements.
%
-spec get_unordered_list( [ html_element() ] ) -> html_element().
get_unordered_list( Elements ) ->

	HTMLElems = [ text_utils:format( "    <li>~s</li>~n", [ E ] )
				  || E <- Elements ],

	text_utils:format( "  <ul>~n~s  </ul>~n", [ lists:flatten( HTMLElems ) ] ).



% Escapes specified text, so that it can be included safely as an HTML content.
-spec escape_as_html_content( text_utils:any_string() ) -> html_element().
escape_as_html_content( BinString ) when is_binary( BinString ) ->
	escape_as_html_content( text_utils:binary_to_string( BinString ) );

escape_as_html_content( String ) ->
	% Flatten needed if having an IO list as input:
	escape_as_html_content( unicode:characters_to_list( String ), _Acc=[] ).


% (helper)
escape_as_html_content( _String=[], Acc ) ->
	lists:reverse( unicode:characters_to_list( Acc ) );

% Replacements are pre-reversed:
escape_as_html_content( _String=[ $& | T ], Acc ) ->
	escape_as_html_content( T, [ ";pma&" | Acc ] ) ;

escape_as_html_content( _String=[ $< | T ], Acc ) ->
	escape_as_html_content( T, [ ";tl&" | Acc ] ) ;

escape_as_html_content( _String=[ $> | T ], Acc ) ->
	escape_as_html_content( T, [ ";tg&" | Acc ] ) ;


% These two clauses apply only inside of attribute values, yet are a general,
% safer measure:
%
escape_as_html_content( _String=[ $" | T ], Acc ) ->
	escape_as_html_content( T, [ ";touq&" | Acc ] ) ;

escape_as_html_content( _String=[ $' | T ], Acc ) ->
	escape_as_html_content( T, [ ";93#&" | Acc ] ) ;


% All others:
escape_as_html_content( _String=[ Other | T ], Acc ) ->
	escape_as_html_content( T, [ Other | Acc ] ).




% Escapes specified term (most probably non-string), so that it can be included
% safely as an HTML content.
%
-spec escape_term_as_html_content( term() ) -> html_element().
escape_term_as_html_content( Term ) ->
	escape_as_html_content( text_utils:term_to_string( Term ) ).
