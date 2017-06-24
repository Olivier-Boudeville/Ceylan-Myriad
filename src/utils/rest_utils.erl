% Copyright (C) 2015-2017 Olivier Boudeville
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
% Creation date: Tuesday, December 1, 2015



% Gathering of management facilities for REST architectures (Representational
% State Transfer).
%
% See rest_utils_test.erl for the corresponding test.
%
-module(rest_utils).



% Implementation notes:
%
% We basically rely here on following prerequisites:
%
% - an HTTP client, namely the built-in one, httpc
% (http://erlang.org/doc/man/httpc.html), with some usage information
% (http://erlang.org/doc/apps/inets/http_client.html)
%
% - a JSON parser, namely jsx (https://github.com/talentdeficit/jsx/), version
% 2.8.0 at the time of this writing; we expect the BEAM files from jsx to be
% available on the code path (we typically expect to find them in
% ~/Software/jsx/jsx-current-install)



-export([ start/0, start/1, is_json_parser_available/0,
		  to_json/1, from_json/1, stop/0 ]).


% Tells whether the SSL support is needed (typically for https):
%
-type ssl_opt() :: 'no_ssl' | 'ssl'.


% JSON document:
-type json() :: binary() | string().


% Content type (ex: "text/html;charset=utf-8", "application/json"):
-type content_type() :: string().


-type field() :: string().

-type value() :: string().

-type header() :: { field(), value() }.

-type headers() :: [ header() ].



% Context of a REST exchange:
-type context() :: { net_utils:url_info(), headers() }.


-export_type([ ssl_opt/0, json/0, content_type/0, field/0, value/0,
			   header/0, headers/0, context/0 ]).



% Starts the REST service, with default settings.
%
-spec start() -> basic_utils:void().
start() ->
	start( no_ssl ).



% Starts the REST service.
%
-spec start( ssl_opt() ) -> basic_utils:void().
start( Option ) ->

	% Starts the (built-in) HTTP client:
	ok = inets:start( _DefaultInetsType=temporary ),

	% Starts the SSL support if requested:
	case Option of

		no_ssl ->
			ok;

		ssl ->
			ok = ssl:start( _DefaultSSLType=temporary)

	end,

	start_json_parser().



% Stops the REST service.
%
-spec stop() -> basic_utils:void().
stop() ->

	stop_json_parser(),

	% Maybe not launched, hence not pattern matched:
	ssl:stop(),

	ok = inets:stop().



% Starts the JSON parser.
%
-spec start_json_parser() -> basic_utils:void().
start_json_parser() ->

	% We use the 'jsx' parser, an external prerequisite.

	case is_json_parser_available() of

		true ->
			ok;

		false ->
			basic_utils:display( "\nError: jsx JSON parser not available.\n"
				++ system_utils:get_json_unavailability_hint() ),
			throw( { json_parser_not_found, jsx } )

	end,

	% This is a way to check its BEAMs are available and usable:
	%
	try jsx:is_json( <<"\"test\"">> ) of

		true ->
			ok

	catch

		error:undef ->
			basic_utils:display( "\nError: jsx JSON parser not operational.\n"
				++ system_utils:get_json_unavailability_hint() ),
			throw( { json_parser_not_operational, jsx } )

	end.



% Tells whether the JSON parser is available.
%
-spec is_json_parser_available() -> boolean().
is_json_parser_available() ->

	case code_utils:is_beam_in_path( 'jsx' ) of

		not_found ->
			false;

		[ _Path ] ->
			true ;

		Paths ->
			throw( { multiple_jsx_found, Paths } )

	end.



% Converts specified Erlang term into a JSON counterpart element.
%
-spec to_json( term() ) -> json().
to_json( Term ) ->
	jsx:encode( Term ).



% Converts specified JSON element into an Erlang term counterpart.
%
-spec from_json( json() ) -> term().
from_json( BinJson ) when is_binary( BinJson ) ->
	%io:format( "Decoding '~p':~n", [ BinJson ] ),

	% We prefer {state,<<"PUBLISHED">>} to {<<"state">>,<<"PUBLISHED">>}:
	jsx:decode( BinJson, _Opts=[ { labels, atom } ] );

from_json( StringJson ) when is_list( StringJson ) ->

	BinJson = text_utils:string_to_binary( StringJson ),
	from_json( BinJson ).


% Stops the JSON parser.
%
-spec stop_json_parser() -> basic_utils:void().
stop_json_parser() ->
	ok.
