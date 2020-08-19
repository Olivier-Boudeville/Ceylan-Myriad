% Copyright (C) 2020-2020 Olivier Boudeville
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
% Authors: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Creation date: Friday, February 21, 2020.


% Gathering of management facilities for JSON processing.
%
% See json_utils_test.erl for the corresponding test.
%
-module(json_utils).



% Implementation notes:
%
% We rely here on a JSON parser, namely by default jsx
% (https://github.com/talentdeficit/jsx/), version 3.0.0 at the time of this
% writing; we expect the BEAM files from jsx to be available on the code path
% (out of a rebar3 context, we typically expect to find them in
% ~/Software/jsx/jsx-current-install).
%
% Using jiffy (https://github.com/davisp/jiffy) is the second support backend.

% The typical type of (Erlang) terms to be encoded in JSON is a map whose keys
% are binaries.

% Comments are not supported in JSON; for them we rely on (non-duplicated)
% "_comment" entries.



-export([ start_parser/0, stop_parser/0,

		  get_parser_backend_name/0, is_parser_available/0,

		  to_json/1,
		  from_json/1, from_json/2, from_json_as_maps/1,
		  from_json_file/1, from_json_file/2, from_json_file_as_maps/1 ]).


% JSON document:
-type json() :: binary() | string().


% Shorthands;

-type any_file_path() :: file_utils:any_file_path().




% Options for the JSON parsing:
%
% (see https://github.com/talentdeficit/jsx#decode12 for more information; no
% type is defined there yet)
%
-type json_parsing_option() :: any().



-export_type([ json/0, json_parsing_option/0 ]).



% Starts the JSON parser.
-spec start_parser() -> void().
start_parser() ->

	case is_parser_available() of

		true ->
			ok;

		false ->
			JSONBackend = get_parser_backend_name(),
			trace_utils:error_fmt( "The ~s JSON parser is not available.~n~s",
				[ JSONBackend,
				  system_utils:get_json_unavailability_hint( JSONBackend ) ] ),

			throw( { json_parser_backend_not_found, JSONBackend } )

	end,

	check_parser_operational().



% Returns (as an atom) the JSON parser (as an OTP application name) currently used.
-spec get_parser_backend_name() -> 'jsx' | 'jiffy' | otp_utils:application_name().

-ifdef( use_jsx_json_backend ).

get_parser_backend_name() ->
	% We currently use the 'jsx' parser, an external prerequisite.
	jsx.

-else. % use_jsx_json_backend

-ifdef( use_jiffy_json_backend ).

get_parser_backend_name() ->
	% Another external prerequisite can be used, Jiffy:
	jiffy.

-else. % use_jiffy_json_backend

get_parser_backend_name() ->
	throw( no_json_backend_enabled ).

-endif. % use_jiffy_json_backend

-endif. % use_jsx_json_backend



% Tells whether the JSON parser is available.
-spec is_parser_available() -> boolean().
is_parser_available() ->

	% The backend is supposed to define a module of the same name:
	BackendMainModule = get_parser_backend_name(),

	case code_utils:is_beam_in_path( BackendMainModule ) of

		not_found ->
			false;

		[ _Path ] ->
			%trace_utils:debug_fmt( "JSON parser found as '~s'.", [ Path ] ),
			true ;

		Paths ->
			throw( { multiple_json_backends_found, Paths } )

	end.



% Checks whether the JSON parser is operational; throws an exception if not.
-spec check_parser_operational() -> void().

-ifdef( use_jsx_json_backend ).

check_parser_operational() ->

	% This is a way to check its BEAMs are available and fully usable:
	try jsx:is_json( <<"\"test\"">> ) of

		true ->
			ok

	catch

		error:undef ->
			trace_utils:error_fmt(
			  "The jsx JSON parser is not operational.~n~s",
			  [ system_utils:get_json_unavailability_hint() ] ),
			throw( { json_parser_not_operational, jsx } )

	end.

-else. % use_jsx_json_backend

-ifdef( use_jiffy_json_backend ).

check_parser_operational() ->
	trace_utils:warning( "Jiffy supposed to be operational." ).

-else. % use_jiffy_json_backend

check_parser_operational() ->
	throw( no_json_backend_enabled ).

-endif. % use_jiffy_json_backend

-endif. % use_jsx_json_backend



% Converts (encodes) specified Erlang term into a JSON counterpart element.
%
% Ex: json_utils:to_json( #{ <<"protected">> => Protected,
%							 <<"payload">> => Payload,
%							 <<"signature">> => EncSigned } ).
%
-spec to_json( term() ) -> json().

-ifdef( use_jsx_json_backend ).

to_json( Term ) ->
	jsx:encode( Term ).

-else. % use_jsx_json_backend

-ifdef( use_jiffy_json_backend ).

to_json( Term ) ->
	jiffy:encode( Term ).

-else. % use_jiffy_json_backend

to_json( _Term ) ->
	throw( no_json_backend_enabled ).

-endif. % use_jiffy_json_backend

-endif. % use_jsx_json_backend



% Returns the default options for the JSON decoding.
-spec get_default_json_decoding_options() -> [ json_parsing_option() ].

-ifdef( use_jsx_json_backend ).

get_default_json_decoding_options() ->
	% We used to prefer {state,<<"PUBLISHED">>} to
	% {<<"state">>,<<"PUBLISHED">>}, yet for compatibility with jiffy we stick
	% to binaries now, so:
	%
	%[ { labels, atom } ].
	[].

-else. % use_jsx_json_backend

-ifdef( use_jiffy_json_backend ).

get_default_json_decoding_options() ->
	% Jiffy only understands UTF-8 in binaries.
	[ use_nil, force_utf8 ].

-else. % use_jiffy_json_backend

get_default_json_decoding_options() ->
	throw( no_json_backend_enabled ).

-endif. % use_jiffy_json_backend

-endif. % use_jsx_json_backend



% Converts (decodes) specified JSON element into an Erlang term counterpart.
-spec from_json( json() ) -> term().
from_json( Json ) ->
	from_json( Json, get_default_json_decoding_options() ).



% Converts (decodes) specified JSON element into an Erlang term counterpart,
% with specified parsing options.
%
-spec from_json( json(), [ json_parsing_option() ] ) -> term().

-ifdef( use_jsx_json_backend ).

from_json( BinJson, Opts ) when is_binary( BinJson ) ->

	%trace_utils:debug_fmt( "Decoding '~p'.", [ BinJson ] ),

	% Note that at least some errors in the JSON file (ex: missing comma) will
	% lead only to an exception such as:
	%
	% ** exception error: bad argument
	%  in function  jsx_decoder:maybe_done/4
	%
	% (not even returning a line number for the faulty JSON part...)

	jsx:decode( BinJson, Opts );

% This clause cannot be even factorised:
from_json( StringJson, Opts ) when is_list( StringJson ) ->
	BinJson = text_utils:string_to_binary( StringJson ),
	from_json( BinJson, Opts ).

-else. % use_jsx_json_backend

-ifdef( use_jiffy_json_backend ).

from_json( BinJson, _Opts ) when is_binary( BinJson ) ->
	throw( not_implemented ).

-else. % use_jiffy_json_backend

from_json( BinJson, _Opts ) when is_binary( BinJson ) ->
	throw( no_json_backend_enabled ).

-endif. % use_jiffy_json_backend

-endif. % use_jsx_json_backend




% Converts (decodes) specified JSON element recursively so that it returns a
% table containing tables, themselves containing potentially tables, etc.
%
% Note that if in a given scope a key is present more than once, only one of its
% values will be retained (actually the lastly defined one).
%
-spec from_json_as_maps( json() ) -> table().

-ifdef( use_jsx_json_backend ).

from_json_as_maps( BinJson ) when is_binary( BinJson ) ->
	Opts = [ return_maps | get_default_json_decoding_options() ],
	from_json( BinJson, Opts ).

-else. % use_jsx_json_backend

-ifdef( use_jiffy_json_backend ).

from_json_as_maps( BinJson ) when is_binary( BinJson ) ->

	Opts = [ return_maps | get_default_json_decoding_options() ],
	from_json( BinJson, Opts ).

-else. % use_jiffy_json_backend

from_json_as_maps( BinJson ) when is_binary( BinJson ) ->
	throw( no_json_backend_enabled ).

-endif. % use_jiffy_json_backend

-endif. % use_jsx_json_backend



% Converts (decodes) specified JSON file into an Erlang term counterpart.
-spec from_json_file( any_file_path() ) -> term().
from_json_file( JsonFilePath ) ->
	BinJson = file_utils:read_whole( JsonFilePath ),
	from_json( BinJson ).



% Converts (decodes) specified JSON file into an Erlang term counterpart, with
% specified parsing options.
%
-spec from_json_file( any_file_path(),
					  [ json_parsing_option() ] ) -> term().
from_json_file( JsonFilePath, Opts ) ->
	BinJson = file_utils:read_whole( JsonFilePath ),
	from_json( BinJson, Opts ).



% Converts (decodes) specified JSON file recursively so that it returns a table
% containing tables, themselves containing potentially tables, etc.
%
% Note that if in a given scope a key is present more than once, only one of its
% values will be retained (actually the lastly defined one).
%
-spec from_json_file_as_maps( any_file_path() ) -> table().
from_json_file_as_maps( JsonFilePath ) ->
	BinJson = file_utils:read_whole( JsonFilePath ),
	from_json_as_maps( BinJson ).



% Stops the JSON parser.
-spec stop_parser() -> void().

-ifdef( use_jsx_json_backend ).

stop_parser() ->
	ok.

-else. % use_jsx_json_backend

-ifdef( use_jiffy_json_backend ).

stop_parser() ->
	ok.

-else. % use_jiffy_json_backend

stop_parser() ->
	ok.

-endif. % use_jiffy_json_backend

-endif. % use_jsx_json_backend
