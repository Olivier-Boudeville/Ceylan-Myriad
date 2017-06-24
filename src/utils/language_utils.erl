% Copyright (C) 2017-2017 Olivier Boudeville
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
% Creation date: Saturday, July 12, 2008.


% Gathering of various convenient facilities regardingthe support of various
% programming languages
%
% See language_utils_test.erl for the corresponding test.
%
-module(language_utils).



% Type to designate all known programming languages:
%
-type language() :: 'erlang' | 'python' | 'java'.



% Type to designate an (Erlang) process driving a runtime container (ex: a
% Python interpreter or a Java virtual machine) of a given language runtime
% (typically on a given node).
%
-type runtime_container_pid() :: pid().


% The PID of a Python interpreter runtime container:
%
-type python_interpreter_container_pid() :: python_utils:interpreter_pid().


% The PID of a Java virtual machine runtime container:
%
-type java_vm_container_pid() :: runtime_container_pid().



-export_type([ language/0,
			   runtime_container_pid/0, python_interpreter_container_pid/0,
			   java_vm_container_pid/0 ]).



-export([ get_supported_foreign_languages/0, get_supported_languages/0,
		  language_to_string/1, get_additional_beam_directories_for/1 ]).



% Returns a list of the supported foreign (non-native, i.e. non-Erlang)
% programming languages.
%
-spec get_supported_foreign_languages() -> [ language() ].
get_supported_foreign_languages() ->
	[ python, java ].



% Returns a list of all supported programming languages (including Erlang).
%
-spec get_supported_languages() -> [ language() ].
get_supported_languages() ->
	[ erlang | get_supported_foreign_languages() ].



% Returns a string describing the specified language.
%
-spec language_to_string( language() ) -> string().
language_to_string( erlang ) ->
	"Erlang";

language_to_string( python ) ->
	"Python";

language_to_string( java ) ->
	"Java";

language_to_string( Language ) when is_atom( Language ) ->
	throw( { unknown_language, Language } );

language_to_string( LanguageInvalidArg ) ->
	throw( { invalid_language_specification, LanguageInvalidArg } ).



% Returns the BEAM locations of all the dependencies related to the specified
% language bindings.
%
-spec get_additional_beam_directories_for( [ language() ] ) ->
											 [ file_utils:directory_name() ].
get_additional_beam_directories_for( Languages ) ->
	get_additional_beam_directories_for( Languages, _Acc=[] ).


% (helper)
%
get_additional_beam_directories_for( _Languages=[], Acc ) ->
	lists:reverse( Acc );


get_additional_beam_directories_for( _Languages=[ erlang | T ], Acc ) ->
	% Erlang natively supported "as is":
	get_additional_beam_directories_for( T, Acc );


get_additional_beam_directories_for( _Languages=[ python | T ], Acc ) ->

	% Finds the BEAM directories of the Python-specific dependencies :
	NewBeamDirs = python_utils:get_beam_directories_for_binding(),

	% Adds them to list of dependencies and goes for the next language:
	get_additional_beam_directories_for( T, NewBeamDirs ++ Acc );


get_additional_beam_directories_for( _Languages=[ java | T ], Acc ) ->

	% Finds the BEAM directories of the Java-specific dependencies :
	NewBeamDirs = java_utils:get_beam_directories_for_binding(),

	% Adds them to list of dependencies and goes for the next language:
	get_additional_beam_directories_for( T, NewBeamDirs ++ Acc ).
