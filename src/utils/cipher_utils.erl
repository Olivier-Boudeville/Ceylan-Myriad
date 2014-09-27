% Copyright (C) 2014 Olivier Boudeville
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
% Creation date: Friday, November 1, 2013.


% Gathering of various cipher-related facilities.
%
% We focus on symmetric ciphering here.
%
% See cipher_utils_test.erl for testing.
%
-module(cipher_utils).


-export([ generate_key/2, encrypt/3, decrypt/3 ]).


% Implementation notes.
%
% To encrypt a file, one shall use a key file, whose extension is by convention
% 'cipher' (ex: "my-key-file.cipher").
%
% The same file can be used to perform the reverse operation.
%
% The mode of operation is to chain a series of elementary transformations that
% can be reversed. These operations are listed and described in the
% aforementioned file, which contains Erlang terms for that.


% Available transformations are:
%
% - offset
% - compress
% - insert_random
% - delta_combine
% - shuffle
% - xor
% - mealy


-type offset_transform() :: { 'offset', integer() }.


-type compress_tool() :: 'zip' | 'bz2' | 'xz'.


-type compress_transform() :: { 'compress', compress_tool() }.


-type insert_random_transform() :: { 'insert_random', random_utils:seed() }.


-type delta_combine_transform() :: 'delta_combine'.


-type shuffle_transform() :: 'shuffle'.


-type xor_transform() :: { 'xor', integer() }.


-type mealy_state() :: integer().
-type mealy_table() :: any().

-type mealy_transform() :: { 'mealy', mealy_state(), mealy_table() }.

-type cipher_transform() :: offset_transform()
				   | compress_transform()
				   | insert_random_transform()
				   | delta_combine_transform()
				   | shuffle_transform()
				   | xor_transform()
				   | mealy_transform().


-export_type([ cipher_transform/0 ]).


-spec generate_key( file_utils:file_name(), [ cipher_transform() ] ) ->
						  basic_utils:void().
generate_key( _TargetFilename, _Transforms ) ->
	ok.


% Encrypts specified source file using specified key file, and writes the result
% in specified target file.
%
-spec encrypt( file_utils:file_name(), file_utils:file_name(),
			   file_utils:file_name() ) -> basic_utils:void().
encrypt( SourceFilename, TargetFilename, KeyFilename ) ->

	_SourceFile = case file_utils:is_existing_file_or_link( SourceFilename ) of

		true ->
			ok;

		false ->
			throw( { non_existing_source_file, SourceFilename } )

	end,


	_TargetFile = case file_utils:exists( TargetFilename ) of

		true ->
			throw( { already_existing_target_file, TargetFilename } );

		false ->
			ok

	end,


	_KeyFile = case file_utils:is_existing_file_or_link( KeyFilename ) of

		true ->
			ok;

		false ->
			throw( { non_existing_key_file, KeyFilename } )

	end,

	io:format( "Encrypting source file '~s' with key file '~s', "
			   "storing the result in '~s'.",
			   [ SourceFilename, KeyFilename, TargetFilename ] ).






% Decrypts specified source file using specified key file, and writes the result
% in specified target file.
%
-spec decrypt( file_utils:file_name(), file_utils:file_name(),
			   file_utils:file_name() ) -> basic_utils:void().
decrypt( _SourceFilename, _TargetFilename, _KeyFilename ) ->
	ok.
