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


% Ciphers:
-export([ id_cipher/1, id_decipher/1 ]).



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


% When ciphering or deciphering a file of size N:
%
% - the whole file is streamed, hence it will never be loaded fully in memory
%
% - we expect that the free storage capacity is at least 2.N bytes


% Available transformations are:
%
% - id: identity (content not changed)
%
% - set_seed: set the random seed to be used from now on (content not changed)
%
% - offset: the specified value is added to all bytes of the file
%
% - compress: the file content is replaced by a compressed version thereof, using xz
%
% - insert_random: based on the specified seed and on the specified range R, a
% series of strictly positive values is uniformly drawn in [1,R]; these values
% are offsets relative to the last random insertion (initial one is 0); at each
% position determined thanks to offsets, a random value in [0,255] is inserted
%
% - delta_combine: a byte Bk+1 is replaced by its difference with the previous
% byte, with B0=128; hence Bk+1 is replaced by Bk+1 - Bk (Bk having obeyed the
% same rule)
%
% - shuffle: based on specified length L, each series of up to L bytes is
% uniformly shuffled
%
% - xor: based on the specified list of bytes, the content of the file is XOR'ed
%
% - mealy: based on specified state-transition data, the content of the file is
% modified accordingly


% Just for testing:
-type id_transform() :: 'id'.


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

-type cipher_transform() :: id_transform()
				   | offset_transform()
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
			file_utils:open( SourceFilename, _Opts=[ read, raw, read_ahead ] );

		false ->
			throw( { non_existing_source_file, SourceFilename } )

	end,


	_TargetFile = case file_utils:exists( TargetFilename ) of

		true ->
			throw( { already_existing_target_file, TargetFilename } );

		false ->
			ok

	end,


	KeyInfos = case file_utils:is_existing_file_or_link( KeyFilename ) of

		true ->
			file_utils:read_terms( KeyFilename );

		false ->
			throw( { non_existing_key_file, KeyFilename } )

	end,

	io:format( "Encrypting source file '~s' with key file '~s', "
			   "storing the result in '~s'. Key: '~p'.~n",
			   [ SourceFilename, KeyFilename, TargetFilename, KeyInfos ] ),

	TempFilename = apply_key( KeyInfos, SourceFilename ),

	file_utils:rename( TempFilename, TargetFilename ).





% Decrypts specified source file using specified key file, and writes the result
% in specified target file.
%
-spec decrypt( file_utils:file_name(), file_utils:file_name(),
			   file_utils:file_name() ) -> basic_utils:void().
decrypt( SourceFilename, TargetFilename, KeyFilename ) ->

	_SourceFile = case file_utils:is_existing_file_or_link( SourceFilename ) of

		true ->
			file_utils:open( SourceFilename, _Opts=[ read, raw, read_ahead ] );

		false ->
			throw( { non_existing_source_file, SourceFilename } )

	end,


	_TargetFile = case file_utils:exists( TargetFilename ) of

		true ->
			throw( { already_existing_target_file, TargetFilename } );

		false ->
			ok

	end,


	KeyInfos = case file_utils:is_existing_file_or_link( KeyFilename ) of

		true ->
			file_utils:read_terms( KeyFilename );

		false ->
			throw( { non_existing_key_file, KeyFilename } )

	end,

	io:format( "Decrypting source file '~s' with key file '~s', "
			   "storing the result in '~s'. Key: '~p'.~n",
			   [ SourceFilename, KeyFilename, TargetFilename, KeyInfos ] ).


% Applies specified key to specified file.
%
% Returns the filename of the resulting file.
%
apply_key( KeyInfos, SourceFilename ) ->
	apply_key( KeyInfos, SourceFilename, _IsFirstTransform=true ).


apply_key( _KeyInfos=[], SourceFilename, _IsFirstTransform ) ->
	SourceFilename;

apply_key( _KeyInfos=[ K | H ], SourceFilename, IsFirstTransform ) ->

	NewFilename = apply_cipher( K, SourceFilename ),

	case IsFirstTransform of

		true ->
			ok;

		false ->
			% Not wanting to saturate the storage space with intermediate files:
			file_utils:remove_file( SourceFilename )

	end,

	apply_key( H, NewFilename, _IsFirstTransform=false ).



% Applies specified cipher to specified file.
%
% Returns the filename of the resulting file.
%
apply_cipher( id, SourceFilename ) ->
	id_cipher( SourceFilename );

apply_cipher( C, _SourceFilename ) ->
	throw( { unknown_cipher, C } ).





% Cipher section.


% We must though create a new file, as the semantics is to create an additional
% file in all cases.
%
id_cipher( SourceFilename ) ->
	CipheredFilename = generate_filename(),
	file_utils:copy_file( SourceFilename, CipheredFilename ),
	SourceFilename.


id_decipher( SourceFilename ) ->
	% Symmetric here:
	id_cipher( SourceFilename ).



generate_filename() ->

	Filename = ".cipher-" ++ basic_utils:generate_uuid(),

	case file_utils:is_existing_file( Filename ) of

		% Rather unlikely:
		true ->
			generate_filename();

		false ->
			Filename

	end.
