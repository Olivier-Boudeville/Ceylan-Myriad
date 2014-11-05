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


% When ciphering or deciphering a file of size N, as much as possible:
%
% - the whole file is streamed, hence it will never be loaded fully in memory
%
% - we expect that the free storage capacity is at least 2.N bytes
%
% - we could apply all transformations in-memory once (instead of writing as
% many intermediate files as there are transformations), however it would be
% difficult to implement (as such, and because of streaming, and because, from a
% transformation to another, the access patterns are usually different)




% Available transformations are:
%
% - id: identity (content not changed)
%
% - offset: the specified value is added to all bytes of the file
%
% - compress: the file content is replaced by a compressed version thereof,
% using one of the supported formats (see compress_format/0)
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


-type compress_transform() :: { 'compress', file_utils:compression_format() }.


-type insert_random_transform() :: { 'insert_random', random_utils:seed(),
									 basic_utils:count() }.


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
generate_key( KeyFilename, Transforms ) ->

	case file_utils:exists( KeyFilename ) of

		true ->
			throw( { already_existing_key_file, KeyFilename } );

		false ->
			ok

	end,

	KeyFile = file_utils:open( KeyFilename, _Opts=[ write, raw ] ),

	Header = text_utils:format( "% Key generated on ~s, by ~s, on ~s.~n",
								[ basic_utils:get_textual_timestamp(),
								  system_utils:get_user_name(),
								  net_utils:localhost() ] ),

	file_utils:write( KeyFile, Header ),

	file_utils:write( KeyFile, "~n~w.~n~n", [ Transforms ] ),

	file_utils:write( KeyFile, "% End of key file.~n", [] ),

	file_utils:close( KeyFile ).



% Encrypts specified source file using specified key file, and writes the result
% in specified target file.
%
% The original file is kept as is.
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

	KeyInfos = read_key( KeyFilename ),

	io:format( "Encrypting source file '~s' with key file '~s', "
			   "storing the result in '~s'. Key: '~p'.~n",
			   [ SourceFilename, KeyFilename, TargetFilename, KeyInfos ] ),

	% We may use randomised ciphers:
	random_utils:start_random_source( default_seed ),

	TempFilename = apply_key( KeyInfos, SourceFilename ),

	file_utils:rename( TempFilename, TargetFilename ).





% Decrypts specified source file using specified key file, and writes the result
% in specified target file.
%
% The ciphered file is kept as is.
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


	KeyInfos = read_key( KeyFilename ),

	io:format( "Decrypting source file '~s' with key file '~s', "
			   "storing the result in '~s'. Key: '~p'.~n",
			   [ SourceFilename, KeyFilename, TargetFilename, KeyInfos ] ),

	% We may use randomised ciphers:
	random_utils:start_random_source( default_seed ),

	ReverseKey = get_reverse_key_from( KeyInfos ),

	io:format( "Determined reverse key: '~p', using it.~n", [ ReverseKey ] ),

	TempFilename = apply_key( ReverseKey, SourceFilename ),

	file_utils:rename( TempFilename, TargetFilename ).



% Helper section.


% Reads key from specified filename.
%
% (helper)
%
read_key( KeyFilename ) ->

	case file_utils:is_existing_file_or_link( KeyFilename ) of

		true ->
			case file_utils:read_terms( KeyFilename ) of

				[] ->
					throw( { empty_key, KeyFilename } );

				[ Key ] ->
					Key;

				Invalid ->
					throw( { invalid_multiline_key, Invalid } )

			end;

		false ->
			throw( { non_existing_key_file, KeyFilename } )

	end.



% Returns the reverse key of specified one.
%
% (helper)
%
get_reverse_key_from( KeyInfos ) ->
	get_reverse_key_from( KeyInfos, _Acc=[] ).



get_reverse_key_from( _KeyInfos=[], Acc ) ->
	% Order already reversed by design:
	Acc;

get_reverse_key_from( _KeyInfos=[ K | H ], Acc ) ->
	ReversedK = reverse_cipher( K ),
	get_reverse_key_from( H, [ ReversedK | Acc ] ).





% Applies specified key to specified file.
%
% Returns the filename of the resulting file.
%
%
apply_key( KeyInfos, SourceFilename ) ->
	apply_key( KeyInfos, SourceFilename, _CipherCount=1 ).


apply_key( _KeyInfos=[], SourceFilename, _CipherCount ) ->
	SourceFilename;

apply_key( _KeyInfos=[ K | H ], SourceFilename, CipherCount ) ->

	io:format( " - applying cipher #~B: '~p'~n", [ CipherCount, K ] ),

	% Filename of the ciphered version:
	CipheredFilename = generate_filename(),

	apply_cipher( K, SourceFilename, CipheredFilename ),

	case CipherCount of

		1 ->
			ok;

		_ ->
			% Not wanting to saturate the storage space with intermediate files:
			file_utils:remove_file( SourceFilename )

	end,

	apply_key( H, CipheredFilename, CipherCount + 1 ).



% Applies specified cipher to specified file.
%
% Some ciphers are better managed if special-cased, whereas others can rely on
% base (yet generic) mechanisms.
%
apply_cipher( id, SourceFilename, CipheredFilename ) ->
	id_cipher( SourceFilename, CipheredFilename );


apply_cipher( { offset, Offset }, SourceFilename, CipheredFilename ) ->

	% CypherState is simply the constant offset used:

	OffsetFun = fun( InputByte, CypherState ) ->
						OutputByte = InputByte + Offset,
						{ OutputByte, CypherState }
				end,

	apply_byte_level_cipher( SourceFilename, CipheredFilename,
							 _Transform=OffsetFun, _InitialCipherState=Offset );


apply_cipher( { insert_random, Seed, Range }, SourceFilename,
			  CipheredFilename ) ->

	random_utils:start_random_source( Seed ),

	insert_random_cipher( SourceFilename, CipheredFilename, Range );


apply_cipher( { extract_random, Seed, Range }, SourceFilename,
			  CipheredFilename ) ->

	random_utils:start_random_source( Seed ),

	extract_random_cipher( SourceFilename, CipheredFilename, Range );


apply_cipher( { compress, CompressFormat }, SourceFilename,
			  CipheredFilename ) ->
	compress_cipher( SourceFilename, CipheredFilename, CompressFormat );


apply_cipher( { decompress, CompressFormat }, SourceFilename,
			  CipheredFilename ) ->
	decompress_cipher( SourceFilename, CipheredFilename, CompressFormat );


apply_cipher( delta_combine, SourceFilename, CipheredFilename ) ->

	% CypherState is simply the last value read:

	DeltaFun = fun( InputByte, CypherState ) ->
						OutputByte = InputByte - CypherState,
						{ OutputByte, InputByte }
				end,

	apply_byte_level_cipher( SourceFilename, CipheredFilename,
							 _Transform=DeltaFun, _InitialCipherState=100 );


apply_cipher( delta_combine_reverse, SourceFilename, CipheredFilename ) ->

	% CypherState is simply the last value read:

	ReverseDeltaFun = fun( InputByte, CypherState ) ->
						OutputByte = InputByte + CypherState,
						{ OutputByte, OutputByte }
				end,

	apply_byte_level_cipher( SourceFilename, CipheredFilename,
					 _Transform=ReverseDeltaFun, _InitialCipherState=100 );


apply_cipher( C, _SourceFilename, _CipheredFilename ) ->
	throw( { unknown_cipher_to_apply, C } ).



% Returns the reverse cipher of the specified one.
%
reverse_cipher( id ) ->
	id;

reverse_cipher( { offset, Offset } ) ->
	{ offset, 256 - Offset };

reverse_cipher( { compress, CompressFormat } ) ->
	{ decompress, CompressFormat };

reverse_cipher( { insert_random, Seed, Range } ) ->
	{ extract_random, Seed, Range };

reverse_cipher( delta_combine ) ->
	delta_combine_reverse;

reverse_cipher( delta_combine_reverse ) ->
	delta_reverse;

reverse_cipher( C ) ->
	throw( { unknown_cipher_to_reverse, C } ).



% Cipher section.


% For all ciphers that can be expressed by a byte-level, stateful transformation
% fun.
%
apply_byte_level_cipher( SourceFilename, CipheredFilename, CipherFun,
					   CipherInitialState ) ->

	% No need for intermediate buffering, thanks to read_ahead and
	% delayed_write;

	SourceFile = file_utils:open( SourceFilename,
								  _ReadOpts=[ read, raw, binary, read_ahead ] ),

	TargetFile = file_utils:open( CipheredFilename,
								  _WriteOpts=[ write, raw, delayed_write ] ),

	apply_byte_level_helper( SourceFile, TargetFile, CipherFun,
							 CipherInitialState ).



% Actual application of a byte-level transform.
%
apply_byte_level_helper( SourceFile, TargetFile, CipherFun,
						 CipherInitialState ) ->

	Count = 1024 * 8,

	case file_utils:read( SourceFile, Count ) of

		eof ->
			file_utils:close( SourceFile ),
			file_utils:close( TargetFile );

		{ ok, DataBin } ->

			{ NewDataBin, NewCipherState } = transform_bytes( DataBin,
									CipherFun, CipherInitialState ),

			file_utils:write( TargetFile, NewDataBin ),

			apply_byte_level_helper( SourceFile, TargetFile, CipherFun,
									 NewCipherState )

	end.



% There must be a way of folding onto binaries (bitstring comprehensions):
transform_bytes( DataBin, CipherFun, CipherInitialState ) ->
	transform_bytes( DataBin, CipherFun, CipherInitialState, _AccBin = <<>> ).


transform_bytes( <<>>, _CipherFun, CipherState, AccBin ) ->
	{ AccBin, CipherState };

transform_bytes( _A = << InputByte:8, T/binary >>, CipherFun,
				 CipherState, AccBin ) ->

	{ OutputByte, NewCipherState } = CipherFun( InputByte, CipherState ),

	transform_bytes( T, CipherFun, NewCipherState,
					 << AccBin/binary, OutputByte >> ).




% We must though create a new file, as the semantics is to create an additional
% file in all cases.
%
id_cipher( SourceFilename, CipheredFilename ) ->
	file_utils:copy_file( SourceFilename, CipheredFilename ).



compress_cipher( SourceFilename, CipheredFilename, CompressFormat ) ->

	CompressedFilename = file_utils:compress( SourceFilename, CompressFormat ),

	% Preserves the caller-naming convention:
	file_utils:rename( CompressedFilename, CipheredFilename ).


decompress_cipher( CipheredFilename, TargetFilename, CompressFormat ) ->

	% The decompressing function will check for the relevant extension:

	% We must avoid, to decompress X, to rename it to X.bzip2 and then to
	% decompress it, as this would produce a new decompressed file named X,
	% overwriting the initial one.

	%NewCipheredFilename = CipheredFilename
	NewCipheredFilename = generate_filename()
		++ file_utils:get_extension_for( CompressFormat ),

	file_utils:rename( CipheredFilename, NewCipheredFilename ),

	DecompressedFilename = file_utils:decompress( NewCipheredFilename,
												  CompressFormat ),

	file_utils:rename( NewCipheredFilename, CipheredFilename ),

	% Preserves the caller-naming convention:
	file_utils:rename( DecompressedFilename, TargetFilename ).



insert_random_cipher( SourceFilename, CipheredFilename, Range )
  when Range > 1 ->

	SourceFile = file_utils:open( SourceFilename,
								  _ReadOpts=[ read, raw, binary, read_ahead ] ),

	TargetFile = file_utils:open( CipheredFilename,
								  _WriteOpts=[ write, raw, delayed_write ] ),

	_InsertedCount = insert_helper( SourceFile, TargetFile, Range, _Count=0 ).

	%io:format( "insert_random_cipher: inserted ~B bytes.~n",
	%		   [ InsertedCount ] ).


% We insert at random places random values in the content:
insert_helper( SourceFile, TargetFile, Range, Count ) ->

	NextInsertionOffset = random_utils:get_random_value( Range ),

	case file_utils:read( SourceFile, NextInsertionOffset ) of

		eof ->
			file_utils:close( SourceFile ),
			file_utils:close( TargetFile ),
			Count;


		{ ok, DataBin } when size( DataBin ) =:= NextInsertionOffset ->

			RandomByte = random_utils:get_random_value( 255 ),

			NewDataBin = << DataBin/binary, RandomByte:8 >>,

			file_utils:write( TargetFile, NewDataBin ),

			insert_helper( SourceFile, TargetFile, Range, Count + 1 );


		{ ok, PartialDataBin } ->

			% Drawn offset not reachable, just finished then:

			file_utils:write( TargetFile, PartialDataBin ),
			file_utils:close( SourceFile ),
			file_utils:close( TargetFile ),
			Count

	end.




extract_random_cipher( CipheredFilename, TargetFilename, Range )
  when Range > 1 ->

	CipheredFile = file_utils:open( CipheredFilename,
							  _ReadOpts=[ read, raw, binary, read_ahead ] ),

	TargetFile = file_utils:open( TargetFilename,
								  _WriteOpts=[ write, raw, delayed_write ] ),

	_ExtractedCount = extract_helper( CipheredFile, TargetFile, Range,
									 _Count=0 ).

	%io:format( "extract_random_cipher: extracted ~B bytes.~n",
	%		   [ ExtractedCount ] ).



% We extract at random places the bytes found in the content:
extract_helper( CipheredFile, TargetFile, Range, Count ) ->

	NextExtractionOffset = random_utils:get_random_value( Range ),

	case file_utils:read( CipheredFile, NextExtractionOffset ) of

		eof ->
			file_utils:close( CipheredFile ),
			file_utils:close( TargetFile ),
			Count;

		{ ok, DataBin } when size( DataBin ) =:= NextExtractionOffset ->

			% We drop on the floor the previously inserted byte:
			case file_utils:read( CipheredFile, 1 ) of

				eof ->
					file_utils:close( CipheredFile ),
					file_utils:close( TargetFile ),
					Count;

				{ ok, <<_ExtractedByte:8>> } ->

					% Dummy operation, needed to reproduce the insertion random
					% state:
					_RandomByte = random_utils:get_random_value( 255 ),

					file_utils:write( TargetFile, DataBin ),

					extract_helper( CipheredFile, TargetFile, Range, Count + 1 )

			end;

		{ ok, PartialDataBin } ->
			% Finished:
			file_utils:write( TargetFile, PartialDataBin ),
			file_utils:close( CipheredFile ),
			file_utils:close( TargetFile ),
			Count

	end.



generate_filename() ->

	Filename = ".cipher-" ++ basic_utils:generate_uuid(),

	case file_utils:is_existing_file( Filename ) of

		% Rather unlikely:
		true ->
			generate_filename();

		false ->
			Filename

	end.
