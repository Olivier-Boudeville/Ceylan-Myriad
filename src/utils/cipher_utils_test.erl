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


% Unit tests for the cipher_utils toolbox.
%
% See the cipher_utils.erl tested module.
%
-module(cipher_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	%TransformList = [ id ],
	%TransformList = [ id, id ],
	%TransformList = [ { offset, 117 } ],

	%TransformList = [ { compress, zip } ],
	%TransformList = [ { compress, bzip2 } ],
	%TransformList = [ { compress, xz } ],

	%TransformList = [ delta_combine ],

	TransformList = [ { shuffle, _Seed={ 140, 98, 250 }, _Length=7 } ],

	%TransformList = [ { insert_random, _Seed={ 4, 48, 25 },
	%				  _Range=10 } ],


	%TransformList = [ {compress, xz }, { offset, 10 }, delta_combine,
	%				  { insert_random, _Seed={ 412, 1418, 2565 },
	%					_Range=10 } ],

	KeyFilename = "my-test-key-file.cipher",


	test_facilities:display( "Generating a key from transform list '~p', "
							 "to be stored in file '~s'.",
							 [ TransformList, KeyFilename ] ),

	case file_utils:is_existing_file( KeyFilename ) of

		true ->
			% Otherwise generation will halt on error:
			test_facilities:display( "(removing previously existing "
									 "key file '~s')~n", [ KeyFilename ] ),
			file_utils:remove_file( KeyFilename );

		false ->
			ok

	end,

	cipher_utils:generate_key( KeyFilename, TransformList ),

	SourceFilename = "my-content-file",

	% Add some content:
	file_utils:copy_file( "GNUmakefile", SourceFilename ),

	OriginalContent = file_utils:read_whole( SourceFilename ),

	EncryptedFilename = SourceFilename ++ ".encrypted",


	test_facilities:display( "Encrypting '~s' into '~s', using key file '~s'.",
					 [ SourceFilename, EncryptedFilename, KeyFilename ] ),

	cipher_utils:encrypt( SourceFilename, EncryptedFilename, KeyFilename ),


	DecryptedFilename =  SourceFilename ++ ".decrypted",

	test_facilities:display( "Decrypting '~s' into '~s', using the same key.",
							 [ EncryptedFilename, DecryptedFilename ] ),

	cipher_utils:decrypt( EncryptedFilename, DecryptedFilename, KeyFilename ),

	FinalContent = file_utils:read_whole( DecryptedFilename ),

	case OriginalContent =:= FinalContent of

		true ->
			test_facilities:display( "Original file and decrypted one match." );

		false ->
			throw( decrypted_content_differs )

	end,

	%file_utils:remove_files( [ KeyFilename, EncryptedFilename,
	%						   DecryptedFilename, SourceFilename ] ),

	test_facilities:stop().
