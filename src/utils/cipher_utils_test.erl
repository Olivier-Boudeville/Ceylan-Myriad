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

	SourceFilename = "GNUmakefile",

	OriginalContent = file_utils:read_whole( SourceFilename ),

	EncryptedFilename = "GNUmakefile.encrypted",

	KeyFilename = "my-test-key.cipher",

	test_facilities:display( "Encrypting '~s' in '~s', using key file '~s'.",
							 [ SourceFilename, EncryptedFilename, KeyFilename ] ),

	cipher_utils:encrypt( SourceFilename, EncryptedFilename, KeyFilename ),


	DecryptedFilename = "GNUmakefile.decrypted",

	test_facilities:display( "Decrypting '~s' in '~s', using the same key.",
							 [ EncryptedFilename, DecryptedFilename ] ),

	cipher_utils:decrypt( EncryptedFilename, DecryptedFilename, KeyFilename ),

	FinalContent = file_utils:read_whole( DecryptedFilename ),

	case OriginalContent =:= FinalContent of

		true ->
			ok;

		false ->
			throw( decrypted_content_differs )

	end,

	test_facilities:stop().
