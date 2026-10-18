% Copyright (C) 2026-2026 Olivier Boudeville
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
% Creation date: Sunday, October 18, 2026.

-module(file_output_stream_test).

-moduledoc """
Unit tests for the **file output stream** utilities.

See the `file_output_stream` tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    WriteFilePath = "Testing-file-output-stream.txt",

    BinData = <<"This is a writing test for the file_output_stream module.">>,

    test_facilities:display(
        "Writing in '~ts', based on a file output stream: '~ts'.",
        [ WriteFilePath, BinData ] ),

    % Any already-existing file at this path will be silently overwritten:
    file_output_stream:write_whole( WriteFilePath, BinData ),

     test_facilities:display( "Checking the content of this file." ),

     case file_utils:read_whole( WriteFilePath ) of

         BinData ->
             test_facilities:display( "Matching data read." );

         OtherData ->
             trace_utils:error_fmt( "Mismatching data read:~n ~p",
                                    [ OtherData ] ),
             throw( incorrect_write )

     end,

    % Cleanup:
    file_utils:remove_file( WriteFilePath ),

    test_facilities:stop().
