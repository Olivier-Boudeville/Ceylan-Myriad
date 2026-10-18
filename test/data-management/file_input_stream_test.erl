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

-module(file_input_stream_test).

-moduledoc """
Unit tests for the **file input stream** utilities.

See the `file_input_stream` tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").



-doc "Testing read_some/1.".
test_read_some( ReadFilePath, ReferenceData ) ->

    test_facilities:display( "Testing reading based on read_some/1." ),

    { ok, FIS } = file_input_stream:open( ReadFilePath ),

    test_facilities:display( "At opening: ~ts.",
                             [ file_input_stream:to_string( FIS ) ] ),

    case read_some_full_read( FIS, _AccBin= <<>> ) of

        ReferenceData ->
            test_facilities:display( "Matching data read." );

        ReadData ->
            trace_utils:error_fmt( "Mismatching data read for '~ts':~n ~p",
                                   [ ReadFilePath, ReadData ] ),
            throw( incorrect_read_some )

    end.


read_some_full_read( FIS, AccBin )->

    case file_input_stream:read_some( FIS ) of

        end_of_stream ->
            AccBin;

        { error, Reason } ->
            throw( { read_some_failed, Reason } );

        { DataBin, NewFIS } ->
            %trace_utils:debug_fmt( "Read ~B bytes.", [ size( DataBin ) ] ),
            NewAccBin = <<AccBin/binary, DataBin/binary>>,
            read_some_full_read( NewFIS, NewAccBin )

    end.



-doc "Testing read_at_least/2.".
test_read_at_least( ReadFilePath, ReferenceData ) ->

    test_facilities:display( "Testing reading based on read_at_least/2." ),

    { ok, FIS } = file_input_stream:open( ReadFilePath ),

    case read_at_least_full_read( FIS, _AccBin= <<>> ) of

        ReferenceData ->
            test_facilities:display( "Matching data read." );

        ReadData ->
            trace_utils:error_fmt( "Mismatching data read for '~ts':~n ~p",
                                   [ ReadFilePath, ReadData ] ),
            throw( incorrect_read_at_least )

    end.


read_at_least_full_read( FIS, AccBin )->

    case file_input_stream:read_at_least( _MinCount=1, FIS ) of

        end_of_stream ->
            AccBin;

        { error, Reason } ->
            throw( { read_at_least_failed, Reason } );

        { DataBin, NewFIS } ->
            %trace_utils:debug_fmt( "Read ~B bytes.", [ size( DataBin ) ] ),
            NewAccBin = <<AccBin/binary, DataBin/binary>>,
            read_at_least_full_read( NewFIS, NewAccBin )

    end.



-doc "Testing read_at_most/2.".
test_read_at_most( ReadFilePath, ReferenceData ) ->

    test_facilities:display( "Testing reading based on read_at_most/2." ),

    { ok, FIS } = file_input_stream:open( ReadFilePath ),

    case read_at_most_full_read( FIS, _AccBin= <<>> ) of

        ReferenceData ->
            test_facilities:display( "Matching data read." );

        ReadData ->
            trace_utils:error_fmt( "Mismatching data read for '~ts':~n ~p",
                                   [ ReadFilePath, ReadData ] ),
            throw( incorrect_read_at_most )

    end.


read_at_most_full_read( FIS, AccBin )->

    case file_input_stream:read_at_most( _MaxCount=1, FIS ) of

        end_of_stream ->
            AccBin;

        { error, Reason } ->
            throw( { read_at_most_failed, Reason } );

        { DataBin, NewFIS } ->
            %trace_utils:debug_fmt( "Read ~B bytes.", [ size( DataBin ) ] ),
            basic_utils:assert_equal( size( DataBin ), 1 ),
            NewAccBin = <<AccBin/binary, DataBin/binary>>,
            read_at_most_full_read( NewFIS, NewAccBin )

    end.



-doc "Testing read_between/2.".
test_read_between( ReadFilePath, ReferenceData ) ->

    test_facilities:display( "Testing reading based on read_between/2." ),

    { ok, FIS } = file_input_stream:open( ReadFilePath ),

    case read_between_full_read( FIS, _AccBin= <<>> ) of

        ReferenceData ->
            test_facilities:display( "Matching data read." );

        ReadData ->
            trace_utils:error_fmt( "Mismatching data read for '~ts':~n ~p",
                                   [ ReadFilePath, ReadData ] ),
            throw( incorrect_read_between )

    end.


read_between_full_read( FIS, AccBin )->

    case file_input_stream:read_between( _MinCount=1, _MaxCount=2, FIS ) of

        end_of_stream ->
            AccBin;

        { error, Reason } ->
            throw( { read_between_failed, Reason } );

        { DataBin, NewFIS } ->
            % Always 2 bytes, except the last read, of 1 byte:
            %trace_utils:debug_fmt( "Read ~B bytes.", [ size( DataBin ) ] ),

            NewAccBin = <<AccBin/binary, DataBin/binary>>,

            read_between_full_read( NewFIS, NewAccBin )

    end.



-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    ReadFilePath = "GNUmakefile",

    test_facilities:display( "Reading, based on a file input stream, '~ts'.",
                             [ ReadFilePath ] ),

    ReadData = file_input_stream:read_whole( ReadFilePath ),

     test_facilities:display(
         "Comparing it with the content obtained by a direct reading." ),

     case file_utils:read_whole( ReadFilePath ) of

         ReadData ->
             test_facilities:display( "Matching data read." );

         _ ->
             trace_utils:error_fmt( "Mismatching data read:~n ~p",
                                    [ ReadData ] ),
             throw( incorrect_read )

     end,

    test_facilities:display( "Testing now partial readings." ),

    test_read_some( ReadFilePath, _ReferenceData=ReadData ),
    test_read_at_least( ReadFilePath, ReadData ),
    test_read_at_most( ReadFilePath, ReadData ),
    test_read_between( ReadFilePath, ReadData ),

    test_facilities:stop().
