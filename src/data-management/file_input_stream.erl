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
% Creation date: Tuesday, August 11, 2026.

-module(file_input_stream).

-moduledoc """
Utility module for **file-based input data streams**, to read from a (possibly
read-only file).

See also the `input_stream` module and implemented behaviour.
""".


%-behaviour(input_stream).


-record #file_input_stream{

   % The current module is in charge of handling that stream.

   % The path of the (read-only) file to which this file input stream
   % corresponds:
   %
   path :: bin_file_path(),

   % The actual file handle that corresponds to this input stream:
   file :: file_utils:file(),

   % Any buffered bytes (already obtained, but not returned yet to the caller):
   buffer :: binary() % (possibly empty)

   % Not relevant, as already available in the header of the binary:
   %
   % Caching the size of the internal buffer, as generally available anyway:
   % buf_size :: byte_size()

}.



-doc """
A file-based input data streams, to read from a (possibly read-only file).

Possibly designated by FIS.
""".
-type file_input_stream() :: #file_input_stream{}.


-doc "Shorthand for `file_input_stream/0`.".
-type fis() :: file_input_stream().



-doc "Result of the reading of a file input stream.".
-type data_reading() :: { ReadData :: stream_utils:stream_data(),
                          %ReadDataSize :: byte_size(),
                          UpdatedFIS :: file_input_stream() }
                      | 'end_of_stream' % Similar to eof
                      | { 'error', Reason :: any() }.


-export_record([ file_input_stream ]).

-export_type([ file_input_stream/0, fis/0 ]).


-export([ open/1, read_all/1, terminate/1,
          read_some/1, read_at_least/2, read_at_most/2,
          read_between/3, to_string/1 ]).



% Implementation notes: refer to the ones of the stream_utils module.


% The default size when having to read a chunk:
-define( default_read_chunk_size, 4096*16 ).



% Type shorthands:

-type ustring() :: text_utils:ustring().

-type byte_size() :: system_utils:byte_size().

-type any_file_path() :: file_utils:any_file_path().
-type bin_file_path() :: file_utils:bin_file_path().



% File Input stream interface.
%
% The stream on which to operate is always the last argument.



% File Input stream constructors.


-doc """
Creates a file-based input stream, by (only) opening the file at the specified
path.

Only the Erlang process that opened this file stream can use it.
""".
-spec open( any_file_path() ) -> file_input_stream().
open( AnyFilePath ) ->

    AbsBinPath = text_utils:ensure_binary(
        file_utils:ensure_path_is_absolute( AnyFilePath ) ),

    File = file:open( AbsBinPath,
        _Opts=[ read, { read_ahead, _Size=128000 }, raw, binary ] ),

    FIS = #file_input_stream{ path=AbsBinPath, file=File, buffer= <<>> },

    cond_utils:if_defined( myriad_check_streams, check_fis( FIS ) ),

    FIS.



-doc """
Creates a file-based input stream by opening the file at the specified
path and reading its full content in the internal buffer of that stream.
""".
-spec read_all( any_file_path() ) -> file_input_stream().
read_all( AnyFilePath ) ->
    % Not relying on file:read_file/2, so that the constructed FIS behaves the
    % same as others (typically with a file handle):
    %
    FIS = open( AnyFilePath ),
    ReadFIS = bufferize_all( FIS ),
    cond_utils:if_defined( myriad_check_streams, check_fis( ReadFIS ) ),
    ReadFIS.



-doc """
Exhausts the specified file input stream by reading its full content and storing
it in its buffer.
""".
-spec bufferize_all( file_input_stream() ) -> file_input_stream().
bufferize_all( FIS=#file_input_stream{ file=File, buffer=Buf } ) ->
    case file:read( File, ?default_read_chunk_size ) of

        { ok, Data } ->
            NewBuf = << Buf/binary, Data/binary >>,
            bufferize_all( FIS#file_input_stream{ buffer=NewBuf } );

        eof ->
            FIS;

        E={ error, _Reason } ->
            E

    end.




% File Input stream "destructor".


-doc """
Terminates the specified file input stream.

Not expected to fail, but may trace errors.
""".
-spec terminate( file_input_stream() ) -> void().
% Empty buffer, as usually expected:
terminate( FIS=#file_input_stream{ file=File, buffer= <<>> } ) ->
    case file:close( File ) of

        ok ->
            ok;


        { error, Reason } ->
            trace_utils:error_fmt( "When terminating the file input stream "
                "corresponding to '~ts', its closing failed with ~p.",
                [ FIS#file_input_stream.path, Reason ] )

    end;

% Non-empty buffer:
terminate( FIS=#file_input_stream{ path=BinPath, buffer=Buf } ) ->

    trace_utils:error_fmt( "When terminating the file input stream "
        "corresponding to file '~ts', a non-empty buffer (of ~B bytes) "
        "was found and has been dropped.", [ BinPath, size( Buf ) ] ),

    terminate( FIS#file_input_stream{ buffer= <<>> } ).





% We start here with the synchronous (blocking) interface.
%
% Note that no read_exactly/2 function has been defined, since end of streams
% have to be managed anyway: this corresponds to read_at_most/2.


-doc """
Reads synchronously some (reasonable, bounded) number of bytes from the
specified file input stream.
""".
-spec read_some( file_input_stream() ) -> data_reading().
read_some( FIS=#file_input_stream{ file=File, buffer=Buf } ) ->
    Reading = case file:read( File, ?default_read_chunk_size ) of

        { ok, Data } ->
            % Not being constrained here, returning as much data as possible, so
            % concatenates these binaries:
            %
            RetData = << Buf/binary, Data/binary >>,
            RetFIS = FIS#_{ buffer= <<>> },

            cond_utils:if_defined( myriad_check_streams, check_fis( RetFIS ) ),

            { RetData, RetFIS };

        eof ->
            manage_eof( Buf, FIS );

        E={ error, _Reason } ->
            E

    end,

    cond_utils:if_defined( myriad_check_streams, check_reading( Reading ) ),

    Reading.



-doc """
Reads synchronously at least the specified number of bytes from the specified
file input stream (minimum included).

Typically useful if needing a full chunk of data whose size is already known of
the caller.
""".
-spec read_at_least( MinCount :: byte_size(), file_input_stream() ) ->
                                                    data_reading().
read_at_least( MinCount, FIS=#file_input_stream{ file=File,
                                                 buffer=Buf } ) ->
    % Preferring larger chunks, so more than MinCount asked:
    Reading = case file:read( File, 1024 * MinCount ) of

        { ok, Data } ->

            % Can be lower than MinCount if eof is to come next:
            DataSize = size( Data ),

            FullData = << Buf/binary, Data/binary >>,
            FullDataSize = size( Buf ) + DataSize,

            % Possibly negative:
            MarginSize = FullDataSize - MinCount,

            case MarginSize >= 0 of

                true ->
                    RetFIS = FIS#_{ buffer= <<>> },
                    { FullData, RetFIS };

                _False ->
                    % Not enough data yet, wait for it:
                    NewFIS = FIS#_{ buffer=FullData },
                    read_at_least( -MarginSize, NewFIS )

            end;

        eof ->
            manage_eof( Buf, FIS );

        E={ error, _Reason } ->
            E

    end,

    cond_utils:if_defined( myriad_check_streams, check_reading( Reading ) ),

    Reading.



-doc """
Reads synchronously at most the specified number of bytes from the specified
file input stream (maximum included).

Useful if a maximum bound on read data applies (e.g. maximum packet size),
typically to avoid being overwhelmed by larger, forged entries.
""".
-spec read_at_most( MaxCount :: byte_size(), file_input_stream() ) ->
                                        data_reading().
read_at_most( MaxCount, FIS=#file_input_stream{ file=File, buffer=Buf } ) ->
   Reading = case file:read( File, MaxCount ) of

        { ok, Data } ->

            % Can be lower than MaxCount if eof is to come next:
            DataSize = size( Data ),

           % The buffer might be already larger:
           FullData = << Buf/binary, Data/binary >>,
           FullDataSize = size( Buf ) + DataSize,

           case FullDataSize =< MaxCount of

               % Already good:
               true ->
                   RetFIS = FIS#_{ buffer= <<>> },
                   { FullData, RetFIS };

               % Current data too large here, we send back the max size:
               _False ->
                   << MaxChunk:MaxCount/binary, RestBin/binary >> = FullData,
                   RetFIS = FIS#_{ buffer=RestBin },
                   { _RetData=MaxChunk, RetFIS }

           end;


        eof ->
            manage_eof( Buf, FIS );

        E={ error, _Reason } ->
            E

    end,

    cond_utils:if_defined( myriad_check_streams, check_reading( Reading ) ),

    Reading.



-doc """
Reads synchronously a number of bytes in the specified range (bounds included)
from the specified input stream.

Allows to secure a minimum size yet to limit the overall one.
""".
-spec read_between( MinCount :: byte_size(), MaxCount :: byte_size(),
                    file_input_stream() ) -> data_reading().
read_between( MinCount, MaxCount,
              FIS=#file_input_stream{ file=File, buffer=Buf } )
                                            when MinCount =< MaxCount ->
    Reading = case file:read( File, MaxCount ) of

        { ok, Data } ->
            DataSize = size( Data ),

            FullData = << Buf/binary, Data/binary >>,
            FullDataSize = size( Buf ) + DataSize,

            % Possibly negative:
            MarginSize = FullDataSize - MinCount,

            % First check against minimum:
            case MarginSize >= 0 of

                true ->
                    % Let's check now against max:
                    case FullDataSize =< MaxCount of

                       true ->
                           % Perfect case, return the whole:
                           RetFIS = FIS#_{ buffer= <<>> },
                           { FullData, RetFIS };

                       _False ->
                           % Too large, truncating needed (then at MaxCount):
                           << MaxChunk:MaxCount/binary, RestBin/binary >> =
                                FullData,

                           RetFIS = FIS#_{ buffer=RestBin },

                           { _RetData=MaxChunk, RetFIS }

                   end;

                _False ->
                    % Not enough data yet, wait for it:
                    NewFIS = FIS#_{ buffer=FullData },
                    read_at_least( -MarginSize, NewFIS )
            end;

        eof ->
            manage_eof( Buf, FIS );

        E={ error, _Reason } ->
            E

    end,

    cond_utils:if_defined( myriad_check_streams, check_reading( Reading ) ),

    Reading.




% Now any implemented asynchronous (non-blocking) interface:
%
% (as they may return an empty binary, no need to have them return a specific
% atom when they would block)



% -doc """
% Reads asynchronously any number of bytes from the specified input stream.
% """.
% -spec read_async( input_stream() ) -> data_reading().


% -doc """
% Reads asynchronously at least the specified number of bytes from the specified
% input stream (minimum included).
% """.
% -spec read_at_least_async( MinCount :: byte_size(), input_stream() ) ->
%                                         data_reading().


% -doc """
% Reads asynchronously at most the specified number of bytes from the specified
% input stream (maximum included).
% """.
% -spec read_at_most_async( MaxCount :: byte_size(), input_stream() ) ->
%                                         data_reading().


% -doc """
% Reads asynchronously a number of bytes in the specified range (bounds
% included) from the specified input stream.
% """.
% -spec read_between_async( MinCount :: byte_size(), MaxCount :: byte_size(),
%                           input_stream() ) -> data_reading().



-doc "Returns a textual description of the specified file input stream.".
-spec to_string( file_input_stream() ) -> ustring().
to_string( #file_input_stream{ path=AbsBinPath, buffer=Buf } ) ->
    text_utils:format( "input stream based on '~ts' (with ~B bytes in buffer)",
                       [ AbsBinPath, size( Buf ) ] ).




% Helpers


-doc """
Manages when the end of file is reached, depending on whether a buffer is still
available or not.
""".
% The internal file handle could be closed here, but it would force handlers to
% take into account a case where it would be equal to 'undefined' (whereas some
% I/O stream may want to append content).
%
-spec manage_eof( binary(), file_input_stream() ) -> data_reading().
manage_eof( _Buf= <<>>, FIS ) ->
    terminate( FIS ),
    end_of_stream;

manage_eof( Buf, FIS ) ->

    RetFIS = FIS#_{ buffer= <<>> },

    cond_utils:if_defined( myriad_check_streams, check_fis( RetFIS ) ),

    % Next time, will be end_of_stream:
    { Buf, RetFIS }.






-doc "Checks that the specified term is a legit file input stream.".
% file_input_stream() expected:
-spec check_fis( term() ) -> void().
check_fis( #file_input_stream{ path=Path, file=File, buffer=Buf } ) ->
    is_binary( Path ) orelse throw( { invalid_path, Path } ),
    file_utils:is_file_reference( File ) orelse throw( { invalid_file, File } ),
    is_binary( Buf ) orelse throw( { invalid_buffer, Buf } ).


-doc "Checks that the specified term is a legit read return value.".
% data_reading() expected:
-spec check_reading( term() ) -> void().
check_reading( { error, _Reason } ) ->
    ok;

check_reading( { ReadData, FIS } ) ->
    is_binary( ReadData ) orelse throw( { invalid_read_data, ReadData } ),
    check_fis( FIS );

check_reading( end_of_stream ) ->
    ok.

