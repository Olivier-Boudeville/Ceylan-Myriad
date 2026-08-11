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
   file :: file(),

   % Any buffered bytes (already obtained, but not returned yet to the caller):
   buffer :: binary() % (possibly empty)

}.



-doc """
A file-based input data streams, to read from a (possibly read-only file).

Possibly designated by FIS.
""".
-type file_input_stream() :: #file_input_stream{}.


-export_record([ file_input_stream ]).


-type data_reading() :: { stream_utils:stream_data(), file_input_stream() }
                      | 'end_of_stream' % Similar to eof
                      | { 'error', Reason :: any() }.


-export_type([ file_input_stream/0 ]).

%-export([ to_string/1 ]).




% Type shorthands:

-type byte_size() :: system_utils:byte_size().

-type any_file_path() :: file_utils:any_file_path().
-type bin_file_path() :: file_utils:bin_file_path().



% File Input stream interface.
%
% The stream on which to operate is always the last argument.


-doc """
Opens in read-only the file at the specified path, and returns the corresponding
(file-based) input stream.

Only the Erlang process that opened this file stream can use it.


""".
-spec open( any_file_path() ) -> file_input_stream().
open( AnyFilePath ) ->

    File = file:open( AnyFilePath,
        _Opts=[ read, { read_ahead, _Size=128000 }, raw, binary ] ),

    #file_input_stream{
        path=text_utils:ensure_binary( AnyFilePath ),
        file=File,
        buffer= <<>> }.



% We start here with the synchronous (blocking) interface:


-doc """
Reads synchronously any number of bytes from the specified file input stream.
""".
-spec read( file_input_stream() ) -> data_reading().
read( FIS=#file_input_stream{ file=File,
                              buffer=Buf } ) ->
    case file:read( File ) of

        { ok, Data } ->
            % Concatenates these binaries:
            RetData = << Buff/binary, Data/binary >>,
            RetFIS = FIS#{ buffer= <<>> },
            { RetData, RetFIS };



-doc """
Reads synchronously at least the specified number of bytes from the specified
file input stream (minimum included).
""".
-spec read_at_least( MinCount :: byte_size(), file_input_stream() ) ->
                                        data_reading().


-doc """
Reads synchronously at most the specified number of bytes from the specified
file input stream (maximum included).
""".
-spec read_at_most( MaxCount :: byte_size(), file_input_stream() ) ->
                                        data_reading().


-doc """
Reads synchronously a number of bytes in the specified range (bounds included)
from the specified input stream.
""".
-spec read_between( MinCount :: byte_size(), MaxCount :: byte_size(),
                    file_input_stream() ) -> data_reading().




% Now any implemented asynchronous (non-blocking) interface:
%
% (as they may return an empty binary, no need to have them return a specific
% atom when they would block)



-doc """
Reads asynchronously any number of bytes from the specified input stream.
""".
-spec read_async( input_stream() ) -> data_reading().


-doc """
Reads asynchronously at least the specified number of bytes from the specified
input stream (minimum included).
""".
-spec read_at_least_async( MinCount :: byte_size(), input_stream() ) ->
                                        data_reading().


-doc """
Reads asynchronously at most the specified number of bytes from the specified
input stream (maximum included).
""".
-spec read_at_most_async( MaxCount :: byte_size(), input_stream() ) ->
                                        data_reading().


-doc """
Reads asynchronously a number of bytes in the specified range (bounds included)
from the specified input stream.
""".
-spec read_between_async( MinCount :: byte_size(), MaxCount :: byte_size(),
                    input_stream() ) -> data_reading().
