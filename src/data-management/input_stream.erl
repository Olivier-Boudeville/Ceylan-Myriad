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

-module(input_stream).

-moduledoc """
Utility module for **input data streams**, which are abstractions of any kind of
input devices - hence from which bytes can be read - like a read-only file, the
output of a pipe, a socket, an abstract output stream, etc.

An *input stream* corresponds to an instance created from a module that
implements the ``input_stream`` behaviour specified here.

This behaviour exposes functions that are synchronous (blocking), like `read/1`,
and their asynchronous (non-blocking) counterparts, whose name is postfixed with
``_async``, like ``read_async/1``.

See also the `output_stream`, `input_output_stream` and `stream_utils` modules.
""".



-doc """
Any kind of **input** stream, i.e. a stream that can be read from, like a
read-only file, the output of a pipe, a socket, an abstract output stream, etc.
""".
-type input_stream() :: any(). % Actually closer to stream_utils:#stream{}.


%-export_record([ input_stream ]).


% Type shorthands:

-type stream_data() :: stream_utils:stream_data().

-doc "Term returned in case of read error.".
-type read_error() :: { 'error', Reason :: any() }


-doc "A full (complete) data reading, typically done from an input stream.".
-type full_data_reading() :: { stream_data(), input_stream() } | read_error().


-doc "A data reading, typically done from an input stream.".
-type data_reading() :: full_data_reading()
                      | 'end_of_stream'. % Similar to eof


-export_type([ input_stream/0, data_reading/0, full_data_reading/0,
               read_error/0 ]).


% Exported helper functions:
-export([ bufferize_all/1, read_all/1 ]).



% Type shorthands:

-type byte_size() :: system_utils:byte_size().

-type ustring() :: text_utils:ustring().

-type buffer() :: stream_utils:buffer().


% Refer to the implementation notes centralised in the stream_utils module.


% Input stream interface.
%
% Most input streams will also support:
%
% - at least one constructor function (e.g. create/*, open/*), returning an
% input stream of that kind
%
% - possibly a read_all/* function, constructing an input stream of that kind,
% read all content from it, terminating that stream, and returning a
% corresponding full_data_reading/0
%
% - at least one destructor function (reciprocal of the constructor ones,
% e.g. destruct/*, close/*), terminating properly an input stream of that kind



% We start here with the synchronous (blocking) interface:


-doc """
Reads synchronously any number of bytes from the specified input stream.

So will block until at least one byte can be read - yet will attempt then to
read and return as many bytes as available.
""".
-callback read_some( input_stream() ) -> data_reading().


-doc """
Reads synchronously at least the specified number of bytes from the specified
input stream (minimum included).

So will block until at least `MinCount` bytes can be read - yet will attempt
then to read and return as many bytes as available.
""".
-callback read_at_least( MinCount :: byte_size(), input_stream() ) ->
                                        data_reading().


-doc """
Reads synchronously at most the specified number of bytes from the specified
input stream (maximum included).

So will block until at least one byte can be read - yet will attempt
then to read and return as many bytes as available, up to `MaxCount` bytes.
""".
-callback read_at_most( MaxCount :: byte_size(), input_stream() ) ->
                                        data_reading().


-doc """
Reads synchronously a number of bytes in the specified range (bounds included)
from the specified input stream.

So will block until at least `MinCount` bytes can be read - yet will attempt
then to read and return as many bytes as available, up to `MaxCount` bytes.
""".
-callback read_between( MinCount :: byte_size(), MaxCount :: byte_size(),
                        input_stream() ) -> data_reading().


-doc """
Reads synchronously exactly the specified number of bytes from the specified
input stream.

So will block until exactly `Count` bytes can be read and returned.
""".
-callback read_exactly( Count :: byte_size(), input_stream() ) ->
                                        data_reading().


-doc """
Reads data (once) from the underlying source, and appends it in its internal
buffer, returning the updated stream - unless reaching its end or encountering
an error.
""".
-callback feed_buffer( input_stream() ) ->
    input_stream() | 'end_of_stream' | read_error().


-doc """
Appends the specified (binary) data to the internal of the specified stream,
returning it.
""".
-callback append_to_buffer( binary(), input_stream() ) -> input_stream()


-doc """
Returns the current internal buffer of the specified input stream, flushing it
(the stream has therefore no more data available internally).
""".
-callback flush_buffer( input_stream() ) -> { buffer(), input_stream() }.



% Now any implemented asynchronous (non-blocking) interface:
%
% (as they may return an empty binary, no need to have them return a specific
% atom when they would block)


% Currently not implemented, as asynchronous I/O may not be relevant in Erlang.

%-doc """
%Reads asynchronously any number of bytes from the specified input stream.
%""".
%-callback read_async( input_stream() ) -> data_reading().


% -doc """
% Reads asynchronously at least the specified number of bytes from the specified
% input stream (minimum included).
% """.
% -callback read_at_least_async( MinCount :: byte_size(), input_stream() ) ->
%                                         data_reading().


% -doc """
% Reads asynchronously at most the specified number of bytes from the specified
% input stream (maximum included).
% """.
% -callback read_at_most_async( MaxCount :: byte_size(), input_stream() ) ->
%                                         data_reading().


% -doc """
% Reads asynchronously a number of bytes in the specified range (bounds
% included) from the specified input stream.
% """.
% -callback read_between_async( MinCount :: byte_size(),
%    MaxCount :: byte_size(), input_stream() ) -> data_reading().



-doc "Returns a textual description of this input stream.".
-callback to_string( input_stream() ) -> ustring().



% Helper (exported) functions.


-doc """
Reads synchronously all available content from that input stream in its internal
buffer, until reaching its end-of-stream.
""".
-spec bufferize_all( input_stream() ) -> input_stream() | read_error().
bufferize_all( IS ) ->
    case feed_buffer( IS ) of

        end_of_stream ->
            IS;

        P={ error, Reason } ->
            P;

        ReadIS ->
            bufferize_all( ReadIS )

    end.




-doc """
Reads all data from the underlying source, and returns it, together with a
flushed input stream.
""".
-spec read_all( input_stream() ) -> full_data_reading().
read_all( IS ) ->
    case bufferize_all( IS ) of

        P={ error, Reason } ->
            P;

        IS ->
            flush_buffer( IS )

    end.
