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
% Creation date: Friday, October 30, 2026.

-module(input_output_stream).

-moduledoc """
Utility module for **input/output data streams** (a.k.a IO streams), which are
abstractions of any kind of devices from which bytes can be read and/or written
- like a suitable file, a socket, any concept of a channel or a pipe, etc.

An *input/output stream* corresponds to an instance created from a module that
implements the ``input_output_stream`` behaviour specified here.

See also the `output_stream`, `input_output_stream` and `stream_utils` modules.
""".


%-behaviour(input_stream).


-doc """
Any kind of **input/output** stream, from which bytes can be read and/or written
- like a suitable file, a socket, any concept of a channel or a pipe, etc.
""".
%input_output_stream:#input_output_stream{}.
-type input_output_stream() :: any().

%-export_record([ input_output_stream ]).


-doc "Convenient shorthand.".
-type io_stream() :: input_output_stream().

-export_type([ input_output_stream/0, io_stream/0 ]).

%-export([ to_string/1 ]).


% Type shorthands:

-type byte_size() :: system_utils:byte_size().






% Input/Output stream interface.
%
% This is just the union of an input stream and an output one with, in-between,
% a processing function to link them.
%
% Unfortunately

% Listing the callback of the input_stream).


-doc """
This interface function, a (stream) *processing function*, is meant to be
defined by any module implementing an IO stream. Its purpose is, based on the
role of this stream, to read data from its input, and to output data (most often
based on previous readings).

As IO streams may keep an internal memory, in the general case such processing
functions may generate output data based on the input one and on the state of
that stream.

At each call of a processing function, any number of readings can be
attempted/performed, and any data of any size can be output by a stream,
regardless of the size of the input data (therefore even if no data was read).

For example a `pipe` can be implemented simply by writing in the output of the
stream the successive chunks of data that it reads.

Conversely, a command like `sponge` can be implemented by exhausting the input
stream, caching it (by storing it in the internal buffer of that stream), and
allowing any consumer connected to its output stream to consume it.

A `random generator` I/O stream may either read nothing or just a seed, and
then, still based on its internal state, generate an arbitrary long series of
bytes.

Side-effects may also be done by processing functions, like for example sending
messages to processes, writing a separate file (e.g. to track stream
statistics), etc.

""".
-callback process_stream( io_stream() ) -> io_stream().




% Refer to the implementation notes centralised in the stream_utils module.




% We start here with the synchronous (blocking) interface:


-doc """
Reads synchronously any number of bytes from the specified input stream.
""".
-spec read( input_stream() ) -> data_reading().


-doc """
Reads synchronously at least the specified number of bytes from the specified
input stream (minimum included).
""".
-spec read_at_least( MinCount :: byte_size(), input_stream() ) ->
                                        data_reading().


-doc """
Reads synchronously at most the specified number of bytes from the specified
input stream (maximum included).
""".
-spec read_at_most( MaxCount :: byte_size(), input_stream() ) ->
                                        data_reading().


-doc """
Reads synchronously a number of bytes in the specified range (bounds included)
from the specified input stream.
""".
-spec read_between( MinCount :: byte_size(), MaxCount :: byte_size(),
                    input_stream() ) -> data_reading().


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
