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
output of a pipe, etc.

An *input stream* corresponds to an instance created from a module that
implements the ``input_stream`` behaviour specified here.

This behaviour exposes functions that are synchronous (blocking), like `read/1`,
and their asynchronous (non-blocking) counterparts, whose name is postfixed with
``_async``, like ``read_async/1``.

See also the `output_stream`, `input_output_stream` and `stream_utils` modules.
""".


%-behaviour(input_stream).


-doc """
Any kind of **input** stream, i.e. a stream that can be read from, like a
read-only file, the output of a pipe, etc.
""".
-type input_stream() :: any(). %stream:#stream{}.


%-export_record([ input_stream ]).


-type data_reading() :: { stream_utils:stream_data(), input_stream() }
                      | 'end_of_stream' % Similar to eof
                      | { 'error', Reason :: any() }.


-export_type([ input_stream/0, data_reading/0 ]).

%-export([ to_string/1 ]).


% Type shorthands:

-type byte_size() :: system_utils:byte_size().


% Refer to the implementation notes centralised in the stream_utils module.


% Input stream interface.
%




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
