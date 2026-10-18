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

-module(output_stream).

-moduledoc """
Utility module for **output data streams**, which are abstractions of any kind
of output devices - hence to which bytes can be written - like a writable file,
the input of a pipe, a socket, an abstract input stream, etc.

An *output stream* corresponds to an instance created from a module that
implements the ``output_stream`` behaviour specified here.

This behaviour exposes functions that are synchronous (blocking), like
`write/1`.

See also the `input_stream`, `input_output_stream` and `stream_utils` modules.
""".

% In the future asynchronous (non-blocking) counterparts, whose name is
% postfixed with ``_async``, like ``write_async/1``, could be considered.



-doc """
Any kind of **output** stream, i.e. a stream that can be written to, like a
writable file, the input of a pipe, a socket, an abstract input stream, etc.
""".
-type output_stream() :: any(). % Actually closer to stream_utils:#stream{}.


%-export_record([ output_stream ]).



-doc "Term returned in case of write error.".
% 'write_error' would be less convenient for matching:
-type write_error() :: { 'error', Reason :: any() }.


-doc "A full (complete) data writing, typically done to an output stream.".
-type writing_outcome() :: output_stream() | write_error().


-export_type([ output_stream/0, writing_outcome/0, write_error/0 ]).



% Type shorthands:

%-type byte_size() :: system_utils:byte_size().

-type ustring() :: text_utils:ustring().

-type stream_data() :: stream_utils:stream_data().
%-type buffer() :: stream_utils:buffer().


% Refer to the implementation notes centralised in the stream_utils module.


% Output stream interface.
%
% Most output streams will also support:
%
% - at least one constructor function (e.g. create/*, open/*), returning an
% output stream of that kind
%
% - possibly a write/2 function, constructing an output stream of that kind,
% writing the specified content to it
%
% - at least one destructor function (reciprocal of the constructor ones,
% e.g. destruct/*, close/*), terminating properly an output stream of that kind



% We start here with the synchronous (blocking) interface:
% (as we are in a behaviour module, these are callback specifications)


-doc """
Writes synchronously the specified data content to the specified output stream.
""".
-callback write( stream_data(), output_stream() ) -> writing_outcome().




% Now any implemented asynchronous (non-blocking) interface:
%
% (as they may return an empty binary, no need to have them return a specific
% atom when they would block)


% Currently not implemented, as asynchronous I/O may not be relevant in Erlang.

%-doc """
%Writes asynchronously any number of bytes from the specified output stream.
%""".
%-callback write_async( stream_data(), output_stream() ) ->
%                                       async_writing_outcome().



-doc "Returns a textual description of this output stream.".
-callback to_string( output_stream() ) -> ustring().
