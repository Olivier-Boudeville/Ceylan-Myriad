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
% Creation date: Thursday, July 9, 2026.

-module(stream_utils).

-moduledoc """
Utility module for **data streams**, which are abstractions of any kind of
input/output devices, like files, sockets, etc.

Streams are similar to the outputs of shell commands, whcih can be processed
(e.g. filtered) by pipes. Minor utilities to manage **pairs** (that is: all
kinds of 2-element tuples).
""".


-doc "A module in charge of handling a stream.".
-type stream_handler() :: basic_utils:module_name().



% Any kind of stream; refer to `stream/0`.
-record #stream{

   % The module in charge of handling that stream:
   handler :: stream_handler()

}.



-doc """
Any kind of stream.
""".
-type stream() :: #stream{}.


-export_record([ stream ]).


-export_type([ stream_handler/0, stream/0 ]).

-export([ get_handler/1, to_string/1 ]).




% Type shorthands:

-type ustring() :: text_utils:ustring().


-doc "Returns the handler module for the specified stream.".
-spec get_handler( stream() ) -> stream_handler().
get_handler( #stream{ handler=HandlerMod } ) ->
    HandlerMod.


-doc "Returns a textual description of the specified stream.".
-spec to_string( stream() ) -> ustring().
to_string( #stream{ handler=HandlerMod } ) ->
    text_utils:format( "stream of ~ts type", [ HandlerMod ] ).
