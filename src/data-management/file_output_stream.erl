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

-module(file_output_stream).

-moduledoc """
Utility module for **file-based output data streams**, to write to a (writable)
file.

See also the `output_stream` module and its corresponding behaviour.
""".


% Implementing it here:
-behaviour(output_stream).


-record #file_output_stream{

   % The current module is in charge of handling that stream.

   % The path of the (writable) file to which this file output stream
   % corresponds:
   %
   path :: bin_file_path(),

   % The actual file handle that corresponds to this output stream:
   file :: file_utils:file()

   % No real need for an internal buffer.

}.

-export_record([ file_output_stream ]).



-doc """
A file-based output data stream, in order to write to a (writable) file.

Possibly designated by FOS.
""".
-type file_output_stream() :: #file_output_stream{}.


-doc "Shorthand for `file_output_stream/0`.".
-type fos() :: file_output_stream().


-doc "A full (complete) data writing, typically done to a file output stream.".
-type writing_outcome() :: file_output_stream() | write_error().


-export_type([ file_output_stream/0, fos/0, writing_outcome/0 ]).



% FOS-specific API:
-export([ open/1, write_whole_nothrow/2, write_whole/2, close/1 ]).


% Implementation of the output_stream behaviour:
-export([ write/2, to_string/1 ]).



% Implementation notes: refer to the ones of the stream_utils module.




% Type shorthands:

-type ustring() :: text_utils:ustring().


-type any_file_path() :: file_utils:any_file_path().
-type bin_file_path() :: file_utils:bin_file_path().

-type base_outcome() :: basic_utils:base_outcome().

-type stream_data() :: stream_utils:stream_data().
-type write_error() :: output_stream:write_error().



% File Output stream interface.
%
% The stream on which to operate is always the last argument.



% File Output stream constructors.


-doc """
Creates a file-based output stream, by (only) opening the file at the specified
path.

Only the Erlang process that opened this file stream can use it.

Return `{ok, OpenedFOS}` or `{error, AtomReason}`.
""".
-spec open( any_file_path() ) -> result_fallible( file_output_stream() ).
open( AnyFilePath ) ->

    AbsBinPath = text_utils:ensure_binary(
        file_utils:ensure_path_is_absolute( AnyFilePath ) ),

    % More direct control than with file_utils:open/2:
    case file:open( AbsBinPath,
                    _Opts=[ write, delayed_write, raw, binary ] ) of

        { ok, File } ->
            FOS = #file_output_stream{ path=AbsBinPath, file=File },

            cond_utils:if_defined( myriad_check_streams, check_fos( FOS ) ),

            { ok, FOS };


        E={ error, _AtomReason } ->
            E

    end.





% File Output stream "destructor".


-doc """
Closes the specified file output stream.

Not expected to fail, but may trace errors.
""".
-spec close( file_output_stream() ) -> base_outcome().
close( _FOS=#file_output_stream{ file=File } ) ->

    %trace_utils:debug_fmt( "Closing ~p", [ FOS ] ),

    % Sufficient:
    file:close( File ).

%     case file:close( File ) of
%
%         ok ->
%             ok;
%
%         P={ error, Reason } ->
%             trace_utils:error_fmt( "When terminating the file output stream "
%                 "corresponding to '~ts', its closing failed with ~p.",
%                 [ FOS#_.path, Reason ] ),
%           P
%
%     end.




% We start here with the synchronous (blocking) interface.


-doc """
Writes synchronously some (reasonable, bounded) number of bytes from the
specified file output stream.

If returning `end_of_stream`, closes automatically the specified file output
stream.
""".
-spec write( stream_data(), file_output_stream() ) ->  writing_outcome().
write( BinData, FOS=#file_output_stream{ file=File } ) ->
    case file:write( File, BinData ) of

        ok ->
            FOS;

        % E={error, _Reason}:
        E ->
            E

    end.




-doc """
Writes in the specified file the whole specified content, by creating a
corresponding file output stream, writing it fully, and closing that stream.

Any already-existing file at this path will be silently overwritten.

Does not throw.
""".
-spec write_whole_nothrow( any_file_path(), stream_data() ) -> base_outcome().
write_whole_nothrow( AnyFilePath, BinData ) ->
    case open( AnyFilePath ) of

        { ok, FOS } ->
            case write( BinData, FOS ) of

                E={ error, _Reason } ->
                    E;

                WrittenFOS ->
                     close( WrittenFOS )

            end;

        TaggedError ->
            TaggedError

    end.



-doc """
Writes in the specified file the whole specified content, by creating a
corresponding file output stream, writing it fully, and closing that stream.

Any already-existing file at this path will be silently overwritten.

Throws an exception if the operation failed.
""".
-spec write_whole( any_file_path(), stream_data() ) -> void().
write_whole( AnyFilePath, BinData ) ->
    case write_whole_nothrow( AnyFilePath, BinData ) of

        ok ->
            ok;

        % Quite alike file_utils:write_whole/2:

        { error, eacces } ->
            throw( { write_whole_failed,
                     text_utils:ensure_string( AnyFilePath ), access_denied,
                     file_utils:get_file_access_denied_info( AnyFilePath ) } );

        { error, Error } ->
            throw( { write_whole_failed,
                     text_utils:ensure_string( AnyFilePath ), Error } )

    end.




% Helpers




-doc "Checks that the specified term is a legit file output stream.".
% file_output_stream() expected:
-spec check_fos( term() ) -> void().
check_fos( #file_output_stream{ path=Path, file=File } ) ->

    is_binary( Path ) orelse throw( { invalid_path, Path } ),

    % Not expected to be a PID though:
    file_utils:is_file_reference( File ) orelse throw( { invalid_file, File } ).


-doc "Returns a textual description of the specified file output stream.".
-spec to_string( file_output_stream() ) -> ustring().
to_string( #file_output_stream{ path=AbsBinPath } ) ->
    text_utils:format( "output stream based on '~ts'", [ AbsBinPath ] ).
