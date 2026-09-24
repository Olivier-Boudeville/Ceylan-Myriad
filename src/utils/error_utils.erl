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
% Creation date: Tuesday, September 8, 2026.

-module(error_utils).

-moduledoc """
Services regarding error management.
""".


-doc "The various errors that may be reported by POSIX calls.".
-type posix_error() :: 'eacces' | 'eagain' | 'ebadf' | 'ebusy' | 'edquot'
    | 'eexist' | 'efault' | 'efbig' | 'eintr' | 'einval' | 'eio' | 'eisdir'
    | 'eloop' | 'emfile' | 'emlink' | 'enametoolong' | 'enfile' | 'enodev'
    | 'enoent' | 'enomem' | 'enospc' | 'enotblk' | 'enotdir' | 'enotsup'
    | 'eperm' | 'erofs' | 'espipe' | 'esrch' | 'exdev'.


-doc "A (generic) short description of an error.".
-type short_description() :: ustring().

-doc "A (generic) longer description of an error.".
-type long_description() :: ustring().


-export_type([ posix_error/0, short_description/0, long_description/0 ]).


-export([ get_descriptions/1 ]).


-type ustring() :: text_utils:ustring().



-doc """
Returns the short and long descriptions associated to the specified POSIX error.
""".
-spec get_descriptions( posix_error() ) ->
                                { short_description(), long_description() }.
get_descriptions( _PosixError=eacces ) ->
    { "Permission denied",
      "The process does not have the required rights to read the file or one of its parent directories" };

get_descriptions( _PosixError=eagain ) ->
    { "Resource temporarily unavailable",
      "The filesystem or underlying resource cannot serve the request at the moment" };

get_descriptions( _PosixError=ebadf ) ->
    { "Bad file descriptor",
      "An invalid or inappropriate file descriptor was used internally" };

get_descriptions( _PosixError=ebusy ) ->
    { "Resource busy",
      "The file or directory is currently locked or in use by the system" };

get_descriptions( _PosixError=edquot ) ->
    { "Disk quota exceeded",
      "The user has reached their allowed quota on the filesystem" };

get_descriptions( _PosixError=eexist ) ->
    { "File exists", "The operation conflicts with an existing file or link" };

get_descriptions( _PosixError=efault ) ->
    { "Bad address",
      "A pointer passed to the system call refers to invalid memory" };

get_descriptions( _PosixError=efbig ) ->
    { "File too large",
      "The operation would exceed the maximum allowed file size" };

get_descriptions( _PosixError=eintr ) ->
    { "Interrupted system call",
      "The operation was interrupted by a signal before completion" };

get_descriptions( _PosixError=einval ) ->
    { "Invalid argument",
      "The path or parameters are not valid for this operation" };

get_descriptions( _PosixError=eio ) ->
    { "I/O error", "A low‑level disk or filesystem error occurred" };

get_descriptions( _PosixError=eisdir ) ->
    { "Is a directory", "The operation is not allowed on a directory" };

get_descriptions( _PosixError=eloop ) ->
    { "Too many levels of symbolic links",
      "The path contains a symlink loop or excessive indirection" };

get_descriptions( _PosixError=emfile ) ->
    { "Too many open files",
      "The process has reached its file descriptor limit" };

get_descriptions( _PosixError=emlink ) ->
    { "Too many links", "The maximum number of hard links has been reached" };

get_descriptions( _PosixError=enametoolong ) ->
    { "Filename too long", "The path exceeds system limits" };

get_descriptions( _PosixError=enfile ) ->
    { "File table overflow",
      "The system-wide limit on open files has been reached" };

get_descriptions( _PosixErrorenodev=enodev ) ->
    { "No such device",
      "The filesystem or device does not exist or is not available" };

get_descriptions( _PosixError=enoent ) ->
    { "No such file or directory", "The path does not exist" };

get_descriptions( _PosixError=enomem ) ->
    { "Out of memory", "The system cannot allocate memory for the operation" };

get_descriptions( _PosixError=enospc ) ->
    { "No space left on device", "The filesystem is full" };

get_descriptions( _PosixError=enotblk ) ->
    { "Block device required",
      "The operation requires a block device but the target is not one" };

get_descriptions( _PosixError=enotdir ) ->
    { "Not a directory", "A component of the path is not a directory" };

get_descriptions( _PosixError=enotsup ) ->
    { "Operation not supported",
      "The filesystem does not support this operation" };

get_descriptions( _PosixError=eperm ) ->
    { "Operation not permitted", "The process lacks the privileges required" };

get_descriptions( _PosixError=erofs ) ->
    { "Read-only filesystem",
      "The operation would modify a read-only filesystem" };

get_descriptions( _PosixError=espipe ) ->
    { "Illegal seek",
      "The operation attempted a seek on a non-seekable stream" };

get_descriptions( _PosixError=esrch ) ->
    { "No such process", "A required process or thread does not exist" };

get_descriptions( _PosixError=exdev ) ->
    { "Cross-device link",
      "The operation is not allowed across different filesystems" }.

