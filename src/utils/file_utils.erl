% Copyright (C) 2008-2025 Olivier Boudeville
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
% Creation date: Saturday, July 12, 2008.

-module(file_utils).

-moduledoc """
Gathering of various facilities regarding **files and other filesystem
elements**.

See `file_utils_test.erl` for the corresponding test.
""".



% Related standard modules: file, filename.


% Implementation notes:

% Using the file module has been known to cause synchronization overheads, often
% prim_file is used instead.

% For file formats, notably the ETF one, refer to:
% https://myriad.esperide.org/#file-formats


% Filename-related operations.
-export([ join/1, join/2, bin_join/1, bin_join/2, any_join/1, any_join/2,
		  split/1,

		  get_base_path/1, get_base_path/2,
		  get_last_path_element/1, split_path/1,

		  resolve_path/1, resolve_any_path/1,

		  convert_to_filename/1, convert_to_filename_with_extension/2,
		  escape_path/1,

		  get_extensions/1, get_extension/1, get_dotted_extension_for/1,
		  add_extension/2,
		  remove_extension/1, remove_extension/2, replace_extension/3,

		  exists/1, get_type_of/1, resolve_type_of/1,
		  resolve_symlink_once/1, resolve_symlink_fully/1,

		  get_owner_of/1, describe_owner_of/1,
          get_group_of/1, describe_group_of/1,

		  is_file/1, is_link/1,
		  is_existing_file/1, is_existing_link/1,
		  is_existing_file_or_link/1,
		  is_owner_readable/1, is_owner_writable/1, is_owner_executable/1,
		  is_user_readable/1, is_user_writable/1, is_user_executable/1,

		  is_directory/1, is_existing_directory/1,
		  is_existing_directory_or_link/1,
		  list_dir_elements/1, list_dir_elements/2,

		  check_existing_file/1, check_existing_file_or_link/1,
		  check_existing_directory/1,

          get_element_access_denied_info/1, get_file_access_denied_info/1,
          get_directory_access_denied_info/1,

		  get_size/1, get_last_modification_time/1, touch/1,
		  create_empty_file/1, create_non_clashing_file/0,

		  get_current_directory/0, get_bin_current_directory/0,
		  set_current_directory/1,

		  get_first_existing_directory_in/1, get_first_file_or_link_for/2,

		  filter_by_extension/2, filter_by_extensions/2,
		  filter_by_included_suffixes/2, filter_by_excluded_suffixes/2,
		  has_matching_suffix/2,

		  find_files_from/1, find_files_from/2, find_files_from/3,
		  find_regular_files_from/1, find_links_from/1, find_links_from/2,
		  find_files_with_extension_from/2, find_files_with_extension_from/3,

		  find_files_with_excluded_dirs/2, find_files_with_excluded_dirs/3,
		  find_files_with_excluded_dirs/4,

		  find_files_with_excluded_suffixes/2,
		  find_files_with_excluded_suffixes/3,
		  find_files_with_excluded_dirs_and_suffixes/3,
		  find_directories_from/1,

		  create_directory/1, create_directory/2,
		  create_directory_if_not_existing/1,
		  create_directory_if_not_existing/2,
		  create_temporary_directory/0,

		  remove_file/1, remove_file_if_existing/1,
		  remove_files/1, remove_files_if_existing/1,

		  remove_symlink/1, remove_symlink_if_existing/1,

		  % Note: no need for remove_file_or_link/1, remove_file does that.
		  remove_file_or_link_if_existing/1,

		  remove_empty_directory/1, remove_empty_path/1, remove_empty_tree/1,
		  remove_directory/1, remove_directory_if_existing/1,
		  remove_directories/1, remove_directories_if_existing/1,

		  copy_file/2, try_copy_file/2, copy_file_if_existing/2, copy_file_in/2,
		  copy_as_regular_file_in/2,

		  copy_tree/2,

		  rename/2, rename_preserving/2, rename_preserving/3,
		  move_file/2, create_link/2,
		  hide/1, hide/2, hide_overwriting/1, hide_overwriting/2,

		  get_non_clashing_entry_name_from/1,

		  append_file/2,

		  list_permission_pairs/0, to_permission_mask/1, from_permission_mask/1,

		  get_permissions_of/1, describe_permissions_of/1,
          change_permissions/2,

		  is_absolute_path/1,
		  ensure_path_is_absolute/1, ensure_path_is_absolute/2,
		  normalise_path/1, make_relative/1, make_relative/2,
		  get_longest_common_path/1, get_shortest_unique_ending_paths/2,

		  is_leaf_among/2,

		  update_with_keywords/3, update_with_keywords/4,

		  path_to_variable_name/1, path_to_variable_name/2,

		  remove_upper_levels_and_extension/1,

		  get_image_extensions/0, get_image_file_png/1, get_image_file_gif/1 ]).


% OS / project / application specific paths:
-export([ get_cache_directory/1,

		  get_configuration_directory/1,
		  get_most_suitable_configuration_directory/1,

		  get_extra_configuration_directories/1,

		  get_data_directory/1, get_extra_data_directories/1,

		  get_log_directory/1 ]).



% I/O section.
-export([ get_default_encoding/0, get_default_encoding_option/0,
		  latin1_file_to_unicode/1,

		  open/2, open/3, create_preserving/2, create_preserving/3,
		  close/1, close/2,

		  read/2, write/2, write_ustring/2, write_ustring/3,
		  read_whole/1, write_whole/2, write_whole/3,
		  write_whole_in_non_clashing/1,
		  read_lines/1,
		  read_etf_file/1, read_terms/1,
		  write_etf_file/2, write_etf_file/4,
		  write_terms/2, write_terms/4,
		  write_direct_terms/2,
		  is_file_reference/1 ]).


% Compression-related operations.
-export([ get_extension_for/1,
		  compress/1, compress/2, decompress/1, decompress/2,
		  file_to_zipped_term/1, zipped_term_to_unzipped_file/1,
		  zipped_term_to_unzipped_file/2,
		  files_to_zipped_term/1, files_to_zipped_term/2,
		  zipped_term_to_unzipped_files/1, zipped_term_to_unzipped_files/2 ]).



% For the file_info record:
-include_lib("kernel/include/file.hrl").


% For default_encoding*:
-include("system_utils.hrl").

% For the app_info record:
-include("app_facilities.hrl").



% Type declarations:


-doc """
A path may designate either a file or a directory (in both case with leading,
root directories possibly specified).
""".
-type path() :: ustring().


-doc "A binary path.".
-type bin_path() :: bin_string().


% We do not believe that atoms shall be legit paths:
-doc "Any kind of path.".
-type any_path() :: path() | bin_path().



-doc """
A path that can be resolved at runtime, all its elements being joined
accordingly.

For example `[home, "Project", lang, <<"simulation">>, user]` being translated
to the `"/home/bond/Project/en_GB.utf8/simulation/bond"` path.

See `resolve_path/1`.
""".
-type resolvable_path() :: [ resolvable_path_element() ].



-doc "An element of a resolvable path.".
-type resolvable_path_element() :: any_path() | resolvable_token_path().



-doc "A token translated at runtime as a path element.".
-type resolvable_token_path() ::

	'user_name' % Will be translated to the name of the current OS-level user;
				% e.g. "bond".

  | 'user_id' % Will be translated to the OS-level (typically UNIX uid);
			  % e.g. "61917".

  | 'group_name' % Will be translated to the name of the current OS-level group;
				 % e.g. "wheel".

  | 'group_id' % Will be translated to the OS-level (typically UNIX gid)
			   % identifier of the current group; e.g. "61000".

  | 'home' % Will be translated to the path to the home directory of the
		   % current user; e.g. "/home/bond".

  | 'locale_charset' % Will be translated to the current locale with the
					 % associated character set; e.g. "en_GB.utf8".

  | 'fqdn' % Will be translated to the current FQDN of the local host;
		   % e.g. "hurricane.foobar.org".

  | 'short_hostname'. % Will be translated to the current short name of
					  % the local host; e.g. "hurricane".



-doc "Any kind of path, resolvable or not.".
-type possibly_resolvable_path() :: resolvable_path() | any_path().



-doc """
Designates a filename, generally without a path (e.g. `"foobar.txt"`).
""".
-type file_name() :: path().


-doc "Just a convenience type alias (which is the preferred version).".
-type filename() :: file_name().



-doc """
Designates a path to a file (including its filename),
e.g. `"../my_dir/other/foobar.txt"`.
""".
-type file_path() :: path().


-doc "A binary filename.".
-type bin_file_name() :: bin_string().


-doc "A binary file path.".
-type bin_file_path() :: bin_string().



% Could also be the more general file:name_all().
-doc "Any filename.".
-type any_file_name() :: file_name() | bin_file_name().


-doc "Any file path.".
-type any_file_path() :: file_path() | bin_file_path().



-doc """
Designates a path to a device (including its device name),
e.g. `"/dev/ttyUSB0"`.
""".
-type device_path() :: path().


-doc """
Designates a (binary) path to a device (including its device name), e.g.
`"/dev/ttyUSB0"`.
""".
-type bin_device_path() :: bin_path().


-doc """
Designates any type of path to a device (including its device name), e.g.
`"/dev/ttyUSB0"`.
""".
-type any_device_path() :: device_path() | bin_device_path().


-doc "The name of a (symbolic) link.".
-type link_name() :: ustring().


-doc "The path of a (symbolic) link.".
-type link_path() :: file_path().


-doc "The (binary) path of a (symbolic) link.".
-type bin_link_path() :: bin_file_path().


-doc "Any type of path for a (symbolic) link.".
-type any_link_path() :: link_path() | bin_file_path().



-doc """
Designates an executable, generally without a path (e.g. `"foobar"`).
""".
-type executable_name() :: file_name().


-doc """
Designates a path to an executable, e.g. `"../my_dir/other/run.exe"`.
""".
-type executable_path() :: file_path().


-doc "Designates a path to an executable, as a binary.".
-type bin_executable_path() :: bin_file_path().


-doc "Any type of path to an executable.".
-type any_executable_path() :: executable_path() | bin_executable_path().



-doc """
Designates a path to an (executable) script, e.g. `"../my_dir/other/run.sh"`.
""".
-type script_path() :: file_path().


-doc "Designates a path to an (executable) script, as a binary.".
-type bin_script_path() :: bin_file_path().


-doc "A name of a directory.".
-type directory_name() :: path().


-doc "A name of a directory, as a binary.".
-type bin_directory_name() :: bin_string().


-doc "Any directory name.".
-type any_directory_name() :: directory_name() | bin_directory_name().



-doc "A path to a directory.".
-type directory_path() :: path().


-doc "A path to a directory, as a binary.".
-type bin_directory_path() :: bin_string().


-doc "Any directory path.".
-type any_directory_path() :: directory_path() | bin_directory_path().


-doc "An absolute directory path.".
-type abs_directory_path() :: directory_path().

-doc "Any absolute directory path.".
-type any_abs_directory_path() :: any_directory_path().



-doc """
The part of a filename before the dot of the first extension.

For example the filename radix of `"hello.tar.gz"` is `"hello"`.
""".
-type filename_radix() :: ustring().



-doc """
The part of a file path before the dot of the first extension.

For example the filepath radix of `"/home/bond/hello.tar.gz"` is
`"/home/bond/hello"`.
""".
-type filepath_radix() :: ustring().



-doc """
An extension of a filename, either unitary (e.g. `"baz"`, in
`"foobar.baz.json"`) or composed (e.g. `"tar.gz"` in `"hello.tar.gz"`).

An extension by itself does not include the leading dot (e.g. `"gz"`, not
`".gz"`), see `dotted_extension/0`.
""".
-type extension() :: ustring().



-doc """
A dot followed by the extension of a filename, either unitary (e.g. `".baz"`, in
`"foobar.baz.json"`) or composed (e.g. `".tar.gz"` in `"hello.tar.gz"`).
""".
-type dotted_extension() :: ustring().



-doc """
The suffix (final part) in a path element (e.g. `"share"` in
`"/usr/local/share"`).
""".
-type any_suffix() :: any_string().


-doc """
A (legit) part of a path (e.g. `"local"` in `"/usr/local/share"`); preferably
without whitespaces.

`".."` means the parent directory.
""".
-type path_element() :: ustring().


-doc """
A (legit) part, as a binary, of a path (e.g. `<<"local">>` in
`"/usr/local/share"`); preferably without whitespaces.

`<<"..">>` means the parent directory.
""".
-type bin_path_element() :: bin_string().


-doc """
Any (legit) type of a part of a path (e.g. `<<"local">>` in
`"/usr/local/share"`); preferably without whitespaces.

`".."` / `<<"..">>` mean the parent directory.
""".
-type any_path_element() :: path_element() | bin_path_element().



-doc """
A depth in a filesystem, when seen as a tree.

For example the depth of `"a"` in `"foo/bar/a"` is 3.
""".
-type depth() :: count().



-doc """
A leaf name, i.e. the final element of a path (possibly a file or directory).

For example in `aaa/bbb/ccc`, `aaa` is the root, and `ccc` is the (single) leaf.
""".
-type leaf_name() :: path_element().


-doc "All known types of filesystem entries.".
-type entry_type() :: 'device' | 'directory' | 'other' | 'regular' | 'symlink'.



-doc "Tells whether parent directories shall be created.".
-type parent_creation() :: 'create_no_parent' | 'create_parents'.



-doc """
Relevant flags when opening a file (e.g. read, write, append, exclusive, raw,
etc.).

See [http://erlang.org/doc/man/file.html#open-2] for their detailed description.
""".
% (type file:mode() not exported currently unfortunately, see
% lib/kernel/src/file.erl)
%
%-type file_open_mode() :: file:mode() | 'ram'.
-type file_open_mode() :: tuple() | atom() | 'ram'.



-doc "The supported compression formats.".
-type compression_format() :: 'zip' | 'bzip2' | 'xz'.



-doc """
Corresponds to the handle to an open file (typically a file descriptor
counterpart), but also, possibly, `standard_io` (for standard output, descriptor
1), `standard_error` (for standard error, descriptor 2), a registered name (as
an atom), or any PID handling the I/O protocols.
""".
-type file() :: file:io_device().


-doc "Information about a file".
-type file_info() :: #file_info{}.


-doc "The various permissions that can be combined for file-like elements.".
-type permission() :: 'owner_read'  | 'owner_write' | 'owner_execute'
					| 'group_read'  | 'group_write' | 'group_execute'
					| 'other_read'  | 'other_write' | 'other_execute'
					| 'set_user_id' | 'set_group_id'.



-doc "The binary mask corresponding to a filesystem permission.".
-type permission_mask() :: non_neg_integer().



% We previously considered also (was not satisfactory, as introducing a
% different return type):
%    | 'list'.    % List separately (as a returned pair) the raw filenames
%                  % (regardless of their actual filesystem-level type)
-doc """
Action to trigger whenever a file element does not have a proper Unicode
filename.

(refer to
[https://erlang.org/doc/apps/stdlib/unicode_usage.html#notes-about-raw-filenames]
for further information)
""".
-type improper_encoding_action() ::

	% Throw an exception as soon as a raw filename is found:
	'throw'

	% Emit a warning trace if a raw filename is found, and do not consider the
	% corresponding file element:
	%
  | 'warn'

	% Ignore as a whole such a file element (do not even emit a trace):
  | 'ignore'

	% Return such raw filenames (thus as binaries) among the other ones
	% (which are plain strings):
	%
  | 'include'.


-export_type([ path/0, bin_path/0, any_path/0,

			   resolvable_path/0, resolvable_path_element/0,
			   resolvable_token_path/0, possibly_resolvable_path/0,

			   file_name/0, filename/0, file_path/0,
			   bin_file_name/0, bin_file_path/0,
			   any_file_name/0, any_file_path/0,

			   link_name/0, link_path/0, bin_link_path/0, any_link_path/0,

			   device_path/0, bin_device_path/0, any_device_path/0,

			   any_directory_name/0, any_directory_path/0,
			   abs_directory_path/0, any_abs_directory_path/0,
			   executable_name/0, executable_path/0, bin_executable_path/0,
			   any_executable_path/0,
			   script_path/0, bin_script_path/0,
			   directory_name/0, bin_directory_name/0,
			   directory_path/0, bin_directory_path/0,
			   filename_radix/0, filepath_radix/0,
			   extension/0, dotted_extension/0, any_suffix/0,
			   path_element/0, bin_path_element/0, any_path_element/0,
			   depth/0, leaf_name/0,
			   entry_type/0, parent_creation/0, file_open_mode/0,
			   compression_format/0, file/0, file_info/0,
			   permission/0, permission_mask/0, improper_encoding_action/0 ]).


% Type shorthands:

-type count() :: basic_utils:count().
-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type format_string() :: text_utils:format_string().
-type format_values() :: format_values().

-type any_app_info() :: app_facilities:any_app_info().


-define( default_read_ahead_size, 2000 ).


% The suffix used by default to rename files to be hidden:
-define( default_hiding_suffix, <<".previous">> ).


% Regarding encodings and Unicode:
%
% - their support may be specified when opening a file, notably for writing
% (then a transparent encoding will be done), yet we found it safer and offering
% more control not to request such an automatic encoding, and to perform it by
% ourselves, either by relying on write_ustring/{2,3} or by calling write/2 with
% a binary content that is already properly encoded (see
% text_utils:to_unicode_{list,binary}/{1,2}); otherwise for example a double
% encoding could easily happen or, possibly, the encoding may fail with little
% control; so we tend now to stay away from get_default_encoding_option/0 for
% example and use the previous functions instead
%
% - more precisely, specifying an encoding like {encoding, utf8} at file opening
% was troublesome in our test cases, as we were not able to properly write
% strings like "cœur" afterwards (no matter any encoding or lack thereof was
% experimented); as mentioned, it proved useful to open such a file for writing
% without specifying any encoding, and then only to write it directly with
% pre-encoded content (a "~ts" formatter then sufficed, but binaries tend to be
% even safer bets); so the 'encoding' options, at least for writing, may not be
% that convenient
%
% - so the content itself may have to be encoded before writing; for example,
% writing "éèôù" (interpreted to be Latin1 or alike) in a file opened as utf8
% will result in a garbled content, unless it has been converted beforehand,
% typically thanks to our to_unicode_{list,binary}/{1,2}
%
% - it seemed possible that in some cases specifying the 'raw' option results in
% the requested encoding (e.g. utf8) not being respected (e.g. having ISO/IEC
% 8859 instead); with newer versions of Myriad and of Erlang, we believe this
% issue does not exist anymore
%
% - if an opened file fails to be correctly read encoding-wise (characters like
% 'à' being not only displayed but also read garbled, and if setting
% {encoding,unicode} returns an error such as {read_error, {no_translation,
% unicode,unicode}}, then this may be an (unfortunate) side-effect of having run
% the VM with the -noinput option; in this case, the best option is to execute
% once, preferably early (e.g. as first statement)
% system_utils:force_unicode_support/0.
%
% - some file elements may be improperly named regarding Unicode encoding ("raw
% filenames"); use list_dir_elements/2 to decide how they should be handled
%
% - notably in this module, calls akin to text_utils:binary_to_string/1 shall be
% carefully studied, as conversions from binaries to strings shall be avoided
% whenever possible due to their limitations; sticking to binaries everywhere
% might be a safer option

% - the way the VM is started matters; see the comment about the "-noinput"
% option, in open/{2,3}; one may use the following to check the current settings
% of the VM:
%
% trace_utils:info_fmt( "Encoding: ~p.",
%                       [ lists:keyfind(encoding, 1, io:getopts()) ] ),
%
% See also:
% [https://erlang.org/doc/apps/stdlib/unicode_usage.html#unicode-data-in-files]
% and the read_options define in csv_utils.erl for further details.
%
% Summary: use the 'file' module only for files opened for bytewise access
% ({encoding,latin1}) - otherwise use the 'io' module.


% Regarding filesystem identifiers (e.g. user_id), they can be converted from
% integers to actual names, yet apparently with nothing simpler than something
% akin to:
%
% awk -v val=USER_ID -F ":" '$3==val{print $1}' /etc/passwd


% Regarding tree traversals:
%
% In the course of a traversal, we chose not to follow the symbolic links that
% point to a directory, in order to avoid having a traversal escape a given tree
% and/or enter cycles and/or having symlinks pointing to symlinks etc.
%
% To nevertheless introduce such a feature, helpers such as find_files_from/5
% may recurse in a Directories list augmented with the symlinks that point to
% directories:
%
%	% Some symlinks may point to directories:
%	AllDirectories = lists:foldl(
%		fun( SymLnk, Acc ) ->
%			case file:read_link_all( SymLnk ) of
%
%				{ ok, SymTarget } ->
%					% TO-DO: might be in turn a symlink, which shall be
%                   % fully resolved first.
%                   %
%					case is_existing_directory( SymTarget ) of
%
%						true ->
%							[ SymTarget | Acc ];
%
%						false ->
%							Acc
%
%					end;
%
%				{ error, _ } ->
%					Acc
%
%			end
%
%		end,
%		_Acc=Directories,
%		_List=Symlinks ),




% Filename-related operations.


% Platform-specific:

% UNIX conventions:
-define( directory_separator, $/ ).

% Windows conventions:
%-define( directory_separator, $\ ).


% For proper naming in filesystems:

% Replaces each series of spaces (' '), lower than ('<'), greater than ('>'),
% comma (','), left ('(') and right (')') parentheses, single (''') and double
% ('"') quotes, forward ('/') and backward ('\') slashes, ampersand ('&'), tilde
% ('~'), sharp ('#'), at sign ('@'), all other kinds of brackets ('{', '}', '[',
% ']'), pipe ('|'), dollar ('$'), star ('*'), marks ('?' and '!'), plus ('+'),
% equal ('='), other punctuation signs (';' and ':') by exactly one underscore:
%
% (see also: net_utils:generate_valid_node_name_from/1)
%
-define( patterns_to_replace_for_paths, "( |<|>|,|\\(|\\)|'|\"|/|\\\\|\&|~|"
		 "#|@|{|}|\\[|\\]|\\||\\$|\\*|\\?|!|\\+|\\=|;|:)+" ).

-define( replacement_for_paths, "_" ).



% No cross-module inlining unfortunately (parse-transforms...):
-compile( { inline, [ get_base_path/1, get_last_path_element/1 ] } ).



-doc """
Joins the specified list of path elements.

This function has been added back to this module; `filename:join(Components)`
could be used instead (at least to some extent), however `filename:join(["",
"my_dir"])` results in `"/my_dir"`, whereas often we would want `"my_dir"`
instead - which is returned by our function; moreover, if one of the components
includes an absolute path (such as `"/xxx"` with UNIX conventions), the
preceding components, if any, were removed from the result (which does not seem
desirable); here we throw an exception instead.

So we deem our version simpler and less prone to surprise (least astonishment).

Plain and binary strings can be freely used as arguments, and a plain string is
returned in all cases.

See `split/1` for the reverse operation.
""".
-spec join( [ any_path_element() ] ) -> path().
join( ComponentList ) when is_list( ComponentList ) ->
	lists:foldr( fun join/2, _Acc0="", _List=ComponentList );

join( NonList ) ->
	throw( { cannot_join, NonList } ).



-doc """
Joins the two specified path elements, returns a corresponding plain string.

This function has been added back to this module; `filename:join(Name1, Name2)`
could be used instead (at least to some extent); however `filename:join("",
"my_dir")` results in `"/my_dir"`, whereas often we would want `"my_dir"` -
which is returned by our function ; moreover `filename:join(SomePath, AbsPath=[
?directory_separator | _])` returns AbsPath, dropping SomePath for some reason
(which does not seem desirable); here we throw an exception instead.

So we deem our version simpler and less prone to surprise (least astonishment).

Plain and binary strings can be freely used as arguments; a plain string is
returned in all cases.

See `split/1` for the reverse operation.

Prefer `bin_join/2` if having to possibly deal with so-called "raw filenames".
""".
-spec join( any_path(), any_path() ) -> path().
% Skips only initial empty paths of all sorts:
join( _FirstPath="", SecondPath ) ->
	SecondPath ;

join( _FirstPath= <<"">>, SecondPath ) ->
	SecondPath ;

% If second is string (already):
join( FirstPath, SecondPath=[ ?directory_separator | _ ] ) ->
	throw( { rightwise_absolute_directory, SecondPath,
			 { leftwise, FirstPath } } );

join( FirstPath, SecondPath ) when is_binary( FirstPath ) ->
	join( text_utils:binary_to_string( FirstPath ), SecondPath );

% Second as string to match separator above:
join( FirstPath, SecondPath ) when is_binary( SecondPath ) ->
	join( FirstPath, text_utils:binary_to_string( SecondPath ) );

% Both are strings from this point:
join( FirstPath, _SecondPath="" ) ->
	% We do not want to add a trailing separator (e.g. we want "/home/lisa", not
	% "/home/lisa/"):
	%
	FirstPath;

join( FirstPath, SecondPath ) ->

	% First as at least one element by design; avoid adding an extra separator:
	%
	% (we do not use list_utils:get_last_element/1 here as we do not want this
	% very common function of this file_utils module to depend on a list_utils
	% one)
	%
	case get_last_element( FirstPath ) of

		?directory_separator ->
			text_utils:format( "~ts~ts", [ FirstPath, SecondPath ] );

		_ ->
			text_utils:format( "~ts~c~ts",
				[ FirstPath, ?directory_separator, SecondPath ] )

	end.



-doc """
Joins the specified list of path elements, returns a corresponding binary
string.

See `join/1` for API details.

Plain and binary strings can be freely used as arguments, and a binary string is
returned in all cases.

See `split/1` for the reverse operation.
""".
-spec bin_join( [ any_path_element() ] ) -> bin_path().
bin_join( ComponentList ) when is_list( ComponentList ) ->
	lists:foldr( fun bin_join/2, _Acc0="", _List=ComponentList );

bin_join( NonList ) ->
	throw( { cannot_join, NonList } ).



-doc """
Joins the two specified path elements, returns a corresponding binary string.

Never attempts a binary-to-string conversion.

Introduced to support the case where at least one argument is an
improperly-encoded Unicode binary path: any operation implying a conversion to
string of it will fail, so the operation must take place exclusively among
binaries.
""".
-spec bin_join( any_path(), any_path() ) -> bin_path().
% Use the same semantics as join/2:
bin_join( _FirstPath="", SecondPath ) ->
	text_utils:ensure_binary( SecondPath );

bin_join( _FirstPath= <<"">>, SecondPath ) ->
	text_utils:ensure_binary( SecondPath ) ;

bin_join( FirstPath, SecondPath )
			when is_binary( FirstPath ) orelse is_binary( SecondPath ) ->
	% As soon as at least one argument of filename:join/2 is a binary, returns a
	% binary, which is what we want:
	%
	filename:join( FirstPath, SecondPath );

% Here both are expected to be plain strings, cannot be a problem:
bin_join( FirstPath, SecondPath ) ->
	filename:join( text_utils:to_unicode_binary( FirstPath ), SecondPath ).



-doc """
Joins the specified list of path elements; returns a corresponding binary string
if at least one element is a binary string itself, otherwise returns a plain
string.

Never attempts a binary-to-string conversion; introduced to promote to binary
string only when necessary.

See `join/1` for API details.

Plain and binary strings can be freely used as arguments.

See `split/1` for the reverse operation.
""".
-spec any_join( [ any_path_element() ] ) -> any_path().
any_join( ComponentList ) when is_list( ComponentList ) ->
	lists:foldr( fun any_join/2, _Acc0="", _List=ComponentList );

any_join( NonList ) ->
	throw( { cannot_join, NonList } ).



-doc """
Joins the two specified path elements; returns a corresponding binary string if
at least one element is a binary string itself, otherwise returns a plain
string.

Never attempts a binary-to-string conversion; introduced to promote to binary
string only when necessary.
""".
-spec any_join( any_path(), any_path() ) -> any_path().
% Use the same semantics as join/2:
any_join( _FirstPath="", SecondPath ) ->
	SecondPath;

any_join( _FirstPath= <<"">>, SecondPath ) ->
	text_utils:ensure_binary( SecondPath ) ;

% As soon as at least one argument of filename:join/2 is a binary, returns a
% binary, otherwise returns a plain string:
%
any_join( FirstPath, SecondPath )  ->
	filename:join( FirstPath, SecondPath ).



-doc "Splits the specified path in elements, returned as a list.".
-spec split( any_path() ) -> [ any_path_element() ].
% Defined for completeness/consistency with join counterparts:
split( Path ) ->
	filename:split( Path ).



% Duplicated verbatim from list_utils, so that file_utils can remain a mostly
% autonomous, pioneer module.
%
-doc """
Returns the last element of the specified list.

Note: not computationnally efficient, usually having to retrieve the last
element suggests a bad code design.

Crashes (with `no function clause`) if the input list is empty.
""".
%-spec get_last_element( list() ) -> element().
get_last_element( _List=[ SingleElement ] ) ->
	SingleElement;

get_last_element( _List=[ _H | T ] ) ->
	get_last_element( T ).



-doc """
Returns the complete leading, "directory" part of the specified path, that is
the one with all its elements but the last one.

For example: `"/aaa/bbb/ccc" =
		file_utils:get_base_path("/aaa/bbb/ccc/foobar.txt").`.

Note that the return type is the same of the input path, i.e. plain string or
binary string.

Alias name for `filename:dirname/1` (better in `file_utils`, and hopefully
clearer).

See `get_last_path_element/1` for the counterpart function.
""".
-spec get_base_path( any_path() ) -> any_path().
get_base_path( AnyPath ) ->
	get_base_path( AnyPath, _Depth=1 ).



-doc """
Returns the leading part of the specified path, obtained at the specified depth,
that is when the specified number of bottom-level elements have been chopped.

For example: `"/aaa/bbb" =
		file_utils:get_base_path("/aaa/bbb/ccc/foobar.txt", _Depth=2).`.

Note that the return type is the same of the input path, i.e. plain string or
binary string.
""".
-spec get_base_path( any_path(), depth() ) -> any_path().
get_base_path( AnyPath, _Depth=0 ) ->
	AnyPath;

get_base_path( AnyPath, Depth ) ->
	ShrunkPath = filename:dirname( AnyPath ),
	get_base_path( ShrunkPath, Depth-1 ).



-doc """
Returns the final, "file" part of specified path, that is its last element, as a
one-element path, corresponding either to a file or a directory.

For example: `<<"foobar.txt">> =
		file_utils:get_last_path_element(<<"/aaa/bbb/ccc/foobar.txt">>).`

Note that the return type is the same of the input path, i.e. plain string or
binary string.

Replacement name for `filename:basename/1` (more convenient if in `file_utils`,
and hopefully clearer).

See `get_base_path/1` for the counterpart function.
""".
-spec get_last_path_element( any_path() ) ->  any_path_element().
get_last_path_element( AnyPath ) ->
	filename:basename( AnyPath ).



-doc """
Splits the specified path into a full base directory path and a final entry
(filename or directory name).

For example: `{"/aaa/bbb/ccc", "foobar.txt"} =
    file_utils:split_path("/aaa/bbb/ccc/foobar.txt").`.
""".
-spec split_path( any_path() ) -> { any_path(), any_path_element() }.
split_path( AnyPath ) ->
	{ get_base_path( AnyPath ), get_last_path_element( AnyPath ) }.



-doc """
Resolves the specified resolvable path as a standard path.

For example: `resolve_path([home, "computer-info", short_hostname, "info.txt"])`
may return `"/home/john/computer-info/hurricane/info.txt"`.
""".
-spec resolve_path( resolvable_path() ) -> path().
resolve_path( ResolvablePath ) when is_list( ResolvablePath ) ->
	resolve_path( ResolvablePath, _Acc=[] ).


% (helper)
resolve_path( _ResolvablePath=[], Acc ) ->
	join( lists:reverse( Acc ) );

resolve_path( _ResolvablePath=[ user_name | T ], Acc ) ->
	NewAcc = [ system_utils:get_user_name() | Acc ],
	resolve_path( T, NewAcc );

resolve_path( _ResolvablePath=[ user_id | T ], Acc ) ->
	NewAcc = [ system_utils:get_user_id() | Acc ],
	resolve_path( T, NewAcc );

resolve_path( _ResolvablePath=[ group_name | T ], Acc ) ->
	NewAcc = [ system_utils:get_group_name() | Acc ],
	resolve_path( T, NewAcc );

resolve_path( _ResolvablePath=[ group_id | T ], Acc ) ->
	NewAcc = [ system_utils:get_group_id() | Acc ],
	resolve_path( T, NewAcc );

resolve_path( _ResolvablePath=[ home | T ], Acc ) ->
	NewAcc = [ system_utils:get_user_home_directory() | Acc ],
	resolve_path( T, NewAcc );

resolve_path( _ResolvablePath=[ locale_charset | T ], Acc ) ->
	NewAcc = [ locale_utils:get_locale_charset() | Acc ],
	resolve_path( T, NewAcc );

resolve_path( _ResolvablePath=[ fqdn | T ], Acc ) ->
	NewAcc = [ net_utils:localhost( fqdn ) | Acc ],
	resolve_path( T, NewAcc );

resolve_path( _ResolvablePath=[ short_hostname | T ], Acc ) ->
	NewAcc = [ net_utils:localhost( short ) | Acc ],
	resolve_path( T, NewAcc );

resolve_path( _ResolvablePath=[ UnexpectedToken | _T ], _Acc )
						when is_atom( UnexpectedToken ) ->
	throw( { unexpected_resolvable_token_path, UnexpectedToken } );

resolve_path( _ResolvablePath=[ S | T ], Acc )
						when is_list( S ) orelse is_binary( S ) ->
	resolve_path( T, [ S| Acc ] );
resolve_path( _ResolvablePath=[ UnexpectedTerm | _T ], _Acc ) ->
	throw( { invalid_resolvable_token_path, UnexpectedTerm } ).



-doc """
Resolves the specified path - either a standard one or a resolvable one - in all
cases as a plain, standard path.

For example: `resolve_any_path([home, "computer-info", short_hostname,
"info.txt"])` may return `"/home/john/computer-info/hurricane/info.txt"`.
""".
-spec resolve_any_path( possibly_resolvable_path() ) -> path().
resolve_any_path( BinPath ) when is_binary( BinPath ) ->
	text_utils:binary_to_string( BinPath );

resolve_any_path( AnyPath ) when is_list( AnyPath ) ->
	% Either already a plain string or a resolvable path:
	case text_utils:is_string( AnyPath ) of

		true ->
			AnyPath;

		false ->
			resolve_path( AnyPath )

	end;

resolve_any_path( Other ) ->
	throw( { invalid_path, Other } ).



-doc """
Converts the specified name into an acceptable filename (or file path),
filesystem-wise.

Returns the same type of string as the provided one.
""".
-spec convert_to_filename( any_string() ) -> any_file_name().
convert_to_filename( BinName ) when is_binary( BinName ) ->
	re:replace( BinName, ?patterns_to_replace_for_paths, ?replacement_for_paths,
				[ global, { return, binary } ] );

convert_to_filename( Name ) ->

	% Currently we use exactly the same translation rules both for node names
	% and file names (see net_utils:generate_valid_node_name_from/1).

	% Note however that now we duplicate the code instead of calling the
	% net_utils module from here, as otherwise there would be one more module to
	% deploy under some circumstances (and over time they may have to be
	% different).

	re:replace( lists:flatten( Name ), ?patterns_to_replace_for_paths,
		?replacement_for_paths, [ global, { return, list } ] ).



-doc """
Converts the specified name into an acceptable filename (or file path),
filesystem-wise.

Returns the same type of string as the provided one.
""".
-spec convert_to_filename_with_extension( any_string(), extension() ) ->
											any_file_name().
convert_to_filename_with_extension( Name, Ext ) ->
	convert_to_filename( add_extension( Name, Ext ) ).



-doc """
Escapes the specified path so that it can safely be included as a serialised
(string) content.

Returns the same type of string as the specified one.
""".
-spec escape_path( any_path() ) -> any_string().
escape_path( Path ) when is_list( Path ) ->
	% To properly flatten:
	text_utils:to_unicode_list( escape_path_helper( Path ), _CanFail=true );

escape_path( BinPath ) ->
	% Not wanting for example [<<XX>>, 92, 34, <<YY>>]:
	text_utils:to_unicode_binary( escape_path_helper( BinPath ),
								  _CanFail=true ).


-spec escape_path_helper( any_path() ) -> any_path().
escape_path_helper( Path ) ->

	Direction = all,

	% Order of replacements matters.

	% Escaping first antislashes, as some filenames *might* include some of
	% them; so:    \ -> \\
	%
	AntislashEscapes = string:replace( Path, _ASSearchPattern="\\",
									   _ASReplacement="\\\\", Direction ),

	% Then do the same for double quotes; so:    " -> \"
	string:replace( AntislashEscapes, _DQSearchPattern="\"",
					_DQReplacement="\\\"", Direction ).



-doc """
Returns the (ordered) extension(s) of the specified file path.

For example: `["baz", "json"] = get_extensions("/home/joe/foobar.baz.json").`.
""".
-spec get_extensions( file_path() ) -> [ extension() ] | 'no_extension'.
get_extensions( Filename ) ->

	case text_utils:split( Filename, _Delimiter=$. ) of

		[] ->
			no_extension;

		[ _Basename ] ->
			no_extension;

		[ _Basename | Extensions ] ->
			Extensions

	end.



-doc """
Returns the (last) extension of the specified file path.

For example: `"json" = get_extension("/home/joe/foobar.baz.json").`.
""".
-spec get_extension( file_path() ) -> extension() | 'no_extension'.
get_extension( Filename ) ->

	case get_extensions( Filename ) of

		no_extension ->
			no_extension;

		Extensions ->
			list_utils:get_last_element( Extensions )

	end.



-doc """
Adds the specified extension to the specified filename prefix.

For example: `"/mnt/foobar.txt" = add_extension("/mnt/foobar", "txt").`.

Returns a file path of the same type as the specified one.
""".
-spec add_extension( any_file_path(), extension() ) -> any_file_path().
add_extension( BinFilePath, Ext ) when is_binary( BinFilePath ) ->
	text_utils:bin_format( "~ts.~ts", [ BinFilePath, Ext ] );

add_extension( FilePathStr, Ext ) ->
	text_utils:format( "~ts.~ts", [ FilePathStr, Ext ] ).



-doc """
Removes the (last) extension (regardless of its actual value) of the specified
file path.

For example: `"/home/jack/rosie.tmp" =
   remove_extension("/home/jack/rosie.tmp.ttf")`.
""".
-spec remove_extension( file_path() ) -> file_path();
					  ( bin_file_path() ) -> bin_file_path().
remove_extension( FilePath ) ->

	case text_utils:split( FilePath, _Delimiter=$. ) of

		% Returning an empty string for an empty string:
		[] ->
			FilePath;

		[ FilePath ] ->
			FilePath;

		[ Basename | Extensions ] ->
			text_utils:join( $.,
				[ Basename | list_utils:remove_last_element( Extensions ) ] )

	end.



-doc """
Checks that the (last) extension of the specified file path is the specified
one, and returns that path once this extension has been removed.

For example: `"/home/jack/rosie" = remove_extension("/home/jack/rosie.tmp.ttf",
"ttf")`.
""".
-spec remove_extension( any_file_path(), extension() ) -> file_path().
remove_extension( BinFilePath, ExpectedExtension )
										when is_binary( BinFilePath ) ->
	remove_extension( text_utils:binary_to_string( BinFilePath ),
					  ExpectedExtension );

remove_extension( FilePath, ExpectedExtension )
										% To secure the match:
										when is_list( ExpectedExtension ) ->
	% Used more than once:
	Separator = $.,

	case text_utils:split( FilePath, Separator ) of

		[] ->
			throw( empty_path );

		[ _SingleElem ] ->
			throw( { no_extension_in_path, FilePath } );

		BasenamePlusExtensions ->
			case list_utils:extract_last_element( BasenamePlusExtensions ) of

				{ ExpectedExtension, BasenamePlusFirstExtensions } ->
					text_utils:join( Separator, BasenamePlusFirstExtensions );

				{ OtherExtension, _ } ->
					throw( { unmatching_extension, OtherExtension,
							 ExpectedExtension, FilePath } )

			end

	end.



-doc """
Returns a new file path whose extension has been updated.

For example: `replace_extension("/home/jack/rosie.ttf", "ttf", "wav")` should
return `"/home/jack/rosie.wav"`.

Use `remove_extension/2` to remove extension, as replacing an extension by an
empty one would leave the leading dot.
""".
-spec replace_extension( file_path(), extension(), extension() ) -> file_path().
replace_extension( FilePath, SourceExtension, TargetExtension ) ->

	case string:rstr( FilePath, SourceExtension ) of

		0 ->
			throw( { extension_not_found, SourceExtension, FilePath } );

		Index ->
			string:substr( FilePath, _From=1, Index-1 ) ++ TargetExtension

	end.



-doc """
Tells whether the specified filesystem entry exists, regardless of its actual
type.
""".
-spec exists( any_path() ) -> boolean().
exists( EntryName ) ->

	% Dead symbolic links are deemed existing:
	case file:read_link_info( EntryName ) of
	%case file:read_file_info( EntryName ) of

		{ ok, _FileInfo } ->
			true;

		{ error, _Reason=eacces } ->
			throw( { exists_failed, text_utils:ensure_string( EntryName ),
                     access_denied,
                     get_element_access_denied_info( EntryName ) } );

		{ error, _Reason } ->
			false

	end.



-doc """
Returns the (direct) type of the specified file entry (hence may return
`symlink` if the path of a symbolic link is specified).

See `resolve_type_of/1` to go through symbolic links, and return the actual,
ultimate entry type resolved.
""".
-spec get_type_of( any_path() ) -> entry_type().
get_type_of( Path ) ->

	% We used to rely on file:read_file_info/1, but an existing symlink pointing
	% to a non-existing entry was triggering the enoent error, while we just
	% wanted to know that the specified entry is an existing (yet dead) symlink.

	% Some tools (e.g. emacs) used thus to get in the way, as apparently they
	% create dead symlinks on purpose, to store information.

	case file:read_link_info( Path ) of

		{ ok, #file_info{ type=FileType } } ->
			FileType;

		{ error, _Reason=eacces } ->
			throw( { get_type_of_failed, text_utils:ensure_string( Path ),
                     access_denied,
                     get_element_access_denied_info( Path ) } );

		{ error, eloop } ->
			% Probably a recursive symlink:
			throw( { too_many_symlink_levels, Path } );

		{ error, enoent } ->
			throw( { non_existing_entry, Path } )

	end.



-doc """
Returns the actual, ultimate type of the specified file entry (hence may not
return `symlink`).

Refer to `get_type_of/1` to return the type into which the specified entry
resolves first (thus possibly resolving in a symbolic link).
""".
-spec resolve_type_of( any_path() ) -> entry_type().
resolve_type_of( Path ) ->

	case file:read_file_info( Path ) of

		{ ok, #file_info{ type=FileType } } ->
			FileType;

		{ error, eloop } ->
			% Probably a recursive symlink:
			throw( { too_many_symlink_levels, Path } );

		{ error, enoent } ->
			throw( { non_existing_entry, Path } );

		{ error, eacces } ->
			throw( { resolve_type_of_failed,
                     text_utils:ensure_string( Path ), access_denied,
                     get_element_access_denied_info( Path ) } )

	end.



-doc """
Resolves the specified symbolic link once: returns the entry (potentially
another symbolic link) it points to.
""".
-spec resolve_symlink_once( any_path() ) -> any_path().
resolve_symlink_once( SymlinkPath ) ->

	case file:read_link_all( SymlinkPath ) of

		{ ok, TargetPath } ->
			TargetPath;

		{ error, eacces } ->
			throw( { resolve_type_of_failed,
                     text_utils:ensure_string( SymlinkPath ), access_denied,
                     get_element_access_denied_info( SymlinkPath ) } );

		{ error, Reason } ->
			throw( { symlink_resolution_failed, Reason, SymlinkPath } )

	end.



-doc """
Resolves fully the specified symbolic link: returns the entry it points
ultimately to (therefore this entry cannot be a symbolic link), or throws an
exception (including if exceeding a larger link depth, which happens most
probably because these links form a cycle; throwing arbitrarily an exception is
better than looping for ever).
""".
-spec resolve_symlink_fully( any_path() ) -> any_path().
resolve_symlink_fully( SymlinkPath ) ->
	resolve_symlink_fully( SymlinkPath, SymlinkPath, _MaxDepth=50 ).


% (helper)
resolve_symlink_fully( _SymlinkPath, OrigSymlinkPath, _Depth=0 ) ->

	trace_utils:error_fmt( "Maximum symlink depth reached for '~ts'; "
		"most probably these links form a cycle.", [ OrigSymlinkPath ] ),

	throw( { max_symlink_depth_reached_for, OrigSymlinkPath } );


resolve_symlink_fully( SymlinkPath, OrigSymlinkPath, Depth ) ->

	case file:read_link_all( SymlinkPath ) of

		{ ok, TargetPath } ->
			case is_link( TargetPath ) of

				true ->
					resolve_symlink_fully( TargetPath, OrigSymlinkPath,
										   Depth-1 );

				false ->
					TargetPath

			end;

		{ error, eacces } ->
			throw( { resolve_symlink_fully_failed,
                     text_utils:ensure_string( SymlinkPath ), access_denied,
                     get_element_access_denied_info( SymlinkPath ) } );

		{ error, Reason } ->
			throw( { symlink_resolution_failed, Reason, OrigSymlinkPath } )

	end.



-doc """
Returns the user identifier (uid) of the owner of the specified file entry.
""".
-spec get_owner_of( any_path() ) -> system_utils:user_id().
get_owner_of( Path ) ->

	case file:read_file_info( Path ) of

		{ ok, #file_info{ uid=UID } } ->
			UID;

		{ error, _Reason=eacces } ->
			throw( { owner_inquiry_failed, text_utils:ensure_string( Path ),
                     access_denied, get_element_access_denied_info( Path ) } );

		{ error, Reason } ->
			throw( { owner_inquiry_failed, text_utils:ensure_string( Path ),
                     Reason } )

	end.



-doc """
Returns any description of the owner of the specified file entry.

Never fails.
""".
-spec describe_owner_of( any_path() ) -> ustring().
describe_owner_of( Path ) ->

	case file:read_file_info( Path ) of

		{ ok, #file_info{ uid=UID } } ->
			text_utils:format( "user of UID ~B", [ UID ] );

		{ error, _Reason=eacces } ->

			%throw( { describe_owner_of_failed,
            %   text_utils:ensure_string( Path ),
            %   access_denied, get_element_access_denied_info( Path ) } );

			"unknown user (insufficient permissions)";

		{ error, Reason } ->
			text_utils:format( "unknown user (reason: ~p)", [ Reason ] )

	end.



-doc """
Returns the group identifier (gid) of the group of the specified file entry.
""".
-spec get_group_of( any_path() ) -> system_utils:group_id().
get_group_of( Path ) ->

	case file:read_file_info( Path ) of

		{ ok, #file_info{ gid=GID } } ->
			GID;

		{ error, _Reason=eacces } ->
			throw( { get_group_of_failed, text_utils:ensure_string( Path ),
                     access_denied, get_element_access_denied_info( Path ) } );

		{ error, Reason } ->
			throw( { get_group_of_failed, Reason, Path } )

	end.



-doc """
Returns any description of the group of the specified file entry.

Never fails.
""".
-spec describe_group_of( any_path() ) -> ustring().
describe_group_of( Path ) ->

	case file:read_file_info( Path ) of

		{ ok, #file_info{ gid=GID } } ->
			text_utils:format( "group of GID ~B", [ GID ] );

		{ error, _Reason=eacces } ->

			%throw( { describe_group_of_failed,
            %   text_utils:ensure_string( Path ),
            %   access_denied, get_element_access_denied_info( Path ) } );

			"unknown group (insufficient permissions)";

		{ error, Reason } ->
			text_utils:format( "unknown group (reason: ~p)", [ Reason ] )

	end.



-doc """
Returns whether the specified path entry, supposedly existing, is a regular
file.

If the specified entry happens not to exist, a `{non_existing_entry, EntryName}`
exception will be thrown.

Not to be confused with `is_file_reference/1`, which deals with opened file IO
devices.
""".
-spec is_file( any_path() ) -> boolean().
is_file( Path ) ->
	get_type_of( Path ) =:= regular.



-doc """
Returns whether the specified path entry exists and is a regular file.

Returns `true` or `false`, and cannot trigger an exception.
""".
-spec is_existing_file( any_path() ) -> boolean().
is_existing_file( Path ) ->
	exists( Path ) andalso get_type_of( Path ) =:= regular.



-doc """
Returns whether the specified path entry, supposedly existing, is a symbolic
file.

Returns `true` or `false`, and cannot trigger an exception.
""".
-spec is_link( any_path() ) -> boolean().
is_link( Path ) ->
	get_type_of( Path ) =:= symlink.



-doc """
Returns whether the specified path entry exists and is a symbolic file.

Returns `true` or `false`, and cannot trigger an exception.
""".
-spec is_existing_link( any_path() ) -> boolean().
is_existing_link( Path ) ->
	exists( Path ) andalso get_type_of( Path ) =:= symlink.



-doc """
Returns whether the specified path entry exists and is either a regular file or
a symbolic link.

Returns `true` or `false`, and cannot trigger an exception.
""".
-spec is_existing_file_or_link( any_path() ) -> boolean().
is_existing_file_or_link( Path ) ->

	case exists( Path ) andalso get_type_of( Path ) of

		regular ->
			true ;

		symlink ->
			true ;

		_ ->
			false

	end.



-doc """
Returns whether the specified path entry (can be either a regular file or a
symbolic link) exists and is readable for its current owner - not telling
anything about whether the current user can read it.

Returns `true` or `false`, and cannot trigger an exception.

See also: `is_user_readable/1`.
""".
-spec is_owner_readable( any_path() ) -> boolean().
is_owner_readable( Path ) ->

	case file:read_file_info( Path ) of

		{ ok, FileInfo } ->

			#file_info{ type=FileType, mode=Mode } = FileInfo,

			case FileType of

				regular ->

					OwnerReadMask = to_permission_mask( owner_read ),
					case Mode band OwnerReadMask of

						0 ->
							% Not readable:
							false;

						_ ->
							% One positive case:
							true

					end;

				_ ->
					false

			end;

		_ ->
			false

	end.



-doc """
Returns whether the specified path entry (can be either a regular file or a
symbolic link) exists and is writable for its current owner) - not telling
anything about whether the current user can write it.

Returns `true` or `false`, and cannot trigger an exception.

See also: `is_user_writable/1`.
""".
-spec is_owner_writable( any_path() ) -> boolean().
is_owner_writable( Path ) ->

	case file:read_file_info( Path ) of

		{ ok, FileInfo } ->

			#file_info{ type=FileType, mode=Mode } = FileInfo,

			case FileType of

				regular ->

					OwnerWriteMask = to_permission_mask( owner_write ),
					case Mode band OwnerWriteMask of

						0 ->
							% Not writable:
							false;

						_ ->
							% One positive case:
							true

					end;

				_ ->
					false

			end;

		_ ->
			false

	end.



-doc """
Returns whether the specified path entry (can be either a regular file or a
symbolic link) exists and is executable for its current owner - not telling
anything about whether the current user can execute it.

Returns `true` or `false`, and cannot trigger an exception.

See also: `is_owner_writable/1`.
""".
-spec is_owner_executable( any_path() ) -> boolean().
is_owner_executable( Path ) ->

	case file:read_file_info( Path ) of

		{ ok, FileInfo } ->

			#file_info{ type=FileType, mode=Mode } = FileInfo,

			case FileType of

				regular ->

					OwnerExecMask = to_permission_mask( owner_execute ),
					case Mode band OwnerExecMask of

						0 ->
							% Not executable:
							false;

						_ ->
							% One positive case:
							true

					end;

				_ ->
					false

			end;

		_ ->
			false

	end.



-doc """
Returns whether the specified path entry (can be either a regular file or a
symbolic link) exists and is readable for the current user.

Returns `true` or `false`, and cannot trigger an exception.
""".
-spec is_user_readable( any_path() ) -> boolean().
is_user_readable( Path ) ->

	% Rather than using file:read_file_info/1 and having to try to fetch and
	% filter user/group information, it is easier, maybe more efficient and
	% most probably more reliable to try to open it for reading:

	case file:open( _File=Path, _Mode=[ read ] ) of

		{ ok, File } ->
			file:close( File ),
			true;

		{ error, _Reason } ->
			false

	end.



-doc """
Returns whether the specified path entry (can be either a regular file or a
symbolic link) exists and is writable for the current user.

Returns `true` or `false`, and cannot trigger an exception.
""".
-spec is_user_writable( any_path() ) -> boolean().
is_user_writable( Path ) ->

	% Rather than using file:read_file_info/1 and having to try to fetch and
	% filter user/group information, it is easier, maybe more efficient and
	% reliable to try to open it for writing:

	case file:open( _File=Path, _Mode=[ write ] ) of

		{ ok, File } ->
			file:close( File ),
			true;

		{ error, _Reason } ->
			false

	end.



-doc """
Returns whether the specified path entry exists and is executable for the
current user (can be either a regular file or a symbolic link).

Returns `true` or `false`, and cannot trigger an exception.

WARNING: not properly implemented yet.

See also: `is_owner_executable/1`.
""".
-spec is_user_executable( any_path() ) -> boolean().
is_user_executable( Path ) ->
	is_owner_executable( Path ).



-doc """
Returns whether the specified path entry, supposedly existing, is a directory.

If the specified entry happens not to exist, a `{non_existing_entry, Path}`
exception will be thrown.
""".
-spec is_directory( any_path() ) -> boolean().
is_directory( Path ) ->
	get_type_of( Path ) =:= directory.



-doc """
Returns whether the specified path entry exists and is a directory.

Returns `true` or `false`, and cannot trigger an exception.
""".
-spec is_existing_directory( any_path() ) -> boolean().
is_existing_directory( Path ) ->
	exists( Path ) andalso get_type_of( Path ) =:= directory.



-doc """
Returns whether the specified path entry exists and is a directory or a symbolic
link.

Returns `true` or `false`, and cannot trigger an exception.
""".
-spec is_existing_directory_or_link( any_path() ) -> boolean().
is_existing_directory_or_link( Path ) ->

	case exists( Path ) andalso get_type_of( Path ) of

		directory ->
			true ;

		symlink ->
			true ;

		_ ->
			false

	end.



-doc """
Returns a tuple containing five lists corresponding to the per-type dispatching
of all filesystem elements local to specified directory (hence not recursively
traversed), namely: `{RegularFiles, Symlinks, Directories, OtherFiles,
Devices}`.

If a raw filename is found (i.e. a file element whose name is not properly
Unicode-encoded), a warning trace is emitted and the corresponding file element.

Note that:
- symbolic links may or may not be dead
- only plain strings are returned (any raw filename found will trigger a warning
trace and then will be ignored)
""".
-spec list_dir_elements( directory_name() ) ->
			{ [ file_name() ], [ file_name() ], [ directory_name() ],
			  [ file_name() ], [ file_name() ] }.
list_dir_elements( DirName ) ->
	list_dir_elements( DirName, _ImproperEncodingAction=warn ).



-doc """
Returns a tuple containing five lists corresponding to the per-type dispatching
of all filesystem elements local to specified directory (hence not recursively
traversed), namely: `{RegularFiles, Symlinks, Directories, OtherFiles,
Devices}`.

If a raw filename is found (i.e. a file element whose name is not properly
Unicode-encoded), ImproperEncodingAction will determine how it will be handled.

Note that:
- symbolic links may or may not be dead
- generally the returned elements are strings, yet, if ImproperEncodingAction is
`include`, binaries may also returned, should raw ("incorrectly-encoded")
filenames be found (they are then returned verbatim, short of being able to
stringify them)
""".
-spec list_dir_elements( directory_name(), improper_encoding_action() ) ->
		{ [ any_file_name() ], [ any_file_name() ], [ any_directory_name() ],
		  [ any_file_name() ], [ any_file_name() ] }.
list_dir_elements( DirName, ImproperEncodingAction ) ->

	%trace_utils:debug_fmt( "list_dir_elements for '~ts'.", [ DirName ] ),

	% Previously file:list_dir_all/1 was tested in order to collect raw
	% filenames as well (hoping to avoid warning reports such as "Non-unicode
	% filename <<"XXX">> ignored"), yet the returned names were mangled, leading
	% to enoent whenever trying to open them :

	% Sometimes filenames are "wrongly encoded" (e.g. 'foo'$'\200\246''bar'); if
	% using file:list_dir/1 instead of file:list_dir_all/1, warnings are
	% then issued (only on the console) like:
	%
	% =WARNING REPORT==== 25-Mar-2021::22:41:11.577989 ===
	% Non-unicode filename <<XXX>> ignored
	%
	% Now such elements are listed as binaries (refer to the implementation
	% notes at top, for further detail)

	LocalDirElements = case file:list_dir_all( DirName ) of

		{ ok, LElems } ->
			LElems;

		{ error, enoent } ->
			throw( { directory_not_found, DirName } );

		{ error, Other } ->
			throw( { Other, DirName } )

	end,

	%trace_utils:debug_fmt( "LocalDirElements: ~p", [ LocalDirElements ] ),

	classify_dir_elements( DirName, LocalDirElements, _Devices=[],
		_Directories=[], _Files=[], _Symlinks=[], _OtherFiles=[],
		ImproperEncodingAction ).



-doc """
Checks that the specified path entry exists and is a regular file, or throws an
exception if not.
""".
-spec check_existing_file( any_path() ) -> void().
check_existing_file( Path ) ->
	is_existing_file( Path ) orelse throw( { non_existing_file, Path } ).



-doc """
Checks that the specified path entry exists and is either a regular file or a
symbolic link, or throws an exception if not.
""".
-spec check_existing_file_or_link( any_path() ) -> void().
check_existing_file_or_link( Path ) ->
	is_existing_file_or_link( Path ) orelse
        throw( { non_existing_file_or_link, Path } ).



-doc """
Checks that the specified path entry exists and is a directory, or throws an
exception if not.
""".
-spec check_existing_directory( any_path() ) -> void().
check_existing_directory( Path ) ->
	is_existing_directory( Path ) orelse
		throw( { non_existing_directory, Path } ).



-doc "Returns the size, in bytes, of the specified file.".
-spec get_size( any_file_path() ) -> system_utils:byte_size().
get_size( FilePath ) ->

	case file:read_file_info( FilePath ) of

		{ ok, #file_info{ size=Size } } ->
			Size;

		{ error, _Reason=eacces } ->
			throw( { size_inquiry_failed, text_utils:ensure_string( FilePath ),
                     access_denied,
                     get_file_access_denied_info( FilePath ) } );

		{ error, Reason } ->
			throw( { size_inquiry_failed, Reason, FilePath } )

	end.



-doc """
Returns the last time at which the content of specified file entry was modified
(not counting attribute or permission changes), according to the filesystem.

Said timestamp will be expressed as an integer number of seconds since (or
before) Unix time epoch, which is `1970-01-01 00:00 UTC`.
""".
-spec get_last_modification_time( any_path() ) -> time_utils:posix_seconds().
get_last_modification_time( Path ) ->

	case file:read_file_info( Path, [ { time, posix } ] ) of

		{ ok, #file_info{ mtime=Seconds } } ->
			Seconds;

		{ error, _Reason=eacces } ->
			throw( { file_info_failure, text_utils:ensure_string( Path ),
                     access_denied,
                     get_element_access_denied_info( Path ) } );

		{ error, Reason } ->
			throw( { file_info_failure, Reason, Path } )

	end.



-doc """
Updates the modification time (the last time at which its content was reported
as modified according to the filesystem) of the specified file entry, which must
already exist.

Note: leaves last access time unchanged, updates both modification and change
times.

See also: `create_empty_file/1`.
""".
-spec touch( any_path() ) -> void().
touch( Path ) ->

	case exists( Path ) of

		true ->
			% -c: do not create any file
			% -m: change only the modification time
			%
			case system_utils:run_executable( _ExecPath="/bin/touch",
					_Args=[ "-c", "-m", Path ] ) of

				{ 0, _Output } ->
					ok;

				{ ErrorCode, Output } ->
					throw( { touch_failed, Output, ErrorCode, Path } )

			end;

		false ->
			throw( { non_existing_file_element_to_touch, Path } )

	end.



-doc """
Creates an empty file bearing the specified filename (other use of touch).

Potentially useful as a last-resort debugging tool (when no console output or
applicative trace can be relied upon, we can at least leave side-effects on the
filesystem).

Note: of course a simple `os:cmd("/bin/touch ~/my-message.debug").` may be of
use as well.

See also: `touch/1`.
""".
-spec create_empty_file( any_file_path() ) -> void().
create_empty_file( AnyFilePath ) ->

	FilePathStr = text_utils:ensure_string( AnyFilePath ),

	case system_utils:run_command( "/bin/touch '" ++ FilePathStr ++ "'" ) of

		{ 0, _Output } ->
			ok;

		{ ErrorCode, Output } ->
			throw( { empty_file_creation_failed, Output, ErrorCode,
					 FilePathStr } )

	end.



-doc """
Creates on the filesystem a file whose path is guaranteed not to clash with any
other.

Typically useful to create a temporary file.

An empty file is created for that name, whose path is returned.

May for example return `"/tmp/tmp.QgHRjzI2TZ"`.
""".
-spec create_non_clashing_file() -> file_path().
create_non_clashing_file() ->
	% Typically in /bin/mktemp:
	MkTempExecPath = executable_utils:find_executable( "mktemp" ),

	case system_utils:run_executable( MkTempExecPath ) of

		{ _ReturnCode=0, CmdOutput } ->
			CmdOutput;

		{ ErrorCode, Output } ->
			throw( { non_clashing_file_creation_failed, Output, ErrorCode } )

	end.



-doc """
Returns the current directory, as a plain string.

Throws an exception on failure.
""".
-spec get_current_directory() -> directory_path().
get_current_directory() ->

	case file:get_cwd() of

		{ ok, Dir } ->
			Dir;

		{ error, Reason } ->
			throw( { failed_to_determine_current_directory, Reason } )

	end.



-doc """
Returns the current directory, as a binary string.

Throws an exception on failure.
""".
-spec get_bin_current_directory() -> bin_directory_path().
get_bin_current_directory() ->
	text_utils:string_to_binary( get_current_directory() ).



-doc """
Sets the specified directory as current directory.

Throws an exception on failure.
""".
-spec set_current_directory( directory_path() ) -> void().
set_current_directory( DirPath ) ->

	case file:set_cwd( DirPath ) of

		ok ->
			ok;

		% For more detail about {'error', atom()}, refer to type specifications
		% of erlang files: file.erl and file.hrl.
		%
		{ error, Error } ->
			throw( { set_current_directory_failed, DirPath, Error } )

	end.



-doc """
Returns the first (if any) existing directory found in the specified list, or
throws an exception if none is found.

Each of the directory components involved may be an actual directory or a
symbolic link.

Typically useful when having multiple possible paths depending on settings, only
one of them being relevant.
""".
-spec get_first_existing_directory_in( [ any_directory_path() ] ) ->
												any_directory_path().
get_first_existing_directory_in( DirPaths ) ->
	get_first_existing_dir( DirPaths, _Acc=[] ).


% (helper)
get_first_existing_dir( _DirPaths=[], Acc ) ->
	throw( { no_existing_directory_found_in, lists:reverse( Acc ),
			 get_current_directory() } );

get_first_existing_dir( _DirPaths=[ Dir | T ], Acc ) ->

	case is_existing_directory_or_link( Dir ) of

		true ->
			Dir;

		_False ->
			get_first_existing_dir( T, [ Dir | Acc ] )

	end.



-doc """
Returns, as a path (made of a directory and the filename), the first occurrence
(if any) of the specified filename found (as a regular file or a symbolic link)
through the specified (ordered) list of directories.

Note that any returned path is not necessarily absolute, as the specified
directories may be relative.

For example: `get_first_file_or_link_for("foobar.etf", ["/home/prefs",
"/var/config"])` may return `"/var/config/foobar.etf"`, if this file exists and
`"/home/prefs/foobar.etf"` does not.
""".
-spec get_first_file_or_link_for( any_file_name(), [ any_directory_path() ] ) ->
											option( any_file_path() ).
get_first_file_or_link_for( _TargetFilename, _CandidateDirs=[] ) ->
	undefined;

get_first_file_or_link_for( TargetFilename, _CandidateDirs=[ Dir | T ] ) ->
	TargetFilePath = join( Dir, TargetFilename ),
	case is_existing_file_or_link( TargetFilePath ) of

		true ->
			TargetFilePath;

		_False ->
			get_first_file_or_link_for( TargetFilename, T )

	end.



% (helper)
%
% Returns a tuple containing five lists corresponding to the sorting of all
% filesystem elements, namely {RegularFiles, Symlinks, Directories, OtherFiles,
% Devices}, depending on ImproperEncodingAction.
%
classify_dir_elements( _DirName, _Elements=[], Devices, Directories, Files,
					   Symlinks, OtherFiles, _ImproperEncodingAction ) ->
	% Note the reordering:
	{ Files, Symlinks, Directories, OtherFiles, Devices };

classify_dir_elements( DirName, _Elements=[ Str | T ], Devices, Directories,
					   Files, Symlinks, OtherFiles, ImproperEncodingAction )
							when is_list( Str ) ->

	case get_type_of( filename:join( DirName, Str ) ) of

		device ->
			classify_dir_elements( DirName, T, [ Str | Devices ], Directories,
				Files, Symlinks, OtherFiles, ImproperEncodingAction );

		directory ->
			classify_dir_elements( DirName, T, Devices, [ Str | Directories ],
				Files, Symlinks, OtherFiles, ImproperEncodingAction );

		regular ->
			classify_dir_elements( DirName, T, Devices, Directories,
				[ Str | Files ], Symlinks, OtherFiles, ImproperEncodingAction );

		% Used to be managed as regular files:
		symlink ->
			classify_dir_elements( DirName, T, Devices, Directories, Files,
				[ Str | Symlinks ], OtherFiles, ImproperEncodingAction );

		other ->
			classify_dir_elements( DirName, T, Devices, Directories,
				Files, Symlinks, [ Str | OtherFiles ], ImproperEncodingAction )

	end;

% Implicit: when is_binary( Bin ) ->
classify_dir_elements( DirName, _Elements=[ Bin | _T ], _Devices, _Directories,
		_Files, _Symlinks, _OtherFiles, _ImproperEncodingAction=throw ) ->
	throw( { improperly_encoded_file_element, Bin, DirName } );


classify_dir_elements( DirName, _Elements=[ Bin | T ], Devices, Directories,
		Files, Symlinks, OtherFiles, ImproperEncodingAction=warn ) ->

	trace_bridge:warning_fmt( "Ignoring improperly-encoded "
		"file element found in directory '~ts': '~ts'.", [ DirName, Bin ] ),

	% Then ignore:
	classify_dir_elements( DirName, T, Devices, Directories, Files, Symlinks,
						   OtherFiles, ImproperEncodingAction );


classify_dir_elements( DirName, _Elements=[ _Bin | T ], Devices, Directories,
		Files, Symlinks, OtherFiles, ImproperEncodingAction=ignore ) ->
	classify_dir_elements( DirName, T, Devices, Directories, Files, Symlinks,
						   OtherFiles, ImproperEncodingAction );


classify_dir_elements( DirName, _Elements=[ Bin | T ], Devices, Directories,
		Files, Symlinks, OtherFiles, ImproperEncodingAction=include ) ->
	% We cannot re-use the clause for plain string (Bin cannot be converted), we
	% have to remain in the world of binaries instead, so:
	%
	case get_type_of( filename:join( DirName, Bin ) ) of

		device ->
			classify_dir_elements( DirName, T, [ Bin | Devices ], Directories,
				Files, Symlinks, OtherFiles, ImproperEncodingAction );

		directory ->
			classify_dir_elements( DirName, T, Devices, [ Bin | Directories ],
				Files, Symlinks, OtherFiles, ImproperEncodingAction );

		regular ->
			classify_dir_elements( DirName, T, Devices, Directories,
				[ Bin | Files ], Symlinks, OtherFiles, ImproperEncodingAction );

		% Used to be managed as regular files:
		symlink ->
			classify_dir_elements( DirName, T, Devices, Directories, Files,
				[ Bin | Symlinks ], OtherFiles, ImproperEncodingAction );

		other ->
			classify_dir_elements( DirName, T, Devices, Directories,
				Files, Symlinks, [ Bin | OtherFiles ], ImproperEncodingAction )

	end.



% Regarding extensions: we could canonicalise their case, so that ".png" and
% ".PNG" are treated the same.


-doc """
Returns a list containing all elements of the specified file paths whose
(dotted) extension is the specified one (e.g. `".dat"`).
""".
-spec filter_by_extension( [ file_path() ], dotted_extension() ) ->
												[ file_path() ].
filter_by_extension( Filenames, Extension ) ->
	filter_by_extension( Filenames, Extension, _Acc=[] ).


filter_by_extension( _Filenames=[], _Extension, Acc ) ->
	Acc ;

filter_by_extension( _Filenames=[ H | T ], Extension, Acc ) ->

	case filename:extension( H ) of

		Extension ->
			filter_by_extension( T, Extension, [ H | Acc ] ) ;

		_Other ->
			filter_by_extension( T, Extension, Acc )

	end.



-doc """
Returns a list containing all elements of the specified file paths whose
(dotted) extension is among the specified ones (e.g. `[".dat", ".png"]`).
""".
-spec filter_by_extensions( [ file_path() ], [ extension() ] ) ->
												[ file_path() ].
filter_by_extensions( Filenames, Extensions ) ->
	filter_by_extensions( Filenames, Extensions, _Acc=[] ).


filter_by_extensions( _Filenames=[], _Extensions, Acc ) ->
	Acc ;

filter_by_extensions( _Filenames=[ F | T ], Extensions, Acc ) ->

	case lists:member( filename:extension( F ), Extensions ) of

		true ->
			filter_by_extensions( T, Extensions, [ F | Acc ] ) ;

		false ->
			filter_by_extensions( T, Extensions, Acc )

	end.



-doc """
Returns a list containing all paths in the specified list (in an unspecified
order) that match any of the specified suffixes.
""".
-spec filter_by_included_suffixes( [ any_path() ], [ any_suffix() ] ) ->
											[ any_path() ].
filter_by_included_suffixes( Paths, IncludedSuffixes ) ->
	% Not a list comprehension to better detect any non-matching element:
	Res = filter_by_included_suffixes( Paths, IncludedSuffixes, _Acc=[] ),
	%trace_utils:debug_fmt( "Filtering by included suffixes ~p~n  * input: ~p"
	%   "~n * output: ~p", [ IncludedSuffixes, Paths, Res ] ),
	Res.


% (helper)
filter_by_included_suffixes( _Paths=[], _IncludedSuffixes, Acc ) ->
	% Order does not matter:
	Acc;

filter_by_included_suffixes( _Paths=[ P | T ], IncludedSuffixes, Acc ) ->

	NewAcc = case has_matching_suffix( P, IncludedSuffixes ) of

		true ->
			[ P | Acc ];

		false ->
			Acc

	end,

	filter_by_included_suffixes( T, IncludedSuffixes, NewAcc ).



-doc """
Returns a list containing all paths in the specified list (in an unspecified
order) that do not match any of the specified suffixes.
""".
-spec filter_by_excluded_suffixes( [ any_path() ], [ any_suffix() ] ) ->
											[ any_path() ].
% Below there is at least one excluded suffix:
filter_by_excluded_suffixes( Paths, ExcludedSuffixes ) ->
	% Not a list comprehension to better detect any non-matching element:
	filter_by_excluded_suffixes( Paths, ExcludedSuffixes, _Acc=[] ).


% (helper)
filter_by_excluded_suffixes( _Paths=[], _ExcludedSuffixes, Acc ) ->
	% Order does not matter:
	Acc;

filter_by_excluded_suffixes( _Paths=[ P | T ], ExcludedSuffixes, Acc ) ->

	NewAcc = case has_matching_suffix( P, ExcludedSuffixes ) of

		true ->
			Acc;

		false ->
			[ P | Acc ]

	end,

	filter_by_excluded_suffixes( T, ExcludedSuffixes, NewAcc ).



-doc "Tells whether specified path matches one of the specified suffixes.".
-spec has_matching_suffix( any_path(), [ any_suffix() ] ) -> boolean().
has_matching_suffix( _Path, _Suffixes=[] ) ->
	false;

has_matching_suffix( Path, [ Suffix | T ] ) ->

	% Deadly bugs may happen if plain and binary strings are mixed:
	%cond_utils:assert( myriad_check_strings,
	%                   text_utils:are_of_same_string_type( Path, Suffix ) ),

	% Path and Suffix must be of the same type of strings (either plain or
	% binary). If not, as a conversion from binary to plain may fail (raw
	% filenames), we promote the plain to binary instead:
	%
	{ ActualPath, ActualSuffix } = case text_utils:is_string( Path ) of

		true ->
			case text_utils:is_string( Suffix ) of

				true ->
					{ Path, Suffix };

				false ->
					{ text_utils:string_to_binary( Path ), Suffix }

			end;

		false ->
			case text_utils:is_string( Suffix ) of

				true ->
					{ Path, text_utils:string_to_binary( Suffix ) };

				false ->
					{ Path , Suffix }

			end

	end,

	% To work both for plain and binary strings:
	LenPath = string:length( ActualPath ),
	LenSuffix = string:length( ActualSuffix ),

	case LenPath - LenSuffix  of

		StartPos when StartPos >= 0 ->

			case string:slice( ActualPath, StartPos ) of

				% Thus Suffix must be of the same type of string as Path:
				ActualSuffix ->
					true;

				_ ->
					% Not specifically ActualPath:
					has_matching_suffix( Path, T )

			end;

		_ ->
			% Not specifically ActualPath:
			has_matching_suffix( Path, T )

	end.




% Section dedicated to the look-up of files, with various variations (with or
% without extensions, with or without excluded directories, etc.)
%
% During a tree traversal, no symbolic link is ever followed. See the 'Regarding
% tree traversals' section at the top of this file.
%
% Excluded directories are all promoted to binary strings at first, so that no
% upcoming lists:member(D, ExcludedDirs) can fail if ever D happens to be a
% binary (because of a 'raw directory').
%
% See also filelib:wildcard/1.


-doc """
Returns a list of all files (regular ones and symlinks) found from the root, in
the whole subtree (that is recursively).

All extensions and suffixes accepted, no excluded directories. Elements whose
name is improperly encoded are notified thanks to a warning trace, and then are
ignored.

All returned pathnames are relative to this root. For example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_from( any_directory_path() ) -> [ file_path() ].
find_files_from( RootDir ) ->
	Res = find_files_from( RootDir, _IncludeSymlinks=true ),
	%trace_utils:debug_fmt( "All files found from '~ts':~n~p",
	%                       [ RootDir, Res ] ),
	Res.



-doc """
Returns a list of all regular files (hence not including symlinks) found from
the root, in the whole subtree (that is recursively).

All extensions and suffixes accepted, no excluded directories. Elements whose
name is improperly encoded are notified thanks to a warning trace, and then are
ignored.

All returned pathnames are relative to this root. For example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_regular_files_from( any_directory_path() ) -> [ file_path() ].
find_regular_files_from( RootDir ) ->
	find_files_from( RootDir, _IncludeSymlinks=false ).



-doc """
Returns a list of all files (regular ones and, if requested, symlinks) found
from the root, in the whole subtree (that is recursively).

All extensions and suffixes accepted, no excluded directories. Elements whose
name is improperly encoded are notified thanks to a warning trace, and then are
ignored.

All returned pathnames are relative to this root. For example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_from( any_directory_path(), boolean() ) -> [ file_path() ].
find_files_from( RootDir, IncludeSymlinks ) ->
	find_files_from( RootDir, IncludeSymlinks, _IfImproperEncoding=warn ).



-doc """
Returns a list of all files (regular ones and, if requested, symlinks) found
from the root, in the whole subtree (that is recursively).

All extensions and suffixes accepted, no excluded directories. Elements whose
name is improperly encoded are managed according to the IfImproperEncoding
parameter; if set to `include`, the return type of this function is the more
general `[any_file_path()]`, otherwise it is `[file_path()]`.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_from( any_directory_path(), boolean(),
					   improper_encoding_action() ) -> [ any_file_path() ].
find_files_from( RootDir, IncludeSymlinks, IfImproperEncoding ) ->
	Res = find_files_from( RootDir, _CurrentRelativeDir="", IncludeSymlinks,
						   IfImproperEncoding, _Acc=[] ),
	%trace_utils:debug_fmt( "Files found from '~ts':~n~p", [ RootDir, Res ] ),
	Res.



% (helper)
find_files_from( RootDir, CurrentRelativeDir, IncludeSymlinks,
				 IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "find_files_from with root = '~ts', "
	%    "current = '~ts'.", [ RootDir, CurrentRelativeDir ] ),

	CurrentDir = any_join( RootDir, CurrentRelativeDir ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( CurrentDir, IfImproperEncoding ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	Acc ++ list_files_in_subdirs( Directories, RootDir,
			CurrentRelativeDir, IncludeSymlinks, IfImproperEncoding,
			_NextAcc=[] ) ++ prefix_files_with( CurrentRelativeDir, Files ).



% Specific helper for find_files_from/5 above:
list_files_in_subdirs( _Dirs=[], _RootDir, _CurrentRelativeDir,
					   _IncludeSymlinks, _IfImproperEncoding, Acc ) ->
	Acc;

list_files_in_subdirs( _Dirs=[ D | T ], RootDir, CurrentRelativeDir,
					   IncludeSymlinks, IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "list_files_in_subdirs with root = '~ts', "
	% "current = '~ts' and D='~ts'.", [ RootDir, CurrentRelativeDir, D ] ),

	NewAcc = find_files_from( RootDir, any_join( CurrentRelativeDir, D ),
					IncludeSymlinks, IfImproperEncoding, _NextAcc=[] ) ++ Acc,

	list_files_in_subdirs( T, RootDir, CurrentRelativeDir, IncludeSymlinks,
						   IfImproperEncoding, NewAcc ).



-doc """
Returns a list of all symlinks found from the root, in the whole subtree (that
is recursively).

All extensions and suffixes accepted, no excluded directories. Elements whose
name is improperly encoded are notified thanks to a warning trace, and then are
ignored.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_links_from( any_directory_path() ) -> [ file_path() ].
find_links_from( RootDir ) ->
	find_links_from( RootDir, _IfImproperEncoding=warn ).



-doc """
Returns a list of all symlinks found from the root, in the whole subtree (that
is recursively).

All extensions and suffixes accepted, no excluded directories. Elements whose
name is improperly encoded are managed according to the IfImproperEncoding
parameter; if set to `include`, the return type of this function is the more
general `[any_file_path()]`, otherwise it is `[file_path()]`.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_links_from( any_directory_path(), improper_encoding_action() ) ->
								[ file_path() ].
find_links_from( RootDir, IfImproperEncoding ) ->
	find_links_from( RootDir, _CurrentRelativeDir="", IfImproperEncoding,
					 _Acc=[] ).


% (helper)
find_links_from( RootDir, CurrentRelativeDir, IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "find_links_from with root = '~ts', "
	%  "current = '~ts'.", [ RootDir, CurrentRelativeDir ] ),

	CurrentDir = any_join( RootDir, CurrentRelativeDir ),

	{ _RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( CurrentDir, IfImproperEncoding ),

	Acc ++ list_links_in_subdirs( Directories, RootDir, CurrentRelativeDir,
								  IfImproperEncoding, _NextAcc=[] )
		++ prefix_files_with( CurrentRelativeDir, Symlinks ).



% Specific helper for find_links_from/4 above:
list_links_in_subdirs( _Dirs=[], _RootDir, _CurrentRelativeDir,
					   _IfImproperEncoding, Acc ) ->
	Acc;

list_links_in_subdirs( _Dirs=[ D | T ], RootDir, CurrentRelativeDir,
					   IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "list_links_in_subdirs with root = '~ts', "
	%   "current = '~ts' and D='~ts'.", [ RootDir, CurrentRelativeDir, D ] ),

	NewAcc = find_links_from( RootDir, any_join( CurrentRelativeDir, D ),
							  IfImproperEncoding, _NextAcc=[] ) ++ Acc,

	list_links_in_subdirs( T, RootDir, CurrentRelativeDir, IfImproperEncoding,
						   NewAcc ).



-doc """
Returns a list of all files (regular ones and symlinks) found from the root with
specified extension, in the whole subtree (that is recursively).

All suffixes accepted, no excluded directories. Elements whose name is
improperly encoded are notified thanks to a warning trace, and then are ignored.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_with_extension_from( any_directory_path(), extension() ) ->
											[ file_path() ].
find_files_with_extension_from( RootDir, Extension ) ->
	find_files_with_extension_from( RootDir, Extension,
									_IfImproperEncoding=warn ).



-doc """
Returns a list of all files (regular ones and symlinks) found from the root with
specified extension, in the whole subtree (that is recursively).

All suffixes accepted, no excluded directories. Elements whose name is
improperly encoded are managed according to the IfImproperEncoding parameter; if
set to `include`, the return type of this function is the more general
`[any_file_path()]`, otherwise it is `[file_path()]`.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_with_extension_from( any_directory_path(), extension(),
							improper_encoding_action() ) -> [ file_path() ].
find_files_with_extension_from( RootDir, Extension, IfImproperEncoding ) ->
	find_files_with_extension_from( RootDir, Extension, _IncludeSymlinks=true,
									IfImproperEncoding ).



-doc """
Returns a list of all files (regular ones and, if requested, symlinks) found
from the root with specified extension, in the whole subtree (that is
recursively).

All suffixes accepted, no excluded directories. Elements whose name is
improperly encoded are managed according to the IfImproperEncoding parameter; if
set to `include`, the return type of this function is the more general
`[any_file_path()]`, otherwise it is `[file_path()]`.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_with_extension_from( any_directory_path(), extension(),
			boolean(), improper_encoding_action() ) -> [ file_path() ].
find_files_with_extension_from( RootDir, Extension, IncludeSymlinks,
								IfImproperEncoding ) ->
	find_files_with_extension_from( RootDir, _CurrentRelativeDir="",
		Extension, IncludeSymlinks, IfImproperEncoding, _Acc=[] ).


% (helper)
find_files_with_extension_from( RootDir, CurrentRelativeDir, Extension,
								IncludeSymlinks, IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "find_files_with_extension_from in '~ts'.",
	%                       [ CurrentRelativeDir ] ),

	CurrentDir = any_join( RootDir, CurrentRelativeDir ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( CurrentDir, IfImproperEncoding ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	Acc ++ list_files_in_subdirs_with_extension( Directories, Extension,
			RootDir, CurrentRelativeDir, IncludeSymlinks, IfImproperEncoding,
			_NextAcc=[] )
		++ prefix_files_with( CurrentRelativeDir,
							  filter_by_extension( Files, Extension ) ).



% Helper for find_files_with_extension_from/6:
list_files_in_subdirs_with_extension( _Dirs=[], _Extension, _RootDir,
		_CurrentRelativeDir, _IncludeSymlinks, _IfImproperEncoding, Acc ) ->
	Acc;

list_files_in_subdirs_with_extension( _Dirs=[ H | T ], Extension, RootDir,
		CurrentRelativeDir, IncludeSymlinks, IfImproperEncoding, Acc ) ->

	NewAcc = find_files_with_extension_from( RootDir,
		any_join( CurrentRelativeDir, H ), Extension, IncludeSymlinks,
		IfImproperEncoding, _NextAcc=[] ) ++ Acc,

	list_files_in_subdirs_with_extension( T, Extension, RootDir,
		CurrentRelativeDir, IncludeSymlinks, IfImproperEncoding, NewAcc ).



-doc """
Returns a list of all files (regular ones and symlinks) found from the root, in
the whole subtree (that is recursively), with specified directories excluded.

Note that an excluded directory can be specified as a full (relative) path
(e.g. `"foo/bar/not-wanted"`), or just as a final directory name (e.g.
`"my-excluded-name"`). In the latter case, all directories bearing that name
(e.g. `"foo/bar/any/my-excluded-name"`) will be excluded as well.

Thus when a directory D is specified in the excluded list, each traversed
directory T will be compared twice to D: T will be matched against D, and
against `filename:basename(T)`, i.e. its final name, as well. As soon as one
matches, T will be excluded.

All extensions and suffixes accepted. Elements whose name is improperly encoded
are notified thanks to a warning trace, and then are ignored.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_with_excluded_dirs( any_directory_path(),
									 [ directory_path() ] ) -> [ file_path() ].
find_files_with_excluded_dirs( RootDir, ExcludedDirs ) ->
	find_files_with_excluded_dirs( RootDir, ExcludedDirs,
                                   _IncludeSymlinks=true ).



-doc """
Returns a list of all files (regular ones and, if requested, symlinks) found
from the root, in the whole subtree (that is recursively), with specified
directories excluded.

Note that an excluded directory can be specified as a full (relative) path
(e.g. `"foo/bar/not-wanted"`), or just as a final directory name (e.g.
`"my-excluded-name"`). In the latter case, all directories bearing that name
(e.g. `"foo/bar/any/my-excluded-name"`) will be excluded as well.

Thus when a directory D is specified in the excluded list, each traversed
directory T will be compared twice to D: T will be matched against D, and
against `filename:basename(T)`, i.e. its final name, as well. As soon as one
matches, T will be excluded.

All extensions and suffixes accepted. Elements whose name is improperly encoded
are notified thanks to a warning trace, and then are ignored.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_with_excluded_dirs( any_directory_path(), [ directory_path() ],
									 boolean() ) -> [ file_path() ].
find_files_with_excluded_dirs( RootDir, ExcludedDirs, IncludeSymlinks ) ->
	find_files_with_excluded_dirs( RootDir, ExcludedDirs, IncludeSymlinks,
								   _IfImproperEncoding=warn ).



-doc """
Returns a list of all files (regular ones and, if requested, symlinks) found
from the root, in the whole subtree (that is recursively), with specified
directories excluded.

Note that an excluded directory can be specified as a full (relative) path
(e.g. `"foo/bar/not-wanted"`), or just as a final directory name (e.g.
`"my-excluded-name"`). In the latter case, all directories bearing that name
(e.g. `"foo/bar/any/my-excluded-name"`) will be excluded as well.

Thus when a directory D is specified in the excluded list, each traversed
directory T will be compared twice to D: T will be matched against D, and
against `filename:basename(T)`, i.e. its final name, as well. As soon as one
matches, T will be excluded.

All extensions and suffixes accepted. Elements whose name is improperly encoded
are managed according to the IfImproperEncoding parameter; if set to `include`,
the return type of this function is the more general `[any_file_path()]`,
otherwise it is `[file_path()]`.

All returned pathnames are relative to this root. For example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_with_excluded_dirs( any_directory_path(), [ directory_path() ],
			boolean(), improper_encoding_action() ) -> [ file_path() ].
find_files_with_excluded_dirs( RootDir, ExcludedDirs, IncludeSymlinks,
							   IfImproperEncoding ) ->

	% Not wanting a lists:member/1 to fail because of a wrong string type:
	BinExcludedDirs = text_utils:ensure_binaries( ExcludedDirs ),

	find_files_with_excluded_dirs( RootDir, _CurrentRelativeDir="",
		BinExcludedDirs, IncludeSymlinks, IfImproperEncoding, _Acc=[] ).


% (helper)
find_files_with_excluded_dirs( RootDir, CurrentRelativeDir, BinExcludedDirs,
							   IncludeSymlinks, IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "find_files_with_excluded_dirs in '~ts'.",
	%                       [ CurrentRelativeDir ] ),

	CurrentDir = any_join( RootDir, CurrentRelativeDir ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( CurrentDir, IfImproperEncoding ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	% If for example ExcludedDirs=[".svn"], we want to eliminate not only
	% ".svn" but also all "foo/bar/.svn", i.e. all directories having the same
	% (last) name:
	%
	FilteredDirectories = [ D || D <- Directories,
		not ( lists:member( bin_join( CurrentRelativeDir, D ), BinExcludedDirs )
			  orelse lists:member( text_utils:ensure_binary( D ),
								   BinExcludedDirs ) ) ],

	Acc ++ list_files_in_subdirs_excluded_dirs( FilteredDirectories, RootDir,
				CurrentRelativeDir, BinExcludedDirs, IncludeSymlinks,
				IfImproperEncoding, _Acc=[] )
		++ prefix_files_with( CurrentRelativeDir, Files ).



% Specific helper for find_files_with_excluded_dirs/6 above:
list_files_in_subdirs_excluded_dirs( _Dirs=[], _RootDir,
		_CurrentRelativeDir, _BinExcludedDirs, _IncludeSymlinks,
		_IfImproperEncoding, Acc ) ->
	Acc;

list_files_in_subdirs_excluded_dirs( _Dirs=[ D | T ], RootDir,
		CurrentRelativeDir, BinExcludedDirs, IncludeSymlinks,
		IfImproperEncoding, Acc ) ->

	NewAcc = find_files_with_excluded_dirs( RootDir,
		any_join( CurrentRelativeDir, D ), BinExcludedDirs, IncludeSymlinks,
		IfImproperEncoding, _NextAcc=[] ) ++ Acc,

	list_files_in_subdirs_excluded_dirs( T, RootDir, CurrentRelativeDir,
		BinExcludedDirs, IncludeSymlinks, IfImproperEncoding, NewAcc ).



-doc """
Returns a list of all files (regular ones and symlinks) found from the root
which do not match any of the specified suffixes, in the whole subtree (that is
recursively).

No excluded directories. Elements whose name is improperly encoded are notified
thanks to a warning trace, and then are ignored.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_with_excluded_suffixes( any_directory_path(),
										 [ any_suffix() ] ) -> [ file_path() ].
find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes ) ->
	find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes,
									   _IfImproperEncoding=warn ).



-doc """
Returns a list of all files (regular ones and symlinks) found from the root
which do not match any of the specified suffixes, in the whole subtree (that is
recursively).

No excluded directories. Elements whose name is improperly encoded are managed
according to the IfImproperEncoding parameter; if set to `include`, the return
type of this function is the more general `[any_file_path()]`, otherwise it is
`[file_path()]`.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_with_excluded_suffixes( any_directory_path(),
			[ any_suffix() ], improper_encoding_action() ) -> [ file_path() ].
find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes,
								   IfImproperEncoding  ) ->
	find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes,
		_IncludeSymlinks=true, IfImproperEncoding ).



-doc """
Returns a list of all files (regular ones and, if requested, symlinks) found
from the root which do not match any of the specified suffixes, in the whole
subtree (that is recursively).

No excluded directories. Elements whose name is improperly encoded are managed
according to the IfImproperEncoding parameter; if set to `include`, the return
type of this function is the more general `[any_file_path()]`, otherwise it is
`[file_path()]`.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_with_excluded_suffixes( any_directory_path(), [ any_suffix() ],
					boolean(), improper_encoding_action() ) -> [ file_path() ].
find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes, IncludeSymlinks,
								   IfImproperEncoding ) ->
	find_files_with_excluded_suffixes( RootDir, _CurrentRelativeDir="",
		ExcludedSuffixes, IncludeSymlinks, IfImproperEncoding, _Acc=[] ).



% (helper)
find_files_with_excluded_suffixes( RootDir, CurrentRelativeDir,
			ExcludedSuffixes, IncludeSymlinks, IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "find_files_with_excluded_suffixes in '~ts'.",
	%                       [ CurrentRelativeDir ] ),

	CurrentDir = any_join( RootDir, CurrentRelativeDir ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( CurrentDir, IfImproperEncoding ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	Acc ++ list_files_in_subdirs_with_excluded_suffixes( Directories,
			ExcludedSuffixes, RootDir, CurrentRelativeDir, IncludeSymlinks,
			IfImproperEncoding, _NextAcc=[] )
		++ prefix_files_with( CurrentRelativeDir,
			filter_by_excluded_suffixes( Files, ExcludedSuffixes ) ).




% Helper for find_files_with_excluded_suffixes/6:
-spec list_files_in_subdirs_with_excluded_suffixes( [ directory_name() ],
		[ any_suffix() ], directory_path(), directory_path(), boolean(),
		improper_encoding_action(), [ file_path() ] ) -> [ file_path() ].
list_files_in_subdirs_with_excluded_suffixes( _Dirs=[], _ExcludedSuffixes,
		_RootDir, _CurrentRelativeDir, _IncludeSymlinks, _IfImproperEncoding,
		Acc ) ->
	Acc;

list_files_in_subdirs_with_excluded_suffixes( _Dirs=[ D | T ], ExcludedSuffixes,
		RootDir, CurrentRelativeDir, IncludeSymlinks, IfImproperEncoding,
		Acc ) ->

	NewAcc = find_files_with_excluded_suffixes( RootDir,
		any_join( CurrentRelativeDir, D ), ExcludedSuffixes, IncludeSymlinks,
		IfImproperEncoding, _NextAcc=[] ) ++ Acc,

	list_files_in_subdirs_with_excluded_suffixes( T, ExcludedSuffixes, RootDir,
		CurrentRelativeDir, IncludeSymlinks, IfImproperEncoding, NewAcc ).



-doc """
Returns a list of all files (regular ones and symlinks) found from the root, in
the whole subtree (that is recursively), with specified directories and suffixes
excluded.

Note that an excluded directory can be specified as a full (relative) path
(e.g. `"foo/bar/not-wanted"`), or just as a final directory name (e.g.
`"my-excluded-name"`). In the latter case, all directories bearing that name
(e.g. `"foo/bar/any/my-excluded-name"`) will be excluded as well.

Thus when a directory D is specified in the excluded list, each traversed
directory T will be compared twice to D: T will be matched against D, and
against `filename:basename(T)`, i.e. its final name, as well. As soon as one
matches, T will be excluded.

Elements whose name is improperly encoded are notified thanks to a warning
trace, and then are ignored.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_with_excluded_dirs_and_suffixes( any_directory_path(),
		[ directory_path() ], [ any_suffix() ] ) -> [ file_path() ].
find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirs,
											ExcludedSuffixes ) ->
	find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirs,
								ExcludedSuffixes, _IncludeSymlinks=true ).



-doc """
Returns a list of all files (regular ones and, if requested, symlinks) found
from the root, in the whole subtree (that is recursively), with specified
directories and suffixes excluded.

Note that an excluded directory can be specified as a full (relative) path
(e.g. `"foo/bar/not-wanted"`), or just as a final directory name (e.g.
`"my-excluded-name"`). In the latter case, all directories bearing that name
(e.g. `"foo/bar/any/my-excluded-name"`) will be excluded as well.

Thus when a directory D is specified in the excluded list, each traversed
directory T will be compared twice to D: T will be matched against D, and
against `filename:basename(T)`, i.e. its final name, as well. As soon as one
matches, T will be excluded.

Elements whose name is improperly encoded are notified thanks to a warning
trace, and then are ignored.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_with_excluded_dirs_and_suffixes( any_directory_path(),
		[ directory_path() ], [ any_suffix() ], boolean() ) -> [ file_path() ].
find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirs,
									ExcludedSuffixes, IncludeSymlinks ) ->
	find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirs,
		ExcludedSuffixes, IncludeSymlinks, _IfImproperEncoding=warn ).


-doc """
Returns a list of all files (regular ones and, if requested, symlinks) found
from the root, in the whole subtree (that is recursively), with specified
directories and suffixes excluded.

Note that an excluded directory can be specified as a full (relative) path
(e.g. `"foo/bar/not-wanted"`), or just as a final directory name (e.g.
`"my-excluded-name"`). In the latter case, all directories bearing that name
(e.g. `"foo/bar/any/my-excluded-name"`) will be excluded as well.

Thus when a directory D is specified in the excluded list, each traversed
directory T will be compared twice to D: T will be matched against D, and
against `filename:basename(T)`, i.e. its final name, as well. As soon as one
matches, T will be excluded.

Elements whose name is improperly encoded are managed according to the
IfImproperEncoding parameter; if set to `include`, the return type of this
function is the more general `[any_file_path()]`, otherwise it is
`[file_path()]`.

All returned pathnames are relative to this root; for example `["./a.txt",
"./tmp/b.txt"]`.
""".
-spec find_files_with_excluded_dirs_and_suffixes( any_directory_path(),
			[ directory_path() ], [ any_suffix() ], boolean(),
			improper_encoding_action() ) -> [ file_path() ].
find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirs,
					ExcludedSuffixes, IncludeSymlinks, IfImproperEncoding ) ->

	%trace_utils:debug_fmt( "find_files_with_excluded_dirs_and_suffixes: from "
	%   "'~ts': RootDir = '~ts', ExcludedDirs = ~p, ExcludedSuffixes = ~p",
	%   [ get_current_directory(), RootDir, ExcludedDirs,
	%     ExcludedSuffixes ] ),

	% Not wanting a lists:member/1 to fail because of a wrong string type:
	BinExcludedDirs = text_utils:ensure_binaries( ExcludedDirs ),

	find_files_with_excluded_dirs_and_suffixes( RootDir,
		_CurrentRelativeDir="", BinExcludedDirs, ExcludedSuffixes,
		IncludeSymlinks, IfImproperEncoding, _Acc=[] ).



% (helper)
find_files_with_excluded_dirs_and_suffixes( RootDir, CurrentRelativeDir,
		ExcludedDirs, ExcludedSuffixes, IncludeSymlinks, IfImproperEncoding,
		Acc ) ->

	%trace_utils:debug_fmt( "find_files_with_excluded_dirs_and_suffixes in "
	%   "~ts / ~ts.", [ RootDir, CurrentRelativeDir ] ),

	CurrentDir = any_join( RootDir, CurrentRelativeDir ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( CurrentDir, IfImproperEncoding ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	% If for example ExcludedDirs=[".svn"], we want to eliminate not only
	% ".svn" but also all "foo/bar/.svn", i.e. all directories having the same
	% (last) name:
	%
	FilteredDirectories = [ D || D <- Directories,
		not ( lists:member( bin_join( CurrentRelativeDir, D ), ExcludedDirs )
			  orelse lists:member( text_utils:ensure_binary( D ),
								   ExcludedDirs ) ) ],

	Acc ++ list_files_in_subdirs_excluded_dirs_and_suffixes(
			FilteredDirectories, RootDir, CurrentRelativeDir,
			ExcludedDirs, ExcludedSuffixes, IncludeSymlinks, IfImproperEncoding,
			_Acc=[] )
		++ prefix_files_with( CurrentRelativeDir,
				filter_by_excluded_suffixes( Files, ExcludedSuffixes ) ).



% Specific helper for find_files_with_excluded_dirs_and_suffixes/7 above:
list_files_in_subdirs_excluded_dirs_and_suffixes( _Dirs=[], _RootDir,
		_CurrentRelativeDir, _ExcludedDirs, _ExcludedSuffixes,
		_IncludeSymlinks, _IfImproperEncoding, Acc ) ->
	Acc;

list_files_in_subdirs_excluded_dirs_and_suffixes( _Dirs=[ D | T ], RootDir,
		CurrentRelativeDir, ExcludedDirs, ExcludedSuffixes, IncludeSymlinks,
		IfImproperEncoding, Acc ) ->

	NewAcc = find_files_with_excluded_dirs_and_suffixes( RootDir,
			any_join( CurrentRelativeDir, D ), ExcludedDirs, ExcludedSuffixes,
			IncludeSymlinks, IfImproperEncoding, _NextAcc=[] ) ++ Acc,

	list_files_in_subdirs_excluded_dirs_and_suffixes( T, RootDir,
		CurrentRelativeDir, ExcludedDirs, ExcludedSuffixes, IncludeSymlinks,
		IfImproperEncoding, NewAcc ).



% Prefixes specified paths with specified root directory.
-spec prefix_files_with( directory_path(), [ file_name() ] ) -> [ file_path() ].
prefix_files_with( RootDir, Files ) ->
	%trace_utils:debug_fmt( "Prefixing ~p with '~ts'.", [ Files, RootDir ] ),
	prefix_files_with( RootDir, Files, _Acc=[] ).


% (helper)
prefix_files_with( _RootDir, _Files=[], Acc ) ->
	Acc;

prefix_files_with( RootDir, [ Str | T ], Acc ) when is_list( Str ) ->
	prefix_files_with( RootDir, T, [ any_join( RootDir, Str ) | Acc ] );

% Trying to overcome weirdly-named files:
prefix_files_with( RootDir, [ BinStr | T ], Acc ) when is_binary( BinStr ) ->
	prefix_files_with( RootDir, T, [ any_join( RootDir, BinStr ) | Acc ] ).



-doc """
Returns a list of all directories found from the root, in the whole subtree
(that is recursively).

All returned pathnames are relative to this root; for example `["./my-dir",
"./tmp/other-dir"]`.
""".
-spec find_directories_from( any_directory_name() ) -> [ directory_name() ].
find_directories_from( RootDir ) ->
	find_directories_from( RootDir, "", _Acc=[] ).


% (helper)
find_directories_from( RootDir, CurrentRelativeDir, Acc ) ->

	%trace_utils:debug_fmt( "find_directories_from in ~ts.",
	%                       [ CurrentRelativeDir ] ),

	{ _RegularFiles, _Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( any_join( RootDir, CurrentRelativeDir ) ),

	Acc ++ list_directories_in_subdirs( Directories, RootDir,
										CurrentRelativeDir, _Acc=[] )
		++ prefix_files_with( CurrentRelativeDir, Directories ).



% (helper)
list_directories_in_subdirs( _Dirs=[], _RootDir, _CurrentRelativeDir, Acc ) ->
	Acc;

list_directories_in_subdirs( _Dirs=[ H | T ], RootDir, CurrentRelativeDir,
							 Acc ) ->
	list_directories_in_subdirs( T, RootDir, CurrentRelativeDir,
		find_directories_from( RootDir, any_join( CurrentRelativeDir, H ),
							   _Acc=[] ) ++ Acc ).



-doc """
Creates the specified directory (`"mkdir"`), without creating any intermediate
(parent) directory that would not exist.

Throws an exception if the operation failed, for example if the directory is
already existing (`{create_directory_failed, "foobar", eexist}`).
""".
-spec create_directory( any_directory_path() ) -> void().
create_directory( AnyDirPath ) ->
	create_directory( AnyDirPath, create_no_parent ).



-doc """
Creates the specified directory.

If `create_no_parent` is specified, no intermediate (parent) directory will be
created.

If `create_parents` is specified, any non-existing intermediate (parent)
directory will be created.

Throws an exception if the operation fails, for example if the directory is
already existing (`{create_directory_failed, "foobar", eexist}`).
""".
-spec create_directory( any_directory_path(), parent_creation() ) -> void().
create_directory( AnyDirPath, create_no_parent ) ->

	case file:make_dir( AnyDirPath ) of

		ok ->
			ok;

		{ error, eacces } ->
			throw( { create_directory_failed,
					 text_utils:ensure_string( AnyDirPath ), access_denied,
					 get_directory_access_denied_info( AnyDirPath ) } );

		{ error, Reason } ->
			throw( { create_directory_failed, AnyDirPath, Reason } )

	end;

create_directory( AnyDirPath, create_parents ) ->
	DirPath = text_utils:ensure_string( AnyDirPath ),
	create_dir_elem( filename:split( DirPath ),  _Prefix="" ).



% (helper)
create_dir_elem( _Elems=[], _Prefix ) ->
	ok;

create_dir_elem( _Elems=[ H | T ], Prefix ) ->

	NewPrefix = join( Prefix, H ),

	exists( NewPrefix ) orelse create_directory( NewPrefix, create_no_parent ),

	create_dir_elem( T, NewPrefix ).



-doc """
Creates the specified directory (but not any parent thereof), if not already
existing.

Throws an exception if the operation fails.
""".
-spec create_directory_if_not_existing( any_directory_path() ) -> void().
create_directory_if_not_existing( AnyDirPath ) ->
	create_directory_if_not_existing( AnyDirPath, create_no_parent ).



-doc """
Creates the specified directory (and, if specified, any needed parent as well),
if not already existing.

Throws an exception if the operation fails.
""".
-spec create_directory_if_not_existing( any_directory_path(),
										parent_creation() ) -> void().
create_directory_if_not_existing( AnyDirPath, ParentCreation ) ->
	is_existing_directory( AnyDirPath ) orelse
		create_directory( AnyDirPath, ParentCreation ).



-doc """
Creates a non-previously existing temporary directory, and returns its full
path.

See also `system_utils:get_default_temporary_directory/0`.
""".
-spec create_temporary_directory() -> directory_path().
create_temporary_directory() ->

	TmpDir = join( [ system_utils:get_default_temporary_directory(),
					 system_utils:get_user_name(), id_utils:generate_uuid() ] ),

	case exists( TmpDir ) of

		true ->
			% Very bad luck apparently, or same random root:
			create_temporary_directory();

		false ->
			create_directory( TmpDir, create_parents ),
			TmpDir

	end.



-doc """
Removes (deletes) the specified file (be them regular files or symbolic links),
specified as any kind of string.

Throws an exception if any problem occurs (e.g. the file does not exist, or
could not be removed for any reason).
""".
-spec remove_file( any_file_path() ) -> void().
remove_file( FilePath ) ->

	%trace_utils:warning_fmt( "Removing file '~ts'.", [ FilePath ] ),

	case file:delete( FilePath ) of
	% To disable side-effect: case ok of

		ok ->
			ok;

		Error ->
			throw( { remove_file_failed, FilePath, Error } )

	end.



-doc """
Removes (deletes) the specified files (be them regular files or symbolic links),
specified as a list of any kind of strings.

Throws an exception if any problem occurs (e.g. a file does not exist, or could
not be removed for any reason).
""".
-spec remove_files( [ any_file_path() ] ) -> void().
remove_files( FilePaths ) ->

	%trace_utils:debug_fmt( "Removing following files: ~ts",
	%                       [ text_utils:strings_to_string( FilePaths ) ] ),

	[ remove_file( FP ) || FP <- FilePaths ].



-doc """
Removes the specified regular file, specified as any kind of string, iff it is
already existing, otherwise does nothing.
""".
-spec remove_file_if_existing( any_file_path() ) -> void().
remove_file_if_existing( FilePath ) ->

	case is_existing_file( FilePath ) of

		true ->
			%trace_utils:debug_fmt( "Removing existing file '~ts'.",
			%                       [ FilePath ] ),
			remove_file( FilePath );

		false ->
			%trace_utils:debug_fmt( "No existing file '~ts' to remove.",
			%                       [ FilePath ] ),
			ok

	end.



-doc """
Removes each of the specified regular files, in the specified list of any kind
of strings, iff it is already existing.
""".
-spec remove_files_if_existing( [ any_file_path() ] ) -> void().
remove_files_if_existing( FilePaths ) ->
	[ remove_file_if_existing( FP ) || FP <- FilePaths ].



-doc """
Removes (deletes) the specified symbolic link, specified as any kind of string.

Checks that the specified path designates indeed a symbolic link (dead or not).

Throws an exception if any problem occurs.
""".
-spec remove_symlink( any_file_path() ) -> void().
remove_symlink( SymlinkPath ) ->

	%trace_utils:warning_fmt( "Removing symlink '~ts'.", [ SymlinkPath ] ),

	case file:read_link_info( SymlinkPath ) of

		{ ok, #file_info{ type=symlink } } ->
			ok;

		{ error, eloop } ->
			% Probably a recursive symlink:
			throw( { too_many_symlink_levels, SymlinkPath } );

		{ error, enoent } ->
			throw( { non_existing_entry, SymlinkPath } );

		{ error, _Reason=eacces } ->
			throw( { remove_symlink_failed,
                     text_utils:ensure_string( SymlinkPath ), access_denied,
                     get_element_access_denied_info( SymlinkPath ) } );

		{ error, Reason } ->
			throw( { remove_symlink_failed,
                     text_utils:ensure_string( SymlinkPath ), Reason } )

	end,

	case file:delete( SymlinkPath ) of

		ok ->
			ok;

		Error ->
			throw( { remove_symlink_failed, SymlinkPath, Error } )

	end.



-doc """
Removes (deletes) the specified symbolic link, specified as any kind of string,
iff it is already existing, otherwise does nothing.

Checks that the specified path designates indeed a symbolic link (dead or not).

Throws an exception if any problem occurs.
""".
-spec remove_symlink_if_existing( any_file_path() ) -> void().
remove_symlink_if_existing( SymlinkPath ) ->

	case is_existing_link( SymlinkPath ) of

		true ->
			%trace_utils:debug_fmt( "Removing existing symlink '~ts'.",
			%                       [ SymlinkPath ] ),
			remove_symlink( SymlinkPath );

		false ->
			%trace_utils:debug_fmt( "No existing symlink '~ts' to remove.",
			%                       [ SymlinkPath ] ),
			ok

	end.



-doc """
Removes (deletes) the specified file (regular or symbolic link), specified as
any kind of string, iff it is already existing, otherwise does nothing.

Throws an exception if any problem occurs.
""".
-spec remove_file_or_link_if_existing( any_file_path() ) -> void().
remove_file_or_link_if_existing( FileOrLinkPath ) ->

	case is_existing_file_or_link( FileOrLinkPath ) of

		true ->
			%trace_utils:debug_fmt( "Removing existing file or link '~ts'.",
			%                       [ FileOrLinkPath ] ),

			% Works for regular files and symbolic links:
			remove_file( FileOrLinkPath );

		false ->
			%trace_utils:debug_fmt( "No existing file or link '~ts' to remove.",
			%                       [ FileOrLinkPath ] ),
			ok

	end.



-doc """
Removes the specified directory, which must be empty (so: behaves mostly like
the `rmdir` shell command).
""".
-spec remove_empty_directory( any_directory_path() ) -> void().
remove_empty_directory( AnyDirPath ) ->

	%trace_utils:warning_fmt( "## Removing empty directory '~ts'.",
	%                         [ AnyDirPath ] ),

	case file:del_dir( AnyDirPath ) of

		ok ->
			ok;

		{ error, eacces } ->
			throw( { remove_empty_directory_failed,
					 text_utils:ensure_string( AnyDirPath ), access_denied,
					 get_directory_access_denied_info( AnyDirPath ) } );

		{ error, Reason } ->
			trace_utils:error_fmt( "Removal of directory '~ts' failed: ~p.",
								   [ AnyDirPath, Reason ] ),
			% Probably not so empty:
			throw( { remove_empty_directory_failed,
					 text_utils:ensure_string( AnyDirPath ), Reason } )

	end.



-doc """
Removes all (supposedly) empty directories pertaining to the specified local,
relative directory path, that is this path (e.g. `a/b/c`) and all its ancestors
(hence `a/b` and `a` are - if empty - removed as well, and none of their
possible siblings of course); so behaves mostly like the `rmdir --parents` shell
command.

Note: does not remove an (empty) tree, just a given directory and its local
ancestors.
""".
-spec remove_empty_path( any_directory_path() ) -> void().
remove_empty_path( DirectoryPath ) ->

	%trace_utils:warning_fmt( "## Removing empty directory '~ts'.",
	%                         [ DirectoryPath ] ),

	remove_empty_path_helper( DirectoryPath ).


% (helper)
remove_empty_path_helper( _DirectoryPath="." ) ->
	ok;

remove_empty_path_helper( DirectoryPath ) ->
	remove_empty_directory( DirectoryPath ),
	remove_empty_path_helper( filename:dirname( DirectoryPath ) ).



-doc """
Removes all (supposedly) empty directories found from the specified directory,
expected to be the root of a tree that contains only (possibly nested)
directories (and no other kind of filesystem entry).
""".
-spec remove_empty_tree( any_directory_path() ) -> void().
remove_empty_tree( DirectoryPath ) ->

	%trace_utils:warning_fmt( "## Removing empty tree '~ts'.",
	%                         [ DirectoryPath ] ),

	% For clarity:
	is_existing_directory( DirectoryPath ) orelse
		throw( { directory_not_found, DirectoryPath } ),

	{ RegularFiles, Symlinks, Directories, OtherFiles, Devices } =
		list_dir_elements( DirectoryPath, _ImproperEncodingAction=include ),

	RegularFiles =:= [] orelse
		throw( { regular_files_found, RegularFiles, DirectoryPath } ),

	Symlinks =:= [] orelse
		throw( { symbolic_links_found, Symlinks, DirectoryPath } ),

	OtherFiles =:= [] orelse
		throw( { other_files_found, OtherFiles, DirectoryPath } ),

	Devices =:= [] orelse
		throw( { devices_found, Devices, DirectoryPath } ),

	[ remove_empty_tree( any_join( DirectoryPath, D ) ) || D <- Directories ],

	% Now an empty directory, so:
	remove_directory( DirectoryPath ).



-doc """
Removes the specified (possibly non-empty) directory as a whole (i.e. including
its full content), recursively (so: behaves mostly like the `rm -rf` shell
command; of course to use with much care).

Note that if any unusual file entry is found in the tree (e.g. device or file
that is neither regular nor a symbolic link), the operation will stop on error
(whereas elements may already have been removed).
""".
-spec remove_directory( any_directory_path() ) -> void().
remove_directory( DirectoryPath ) ->

	%trace_utils:warning_fmt( "## Removing recursively directory '~ts'.",
	%                         [ DirectoryPath ] ),

	% We do it programmatically, rather than running a command like '/bin/rm -rf
	% ...':

	% All local elements:
	{ RegularFiles, Symlinks, Directories, OtherFiles, Devices } =
		list_dir_elements( DirectoryPath, _ImproperEncodingAction=include ),

	Devices =:= [] orelse
		begin
			trace_utils:error_fmt( "Interrupting removal of directory '~ts', "
				"as device entries have been found: ~p.", [ Devices ] ),

			throw( { device_entries_found, Devices } )
		end,

	OtherFiles =:= [] orelse
		begin
			trace_utils:error_fmt( "Interrupting removal of directory '~ts', "
				"as unexpected filesystem entries have been found: ~p.",
				[ OtherFiles ] ),

			throw( { unexpected_entries_found, OtherFiles } )
	end,

	% Depth-first of course:
	[ remove_directory( any_join( DirectoryPath, SubDir ) )
                                || SubDir <- Directories ],

	% Then removing all local regular files and symlinks:
	[ remove_file( any_join( DirectoryPath, F ) )
                                || F <- Symlinks ++ RegularFiles ],

	% Finally removing this (now empty) directory as well:
	remove_empty_directory( DirectoryPath ).



-doc """
Removes the specified (possibly non-empty) directories as a whole
(i.e. including its full content), recursively (so: behaves mostly like the `rm
-rf` shell command; of course to use with much care).

Note that if any unusual file entry is found in the tree (e.g. device or file
that is neither regular nor a symbolic link), the operation will stop on error
(whereas elements may already have been removed).
""".
-spec remove_directories( [ any_directory_path() ] ) -> void().
remove_directories( DirectoryPaths ) ->
	[ remove_directory( DP ) || DP <- DirectoryPaths ].



-doc """
Removes, if it exists, the specified (possibly non-empty) directory as a whole
(i.e. including its full content), recursively (so: behaves mostly like the `rm
-rf` shell command; of course to use with much care).

Note that if any unusual file entry is found in the tree (e.g. device or file
that is neither regular nor a symbolic link), the operation will stop on error
(whereas elements may already have been removed).
""".
-spec remove_directory_if_existing( any_directory_path() ) -> void().
remove_directory_if_existing( DirectoryPath ) ->
	is_existing_directory( DirectoryPath ) andalso
		remove_directory( DirectoryPath ).



-doc """
Removes, if they exist, the specified (possibly non-empty) directories as a
whole (i.e. including its full content), recursively (so: behaves mostly like
the `rm -rf` shell command; of course to use with care).

Note that if any unusual file entry is found in the tree (e.g. device or file
that is neither regular nor a symbolic link), the operation will stop on error
(whereas elements may already have been removed).
""".
-spec remove_directories_if_existing( [ any_directory_path() ] ) -> void().
remove_directories_if_existing( DirectoryPaths ) ->
	[ remove_directory_if_existing( DP ) || DP <- DirectoryPaths ].



-doc """
Copies the specified file to a given destination filename (not a directory name,
see `copy_file_in/2` for that), overwriting any previous file.

Note: content is copied and permissions are preserved (e.g. the copy of an
executable file will be itself executable, and other permissions as well, unlike
`/bin/cp` that relies on umask).
""".
-spec copy_file( any_file_path(), any_file_path() ) -> void().
copy_file( SourceFilePath, DestinationFilePath ) ->

	case try_copy_file( SourceFilePath, DestinationFilePath ) of

		ok ->
			ok;

		{ error, eacces } ->
			throw( { copy_file_failed,
					 text_utils:ensure_string( SourceFilePath ),
					 text_utils:ensure_string( DestinationFilePath ),
					 access_denied,
					 get_file_access_denied_info( SourceFilePath ),
					 get_file_access_denied_info( DestinationFilePath ) } );

		{ error, Reason } ->
			throw( { copy_file_failed,
					 text_utils:ensure_string( SourceFilePath ),
					 text_utils:ensure_string( DestinationFilePath ), Reason } )

	end.



-doc """
Copies the specified file to a given destination filename (not a directory name,
see `copy_file_in/2` for that), overwriting any previous file.

Symlinks are copied as symlinks (whereas `file:copy/2` would copy their target
as new files).

Note: content is copied and permissions are preserved (e.g. the copy of an
executable file will be itself executable, and other permissions as well, unlike
`/bin/cp` that relies on umask).
""".
-spec try_copy_file( any_file_path(), any_file_path() ) ->
                                            basic_utils:base_status().
try_copy_file( SourceFilePath, DestinationFilePath ) ->

	% First, checks the source file exists and retrieves its meta-information:
	%
	% (note: not using file:read_file_info/1, as we prefer referring to symlinks
	% themselves rather than to the element they point to; otherwise a broken
	% link would trigger 'enoent')
	%
	case file:read_link_info( SourceFilePath ) of

		% Here we want to create in turn a symlink (broken or not, pointing
		% through the exact same path definition, absolute or relative):
		%
		{ ok, #file_info{ type=symlink } } ->
			case file:read_link_all( SourceFilePath ) of

				% Possibly a raw file element:
				{ ok, LinkTargetPath } ->
					case file:make_symlink( LinkTargetPath,
											DestinationFilePath ) of

						ok ->
							ok;

						{ error, Reason } ->
							trace_utils:error_fmt( "Cannot create symlink "
								"'~ts' pointing to '~ts': ~p.",
								[ DestinationFilePath, LinkTargetPath,
                                  Reason ] ),
							throw( { symlink_creation_failed, Reason,
									 DestinationFilePath, LinkTargetPath } )

					end;

				{ error, Reason } ->
					trace_utils:error_fmt( "Cannot determine the target of "
						"symbolic link '~ts': ~p.",
						[ SourceFilePath, Reason ] ),
					throw( { symlink_resolution_failed, Reason,
							 SourceFilePath } )

			end;

		{ ok, #file_info{ type=regular, mode=Mode } } ->

			% Yet file:copy/2 will fail (with 'enoent') if SourceFilePath is a
			% broken symlink, as it copies the target of the link, not the link
			% itself.
			%
			case file:copy( SourceFilePath, DestinationFilePath ) of

				{ ok, _ByteCount } ->
					% Now sets the permissions of the copy:
					file:change_mode( DestinationFilePath, Mode );

				CopyError ->
					CopyError

			end;

		ReadError ->
			ReadError

	end.



-doc """
Copies the specified file in the specified destination directory, overwriting
any previous file, and returning the full path of the copied file.

Note: content is copied and permissions are preserved (e.g. the copy of an
executable file will be itself executable, like for the other permissions - and
unlike `/bin/cp`, which relies on umask).
""".
-spec copy_file_in( any_file_path(), any_directory_name() ) -> any_file_path().
copy_file_in( SourcePath, DestinationDirectory ) ->

	Filename = filename:basename( SourcePath ),

	TargetPath = any_join( DestinationDirectory, Filename ),

	copy_file( SourcePath, TargetPath ),

	TargetPath.



-doc """
Copies the actual regular file specified - either directly a regular file, or a
regular file ultimately pointed to by any specified symbolic link - in the
specified destination directory, overwriting any previous file, and returning
the full path of the copied file.

Note: content is copied and permissions are preserved (e.g. the copy of an
executable file will be itself executable, like for the other permissions - and
unlike `/bin/cp`, which relies on umask).
""".
-spec copy_as_regular_file_in( any_file_path(), any_directory_name() ) ->
		  any_file_path().
copy_as_regular_file_in( SourcePath, DestinationDirectory ) ->

	ActualSourcePath = case is_link( SourcePath ) of

		true ->
			resolve_symlink_fully( SourcePath );

		false ->
			SourcePath

	end,

	copy_file_in( ActualSourcePath, DestinationDirectory ).



-doc """
Copies the specified file to a given destination iff this source file is already
existing.

Note: content is copied and permissions are preserved (e.g. the copy of an
executable file will be itself executable, likz for the other permissions - and
unlike `/bin/cp`, which relies on umask).
""".
-spec copy_file_if_existing( any_file_path(), any_file_path() ) -> void().
copy_file_if_existing( SourceFilePath, DestinationFilePath ) ->
	is_existing_file( SourceFilePath )
		andalso copy_file( SourceFilePath, DestinationFilePath ).



-doc "Copies the specified source tree in specified target directory.".
-spec copy_tree( any_directory_path(), any_directory_path() ) -> void().
copy_tree( SourceTreePath, TargetDirectory ) ->

	is_existing_directory_or_link( SourceTreePath )
		orelse throw( { non_existing_source_tree, SourceTreePath } ),

	 is_existing_directory_or_link( TargetDirectory )
		orelse throw( { non_existing_target_directory, TargetDirectory } ),

	Cmd = text_utils:format( "/bin/cp -r '~ts' '~ts'",
							 [ SourceTreePath, TargetDirectory ] ),

	case system_utils:run_command( Cmd ) of

		{ _ExitCode=0, _Output=[] } ->
			ok;

		{ ExitCode, ErrorOutput } ->
			throw( { copy_tree_failed, { SourceTreePath, TargetDirectory },
					 ExitCode, ErrorOutput } )

	end.



-doc """
Renames the specified file.

Returns, for convenience, the new name.
""".
-spec rename( any_file_path(), any_file_path() ) -> any_file_path().
rename( SourceFilePath, DestinationFilePath ) ->
	move_file( SourceFilePath, DestinationFilePath ).



-doc """
Renames the specified file; if the destination file already exists, renames it
first by suffixing `.previous` to its name (then overwriting any
identically-named file that would already exist), before performing the
renaming.

Returns, for convenience, the new name.
""".
-spec rename_preserving( any_file_path(), any_file_path() ) -> any_file_path().
rename_preserving( SourceFilePath, DestinationFilePath ) ->
	rename_preserving( SourceFilePath, DestinationFilePath,
					   _HidingSuffix=?default_hiding_suffix ).



-doc """
Renames the specified file; if the destination file already exists, renames it
first by adding the specified suffix to its name (then overwriting any
identically-named file that would already exist), before performing the
renaming.

Returns, for convenience, the new name.
""".
-spec rename_preserving( any_file_path(), any_file_path(),
						 any_string() ) -> any_file_path().
rename_preserving( SourceFilePath, DestinationFilePath, HidingSuffix ) ->

	is_existing_file_or_link( DestinationFilePath ) andalso
		hide_overwriting( DestinationFilePath, HidingSuffix ),

	rename( SourceFilePath, DestinationFilePath ).



-doc """
Hides the specified file: renames it to a conventionally-deriving name, to have
it out of the way; throws an exception if the resulting file already exists.

Returns its new name.
""".
-spec hide( any_file_path() ) -> any_file_path().
hide( ToHidePath ) ->
	hide( ToHidePath, _HidingSuffix=?default_hiding_suffix ).



-doc """
Hides the specified file: renames it based on the specified suffix, to have it
out of the way; throws an exception if the resulting file already exists.

Returns its new name.
""".
-spec hide( any_file_path(), any_string() ) -> any_file_path().
hide( ToHidePath, HidingSuffix ) ->

	% To support string_like():
	HiddenPathBin = text_utils:bin_concatenate(
		[ text_utils:ensure_binary( ToHidePath ),
		  text_utils:ensure_binary( HidingSuffix ) ] ),

	is_existing_file_or_link( HiddenPathBin ) andalso
		throw( { file_to_hide_already_exists,
                 text_utils:binary_to_string(  HiddenPathBin ) } ),

	move_file( ToHidePath, HiddenPathBin ).



-doc """
Hides the specified file: renames it to a conventionally-deriving name, to have
it out of the way; if the resulting file already exists, it is overwritten.

Returns its new name.
""".
-spec hide_overwriting( any_file_path() ) -> any_file_path().
hide_overwriting( ToHidePath ) ->
	hide_overwriting( ToHidePath, _HidingSuffix=?default_hiding_suffix ).



-doc """
Hides the specified file: renames it to a conventionally-deriving name, to have
it out of the way; if the resulting file already exists, it is overwritten.

Returns its new name.
""".
-spec hide_overwriting( any_file_path(), any_string() ) -> any_file_path().
hide_overwriting( ToHidePath, HidingSuffix ) ->

	% To support string_like():
	HiddenPathBin = text_utils:bin_concatenate(
		[ text_utils:ensure_binary( ToHidePath ),
		  text_utils:ensure_binary( HidingSuffix ) ] ),

	is_existing_file_or_link( HiddenPathBin ) andalso
		remove_file( HiddenPathBin ),

	move_file( ToHidePath, HiddenPathBin ).



-doc """
Moves the specified file or symbolic link so that it is now designated by
the specified path.

Note:
 - no check that source is a file or symlink (e.g. not a directory) is done
 - destination is a file path, not a directory path, and it is expected not to
 exist already

Returns, for convenience, the new path.
""".
-spec move_file( any_file_path(), any_file_path() ) -> any_file_path().
move_file( SourceFilePath, DestinationFilePath ) ->

	%trace_utils:warning_fmt( "## Moving file '~ts' to '~ts'.",
	%                         [ SourceFilePath, DestinationFilePath ] ),

	%copy_file( SourceFilePath, DestinationFilePath ),
	%remove_file( SourceFilePath ).

	% Simpler, better than above, yet does not works across filesystems:
	case file:rename( SourceFilePath, DestinationFilePath ) of

		ok ->
			DestinationFilePath;

		{ error, exdev } ->
			%trace_utils:info_fmt( "Moving across filesystems '~ts' to '~ts'.",
			%                      [ SourceFilePath, DestinationFilePath ] ),
			copy_file( SourceFilePath, DestinationFilePath ),
			remove_file( SourceFilePath );

		{ error, _Reason=eacces } ->
			throw( { move_file_failed,
                     text_utils:ensure_string( SourceFilePath ),
                     text_utils:ensure_string( DestinationFilePath ),
                     access_denied,
                     get_file_access_denied_info( SourceFilePath ),
                     get_file_access_denied_info( DestinationFilePath ) } );

		Error ->
			throw( { move_file_failed, Error, SourceFilePath,
					 DestinationFilePath } )

	end.



-doc """
Creates a symbolic link pointing to the specified target path, at the specified
new (link) path.

For example `create_link("Projects/SomeProject", "/home/joe/my-link")` will
create a `"/home/joe/my-link"` symlink pointing to `"Projects/SomeProject"`
(thus relatively to `"/home/joe/"`), whether or not this
`"/home/joe/Projects/SomeProject"` target exists (so the current directory does
not matter here).
""".
-spec create_link( any_path(), link_path() ) -> void().
create_link( TargetPath, NewLinkPath ) ->

	%trace_utils:debug_fmt( "Creating a link '~ts' to '~ts'.",
	%                       [ NewLinkPath, TargetPath ] ),

	case file:make_symlink( TargetPath, NewLinkPath ) of

		ok ->
			ok;

		{ error, _Reason=eacces } ->
			throw( { create_link_failed, text_utils:ensure_string( TargetPath ),
                     text_utils:ensure_string( NewLinkPath ),
                     access_denied,
                     get_element_access_denied_info( TargetPath ),
                     get_element_access_denied_info( NewLinkPath ) } );

		{ error, Reason } ->
			throw( { link_creation_failed, { target, TargetPath },
					 { link, NewLinkPath }, Reason } )

	end.



-doc """
Returns a path deriving from the specified one (and of the same type) so that it
is unique, meaning that it does not clash with any pre-existing entry.

Note: of course multiple, parallel calls to this function with the same base
path will result in potential race conditions and risks of collisions.

See also `basic_utils:get_unix_process_specific_string/0`.
""".
-spec get_non_clashing_entry_name_from( any_path() ) -> any_path().
get_non_clashing_entry_name_from( Path ) ->

	% For example
	% - if "aaa/bbb/foobar.txt" is specified, returns "aaa/bbb/foobar.txt-1"
	% - if "aaa/bbb/foobar.txt-4" is specified, returns "aaa/bbb/foobar.txt-5"

	% More reliable than looping over random names forged from for example:
	%Uniq = basic_utils:get_process_specific_value()
	%   + random_utils:get_uniform_value( _Min=0, _Max=10000 ),
	% until no collision occurs.

	%trace_utils:debug_fmt( "Testing whether path '~ts' already exists...",
	%                       [ Path ] ),

	case exists( Path ) of

		true ->
			PathStr = text_utils:ensure_string( Path ),
			PathToTestStr = case string:split( PathStr, _SearchPattern="-",
											   _Where=trailing ) of

				[ _Path ] ->
					text_utils:format( "~ts-1", [ Path ] );

				[ BasePath, FinalPart ] ->
					case text_utils:try_string_to_integer( FinalPart ) of

						% Not already ending with a dash plus a number:
						undefined ->
							text_utils:format( "~ts-1", [ Path ] );

						Count ->
							text_utils:format( "~ts-~B", [ BasePath, Count+1 ] )

					end

			end,

			PathToTest = case is_binary( Path ) of

				true ->
					text_utils:string_to_binary( PathToTestStr );

				false ->
					PathToTestStr

			end,

			 % As clashes may happen for any name:
			get_non_clashing_entry_name_from( PathToTest );

		false ->
			Path

	end.



-doc """
Appends, at the end of the first specified file, the content of the second
specified one: concatenates the second with the first one.
""".
-spec append_file( file_name(), file_name() ) -> void().
append_file( TargetFilename, ToAppendFilename ) ->

	ToAppendBin = read_whole( ToAppendFilename ),

	% Test needed, otherwise the next append could create from scratch the
	% target file (would be a masked failure):
	%
	case is_existing_file_or_link( TargetFilename ) of

		true ->
			TargetFile = open( TargetFilename, _Opts=[ append ] ),

			write( TargetFile, ToAppendBin ),

			close( TargetFile );

		false ->
			throw( { append_target_file_not_found, TargetFilename } )

	end.



-doc "Lists all known permission types, as `{Perm,Mask}` pairs.".
-spec list_permission_pairs() -> [ { permission(), permission_mask() } ].
list_permission_pairs() ->
	[ { owner_read,    8#00400 },
	  { owner_write,   8#00200 },
	  { owner_execute, 8#00100 },

	  { group_read,    8#00040 },
	  { group_write,   8#00020 },
	  { group_execute, 8#00010 },

	  { other_read,    8#00004 },
	  { other_write,   8#00002 },
	  { other_execute, 8#00001 },

	  { set_user_id,  16#800 },
	  { set_group_id, 16#400 } ].



-doc """
Encodes the specified symbolic permission(s) into its/their low-level
counterpart mask(s).
""".
-spec to_permission_mask( maybe_list( permission() ) ) -> permission_mask().
to_permission_mask( PermissionList ) when is_list( PermissionList ) ->
	PermPairs = list_permission_pairs(),
	lists:foldl( fun( P, Acc ) ->
					 to_permission_mask( P, PermPairs ) + Acc
				 end,
				 _Acc0=0,
				 PermissionList );

to_permission_mask( PermAtom ) ->
	to_permission_mask( PermAtom, _PermPairs=list_permission_pairs() ).


% (helper)
to_permission_mask( PermAtom, PermPairs ) ->
	case lists:keyfind( _K=PermAtom, _Index=1, PermPairs ) of

		false ->
			throw( { invalid_permission, PermAtom } );

		{ _PermAtom, PermMask } ->
			PermMask

	end.



-doc """
Decodes the specified permission mask into a list of the corresponding
permissions.
""".
-spec from_permission_mask( permission_mask() ) -> [ permission() ].
from_permission_mask( Mask ) ->
	PermPairs = list_permission_pairs(),
	from_permission_mask( PermPairs, Mask, _AccPerms=[] ).


% (helper)
from_permission_mask( _PermPairs=[], _Mask, AccPerms ) ->
	% In-order preferred:
	lists:reverse( AccPerms );

from_permission_mask( _PermPairs=[ { Perm, PermMask } | T ], Mask, AccPerms ) ->

	%trace_utils:debug_fmt( "From permission mask '~p': testing ~p (i.e. ~p).",
	%                       [ Mask, Perm, PermMask ] ),

	NewAccPerms = case Mask band PermMask of

		0 ->
			AccPerms;

		_ ->
			[ Perm | AccPerms ]

	end,

	from_permission_mask( T, Mask, NewAccPerms ).



-doc """
Returns the (UNIX) permissions associated to the specified filesystem entry.
""".
-spec get_permissions_of( any_path() ) -> [ permission() ].
get_permissions_of( EntryPath ) ->

	case file:read_file_info( EntryPath ) of

		{ ok, #file_info{ mode=Mode } } ->
			from_permission_mask( Mode );

		{ error, _Reason=eacces } ->
			throw( { get_permissions_of_failed,
                     text_utils:ensure_string( EntryPath ), access_denied,
                     get_element_access_denied_info( EntryPath ) } );

		{ error, Reason } ->
			throw( { get_permissions_of_failed, EntryPath, Reason } )

	end.



-doc """
Returns any description of the permissions corresponding to the specified file
entry.

Never fails.
""".
-spec describe_permissions_of( any_path() ) -> ustring().
describe_permissions_of( EntryPath ) ->

	case file:read_file_info( EntryPath ) of

		{ ok, #file_info{ mode=Mode } } ->
			text_utils:format( "~w", [ from_permission_mask( Mode ) ] );

		{ error, eacces} ->
			"unknown (insufficient permissions)";

		{ error, Reason } ->
			text_utils:format( "unknown (reason: ~p)", [ Reason ] )

	end.



-doc """
Changes the permissions (`chmod`) of the specified filesystem element.

Note: erases any prior permissions, i.e. if specifying `[other_read]` then a
corresponding file will end up with (exactly) a `-------r--` permission.
""".
-spec change_permissions( any_path(), permission() | [ permission() ] ) ->
								void().
change_permissions( Path, NewPermissions ) ->

	NewPermMask = to_permission_mask( NewPermissions ),

	%trace_utils:debug_fmt( "Permissions to be changed to ~p, i.e. ~p.",
	%					   [ NewPermissions, NewPermMask ] ),

	case file:change_mode( Path, NewPermMask ) of

		ok ->
			ok;

		{ error, _Reason=eacces } ->
			throw( { change_permission_failed, access_denied, Path,
                     NewPermissions, get_element_access_denied_info( Path ) } );

		{ error, Reason } ->
			throw( { change_permission_failed, Reason, Path, NewPermissions } )

	end.



-doc """
Tells whether the specified path is an absolute one.

A path is deemed absolute iff it starts with `"/"`.
""".
-spec is_absolute_path( any_path() ) -> boolean().
%is_absolute_path( _Path=[ $/ | _Rest ] ) ->
%   true;

% Not wanting to let for example atoms slip through:
%is_absolute_path( Path ) when is_list( Path )->
%   false;

%is_absolute_path( Other ) ->
%   throw( { not_a_string_path, Other } ).
is_absolute_path( AnyPath ) ->
	% To support also binary (and even atom) paths:
	case filename:pathtype( AnyPath ) of

		absolute ->
			true;

		relative ->
			false

		%volumerelative -> intentional case_clause

	end.



-doc """
Returns an absolute, normalised path corresponding to the specified path.

Returns a string of the same type as the specified one.

If it is not already absolute, it will made so by using the current working
directory.

Acts a bit like the `realpath` command.
""".
-spec ensure_path_is_absolute( path() ) -> path();
							 ( bin_path() ) -> bin_path().
ensure_path_is_absolute( Path ) ->

	AbsPath = case is_absolute_path( Path ) of

		true ->
			% Already absolute:
			Path;

		false ->
			% Relative, using current directory as base, and returning the same
			% string type as Path:
			%
			any_join( get_current_directory(), Path )

	end,

	normalise_path( AbsPath ).



-doc """
Returns an absolute, normalised path corresponding to the specified target path,
using base path as root directory (this must be an absolute path) if the target
path is not absolute.

Returns a plain string iff both specified ones are plain, otherwise returns a
binary.

For example `ensure_path_is_absolute("tmp/foo", "/home/dalton")` will return
`"/home/dalton/tmp/foo"`.
""".
-spec ensure_path_is_absolute( any_path(), any_path() ) -> any_path().
ensure_path_is_absolute( TargetPath, BasePath ) ->

	case is_absolute_path( TargetPath ) of

		true ->
			% Already absolute:
			NormTargetPath = normalise_path( TargetPath ),

			% Ensure correct string type:
			case is_list( BasePath ) of

				true ->
					NormTargetPath;

				% BasePath expected to be a binary then, so returning such type:
				false ->
					text_utils:ensure_binary( NormTargetPath )

			end;


		false ->
			% Relative, using specified base directory (types follow):
			case is_absolute_path( BasePath ) of

				true ->
					normalise_path( any_join( BasePath, TargetPath ) );

				false ->
					throw( { base_path_not_absolute, BasePath } )

			end

	end.



-doc """
Normalises specified path (canonicalises it), by translating it so that no
intermediate, superfluous `.` or `..` elements are present afterwards.

For example, `"/home/garfield/../lisa/./src/.././tube"` shall be normalised in
`"/home/lisa/tube"`.

Returns a path of the same string type as the specified parameter.
""".
-spec normalise_path( path() ) -> path();
					( bin_path() ) -> bin_path().
normalise_path( _Path="." ) ->
	".";
	%get_current_directory();

normalise_path( Path ) when is_list( Path ) ->

	ElemList = filename:split( Path ),

	%trace_utils:debug_fmt( "ElemList: ~p", [ ElemList ] ),

	ResPath = join( filter_elems_plain( ElemList, _Acc=[] ) ),

	%trace_utils:debug_fmt( "Normalising path '~ts' as '~ts'.",
	%                       [ Path, ResPath ] ),

	ResPath;


normalise_path( BinPath ) when is_binary( BinPath ) ->

	ElemList = filename:split( BinPath ),

	%trace_utils:debug_fmt( "ElemList: ~p", [ ElemList ] ),

	ResPath = bin_join( filter_elems_bin( ElemList, _Acc=[] ) ),

	%trace_utils:debug_fmt( "Normalising path '~ts' as '~ts'.",
	%                       [ BinPath, ResPath ] ),

	ResPath.



% (helper)
filter_elems_plain( _ElemList=[], Acc ) ->
	lists:reverse( Acc );

filter_elems_plain( _ElemList=[ "." | T ], Acc ) ->
	filter_elems_plain( T, Acc );

% We can remove one level iff there is at least one accumulated *and* this one
% is not already ".." (otherwise the ".." will cancel out):
%
filter_elems_plain( _ElemList=[ ".." | T ], _Acc=[ PrevElem | AccT ] )
                                            when PrevElem =/= ".." ->
	filter_elems_plain( T, AccT );


% No level left, so this ".." should not be filtered out:
%
% (however this clause is a special case of the next, hence can be commented
% out)
%
%filter_elems_plain( _ElemList=[ PathElement=".." | T ], Acc ) ->
%   filter_elems_plain( T, [ PathElement | Acc ] );

filter_elems_plain( _ElemList=[ E | T ], Acc ) ->
	filter_elems_plain( T, [ E | Acc ] ).


% The approach below would not work with, for example, "X/Y/Z/../../A":

% RevElemList = lists:reverse( filename:split( Path ) ),

% % Returns in the right order:
% join( filter_elems_plain( RevElemList, _Acc=[] ) ).


% filter_elems_plain( _Elems=[], Acc ) ->
%   Acc;

% filter_elems_plain( _Elems=[ "." | T ], Acc ) ->
%   filter_elems_plain( T, Acc );

% filter_elems_plain( _Elems=[ "..", _E | T ], Acc ) ->
%   filter_elems_plain( T, Acc );

% filter_elems_plain( _Elems=[ E | T ], Acc ) ->
%   filter_elems_plain( T, [ E | Acc ] ).



% (helper)
filter_elems_bin( _ElemList=[], Acc ) ->
	lists:reverse( Acc );

filter_elems_bin( _ElemList=[ <<".">> | T ], Acc ) ->
	filter_elems_bin( T, Acc );

% We can remove one level iff there is at least one:
filter_elems_bin( _ElemList=[ <<"..">> | T ], _Acc=[ _ | AccT ] ) ->
	filter_elems_bin( T, AccT );

% No level left, so this <<"..">> should not be filtered out:
%
% (however this clause is a special case of the next, hence can be commented
% out)
%
%filter_elems_bin( _ElemList=[ PathElement=<<"..">> | T ], Acc ) ->
%   filter_elems_bin( T, [ PathElement | Acc ] );

filter_elems_bin( _ElemList=[ E | T ], Acc ) ->
	filter_elems_bin( T, [ E | Acc ] ).



-doc """
Returns a version of the specified path that is relative to the current
directory; returns the same type (plain or binary string) as the one of the
specified path.
""".
-spec make_relative( any_path() ) -> any_path().
make_relative( Path ) ->
	make_relative( Path, _RefDir=get_current_directory() ).



-doc """
Returns a version of the first specified path that is relative to the specified
second reference directory; returns the same type (plain or binary string) as
the one of the first specified path.
""".
-spec make_relative( any_path(), any_directory_path() ) -> any_path().
make_relative( Path, RefDir ) when is_list( Path ) andalso is_list( RefDir ) ->

	% Different from filename:absname/2:
	% file_utils:make_relative("/aa/bb/cc","/aa/bb").
	% "cc"
	% filename:absname("/aa/bb/cc","/aa/bb").
	% "/aa/bb/cc"

	AbsPath = ensure_path_is_absolute( Path ),

	AbsRefDir = ensure_path_is_absolute( RefDir ),

	%trace_utils:debug_fmt( "Making path '~ts' (absolute form: '~ts') relative "
	%   "to reference directory '~ts' (absolute form: '~ts').",
	%   [ Path, AbsPath, RefDir, AbsRefDir ] ),

	TargetPathElems = filename:split( AbsPath ),
	RefPathElems = filename:split( AbsRefDir ),

	% Requires both arguments to be plain strings:
	make_relative_plain( TargetPathElems, RefPathElems );


% At least one of them expected to be a binary, so switching to binary:
make_relative( Path, RefDir ) ->

	BinAbsPath = ensure_path_is_absolute( text_utils:ensure_binary( Path ) ),

	BinAbsRefDir =
		ensure_path_is_absolute( text_utils:ensure_binary( RefDir ) ),

	%trace_utils:debug_fmt( "Making path '~ts' (absolute form: '~ts') relative "
	%   "to reference directory '~ts' (absolute form: '~ts').",
	%   [ Path, BinAbsPath, RefDir, BinAbsRefDir ] ),

	TargetPathElems = filename:split( BinAbsPath ),
	RefPathElems = filename:split( BinAbsRefDir ),

	make_relative_binary( TargetPathElems, RefPathElems ).



% First, drop any common path prefix:
make_relative_plain( [ E | TPathElems ], [ E | TRefPathElems ] ) ->
	make_relative_plain( TPathElems, TRefPathElems );

% Found first non-matching directory element:
make_relative_plain( PathElems, RefPathElems ) ->

	%trace_utils:debug_fmt( "Paths split at: ~p vs ~p.",
	%                       [ PathElems, RefPathElems ] ),

	FromRef = [ ".." || _ <- lists:seq( 1, length( RefPathElems ) ) ],

	Res = join( FromRef ++ PathElems ),

	%trace_utils:debug_fmt( "Returned path: '~ts'.", [ Res ] ),

	Res.


% First, drop any common path prefix:
make_relative_binary( [ E | TPathElems ], [ E | TRefPathElems ] ) ->
	make_relative_binary( TPathElems, TRefPathElems );

% Found first non-matching directory element:
make_relative_binary( PathElems, RefPathElems ) ->

	%trace_utils:debug_fmt( "Paths split at: ~p vs ~p.",
	%                       [ PathElems, RefPathElems ] ),

	FromRef = [ <<"..">> || _ <- lists:seq( 1, length( RefPathElems ) ) ],

	Res = bin_join( FromRef ++ PathElems ),

	%trace_utils:debug_fmt( "Returned path: '~ts'.", [ Res ] ),

	Res.



-doc """
Returns a pair made of the longest path common to all specified directory paths,
and the corresponding suffixes, that is an (unordered) list of the input paths
(as binaries) once the common prefix elements have been removed.

Note: operates per-directory (as a whole), not per-character.

For example: `get_longest_common_path(["/tmp/aa/bb/c1/foobar.txt",
							 "/tmp/aa/bb/c2/foobar.txt"])` returns:
`{"/tmp/aa/bb", ["c1","foobar.txt"], ["c2","foobar.txt"]]}`.

Like `text_utils:get_longest_common_prefix/1`, except that operates on whole
path elements, not individual characters.
""".
-spec get_longest_common_path( [ any_path() ] ) ->
										{ any_path(), [ any_path() ] }.
get_longest_common_path( DirPaths ) ->

	%trace_utils:debug_fmt( "Getting longest common path for:~n~p",
	%                       [ DirPaths ] ),

	DirElems = [ filename:split( D ) || D <- DirPaths ],

	get_longest_common_path_helper( DirElems, _AccCommon=[] ).


% (helper)
get_longest_common_path_helper( DirElems, AccCommon ) ->

	%trace_utils:debug_fmt( "get_longest_common_path_helper from ~p "
	%                       "(acc being ~p).", [ DirElems, AccCommon ] ),

	case get_common_head_of( DirElems ) of

		{ none, Tails } ->
			%trace_utils:debug_fmt( "Finished, with common path ~ts and "
			%                       "tails: ~p.", [ AccCommon, Tails ] ),

			% As filename:join/1 requires a non-empty list:
			LongestPath = case AccCommon of

				[] ->
					<<"">>;

				_ ->
					filename:join( lists:reverse( AccCommon ) )

			end,

			JoinedTails = [ filename:join( T ) || T <- Tails ],

			{ LongestPath, JoinedTails };


		{ Elem, DirElemsTails } ->
			%trace_utils:debug_fmt( "Adding prefix '~w'.", [ Elem ] ),
			get_longest_common_path_helper( DirElemsTails,
											[ Elem | AccCommon ] )

	end.



% Returns {none, Tails} or {CommonElem, RemainingTails}.
%
% (sub-helper)
%
% Degenerate case where a first list does not exist:
get_common_head_of( _DirElemsTails=[] ) ->
	{ none, _Tails=[] };

% We use the head (if any) of the first list as the one to check at the level of
% the head of all others:
%
get_common_head_of( DirElemsTails=[ _First=[] | _Others ] ) ->
	% No head here for first list; common path ended:
	{ none, DirElemsTails };

get_common_head_of( DirElemsTails=[ _First=[ Elem | T ] | Others ] ) ->
	% See whether the non-first tails also start with Elem:
	case try_behead_with( Elem, Others ) of

		non_matching ->
			% Leave lists as they are:
			{ none, DirElemsTails };

		NewOthers ->
			% Do not drop the tail of the first element:
			{ Elem, [ T | NewOthers ] }

	end.


% (sub-sub-helper)
try_behead_with( Elem, Others ) ->
	%trace_utils:debug_fmt( "Beheading of ~p from '~ts'", [ Others, Elem ] ),
	try_behead_with( Elem, Others, _Acc=[] ).


% Others depleted, success:
try_behead_with( _Elem, _Others=[], Acc ) ->
	% Order does not matter, no need to reverse:
	Acc;

% A good Other:
try_behead_with( Elem, _Others=[ [ Elem | R ] | T ], Acc ) ->
	try_behead_with( Elem, T, [ R | Acc ] );

% A bad Other:
% Corresponds to: try_behead_with( Elem, Others=[ [ OtherElem | _R ] | _T ],
%                                  _Acc ) ->
% or to:          try_behead_with( Elem, Others=[ [] | _T ], _Acc ) ->
try_behead_with( _Elem, _Others, _Acc ) ->
	%trace_utils:debug_fmt( "'~ts' could not be removed from ~p",
	%                       [ Elem, Others ] ),
	non_matching.



-doc """
Returns a pair made of the shortest ending paths that allows to discriminate
between the specified paths (expected to be of the same string type).

For example: `get_shortest_unique_ending_paths("/aa/bb/foo/bar/hello.txt",
	"/tmp/buzz/frob/aa/foo/bar/hello.txt")` returns
`{"bb/foo/bar/hello.txt", "aa/foo/bar/hello.txt"}`.
""".
-spec get_shortest_unique_ending_paths( any_path(), any_path() ) ->
												{ any_path(), any_path() }.
get_shortest_unique_ending_paths( Path, Path ) ->
	throw( { same_path, Path } );

get_shortest_unique_ending_paths( FirstPath, SecondPath ) ->

	FirstElems = lists:reverse( filename:split( FirstPath ) ),
	SecondElems = lists:reverse( filename:split( SecondPath ) ),

	get_shorted_ending_helper( FirstElems, SecondElems, _Acc=[] ).


% Can newer happen by design (checked first to be different):
%get_shorted_ending_helper( _FirstElems=[], _SecondElems=[], Acc ) ->
get_shorted_ending_helper( _FirstElems=[], _SecondElems=[ S | _T ],
								Acc ) ->
	RevPath = join( Acc ),
	{ RevPath, join( S, RevPath ) };

get_shorted_ending_helper( _FirstElems=[ S | _T ], _SecondElems=[], Acc ) ->
	RevPath = join( Acc ),
	{ join( S, RevPath ), RevPath };

% Same element:
get_shorted_ending_helper( _FirstElems=[ H | TF ], _SecondElems=[ H | TS ],
						   Acc ) ->
	get_shorted_ending_helper( TF, TS, [ H | Acc ] );

% Different:
get_shorted_ending_helper( _FirstElems=[ HF | _TF ], _SecondElems=[ HS | _TS ],
						   Acc ) ->
	RevPath = join( Acc ),
	{ join( HF, RevPath ),  join( HS, RevPath ) }.



-doc """
Tells whether specified basename (e.g. a pathless filename) is among the
specified list of full paths; returns either false or the first full path found
corresponding to that leaf element.

For example:
```
  false = file_utils:is_leaf_among( "xx", [ "a/b/c/yy", "d/e/zz"] ),
  "a/b/c/xx"  = file_utils:is_leaf_among( "xx", [ "a/b/c/xx", "d/e/zz"] )
```
""".
-spec is_leaf_among( leaf_name(), [ path() ] ) -> 'false' | path().
is_leaf_among( _LeafName, _PathList=[] ) ->
	false;

is_leaf_among( LeafName, _PathList=[ Path | T ] ) ->

	case filename:basename( Path ) of

		LeafName ->
			Path;

		_ ->
			is_leaf_among( LeafName, T )

	end.



-doc """
Updates the specified file with the specified keywords, that is copies the
original file into a target, updated one (supposedly non-already existing), in
which all the specified keywords (the keys of the translation table) have been
replaced by their associated value (that is the value in table corresponding to
that key).

For example: `file_utils:update_with_keywords("original.txt", "updated.txt",
   table:new([{"hello", "goodbye"}, {"Blue", "Red"}])).`.

The resulting file will be written with no additional encoding options.

In-place update can be done (by specifying the same file).
""".
-spec update_with_keywords( any_file_path(), any_file_path(),
							text_utils:translation_table() ) -> void().
update_with_keywords( OriginalFilePath, TargetFilePath, TranslationTable ) ->
	update_with_keywords( OriginalFilePath, TargetFilePath, TranslationTable,
						  _EncodingOpts=[] ).



-doc """
Updates the specified file with the specified keywords, that is copies the
original file into a target, updated one (supposedly non-already existing), in
which all the specified keywords (the keys of the translation table) have been
replaced by their associated value (that is the value in table corresponding to
that key).

For example: `file_utils:update_with_keywords("original.txt", "updated.txt",
	table:new([{"hello", "goodbye"}, {"Blue", "Red"}]), []).`.
""".
-spec update_with_keywords( any_file_path(), any_file_path(),
		text_utils:translation_table(), system_utils:encoding_options() ) ->
									void().
update_with_keywords( OriginalFilePath, TargetFilePath, TranslationTable,
					  EncodingOpts ) ->

	exists( TargetFilePath )
		andalso throw( { already_existing_target_file, TargetFilePath } ),

	BinOrigContent = read_whole( OriginalFilePath ),

	BinUpdatedContent = text_utils:update_with_keywords( BinOrigContent,
														 TranslationTable ),

	%trace_utils:debug_fmt( "Original content: ~ts;~n Translation table: ~p;~n"
	%   " Updated content: ~ts.",
	%   [ BinOrigContent, TranslationTable, BinUpdatedContent ] ),

	write_whole( TargetFilePath, BinUpdatedContent, EncodingOpts ).



-doc """
Converts the specified path (full filename, like `/home/jack/test.txt` or
`./media/test.txt`) into a variable name licit in most programming languages
(e.g. C/C++).

Rule here is:
- variable name starts with a prefix, user-supplied or the default one
- any leading `./` is removed
- `-` becomes `_`
- `.` becomes `_`
- `/` becomes `_`
""".
-spec path_to_variable_name( path() ) -> ustring().
path_to_variable_name( Filename ) ->
	path_to_variable_name( Filename, "File_" ).



-doc """
Converts the specified path (full filename, like `/home/jack/test.txt` or
`./media/test.txt`) into a variable name licit in most programming languages
(e.g. C/C++), based on the specified prefix.
""".
% Removes any leading './'.
-spec path_to_variable_name( path(), ustring() ) -> ustring().
path_to_variable_name( [ $.,$/ | T ], Prefix ) ->
	convert( T, Prefix );

path_to_variable_name( Filename, Prefix ) ->
	convert( Filename, Prefix ).


% (helper)
convert( Filename, Prefix ) ->

	NoDashName = re:replace( lists:flatten( Filename ), "-+", "_",
		[ global, { return, list } ] ),

	NoDotName = re:replace( NoDashName, "\\.+", "_",
		[ global, { return, list } ] ),

	Prefix ++ re:replace( NoDotName, "/+", "_",
		[ global, { return, list } ] ).



-doc """
Removes all upper levels of a path (absolute or not), as well as the extension
of the resulting file name.

For example: `"foobar" =
	file_utils:remove_upper_levels_and_extension("aa/bb/foobar.txt").`.
""".
remove_upper_levels_and_extension( FilePath ) ->

	PathLevels = filename:split( FilePath ),

	FileName = lists:last( PathLevels ),

	case string:rchr( FileName, $. ) of

		0 ->
			FileName;

		Index ->
			string:sub_string( FileName, 1, Index-1 )

	end.



-doc """
Returns a list of the known file extensions that refer to image files.
""".
-spec get_image_extensions() -> [ dotted_extension() ].
get_image_extensions() ->
	% TIFF, TGA and al deemed deprecated:
	[ ".png", ".jpg", ".jpeg", ".bmp", ".webp" ].



-define( ResourceDir, "resources" ).


-doc "Returns the image path corresponding to the specified file.".
-spec get_image_file_png( file_name() ) -> path().
get_image_file_png( Image ) ->
	filename:join( [ ?ResourceDir, "images", Image ++ ".png"] ).



-doc "Returns the image path corresponding to the specified file.".
-spec get_image_file_gif( file_name() ) -> path().
get_image_file_gif( Image ) ->
	filename:join( [ ?ResourceDir, "images", Image ++ ".gif"] ).



% Section for OS / project / application specific paths.
%
% See also: app_facilities:get_app_info/3 to obtain a relevant app_info record.



-doc """
Returns the directory path location intended for the storage of transient data
files that the specified application may perform on the local machine, that is
any cache that it may use.
""".
-spec get_cache_directory( any_app_info() ) -> directory_path().
get_cache_directory( AppInfo=#app_info{} ) ->
	AppInfoMap = app_facilities:get_app_info_map( AppInfo ),
	get_cache_directory( AppInfoMap );

get_cache_directory( AppInfoMap=#{ name := BinAppName } ) ->
	filename:basedir( _PathType=user_cache, BinAppName, _Opts=AppInfoMap ).



-doc """
Returns the main directory path location intended for the storage of persistent
user-level configuration files that the specified application may perform on the
local machine.

Does not check whether this directory exists, and of course a configuration file
of interest may or may not be available there.

May return for example `"~/.config/foobar/0.0.1"`.
""".
-spec get_configuration_directory( any_app_info() ) -> directory_path().
get_configuration_directory( AppInfo=#app_info{} ) ->
	AppInfoMap = app_facilities:get_app_info_map( AppInfo ),
	get_configuration_directory( AppInfoMap );

get_configuration_directory( AppInfoMap=#{ name := BinAppName } ) ->
	filename:basedir( _PathType=user_config, BinAppName, _Opts=AppInfoMap ).



-doc """
Returns the best, existing directory path intended for the storage of persistent
user-level configuration files that the specified application may perform on the
local machine: looks up in turn the candidate directories, by decreasing
priority order, and returns the relevant, most suitable one (if any).

Of course a configuration file of interest may or may not be available there,
which may limit the interest of this function; see then
`preferences:get_most_suitable_configuration_file/1`.

May return for example `"~/.config/foobar/0.0.1"` (if existing), otherwise
`"~/.config/foobar"` (if existing), otherwise `undefined`.
""".
-spec get_most_suitable_configuration_directory( any_app_info() ) ->
											option( directory_path() ).
get_most_suitable_configuration_directory( AppInfo=#app_info{} ) ->
	AppInfoMap = app_facilities:get_app_info_map( AppInfo ),
	get_configuration_directory( AppInfoMap );

get_most_suitable_configuration_directory(
		AppInfoMap=#{ name := BinAppName, version := _VersionStr } ) ->

	FirstCandidateDir =
		filename:basedir( _PathType=user_config, BinAppName, _Opts=AppInfoMap ),

	case file_utils:is_existing_directory_or_link( FirstCandidateDir ) of

		true ->
			FirstCandidateDir;

		false ->
			% Then trying without the version information:
			NoVersionMap = maps:remove( version, AppInfoMap ),
			get_most_suitable_configuration_directory( NoVersionMap )

	end;

get_most_suitable_configuration_directory(
		AppInfoMap=#{ name := BinAppName } ) ->
	CandidateDir =
		filename:basedir( _PathType=user_config, BinAppName, _Opts=AppInfoMap ),

	case file_utils:is_existing_directory_or_link( CandidateDir ) of

		true ->
			CandidateDir;

		false ->
			undefined

	end.



-doc """
Returns the extra path locations intended for the storage of persistent
configuration files that the specified application may perform on the local
machine.
""".
-spec get_extra_configuration_directories( any_app_info() ) ->
												[ directory_path() ].
get_extra_configuration_directories( AppInfo=#app_info{} ) ->
	AppInfoMap = app_facilities:get_app_info_map( AppInfo ),
	get_extra_configuration_directories( AppInfoMap );

get_extra_configuration_directories( AppInfoMap=#{ name := BinAppName } ) ->
	filename:basedir( _PathType=site_config, BinAppName, _Opts=AppInfoMap ).



-doc """
Returns the path location intended for the storage of persistent data files that
the specified application may perform on the local machine.
""".
-spec get_data_directory( any_app_info() ) -> directory_path().
get_data_directory( AppInfo=#app_info{} ) ->
	AppInfoMap = app_facilities:get_app_info_map( AppInfo ),
	get_data_directory( AppInfoMap );

get_data_directory( AppInfoMap=#{ name := BinAppName } ) ->
	filename:basedir( _PathType=user_data, BinAppName, _Opts=AppInfoMap ).



-doc """
Returns the extra path locations intended for the storage of persistent data
files that the specified application may perform on the local machine.
""".
-spec get_extra_data_directories( any_app_info() ) ->
												[ directory_path() ].
get_extra_data_directories( AppInfo=#app_info{} ) ->
	AppInfoMap = app_facilities:get_app_info_map( AppInfo ),
	get_extra_data_directories( AppInfoMap );

get_extra_data_directories( AppInfoMap=#{ name := BinAppName } ) ->
	filename:basedir( _PathType=site_data, BinAppName, _Opts=AppInfoMap ).



-doc """
Returns the path location intended for the storage of transient log files that
the specified application may perform on the local machine.
""".
-spec get_log_directory( any_app_info() ) -> directory_path().
get_log_directory( AppInfo=#app_info{} ) ->
	AppInfoMap = app_facilities:get_app_info_map( AppInfo ),
	get_log_directory( AppInfoMap );

get_log_directory( AppInfoMap=#{ name := BinAppName } ) ->
	filename:basedir( _PathType=user_log, BinAppName, _Opts=AppInfoMap ).



% I/O section.


-doc """
Returns the default recommended encoding, for example when needing to open a
file for writing.

See the notes above in the `Regarding encodings and Unicode` section, notably
about the consequences of specifying an encoding at file opening (generally
directly writing encoded content is safer and offers more control).
""".
-spec get_default_encoding() -> system_utils:encoding().
get_default_encoding() ->
	system_utils:get_default_encoding().


-doc """
Returns the default recommended option encoding option, for example when needing
to open a file for writing - should such an option be used.

See the notes above in the `Regarding encodings and Unicode` section, notably
about the consequences of specifying an encoding at file opening (generally
directly writing encoded content is safer and offers more control).
""".
-spec get_default_encoding_option() -> system_utils:encoding_option().
get_default_encoding_option() ->
	system_utils:get_default_encoding_option().



-doc """
Converts in-place the specified file, whose current encoding is expected to be
Latin1, to Unicode.
""".
-spec latin1_file_to_unicode( any_file_path() ) -> void().
latin1_file_to_unicode( AnyFilePath ) ->

	{ ok, Latin1Content } = file:read_file( AnyFilePath ),

	Utf8Content = unicode:characters_to_binary( Latin1Content, _From=latin1,
												_To=utf8 ),

	ok = file:write_file( AnyFilePath, Utf8Content ).



-doc """
Opens the file corresponding to the specified path, with the specified list of
options (as listed for `file:open/2` in [this
section](http://erlang.org/doc/man/file.html#open-2), that is: read, write,
append, exclusive, raw, etc).

See `read_etf_file/1` if planning to read that content as terms later, notably
with regard to encoding.

Returns the file reference, or throws an exception.

Will attempt to open the specified file only once, as looping endlessly does not
seem a viable solution right now (risk of exhausting the descriptors, making the
VM fail for example when loading a new BEAM).

As soon as a file is opened for writing, a corresponding empty file appears in
the filesystem.

For all questions in link with the Unicode support or the use of the `raw`
option, read the `Regarding encodings and Unicode` section at the top of this
file.
""".
-spec open( any_file_path(), [ file_open_mode() ] ) -> file().
open( AnyFilePath, Options ) ->
	open( AnyFilePath, Options, _Default=try_once ).



-doc """
Opens the file corresponding to the specified path (first parameter) with the
specified list of options (second parameter; refer to `file:open/2` for detailed
documentation, see [http://erlang.org/doc/man/file.html#open-2]).

Third parameter is the "attempt mode", either `try_once`, `try_endlessly` or
`try_endlessly_safer`, depending respectively on whether we want to try to open
the file once (no other attempt will be made), endlessly (until a file
descriptor can be gained), possibly with a safer setting.

Returns the file reference, or throws an exception.

Will try to obtain a file descriptor iteratively (and endlessly) with
process-specific random waitings, should no descriptor be available.

A risk of that approach is that all available file descriptors will be taken,
thus potentially preventing other processes (including the VM itself) to perform
any file operation, like loading a new BEAM, e.g.
```
File operation error: system_limit. Target:
lib/erlang/lib/kernel-x.y.z/ebin/timer.beam. Function: get_file.
Process: code_server.
```

This is done in order to support situations where potentially more Erlang
processes than available file descriptors try to access to files. An effort is
made to desynchronize these processes to smooth the use of descriptors.

For all questions in link with the Unicode support or the use of the `raw`
option, read the `Regarding encodings and Unicode` section at the top of this
file.
""".
-spec open( any_file_path(), [ file_open_mode() ],
			'try_once' | 'try_endlessly' | 'try_endlessly_safer' ) -> file().
open( AnyFilePath, Options, _AttemptMode=try_endlessly_safer ) ->

	%trace_utils:debug_fmt( "Opening '~ts' endlessly yet safe, "
	%   "with options ~w.", [ Filename, Options ] ),

	File = open( AnyFilePath, Options, try_endlessly ),

	% We could check here that at least one descriptor remains, by adding a
	% dummy file open/close and catching emfile, however one could need more
	% than one spare descriptor.
	%
	% The correct solution would involve knowing the max number of descriptors
	% for that process and the current number of open ones, no information we
	% seems able to know.
	%
	% So for the moment we do not do anything more than 'try_endlessly':
	File;


open( AnyFilePath, Options, _AttemptMode=try_endlessly ) ->

	%trace_utils:debug_fmt( "Opening '~ts' endlessly, with options ~w.",
	%                       [ AnyFilePath, Options ] ),

	case file:open( AnyFilePath, Options ) of

		{ ok, File } ->
			File;

		{ error, FileError } when FileError == emfile
				orelse FileError == system_limit ->

			% File descriptors exhausted for this OS process.
			% Kludge to desynchronize file opening in order to remain below 1024
			% file descriptor opened:
			%
			Duration =
				basic_utils:get_process_specific_value( _Min=50, _Max=200 ),

			% Attempt not to use timer:sleep (anyway will trigger errors
			% afterwards when the system will try to look-up some BEAMs):
			%
			receive

			after Duration ->

				open( AnyFilePath, Options, try_endlessly )

			end;

		{ error, eacces } ->
			throw( { open_failed,
					 { text_utils:ensure_string( AnyFilePath ), Options },
					 access_denied,
					 get_file_access_denied_info( AnyFilePath ) } );

		{ error, OtherFileError } ->
			throw( { open_failed,
					 { text_utils:ensure_string( AnyFilePath ), Options },
					 OtherFileError } )

	end;


% By far the most commonly-used clause:
open( AnyFilePath, Options, _AttemptMode=try_once ) ->

	%trace_utils:debug_fmt( "Opening '~ts' once, with the ~w options, "
	%   "from '~ts'.", [ AnyFilePath, Options, get_current_directory() ] ),

	case file:open( AnyFilePath, Options ) of

		{ ok, File } ->
			 File;

		{ error, eacces } ->
			throw( { open_failed,
					 { text_utils:ensure_string( AnyFilePath ), Options },
					 access_denied,
					 get_file_access_denied_info( AnyFilePath ) } );

		{ error, emfile } ->
			throw( { too_many_open_files,
					 { text_utils:ensure_string( AnyFilePath ), Options } } );

		{ error, system_limit } ->
			% Never had system_limit without this cause (yet!):
			throw( { too_many_open_files,
					 { text_utils:ensure_string( AnyFilePath ), Options },
					 system_limit } );

		{ error, OtherError } ->
			throw( { open_failed,
					 { text_utils:ensure_string( AnyFilePath ), Options },
					 OtherError } )

	end.



-doc """
Returns detailed information relative to an access denied error obtained for the
specified filesystem element.
""".
-spec get_element_access_denied_info( any_path() ) -> term().
get_element_access_denied_info( AnyElemPath ) ->

	ParentDir = filename:dirname( AnyElemPath ),

	case is_existing_directory( ParentDir ) of

		true ->
			ElemInfo = case exists( AnyElemPath ) of

				true ->
					{ target_element_exists,
                      { owner, describe_owner_of( AnyElemPath ) },
					  { group, describe_group_of( AnyElemPath ) },
					  { permissions, describe_permissions_of( AnyElemPath ) } };

				false ->
					target_element_does_not_exist

			end,

            % At least generally, 0 is root:
			ParentDirOwnerInfo = { owner, describe_owner_of( ParentDir ) },
			ParentDirGroupInfo = { group, describe_group_of( ParentDir ) },
			ParentDirPerms = { permissions,
                               describe_permissions_of( ParentDir ) },

			ParentDirInfo = { parent_directory_exists, ParentDir,
                ParentDirOwnerInfo, ParentDirGroupInfo, ParentDirPerms },

			{ ElemInfo, ParentDirInfo, get_runtime_user_info() };

		false ->
			{ parent_directory_does_not_exist, ParentDir }

	end.


-doc """
Returns detailed information relative to an access denied error obtained for the
specified file.
""".
-spec get_file_access_denied_info( any_file_path() ) -> term().
get_file_access_denied_info( AnyFilePath ) ->

	ParentDir = filename:dirname( AnyFilePath ),

	case is_existing_directory( ParentDir ) of

		true ->
			FileInfo = case is_existing_file_or_link( AnyFilePath ) of

				true ->
					{ target_file_exists,
                      { owner, describe_owner_of( AnyFilePath ) },
					  { group, describe_group_of( AnyFilePath ) },
					  { permissions, describe_permissions_of( AnyFilePath ) } };

				false ->
					target_file_does_not_exist

			end,

            % At least generally, 0 is root:
			ParentDirOwnerInfo = { owner, describe_owner_of( ParentDir ) },
			ParentDirGroupInfo = { group, describe_group_of( ParentDir ) },
			ParentDirPerms = { permissions,
                               describe_permissions_of( ParentDir ) },

			ParentDirInfo = { parent_directory_exists, ParentDir,
                ParentDirOwnerInfo, ParentDirGroupInfo, ParentDirPerms },

			{ FileInfo, ParentDirInfo, get_runtime_user_info() };

		false ->
			{ parent_directory_does_not_exist, ParentDir }

	end.


-doc """
Returns detailed information relative to an access denied error obtained for the
specified directory.
""".
-spec get_directory_access_denied_info( any_directory_path() ) -> term().
get_directory_access_denied_info( AnyDirPath ) ->

	ParentDir = filename:dirname( AnyDirPath ),

	case is_existing_directory( ParentDir ) of

		true ->
			DirInfo = case is_existing_directory_or_link( AnyDirPath ) of

				true ->
					{ target_directory_exists,
					  { owner, describe_owner_of( AnyDirPath ) },
					  { group, describe_group_of( AnyDirPath ) },
					  { permissions, describe_permissions_of( AnyDirPath ) } };

				false ->
					target_directory_does_not_exist

			end,

			ParenDirOwnerInfo = { owner, describe_owner_of( ParentDir ) },
			ParenDirGroupInfo = { group, describe_group_of( ParentDir ) },
			ParenDirPerms = { permissions,
                              describe_permissions_of( ParentDir ) },

			ParentDirInfo = { parent_directory_exists, ParentDir,
                              ParenDirOwnerInfo, ParenDirGroupInfo,
                              ParenDirPerms },

			{ DirInfo, ParentDirInfo, get_runtime_user_info() };

		false ->
			{ parent_directory_does_not_exist, ParentDir }

	end.



% (helper)
get_runtime_user_info() ->
    [ { actual_runtime_user, system_utils:get_user_name_safe(),
        { user_id, system_utils:get_user_id() } },
      { actual_runtime_group,
        system_utils:get_group_name_safe(),
        { group_id, system_utils:get_group_id() } } ].



-doc """
Opens for a creation from scratch the specified file with the specified options
(the `write` one being implied and automatically added here).

If the target file already exists, renames it first by suffixing `.previous` to
its name (then overwriting any identically-named file that would already exist),
before performing the creation.
""".
-spec create_preserving( any_file_path(), [ file_open_mode() ] ) -> file().
create_preserving( AnyFilePath, Options ) ->
	create_preserving( AnyFilePath, _HidingSuffix=?default_hiding_suffix,
					   Options ).



-doc """
Opens for a creation from scratch the specified file with the specified options
(the `write` one being implied and automatically added here).

If the target file already exists, renames it first based on the specified
suffix (then overwriting any identically-named file that would already exist),
before performing the creation.
""".
-spec create_preserving( any_file_path(), ustring(), [ file_open_mode() ] ) ->
							file().
create_preserving( AnyFilePath, HidingSuffix, Options ) ->

	is_existing_file_or_link( AnyFilePath ) andalso
		hide_overwriting( AnyFilePath, HidingSuffix ),

	open( AnyFilePath, [ write | Options ] ).



-doc """
Closes the specified file reference.

Throws an exception on failure.
""".
-spec close( file() ) -> void().
close( File ) ->
	close( File, throw_if_failed ).



-doc """
Closes the specified file reference.

Throws an exception on failure or not, depending on specified failure mode.
""".
-spec close( file(), 'overcome_failure' | 'throw_if_failed' ) -> void().
close( File, _FailureMode=throw_if_failed ) ->

	case file:close( File ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { file_closing_failed, Reason, File } )

	end;

close( File, _FailureMode=overcome_failure ) ->
	file:close( File ).



-doc """
Reads the specified number of bytes/characters from the specified file.

Returns either `{ok, Data}` if at least some data could be read, or `eof` if at
least one element was to read and end of file was reached before anything at all
could be read.

Throws an exception on failure.
""".
-spec read( file(), count() ) -> { 'ok', ustring() | binary() } | 'eof'.
read( File, Count ) ->

	case file:read( File, Count ) of

		R={ ok, _Data } ->
			R;

		eof ->
			eof;

		{ error, Reason } ->
			throw( { read_failed, Reason } )

	end.



-doc """
Writes the specified byte-oriented content in the specified file.

Operates on files opened in raw mode (only way to do so), or not (works for
normal mode as well).

Throws an exception on failure.

See `write_ustring/{2,3}` to write Unicode text.
""".
-spec write( file(), iodata() ) -> void().
write( File, Content ) ->

	%trace_utils:debug_fmt( "Writing '~w' to ~p.", [ Content, File ] ),

	case file:write( File, Content ) of

		ok ->
			ok;

		% If Reason is badarg, possibly an encoding issue (e.g. having used
		% '~ts' instead of '~ts'):
		%
		{ error, Reason } ->
			throw( { write_failed, Reason, Content, File } )

	end.



-doc """
Writes the specified Unicode string in the specified file.

Operates on files opened in raw mode (only way to do so), or not (works for
normal mode as well).

Note that no control character (even no `~n`, for newlines) must exist in the
specified string, otherwise they will be written literally. To convert them,
use: `write_ustring(File, Str, _FormatValues=[])`.

Throws an exception on failure.
""".
-spec write_ustring( file(), ustring() ) -> void().
write_ustring( File, Str ) ->

	%trace_utils:debug_fmt( "Writing '~ts' to ~p.", [ Str, File ] ),

	cond_utils:assert( myriad_check_files, is_file_reference( File ) ),

	Bin = text_utils:to_unicode_binary( Str ),
	%trace_utils:debug_fmt( " - Bin: ~p.", [ Bin ] ),

	%BinStr = io_lib:format( "~ts", [ Bin ] ),
	%trace_utils:debug_fmt( " - BinStr: ~p.", [ BinStr ] ),

	% Using current encoding (i.e. the one that file was opened with):
	case file:write( File, Bin ) of

		ok ->
			ok;

		% If Reason is badarg, possibly an encoding issue (for example if having
		% used '~s' instead of '~ts'):
		%
		{ error, Reason } ->
			throw( { write_ustring_failed, Reason, Str, File } )

	end.



-doc """
Writes the specified formatted content in the specified file.

Throws an exception on failure.
""".
-spec write_ustring( file(), format_string(), format_values() ) -> void().
write_ustring( File, FormatString, FormatValues ) ->
	Text = text_utils:format( FormatString, FormatValues ),
	write_ustring( File, Text ).



-doc """
Reads the content of the specified file, based on its filename specified as any
kind of string (plain, binary, atom, etc.), and returns the corresponding
binary, or throws an exception on failure.

See also: `read_etf_file/1` to read directly Erlang terms instead.
""".
-spec read_whole( any_file_path() ) -> binary().
read_whole( FilePath ) ->

	%trace_utils:debug_fmt( "Reading as a whole '~ts'.", [ FilePath ] ),

	case file:read_file( FilePath ) of

		{ ok, Binary } ->
			Binary;

		{ error, eacces } ->
			throw( { read_whole_failed,
					 text_utils:ensure_string( FilePath ), access_denied,
					 get_file_access_denied_info( FilePath ) } );

		{ error, Error } ->
			throw( { read_whole_failed, FilePath, Error } )

	end.



-doc """
Reads the content of the specified file, expected to be a text one, based on its
filename specified as any kind of string (plain, binary, atom, etc.) and returns
its content as a list of plain strings, or throws an exception on failure.

Each returned line has any (trailing) newline(s) removed (knowing that the last
one may or may not have a newline). See
[https://erlang.org/doc/man/file.html#read_line-1] for more details regarding
end-of-line characters.
""".
-spec read_lines( any_file_path() ) -> [ ustring() ].
read_lines( FilePath ) ->

	%trace_utils:debug_fmt( "Reading all lines from '~ts'.", [ FilePath ] ),

	Modes = [ read, raw, { read_ahead, ?default_read_ahead_size } ],

	File = case file:open( FilePath, Modes ) of

		{ ok, F } ->
			F;

		{ error, eacces } ->
			throw( { read_lines_failed, text_utils:ensure_string( FilePath ),
					 access_denied,
					 get_file_access_denied_info( FilePath ) } );

		{ error, Error } ->
			throw( { read_lines_failed, FilePath, opening, Error } )

	end,

	read_lines( File, FilePath, _Acc=[] ).



% (helper)
read_lines( File, FilePath, Acc ) ->
	case file:read_line( File ) of

		{ ok, Line } ->
			% If any, are removed:
			CleanedLine = text_utils:remove_ending_carriage_return( Line ),
			read_lines( File, FilePath, [ CleanedLine | Acc ] );

		eof ->
			file:close( File ),
			lists:reverse( Acc );

		{ error, Error } ->
			% No 'file:close( File )'?
			throw( { read_lines_failed, FilePath, reading, Error } )

	end.



-doc """
Writes the specified content in the specified file, whose path is specified as
any kind of string, using a default encoding if a plain string is specified.

Note that specifying a binary allows to avoid any potential unwanted encoding.

Any already-existing file at that path will be silently overwritten.

Throws an exception on failure.
""".
-spec write_whole( any_file_path(), ustring() | binary() ) -> void().
write_whole( AnyFilePath, Content ) ->
	write_whole( AnyFilePath, Content, _Modes=[] ).



-doc """
Writes the specified content in the file whose path is specified as any kind of
string, using the specified modes options, and applying before a default
encoding if a plain string is specified.

Note that no transparent encoding-to-file is thus expected to be specified
through modes, as this function already performs (through
`text_utils:string_to_binary/1`) such encoding on plain strings (otherwise this
would result in a double encoding); specifying a binary allows to avoid any
potential unwanted encoding.

Any already-existing file at that path will be silently overwritten.

Throws an exception on failure.
""".
-spec write_whole( any_file_path(), ustring() | binary(), [ file:mode() ] ) ->
														void().
write_whole( AnyFilePath, StringContent, Modes )
								when is_list( StringContent ) ->

	% Warning, implies performing an encoding (typically based on
	% unicode:characters_to_binary/1):
	%
	write_whole( AnyFilePath, text_utils:string_to_binary( StringContent ),
				 Modes );

write_whole( AnyFilePath, BinaryContent, Modes ) ->

	%trace_utils:debug_fmt( "Writing to '~ts', with modes ~p, "
	%   "following content:~n~ts", [ , Modes, BinaryContent ] ),

	% 'write' and 'binary' are implicit here; if relevant BinaryContent must be
	% correctly Unicode-encoded:
	%
	case file:write_file( AnyFilePath, BinaryContent, Modes ) of

		ok ->
			% Useless, paranoid checking:
			%case is_existing_file( AnyFilePath ) of
			%
			%   true ->
			%       trace_utils:debug_fmt( "'~ts' written as a whole.",
			%                              [ AnyFilePath ] ),
			%       ok;
			%
			%   false ->
			%       throw( { write_whole_failed,
			%            text_utils:ensure_string( AnyFilePath ), no_file } )
			%
			%end;
			ok;

		{ error, eacces } ->
			throw( { write_whole_failed,
						{ text_utils:ensure_string( AnyFilePath ), Modes },
						access_denied,
						get_file_access_denied_info( AnyFilePath ) } );

		{ error, Error } ->
			throw( { write_whole_failed,
						{ text_utils:ensure_string( AnyFilePath ), Modes },
						Error } )

	end.



-doc """
Writes the specified content in a new file, whose path is chosen not to clash
with any other (typically a temporary file), and returns that path.

Throws an exception on failure.
""".
-spec write_whole_in_non_clashing( ustring() | binary() ) -> file_path().
write_whole_in_non_clashing( Content ) ->
	FilePath = create_non_clashing_file(),
	write_whole( FilePath, Content ),
	FilePath.



-doc """
Reads the specified file, which is supposedly in ETF format (*Erlang Term
Format*): tries to parse a list of terms (one per line, terminating with a dot)
from it (as `file:consult/1` does), and returns it. Lines starting with `%` are
ignored (just considered as comments).

If expecting to read UTF-8 content from a file, it should:

- have been then opened for writing typically while including the {encoding,
 utf8} option, or have been written with content already properly encoded (it
 may be more reliable that way)

- start with a `%% -*- coding: utf-8 -*-` header

See [this section](http://myriad.esperide.org/#etf) for more details.

Throws an exception on error.
""".
-spec read_etf_file( any_file_path() ) -> [ term() ].
read_etf_file( AnyFilePath ) ->
	read_terms( AnyFilePath ).



-doc """
Reads the specified file supposedly in ETF format (*Erlang Term Format*): tries
to parse a list of terms (one per line, terminating with a dot) from it (as
`file:consult/1` does), and returns it. Lines starting with `%` are ignored
(just considered as comments).

If expecting to read UTF-8 content from a file, it should:

- have been then opened for writing typically while including the `{encoding,
 utf8}` option, or have been written with content already properly encoded (it
 may be more reliable that way)

- start with a `%% -*- coding: utf-8 -*-` header

See [this section](http://myriad.esperide.org/#etf) for more details.

Throws an exception on error.
""".
-spec read_terms( any_file_path() ) -> [ term() ].
read_terms( AnyFilePath ) ->

	case file:consult( AnyFilePath ) of

		{ ok, Terms } ->
			Terms;

		{ error, eacces }  ->
			throw( { etf_reading_failed,
                     text_utils:ensure_string( AnyFilePath ),
					 { reason, access_denied },
                     get_file_access_denied_info( AnyFilePath ) } );

		{ error, { _, file_io_server, invalid_unicode } } ->
			% See also latin1_file_to_unicode/1:
			throw( { etf_reading_failed,
                     text_utils:ensure_string( AnyFilePath ),
                     { reason, not_unicode } } );

		{ error, Error } when is_atom( Error ) ->
			throw( { etf_reading_failed,
                     text_utils:ensure_string( AnyFilePath ),
                     { reason, Error } } );

		{ error, Error={ Line, Module, Term } } ->
			Reason = file:format_error( Error ),
			throw( { etf_interpretation_failed,
					 text_utils:ensure_string( AnyFilePath ),
                     { reason, Reason }, { line, Line },
					 { module, Module },
                     { raw_term, Term } } )

	end.



-doc """
Writes the specified terms in the specified file, in the ETF format, with no
specific header or footer.

See [http://myriad.esperide.org/#etf] for more details.

Heavily inspired from Joe Armstrong's `lib_misc:unconsult/2`.
""".
-spec write_etf_file( [ term() ], any_file_path() ) -> void().
write_etf_file( Terms, AnyFilePath ) ->
	write_terms( Terms, AnyFilePath ).



-doc """
Writes the specified terms in the specified file, in the ETF format, with no
specific header or footer.

Refer to `write_etf_file/2` for more details.
""".
-spec write_terms( [ term() ], any_file_path() ) -> void().
write_terms( Terms, AnyFilePath ) ->
	write_terms( Terms, _Header=undefined, _Footer=undefined, AnyFilePath ).



-doc """
Writes the specified terms in the specified file, in the ETF format, with the
specified header and footer.

Refer to `write_etf_file/2` for more details.
""".
-spec write_etf_file( [ term() ], option( ustring() ), option( ustring() ),
					  file_path() ) -> void().
write_etf_file( Terms, Header, Footer, Filename ) ->
	write_terms( Terms, Header, Footer, Filename ).



-doc """
Writes the specified terms in the specified file, in the ETF format, with the
specified header and footer.

Refer to `write_etf_file/2` for more details.
""".
-spec write_terms( [ term() ], option( ustring() ), option( ustring() ),
				   any_file_path() ) -> void().
write_terms( Terms, Header, Footer, AnyFilePath ) ->

	F = open( AnyFilePath, _Opts=[ write, raw, delayed_write ] ),

	Header =:= undefined orelse write_ustring( F, "% ~ts~n~n~n", [ Header ] ),

	write_direct_terms( F, Terms ),

	Footer =:= undefined orelse write_ustring( F, "~n~n% ~ts~n", [ Footer ] ),

	close( F ).



-doc """
Writes directly the specified terms int the specified already opened file, in
the ETF format.

Refer to `write_etf_file/2` for more details.
""".
-spec write_direct_terms( file(), [ term() ] ) -> void().
write_direct_terms( File, Terms ) ->
	%trace_utils:debug_fmt( "Writing direct terms ~p.", [ Terms ] ),
	[ write_ustring( File, "~p.~n", [ T ] ) || T <- Terms ].



-doc """
Tells whether the specified term is a file reference, i.e. a file object
(pseudo-guard).

Not to be confused with `is_file/1`, which is about file paths.
""".
-spec is_file_reference( term() ) -> boolean().
is_file_reference( { file_descriptor, _Mode, _BufferMap } ) ->
	true;

is_file_reference( FilePid ) when is_pid( FilePid ) ->
	true;

is_file_reference( _Other ) ->
	false.



% Compression-related operations.


-doc """
Returns the file extension corresponding to filenames compressed with specified
format.
""".
-spec get_extension_for( compression_format() ) -> extension().
get_extension_for( _CompressionFormat=zip ) ->
	"zip";

get_extension_for( _CompressionFormat=bzip2 ) ->
	"bz2";

get_extension_for( _CompressionFormat=xz ) ->
	"xz".



-doc """
Returns the dotted file extension (e.g. `".xz"`, not just `"xz"`) corresponding
to filenames compressed with specified format.
""".
-spec get_dotted_extension_for( compression_format() ) -> dotted_extension().
get_dotted_extension_for( CompressionFormat ) ->
	[ $. | get_extension_for( CompressionFormat ) ].



-doc """
Compresses the specified file: creates a compressed version thereof (using the
most efficient, compacity-wise, compression tool available), whose filename,
established based on usual conventions, is returned. If a file with that name
already exists, it will be overwritten.

For example, `compress("hello.png")` will generate a `"hello.png.xz"` file.

The original file remain as is.

Note: this function just takes care of compressing a single file, even if some
compressors (e.g. zip) include features to create an archive of multiple files
first.
""".
-spec compress( file_name() ) -> file_name().
compress( Filename ) ->
	compress( Filename, _CompressionFormat=xz ).



-doc """
Compresses the specified file: creates a compressed version thereof, whose
filename, established based on usual conventions, is returned. If a file with
that name already exists, it will be overwritten.

For example, `compress("hello.png", zip)` will generate a `"hello.png.zip"`
file.

The original file remain as is.

Note: this function just takes care of compressing a single file, even if some
compressors (e.g. zip) include features to create an archive of multiple files
first.
""".
-spec compress( file_name(), compression_format() ) -> file_name().
compress( Filename, _CompressionFormat=zip ) ->

	% Rather than using a standalone zip tool, we use the Erlang support here:

	%ZipExec = executable_utils:get_default_zip_compress_tool(),

	ZipFilename = Filename ++ get_dotted_extension_for( zip ),

	% Exactly this one file in the archive:
	%Command = ZipExec ++ " --quiet " ++ ZipFilename ++ " " ++ Filename,

	%[] = os:cmd( Command ),

	zip:zip( ZipFilename, [ Filename ] ),

	% Check:
	true = is_existing_file( ZipFilename ),

	ZipFilename;


compress( Filename, _CompressionFormat=bzip2 ) ->

	Bzip2Exec = executable_utils:get_default_bzip2_compress_tool(),

	% --keep allows to avoid that bzip2 removes the original file:
	case system_utils:run_command(
			Bzip2Exec ++ " --keep --force --quiet " ++ Filename ) of

		{ _ExitCode=0, _Output=[] } ->
			% Check:
			Bzip2Filename = Filename ++ get_dotted_extension_for( bzip2 ),
			true = is_existing_file( Bzip2Filename ),
			Bzip2Filename;

		{ _ExitCode=0, Output } ->
			throw( { bzip2_compress_failed, Filename, Output } );

		{ ExitCode, Output } ->
			throw( { bzip2_compress_failed, Filename, ExitCode, Output } )

	end;


compress( Filename, _CompressionFormat=xz ) ->

	XZExec = executable_utils:get_default_xz_compress_tool(),

	% --keep allows to avoid that bzip2 removes the original file:
	case system_utils:run_command(
			XZExec ++ " --keep --force --quiet " ++ Filename ) of

		{ _ExitCode=0, _Output=[] } ->
			% Check:
			XZFilename = Filename ++ get_dotted_extension_for( xz ),
			true = is_existing_file( XZFilename ),
			XZFilename;

		{ _ExitCode=0, Output } ->
			throw( { xz_compress_failed, Filename, Output } );

		{ ExitCode, Output } ->
			throw( { xz_compress_failed, Filename, ExitCode, Output } )

	end;

compress( _Filename, CompressionFormat ) ->
	throw( { unsupported_compression_format, CompressionFormat } ).



-doc """
Decompresses the specified compressed file, expected to bear the extension
corresponding to the implicit, most compact format: recreates the original,
decompressed version thereof, whose filename, established based on usual
conventions, is returned: the name of the input file without its extension.

This function works in pair with `compress/2`, and as such expects that each
compressed file contains exactly one file, bears the same filename except the
compressor extension.

Typically, when a format `MY_FORMAT` is specified, converts a compressed file
name `foo.extension_of(MY_FORMAT)` into an uncompressed version of it named
`foo`.

So, for example, `decompress("foo.xz")` will generate a `"foo"` file.

If a file with that name already exists, it will be overwritten.

The compressed file remains as is.
""".
-spec decompress( file_name() ) -> file_name().
decompress( Filename ) ->
	decompress( Filename, _CompressionFormat=xz ).



-doc """
Decompresses the specified compressed file, expected to bear the extension
corresponding to the specified format: recreates the original, decompressed
version thereof, whose filename, established based on usual conventions, is
returned: the name of the input file without its extension.

This function works in pair with `compress/2`, and as such expects that each
compressed file contains exactly one file, bear the same filename except the
compressor extension.

Typically, when a format `MY_FORMAT` is specified, converts a compressed file
name `foo.extension_of(MY_FORMAT)` into an uncompressed version of it named
`foo`.

So, for example, `decompress("foo.xz", xz)` will generate a `"foo"` file.

If a file with that name already exists, it will be overwritten.

The compressed file remains as is.
""".
-spec decompress( file_name(), compression_format() ) -> file_name().
decompress( ZipFilename, _CompressionFormat=zip ) ->

	% An annoying problem with zip is that the name of the (single) file in the
	% archive might differ from the filename deduced from the archive name (e.g.
	% "foo.zip" might contain "bar" instead of "foo"). We need to return "bar",
	% not "foo".

	% Rather than using a standalone zip tool, we use the Erlang support here:

	%UnzipExec = executable_utils:get_default_zip_decompress_tool(),

	% Checks and removes extension:
	%Filename = remove_extension( ZipFilename, get_extension_for( zip ) ),

	% Quiet, overwrite:
	%Command = UnzipExec ++ " -q -o " ++ ZipFilename,

	%[] = os:cmd( Command ),

	% Exactly one file per such archives:
	{ ok, [ Filename ] } = zip:unzip( ZipFilename ),

	% We expect here than only the compression feature (not the archive-making
	% feature) of zip has been used, as for all other compressors:
	%
	true = is_existing_file( Filename ),

	Filename;


decompress( Bzip2Filename, _CompressionFormat=bzip2 ) ->

	Bzip2Exec = executable_utils:get_default_bzip2_decompress_tool(),

	% Checks and removes extension:
	Filename = remove_extension( Bzip2Filename, get_extension_for( bzip2 ) ),

	% The result will be named Filename by bunzip2:

	case system_utils:run_command(
			Bzip2Exec ++ " --keep --force --quiet " ++ Bzip2Filename ) of

		{ _ExitCode=0, _Output=[] } ->
			% Check:
			Bzip2Filename = Filename ++ get_dotted_extension_for( bzip2 ),
			true = is_existing_file( Filename ),
			Filename;

		{ _ExitCode=0, Output } ->
			throw( { bzip2_decompress_failed, Filename, Output } );

		{ ExitCode, Output } ->
			throw( { bzip2_decompress_failed, Filename, ExitCode, Output } )

	end;


decompress( XzFilename, _CompressionFormat=xz ) ->

	XZExec = executable_utils:get_default_xz_decompress_tool(),

	% Checks and removes extension:
	Filename = remove_extension( XzFilename, get_extension_for( xz ) ),

	case system_utils:run_command(
			XZExec ++ " --keep --force --quiet " ++ XzFilename ) of

		{ _ExitCode=0, _Output=[] } ->
			% Check:
			true = is_existing_file( Filename ),
			Filename;

		{ _ExitCode=0, Output } ->
			throw( { xz_decompress_failed, Filename, Output } );

		{ ExitCode, Output } ->
			throw( { xz_decompress_failed, Filename, ExitCode, Output } )

	end;


decompress( _Filename, CompressionFormat ) ->
	throw( { unsupported_compression_format, CompressionFormat } ).



-doc """
Reads in memory the file specified from its filename, zips the corresponding
term, and returns it, as a compressed binary.

Note: useful for network transfers of small files.

Larger ones should be transferred with TCP/IP and by chunks.

Returns a binary.
""".
-spec file_to_zipped_term( file_name() ) -> binary().
file_to_zipped_term( Filename ) ->

	DummyFileName = "dummy",

	{ ok, { _DummyFileName, Bin } } =
		%zip:zip( DummyFileName, [ Filename ], [ verbose, memory ] ),
		zip:zip( DummyFileName, [ Filename ], [ memory ] ),

	Bin.



-doc """
Reads the specified binary, extracts the zipped file in it and writes it on
disk, in the current directory.

Returns the filename of the unzipped file.
""".
-spec zipped_term_to_unzipped_file( binary() ) -> file_name().
zipped_term_to_unzipped_file( ZippedTerm ) ->
	%zip:unzip( ZippedTerm, [ verbose ] ).
	{ ok, [ FileName ] } = zip:unzip( ZippedTerm ),
	FileName.



-doc """
Reads the specified binary, extracts the zipped file in it and writes it on
disk, in the current directory, under the specified filename instead of under
the filename stored in the zip archive.

Any pre-existing file will be overwritten.

Note: only one file is expected to be stored in the specified archive.
""".
-spec zipped_term_to_unzipped_file( binary(), file_name() ) -> void().
zipped_term_to_unzipped_file( ZippedTerm, TargetFilename ) ->

	{ ok, [ { _AFilename, Binary } ] } = zip:unzip( ZippedTerm, [ memory ] ),

	% { ok, File } = file:open( TargetFilename, [ write ] ),
	% ok = io:format( File, "~ts", [ binary_to_list(Binary) ] ),
	% ok = file:write_file( File, "~ts", [ binary_to_list(Binary) ] ),
	% ok = file:close( File ).
	write_whole( TargetFilename, Binary ).



-doc """
Reads in memory the files specified from their filenames (as plain strings),
zips the corresponding term, and returns it.

Note: useful for network transfers of small files. Larger ones should be
transferred with TCP/IP / send_file and by chunks.
""".
-spec files_to_zipped_term( [ file_name() ] ) -> binary().
files_to_zipped_term( Filenames ) ->

    %trace_utils:debug_fmt( "Selected filenames: ~p.", [ Filenames ] ),

	DummyFileName = "dummy",

	{ ok, { _DummyFileName, Bin } } =
		zip:zip( DummyFileName, Filenames, [ memory ] ),

	Bin.



-doc """
Reads in memory the files specified from their filenames (as plain strings),
assuming their path is relative to the specified base directory, zips the
corresponding term, and returns it.

Note: useful for network transfers of small files. Larger ones should be
transferred with TCP/IP / send_file and by chunks.
""".
-spec files_to_zipped_term( [ file_name() ], any_directory_path() ) -> binary().
files_to_zipped_term( Filenames, BaseDirectory ) ->

    trace_utils:debug_fmt( "Selected filenames (base directory: '~ts'): ~p.",
                           [ BaseDirectory, Filenames ] ),

	DummyFileName = "dummy",

	%trace_utils:notice_fmt( "files_to_zipped_term operating, from '~ts', "
	%   "on following ~B file(s): ~ts",
	%   [ BaseDirectory, length( Filenames ),
	%     text_utils:terms_to_string( Filenames ) ] ),

    BaseDirStr = text_utils:ensure_string( BaseDirectory ),

	 case zip:zip( DummyFileName, Filenames,
				   [ memory, { cwd, BaseDirStr } ] ) of

		{ ok, { _DummyFileName, Bin } } ->
			Bin;


		{ error, enoent } ->

			% Such a short error might be difficult to diagnose:

			%trace_utils:warning_fmt( "files_to_zipped_term/2 failed "
			%  "from '~ts':~n~n - directory '~p' exists? ~p",
			%      [ get_current_directory(), BaseDirectory,
			%        is_existing_directory( BaseDirectory ) ] ),

			% [ trace_utils:warning_fmt( "~n - file '~p' exists? ~p", [ F,
			%    is_existing_file( F ) ] ) || F <- Filenames ],

			throw( { zip_failed, BaseDirectory, Filenames } );

		% einval might mean for example that at least some filenames are
		% binaries rather that plain strings:
		%
		{ error, Other } ->
			throw( { zip_failed, Other, BaseDirectory, Filenames } )

	 end.



-doc """
Reads the specified binary, extracts the zipped files stored in it and writes
them on disk, in the current directory.

Returns the list of filenames corresponding to the unzipped files.
""".
-spec zipped_term_to_unzipped_files( binary() ) -> [ file_name() ].
zipped_term_to_unzipped_files( ZippedTerm ) ->
	%{ ok, FileNames } = zip:unzip( ZippedTerm, [ verbose ] ),
	{ ok, FileNames } = zip:unzip( ZippedTerm ),
	FileNames.



-doc """
Reads the specified binary, extracts the zipped files in it and writes them on
disk, in the specified directory.

Returns the list of filenames corresponding to the unzipped files.
""".
-spec zipped_term_to_unzipped_files( binary(), directory_name() ) ->
											[ file_name() ].
zipped_term_to_unzipped_files( ZippedTerm, TargetDirectory ) ->

	%{ ok, FileNames } = zip:unzip( ZippedTerm, [ verbose ] ),

	case is_existing_directory( TargetDirectory ) of

		true ->
			{ ok, FileNames } =
				zip:unzip( ZippedTerm, [ { cwd, TargetDirectory } ] ),
			FileNames;

		false ->
			throw( { non_existing_unzip_directory, TargetDirectory } )

	end.
