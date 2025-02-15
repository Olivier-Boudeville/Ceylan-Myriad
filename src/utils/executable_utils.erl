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

-module(executable_utils).

-moduledoc """
Gathering of various convenient facilities regarding the **execution of
third-party programs**.

See executable_utils_test.erl for the corresponding test, and cmd_line_utils.erl
for the management of the command lines.

See system_utils.erl for the actual execution of programs.
""".



% Section for the searching and checking of executables:
-export([ lookup_executable/1, lookup_executable/2, find_executable/1 ]).




% Section for most usual commands:
-export([ can_generate_png_from_graph/0, generate_png_from_graph_file/2,
		  generate_png_from_graph_file/3, display_png_file/1,
		  display_image_file/2, browse_images_in/1,
		  playback_audio_file/1, playback_audio_file/2,
		  display_pdf_file/1, display_text_file/1,
		  display_wide_text_file/2, get_ssh_mute_option/0 ]).


% Section about default tools:
-export([

	get_default_image_viewer_name/0,
	get_secondary_default_image_viewer_name/0,
	get_default_image_viewer_info/0,
	get_default_image_viewer_path/0,

	get_default_image_browser_name/0,
	get_default_image_browser_path/0,

	get_default_web_browser_name/0,
	get_default_web_browser_path/0,

	get_default_pdf_viewer_name/0,
	get_default_pdf_viewer_path/0,

	get_default_text_viewer_name/0,
	get_default_text_viewer_path/0,

	get_default_wide_text_viewer_name/1,
	get_default_wide_text_viewer_path/1,

	get_default_audio_player_name/0,
	get_secondary_default_audio_player_name/0,
	get_default_audio_player_info/0,
	get_default_audio_player_options/1,

	get_default_trace_viewer_name/0,
	get_default_trace_viewer_path/0,

	get_default_erlang_root/0,
	get_default_erlang_interpreter_name/0,
	get_default_erlang_interpreter_path/0,

	get_default_ssh_client_name/0,
	get_default_ssh_client_path/0,

	get_default_scp_executable_name/0,
	get_default_scp_executable_path/0,

	get_default_openssl_executable_name/0,
	get_default_openssl_executable_path/0,

	get_make_path/0,

	get_maybe_gnuplot_path/0,
	get_gnuplot_path/0,
	get_current_gnuplot_version/0,
	get_current_gnuplot_version/1,

	get_default_zip_compress_tool/0,
	get_default_zip_decompress_tool/0,

	get_default_bzip2_compress_tool/0,
	get_default_bzip2_decompress_tool/0,

	get_default_xz_compress_tool/0,
	get_default_xz_decompress_tool/0,

	get_default_hashing_tool/0,

	get_default_sql_client/0,

	get_default_xml_prettyprinter/0,

	get_default_java_runtime/0,
	get_default_jinterface_path/0,

	get_default_graph_stream_tool_name/0,
	get_default_graph_stream_tool_path/0 ]).



-doc "A name, not a path.".
-type executable_name() :: ustring().



-doc "Name and absolute path of an executable.".
-type executable_info() :: { executable_name(), executable_path() }.


-export_type([ executable_name/0, executable_info/0 ]).



% Miscellaneous section:
-export([ is_batch/0 ]).


-define( dot_exec_name, "dot" ).

-define( gnuplot_exec_name, "gnuplot" ).



% Type shorthands:

-type ustring() :: text_utils:ustring().
-type width() :: text_utils:width().

-type file_name() :: file_utils:file_name().
-type file_path() :: file_utils:file_path().
-type any_file_path() :: file_utils:any_file_path().

-type executable_path() :: file_utils:executable_path().
-type directory_path() :: file_utils:directory_path().

-type command_output() :: system_utils:command_output().
-type command_line_argument() :: cmd_line_utils:command_line_argument().

-type image_format() :: gui_image:image_format().



-doc """
Looks-up the specified executable program, whose name is specified as a string
(e.g. "gcc") in the current user PATH.

Returns an absolute filename of the executable program (e.g. "/usr/bin/gcc"), or
the 'false' atom if it was not found.
""".
-spec lookup_executable( executable_name() ) -> executable_path() | 'false'.
lookup_executable( ExecutableName ) ->

	% The Windows platform could be special-cased to detect *.bat executable
	% files and extract the actual executable from them; see
	% wings_job:find_executable/1 as an example.

	% Similar to a call to 'type' / 'which':
	os:find_executable( ExecutableName ).



-doc """
Looks-up the specified executable program, whose name is specified as a string
(e.g. "gcc") in the current user PATH, augmented of the specified list of
directories (whose existence is not checked), placed at first position.

Returns an absolute filename of the executable program (e.g. "/usr/bin/gcc"), or
the 'false' atom if it was not found.

For example: `lookup_executable("my-foo-program", [".", "/tmp"])`.
""".
-spec lookup_executable( executable_name(), [ directory_path() ] ) ->
								executable_path() | 'false'.
lookup_executable( ExecutableName, ExtraDirs ) ->

	% Let's reconstruct a proper PATH-like string:
	ExtraStr = text_utils:join( $:, ExtraDirs ),

	FullStr = case system_utils:get_environment_variable( "PATH" ) of

		false ->
			ExtraStr;

		PathValue ->
			text_utils:join( $:, [ ExtraStr, PathValue ] )

	end,

	% Similar to a call to 'type' / 'which':
	os:find_executable( ExecutableName, FullStr ).



-doc """
Finds the specified executable program, whose name is specified as a string
(e.g. "gcc") in the current user PATH.

Returns an absolute filename of the executable program (e.g. "/usr/bin/gcc") or
throws an exception {executable_not_found,ExecutableName} if it was not found.
""".
-spec find_executable( executable_name() ) -> executable_path().
find_executable( ExecutableName ) ->

	case lookup_executable( ExecutableName ) of

		false ->
			throw( { executable_not_found, ExecutableName } );

		Path ->
			Path

	end.




% Section for most usual commands.


-doc """
Returns whether a PNG file can be generated from a graph file: either confirms
it, or returns an hint why not.
""".
-spec can_generate_png_from_graph() -> 'true' | ustring().
can_generate_png_from_graph() ->

	Tool = ?dot_exec_name,

	case lookup_executable( Tool ) of

		false ->
			text_utils:format( "no '~ts' tool found (to be installed on many "
				"distributions with the 'graphviz' package)", [ Tool ] );

		_Path ->
			true

	end.



-doc """
Generates a PNG file from the specified graph file.

By default does not crash if dot outputs some warnings but does not yield an
error exit status.
""".
-spec generate_png_from_graph_file( file_path(), file_path() ) ->
											command_output().
generate_png_from_graph_file( PNGFilename, GraphFilename ) ->
	generate_png_from_graph_file( PNGFilename, GraphFilename,
								  _HaltOnDotOutput=false ).



-doc """
Generates a PNG image file from the specified graph file, that must respect the
dot (graphviz) syntax.

Arguments are:

- PNGFilePath the filename of the PNG to generate

- GraphFilePath the filename corresponding to the source graph

- HaltOnDotOutput tells whether the process should throw an exception should dot
 output an error or a warning

Returns the (possibly empty) string output by dot, or throws an exception.
""".
-spec generate_png_from_graph_file( file_path(), file_path(), boolean() ) ->
			command_output().
generate_png_from_graph_file( PNGFilePath, GraphFilePath,
							  _HaltOnDotOutput=true ) ->

	case execute_dot( PNGFilePath, GraphFilePath ) of

		[] ->
			% Most correct case:
			[];

		ErrorMessage ->
			throw( { graph_generation_failed, PNGFilePath, GraphFilePath,
					 ErrorMessage } )

	end;

% Any output remains available to the caller.
generate_png_from_graph_file( PNGFilePath, GraphFilePath,
							  _HaltOnDotOutput=false ) ->
	execute_dot( PNGFilePath, GraphFilePath ).



-doc """
Displays (without blocking) to the user the specified PNG file, using an
external viewer.

Throws an exception if an error occurs.
""".
-spec display_png_file( any_file_path() ) -> void().
display_png_file( PNGFilePath ) ->
	% Viewer output is ignored:
	system_utils:run_background_executable( get_default_image_viewer_path(),
		[ PNGFilePath ] ).



-doc """
Displays (without blocking) to the user the specified image file, using an
external viewer.

Throws an exception if an error occurs.
""".
-spec display_image_file( any_file_path(), image_format() ) -> void().
% Tool supposed able to display all image formats:
display_image_file( ImgFilePath, _ImgFormat ) ->
	system_utils:run_background_executable( get_default_image_viewer_path(),
											[ ImgFilePath ] ).



-doc """
Allows to browse (without blocking) the images available in the specified
directory (specified as a plain string)

Throws an exception if an error occurs.
""".
-spec browse_images_in( directory_path() ) -> void().
browse_images_in( DirectoryPath ) ->
	system_utils:run_background_command(
		%get_default_image_browser_path() ++ " " ++ DirectoryPath ).
		% To avoid log-like garbage outputs:
		get_default_image_browser_path() ++ " " ++ DirectoryPath
										 ++ " 2>/dev/null" ).



-doc """
Plays the specified audio file, in a non-blocking (in the background) way.

Throws an exception if an error occurs.
""".
-spec playback_audio_file( file_path() ) -> void().
playback_audio_file( AudioFilePath ) ->
	playback_audio_file( AudioFilePath, _DoBlock=false ).



-doc """
Plays the specified audio file, either in a blocking or in a non-blocking (in
the background) way.

Throws an exception if an error occurs.
""".
-spec playback_audio_file( file_path(), boolean() ) -> void().
playback_audio_file( AudioFilePath, DoBlock ) ->

	{ PlayerName, PlayerPath } = get_default_audio_player_info(),
	Args = get_default_audio_player_options( PlayerName ) ++ [ AudioFilePath ],

	case DoBlock of

		true ->
			system_utils:run_executable( PlayerPath, Args );

		false ->
			system_utils:run_background_executable( PlayerPath, Args )

	end.



-doc """
Displays (without blocking) to the user the specified PDF file, using an
external viewer.

Throws an exception if an error occurs.
""".
-spec display_pdf_file( file_path() ) -> void().
display_pdf_file( PDFFilePath ) ->
	system_utils:run_background_command(
		get_default_pdf_viewer_path() ++ " " ++ PDFFilePath ).



-doc """
Displays, in a blocking way, the specified text file.

Returns the text output by the tool (if any).

Throws an exception if an error occurs.
""".
-spec display_text_file( file_path() ) -> command_output().
display_text_file( TextFilePath ) ->

	case system_utils:run_command(
			get_default_text_viewer_path() ++ " " ++ TextFilePath ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { display_failed_for_text_file, TextFilePath, ExitCode,
					 ErrorOutput } )

	end.



-doc """
Displays, in a blocking way, the specified wide text file.

Returns the text output by the tool (if any).

Throws an exception if an error occurs.
""".
-spec display_wide_text_file( file_path(), width() ) -> command_output().
display_wide_text_file( TextFilePath, CharacterWidth ) ->

	case system_utils:run_command(
			get_default_wide_text_viewer_path( CharacterWidth )
				++ " " ++ TextFilePath ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { wide_display_failed_for_text_file, TextFilePath, ExitCode,
					 ErrorOutput } )

	end.



-doc """
Returns a string to be inserted into a command-line call to ssh/scp so that it
can run as much as possible non-interactively.

Tries notably to avoid following message: "The authenticity of host 'Server
(XXXXX)' can't be established.  RSA key fingerprint is YYYYY. Are you sure you
want to continue connecting (yes/no)?".

Note: only to be used in a trusted environment.

Returns the text output by the tool (if any).

Throws an exception if an error occurs.
""".
-spec get_ssh_mute_option() -> ustring().
get_ssh_mute_option() ->
	" -o \"StrictHostKeyChecking no\" ".



% Section about default tools.


% For each supported third-party feature X (e.g. X=image_viewer), two functions
% are to be defined:
%
%  - get_default_X_name() -> ustring() that returns the name of the tool (useful
%  for error messages)
%
%  - get_default_X_path() -> executable_path() that returns the full path
%  to the corresponding executable



-doc """
Returns the name of the default image viewer tool.

Could be also: xv, firefox, etc.
""".
-spec get_default_image_viewer_name() -> executable_name().
get_default_image_viewer_name() ->
	% Viewer is 'eye of gnome' here:
	%
	% (disabled, as too often not displaying the right version due to strange
	% caching)
	%
	%"eog".
	"gwenview".



-doc "Returns the name of the secondary default image viewer.".
-spec get_secondary_default_image_viewer_name() -> executable_name().
get_secondary_default_image_viewer_name() ->
	"eog".



-doc "Returns an absolute path to the default image viewer tool.".
-spec get_default_image_viewer_info() -> executable_info().
get_default_image_viewer_info() ->
	PrimaryImgViewerName = get_default_image_viewer_name(),
	case lookup_executable( PrimaryImgViewerName ) of

		false ->
			SecondaryImgViewerName = get_secondary_default_image_viewer_name(),
			case lookup_executable( SecondaryImgViewerName ) of

				false ->
					throw( { no_image_viewer_found,
						{ PrimaryImgViewerName, SecondaryImgViewerName } } );

				SecPath ->
					{ SecondaryImgViewerName, SecPath }

			end;

		PrimPath ->
			{ PrimaryImgViewerName, PrimPath }

	end.



-doc """
Returns the information (name and absolute path) relative to the default image
viewer tool.
""".
-spec get_default_image_viewer_path() -> executable_path().
get_default_image_viewer_path() ->
	pair:second( get_default_image_viewer_info() ).



-doc """
Returns the name of the default image browser tool.

Used to be: gqview (renamed since then).
""".
-spec get_default_image_browser_name() -> executable_name().
get_default_image_browser_name() ->
	% Was a mere compatibility alias for gqview:
	"geeqie".



-doc "Returns an absolute path to the default image browser tool.".
-spec get_default_image_browser_path() -> executable_path().
get_default_image_browser_path() ->
	case get_default_image_browser_name() of

		% Workaround for some distributions:
		Tool="geeqie" ->
			%find_executable( Tool ) ++ " --disable-clutter";
			find_executable( Tool );

		OtherTool ->
			find_executable( OtherTool )

	end.



-doc "Returns the name of the default web browser.".
-spec get_default_web_browser_name() -> executable_name().
get_default_web_browser_name() ->
	"firefox".



-doc "Returns an absolute path to the default web browser tool.".
-spec get_default_web_browser_path() -> executable_path().
get_default_web_browser_path() ->
	find_executable( get_default_web_browser_name() ).



-doc """
Returns the name of the default PDF viewer tool.

Could be also: xpdf, acroread, etc.
""".
-spec get_default_pdf_viewer_name() -> executable_name().
get_default_pdf_viewer_name() ->
	"evince".



-doc "Returns an absolute path to the default PDF viewer tool.".
-spec get_default_pdf_viewer_path() -> executable_path().
get_default_pdf_viewer_path() ->
	find_executable( get_default_pdf_viewer_name() ).



-doc """
Returns the name of the default text viewer tool.

Could be also: nedit, emacs, etc.
""".
-spec get_default_text_viewer_name() -> executable_name().
get_default_text_viewer_name() ->
	"gedit".



-doc "Returns an absolute path to the default text viewer tool.".
-spec get_default_text_viewer_path() -> executable_path().
get_default_text_viewer_path() ->
	find_executable( get_default_text_viewer_name() ).



-doc "Returns the name of the default viewer tool for wider texts.".
-spec get_default_wide_text_viewer_name( width() ) -> executable_name().
get_default_wide_text_viewer_name( _CharacterWidth ) ->
	% Could be: "nedit":
	"gedit".



-doc "Returns an absolute path to the default viewer tool for wider texts.".
-spec get_default_wide_text_viewer_path( width() ) -> executable_path().
get_default_wide_text_viewer_path( CharacterWidth ) ->
	% Could be: io_lib:format( "nedit -column ~B", [ CharacterWidth ] )
	find_executable( get_default_wide_text_viewer_name( CharacterWidth ) ).



-doc "Returns the name of the default audio player.".
-spec get_default_audio_player_name() -> executable_name().
get_default_audio_player_name() ->
	"mplayer".



-doc "Returns the name of the secondary default audio player.".
-spec get_secondary_default_audio_player_name() -> executable_name().
get_secondary_default_audio_player_name() ->
	% Command-line VLC client:
	"cvlc".



-doc """
Returns the information (name and absolute path) relative to the default audio
player.
""".
-spec get_default_audio_player_info() -> executable_info().
get_default_audio_player_info() ->
	PrimaryPlayerName = get_default_audio_player_name(),
	case lookup_executable( PrimaryPlayerName ) of

		false ->
			SecondaryPlayerName = get_secondary_default_audio_player_name(),
			case lookup_executable( SecondaryPlayerName ) of

				false ->
					throw( { no_audio_player_found,
								{ PrimaryPlayerName, SecondaryPlayerName } } );

				SecPath ->
					{ SecondaryPlayerName, SecPath }

			end;

		PrimPath ->
			{ PrimaryPlayerName, PrimPath }

	end.



-doc """
Returns a list of the command-line options suitable for the specified audio
player, notably so that it can run as much as possible non-interactively.
""".
-spec get_default_audio_player_options( executable_name() ) ->
			[ command_line_argument() ].
get_default_audio_player_options( _AudioPlayerName="mplayer") ->
	[ "-vc", "null", "-vo", "null", "-really-quiet", "-nolirc", "-msglevel",
	  "all=0:demuxer=0" ];

get_default_audio_player_options( _AudioPlayerName="cvlc") ->
	[ "--quiet", "--novideo", "--play-and-exit" ].



-doc """
Returns the name of the default trace viewer tool.

Could be also: nedit, gedit, etc.
""".
-spec get_default_trace_viewer_name() -> executable_name().
get_default_trace_viewer_name() ->

	case system_utils:has_graphical_output() of

		true ->
			find_executable( "logmx.sh" );

		false ->
			% Poor's man solution:
			"/bin/cat"

	end.



-doc """
Returns an absolute path to the default trace viewer tool.

Could be also: nedit, gedit, etc.
""".
-spec get_default_trace_viewer_path() -> executable_path().
get_default_trace_viewer_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_trace_viewer_name() ).



-doc """
Returns an absolute path to the root directory of the current Erlang
installation.

For example if 'erl' is to be found in
~/Software/Erlang/Erlang-current-install/bin/erl, will return:
~/Software/Erlang/Erlang-current-install.
""".
-spec get_default_erlang_root() -> directory_path().
get_default_erlang_root() ->
	file_utils:normalise_path( file_utils:join( [
		get_default_erlang_interpreter_path(), "..", "..", "..", "..", ".." ]
											  ) ).



-doc "Returns the name of the default Erlang interpreter.".
-spec get_default_erlang_interpreter_name() -> executable_name().
get_default_erlang_interpreter_name() ->
	"erl".



-doc "Returns an absolute path to the default Erlang interpreter.".
-spec get_default_erlang_interpreter_path() -> executable_path().
get_default_erlang_interpreter_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_erlang_interpreter_name() ).



-doc "Returns the name of the default SSH client.".
-spec get_default_ssh_client_name() -> executable_name().
get_default_ssh_client_name() ->
	"ssh".



-doc "Returns an absolute path to the default SSH client.".
-spec get_default_ssh_client_path() -> executable_path().
get_default_ssh_client_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_ssh_client_name() ).



-doc "Returns the name default SSH-based scp executable.".
-spec get_default_scp_executable_name() -> executable_name().
get_default_scp_executable_name() ->
	"scp".



-doc "Returns an absolute path to the default SSH-based scp executable.".
-spec get_default_scp_executable_path() -> executable_path().
get_default_scp_executable_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_scp_executable_name() ).



-doc "Returns the name default openssl-based executable.".
-spec get_default_openssl_executable_name() -> executable_name().
get_default_openssl_executable_name() ->
	"openssl".



-doc "Returns an absolute path to the default openssl-based executable.".
-spec get_default_openssl_executable_path() -> executable_path().
get_default_openssl_executable_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_openssl_executable_name() ).



-doc "Returns an absolute path to the default (GNU) make executable.".
-spec get_make_path() -> executable_path().
get_make_path() ->
	find_executable( "make" ).



-doc """
Tells whether a gnuplot executable is available, by returning its path if found.
""".
-spec get_maybe_gnuplot_path() -> option( executable_path() ).
get_maybe_gnuplot_path() ->
	% Note: expected to be on the PATH:
	case lookup_executable( ?gnuplot_exec_name ) of

		false ->
			undefined;

		Path ->
			Path

	end.



-doc "Returns an absolute path to a gnuplot executable.".
-spec get_gnuplot_path() -> executable_path().
get_gnuplot_path() ->
	% Note: expected to be on the PATH:
	find_executable( ?gnuplot_exec_name ).



-doc """
Returns, as a tuple (e.g. {4,2} for the 4.2 version), the gnuplot version
actually available by default (in the PATH) on this computer.
""".
-spec get_current_gnuplot_version() -> basic_utils:two_digit_version().
get_current_gnuplot_version() ->
	GnuplotPath = get_gnuplot_path(),
	get_current_gnuplot_version( GnuplotPath ).



-doc """
Returns, as a tuple (e.g. {4,2} for the 4.2 version), the gnuplot version
actually available on this computer.
""".
-spec get_current_gnuplot_version( executable_path() ) ->
			basic_utils:two_digit_version().
get_current_gnuplot_version( GnuplotPath ) ->

	% gnuplot -V returns information like "gnuplot 4.4 patchlevel 0"; rather
	% that evaluation a shell expression like:
	%
	% Cmd = get_gnuplot_path() ++ " -V | awk '{print $2}'",
	%
	% we prefer executing directly gnuplot, have then the exit status, and parse
	% the result in Erlang:
	%
	Cmd = GnuplotPath ++ " -V",

	% The returned value of following command is like "4.2":
	case system_utils:run_command( Cmd ) of

		{ _ExitCode=0, Output } ->
			GnuplotVersionInString = lists:nth( _IndexVersion=2,
				text_utils:split_per_element( Output, " " ) ),
			basic_utils:parse_version( GnuplotVersionInString );

		{ ExitCode, ErrorOutput } ->
			throw( { gnuplot_version_detection_failed, ExitCode,
					 ErrorOutput } )

	end.



-doc "Returns the default tool to use to compress in the ZIP format.".
-spec get_default_zip_compress_tool() -> executable_path().
get_default_zip_compress_tool() ->
	find_executable( "zip" ).



-doc "Returns the default tool to use to decompress in the ZIP format.".
-spec get_default_zip_decompress_tool() -> executable_path().
get_default_zip_decompress_tool() ->
	find_executable( "unzip" ).



-doc """
Returns the default tool to use to decompress in the BZIP2 format.
""".
-spec get_default_bzip2_compress_tool() -> executable_path().
get_default_bzip2_compress_tool() ->
	find_executable( "bzip2" ).



-doc """
Returns the default tool to use to decompress in the BZIP2 format.
""".
-spec get_default_bzip2_decompress_tool() -> executable_path().
get_default_bzip2_decompress_tool() ->
	find_executable( "bunzip2" ).



-doc "Returns the default tool to use to compress in the XZ format.".
-spec get_default_xz_compress_tool() -> executable_path().
get_default_xz_compress_tool() ->
	find_executable( "xz" ).



-doc "Returns the default tool to use to decompress in the XZ format.".
-spec get_default_xz_decompress_tool() -> executable_path().
get_default_xz_decompress_tool() ->
	find_executable( "unxz" ).



-doc "Returns the default tool to compute any kind of hash.".
-spec get_default_hashing_tool() -> executable_path().
get_default_hashing_tool() ->
	% Typically obtained thanks to an 'openssl' package:
	find_executable( "openssl" ).



-doc "Returns the default client to interact with a SQL database.".
-spec get_default_sql_client() -> executable_path().
get_default_sql_client() ->
	find_executable( "psql" ).



-doc "Returns the default client to pretty-print XML content.".
-spec get_default_xml_prettyprinter() -> executable_path().
get_default_xml_prettyprinter() ->
	find_executable( "xmllint" ).



-doc "Returns the default tool to execute Java programs.".
-spec get_default_java_runtime() -> executable_path().
get_default_java_runtime() ->
	find_executable( "java" ).



-doc """
Returns the default path to the .jar file implementing JInterface, namely
'OtpErlang.jar'.

Indeed, to make use of JInterface, OtpErlang.jar must be found by the
counterpart Java program.

We chose conventionally its location to be
$(ERLANG_ROOT)/lib/erlang/jinterface/priv/OtpErlang.jar, with ERLANG_ROOT being
typically ~/Software/Erlang/Erlang-current-install.

Indeed, we expect that in $(ERLANG_ROOT)/lib/erlang/ a symbolic link named
'jinterface' has been specifically created in order to point to the directory of
the corresponding version of JInterface (e.g. lib/jinterface-1.8/); our
install-erlang.sh script automatically enforces that convention.
""".
-spec get_default_jinterface_path() -> file_path().
get_default_jinterface_path() ->

	JInterfaceBase = file_utils:join(
		[ get_default_erlang_root(), "lib", "erlang", "jinterface" ] ),

	% Can be directory or, more probably, symlink:
	case file_utils:is_existing_directory_or_link( JInterfaceBase ) of

		true ->
			JInterfaceJar =
				file_utils:join( [ JInterfaceBase, "priv", "OtpErlang.jar" ] ),

			case file_utils:is_existing_file( JInterfaceJar ) of

				true ->
					JInterfaceJar;

				false ->
					throw( { jinterface_jar_not_found, JInterfaceJar } )

			end;

		false ->
			trace_utils:error_fmt( "The JInterface base path (~ts) does not "
				"exist; conventionally this is a symbolic link pointing to, "
				"typically, 'lib/jinterface-x.y/'.", [ JInterfaceBase ] ),
			throw( { jinterface_base_path_not_found, JInterfaceBase } )

	end.



-doc "Returns the name of the default tool used to process streamed graphs.".
-spec get_default_graph_stream_tool_name() -> executable_name().
get_default_graph_stream_tool_name() ->
	% See https://gephi.org/:
	"gephi".



-doc """
Returns an absolute path to the default tool used to process streamed graphs.
""".
-spec get_default_graph_stream_tool_path() -> executable_path().
get_default_graph_stream_tool_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_graph_stream_tool_name() ).




% Miscellaneous section:


-doc """
Tells whether the program is run in batch mode.

By default, a program is not in batch mode (hence is in interactive mode,
meaning it might trigger graphical displays).

The most prioritary setting is if the "--batch" command line argument has been
specified, provided it has been set as a plain argument, i.e. one that it is
specified *after* either "--" or, preferably, "-extra".

Otherwise, the application configuration will be read for the is_batch key
(typically set from any conf/sys.config file defined by the application; see
also the "-config" command-line option in
<https://erlang.org/doc/man/config.html>).

Finally, if not set elsewhere, the application resource file (*.app) will be
searched for such an is_batch key.

Note that, if relying on application configuration, the result will depend on
the {application name, callsite} pair. Indeed, if application foo depends on
application bar, and foo defined in its conf/sys.config file {is_batch,false}
whereas bar defined in its own configuration file {is_batch,true}, should a
process belonging to bar call this function, it will return false.
""".
-spec is_batch() -> boolean().
is_batch() ->

	% Corresponds to the '--batch' command-line option (a *plain* argument,
	% hence expected to be after a -extra command-line switch):
	%
	case cmd_line_utils:get_command_arguments_for_option( '-batch' ) of

		% Normal case if set on the command-line:
		[ [] ] ->
			%trace_utils:debug( "Batch mode activated through command line." ),
			true;

		L when is_list( L ) ->
			trace_utils:error_fmt( "The '--batch' option does not imply any "
				"associated value, whereas the following was specified: '~p'.",
				[ L ] ),
			throw( { unexpected_batch_options, L } );

		% Normal case if not set on the command-line:
		undefined ->
			case application:get_env( is_batch ) of

				{ ok, true } ->
					%trace_utils:debug(
					%  "Batch mode enabled through configuration." ),
					true;

				{ ok, false } ->
					%trace_utils:debug(
					%  "Batch mode disabled through configuration." ),
					false;

				undefined ->
					%trace_utils:debug("Batch mode disabled (default)." ),
					% Default then is:
					false

			end

	end.



% Helper functions.


-doc "Executes the dot (Graphviz) tool.".
-spec execute_dot( file_name(), file_name() ) -> command_output().
execute_dot( PNGFilename, GraphFilename ) ->

	DotExec = find_executable( ?dot_exec_name ),

	Cmd = DotExec ++ " -o" ++ PNGFilename ++ " -Tpng " ++ GraphFilename,

	% Dot might issue non-serious warnings:
	case system_utils:run_command( Cmd ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { rendering_failed, GraphFilename, PNGFilename, ExitCode,
					 ErrorOutput } )

	end.
