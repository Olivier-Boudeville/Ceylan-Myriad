% Copyright (C) 2008-2019 Olivier Boudeville
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


% Gathering of various convenient facilities regarding executing of third-party
% tools.
%
% See executable_utils_test.erl for the corresponding test.
%
-module(executable_utils).




% Section for the searching and checking of executables:
-export([ lookup_executable/1, find_executable/1 ]).



% Section for most usual commands:
-export([ generate_png_from_graph_file/2,
		  generate_png_from_graph_file/3, display_png_file/1,
		  browse_images_in/1, display_pdf_file/1, display_text_file/1,
		  display_wide_text_file/2, get_ssh_mute_option/0,
		  compute_md5_sum/1, compute_sha1_sum/1, compute_sha_sum/2 ]).



% Section about default tools:
-export([

		 get_default_image_viewer_name/0,
		 get_default_image_viewer_path/0,

		 get_default_image_browser_name/0,
		 get_default_image_browser_path/0,

		 get_default_pdf_viewer_name/0,
		 get_default_pdf_viewer_path/0,

		 get_default_text_viewer_name/0,
		 get_default_text_viewer_path/0,

		 get_default_wide_text_viewer_name/1,
		 get_default_wide_text_viewer_path/1,

		 get_default_trace_viewer_name/0,
		 get_default_trace_viewer_path/0,

		 get_default_erlang_root/0,
		 get_default_erlang_interpreter_name/0,
		 get_default_erlang_interpreter_path/0,

		 get_default_ssh_client_name/0,
		 get_default_ssh_client_path/0,

		 get_default_scp_executable_name/0,
		 get_default_scp_executable_path/0,

		 get_gnuplot_path/0,
		 get_current_gnuplot_version/0,

		 get_default_zip_compress_tool/0,
		 get_default_zip_decompress_tool/0,

		 get_default_bzip2_compress_tool/0,
		 get_default_bzip2_decompress_tool/0,

		 get_default_xz_compress_tool/0,
		 get_default_xz_decompress_tool/0,

		 get_default_md5_tool/0,
		 get_default_sha_tool/0,

		 get_default_java_runtime/0,
		 get_default_jinterface_path/0 ]).



% MD5 sum, a 128-bit hash value:
%
-type md5_sum() :: non_neg_integer().


% SHA1 sum, a 160-bit hash value:
%
-type sha1_sum() :: non_neg_integer().


% SHA sum, a hash value of unspecified size:
%
-type sha_sum() :: non_neg_integer().


% Command-line managed like init:get_argument/1:


% The name of a command-line option (ex: '-color', for an actual option that is
% "--color"):
%
% (a flag, for the init standard module)
%
-type command_line_option() :: atom().


% The name of a command-line value (ex: "blue"):
-type command_line_value() :: text_utils:ustring().


% The association between an option and the various values associated to its
% instances.
%
% Ex: if arguments were "--color blue red [...] --color yellow", then the
% corresponding argument is { '-color', [ [ "blue", "red" ]; [ "yellow" ] ] }.
%
-type command_line_argument() ::
		{ command_line_option(), [ command_line_value() ] }.



% A table storing command-line user arguments conveniently (a bit like getopt),
% in a format in the spirit of init:get_arguments/0.
%
% Useful to manage arguments more easily, and also to handle uniformly the
% arguments specified for erl-based executions and escript ones alike.
%
-type argument_table() :: list_table:list_table( command_line_option(),
												 [ command_line_value() ] ).


-export_type([ md5_sum/0, sha1_sum/0, sha_sum/0,
			   command_line_option/0, command_line_value/0,
			   command_line_argument/0, argument_table/0 ]).


% Command-line argument section:
%
-export([ get_argument_table/0, get_argument_table/1, generate_argument_table/1,
		  extract_command_argument/1, extract_command_argument/2,
		  argument_table_to_string/1 ]).


% Miscellaneous section:
%
-export([ is_batch/0 ]).



% Looks-up specified executable program, whose name is specified as a string
% (ex: "gcc") in the current user PATH.
%
% Returns an absolute filename of the executable program (ex: "/usr/bin/gcc"),
% or the 'false' atom if it was not found.
%
-spec lookup_executable( file_utils:file_name() ) ->
							   file_utils:path() | 'false'.
lookup_executable( ExecutableName ) ->
	% Similar to a call to 'type':
	os:find_executable( ExecutableName ).



% Finds specified executable program, whose name is specified as a string (ex:
% "gcc") in the current user PATH.
%
% Returns an absolute filename of the executable program (ex: "/usr/bin/gcc") or
% throws an exception {executable_not_found,ExecutableName} if it was not found.
%
-spec find_executable( file_utils:file_name() ) -> file_utils:path().
find_executable( ExecutableName ) ->

	case lookup_executable( ExecutableName) of

		false ->
			throw( { executable_not_found, ExecutableName } );

		Path ->
			Path

	end.






% Section for most usual commands.


% By default does not crash if dot outputs some warnings but does not yield an
% error exit status.
%
-spec generate_png_from_graph_file( file_utils:path(), file_utils:path() ) ->
										  text_utils:ustring().
generate_png_from_graph_file( PNGFilename, GraphFilename ) ->
	generate_png_from_graph_file( PNGFilename, GraphFilename,
								  _HaltOnDotOutput=false ).



% Generates a PNG image file from specified graph file, that must respect the
% dot (graphviz) syntax:
%
%  - PNGFilename the filename of the PNG to generate
%
%  - GraphFilename the filename corresponding to the source graph
%
%  - HaltOnDotOutput tells whether the process should throw an exception should
%  dot output an error or a warning
%
% Returns the (possibly empty) string output by dot, or throws an exception.
%
-spec generate_png_from_graph_file( file_utils:path(), file_utils:path(),
								   boolean() ) -> string().
generate_png_from_graph_file( PNGFilename, GraphFilename,
							  _HaltOnDotOutput=true ) ->

	case execute_dot( PNGFilename, GraphFilename ) of

		[] ->
			% Most correct case:
			[];

		ErrorMessage ->
			throw( { graph_generation_failed, PNGFilename, GraphFilename,
					 ErrorMessage } )

	end;

% Any output remains available to the caller.
generate_png_from_graph_file( PNGFilename, GraphFilename,
							  _HaltOnDotOutput=false ) ->
	execute_dot( PNGFilename, GraphFilename ).



% Displays (without blocking) to the user the specified PNG, using an external
% viewer.
%
% Returns the text output by the tool (if any).
%
% Throws an exception if an error occurs.
%
-spec display_png_file( file_utils:path() ) -> void().
display_png_file( PNGFilename ) ->
	% Viewer output is ignored:
	system_utils:run_background_executable( get_default_image_viewer_path()
											 ++ " " ++ PNGFilename  ).



% Allows to browse (without blocking) the images available in specified
% directory (specified as a plain string)
%
% Returns the text output by the tool (if any).
%
% Throws an exception if an error occurs.
%
-spec browse_images_in( file_utils:path() ) -> void().
browse_images_in( DirectoryName ) ->
	system_utils:run_background_executable( get_default_image_browser_path()
											++ " " ++ DirectoryName ).



% Displays (without blocking) to the user the specified PNG, using an external
% viewer.
%
% Returns the text output by the tool (if any).
%
% Throws an exception if an error occurs.
%
-spec display_pdf_file( file_utils:path() ) -> void().
display_pdf_file( PDFFilename ) ->
	system_utils:run_background_executable( get_default_pdf_viewer_path()
											 ++ " " ++ PDFFilename ).



% Displays, with blocking, a text file.
%
% Returns the text output by the tool (if any).
%
% Throws an exception if an error occurs.
%
-spec display_text_file( file_utils:path() ) -> string().
display_text_file( TextFilename ) ->

	case system_utils:run_executable( get_default_text_viewer_path()
									  ++ " " ++ TextFilename ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { display_failed_for_text_file, ExitCode, ErrorOutput } )

	end.



% Displays, with blocking, a wide text file.
%
% Returns the text output by the tool (if any).
%
% Throws an exception if an error occurs.
%
-spec display_wide_text_file( file_utils:path(), pos_integer() ) -> string().
display_wide_text_file( TextFilename, CharacterWidth ) ->

	case system_utils:run_executable(
		   get_default_wide_text_viewer_path( CharacterWidth )
		   ++ " " ++ TextFilename ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { wide_display_failed_for_text_file, ExitCode,
					 ErrorOutput } )

	end.



% Returns a string to be inserted into a command-line call to ssh/scp so that it
% can run as much as possible non-interactively.
%
% Tries notably to avoid following message: "The authenticity of host 'Server
% (XXXXX)' can't be established.  RSA key fingerprint is YYYYY. Are you sure you
% want to continue connecting (yes/no)?".
%
% Note: only to be used in a trusted environment.
%
% Returns the text output by the tool (if any).
%
% Throws an exception if an error occurs.
%
-spec get_ssh_mute_option() -> string().
get_ssh_mute_option() ->
  " -o \"StrictHostKeyChecking no\" ".



% Returns the MD5 sum computed from the content of the specified file, as an
% unsigned integer, actually of 128 bits (ex:
% 96950473382892364268626543336313804804, corresponding to hexadecimal string
% "48effb631c66e93c7054c10f798f5804").
%
-spec compute_md5_sum( file_utils:path() ) -> md5_sum().
compute_md5_sum( Filename ) ->

	case file_utils:is_existing_file( Filename ) of

		true ->
			ok;

		false ->
			throw( { file_for_md5_not_found, Filename } )

	end,

	% erlang:md5/1 not used here, would be probably slower:

	% Removes the filename after the MD5 code:
	Cmd = system_utils:run_executable( get_default_md5_tool() ++ " '"
									   ++ Filename ++ "' | sed 's|  .*$||1'" ),

	case Cmd of

		{ _ExitCode=0, OutputString } ->
			list_to_integer( OutputString, _Base=16 );

		{ ExitCode, ErrorOutput } ->
			throw( { md5_computation_failed, ExitCode, ErrorOutput, Filename } )

	end.



% Returns the SHA1 sum computed from the content of the specified file, as an
% unsigned integer, actually of 160 bits (ex:
% 189271338729529876450691804503218393830331783574, corresponding to hexadecimal
% string "212738699f721d8d8c3e58dac2b113bb8d0c1996").
%
-spec compute_sha1_sum( file_utils:path() ) -> sha1_sum().
compute_sha1_sum( Filename ) ->
	compute_sha_sum( Filename, _SizeOfSHAAlgorithm=1 ).



% Returns the SHA sum computed from the content of the specified file, as an
% unsigned integer, whose size depends on the specified algorithm: 1, 224, 256,
% 384, 512, 512224, 512256 (see 'man shasum' for more details).
%
-spec compute_sha_sum( file_utils:path(), basic_utils:count() ) -> sha_sum().
compute_sha_sum( Filename, SizeOfSHAAlgorithm )
  when is_integer( SizeOfSHAAlgorithm ) ->

	case file_utils:is_existing_file( Filename ) of

		true ->
			ok;

		false ->
			throw( { file_for_sha_not_found, Filename } )

	end,

	% Removes the filename after the MD5 code:
	Cmd = system_utils:run_executable( get_default_sha_tool()
					 ++ " --algorithm "
					 ++ text_utils:format( "~B '", [ SizeOfSHAAlgorithm ] )
					 ++ Filename ++ "' | sed 's|  .*$||1'" ),

	case Cmd of

		{ _ExitCode=0, OutputString } ->
			list_to_integer( OutputString, _Base=16 );

		{ ExitCode, ErrorOutput } ->
			throw( { sha_computation_failed, ExitCode, ErrorOutput, Filename } )

	end.



% Section about default tools.


% For each supported third-party feature X (ex: X=image_viewer), two functions
% are to be defined:
%
%  - get_default_X_name() -> string() that returns the name of the tool (useful
%  for error messages)
%
%  - get_default_X_path() -> file_utils:file_name() that returns the full path
%  to the corresponding executable



% Returns the name of the default image viewer tool.
%
% Could be also: xv, firefox, etc.
%
-spec get_default_image_viewer_name() -> string().
get_default_image_viewer_name() ->
	% Viewer is 'eye of gnome' here:
	"eog".


% Returns an absolute path to the default image viewer tool.
-spec get_default_image_viewer_path() -> file_utils:path().
get_default_image_viewer_path() ->
	find_executable( get_default_image_viewer_name() ).



% Returns the name of the default image browser tool.
% Used to be: gqview (renamed)
-spec get_default_image_browser_name() -> string().
get_default_image_browser_name() ->
	% Was a mere compatibility alias for gqview:
	"geeqie".


% Returns an absolute path to the default image browser tool.
-spec get_default_image_browser_path() -> file_utils:file_name().
get_default_image_browser_path() ->
	find_executable( get_default_image_browser_name() ).



% Returns the name of the default PDF viewer tool.
% Could be also: xpdf, acroread, etc.
-spec get_default_pdf_viewer_name() -> string().
get_default_pdf_viewer_name() ->
	"evince".


% Returns an absolute path to the default PDF viewer tool.
-spec get_default_pdf_viewer_path() -> file_utils:file_name().
get_default_pdf_viewer_path() ->
	find_executable( get_default_pdf_viewer_name() ).



% Returns the name of the default text viewer tool.
% Could be also: nedit, emacs, etc.
-spec get_default_text_viewer_name() -> string().
get_default_text_viewer_name() ->
	"gedit".


% Returns an absolute path to the default text viewer tool.
-spec get_default_text_viewer_path() -> file_utils:file_name().
get_default_text_viewer_path() ->
	find_executable( get_default_text_viewer_name() ).



% Returns the name of the default viewer tool for wider texts.
-spec get_default_wide_text_viewer_name( non_neg_integer() ) -> string().
get_default_wide_text_viewer_name( _CharacterWidth ) ->
	% Could be: "nedit":
	"gedit".


% Returns an absolute path to the default viewer tool for wider texts.
-spec get_default_wide_text_viewer_path( non_neg_integer() )
								  -> file_utils:file_name().
get_default_wide_text_viewer_path( CharacterWidth ) ->
	% Could be: io_lib:format( "nedit -column ~B", [ CharacterWidth ] )
	find_executable( get_default_wide_text_viewer_name( CharacterWidth ) ).



% Returns the name of the default trace viewer tool.
% Could be also: nedit, gedit, etc.
-spec get_default_trace_viewer_name() -> string().
get_default_trace_viewer_name() ->

	case system_utils:has_graphical_output() of

		true ->
			find_executable( "logmx.sh" );

		false ->
			% Poor's man solution:
			"/bin/cat"

	end.



% Returns an absolute path to the default trace viewer tool.
% Could be also: nedit, gedit, etc.
-spec get_default_trace_viewer_path() -> file_utils:file_name().
get_default_trace_viewer_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_trace_viewer_name() ).



% Returns an absolute path to the root directory of the current Erlang
% installation.
%
% Ex: if 'erl' is to be found in
% ~/Software/Erlang/Erlang-current-install/bin/erl, will return:
% ~/Software/Erlang/Erlang-current-install.
%
-spec get_default_erlang_root() -> file_utils:directory_name().
get_default_erlang_root() ->
	file_utils:normalise_path( file_utils:join( [
		get_default_erlang_interpreter_path(), "..", "..", "..", "..", ".." ]
											  ) ).



% Returns the name of the default Erlang interpreter.
-spec get_default_erlang_interpreter_name() -> string().
get_default_erlang_interpreter_name() ->
	"erl".


% Returns an absolute path to the default Erlang interpreter.
-spec get_default_erlang_interpreter_path() -> file_utils:file_name().
get_default_erlang_interpreter_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_erlang_interpreter_name() ).



% Returns the name of the default SSH client.
-spec get_default_ssh_client_name() -> string().
get_default_ssh_client_name() ->
	"ssh".


% Returns an absolute path to the default SSH client.
-spec get_default_ssh_client_path() -> file_utils:file_name().
get_default_ssh_client_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_ssh_client_name() ).



% Returns the name default SSH-based scp executable.
-spec get_default_scp_executable_name() -> string().
get_default_scp_executable_name() ->
	"scp".


% Returns an absolute path to the default SSH-based scp executable.
-spec get_default_scp_executable_path() -> file_utils:file_name().
get_default_scp_executable_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_scp_executable_name() ).



-spec get_gnuplot_path() -> file_utils:file_name().
get_gnuplot_path() ->
	% Note: expected to be on the PATH:
	find_executable( "gnuplot" ).



% Returns, as a tuple (ex: {4,2} for the 4.2 version), the gnuplot version
% actually available on the computer.
%
-spec get_current_gnuplot_version() -> basic_utils:two_digit_version().
get_current_gnuplot_version() ->

	% gnuplot -V returns information like "gnuplot 4.4 patchlevel 0"; rather
	% that evaluation a shell expression like:
	%
	% Cmd = get_gnuplot_path() ++ " -V | awk '{print $2}'",
	%
	% we prefer executing directly gnuplot, have then the exit status, and parse
	% the result in Erlang:
	%
	Cmd = get_gnuplot_path() ++ " -V",

	% The returned value of following command is like "4.2":
	%
	case system_utils:run_executable( Cmd ) of

			{ _ExitCode=0, Output } ->
				GnuplotVersionInString = lists:nth( _IndexVersion=2,
										  text_utils:split( Output, " " ) ),
				basic_utils:parse_version( GnuplotVersionInString );

			{ ExitCode, ErrorOutput } ->
				throw( { gnuplot_version_detection_failed, ExitCode,
						 ErrorOutput } )

	end.



% Returns the default tool to use to compress in the ZIP format.
%
-spec get_default_zip_compress_tool() -> file_utils:file_name().
get_default_zip_compress_tool() ->
	find_executable( "zip" ).


% Returns the default tool to use to decompress in the ZIP format.
%
-spec get_default_zip_decompress_tool() -> file_utils:file_name().
get_default_zip_decompress_tool() ->
	find_executable( "unzip" ).


% Returns the default tool to use to decompress in the BZIP2 format.
%
-spec get_default_bzip2_compress_tool() -> file_utils:file_name().
get_default_bzip2_compress_tool() ->
	find_executable( "bzip2" ).


% Returns the default tool to use to decompress in the BZIP2 format.
%
-spec get_default_bzip2_decompress_tool() -> file_utils:file_name().
get_default_bzip2_decompress_tool() ->
	find_executable( "bunzip2" ).


% Returns the default tool to use to compress in the XZ format.
%
-spec get_default_xz_compress_tool() -> file_utils:file_name().
get_default_xz_compress_tool() ->
	find_executable( "xz" ).


% Returns the default tool to use to decompress in the XZ format.
%
-spec get_default_xz_decompress_tool() -> file_utils:file_name().
get_default_xz_decompress_tool() ->
	find_executable( "unxz" ).


% Returns the default tool to compute MD5 sums.
%
-spec get_default_md5_tool() -> file_utils:file_name().
get_default_md5_tool() ->
	find_executable( "md5sum" ).


% Returns the default tool to compute SHA sums.
%
-spec get_default_sha_tool() -> file_utils:file_name().
get_default_sha_tool() ->
	find_executable( "shasum" ).



% Returns the default tool to execute Java programs.
%
-spec get_default_java_runtime() -> file_utils:file_name().
get_default_java_runtime() ->
	find_executable( "java" ).


% Returns the default path to the .jar file implementing JInterface,
% i.e. 'OtpErlang.jar'.
%
% Indeed, to make use of JInterface, OtpErlang.jar must be found by the
% counterpart Java program.
%
% We chose conventionally its location to be
% $(ERLANG_ROOT)/lib/erlang/jinterface/priv/OtpErlang.jar,
% with ERLANG_ROOT being typically ~/Software/Erlang/Erlang-current-install.
%
% Indeed, we expect that in $(ERLANG_ROOT)/lib/erlang/ a symbolic link named
% 'jinterface' has been specifically created in order to point to the directory
% of the corresponding version of JInterface (ex: lib/jinterface-1.8/); our
% install-erlang.sh script automatically enforces that convention.
%
-spec get_default_jinterface_path() -> file_utils:file_name().
get_default_jinterface_path() ->

	JInterfaceBase = file_utils:join(
			[ get_default_erlang_root(), "lib", "erlang", "jinterface" ] ),

	% Can be directory or, more probably, symlink:
	case file_utils:is_existing_directory_or_link( JInterfaceBase ) of

		true ->
			JInterfaceJar = file_utils:join( [ JInterfaceBase, "priv",
											   "OtpErlang.jar" ] ),

			case file_utils:is_existing_file( JInterfaceJar ) of

				true ->
					JInterfaceJar;

				false ->
					throw( { jinterface_jar_not_found, JInterfaceJar } )

			end;

		false ->
			trace_utils:error_fmt( "The JInterface base path (~s) does not "
								   "exist; conventionally this is a symbolic "
								   "link pointing to, typically, "
								   "'lib/jinterface-x.y/'.",
								   [ JInterfaceBase ] ),
			throw( { jinterface_base_path_not_found, JInterfaceBase } )

	end.




% Command-line argument section:


% Returns a canonical argument table, obtained from the user command-line
% arguments supplied to the interpreter.
%
% Note: to be called in the context of a standard erl execution (as opposed to
% an escript one).
%
-spec get_argument_table() -> argument_table().
get_argument_table() ->

	% We do not want to include the VM-specific arguments (such as -noshell,
	% -pz, etc.); use, in the command-line, '-extra', before arguments to
	% consider as plain ones:
	%Args = init:get_arguments(),

	Args = init:get_plain_arguments(),

	%trace_utils:debug_fmt( "Arguments obtained: ~p.", [ Args ] ),

	% To convert a list of strings into per-option list of values:
	get_argument_table( Args ).



% Returns a canonical argument table, obtained from the user command-line
% arguments supplied to this escript.
%
% Note: to be called in the context of an escript (as opposed to a standard erl
% execution), specifying the argument list that its main/1 function received.
%
-spec get_argument_table( [ string() ] ) -> argument_table().
get_argument_table( ArgStrings ) ->
	script_utils:get_arguments( ArgStrings ).



% Returns a canonical argument table, obtained from the specified single string
% containing all options, verbatim ; ex: "--color red --set-foo".
%
% Note: useful for testing, to introduce specific command lines.
%
-spec generate_argument_table( string() ) -> argument_table().
generate_argument_table( ArgString ) ->

	CommandLineArgs = text_utils:split( ArgString, _Delimiters=[ $ ] ),

	get_argument_table( CommandLineArgs ).




% Extracts specified option, i.e. its various associated values (if any), from
% the arguments specified to this executable.
%
% Returns a pair made of these values and of the shrunk argument table.
%
-spec extract_command_argument( command_line_option() ) ->
			  { [ command_line_value() ], argument_table() }.
extract_command_argument( Option ) ->

	ArgumentTable = get_argument_table(),

	extract_command_argument( Option, ArgumentTable ).



% Extracts specified option, i.e. its various associated values (if any), from
% the specified argument table.
%
% Returns a pair made of these values and of the shrunk argument table.
%
-spec extract_command_argument( command_line_option(), argument_table() ) ->
			  { [ command_line_value() ], argument_table() }.
extract_command_argument( Option, ArgumentTable ) ->
	list_table:extractEntryWithDefaults( _K=Option, _DefaultValue=[],
										 ArgumentTable ).



% Returns a textual representation of specified argument table.
%
-spec argument_table_to_string( argument_table() ) -> string().
argument_table_to_string( ArgTable ) ->

	% No-op:
	case list_table:enumerate( ArgTable ) of

		[] ->
			"no command-line option specified";

		ArgPairs ->
			ArgStrings = [ case ArgumentLists of

				[] ->
					text_utils:format( "option '-~s'", [ Option ] );

				_ ->
					text_utils:format( "option '-~s', with argument lists: ~p",
									   [ Option, ArgumentLists ] )

						   end || { Option, ArgumentLists } <- ArgPairs ],

			text_utils:format( "~B command-line options specified "
							   "(listed alphabetically): ~s",
							   [ length( ArgPairs ),
						 text_utils:strings_to_sorted_string( ArgStrings ) ] )

	end.



% Miscellaneous section:


% Tells whether the program is run in batch mode (i.e. with the "--batch"
% command line argument).
%
-spec is_batch() -> boolean().
is_batch() ->

	case init:get_argument( '-batch' ) of

		{ ok, _ } ->
			true;

		_ ->
			false

	end.



% Helper functions.

-spec execute_dot( file_utils:file_name(), file_utils:file_name() ) ->
						 text_utils:ustring().
execute_dot( PNGFilename, GraphFilename ) ->

	DotExec = find_executable( "dot" ),

	Cmd = DotExec ++ " -o" ++ PNGFilename ++ " -Tpng " ++ GraphFilename,

	% Dot might issue non-serious warnings:
	case system_utils:run_executable( Cmd ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { rendering_failed, GraphFilename, PNGFilename, ExitCode,
					 ErrorOutput } )

	end.
