% Copyright (C) 2003-2013 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Saturday, July 12, 2008.


% Gathering of various convenient facilities regarding executing of third-party
% tools.
%
% See executable_utils_test.erl for the corresponding test.
-module(executable_utils).




% Section for most usual commands:
-export([ generate_png_from_graph_file/2,
	generate_png_from_graph_file/3, display_png_file/1, browse_images_in/1,
		 display_pdf_file/1,
	display_text_file/1, display_wide_text_file/2, get_ssh_mute_option/0 ]).



% Section about default tools:
-export([ get_default_image_viewer/0, get_default_image_browser/0,
		 get_default_pdf_viewer/0,
		 get_default_text_viewer/0, get_default_wide_text_viewer/1,
		 get_default_trace_viewer/0, get_default_erlang_interpreter/0,
		 get_default_ssh_client/0, get_default_scp_executable/0 ]).



% Section for most usual commands:


% By default do not crash if dot outputs some warnings.
generate_png_from_graph_file( PNGFilename, GraphFilename ) ->
	generate_png_from_graph_file( PNGFilename, GraphFilename, false ).



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
generate_png_from_graph_file( PNGFilename, GraphFilename, true ) ->

	case execute_dot( PNGFilename, GraphFilename ) of

		[] ->
			ok;

		ErrorMessage ->
			throw( {graph_generation_failed,ErrorMessage} )

	end;

% Any output remains available to the caller.
generate_png_from_graph_file( PNGFilename, GraphFilename, false ) ->
	execute_dot( PNGFilename, GraphFilename ).



% Displays (without blocking) to the user the specified PNG, using an external
% viewer.
display_png_file( PNGFilename ) ->
	% Viewer output is ignored:
	os:cmd( get_default_image_viewer() ++ " " ++ PNGFilename ++ " &" ).



% Allows to browser the images available in specified directory (specified as a
% plain string).
browse_images_in( DirectoryName ) ->
	os:cmd( get_default_image_browser() ++ " " ++ DirectoryName ++ " &" ).



% Displays (without blocking) to the user the specified PNG, using an external
% viewer.
display_pdf_file( PDFFilename ) ->
	% Viewer output is ignored:
	os:cmd( get_default_pdf_viewer() ++ " " ++ PDFFilename ++ " &" ).


% Displays, with blocking, a text file.
display_text_file( TextFilename ) ->
	% Viewer output is ignored:
	os:cmd( get_default_text_viewer() ++ " " ++ TextFilename ).


% Displays, with blocking, a wide text file.
display_wide_text_file( TextFilename, CharacterWidth ) ->
	% Viewer output is ignored:
	os:cmd( get_default_wide_text_viewer(CharacterWidth) ++ " "
		   ++ TextFilename ).


% Returns a string to be inserted into a command-line call to ssh/scp so that it
% can run as much as possible non-interactively.
%
% Tries notably to avoid following message: "The authenticity of host 'Server
% (XXXXX)' can't be established.  RSA key fingerprint is YYYYY. Are you sure you
% want to continue connecting (yes/no)?": Note: only to be used in a trusted
% environment.
get_ssh_mute_option() ->
  " -o \"StrictHostKeyChecking no\" ".



% Section about default tools:


% Returns the name of the default image viewer tool.
% Could be also: xv, firefox, etc.
get_default_image_viewer() ->
	% Viewer is 'eye of gnome' here:
	"eog".


% Returns the name of the default image browser tool.
% Could be also: geeqie (new name)
get_default_image_browser() ->
	% Compatibility alias:
	"gqview".


% Returns the name of the default PDF viewer tool.
% Could be also: xpdf, acroread, etc.
get_default_pdf_viewer() ->
	"evince".


% Returns the name of the default text viewer tool.
% Could be also: nedit, emacs, etc.
get_default_text_viewer() ->
	"gedit".


% Returns the name of the default viewer tool for wider texts.
get_default_wide_text_viewer(_CharacterWidth) ->
	% Could be: io_lib:format( "nedit -column ~B", [CharacterWidth] )
	"gedit".


% Returns the default trace viewer tool.
% Could be also: nedit, gedit, etc.
get_default_trace_viewer() ->
	% Note: expected to be on the PATH:
	"logmx.sh".


% Returns the path to the default Erlang interpreter.
get_default_erlang_interpreter() ->
	% Note: expected to be on the PATH:
	"erl".


% Returns the path to the default SSH client.
get_default_ssh_client() ->
	% Note: expected to be on the PATH:
	"ssh".


% Returns the path to the default SSH-based scp executable.
get_default_scp_executable() ->
	% Note: expected to be on the PATH:
	"scp".



% Helper functions.


execute_dot( PNGFilename, GraphFilename ) ->
	% Dot might issue non-serious warnings:
	os:cmd( "dot -o" ++ PNGFilename ++ " -Tpng " ++ GraphFilename ).
