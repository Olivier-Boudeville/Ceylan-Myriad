% Copyright (C) 2018-2018 Olivier Boudeville
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
% Creation date: Wednesday, May 2, 2018



% This is the second most basic, terminal-based textual interface, with colors,
% dialog boxes, etc., based on the 'dialog' or 'whiptail' tools.
%
% See:
%
% - term_ui_test.erl for the corresponding test
% - text_ui.erl for a more basic text interface
% - gui.erl for a graphical counterpart
%
% See also: trace_utils.erl for another kind of output and test-dialog.sh for an
% autonomous, standalone test.
%
-module(term_ui).



% Implementation notes:
%
% In this very specific case, we use the process dictionary to avoid having to
% keep around a UI-state variable in all calls.
%
% We do not provide here counterpart functions dealing with an explicit state
% instead (too long, for too little interest, and too many arity clashes).




% Dialog-specific section.
%
% These dialogs will take advantage of the current locale (ex: 'OK' vs
% 'Accepter').
%
% See also: test-dialog.sh for a live demo.



% List of known types of dialogs:
%
% (see also: https://invisible-island.net/dialog/dialog-figures.html)


% 'msgbox' dialog: a modal window to be dismissed by hitting Enter
%
% - ex: LANG= dialog --title "Hello" --msgbox 'Hello world!' 6 20
%   (validated after Enter is pressed)


% 'yesno' (yes/no) dialog: a modal window offering two possibilities
%
% - ex: LANG= dialog --title "Message" --yesno "Are you having\n fun?" 6 25
%   (exit status: 0 if Yes, 1 otherwise (No or interrupted))


% 'infobox' dialog: a window displayed once
%
% - ex: LANG= dialog --infobox "Please wait" 10 30 ; sleep 4
%   (disappears after sleep)


% 'pause' dialog: pauses for a number of seconds
%
% - ex: LANG= dialog --pause "Pausing" 10 30 4
%   (disappears once pause is over)


% 'inputbox' dialog: a request for the user to type an echoed string
%
% - ex: LANG= dialog --inputbox "Enter your name:" 8 40
%   (input written to standard error, possibly redirected)


% 'passwordbox' dialog: a request for the user to type a non-echoed string
%
% - ex: LANG= dialog  --passwordbox "Enter some password:" 8 40
%	(no input echoed, but written to standard error, possibly redirected)


% 'textbox' dialog: displays the content of a file
%
% - ex: LANG= dialog --textbox /etc/profile 22 70


% 'menu' dialog: allows to select one option among a set (quite similar to
% radiolist)
%
% - ex: LANG= dialog --menu "Choose one:" 10 30 3 1 red 2 green 3 blue
%   (choice index written to standard error, possibly redirected)


% 'radiolist' dialog: allows to select one option among a set (quite similar to
% menu)
%
% - ex: LANG= dialog --radiolist "Select CPU type:" 10 40 4 1 386SX off 2 386DX
% on 3 486SX off 4 486DX off 2>${result_file}
%   (choice index written to standard error, possibly redirected)


% 'treeview' dialog: allows to select a tree element
%
% - ex: LANG= dialog --treeview "Select tree element:" 10 40 5 1 a on 1 2 b on 2
% 3 c off 1 4 d on 3
%   (element written to standard error, possibly redirected)


% 'checklist' dialog: allows to select non-exclusive options, thanks to a set of
% radio buttons with defaults
%
% - ex: LANG= dialog --checklist "Choose toppings:" 10 40 3 1 Cheese on 2
% "Tomato Sauce" on 3 Anchovies off
%   (choice indexes written to standard error, possibly redirected)


% 'calendar' dialog: allows to select a date
%
% - ex: LANG= dialog --calendar "Select a date:"  10 40 17 5 1977
%   (date written to standard error, possibly redirected)


% 'timebox' dialog: allows to select a time
%
% - ex: LANG= dialog --timebox "Select a time:"  10 40
%   (time written to standard error, possibly redirected)


% 'fselect' dialog: allows to select a file
%
% - ex: LANG= dialog --fselect / 10 40
%   (file path written to standard error, possibly redirected)


% 'dselect' dialog: allows to select a directory
%
% - ex: LANG= dialog --dselect / 10 40
%   (directory path written to standard error, possibly redirected)


% 'gauge' dialog: display percentage values
%
% - ex: LANG= dialog --gauge "My gauge:" 10 20 12



% 'tailbox' dialog:

% 'tailboxbg' dialog:

% 'progressbox' dialog:


% 'buildlist' dialog:

% 'editbox' dialog:

% 'form' dialog:

% 'inputmenu' dialog:

% 'mixedform' dialog:

% 'mixedgauge' dialog:

% 'passwordform' dialog:


% 'prgbox' dialog:

% 'programbox' dialog:


% 'rangebox' dialog:




% Also useful:
%
% - 'dialog --print-maxsize' (ex: MaxSize: 35, 123)
% - 'dialog --clear'



% Basic UI operations.
%
-export([ is_available/0,
		  start/0, start/1,
		  trace/1, trace/2,
		  stop/0,
		  to_string/0 ]).



% The default place where file-based communications are to occur:
-define( default_state_path, "/tmp/.myriad-term_ui.state" ).


-type dialog_tool() :: 'dialog' | 'whiptail'.


% The locale to be used by dialogs:
%
% - default: the current user one
% - none: no locale (defaulting to C)
% - a user-specified one
%
-type dialog_locale() :: 'default' | 'none' | string().


-record( term_ui_state, {
		   state_filename = ?default_state_path :: file_utils:file_path(),
		   dialog_tool :: dialog_tool(),
		   dialog_tool_path :: file_utils:file_name(),
		   locale = default :: dialog_locale(),
		   log_console = false :: boolean(),
		   log_file = undefined :: maybe( file_utils:file() )
}).


-type ui_state() :: #term_ui_state{}.


% For common, transverse defines:
-include("ui.hrl").


-export_type([ ui_state/0 ]).




% The key used by this module to store its state in the process dictionaty:
-define( state_key, term_ui_state ).



% Tells whether this user-interface backend is available.
%
-spec is_available() -> boolean().
is_available() ->

	case lookup_dialog_tool() of

		undefined ->
			false;

		% { T, TPath }:
		_ ->
			true

	end.



% Starts the UI with default settings.
%
% Stores the corresponding state in the process dictionary, yet returns as well
% that state, for any explicit later operation.
%
-spec start() -> ui_state().
start() ->
	start( _Opts=[] ).



% Starts the UI with specified settings.
%
% Stores the corresponding state in the process dictionary, yet returns as well
% that state, for any explicit later operation.
%
-spec start( ui_options() ) -> ui_state().
start( Options ) ->
	BlankUIState = #term_ui_state{},
	start( Options, BlankUIState ).


% (non-exported helper)
start( _Options=[], UIState ) ->

	DialogUIState = case lookup_dialog_tool() of

		undefined ->
			throw( no_dialog_tool_available );

		{ T, TPath } ->
			UIState#term_ui_state{ dialog_tool=T,
								   dialog_tool_path=TPath }

	end,

	case process_dictionary:put( ?ui_name_key, ?MODULE ) of

		undefined ->
			ok;

		UIName ->
			throw( { ui_already_started, UIName } )

	end,

	% No prior state expected:
	case process_dictionary:put( ?ui_state_key, DialogUIState ) of

		undefined ->
			ok;

		_ ->
			throw( term_ui_already_started )

	end,

	DialogUIState;

start( _Options=[ log_file | T ], UIState ) ->
	start( [ { log_file, "ui.log" } | T ], UIState );

start( _Options=[ { log_file, Filename } | T ], UIState ) ->
	LogFile = file_utils:open( Filename, [ write, exclusive ] ),
	file_utils:write( LogFile, "Starting term UI.\n" ),
	NewUIState = UIState#term_ui_state{ log_file=LogFile },
	start( T, NewUIState );

start( SingleElem, UIState ) ->
	start( [ SingleElem ], UIState ).



% Traces specified status string.
%
-spec trace( string() ) -> void().
trace( Text ) ->
	% Reused here:
	text_ui:trace( Text, get_state() ).


% Traces specified status string, by displaying it, and possibly logging it.
%
-spec trace( string(), ui_state() ) -> void().


% Stops the UI.
%
-spec stop() -> void().
stop() ->
	stop( get_state() ).



% Stops the UI.
%
-spec stop( ui_state() ) -> void().
stop( UIState=#term_ui_state{ log_file=undefined } ) ->
	stop_helper( UIState );

stop( UIState=#term_ui_state{ log_file=LogFile } ) ->
	file_utils:write( LogFile, "Stopping UI.\n" ),
	file_utils:close( LogFile ),
	stop_helper( UIState ).


% (helper)
%
stop_helper( #term_ui_state{ state_filename=StateFilename } ) ->

	file_utils:remove_file_if_existing( StateFilename ),

	process_dictionary:remove( ?ui_state_key ).




% Helper section.


% Tries to find a suitable dialog tool.
%
-spec lookup_dialog_tool() ->
					maybe( { dialog_tool(), file_utils:file_path() } ).
lookup_dialog_tool() ->

	case executable_utils:lookup_executable( "dialog" ) of

		false ->
			% Maybe in the future:
			%AcceptWhiptail = true,
			AcceptWhiptail = false,

			case AcceptWhiptail andalso
				executable_utils:lookup_executable( "whiptail" ) of

				false ->
					undefined;

				WPath ->
					{ whiptail, WPath }

			end;

		DPath ->
			{ dialog, DPath }

	end.



% Returns the current UI state.
%
% (helper)
%
-spec get_state() -> ui_state().
get_state() ->

	case process_dictionary:get( ?ui_state_key ) of

		undefined ->
			throw( term_ui_not_started );

		UIState ->
			UIState

	end.



% Returns a textual description of the (implicit) UI state.
%
-spec to_string() -> string().
to_string() ->
	to_string( get_state() ).


% Returns a textual description of the specified UI state.
%
-spec to_string( ui_state() ) -> string().
to_string( #term_ui_state{ state_filename=StateFilename,
						   dialog_tool=DialogTool,
						   dialog_tool_path=DialogToolPath,
						   locale=Locale,
						   log_console=LogConsole,
						   log_file=LogFile }) ->

	DialogString = text_utils:format( "~s (found in '~s')",
									  [ DialogTool, DialogToolPath ] ),

	LocaleString = text_utils:format( "using the ~s locale", [ Locale ] ),

	ConsoleString = case LogConsole of

		true ->
			"";

		false ->
			"not"

	end,

	FileString = case LogFile of

		undefined ->
			"not using a log file";

		_ ->
			text_utils:format( "using log file '~s'", [ LogFile ] )

	end,

	text_utils:format( "term_ui interface, using state file '~s' for tool ~s, "
					   "~s, ~s writing logs on console, and ~s",
					   [ StateFilename, DialogString, LocaleString,
						 ConsoleString, FileString ] ).
