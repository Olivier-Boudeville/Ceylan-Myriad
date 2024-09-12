% Copyright (C) 2024-2024 Olivier Boudeville
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
% Creation date: Wednesday, July 17, 2024.

-module(gui_shell).

-moduledoc """
A GUI component hosting an **Erlang shell**, like an Erlang interpreter (a kind
of REPL) with which the user can interact (input/output) graphically.

This graphical module relies on our shell_utils one.
""".



-doc """
Designates an actual GUI shell instance.

Not to be mixed up with shell_utils:shell_pid().
""".
-type gui_shell() :: widget_pid().

-export_type([ gui_shell/0 ]).



-export([ create/1, create/2, create/3, destruct/1 ]).



% For myriad_spawn_link:
-include_lib("myriad/include/spawn_utils.hrl").



% Type shorthands:

-type bin_string() :: text_utils:bin_ustring().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type timestamp_binstring() :: time_utils:timestamp_binstring().

-type parent() :: gui:parent().
-type backend_environment() :: gui:backend_environment().
-type widget_pid() :: gui_widget:widget_pid().

-type font_size() :: gui_font:font_size().

-type text_editor() :: gui_text_editor:text_editor().

-type shell_pid() :: shell_utils:shell_pid().
-type shell_option() :: shell_utils:shell_option().
-type command_id() :: shell_utils:command_id().



% Implementation notes:
%
% A GUI shell is, in MVC parlance, the View and Controller of an actual shell
% (see shell_utils), which is the Model at hand here.
%
% A GUI shell corresponds, graphically, to an horizontal sizer; the top part is
% a read-only text editor displaying the past operations of that shell, while
% the bottom part is a (smaller) editable text editor, for the next command.

% wxStyledTextCtrl could be used instead of wxTextCtrl.
%
% Using the Scintilla-compliant Erlang lexers (see wxSTC_ERLANG_*) could be
% convenient, yet it is a rather enormous, complex API, and there is little
% interest in syntax-highlighting the user inputs or the interpreter outputs.
%
% Intercepting inputs on a character basis could allow to offer smart key
% shortcuts (e.g. like Emacs), for example to navigate in the history or offer
% autocompletion.
%
% Indeed, rather than using a gui_text_editor for the input commands like here,
% the entered characters shall be read one by one (an event each, rather than as
% a whole string when enter is pressed) and possibly smartly integrated if
% needed (so that Ctrl-A for example is considered as such), so that special
% characters can be specifically handled (e.g. the down arrow, to recall a past
% command, or even for some kind of auto-completion to be implemented).
%
% This could be done with a static text display (see gui_text_display), updated
% based on its subscription to the onKeyPressed, onKeyReleased, onCharEntered,
% etc. events.



-define( default_font_size, 10 ).


% The (internal) state of a GUI shell instance:
-record( gui_shell_state, {

	% The editor used by the shell for the input commands:
	command_editor :: text_editor(),

	% The (read-only) editor displaying the past operations:
	% (different from the shell's history)
	%
	past_ops_editor :: text_editor(),

	% The full text corresponding to the past operations:
	past_ops_text :: bin_string(),

	% The PID of the underlying actual shell logic:
	shell_pid :: shell_pid() } ).


-doc "The (internal) state of a GUI shell instance.".
-type gui_shell_state() :: #gui_shell_state{}.




-doc """
Creates a GUI shell, in the specified parent window, using the default font
size, and shell defaults.
""".
-spec create( parent() ) -> gui_shell().
create( ParentWindow ) ->
	create( ?default_font_size, ParentWindow ).



-doc """
Creates a GUI shell, in the specified parent window, using the specified font
size, and shell defaults.
""".
-spec create( font_size(), parent() ) -> gui_shell().
create( FontSize, ParentWindow ) ->
	create( FontSize, _ShellOpts=[], ParentWindow ).



-doc """
Creates a GUI shell, in the specified parent window, using the specified font
size and shell options.
""".
-spec create( font_size(), maybe_list( shell_option() ), parent() ) ->
											gui_shell().
create( FontSize, ShellOpts, ParentWindow ) ->

	% Not wanting a crash of the shell_utils'shell to crash us in turn:
	process_flag( trap_exit, true ),

	BackendEnv = gui:get_backend_environment(),

	% At least currently, linking to the caller process:
	GUIShellPid = ?myriad_spawn_link(
		fun() ->
			start_gui_shell( FontSize, ShellOpts, BackendEnv, ParentWindow )
		end ),

	cond_utils:if_defined( myriad_debug_gui_shell,
		trace_utils:debug_fmt( "Created GUI shell ~w.", [ GUIShellPid ] ) ),


	GUIShellPid.



-doc "Destructs the specified GUI shell.".
-spec destruct( gui_shell() ) -> void().
destruct( GUIShellPid ) ->

	cond_utils:if_defined( myriad_debug_gui_shell,
		trace_utils:debug_fmt( "Terminating GUI shell ~w.", [ GUIShellPid ] ) ),

	% (asynchronous)
	GUIShellPid ! destruct.




% Helper functions.


-doc """
Starts a GUI shell process, using the specified font size, shell options and
backend environment.
""".
-spec start_gui_shell( font_size(), maybe_list( shell_option() ),
					   backend_environment(), parent() ) -> no_return().
start_gui_shell( FontSize, ShellOpts, BackendEnv, ParentWindow ) ->

	ActualShellPid = shell_utils:start_link_custom_shell( ShellOpts ),

	gui:set_backend_environment( BackendEnv ),

	VSizer = gui_sizer:create( _Orientation=vertical ),

	gui_widget:set_sizer( ParentWindow, VSizer ),


	HSizer = gui_sizer:create( _Or=horizontal ),

	gui_widget:set_sizer( ParentWindow, VSizer ),

	PromptButton = gui_button:create( _NoLabel="", _Position=auto,
		_ButtonSize=auto, _ButtonStyle=[], _BId=prompt_black_button,
		ParentWindow ),

	CmdEditor = gui_text_editor:create(
		[ { style, [ multiline, process_enter_key ] } ], ParentWindow ),

	gui_widget:set_focus( CmdEditor ),


	gui_sizer:add_element( HSizer, PromptButton,
		[ expand_fully, { proportion, _Fixed=0 } ] ),

	gui_sizer:add_element( HSizer, CmdEditor,
		[ expand_fully, { proportion, _Resizable=1 } ] ),


	PastOpsEditor = gui_text_editor:create(
		_Opts=[ % Too early, font not set: {text, InitText},
				{ style, [ multiline, read_only, word_wrap ] } ],
		ParentWindow ),

	% We prefer that the printouts for past operations cannot be selected:
	%gui_widget:set_enable_status( PastOpsEditor, _DoEnable=false ),

	% For example Monospace 11:
	ShellFont = gui_font:create( FontSize, _FontFamily=modern ),

	[ gui_text_editor:set_default_font( Ed, ShellFont )
		|| Ed <- [ CmdEditor, PastOpsEditor ] ],

	gui_font:destruct( ShellFont ),

	InitBinText = text_utils:bin_format(
		"Welcome to the MyriadGUI shell ~w on node ~ts.~n~n",
		[ self(), net_utils:localnode() ] ),

	% Not wanting an onEnterPressed event for that:
	gui_text_editor:set_text( PastOpsEditor, InitBinText ),

	% Past operations at the top, new commands at bottom:
	gui_sizer:add_element( VSizer, PastOpsEditor,
						   [ expand_fully, { proportion, 1 } ] ),

	gui_sizer:add_element( VSizer, HSizer,
						   [ expand_fully, { proportion, 0 } ] ),

	gui:subscribe_to_events( { onEnterPressed, CmdEditor } ),


	% So that the editors take their actual size from the start:
	gui_widget:layout( ParentWindow ),

	InitShellState = #gui_shell_state{ command_editor=CmdEditor,
									   past_ops_editor=PastOpsEditor,
									   past_ops_text= InitBinText,
									   shell_pid=ActualShellPid },

	gui_shell_main_loop( InitShellState ).



-doc "Main loop of the GUI shell process.".
-spec gui_shell_main_loop( gui_shell_state() ) -> no_return().
gui_shell_main_loop( GUIShellState=#gui_shell_state{
										command_editor=CmdEditor,
										past_ops_editor=PastOpsEditor,
										past_ops_text=PastOpsText,
										shell_pid=ShellPid } ) ->

	receive

		{ onEnterPressed, [ _CmdEditor, _EditorId, NewText, _Context ] } ->

			cond_utils:if_defined( myriad_debug_gui_shell,
				trace_utils:debug_fmt( "Read command '~ts'.", [ NewText ] ) ),

			CmdBinStr = text_utils:string_to_binary( NewText ),

			{ BaseText, MaybeTmstpBinStr } =
					case shell_utils:execute_command( CmdBinStr, ShellPid ) of

				{ success, CmdRes, CmdId, MaybeTimestampBinStr } ->
					{ text_utils:bin_format( "~ts~ts~n~ts",
						[ get_prompt_for( CmdId ), CmdBinStr,
						  text_utils:term_to_binary( CmdRes ) ] ),
					  MaybeTimestampBinStr };

				{ error, CmdError, CmdId, MaybeTimestampBinStr } ->
					{ text_utils:bin_format( "~ts~ts~n~ts",
						[ get_prompt_for( CmdId ), CmdBinStr, CmdError ] ),
					  MaybeTimestampBinStr }

			end,

			AddText = format_text( BaseText, MaybeTmstpBinStr ),

			NewPastOpsText = text_utils:bin_concatenate( PastOpsText, AddText ),

			gui_text_editor:set_text( PastOpsEditor, NewPastOpsText ),

			gui_text_editor:show_text_end( PastOpsEditor ),

			%gui_text_editor:set_text( CmdEditor, get_prompt_for( NextCmdId ) ),
			gui_text_editor:clear( CmdEditor ),

			%trace_utils:info_fmt(
			%  "Shell read: '~ts'; new past operations are: <<<~n~ts>>>",
			%  [ NewText, NewPastOpsText ] ),

			gui_shell_main_loop( GUIShellState#gui_shell_state{
				past_ops_text=NewPastOpsText } );

		destruct ->
			[ gui_text_editor:destruct( Ed )
				|| Ed <- [ CmdEditor, PastOpsEditor ] ],
			cond_utils:if_defined( myriad_debug_gui_shell,
				trace_utils:debug_fmt( "GUI shell ~w terminated.",
									   [ self() ] ) );

		Other ->
			trace_utils:warning_fmt( "GUI shell loop ignored the following "
									 "message:~n ~p.", [ Other ] ),
			gui_shell_main_loop( GUIShellState )

	end.





-doc """
Returns the full prompt corresponding to the specified command identifier.
""".
-spec get_prompt_for( command_id() ) -> bin_string().
get_prompt_for( CmdId ) ->
	text_utils:bin_format( "~B> ", [ CmdId ] ).



-doc "Formats the specified text, possibly with a timestamp.".
-spec format_text( bin_string(), option( timestamp_binstring() ) ) ->
											bin_string().
format_text( Text, _MaybeTimestampBinStr=undefined ) ->
	text_utils:bin_format( "~ts~n", [ Text ] );

format_text( Text, TimestampBinStr ) ->
	text_utils:bin_format( "[~ts] ~ts~n", [ TimestampBinStr, Text ] ).
