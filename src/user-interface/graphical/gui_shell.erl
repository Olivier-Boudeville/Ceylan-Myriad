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

Keyboard shortcuts for this shell (partly Emacs-inspired):
- Ctrl-a: to beginning of command line
- Ctrl-e: to end of command line
- Ctrl-k: clear command line from cursor
- Ctrl-c: clear current command (instead of killing/going in Erlang BREAK
  mode/starting an Emacs sequence)
- Ctrl-z: restore any previous editing line (i.e. not the previous command);
  useful for example after a faulty paste

Special-purpose keys:
- Delete: delete any character at cursor
- Backspace: delete any character just previous cursor
- Left/Right arrows: move left/right in command line
- Up/Down: recall previous/next command
- Return: triggers the currently edited command

Text can be intentionally:
 - pasted in the editor (typically with the mouse), at the end of current
   command
 - selected in the past commands (e.g. for re-use)

Shell built-in commands (their shorter names comply with a subset of the ones
documented in
https://www.erlang.org/doc/apps/stdlib/shell.html#module-shell-commands):

 - list_bindings() or b(): lists (as terms) the current variable bindings
 - print_bindings(): displays the current variable bindings
 - clear_bindings() or f() (presumably for forget): clears all variable bindings
 - clear_binding(V) or f(V): clears the binding of variable V

 - print_command_history() or h(): displays the current history of commands
 - print_result_history() or h(): displays the current history of results

 - repeat_command(Id): re-evaluates the command of the specified identifier (if
   it is still in command history)


 - get_result(Id): returns the result corresponding to the command of specified
   identifier (if still in result history)
 - clear_commands() or fc(): clears the full (live) history of commands
 - clear_results()  or fr(): clears the full history of command results
 - set_command_history_depth(D): sets the depth of the command history to D
 - set_result_history_depth(D): sets the depth of the result history to D
 - clear_persistent_command_history(): clears the persistent history of commands

See our shell_default_callbacks module for their detailed signatures; note the
implicit use of shell state variables.

Separate histories, of arbitrary depths, for the commands and their results are
managed.

The command editing is based on a single line so, at least currently, no
multi-line command editing is supported (no series of lines prefixed with '.. '
are displayed). As a result, the terminal dot can be added automatically if
lacking.

The sending of shell commands is synchronous: this widget blocks untils the
shell answers about the corresponding outcome.

Timestamps are determined in the context of the actual (potentially remote)
shell (not of this widget).

This graphical module relies on our shell_utils one.
""".



-doc """
Designates an actual GUI shell instance.

Not to be mixed up with shell_utils:shell_pid().
""".
-type gui_shell() :: widget_pid().



-doc """
The available options when creating a GUI shell.

They include the ones when creating a text edit.
""".
-type gui_shell_option() :: text_edit_option()

	% Whether initially the shell has the event focus:
  | 'focused'.


-export_type([ gui_shell/0, gui_shell_option/0 ]).



-export([ create/1, create/2, create/3, destruct/1 ]).


% Silencing:
-export([ text_state_to_string/1 ]).



% For myriad_spawn_link:
-include_lib("myriad/include/spawn_utils.hrl").


% For scancodes:
-include("ui_keyboard_scancodes.hrl").

% For keycodes:
-include("ui_keyboard_keycodes.hrl").



% Type shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type timestamp_binstring() :: time_utils:timestamp_binstring().

-type command_id() :: shell_utils:command_id().


-type parent() :: gui:parent().

-type backend_environment() :: gui:backend_environment().

-type widget_pid() :: gui_widget:widget_pid().

-type font_size() :: gui_font:font_size().

-type backend_keyboard_event() :: gui_keyboard:backend_keyboard_event().

-type text_editor() :: gui_text_editor:text_editor().

-type text_edit() :: text_edit:text_edit().
-type text_edit_option() :: text_edit:text_edit_option().

-type prefix_info() :: text_edit:prefix_info().



% Implementation notes:
%
% A GUI shell is, in MVC parlance, the View and Controller of an actual shell
% (see shell_utils), which is the Model at hand here. To factor the line-editing
% logic (e.g. between different forms of shells), it has been factored in the
% text_edit module.
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
% Intercepting inputs on a character basis is now done, as least so that past
% commands can be recalled (by pressing the down arrow); this could allow to
% offer smart key shortcuts (e.g. like Emacs), for example to navigate in the
% history or offer autocompletion.
%
% Indeed, rather than using a gui_text_editor for the input commands like here,
% the entered characters shall be read one by one (an event each, rather than as
% a whole string when enter is pressed) and possibly smartly integrated if
% needed (so that Ctrl-A for example is considered as such), so that special
% characters can be specifically handled (e.g. the down arrow, to recall a past
% command, or even for some kind of auto-completion to be implemented).
%
% Initially, for the command editor, a gui_text_editor was used, first while
% subscribing to its onEnterPressed events (yet no cursor control could be
% done), then to its onTextUpdated events (then the wxWidgets cursor control
% would apply, not ours; for example the down arrow would set the cursor to the
% start of text and the up arrow to its end, automatically, without generating
% any event, whereas we want to intercept them to go through the command
% history.
%
% To restore full (cursor) control, lower-level control is needed, obtained with
% a static text display (see gui_text_display), updated based on its
% subscription to the onKeyPressed, onKeyReleased, onCharEntered, etc. events.

% Helper functions that may be used by all kinds of shell interfaces are defined
% in the shell_utils module, for a better centralisation thereof.


-define( default_font_size, 10 ).


% The (internal) state of a GUI shell instance.
%
% Kept minimal, so that anything related to edition is factored in text_edit.
%
-record( gui_shell_state, {

	% The (read-only) editor displaying the past operations:
	% (different from the shell's history)
	%
	past_ops_editor :: text_editor(),

	% The full text corresponding to the past operations:
	past_ops_text :: bin_string(),


	% The editor used by the shell for the input commands:
	%
	% (a simple text_display() would not suffice, for example no cursor would be
	% shown)
	%
	command_editor :: text_editor(),

	% The state of a general-purpose text edition facility:
	text_edit :: text_edit() } ).


-doc "The (internal) state of a GUI shell instance.".
-type gui_shell_state() :: #gui_shell_state{}.





-doc """
Creates a GUI shell, in the specified parent window, using the default font
size, and shell defaults.

A typical parent window is a panel.
""".
-spec create( parent() ) -> gui_shell().
create( ParentWindow ) ->
	create( ?default_font_size, ParentWindow ).



-doc """
Creates a GUI shell, in the specified parent window, using the specified font
size, and shell defaults.

A typical parent window is a panel.
""".
-spec create( font_size(), parent() ) -> gui_shell().
create( FontSize, ParentWindow ) ->
	create( FontSize, _ShellOpts=[], ParentWindow ).



-doc """
Creates a GUI shell, in the specified parent window, using the specified font
size and shell options.

A typical parent window is a panel.
""".
-spec create( font_size(), maybe_list( gui_shell_option() ), parent() ) ->
											gui_shell().
create( FontSize, GUIShellOpts, ParentWindow ) ->

	BackendEnv = gui:get_backend_environment(),

	% At least currently, linking to the caller process:
	GUIShellPid = ?myriad_spawn_link(
		fun() ->
			start_gui_shell( FontSize, GUIShellOpts, BackendEnv, ParentWindow )
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
-spec start_gui_shell( font_size(), maybe_list( gui_shell_option() ),
					   backend_environment(), parent() ) -> no_return().
start_gui_shell( FontSize, MaybeGUIShellOpts, BackendEnv, ParentWindow ) ->

	% We used not to want a crash of the shell_utils'shell process to crash us
	% in turn:
	%
	%process_flag( trap_exit, true ),

	GUIShellOpts = list_utils:ensure_list( MaybeGUIShellOpts ),

	% Splits between GUI shell, the actual text edit options and the
	% shell-specific ones:

	{ SetFocus, OtherOpts } =
			case list_utils:extract_element_if_existing( _FElem=focused,
														 GUIShellOpts ) of

		false ->
			{ false, GUIShellOpts };

		OthOpts ->
			{ true, OthOpts }

	end,

	% The remaining options are by design the shell ones:
	{ AutoAddTrailingDot, WrapCursor, ShellOpts } =
		text_edit:filter_options( OtherOpts ),

	% So now we are able to create our text edit processor, here a shell:
	ActualShellPid = shell_utils:start_link_custom_shell( ShellOpts ),

	% Corresponds here to command submission count, which is not necessarily
	% zero (e.g. if connecting to a reloaded or already live shell):
	%
	% (some interleaving)
	ActualShellPid ! { getEntryCount, [], self() },


	% Initialising in parallel the GUI:

	gui:set_backend_environment( BackendEnv ),

	VSizer = gui_sizer:create( _Orientation=vertical ),

	gui_widget:set_sizer( ParentWindow, VSizer ),


	HSizer = gui_sizer:create( _Or=horizontal ),

	PromptButton = gui_button:create( _NoLabel="", _Position=auto,
		_ButtonSize=auto, _ButtonStyle=[], _BId=prompt_black_button,
		ParentWindow ),

	CmdEditor = gui_text_editor:create(
		[ { style, [] } ], ParentWindow  ),
		%[ { style, [ multiline, process_enter_key ] } ], ParentWindow  ),

	SetFocus andalso gui_widget:set_focus( CmdEditor ),

	gui_sizer:add_element( HSizer, PromptButton,
		[ expand_fully, { proportion, _Fixed=0 } ] ),

	gui_sizer:add_element( HSizer, CmdEditor,
		% All the available width shall be used:
		[ expand_fully, { proportion, _Resizable=1 } ] ),


	PastOpsEditor = gui_text_editor:create(
		_Opts=[ % Too early, font not set: {text, InitText},
				{ style, [ multiline, read_only, word_wrap ] } ],
		ParentWindow ),

	% We prefer that the printouts for past operations cannot be selected:
	%gui_widget:set_enable_status( PastOpsEditor, _DoEnable=false ),

	% For example Monospace 11:
	ShellFont = gui_font:create( FontSize, _FontFamily=modern ),

	gui_text_editor:set_default_font( PastOpsEditor, ShellFont ),

	gui_font:destruct( ShellFont ),

	InitBinText = text_utils:bin_format(
		"Welcome to the MyriadGUI shell ~w (whose GUI is ~w), on node ~ts.~n",
		[ ActualShellPid, self(), net_utils:localnode() ] ),

	% Not wanting an onEnterPressed event for that:
	gui_text_editor:set_text( PastOpsEditor, InitBinText ),

	% Past operations at the top, new commands at bottom:
	gui_sizer:add_element( VSizer, PastOpsEditor,
						   [ expand_fully, { proportion, 1 } ] ),

	gui_sizer:add_element( VSizer, HSizer,
						   [ expand_fully, { proportion, 0 } ] ),

	% To collect commands, set focus, react to paste commands:
	gui:subscribe_to_events( { [ onCharEntered, onMouseLeftButtonPressed,
								 onTextUpdated ], CmdEditor } ),


	% So that the editors take their actual size from the start:
	gui_widget:layout( ParentWindow ),

	% Interleaved for getEntryCount/0:
	{ NextCmdId, HistBinText } = receive

		{ entry_count, _CurrentEntryCount=0 } ->
			HBinText = text_utils:bin_format(
				"~ts(no command history available to reload)~n",
				[ InitBinText ] ),
			{ 1, HBinText };

		{ entry_count, _CurrentEntryCount=1 } ->
			HBinText = text_utils:bin_format(
				"~ts(a history of a single command reloaded)~n",
				[ InitBinText ] ),
			{ 2, HBinText };

		{ entry_count, CurrentEntryCount } ->
			HBinText = text_utils:bin_format(
				"~ts(past history of ~B commands reloaded)~n",
				[ InitBinText, CurrentEntryCount ] ),
			% As this is the next command:
			{ CurrentEntryCount+1, HBinText }

	end,

	% And then the corresponding text edit, to which the ownership of that shell
	% is transferred; prefix set just after:
	%
	TextEdit = text_edit:create( _ProcessorPid=ActualShellPid, NextCmdId,
		AutoAddTrailingDot, WrapCursor ),

	NewTextEdit = edit_new_command( TextEdit, CmdEditor ),

	gui_text_editor:set_text( PastOpsEditor, HistBinText ),

	cond_utils:if_defined( myriad_debug_gui_shell,
		trace_utils:debug_fmt( "Initial ~ts.",
							   [ text_edit:to_string( NewTextEdit ) ] ) ),

	InitShellState = #gui_shell_state{ command_editor=CmdEditor,
									   past_ops_editor=PastOpsEditor,
									   past_ops_text=HistBinText,
									   text_edit=NewTextEdit },

	gui_shell_main_loop( InitShellState ).



-doc "Prepares for the edition of the specified new command.".
-spec edit_new_command( text_edit(), text_editor() ) -> text_edit().
edit_new_command( TE, CmdEditor ) ->

	CmdId = text_edit:get_entry_id( TE ),

	PfxInfo = { _Pfx, PfxLen } = get_prefix_info( CmdId ),

	NewTE = text_edit:set_prefix( TE, PfxInfo ),

	apply_text( NewTE, CmdEditor ),
	gui_text_editor:set_cursor_position( CmdEditor, PfxLen+1 ),

	NewTE.




-doc """
Returns the (immutable) string before the leftmost position of the cursor,
together with its length.
""".
-spec get_prefix_info( command_id() ) -> prefix_info().
get_prefix_info( CmdId ) ->
	Str = text_utils:format( "~B> ", [ CmdId ] ),
	{ Str, length( Str ) }.



-doc "Main loop of the GUI shell process.".
-spec gui_shell_main_loop( gui_shell_state() ) -> no_return().
gui_shell_main_loop( GUIShellState ) ->

	cond_utils:if_defined( myriad_debug_gui_shell,
		trace_utils:debug_fmt( "GUI shell main loop editing '~ts'.",
			[ text_edit:get_entry(
				GUIShellState#gui_shell_state.text_edit ) ] ) ),

	receive

		{ onCharEntered, [ _CmdEditor, _CmdPanelId, EventContext ] } ->

			BackendKeyEvent = gui_keyboard:get_backend_event( EventContext ),

			NewGUIShellState =
					case gui_keyboard:is_control_pressed( BackendKeyEvent ) of

				true ->
					handle_ctrl_modified_key( BackendKeyEvent, GUIShellState );

				false ->
					handle_non_ctrl_modified_key( BackendKeyEvent,
												  GUIShellState )

			end,

			%trace_utils:debug( text_state_to_string( NewGUIShellState ) ),

			gui_shell_main_loop( NewGUIShellState );


		% To restore focus on command display widget:
		{ onMouseLeftButtonPressed,
				[ CmdEditor, _CmdEditorId, _EventContext ] } ->

			cond_utils:if_defined( myriad_debug_gui_shell,
				trace_utils:debug_fmt( "Setting the focus on the command "
									   "editor ~w.", [ CmdEditor ] ) ),

			gui_widget:set_focus( CmdEditor ),

			% Allows to avoid that a mouse click selects the full text, warps
			% the cursor position and leads to a confusing non-operation; now
			% has the same effect as Ctrl-e (oatherwise EventContext /
			% xYToPosition(This, X, Y) might be used):

			TextEdit = GUIShellState#gui_shell_state.text_edit,

			NewTextEdit = text_edit:set_cursor_to_end_of_line( TextEdit ),

			% More reliable than:
			% gui_text_editor:set_cursor_position_to_end( CmdEditor ),
			%
			apply_cursor_position( NewTextEdit, CmdEditor ),

			NewGUIShellState = GUIShellState#gui_shell_state{
				text_edit=NewTextEdit },

			gui_shell_main_loop( NewGUIShellState );


		% Typically called when pasting text with the mouse:
		{ onTextUpdated,
				[ CmdEditor, _CmdEditorId, NewText, _EventContext ] } ->

			%trace_utils:debug_fmt( "onTextUpdated: got '~ts'.", [ NewText ] ),

			% We obtain the whole new line, including the prefix, which should
			% thus be removed first:

			TextEdit = GUIShellState#gui_shell_state.text_edit,

			Prefix = text_edit:get_prefix( TextEdit ),

			ActualCmdStr = case text_utils:split_after_prefix( Prefix,
															   NewText ) of

				no_prefix ->
					"";

				Rest ->
					Rest

			end,

			NewTextEdit = set_full_command( Prefix, ActualCmdStr, CmdEditor,
											TextEdit ),

			gui_shell_main_loop( GUIShellState#gui_shell_state{
								   text_edit=NewTextEdit } );


		acquireFocus ->
			gui_widget:set_focus(
				GUIShellState#gui_shell_state.command_editor ),

			gui_shell_main_loop( GUIShellState );


		destruct ->

			% Destroying widgets (editor, panel, display, etc.) probably useless
			% (done through parent window).
			%
			%gui_text_editor:destruct( PastOpsEditor ),

			cond_utils:if_defined( myriad_debug_gui_shell,
				trace_utils:debug_fmt( "GUI shell ~w terminated.",
									   [ self() ] ),
				ok);


		Other ->
			trace_utils:warning_fmt( "GUI shell loop ignored the following "
									 "message:~n ~p.", [ Other ] ),
			gui_shell_main_loop( GUIShellState )


	end.



-doc "Handles a key with a Control modifier.".
-spec handle_ctrl_modified_key( backend_keyboard_event(),
								gui_shell_state() ) -> gui_shell_state().
handle_ctrl_modified_key( BackendKeyEvent, GUIShellState=#gui_shell_state{
												command_editor=CmdEditor,
												text_edit=TextEdit } ) ->

	Keycode = gui_keyboard:get_keycode( BackendKeyEvent ),

	%cond_utils:if_defined( myriad_debug_gui_shell, trace_utils:debug_fmt(
	%   "Keycode with Control modifier received: ~p (~ts)", [ Keycode,
	%       gui_keyboard:key_event_to_string( BackendKeyEvent ) ] ) ),

	case Keycode of

		% Ctrl-a:
		?MYR_K_CTRL_A ->

			cond_utils:if_defined( myriad_debug_gui_shell,
								   trace_utils:debug( "To start of line." ) ),

			NewTextEdit = text_edit:set_cursor_to_start_of_line( TextEdit ),

			% Better than setting directly the cursor after prefix:
			apply_cursor_position( NewTextEdit, CmdEditor ),

			GUIShellState#gui_shell_state{ text_edit=NewTextEdit };


		% Ctrl-e:
		?MYR_K_CTRL_E ->

			cond_utils:if_defined( myriad_debug_gui_shell,
								   trace_utils:debug( "To end of line." ) ),

			NewTextEdit = text_edit:set_cursor_to_end_of_line( TextEdit ),

			% More reliable than:
			% gui_text_editor:set_cursor_position_to_end( CmdEditor ),
			%
			apply_cursor_position( NewTextEdit, CmdEditor ),

			GUIShellState#gui_shell_state{ text_edit=NewTextEdit };


		% Ctrl-k:
		?MYR_K_CTRL_K ->

			cond_utils:if_defined( myriad_debug_gui_shell,
				trace_utils:debug( "Killing to end of line." ) ),

			NewTextEdit = text_edit:kill_from_cursor( TextEdit ),

			apply_text( NewTextEdit, CmdEditor ),

			% No cursor change.

			GUIShellState#gui_shell_state{ text_edit=NewTextEdit };


		% Ctrl-c:
		?MYR_K_CTRL_C ->

			cond_utils:if_defined( myriad_debug_gui_shell,
				trace_utils:debug( "Clearing command." ) ),

			NewTextEdit = text_edit:clear( TextEdit ),

			apply_text( NewTextEdit, CmdEditor ),

			% Resets the cursor:
			apply_cursor_position( NewTextEdit, CmdEditor ),

			GUIShellState#gui_shell_state{ text_edit=NewTextEdit };


		% Ctrl-z:
		?MYR_K_CTRL_Z ->

			cond_utils:if_defined( myriad_debug_gui_shell,
				trace_utils:debug( "Restoring previous line." ) ),

			NewTextEdit = text_edit:restore_previous_line( TextEdit ),

			apply_text( NewTextEdit, CmdEditor ),

			% No cursor change.

			GUIShellState#gui_shell_state{ text_edit=NewTextEdit };


		% Holding 'Alt Gr' (e.g. in an attempt to obtain a '[' character, on a
		% French keyboard) results into the control and alt modifiers being set,
		% with a corresponding Unicode character available:
		%
		_Other ->

			%cond_utils:if_defined( myriad_debug_gui_shell,
			%   trace_utils:debug_fmt(
			%       "(received keycode with Ctrl modifier ~p)", [ Other ] ),
			%       basic_utils:ignore_unused( Other ) ),

			NewTextEdit = text_edit:add_char( Keycode, TextEdit ),

			apply_text( NewTextEdit, CmdEditor ),

			% More reliable than:
			% gui_text_editor:offset_cursor_position( CmdEditor, _PosOffset=1 ),
			%
			apply_cursor_position( NewTextEdit, CmdEditor ),

			GUIShellState#gui_shell_state{ text_edit=NewTextEdit }

	end.



-doc "Handles a key with no Control modifier.".
-spec handle_non_ctrl_modified_key( backend_keyboard_event(),
									gui_shell_state() ) -> gui_shell_state().
handle_non_ctrl_modified_key( BackendKeyEvent, GUIShellState=#gui_shell_state{
												command_editor=CmdEditor,
												text_edit=TextEdit } ) ->

	Scancode = gui_keyboard:get_scancode( BackendKeyEvent ),

	%cond_utils:if_defined( myriad_debug_gui_shell, trace_utils:debug_fmt(
	%   "Scancode (with no Control modifier) received: ~p (~ts).",
	%   [ Scancode, gui_keyboard:key_event_to_string( BackendKeyEvent ) ] ) ),

	case Scancode of


		% Left arrow (in edited text):
		?MYR_SCANCODE_LEFT ->

			%cond_utils:if_defined( myriad_debug_gui_shell,
			%   trace_utils:debug( "To left (any previous character)." ) ),

			case text_edit:move_cursor_left( TextEdit ) of

				unchanged ->
					GUIShellState;

				NewTextEdit ->
					apply_cursor_position( NewTextEdit, CmdEditor ),

					GUIShellState#gui_shell_state{ text_edit=NewTextEdit }

			end;


		% Right arrow (in edited text):
		?MYR_SCANCODE_RIGHT ->

			%cond_utils:if_defined( myriad_debug_gui_shell,
			%   trace_utils:debug( "To right (any next character)." ) ),

			case text_edit:move_cursor_right( TextEdit ) of

				unchanged ->
					GUIShellState;

				NewTextEdit ->
					apply_cursor_position( NewTextEdit, CmdEditor ),

					GUIShellState#gui_shell_state{ text_edit=NewTextEdit }

			end;


		% Up (in command history):
		?MYR_SCANCODE_UP ->

			%cond_utils:if_defined( myriad_debug_gui_shell,
			%   trace_utils:debug( "Recall previous command." ) ),

			case text_edit:recall_previous_entry( TextEdit ) of

				unchanged ->
					GUIShellState;

				NewTextEdit ->
					apply_text( NewTextEdit, CmdEditor ),
					apply_cursor_position( NewTextEdit, CmdEditor ),
					GUIShellState#gui_shell_state{ text_edit=NewTextEdit }

			end;


		% Down (in command history):
		?MYR_SCANCODE_DOWN ->

			%cond_utils:if_defined( myriad_debug_gui_shell,
			%   trace_utils:debug( "Recall next command." ) ),

			case text_edit:recall_next_entry( TextEdit ) of

				unchanged ->
					GUIShellState;

				NewTextEdit ->
					apply_text( NewTextEdit, CmdEditor ),
					apply_cursor_position( NewTextEdit, CmdEditor ),
					GUIShellState#gui_shell_state{ text_edit=NewTextEdit }

			end;


		% DELETE key:
		?MYR_SCANCODE_DELETE ->

			%cond_utils:if_defined( myriad_debug_gui_shell,
			%                       trace_utils:debug( "Delete entered." ) ),

			case text_edit:delete_current_char( TextEdit ) of

				unchanged ->
					GUIShellState;

				NewTextEdit ->
					apply_text( NewTextEdit, CmdEditor ),

					% No cursor change.

					GUIShellState#gui_shell_state{ text_edit=NewTextEdit }

			end;


		?MYR_SCANCODE_BACKSPACE ->

			%cond_utils:if_defined( myriad_debug_gui_shell,
			%   trace_utils:debug( "Backspace entered." ) ),

			case text_edit:delete_previous_char( TextEdit ) of

				unchanged ->
					GUIShellState;

				NewTextEdit ->

					apply_text( NewTextEdit, CmdEditor ),

					% More reliable than:
					% gui_text_editor:offset_cursor_position( CmdEditor,
					% _PosOffset=-1 ):
					%
					apply_cursor_position( NewTextEdit, CmdEditor ),

					GUIShellState#gui_shell_state{ text_edit=NewTextEdit }

			end;


		?MYR_SCANCODE_TAB ->

			%cond_utils:if_defined( myriad_debug_gui_shell,
			%   trace_utils:debug( "Tab entered." ) ),

			GUIShellState;


		% Either the Return key or its keypad counterpart:
		S when S =:= ?MYR_SCANCODE_RETURN orelse S =:= ?MYR_SCANCODE_KP_ENTER ->
			handle_command_validation( CmdEditor, TextEdit, GUIShellState );


		_Other ->

			Keycode = gui_keyboard:get_keycode( BackendKeyEvent ),

			NewTextEdit = text_edit:add_char( Keycode, TextEdit ),

			apply_text( NewTextEdit, CmdEditor ),

			% More reliable than:
			% gui_text_editor:offset_cursor_position( CmdEditor, _PosOffset=1 ),
			%
			apply_cursor_position( NewTextEdit, CmdEditor ),

			GUIShellState#gui_shell_state{ text_edit=NewTextEdit }


	end.



-doc "Handles the validation of a command.".
-spec handle_command_validation( text_editor(), text_edit(),
								 gui_shell_state() ) -> gui_shell_state().
handle_command_validation( CmdEditor, TextEdit, GUIShellState ) ->

	ThisCmdId = text_edit:get_entry_id( TextEdit ),

	% Note that command/result histories are managed by the shell instance:
	{ BaseText, ProcessTE, MaybeTmstpBinStr } =
			case text_edit:process( TextEdit ) of

		% CmdBinStr is the actual command (e.g. with trailing dot added):
		{ success, RecTextEdit, CmdBinStr, CmdRes, MaybeTimestampBinStr } ->

			NewCurrentCmdId = text_edit:get_entry_id( RecTextEdit ),

			cond_utils:if_defined( myriad_debug_gui_shell,
				trace_utils:debug_fmt( "For command #~B (at ~ts), "
					"success value is:~n ~p; new command is #~B.",
					% Not RecTextEdit:
					[ ThisCmdId, MaybeTimestampBinStr,
					  CmdRes, NewCurrentCmdId ] ) ),

			{ text_utils:bin_format( "~ts~ts~n~ts",
				[ get_prompt_for( ThisCmdId ), CmdBinStr,
				  text_utils:term_to_binary( CmdRes ) ] ),
			  RecTextEdit, MaybeTimestampBinStr };


		{ error, RecTextEdit, CmdBinStr, CmdErrorBinStr,
		  MaybeTimestampBinStr } ->

			NewCurrentCmdId = text_edit:get_entry_id( RecTextEdit ),

			cond_utils:if_defined( myriad_debug_gui_shell,
				trace_utils:debug_fmt( "For command #~B (at ~ts), "
					"error is '~ts'; new command is #~B.",
					% Not RecTextEdit:
					[ ThisCmdId, MaybeTimestampBinStr,
					  CmdErrorBinStr, NewCurrentCmdId ] ) ),

			{ text_utils:bin_format( "~ts~ts~n~ts",
				[ get_prompt_for( ThisCmdId ), CmdBinStr, CmdErrorBinStr ] ),
			  RecTextEdit, MaybeTimestampBinStr };


		{ update_prompt, NewPrompt } ->
			throw( {to_update, NewPrompt } );


		% Mostly useless security:
		InvalidOutcome ->

			trace_utils:error_fmt( "Received an invalid command outcome: ~p",
								   [ InvalidOutcome ] ),

			throw( { invalid_command_outcome, InvalidOutcome } )

	end,

	AddText = format_text( BaseText, MaybeTmstpBinStr ),

	NewPastOpsText = text_utils:bin_concatenate(
		GUIShellState#gui_shell_state.past_ops_text, AddText ),

	PastOpsEditor = GUIShellState#gui_shell_state.past_ops_editor,

	gui_text_editor:set_text( PastOpsEditor, NewPastOpsText ),

	gui_text_editor:show_text_end( PastOpsEditor ),

	% Prepare for next command:

	%gui_text_editor:set_text( CmdEditor, get_prompt_for( NextCmdId ) ),
	gui_text_editor:clear( CmdEditor ),

	NewTextEdit = edit_new_command( ProcessTE, CmdEditor ),

	%trace_utils:info_fmt(
	%  "Shell read: '~ts'; new past operations are: <<<~n~ts>>>",
	%  [ AddText, NewPastOpsText ] ),

	GUIShellState#gui_shell_state{ past_ops_text=NewPastOpsText,
								   text_edit=NewTextEdit }.





-doc """
Sets the specified text in the specified editor.

Does not alter the cursor position.
""".
-spec apply_text( text_edit(), text_editor() ) -> void().
apply_text( TextEdit, CmdEditor ) ->

	Text = text_edit:get_full_text( TextEdit ),

	trace_utils:debug_fmt( "Applying full text: '~ts'.", [ Text ] ),

	% Useless:
	%gui_text_editor:clear( CmdEditor ),

	% Will have to be restored, as setting bound to put it at end:
	SavedPos = gui_text_editor:get_cursor_position( CmdEditor ),

	gui_text_editor:set_text( CmdEditor, Text ),

	gui_text_editor:set_cursor_position( CmdEditor, SavedPos ).

	% Wrong, we may edit in-text:
	%gui_text_editor:set_cursor_position_to_end( CmdEditor ).



-doc "Sets the specified cursor position in the specified editor.".
-spec apply_cursor_position( text_edit(), text_editor() ) -> void().
apply_cursor_position( TextEdit, CmdEditor ) ->
	gui_text_editor:set_cursor_position( CmdEditor,
		text_edit:get_cursor_position( TextEdit ) ).



-doc """
Sets the specified full command, erasing any previous content, setting the
cursor at end.
""".
-spec set_full_command( ustring(), ustring(), text_editor(), text_edit() ) ->
												text_edit().
set_full_command( Prefix, CmdText, CmdEditor, TextEdit ) ->

	%trace_utils:debug_fmt( "Setting command '~ts'.", [ CmdText ] ),

	FullText = Prefix ++ CmdText,

	gui_text_editor:set_text( CmdEditor, FullText ),

	% Not wanting the previous call to reset to first position (which thus would
	% be before prefix):
	%
	gui_text_editor:get_cursor_position( CmdEditor ) =:= 1 andalso
		gui_text_editor:set_cursor_position_to_end( CmdEditor ),

	text_edit:set_entry( TextEdit, CmdText ).



-doc """
Returns a textual description of the text state of the specified shell state.
""".
-spec text_state_to_string( gui_shell_state() ) -> ustring().
text_state_to_string( #gui_shell_state{ command_editor=CmdEditor,
										text_edit=TextEdit } ) ->

	Text = text_edit:get_entry( TextEdit ),

	text_utils:format( "Current text is '~ts' (of length ~B), "
		"cursor position is ~B", [ Text, length( Text ),
			gui_text_editor:get_cursor_position( CmdEditor ) ] ).



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
