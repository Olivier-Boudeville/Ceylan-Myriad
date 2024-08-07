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
""".



-doc """
Designates an actual GUI shell instance.
""".
-type shell() :: pid().

-export_type([ shell/0 ]).



-export([ create/1, create/2, destruct/1 ]).



% For myriad_spawn_link:
-include_lib("myriad/include/spawn_utils.hrl").


% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_ustring().

%-type text() :: ui:text().

-type parent() :: gui:parent().
-type backend_environment() :: gui:backend_environment().

-type font_size() :: gui_font:font_size().

-type text_editor() :: gui_text_editor:text_editor().



% Local type:

-type command_id() :: count().


% Implementation notes:
%
% A shell is, graphically, an horizontal sizer; the top part is a read-only text
% editor for the command/output history, while the bottom part is a (smaller)
% editable text editor.

% wxStyledTextCtrl could be used instead of wxTextCtrl.
%
% Using the Scintilla-compliant Erlang lexers (see wxSTC_ERLANG_*) could be
% convenient, yet it is a rather enormous, complex API, and there is little
% interest in syntax-highlighting the user inputs or the interpreter outputs.


-define( default_font_size, 10 ).


-record( shell_state, {

	% The editor used by the shell for the input commands:
	command_editor :: text_editor(),

	% The identifier of the next command:
	next_command_id :: command_id(),

	% The (read-only) editor displaying history:
	history_editor :: text_editor(),

	% The history of all printouts:
	history :: bin_string(),

	% Tells whether the history elements shall be timestamped:
	do_timestamp = true :: boolean(),

	% Tells whether the full history (inputs and outputs) shall be logged:
	do_log = true :: boolean(),

	% Tells whether a trailing dot should be automatically added if lacking in a
	% command:
	%
	%auto_add_trailing_dot = false :: boolean()
	auto_add_trailing_dot = true :: boolean()

					  } ).

-doc "The (internal) state of a shell instance.".
-type shell_state() :: #shell_state{}.




-doc """
Creates a shell, in the specified parent window, using the default font size.
""".
-spec create( parent() ) -> shell().
create( ParentWindow ) ->
	create( ?default_font_size, ParentWindow ).



-doc """
Creates a shell, in the specified parent window, using the specified font size.
""".
-spec create( font_size(), parent() ) -> shell().
create( FontSize, ParentWindow ) ->

	BackendEnv = gui:get_backend_environment(),

	ShellPid = ?myriad_spawn_link(
		fun() ->
			start_shell( FontSize, BackendEnv, ParentWindow )
		end ),

	cond_utils:if_defined( myriad_debug_gui_shell,
		trace_utils:debug_fmt( "Created shell ~w.", [ ShellPid ] ) ),

	ShellPid.



-doc "Destructs the specified shell.".
-spec destruct( shell() ) -> void().
destruct( ShellPid ) ->
	cond_utils:if_defined( myriad_debug_gui_shell,
		trace_utils:debug_fmt( "Terminating shell ~w.", [ ShellPid ] ) ),
	ShellPid ! destruct.




% Helper functions.


-doc "Starts a shell process, using specified font size.".
-spec start_shell( font_size(), backend_environment(), parent() ) ->
										no_return().
start_shell( FontSize, BackendEnv, ParentWindow ) ->

	gui:set_backend_environment( BackendEnv ),

	InitCmdId = 1,

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


	HistoryEditor = gui_text_editor:create(
		_Opts=[ % Too early, font not set: { text, InitText },
				{ style, [ multiline, read_only, word_wrap ] } ],
		ParentWindow ),

	% We prefer that the history cannot be selected:
	gui_widget:set_enable_status( HistoryEditor, _DoEnable=false ),

	% For example Monospace 11:
	ShellFont = gui_font:create( FontSize, _FontFamily=modern ),

	[ gui_text_editor:set_default_font( Ed, ShellFont )
		|| Ed <- [ CmdEditor, HistoryEditor ] ],

	gui_font:destruct( ShellFont ),

	InitBinText = text_utils:bin_format( "Welcome to the MyriadGUI shell ~w.~n",
										 [ self() ] ),

	% Not wanting an onEnterPressed event for that:
	gui_text_editor:set_text( HistoryEditor, InitBinText ),

	gui_sizer:add_element( VSizer, HistoryEditor,
		[ expand_fully, { proportion, 1 } ] ),

	gui_sizer:add_element( VSizer, HSizer,
		[ expand_fully, { proportion, 0 } ] ),

	gui:subscribe_to_events( { onEnterPressed, CmdEditor } ),


	% So that the editors take their actual size from the start:
	gui_widget:layout( ParentWindow ),

	InitShellState = #shell_state{ command_editor=CmdEditor,
								   next_command_id=InitCmdId,
								   history_editor=HistoryEditor,
								   history=InitBinText,
								   do_timestamp=true,
								   do_log=true
								 },

	shell_main_loop( InitShellState ).



-doc "Main loop of the shell process.".
-spec shell_main_loop( shell_state() ) -> no_return().
shell_main_loop( ShellState=#shell_state{ command_editor=CmdEditor,
										  next_command_id=NextCmdId,
										  history_editor=HistoryEditor,
										  history=BinHistory,
										  do_timestamp=DoTimestamp,
										  do_log=DoLog,
										  auto_add_trailing_dot=AutoAddDot
										} ) ->

	receive

		{ onEnterPressed, [ _CmdEditor, _EditorId, NewText, _Context ] } ->

			%trace_utils:debug_fmt( "Read '~ts'.", [ NewText ] ),

			TimeStr = case DoTimestamp of

				true ->
					text_utils:format( "[~ts] ",
									   [ time_utils:get_textual_timestamp() ] );

				false ->
					""

			end,

			UpdatedText = case AutoAddDot of

				true ->
					TextStr = text_utils:ensure_string( NewText ),

					DotChar = $.,

					case list_utils:get_last_element( TextStr ) of

						DotChar ->
							TextStr;

						_OtherChar ->
							list_utils:append_at_end( DotChar, TextStr )

					end;

				false ->
					NewText

			end,


			AddText = text_utils:bin_format( "~ts~ts~ts",
				[ TimeStr, get_prompt_for( NextCmdId ), UpdatedText ] ),

			DoLog andalso trace_utils:debug( AddText ),

			NewBinHistory = text_utils:bin_format( "~ts~ts~n",
												   [ BinHistory, AddText ] ),

			gui_text_editor:set_text( HistoryEditor, NewBinHistory ),

			gui_text_editor:show_text_end( HistoryEditor ),

			%gui_text_editor:set_text( CmdEditor, get_prompt_for( NextCmdId ) ),
			gui_text_editor:clear( CmdEditor ),

			%% trace_utils:info_fmt(
			%%	"Shell read: '~ts'; new history is: <<<~n~ts>>>",
			%%	[ NewText, NewBinHistory ] ),

			shell_main_loop( ShellState#shell_state{
				history=NewBinHistory,
				next_command_id=NextCmdId+1 } );

		destruct ->
			[ gui_text_editor:destruct( Ed )
				|| Ed <- [ CmdEditor, HistoryEditor ] ],
			cond_utils:if_defined( myriad_debug_gui_shell,
				trace_utils:debug_fmt( "Shell ~w terminated.", [ self() ] ) );

		Other ->
			trace_utils:warning_fmt( "GUI shell loop ignored following "
									 "message:~n ~p.", [ Other ] ),
			shell_main_loop( ShellState )

	end.



-doc """
Returns the full prompt corresponding to the specified command identifier.
""".
-spec get_prompt_for( command_id() ) -> ustring().
get_prompt_for( CmdId ) ->
	text_utils:format( "~B> ", [ CmdId ] ).
