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
% Creation date: Wednesday, May 20, 2020.

-module(shell_utils).

-moduledoc """
Provides our own version of an Erlang shell process, typically in order to
integrate it in a REPL-like interpreter.

See gui_shell_test.erl for an example of use thereof.
""".



% User API:
-export([ start_shell/0, start_link_shell/0,
		  start_integrated_shell/0,

		  execute_command/2

		]).


% To enable spawns of them:
-export([ start_integrated_helper/0 ]).



-doc "The PID of a (Myriad) shell process.".
-type shell_pid() :: pid().


-doc "The PID of a shell client process.".
-type client_pid() :: pid().



% Surprisingly, not a string-like, but term():
-type variable_name() :: erl_eval:name().

% term():
-type variable_value() :: erl_eval:value().



% Apparently not exported as a standalone:
-doc "Logical binding for a variable, as held by a shell.".
-type binding() :: { variable_name(), variable_value() }.


-doc """
A command to submit to a shell, corresponding to a sequence of expressions.

For example: <<"A=1, B=2, A+B.">>.
""".
-type command() :: bin_string().


-doc "The result of a command, as evaluated by a shell.".
-type command_result() :: variable_value().

-doc "An error generated when a shell evaluates a submitted command.".
-type command_error() :: bin_string().



-doc "The outcome of a command submitted to a shell.".
-type command_outcome() :: { 'success', command_result() }
						 | { 'error', command_error() }.



% An history of a shell, that is the list of the previously submitted commands
% (here in antichronological order):
%
-type history() :: [ command() ].


-export_type([ shell_pid/0, client_pid/0,
			   variable_name/0, variable_value/0,
			   binding/0,
			   command/0, command_result/0, command_error/0, command_outcome/0,
			   history/0 ]).



% Local types:

-record( shell_state, {

	% The number of commands submitted:
	submission_count :: count(),

	% Possibly ellipsed:
	history :: history(),

	% Records all current bindings:
	bindings :: binding_struct()

	% TODO: add local/non-local function handlers

} ).


-doc "The state of a shell instance.".
-type shell_state() :: #shell_state{}.



% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().


% Not a [binding()]:
-type binding_struct() :: erl_eval:binding_struct().




% Implementation notes:
%
% Perhaps that a smart use of the built-in 'shell' module could have sufficed.
%
% We see two approaches in order to implement such a separate shell:
%
% - (A) define our own version of it, from scratch, based on our own
% read/scan/eval loop
%
% - (B) plug in the group/user/shell built-in architecture ("integrated shell")
%
%
% See also for approach B:
%
%  * a full description of the Erlang shell:
%  https://ferd.ca/repl-a-bit-more-and-less-than-that.html; we understand that
%  we shall register (as 'user_drv') a process running our own version of the
%  usr_drv module, just in charge, through the 'group' processes that it
%  manages, of feeding a set of standard 'shell/eval' processes (local or
%  remote) and handling their results; one purpose of user_drv is to determine
%  what is the current shell among the ones that it drives, and to drop in into
%  shell management mode if the input text happens to be ^C or ^G (allowing to
%  switch shells)
%
%  * kernel/src/user_drv.erl for the (message-based) applicative protocol
%  between a user_drv process and a group (see message/0 and request/0, which
%  are sent by the user_drv process to its current group)
%
%  * https://erlangforums.com/t/adding-repl-like-feature-to-a-graphical-erlang-application/3795
%  (using edlin)
%
%  * https://erlang.org/pipermail/erlang-questions/2008-September/038476.html


% For myriad_spawn_link/1:
-include("spawn_utils.hrl").




% Section for our own shell (approach A).


%% Shell user API.


-doc """
Starts a (non-linked) shell process according to approach A, returns its PID.
""".
-spec start_shell() -> shell_pid().
start_shell() ->

	ShellPid = ?myriad_spawn(
		fun() ->
			shell_init()
		end ),

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Started (non-linked) shell ~w.",
							   [ ShellPid ] ) ),

	ShellPid.



-doc """
Starts a linked shell process according to approach A, returns its PID.
""".
-spec start_link_shell() -> shell_pid().
start_link_shell() ->

	ShellPid = ?myriad_spawn_link(
		fun() ->
			shell_init()
		end ),

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Started linked shell ~w.", [ ShellPid ] ) ),

	ShellPid.



-doc """
Executes the specified command on the specified shell, and returns its result.

Throws an exception on error.
""".
-spec execute_command( any_string(), shell_pid() ) -> variable_value().
execute_command( CmdAnyStr, ShellPid ) ->
	CmdBinStr = text_utils:ensure_binary( CmdAnyStr ),
	ShellPid ! { processCommand, CmdBinStr, self() },
	receive

		{ success, CmdResValue } ->
			CmdResValue;

		{ error, ErrorInfo } ->
			trace_utils:error_fmt( "Failed to execute command '~ts' "
				"on shell ~w:~n ~p", [ CmdAnyStr, ShellPid, ErrorInfo ] ),
			throw( { shell_command_failed, ErrorInfo, ShellPid, CmdAnyStr } )

	end.



% Implementation helpers.

-doc "Initialises a shell.".
-spec shell_init() -> no_return().
shell_init() ->

	InitShellState = #shell_state{
		submission_count=0,
		history=[],
		bindings=erl_eval:new_bindings() },

	shell_main_loop( InitShellState ).



-doc "Main loop of a shell instance (approach A).".
-spec shell_main_loop( shell_state() ) -> no_return().
shell_main_loop( ShellState ) ->

	% WOOPER-like conventions:

	NewShellState = receive

		{ processCommand, CmdBinStr, ClientPid } ->

			{ CmdOutcome, ProcShellState } =
				process_command( CmdBinStr, ShellState ),

			% A failed command does not kill the shell:
			ClientPid ! CmdOutcome,
			ProcShellState;


		terminate ->
			cond_utils:if_defined( myriad_debug_shell,
								   trace_utils:debug( "Terminating." ) ),

			terminated;


		{ terminateSynch, CallerPid } ->
			cond_utils:if_defined( myriad_debug_shell,
				trace_utils:debug( "Terminating synchronously." ) ),

			CallerPid ! onShellTerminated;

		UnexpectedMsg ->
			trace_utils:error_fmt( "Unexpected message received and ignored "
				"by Myriad shell ~w: ~p", [ self(), UnexpectedMsg ] ),
			ShellState

	end,

	cond_utils:if_defined( myriad_debug_shell, trace_utils:debug_fmt(
		"Now being a ~ts", [ shell_state_to_string( ShellState ) ] ) ),

	shell_main_loop( NewShellState ).




% Shell commands.
%
% We are still in the context of approach A.



-doc "Has this shell process the specified command.".
-spec process_command( command(), shell_state() ) ->
										{ command_outcome(), shell_state() }.
process_command( CmdBinStr, ShellState=#shell_state{ submission_count=SubCount,
													 history=History,
													 bindings=Bindings } ) ->

	cond_utils:if_defined( myriad_debug_shell, trace_utils:debug_fmt(
		"Processing command '~ts'.", [ CmdBinStr ] ) ),

	BaseShellState = ShellState#shell_state{
		submission_count=SubCount+1,
		history=[ CmdBinStr | History ] },

	% Binaries cannot be scanned as are:
	CmdStr = text_utils:binary_to_string( CmdBinStr ),

	case erl_scan:string( CmdStr ) of

		{ ok, Tokens, EndLocation } ->

			cond_utils:if_defined( myriad_debug_shell,
				trace_utils:debug_fmt( "Scanned tokens '~p' (end location: ~p)",
									   [ Tokens, EndLocation ] ) ),

			case erl_parse:parse_exprs( Tokens ) of

				% Supposedly multiple expression forms can be expected ("EXPR1,
				% EXPR2"):
				%
				% { ok, [ ExprForm ] } ->
				{ ok, ExprForms } ->
					cond_utils:if_defined( myriad_debug_shell,
						trace_utils:debug_fmt( "Parsed following expression "
							"forms:~n ~p", [ ExprForms ] ) ),

					% If an expression evaluation fails, what happens?
					% Currently not using local/non-local function handlers:
					{ value, CmdValue, NewBindings } =
						erl_eval:exprs( ExprForms, Bindings ),

					ProcShellState = BaseShellState#shell_state{
						bindings=NewBindings },

					CmdOutcome = { success, CmdValue },

					{ CmdOutcome, ProcShellState };


				CmdOutcome={ error, _ErrorInfo={ Loc, Mod, Desc } } ->

					cond_utils:if_defined( myriad_debug_shell,
						trace_utils:warning_fmt( "Parse error when evaluating "
							"command '~ts' by shell ~w: ~ts (location: ~ts)",
							[ CmdBinStr, self(),
							  ast_utils:interpret_issue_description( Desc,
																	 Mod ),
							  ast_utils:file_loc_to_string( Loc ) ] ),
						basic_utils:ignore_unused( [ Loc, Mod, Desc ] ) ),

					{ CmdOutcome, BaseShellState }

			end;


		{ error, ErrorInfo={ Loc, Mod, Desc }, ErrorLocation } ->
			cond_utils:if_defined( myriad_debug_shell,
				trace_utils:warning_fmt( "Scan error when evaluating "
					"command '~ts' by shell ~w: ~ts (location: ~ts / ~ts)",
					[ CmdBinStr, self(),
					  ast_utils:interpret_issue_description( Desc, Mod ),
					  ast_info:location_to_string( Loc ),
					  ast_info:location_to_string( ErrorLocation ) ] ),
				basic_utils:ignore_unused(
					[ Loc, Mod, Desc, ErrorLocation ] ) ),

			CmdOutcome = { error, ErrorInfo },
			{ CmdOutcome, BaseShellState }

	end.




% Section for an integrated shell (approach B).



-doc """
Starts a shell process according to approach B, returns its PID.
""".
-spec start_integrated_shell() -> shell_pid().
start_integrated_shell() ->

	GrpServerPid = group:start( user_drv:start(),
		{ ?MODULE, start_integrated_helper, [] } ),

	trace_utils:debug_fmt( "Group server for integrated shell: ~w.",
						   [ GrpServerPid ] ),

	GrpServerPid.


start_integrated_helper() ->
	spawn_link( fun() -> integrated_shell_main_loop() end ).



-doc "Main loop of an integrated shell instance (approach B).".
-spec integrated_shell_main_loop() -> no_return().
integrated_shell_main_loop() ->
	throw(to_do).



% Helpers


-doc "Returns a textual description of the specified shell state.".
-spec shell_state_to_string( shell_state() ) -> ustring().
shell_state_to_string( ShellState ) ->
	shell_state_to_string( ShellState, _Verbose=true ).



-doc """
Returns a textual description of the specified shell state, with the specified
verbosity.
""".
-spec shell_state_to_string( shell_state(), boolean() ) -> ustring().
shell_state_to_string( #shell_state{
							submission_count=SubCount,
							history=History,
							bindings=BindingStruct },
					   _Verbose=true ) ->
	text_utils:format( "shell with ~ts and ~B commands already submitted, "
		"with ~ts",
		[ bindings_to_string( BindingStruct ),
		  SubCount, history_to_string( History ) ] );


shell_state_to_string( #shell_state{
							submission_count=SubCount,
							%history=History,
							bindings=BindingStruct },
					   _Verbose=false ) ->
	text_utils:format( "shell with ~B bindings and ~B commands already "
		"submitted",
		[ length( erl_eval:bindings( BindingStruct ) ), SubCount ] ).



-doc "Returns a textual description of the specified bindings.".
-spec bindings_to_string( binding_struct() ) -> ustring().
bindings_to_string( BindingStruct ) ->
	case erl_eval:bindings( BindingStruct ) of

		[] ->
			"no binding";

		Bindings ->
			text_utils:format( "~B bindings: ~ts",
				[ length( Bindings ),
				  text_utils:strings_to_string(
					[ text_utils:format( "variable '~ts' has for value ~p",
						[ N, V ] ) || { N, V } <- lists:sort( Bindings ) ] ) ] )

	end.



-doc "Returns a textual description of the specified history.".
-spec history_to_string( history() ) -> ustring().
history_to_string( _History=[] ) ->
	"no history";

history_to_string( History ) ->
	"the following history: " ++ text_utils:strings_to_enumerated_string(
									lists:reverse( History ) ).
