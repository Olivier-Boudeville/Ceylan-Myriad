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

See shell_utils_test.erl for its testing, and gui_shell_test.erl for an
example of use thereof.
""".


% Two versions were targeted:
%
% - a custom shell, using our own Myriad conventions; we use it
%
% - a standard shell, integrated in Erlang native subsystems; we drop it (see
% reasons in implementation notes below)



% User API:
-export([ start_custom_shell/0, start_link_custom_shell/0,
		  start_custom_shell/1, start_link_custom_shell/1,

		  execute_command/2 ]).



% At least for silencing:
-export([ % As not ready for use:
		  start_standard_shell/0, start_link_standard_shell/0,
		  start_standard_shell/1, start_link_standard_shell/1,

		  custom_shell_state_to_string/1, standard_shell_state_to_string/1 ]).


-doc "The PID of a (Myriad) shell process.".
-type shell_pid() :: custom_shell_pid() | standard_shell_pid().


-doc "The PID of a Myriad custom shell process.".
-type custom_shell_pid() :: pid().

-doc "The PID of a Myriad standard shell process.".
-type standard_shell_pid() :: pid().



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


-doc "The number of a command, which is an identifier thereof.".
-type command_id() :: count().


-doc "The result of a command, as evaluated by a shell.".
-type command_result() :: variable_value().

-doc "An error message generated when a shell evaluates a submitted command.".
-type command_error() :: bin_string().


-doc "The information returned once a command is processed.".
-type command_outcome() ::

	{ 'success', command_result(), command_id(),
	  MaybeTimestampBinStr :: option( timestamp_binstring() ) }

  | { 'error', command_error(),
	  MaybeTimestampBinStr :: option( timestamp_binstring() ) }.


% An element kept in the history:
-type history_element() :: { command(), command_result() }.


-doc """
An history of a shell, that is a list of previously submitted commands and of
their results (here in antichronological order).

Note that this may allow huge terms to be kept around longer than expected (see
the flushHistory request message to avoid that).
""".
-type history() :: queue( history_element() ).



-doc """
Options that can be specified when creating a shell:

- 'timestamp': keep track also of the timestamp of the start of a command

- 'log': logs the commands and their results, in a file whose default name is
  `myriad-shell-CREATOR_PID.log`, where CREATOR_PID corresponds to the PID of
  the shell creator, like in `myriad-shell-0.84.0.log` (then written in the
  current directory)

- {'log', LogPath :: any_file_path()}: logs the commands and their results in a
  file whose path is specified

- {'history', MaxDepth :: count()}: records a command history of the specified
 maximum depth; 'undefined' means unlimited depth

- no_history: does not store any command history (synonym of {history,0})
""".
-type shell_option() ::
	'timestamp'
 |  'log'
 | { 'log', LogPath :: any_file_path() }
 | { 'history', MaybeMaxDepth :: option( count() ) }
 |  'no_history'.



-doc """
The PID of a group leader process for user IO (see lib/kernel/src/group.erl).
""".
-type group_pid() :: pid().


-export_type([ shell_pid/0, custom_shell_pid/0, standard_shell_pid/0,
			   client_pid/0,

			   variable_name/0, variable_value/0,
			   binding/0,

			   command/0, command_id/0, command_result/0, command_error/0,
			   command_outcome/0,

			   history/0,

			   group_pid/0 ]).


-define( default_history_max_depth, 20 ).


% The state of a custom shell instance.
%
% Defaults set in vet_options_for_custom/1:
-record( custom_shell_state, {

	% The number of commands already submitted; corresponds to the number
	% (identifier) of any current command (or the one of the next command, minus
	% 1):
	%
	submission_count = 0 :: count(),

	% Tells whether the start of commands shall be timestamped:
	do_timestamp = false :: boolean(),


	% Tells whether the full history (inputs and outputs) shall be logged on
	% file and, if yes, in which one:
	%
	log_path = undefined :: option( bin_file_path() ),

	% The file (if any) where logs are to be written:
	log_file = undefined :: option( file() ),


	% Maximum number of history elements (0: none; 1: just the last one, etc.;
	% undefined: infinite):
	%
	history_max_depth  = ?default_history_max_depth :: option( count() ),

	% Possibly ellipsed:
	history :: history(),

	% Records all current bindings:
	bindings :: binding_struct()

	% TODO: add local/non-local function handlers

} ).


-doc "The state of a custom shell instance.".
-type custom_shell_state() :: #custom_shell_state{}.


% The state of a standard shell instance.
%
% Defaults set in vet_options_for_standard/1:
-record( standard_shell_state, {

	% The PID of the group leader being currently used (if any):
	current_group :: option( group_pid() )

} ).


-doc "The state of a standard shell instance.".
-type standard_shell_state() :: #standard_shell_state{}.




% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type timestamp_binstring() :: time_utils:timestamp_binstring().

-type bin_file_path() :: file_utils:bin_file_path().
-type any_file_path() :: file_utils:any_file_path().
-type file() :: file_utils:file().

% Not a [binding()]:
-type binding_struct() :: erl_eval:binding_struct().

-type queue( T ) :: queue:queue( T ).



% Implementation notes:
%
% Perhaps that a smart use of the built-in 'shell' module could have sufficed.
%
% We see two approaches in order to implement such a separate shell:
%
% - (A) define our own custom version of it, from scratch, based on our own
% read/scan/eval loop; we prefer here that the shell resists to any failure
% induced by user commands (e.g. not losing its bindings then); it is, at least
% currently, a very basic shell: no command recall, no history, no built-in
% functions (like f()), no shell switching, no remote shell, etc; we considered
% supporting an auto_add_trailing_dot option ("Tells whether a trailing dot
% should be automatically added if lacking in a command") yet it was probably
% not a relevant idea)
%
% - (B) plug in the group/user/shell built-in architecture ("standard shell");
% see also for this approach:
%
%  * a full description of the Erlang shell:
%  https://ferd.ca/repl-a-bit-more-and-less-than-that.html; we understand that
%  we shall mimic the usr_drv module, just in charge, through the 'group'
%  process(es) that it manages, of feeding a set of standard 'shell/eval'
%  processes (local or remote) and handling their results; 'user_drv' is
%  moreover able to determine what is the current shell among the ones that it
%  drives, and to drop into shell management mode if the input text happens to
%  be ^C or ^G (allowing to switch shells); here this would be done by the
%  creator of our shells
%
%  * kernel/src/user_drv.erl (there is no user.erl) for the (message-based)
%  applicative protocol between a user_drv process and a group (see message/0
%  and request/0, which are sent by the user_drv process to its current group);
%  inspiration can also be found from ssh/src/ssh_cli.erl, relying on
%  kernel/src/group.erl (not referenced in user documentation),
%  stdlib/src/edlin.erl
%
%  * https://erlangforums.com/t/adding-repl-like-feature-to-a-graphical-erlang-application/3795
%  (using edlin)
%
%  * https://erlang.org/pipermail/erlang-questions/2008-September/038476.html
%  * https://tryerlang.org/
%  * https://github.com/seriyps/eplaypen and http://tryerl.seriyps.ru/
%  * http://erlang.org/pipermail/erlang-questions/2013-April/073451.html

% We tried to implement both, and found out that developing a custom shell was
% way simpler and more satisfactory than trying to plug in the native shell
% infrastructure. Despite much time spent, understanding how to properly
% implement a sufficient protocol like the one between the prim_tty, group, and
% shell modules is difficult (e.g. overlapping of responsabilities, every module
% having to manage input/output text to some extent, many features getting in
% the way - old/new/remote shell) and inconvenient (not much
% documentation/examples/tests, no easy console or file logging).
%
% So, at least for now, we use exclusively our custom shell, not the standard
% one.
%
% What are we losing in doing so / what extra features should be added in some
% possible future?
%
% - enforce a true MVC pattern? Should be already quite the case; preferably
% without using a FSM (gen_statem), as the code gets considerably less clear
% then; a shell evaluator should not even be aware of multiline editing,
% terminal geometry, etc.; enforce a clear modularity/separation of concerns
% (multiple modules, probably multiple processes)
%
% - support various encodings? No, dealing only with UTF8 binaries is more than
% enough nowadays
%
% - support noshell, oldshell or be compliant with the (native) newshell one?
% Not interesting enough
%
% - switch to keypress/character-based handling with cursor control
% (move/insert/delete, etc.), rather than full commands? Yes, could allow for a
% bit of smart command editing, e.g. set of supported shortcuts - like an
% Emacs-like one, (forward/backward) search mode, syntax highlighting,
% auto-completion of module/function/variable names, and so on; possibly
% callback-based; most probably the first addition to plan
%
% - support remote (Myriad) shells, on other nodes? Certainly, one day
%
% - provide solutions for shell control (like with Ctrl-g: listing, starting,
% connecting, interrupting, killing shells)? Can be added later, with a
% shell_controller, which would manage a set of shells (like the native "job
% control manager" - jcl) and be their (smart) group leader for I/O (filtering
% non-current shells, but possibly notifying of their activity and/or logging
% them as well)
%
% - pseudo-local functions (i.e. "implicit modules", "built-in functions" -
% callable without being prefixed with a module, as if they were local - for
% example to offer built-in shell facilities like
% https://www.erlang.org/doc/apps/stdlib/shell.html#module-shell-commands) in a
% table, to have a set of them readily available on the shell? Yes, certainly
% convenient; this requires most probably parse-transformation (not a large
% problem with Myriad meta)
%
% - restricted shell, i.e. excluded modules and/or functions (to implement a
% safer shell, like for https://www.tryerlang.org/restrictions) could be added?
% Yes, certainly a useful feature, for security and to avoid silly mistakes;
% could be based on a set of whitelisted patterns and/or a set of blacklisted
% ones (or re-using pre-existing infrastructure)
%
% - provide per-shell history and make it persistent? To be determined
%
% - provide terminal multiplexing with transfers, like 'screen'? Some day maybe
%
% - support fancy constructs like records? Use case needed
%
% - enforce robust management of EXIT/DOWN messages, exceptions, etc.?
% Certainly, possibly with aliases/monitors, synchronous operations, etc.
%
% - support fancier operations like password entering? Use case needed


% What we are gaining with a custom shell ? Simplicity, dropping historical
% retrocompatibility, having more proper comments, specs, etc.


% Mode of operation (common to both kinds of shells):
%
% A caller creates such a shell.
%
% The shell may spontaneously send displayRequest messages (e.g. so that
% slogan-like "Eshell V15.0 [...]" texts are displayed by the caller).
%
% The caller may (concurrently) run the execute_command/2 function (sending
% processCommand messages to the shell), so that the corresponding command is
% evaluated by the shell, and a corresponding command outcome is returned to the
% caller.


% Usage example (custom shell, no log or timestamp enabled):

% Welcome to the MyriadGUI shell <0.93.0>.
%
% 1> A=1.
% 1
% 2> B=2.
% 2
% 3> A+B.
% 3
% 4> A
% parsing failed: syntax error before:
% 5> B=1.
% evaluation failed: {badmatch,1}
% 6> self().
% <0.94.0>
% 7> self().
% <0.94.0>
% 8> text_utils:get_timestamp().
% evaluation failed: undef
% 9> time_utils:get_timestamp().
% {{2024,9,3},{22,29,59}}
% 10> observer:start().
% ok % and of course works



% For myriad_spawn_link/1:
-include("spawn_utils.hrl").



%%%
%%% Section for our own custom shell.
%%%


%% Shell user API.



-doc """
Starts a (non-linked) custom shell process with default options, and returns its
PID.

A history of depth ?default_history_max_depth is enabled, and no logging is
performed.
""".
-spec start_custom_shell() -> custom_shell_pid().
start_custom_shell() ->
	start_custom_shell( _Opts=[] ).



-doc """
Starts a (non-linked) custom shell process with the specified options, and
returns its PID.

If logs are enabled, any corresponding file will be deleted first.

See start_custom_shell/0 for defaults.
""".
-spec start_custom_shell( maybe_list( shell_option() ) ) -> custom_shell_pid().
start_custom_shell( Opts ) ->

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Starting (non-linked) custom shell based "
			"on following options:~n ~p.", [ Opts ] ) ),

	% Preferring checking in caller process:
	InitShellState = vet_options_for_custom( Opts ),

	ShellPid = ?myriad_spawn(
		fun() ->
			custom_shell_main_loop( InitShellState )
		end ),

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Started (non-linked) custom shell ~w.",
							   [ ShellPid ] ) ),

	ShellPid.



-doc """
Starts a linked custom shell process with default options, and returns its PID.

See start_custom_shell/0 for defaults.
""".
-spec start_link_custom_shell() -> custom_shell_pid().
start_link_custom_shell() ->
	start_link_custom_shell( _Opts=[] ).



-doc """
Starts a linked custom shell process with the specified options, and returns its
PID.

See start_custom_shell/0 for defaults.
""".
-spec start_link_custom_shell( maybe_list( shell_option() ) ) ->
										custom_shell_pid().
start_link_custom_shell( Opts ) ->

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Starting a linked custom shell based "
			"on following options:~n ~p.", [ Opts ] ) ),

	% Preferring checking in caller process:
	InitShellState = vet_options_for_custom( Opts ),

	ShellPid = ?myriad_spawn_link(
		fun() ->
			custom_shell_main_loop( InitShellState )
		end ),

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Started linked custom shell ~w.",
							   [ ShellPid ] ) ),

	ShellPid.



% (helper)
-spec vet_options_for_custom( maybe_list( shell_option() ) ) ->
											custom_shell_state().
vet_options_for_custom( Opts ) when is_list( Opts ) ->

	% Defaults:
	InitShellState = #custom_shell_state{
		history=queue:new(),
		bindings=erl_eval:new_bindings() },

	vet_options_for_custom( Opts, InitShellState );

vet_options_for_custom( Opt ) ->
	vet_options_for_custom( [ Opt ] ).



% (helper)
vet_options_for_custom( _Opts=[], ShellState ) ->
	ShellState;

vet_options_for_custom( _Opts=[ timestamp | T ], ShellState ) ->
	vet_options_for_custom( T,
		ShellState#custom_shell_state{ do_timestamp=true } );

vet_options_for_custom( _Opts=[ log | T ], ShellState ) ->

	DefaultLogFilename = text_utils:bin_format( "myriad-shell-~ts.log",
		[ text_utils:pid_to_filename( self() ) ] ),

	vet_options_for_custom( [ { log, DefaultLogFilename } | T ], ShellState );

vet_options_for_custom( _Opts=[ { log, AnyLogFilePath } | T ], ShellState ) ->
	BinLogFilePath = text_utils:ensure_binary( AnyLogFilePath ),
	file_utils:remove_file_if_existing( BinLogFilePath ),

	% Cannot be 'raw', as the writer will be the shell process, not the
	% caller one:
	%
	LogFile = file_utils:open( BinLogFilePath, _OpenOpts=[ write, exclusive ] ),

	vet_options_for_custom( T, ShellState#custom_shell_state{
									log_path=BinLogFilePath,
									log_file=LogFile } );


vet_options_for_custom( _Opts=[ { history, MaxLen } | T ], ShellState )
								when is_integer( MaxLen ) andalso MaxLen >= 0 ->
	vet_options_for_custom( T,
		ShellState#custom_shell_state{ history_max_depth=MaxLen } );

vet_options_for_custom( _Opts=[ { history, undefined } | T ], ShellState ) ->
	vet_options_for_custom( T,
		ShellState#custom_shell_state{ history_max_depth=undefined } );

vet_options_for_custom( _Opts=[ no_history | T ], ShellState ) ->
	vet_options_for_custom( T,
		ShellState#custom_shell_state{ history_max_depth=0 } );

vet_options_for_custom( _Opts=[ Other | _T ], _ShellState ) ->
	throw( { unexpected_shell_option, Other } ).



-doc """
Executes the specified command on the specified shell, and returns its result.

Throws an exception on error.
""".
-spec execute_command( any_string(), custom_shell_pid() ) -> command_outcome().
execute_command( CmdAnyStr, ShellPid ) ->
	CmdBinStr = text_utils:ensure_binary( CmdAnyStr ),
	ShellPid ! { processCommand, CmdBinStr, self() },

	% Blocking, so no ShellPid to be pattern-matched to correlate answers:
	receive

		CmdOutcome={ success, _CmdResValue, _CmdId, _MaybeTimestampBinStr } ->
			%CmdResValue;
			CmdOutcome;

		CmdOutcome={ error, ErrorBinStr, _CmdId, _MaybeTimestampBinStr }  ->

			cond_utils:if_defined( myriad_debug_shell,
				trace_utils:error_fmt( "Failed to execute command '~ts' "
					"on custom shell ~w: ~ts",
					[ CmdAnyStr, ShellPid, ErrorBinStr ] ),
				basic_utils:ignore_unused( ErrorBinStr ) ),

			%throw( { shell_command_failed, ErrorBinStr, ShellPid, CmdAnyStr } )
			CmdOutcome

	end.



% Implementation helpers.


-doc "Main loop of a custom shell instance.".
% No specific initialisation needed, like 'process_flag(trap_exit, true)'.
-spec custom_shell_main_loop( custom_shell_state() ) -> no_return().
custom_shell_main_loop( ShellState ) ->

	cond_utils:if_defined( myriad_debug_shell, trace_utils:debug_fmt(
		"Now being ~ts", [ custom_shell_state_to_string( ShellState ) ] ) ),

	% WOOPER-like conventions:
	receive

		{ processCommand, CmdBinStr, ClientPid } ->

			{ CmdOutcome, ProcShellState } =
				process_command_custom( CmdBinStr, ShellState ),

			% A failed command does not kill the shell:
			ClientPid ! CmdOutcome,

			custom_shell_main_loop( ProcShellState );


		flushHistory ->
			custom_shell_main_loop( ShellState#custom_shell_state{
										history=queue:new() } );


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
				"by Myriad custom shell ~w: ~p", [ self(), UnexpectedMsg ] ),

			custom_shell_main_loop( ShellState )

	end.



% (helper)
-spec on_command_success( command(), command_result(), binding_struct(),
	custom_shell_state() ) -> { command_outcome(), custom_shell_state() }.
on_command_success( CmdBinStr, CmdResValue, NewBindings,
					ShellState=#custom_shell_state{
							submission_count=SubCount } ) ->

	% submission_count already incremented:
	ProcShellState = ShellState#custom_shell_state{ bindings=NewBindings },

	% Only updated on success:
	HistShellState = update_history( CmdBinStr, CmdResValue, ProcShellState ),

	MaybeTimestampBinStr =
		manage_success_log( CmdBinStr, CmdResValue, HistShellState ),

	CmdOutcome =
		{ success, CmdResValue, _CmdId=SubCount, MaybeTimestampBinStr },

	{ CmdOutcome, HistShellState }.




% Custom Shell commands.


-doc """
Have this custom shell process the specified command and return its outcome.
""".
-spec process_command_custom( command(), custom_shell_state() ) ->
								{ command_outcome(), custom_shell_state() }.
process_command_custom( CmdBinStr, ShellState=#custom_shell_state{
											submission_count=SubCount,
											bindings=Bindings } ) ->

	cond_utils:if_defined( myriad_debug_shell, trace_utils:debug_fmt(
		"Processing command '~ts'.", [ CmdBinStr ] ) ),

	NewCmdId = SubCount + 1,

	BaseShellState = ShellState#custom_shell_state{ submission_count=NewCmdId },

	% Binaries cannot be scanned as are:
	CmdStr = text_utils:binary_to_string( CmdBinStr ),

	case erl_scan:string( CmdStr ) of

		{ ok, Tokens, EndLocation } ->

			cond_utils:if_defined( myriad_debug_shell,
				trace_utils:debug_fmt(
					"Scanned tokens (end location: ~p):~n ~p",
					[ EndLocation, Tokens ] ),
				basic_utils:ignore_unused( EndLocation ) ),

			case erl_parse:parse_exprs( Tokens ) of

				% Supposedly multiple expression forms can be expected ("EXPR1,
				% EXPR2"):
				%
				% { ok, [ ExprForm ] } ->
				{ ok, ExprForms } ->

					cond_utils:if_defined( myriad_debug_shell,
						trace_utils:debug_fmt( "Parsed following expression "
							"forms:~n ~p", [ ExprForms ] ) ),

					% Currently not using local/non-local function handlers:
					try erl_eval:exprs( ExprForms, Bindings ) of

						{ value, CmdValue, NewBindings } ->
							on_command_success( CmdBinStr, CmdValue,
												NewBindings, BaseShellState )

					catch Class:Reason ->

						cond_utils:if_defined( myriad_debug_shell,
							trace_utils:warning_fmt( "Evaluation error "
								"for command '~ts' by shell ~w: ~p "
								"(class: ~ts)",
								[ CmdBinStr, self(), Reason, Class ] ),
							basic_utils:ignore_unused( Class ) ),

						ReasonBinStr = text_utils:bin_format(
							"evaluation failed: ~p", [ Reason ] ),

						MaybeTimestampBinStr = manage_error_log( CmdBinStr,
							ReasonBinStr, BaseShellState ),

						CmdOutcome = { error, ReasonBinStr, NewCmdId,
									   MaybeTimestampBinStr },

						{ CmdOutcome, BaseShellState }

					end;

				{ error, _ErrorInfo={ Loc, Mod, Desc } } ->

					IssueDesc = ast_utils:interpret_issue_description( Desc,
																	   Mod ),

					cond_utils:if_defined( myriad_debug_shell,
						trace_utils:warning_fmt( "Parse error when evaluating "
							"command '~ts' by shell ~w: ~ts (location: ~ts)",
							[ CmdBinStr, self(), IssueDesc,
							  ast_utils:file_loc_to_string( Loc ) ] ),
						basic_utils:ignore_unused( Loc ) ),

					ReasonBinStr = text_utils:bin_format(
						"parsing failed: ~ts", [ IssueDesc ] ),

					MaybeTimestampBinStr = manage_error_log( CmdBinStr,
						ReasonBinStr, BaseShellState ),

					CmdOutcome = { error, ReasonBinStr, NewCmdId,
								   MaybeTimestampBinStr },

					{ CmdOutcome, BaseShellState }

			end;


		% Not expected to happen frequently:
		{ error, _ErrorInfo={ Loc, Mod, Desc }, ErrorLocation } ->

			IssueDesc = ast_utils:interpret_issue_description( Desc, Mod ),

			cond_utils:if_defined( myriad_debug_shell,
				trace_utils:warning_fmt( "Scan error when evaluating "
					"command '~ts' by shell ~w: ~ts (location: ~ts / ~ts)",
					[ CmdBinStr, self(), IssueDesc,
					  ast_info:location_to_string( Loc ),
					  ast_info:location_to_string( ErrorLocation ) ] ),
				basic_utils:ignore_unused( [ Loc, ErrorLocation ] ) ),

			ReasonBinStr = text_utils:bin_format( "scanning failed: ~ts",
												  [ IssueDesc ] ),

			MaybeTimestampBinStr = manage_error_log( CmdBinStr, ReasonBinStr,
													 BaseShellState ),

			CmdOutcome =
				{ error, ReasonBinStr, NewCmdId, MaybeTimestampBinStr },

			{ CmdOutcome, BaseShellState }

	end.



% (helper)
-spec manage_success_log( command(), command_result(), custom_shell_state() ) ->
										option( timestamp_binstring() ).
manage_success_log( _CmdBinStr, _CmdResValue,
					#custom_shell_state{ do_timestamp=true,
										 log_file=undefined } ) ->
	time_utils:get_bin_textual_timestamp();

manage_success_log( _CmdBinStr, _CmdResValue,
					#custom_shell_state{ do_timestamp=false,
										 log_file=undefined } ) ->
	undefined;

manage_success_log( CmdBinStr, CmdResValue,
					#custom_shell_state{ do_timestamp=true,
										 log_file=LogFile } ) ->
	TimestampBinStr = time_utils:get_bin_textual_timestamp(),

	file_utils:write_ustring( LogFile,
		"[~ts] Command '~ts' -> ~p~n",
		[ TimestampBinStr, CmdBinStr, CmdResValue ] ),

	TimestampBinStr;

manage_success_log( CmdBinStr, CmdResValue,
					#custom_shell_state{ do_timestamp=false,
										 log_file=LogFile } ) ->
	file_utils:write_ustring( LogFile,
		"Command '~ts' -> ~p~n", [ CmdBinStr, CmdResValue ] ),

	undefined.




% (helper)
-spec manage_error_log( command(), command_error(), custom_shell_state() ) ->
										option( timestamp_binstring() ).
manage_error_log( _CmdBinStr, _ReasonBinStr,
				  #custom_shell_state{ do_timestamp=true,
									   log_file=undefined } ) ->
	time_utils:get_bin_textual_timestamp();

manage_error_log( _CmdBinStr, _ReasonBinStr,
				  #custom_shell_state{ do_timestamp=false,
									   log_file=undefined } ) ->
	undefined;

manage_error_log( CmdBinStr, ReasonBinStr,
				  #custom_shell_state{ do_timestamp=true,
									   log_file=LogFile } ) ->
	TimestampBinStr = time_utils:get_bin_textual_timestamp(),

	file_utils:write_ustring( LogFile,
		"[~ts] Evaluation failed for command '~ts': ~ts.~n",
		[ TimestampBinStr, CmdBinStr, ReasonBinStr ] ),

	TimestampBinStr;

manage_error_log( CmdBinStr, ReasonBinStr,
				  #custom_shell_state{ do_timestamp=false,
									   log_file=LogFile } ) ->
	file_utils:write_ustring( LogFile,
		"Evaluation failed for command '~ts': ~ts.~n",
		[ CmdBinStr, ReasonBinStr ] ),

	undefined.




-doc "Updates the shell history for the specified command.".
-spec update_history( command(), command_result(), custom_shell_state() ) ->
										custom_shell_state().
update_history( _Cmd, _CmdRes, ShellState=#custom_shell_state{
											history_max_depth=0 } ) ->
	ShellState;


update_history( Cmd, CmdRes, ShellState=#custom_shell_state{
											history_max_depth=undefined,
											history=HistQ } ) ->

	% No length limit:
	NewHistQ = queue:in( _HistElem={ Cmd, CmdRes }, HistQ ),

	ShellState#custom_shell_state{ history=NewHistQ };


update_history( Cmd, CmdRes, ShellState=#custom_shell_state{
											history_max_depth=HDepth,
											history=HistQ } ) ->
	DropHistQ = case queue:len( HistQ ) of

		HDepth ->
			% Full, thus dropping first (never expected to be empty):
			{ { _ValueAtom, _FirstHItem }, ShrunkHistQ } = queue:out( HistQ ),
			ShrunkHistQ;

		_ ->
			% Not full yet:
			HistQ

	end,

	NewHistQ = queue:in( _HistElem={ Cmd, CmdRes }, DropHistQ ),

	ShellState#custom_shell_state{ history=NewHistQ }.




%%%
%%% Section for integrating a standard shell.
%%%
%%% Currently not functional. Not worth it.


%% Shell user API.



-doc """
Starts a (non-linked) standard shell process with default options, and returns
its PID.
""".
-spec start_standard_shell() -> standard_shell_pid().
start_standard_shell() ->
	start_standard_shell( _Opts=[] ).



-doc """
Starts a (non-linked) standard shell process with the specified options, and
returns its PID.

See start_standard_shell/0 for defaults.
""".
-spec start_standard_shell( maybe_list( shell_option() ) ) ->
											standard_shell_pid().
start_standard_shell( Opts ) ->

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Starting (non-linked) standard shell based "
			"on following options:~n ~p.", [ Opts ] ) ),

	% Preferring checking in caller process:
	InitShellState = vet_options_for_standard( Opts ),

	ShellPid = ?myriad_spawn(
		fun() ->
			standard_shell_init( InitShellState )
		end ),

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Started (non-linked) standard shell ~w.",
							   [ ShellPid ] ) ),

	ShellPid.



-doc """
Starts a linked standard shell process with default options, and returns its
PID.

See start_standard_shell/0 for defaults.
""".
-spec start_link_standard_shell() -> standard_shell_pid().
start_link_standard_shell() ->
	start_link_standard_shell( _Opts=[] ).



-doc """
Starts a linked standard shell process with the specified options, and returns
its PID.

See start_standard_shell/0 for defaults.
""".
-spec start_link_standard_shell( maybe_list( shell_option() ) ) ->
										standard_shell_pid().
start_link_standard_shell( Opts ) ->

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Starting a linked standard shell based "
			"on following options:~n ~p.", [ Opts ] ) ),

	% Preferring checking in caller process:
	InitShellState = vet_options_for_standard( Opts ),

	ShellPid = ?myriad_spawn_link(
		fun() ->
			standard_shell_init( InitShellState )
		end ),

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Started linked standard shell ~w.",
							   [ ShellPid ] ) ),

	ShellPid.



% (helper)
-spec vet_options_for_standard( maybe_list( shell_option() ) ) ->
											standard_shell_state().
vet_options_for_standard( Opts ) when is_list( Opts ) ->

	% Defaults:
	InitShellState = #standard_shell_state{},

	vet_options_for_standard( Opts, InitShellState );

vet_options_for_standard( Opt ) ->
	vet_options_for_standard( [ Opt ] ).


% (helper)
%vet_options_for_standard( _Opts=[], ShellState ) ->
vet_options_for_standard( _Opts, ShellState ) ->
	ShellState.



-doc "Initialises and runs a standard shell process.".
-spec standard_shell_init( standard_shell_state() ) -> no_return().
standard_shell_init( ShellState ) ->

	process_flag( trap_exit, true ),

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug( "Initialising standard shell." ) ),


	% Starting like lib/ssh/src/ssh_cli.erl or lib/kernel/src/user_drv.erl:

	Ancestors = [ self() | case get( '$ancestors' ) of
								undefined -> [];
								Anc -> Anc
						   end ],

	Drv = self(),

	%Shell = {},
	%Shell = {RemoteNode, M, F, A},
	Shell = { _Mod=shell, _Fun=start, _Args=[ init ] },

	%GrpOpts = [],
	%GrpOpts = [ {echo,true}, {noshell,true} ],
	GrpOpts = [ { dumb, false }, { expand_below, true },
				{ echo, true } ], % {expand_fun, ...

	GrpLeaderPid = spawn_link( group, server,
							   [ Ancestors, Drv, Shell, GrpOpts ] ),

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Created group leader ~w.", [ GrpLeaderPid ] ) ),

	% Like primtty.erl:
	%user_drv ! { self(), enable },
	% user_drv ! { self(), {data, unicode:characters_to_binary("Z=3.") }},

	% GrpLeaderPid = group:start( _Drv=self(), _Shell={}, GroupOpts ),
	% %GrpLeaderPid = group:start( _Drv=self(), ActualShellPid, GroupOpts ),

	% trace_utils:debug_fmt( "~w created group ~w.", [ self(), GrpLeaderPid ] ),
	% GrpLeaderPid ! {driver_id,self()},
	% receive

	%	{GrpLeaderPid,driver_id,DrvPid} ->
	%		trace_utils:debug_fmt("Driver: ~w", [ DrvPid ] )

	% end,

	%FirstText = text_utils:format("Hello!~n", []),
	%FirstText = text_utils:format("A=1.~n", []),

	%UTF8Binary = unicode:characters_to_binary(
	%    io_lib:format("~ts", [FirstText])),

	% Never managed to have an answer; either too short/incomplete, or some
	% extra signal to trigger evaluation must happen:
	%
	%GrpLeaderPid ! { self(), { data, UTF8Binary } },

	%GrpLeaderPid ! {self(), echo, true},
	%GrpLeaderPid ! {self(),tty_geometry,{0,0}},
	%GrpLeaderPid ! { _DrvPid=self(), {data, <<"X=9.\n">>} },
	%GrpLeaderPid ! { _DrvPid=self(), {data, <<"X=9.">>} },
	%GrpLeaderPid ! { _DrvPid=self(), {data, "X=9."} },
	Req = {put_chars,unicode, <<"X=1.\n">>},
	From=self(),
	ReplyAs=self(),
	FullReq = {io_request,From,ReplyAs,Req},
	GrpLeaderPid ! FullReq,

	GrpLeaderPid ! {io_request,From,ReplyAs,{put_chars,unicode, <<"Y=2.\n">>}},

	Prompt = <<"ABC\n">>,

	GrpLeaderPid ! {io_request,From,ReplyAs,{get_chars,unicode, Prompt}},


	% We mimic user_drv:start/0; corresponds to group:start/0,
	% gen_statem:start/4, then start_user/0:

	% Start a group leader process and register it as 'user', unless a 'user'
	% already exists (probably off-topic):
	%
	%% _UserGrpLeaderPid = case whereis(user) of
	%%
	%%	undefined ->
	%%		UserPid = group:start(_Drv=self(), _Shell={},
	%%							  _Opts=[{echo,false}, {noshell,true}]),
	%%		trace_utils:debug_fmt( "Creation of 'user' group ~w.",
	%% [ UserPid ] ),
	%%		register(user, UserPid),
	%%		UserPid;

	%%	UserPid ->
	%%		trace_utils:debug( "No 'user' group creation." ),
	%%		UserPid

	%% end,

	InitShellState = ShellState#standard_shell_state{
		current_group=GrpLeaderPid },

	standard_shell_main_loop( InitShellState ).



-doc "Main loop of an standard shell instance.".
-spec standard_shell_main_loop( standard_shell_state() ) -> no_return().
standard_shell_main_loop( ShellState=#standard_shell_state{
										current_group=GrpLeaderPid } ) ->

	cond_utils:if_defined( myriad_debug_shell, trace_utils:debug_fmt(
		"Standard shell ~w waiting for messages.", [ self() ] ) ),

	receive

%% {<0.89.0>,
%%   {put_chars_sync,unicode,
%%         <<"Eshell V15.0 (press Ctrl+G to abort, type help(). for help)\n">>,
%%        {<0.90.0>,#Ref<0.1174729555.344195076.134393>}}}


		% For example MsgBin may be <<"Eshell V15.0 (press Ctrl+G to abort, type
		% help(). for help)\n">>:
		%
		{ GrpLeaderPid, { put_chars_sync, unicode, MsgBin,
						  { _From=ActualShellPid, ReplyRef } } } ->
			trace_utils:debug_fmt( "Display request from actual shell ~w: "
				"'~ts'.", [ ActualShellPid, MsgBin ] ),

% If not answering, ActualShellPid crashes with:

%% {terminated,[{io,fwrite,
%%                  ["Warning! The slogan \"~p\" could not be printed.\n",
%%                   [[69,115,104,101,108,108,32,86,"15.0"]]],
%%                  [{file,"io.erl"},
%%                   {line,198},
%%                   {error_info,#{cause => {io,terminated},
%%                                 module => erl_stdlib_errors}}]},
%%             {shell,server,1,[{file,"shell.erl"},{line,289}]}]}
			ActualShellPid ! {reply, ReplyRef, ok},

			standard_shell_main_loop( ShellState );

		{ send, UTF8Binary } ->
			% No effect:
			GrpLeaderPid ! { self(), { data, UTF8Binary } },
			standard_shell_main_loop( ShellState );

		{ processCommand, CmdBinStr, ClientPid } ->

			{ CmdOutcome, ProcShellState } =
				process_command_standard( CmdBinStr, ShellState ),

			trace_utils:debug_fmt( "Sending back outcome '~p' to client.",
								   [ CmdOutcome ] ),

			% A failed command does not kill the shell:
			ClientPid ! CmdOutcome,

			standard_shell_main_loop( ProcShellState );

		Msg ->
			trace_utils:warning_fmt( "Standard shell ~w received and ignored "
				"the following message:~n ~p", [ self(), Msg ] ),

			standard_shell_main_loop( ShellState )

	end.




% Standard Shell commands.


-doc """
Have this standard shell process the specified command, and return its outcome.
""".
-spec process_command_standard( command(), standard_shell_state() ) ->
								{ command_outcome(), standard_shell_state() }.
process_command_standard( CmdBinStr, ShellState=#standard_shell_state{
											current_group=GrpLeaderPid } ) ->

	%timer:sleep(1000),
	cond_utils:if_defined( myriad_debug_shell, trace_utils:debug_fmt(
		"Processing command '~ts'.", [ CmdBinStr ] ) ),

	ConvBin = case unicode:characters_to_binary( CmdBinStr ) of

		B when is_binary( B ) ->
			B;

		Other ->
			throw( { bin_conv_failed, Other } )

	end,

	GrpLeaderPid ! { self(), { data, ConvBin } },
	%GrpLeaderPid ! { self(), { data, <<"io:format(\"Anyone?\").">> } },
	GrpLeaderPid ! { self(), { data, <<"help().\n"/utf8>> } },
	GrpLeaderPid ! { self(), { data, <<"A=1.\n/utf8">> } },
	GrpLeaderPid ! { self(), { data, <<"B=C+1.\n/utf8">> } },
	%GrpLeaderPid ! {put_chars, unicode, <<"X=1."/utf8>>},
	%GrpLeaderPid ! { self(), {put_chars, unicode, <<"X=1."/utf8>>} },
	%GrpLeaderPid ! { self(), eof },

	receive

		Any ->
			trace_utils:debug_fmt( "Got ~p.", [ Any ] ),
			{ Any, ShellState }

	end.



% Helpers


-doc "Returns a textual description of the specified custom shell state.".
-spec custom_shell_state_to_string( custom_shell_state() ) -> ustring().
custom_shell_state_to_string( ShellState ) ->
	custom_shell_state_to_string( ShellState, _Verbose=true ).



-doc """
Returns a textual description of the specified custom shell state, with the
specified verbosity.
""".
-spec custom_shell_state_to_string( custom_shell_state(), boolean() ) ->
											ustring().
custom_shell_state_to_string( #custom_shell_state{
								submission_count=SubCount,
								history_max_depth=HistMaxDepth,
								history=History,
								bindings=BindingStruct },
							  _Verbose=true ) ->
	HistStr = case HistMaxDepth of

		0 ->
			"no history";

		undefined ->

			text_utils:format( "an unlimited ~ts",
							   [ history_to_string( History ) ] );

		_ ->
			text_utils:format( "a ~B-deep ~ts",
							   [ HistMaxDepth, history_to_string( History ) ] )

	end,

	text_utils:format( "custom shell ~w with ~ts and ~B commands "
		"already submitted, with ~ts",
		[ self(), bindings_to_string( BindingStruct ), SubCount, HistStr ] );


custom_shell_state_to_string( #custom_shell_state{
								submission_count=SubCount,
								%history=History,
								bindings=BindingStruct },
							  _Verbose=false ) ->
	text_utils:format( "custom shell with ~B bindings, and ~B commands already "
		"submitted",
		[ length( erl_eval:bindings( BindingStruct ) ), SubCount ] ).



-doc "Returns a textual description of the specified bindings.".
-spec bindings_to_string( binding_struct() ) -> ustring().
bindings_to_string( BindingStruct ) ->
	case erl_eval:bindings( BindingStruct ) of

		[] ->
			"no binding";

		[ Binding ] ->
			text_utils:format( "a single binding: '~ts'",
							   [ binding_to_string( Binding ) ] );

		Bindings ->
			text_utils:format( "~B bindings: ~ts",
				[ length( Bindings ),
				  text_utils:strings_to_string( [ binding_to_string( B )
						|| B <- lists:sort( Bindings ) ] ) ] )

	end.



-doc "Returns a textual description of the specified bindings.".
-spec binding_to_string( binding() ) -> ustring().
binding_to_string( _Binding={ N, V } ) ->
	text_utils:format( "variable '~ts' has for value ~p", [ N, V ] ).



-doc "Returns a textual description of the specified history.".
-spec history_to_string( history() ) -> ustring().
history_to_string( History ) ->

	case queue:len( History ) of

		0 ->
			"empty history";

		_ ->
			Strs = [ history_element_to_string( HE )
						|| HE <- queue:to_list( History ) ],

			text_utils:format( "history corresponding to: ~ts",
				[ text_utils:strings_to_enumerated_string( Strs ) ] )

	end.



-doc "Returns a textual description of the specified history element.".
-spec history_element_to_string( history_element() ) -> ustring().
history_element_to_string( { Cmd, CmdRes } ) ->
	text_utils:format_ellipsed( "command '~ts' that resulted in ~p",
								[ Cmd, CmdRes ] ).



-doc "Returns a textual description of the specified standard shell state.".
-spec standard_shell_state_to_string( standard_shell_state() ) -> ustring().
standard_shell_state_to_string( ShellState ) ->
	standard_shell_state_to_string( ShellState, _Verbose=true ).


-doc """
Returns a textual description of the specified standard shell state, with the
specified verbosity.
""".
-spec standard_shell_state_to_string( standard_shell_state(), boolean() ) ->
											ustring().
standard_shell_state_to_string( #standard_shell_state{
									current_group=CurrentGrpPid },
								_Verbose ) ->
	text_utils:format( "standard shell ~w relying on group leader ~w",
					   [ self(), CurrentGrpPid ] ).
