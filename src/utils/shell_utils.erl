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
		  start_shell/1, start_link_shell/1,
		  start_integrated_shell/0,

		  execute_command/2 ]).


% To enable spawns of them:
-export([ start_integrated_helper/0 ]).


% At least for silencing:
-export([ shell_state_to_string/1 ]).


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


-doc "The number of a command, which is an identifier thereof.".
-type command_id() :: count().


-doc "The result of a command, as evaluated by a shell.".
-type command_result() :: variable_value().

-doc "An error generated when a shell evaluates a submitted command.".
-type command_error() :: bin_string().


-doc "The information returned once a command is processed.".
-type command_info() ::

	{ 'success', command_result(), command_id(),
	  MaybeTimestampBinStr :: option( bin_timestamp() ) }

  | { 'error', command_error(),
	  MaybeTimestampBinStr :: option( bin_timestamp() ) }.


% An element kept in the history:
-type history_element() :: { command(), command_result() }.


-doc """
An history of a shell, that is the list of the previously submitted commands and
of their results (here in antichronological order).

Note that this may allow huge terms to be kept around longer than expected (see
the flushHistory request message).
""".
-type history() :: queue( history_element() ).


-doc """
Options that can be specified when creating a shell:

- 'timestamp': keep track also of the timestamp of the start of a command

- 'log': logs the commands and their results, in a file whose default name is
  `myriad-shell-SHELL_PID.log`, where SHELL_PID corresponds to the PID of the
  shell creator, like in `myriad-shell-0.84.0.log` (then written in the current
  directory)

- {'log', LogPath :: file_utils:any_file_path()}: logs the commands and their
  results in a file whose path is specified

- {'history', MaxDepth :: count()}: records a command history of the specified
 maximum depth.

- no_history: do not store any command history
""".
-type shell_option() ::
	'timestamp'
 |  'log'
 | { 'log', LogPath :: any_file_path() }
 | { 'history', MaybeMaxDepth :: option( count() ) }
 |  'no_history'.



-export_type([ shell_pid/0, client_pid/0,
			   variable_name/0, variable_value/0,
			   binding/0,

			   command/0, command_id/0, command_result/0, command_error/0,
			   command_info/0,

			   history/0 ]).


-define( default_history_max_depth, 20 ).



% Local types:

-type bin_timestamp() :: bin_string().


% Defaults set in vet_options/1:
-record( shell_state, {

	% The number of commands submitted; corresponds to the number (identifier)
	% of any current command (or the one of the next command, minus 1):
	%
	submission_count :: count(),

	% Tells whether the start of commands shall be timestamped:
	do_timestamp :: boolean(),


	% Tells whether the full history (inputs and outputs) shall be logged on
	% file and, if yes, in which one:
	%
	log_path :: option( bin_file_path() ),

	% The file (if any) where logs are to be written:
	log_file :: option( file() ),


	% Maximum number of history elements (0: none, 1: just the last one, etc.;
	% undefined: infinite):
	%
	history_max_depth :: option( count() ),

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

-type maybe_list( T ) :: list_utils:maybe_list( T ).

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
% - (A) define our own version of it, from scratch, based on our own
% read/scan/eval loop; we prefer here that the shell resists to any failure
% induced by user commands (e.g. not losing its bindings then); it is, at least
% currently, a very basic shell: no command recall, no history, no built-in
% functions (like f()), no shell switching, no remote shell, etc; we considered
% supporting an auto_add_trailing_dot option ("Tells whether a trailing dot
% should be automatically added if lacking in a command") yet it was probably
% not a relevant idea)
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
Starts a (non-linked) shell process according to approach A with default
options, and returns its PID.

If logs are enabled, any corresponding file will be deleted first.

A history of depth ?default_history_max_depth is enabled, and no logging is
performed.
""".
-spec start_shell() -> shell_pid().
start_shell() ->
	start_shell( _Opts=[] ).



-doc """
Starts a (non-linked) shell process according to approach A with the specified
options, and returns its PID.

See start_shell/0 for defaults.
""".
-spec start_shell( maybe_list( shell_option() ) ) -> shell_pid().
start_shell( Opts ) ->

	% Preferring checking in caller process:
	InitShellState = vet_options( Opts ),

	ShellPid = ?myriad_spawn(
		fun() ->
			shell_main_loop( InitShellState )
		end ),

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Started (non-linked) shell ~w.",
							   [ ShellPid ] ) ),

	ShellPid.



-doc """
Starts a linked shell process according to approach A with default options, and
returns its PID.

See start_shell/0 for defaults.
""".
-spec start_link_shell() -> shell_pid().
start_link_shell() ->
	start_link_shell( _Opts=[] ).



-doc """
Starts a linked shell process according to approach A, and returns its PID.

See start_shell/0 for defaults.
""".
-spec start_link_shell( maybe_list( shell_option() ) ) -> shell_pid().
start_link_shell( Opts ) ->

	% Preferring checking in caller process:
	InitShellState = vet_options( Opts ),

	ShellPid = ?myriad_spawn_link(
		fun() ->
			shell_main_loop( InitShellState )
		end ),

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Started linked shell ~w.", [ ShellPid ] ) ),

	ShellPid.



% (helper)
-spec vet_options( maybe_list( shell_option() ) ) -> shell_state().
vet_options( Opts ) when is_list( Opts ) ->

	% Defaults:
	InitShellState = #shell_state{
		submission_count=0,
		do_timestamp=false,
		log_path=undefined,
		log_file=undefined,
		history_max_depth=?default_history_max_depth,
		history=queue:new(),
		bindings=erl_eval:new_bindings() },

	vet_options( Opts, InitShellState );

vet_options( Opt ) ->
	vet_options( [ Opt ] ).



% (helper)
vet_options( _Opts=[], ShellState ) ->
	ShellState;

vet_options( _Opts=[ timestamp | T ], ShellState ) ->
	vet_options( T, ShellState#shell_state{ do_timestamp=true } );

vet_options( _Opts=[ log | T ], ShellState ) ->

	DefaultLogFilename = text_utils:format( "myriad-shell-~ts.log",
		[ text_utils:pid_to_filename( self() ) ] ),

	vet_options( [ { log, DefaultLogFilename } | T ], ShellState );

vet_options( _Opts=[ { log, AnyLogFilePath } | T ], ShellState ) ->
	BinLogFilePath = text_utils:ensure_binary( AnyLogFilePath ),
	file_utils:remove_file_if_existing( BinLogFilePath ),

	% Cannot be 'raw', as the writer will be the shell process, not the
	% caller one:
	%
	LogFile = file_utils:open( BinLogFilePath, _OpenOpts=[ write, exclusive ] ),

	vet_options( T, ShellState#shell_state{ log_path=BinLogFilePath,
											log_file=LogFile } );


vet_options( _Opts=[ { history, MaxLen } | T ], ShellState )
						when is_integer( MaxLen ) andalso MaxLen >= 0 ->
	vet_options( T, ShellState#shell_state{ history_max_depth=MaxLen } );

vet_options( _Opts=[ { history, undefined } | T ], ShellState ) ->
	vet_options( T, ShellState#shell_state{ history_max_depth=undefined } );

vet_options( _Opts=[ no_history | T ], ShellState ) ->
	vet_options( T, ShellState#shell_state{ history_max_depth=0 } ).



-doc """
Executes the specified command on the specified shell, and returns its result.

Throws an exception on error.
""".
-spec execute_command( any_string(), shell_pid() ) -> command_info().
execute_command( CmdAnyStr, ShellPid ) ->
	CmdBinStr = text_utils:ensure_binary( CmdAnyStr ),
	ShellPid ! { processCommand, CmdBinStr, self() },
	receive

		CmdInfo={ success, _CmdResValue, _CmdId, _MaybeTimestampBinStr } ->
			%CmdResValue;
			CmdInfo;

		CmdInfo={ error, ErrorBinStr, _MaybeTimestampBinStr }  ->

			cond_utils:if_defined( myriad_debug_shell,
				trace_utils:error_fmt( "Failed to execute command '~ts' "
					"on shell ~w: ~ts",
					[ CmdAnyStr, ShellPid, ErrorBinStr ] ),
				basic_utils:ignore_unused( ErrorBinStr ) ),

			%throw( { shell_command_failed, ErrorBinStr, ShellPid, CmdAnyStr } )
			CmdInfo

	end.



% Implementation helpers.


-doc "Main loop of a shell instance (approach A).".
-spec shell_main_loop( shell_state() ) -> no_return().
shell_main_loop( ShellState ) ->

	cond_utils:if_defined( myriad_debug_shell, trace_utils:debug_fmt(
		"Now being ~ts", [ shell_state_to_string( ShellState ) ] ) ),

	% WOOPER-like conventions:

	receive

		{ processCommand, CmdBinStr, ClientPid } ->

			{ CmdInfo, ProcShellState } =
				process_command( CmdBinStr, ShellState ),

			% A failed command does not kill the shell:
			ClientPid ! CmdInfo,

			shell_main_loop( ProcShellState );


		flushHistory ->
			shell_main_loop( ShellState#shell_state{ history=queue:new() } );


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

			shell_main_loop( ShellState )

	end.



% (helper)
-spec on_command_success( command(), command_result(), binding_struct(),
						  shell_state() ) -> { command_info(), shell_state() }.
on_command_success( CmdBinStr, CmdResValue, NewBindings,
					ShellState=#shell_state{ submission_count=SubCount } ) ->

	% submission_count already incremented:
	ProcShellState = ShellState#shell_state{ bindings=NewBindings },

	% Only updated on success:
	HistShellState = update_history( CmdBinStr, CmdResValue, ProcShellState ),

	MaybeTimestampBinStr =
		manage_success_log( CmdBinStr, CmdResValue, HistShellState ),

	CmdInfo = { success, CmdResValue, _CmdId=SubCount, MaybeTimestampBinStr },

	{ CmdInfo, HistShellState }.




% Shell commands.
%
% We are still in the context of approach A.


-doc "Has this shell process the specified command.".
-spec process_command( command(), shell_state() ) ->
								{ command_info(), shell_state() }.
process_command( CmdBinStr, ShellState=#shell_state{ submission_count=SubCount,
													 bindings=Bindings } ) ->

	cond_utils:if_defined( myriad_debug_shell, trace_utils:debug_fmt(
		"Processing command '~ts'.", [ CmdBinStr ] ) ),

	BaseShellState = ShellState#shell_state{
		submission_count=SubCount+1 },

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

						CmdInfo={ error, ReasonBinStr, MaybeTimestampBinStr },

						{ CmdInfo, BaseShellState }

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

					{ _CmdInfo={ error, ReasonBinStr, MaybeTimestampBinStr },
					  BaseShellState }

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

			% Current command identifier unchanged:
			{ _CmdInfo={ error, ReasonBinStr, MaybeTimestampBinStr },
			  BaseShellState }

	end.


% (helper)
-spec manage_success_log( command(), command_result(), shell_state() ) ->
										option( bin_timestamp() ).
manage_success_log( _CmdBinStr, _CmdResValue,
					#shell_state{ do_timestamp=true,
								  log_file=undefined } ) ->
	time_utils:get_bin_textual_timestamp();

manage_success_log( _CmdBinStr, _CmdResValue,
					#shell_state{ do_timestamp=false,
								  log_file=undefined } ) ->
	undefined;

manage_success_log( CmdBinStr, CmdResValue,
					#shell_state{ do_timestamp=true,
								  log_file=LogFile } ) ->
	TimestampBinStr = time_utils:get_bin_textual_timestamp(),

	file_utils:write_ustring( LogFile,
		"[~ts] Command '~ts' -> ~p~n",
		[ TimestampBinStr, CmdBinStr, CmdResValue ] ),

	TimestampBinStr;

manage_success_log( CmdBinStr, CmdResValue,
					#shell_state{ do_timestamp=false,
								  log_file=LogFile } ) ->
	file_utils:write_ustring( LogFile,
		"Command '~ts' -> ~p~n", [ CmdBinStr, CmdResValue ] ),

	undefined.




% (helper)
-spec manage_error_log( command(), command_error(), shell_state() ) ->
										option( bin_timestamp() ).
manage_error_log( _CmdBinStr, _ReasonBinStr,
				  #shell_state{ do_timestamp=true,
								log_file=undefined } ) ->
	time_utils:get_bin_textual_timestamp();

manage_error_log( _CmdBinStr, _ReasonBinStr,
				  #shell_state{ do_timestamp=false,
								log_file=undefined } ) ->
	undefined;

manage_error_log( CmdBinStr, ReasonBinStr,
				  #shell_state{ do_timestamp=true,
								log_file=LogFile } ) ->
	TimestampBinStr = time_utils:get_bin_textual_timestamp(),

	file_utils:write_ustring( LogFile,
		"[~ts] Evaluation failed for command '~ts': ~ts.~n",
		[ TimestampBinStr, CmdBinStr, ReasonBinStr ] ),

	TimestampBinStr;

manage_error_log( CmdBinStr, ReasonBinStr,
				  #shell_state{ do_timestamp=false,
								log_file=LogFile } ) ->
	file_utils:write_ustring( LogFile,
		"Evaluation failed for command '~ts': ~ts.~n",
		[ CmdBinStr, ReasonBinStr ] ),

	undefined.




-doc "Updates the shell history for the specified command.".
-spec update_history( command(), command_result(), shell_state() ) ->
										shell_state().
update_history( _Cmd, _CmdRes, ShellState=#shell_state{
											history_max_depth=0 } ) ->
	ShellState;


update_history( Cmd, CmdRes, ShellState=#shell_state{
											history_max_depth=undefined,
											history=HistQ } ) ->

	% No length limit:
	NewHistQ = queue:in( _HistElem={ Cmd, CmdRes }, HistQ ),

	ShellState#shell_state{ history=NewHistQ };


update_history( Cmd, CmdRes, ShellState=#shell_state{
											history_max_depth=HDepth,
											history=HistQ } ) ->
	DropHistQ = case queue:len( HistQ ) of

		HDepth ->
			% Full, thus dropping first (never expected to be empty):
			{ { value, _FirstHItem }, ShrunkHistQ } = queue:out( HistQ ),
			ShrunkHistQ;

		_ ->
			% Not full yet:
			HistQ

	end,

	NewHistQ = queue:in( _HistElem={ Cmd, CmdRes }, DropHistQ ),

	ShellState#shell_state{ history=NewHistQ }.





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

	text_utils:format( "shell ~w with ~ts and ~B commands already submitted, "
		"with ~ts",
		[ self(), bindings_to_string( BindingStruct ), SubCount, HistStr ] );


shell_state_to_string( #shell_state{
							submission_count=SubCount,
							%history=History,
							bindings=BindingStruct },
					   _Verbose=false ) ->
	text_utils:format( "shell with ~B bindings, and ~B commands already "
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
