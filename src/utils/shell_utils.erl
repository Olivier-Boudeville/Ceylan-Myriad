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

This shell supports (possibly timestamped) logging, (possibly bounded) command
and result histories, persistent command history, built-in shell commands, which
can be overridden/enriched based on callback modules.

It can be of course local to the current node, or remote.

See shell_utils_test.erl for its testing, and gui_shell_test.erl for an
example of use thereof.
""".


% Two versions were targeted:
%
% - a custom shell, using our own Myriad conventions; we use it and are fond of
% it
%
% - a standard shell, integrated in Erlang native subsystems; we dropped it (see
% reasons in implementation notes below)



% User API:
-export([ start_custom_shell/0, start_link_custom_shell/0,
		  start_custom_shell/1, start_link_custom_shell/1

		%,execute_command/2

 ]).



% At least for silencing:
-export([ % As not ready for use:
		  start_standard_shell/0, start_link_standard_shell/0,
		  start_standard_shell/1, start_link_standard_shell/1,

		  custom_shell_state_to_string/1, custom_shell_state_to_string/2,
		  standard_shell_state_to_string/1, standard_shell_state_to_string/2 ]).


% Helpers directly called by shell callbacks:
-export([ command_history_to_string_with_ids/1,
		  result_history_to_string_with_ids/1, recall_command/2 ]).


% Various other helpers:
-export([ command_history_to_string/1, result_history_to_string/1,

		  filter_bindings/1,
		  bindings_to_string/1, bindings_to_command_string/1,
		  binding_to_string/1 ]).


-doc "The PID of a (Myriad) shell process.".
-type shell_pid() :: custom_shell_pid() | standard_shell_pid().


-doc "The PID of a Myriad custom shell process.".
-type custom_shell_pid() :: processor_pid().


-doc "The PID of a Myriad standard shell process.".
-type standard_shell_pid() :: processor_pid().



-doc "The PID of a shell client process.".
-type client_pid() :: pid().



-doc """
Surprisingly, not a string-like, but term(); in practice, at least generally, an
atom.
""".
-type variable_name() :: erl_eval:name().


-doc "String-based variable name.".
-type variable_string_name() :: ustring().


% In AST form:
-type variable_ast_name() :: ast_base:form().

% term():
-type variable_value() :: erl_eval:value().



% Apparently not exported as a standalone:
-doc "Logical binding for a variable, as held by a shell.".
-type binding() :: { variable_name(), variable_value() }.



-doc """
A command to submit to a shell, corresponding to a sequence of expressions.

For example: <<"A=1, B=2, A+B.">>.
""".
-type command() :: entry().


-doc """
A command to submit to a shell, corresponding to a sequence of
expressions, as a string.

For example: "A=1, B=2, A+B.".
""".
-type command_str() :: entry_str().


-doc "Any message to be displayed on the shell (e.g. an error).".
-type message() :: ustring().


-doc """
The number (in their history: count since shell start) of a command, which is an
identifier thereof.
""".
-type command_id() :: entry_id().


-doc """
The result of a command, as evaluated by a shell.

More precise than text_edit:process_result/0.
""".
-type command_result() :: variable_value().


-doc "An error message generated when a shell evaluates a submitted command.".
-type command_error() :: text_edit:process_error().


-doc """
The information returned once a command is processed.

A specialisation of text_edit:process_outcome/0.
""".
-type command_outcome() ::

	{ 'success', command_result(), ThisCmdId :: command_id(),
	  MaybeTimestampBinStr :: option( timestamp_binstring() ) }

  | { 'error', command_error(),
	  MaybeTimestampBinStr :: option( timestamp_binstring() ) }.




-doc """
A command history of a shell, that is a list of previously submitted commands
(here in antichronological order).
""".
-type command_history() :: queue( command() ).


-doc """
A result history of a shell, that is a list of the results of previously
submitted commands (here in antichronological order).

Note that this may allow huge terms to be kept around longer than expected (see
the flushResultHistory request message to avoid that).
""".
-type result_history() :: queue( command_result() ).



-doc "Result of the evaluation of a built-in command.".
-type builtin_command_result(T) :: { custom_shell_state(), T }.


-doc """
Result of the evaluation of a built-in command not returning any result of
interest.
""".
% Deemed clearer than 'void':
-type builtin_state_only() :: builtin_command_result( 'ok' ).




-doc """
Options that can be specified when creating a shell:

- 'timestamp': keep track also of the timestamp of the start of a command

- 'log': logs the commands and their results, in a file whose default name is
  `myriad-shell-for-CREATOR_PID-on-CREATION_TIMESTAMP.log`, where CREATOR_PID
  corresponds to the PID of the shell creator, and CREATION_TIMESTAMP is the
  timestamp of the creation of that shell, like in
  `myriad-shell-for-0.93.0-on-2024-11-17-at-14h-31m-19s.log` (then written in
  the current directory)

- {'log', LogPath :: any_file_path()}: logs the commands and their results in a
  file whose path is specified

- {'histories', MaxCmdDepth :: option(count()), MaxResDepth :: option(count())}:
 records a command and result histories of the specified maximum depths,
 'undefined' meaning unlimited depth (beware to memory footprint)

- no_histories: does not record any command or result history (synonym of
  {histories,0,0})

- persistent_command_history: the command history is stored in the filesystem
  for convenience, so that it can be reloaded when launching new shell instances
  (then the number of the first entered command will be the next one after the
  full history); note that it is then an history common to all shell instances,
  thus collecting their various commands (according to a maximum depth of its
  own, see the persistant_command_history_depth define); such a file will be
  reused and then updated iff this option is selected (i.e. this option enables
  both its reading and its writing)

- callback_module: to specify the name of any implementation for the shell
  callback module (see 'shell_default_callbacks' for the default one)
""".
-type shell_option() ::
	'timestamp'
 |  'log'
 | { 'log', LogPath :: any_file_path() }
 | { 'histories', MaxCmdDepth :: option( count() ),
	 MaxCmdDepth :: option( count() ), MaxResDepth :: option( count() ) }
 |  'no_histories'
 | 'persistent_command_history'
 | { 'callback_module', module_name() }.



-doc """
The PID of a group leader process for user IO (see lib/kernel/src/group.erl).
""".
-type group_pid() :: pid().


-export_type([ shell_pid/0, custom_shell_pid/0, standard_shell_pid/0,
			   client_pid/0,

			   variable_name/0, variable_string_name/0, variable_ast_name/0,
			   variable_value/0, binding/0,

			   command/0, command_str/0, message/0, command_id/0,
			   command_result/0, command_error/0, command_outcome/0,

			   command_history/0, result_history/0,

			   builtin_command_result/1, builtin_state_only/0,

			   group_pid/0 ]).




% Stored at the root of the account of the current user:
-define( persistant_command_history_filename,
		 ".ceylan-myriad-shell-history.dat" ).

% The maximum depth of the persistant command history (possibly exceeds the
% depth of the command history of a given shell):
%
-define( persistant_command_history_depth, 50 ).
%-define( persistant_command_history_depth, 2 ).


% For the custom_shell_state record:
-include("shell_utils.hrl").


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
%-type any_string() :: text_utils:any_string().

-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type module_name() :: meta_utils:module_name().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type timestamp_binstring() :: time_utils:timestamp_binstring().

%-type bin_file_path() :: file_utils:bin_file_path().
-type any_file_path() :: file_utils:any_file_path().
%-type file() :: file_utils:file().

% Not a [binding()]:
-type binding_struct() :: erl_eval:binding_struct().

-type queue( T ) :: queue:queue( T ).

-type eval_error_term() :: term().

-type entry() :: text_edit:entry().
-type entry_str() :: text_edit:entry_str().
-type entry_id() :: text_edit:entry_id().
-type processor_pid() :: text_edit:processor_pid().



% Implementation notes:
%
% Perhaps that a smart use of the built-in 'shell' module could have sufficed.
%
% We see two approaches in order to implement such a separate shell:
%
% - (A) define our own custom version of it, from scratch, based on our own
% read/scan/eval loop; we prefer here that the shell resists to any failure
% induced by user commands (e.g. not losing its bindings then); it is, at least
% currently, a very basic shell: no shell switching, no remote shell, etc; we
% considered supporting an auto_add_trailing_dot option ("Tells whether a
% trailing dot should be automatically added if lacking in a command") yet it
% was probably not a relevant idea at the shell level; on the contrary, a
% single-line shell client (e.g. gui_shell) may support such an option
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
%  * https://erlangforums.com/t/adding-repl-like-feature-to-a-graphical-erlang-application/3795 (using edlin)
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
% process messages to the shell), so that the corresponding command is evaluated
% by the shell, and a corresponding command outcome is returned to the caller.


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


% Shell-related security:
%
% The shell user has to be trusted, as they can at least exhaust the local
% resources (e.g. with <<0::size(99999999999999)>>"), or forge binaries that,
% once decoded with binary_to_term/1 as MFA and applied, can executed arbitrary
% code.


% Filtering local/remote calls in commands
%
% This is useful to implement shell built-in commands (e.g. list_bindings/1).
%
% We see two options:
%
% (A) using the prebuilt machinery for that in erl_eval (see
% https://www.erlang.org/doc/apps/stdlib/erl_eval.html#module-local-function-handler
% for instance)
%
% (B) managing the whole by ourselves at the AST level, using our meta
% facilities (see themyriad_parse_transform module as an example thereof)
%
% We start with (A).


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

	EmptyQueue = queue:new(),

	% Defaults:
	InitShellState = #custom_shell_state{
		cmd_history=EmptyQueue,
		res_history=EmptyQueue,
		bindings=erl_eval:new_bindings() },

	vet_options_for_custom( Opts, InitShellState );

vet_options_for_custom( Opt ) ->
	vet_options_for_custom( [ Opt ] ).



% (helper)
vet_options_for_custom( _Opts=[], ShellState=#custom_shell_state{
		callback_module=CallbackMod } ) ->

	cond_utils:if_defined( myriad_debug_shell,
		begin
			FunIds = meta_utils:list_exported_functions( CallbackMod ),
			trace_utils:debug_fmt( "The shell_utils callback module '~ts' "
				"exports the following ~B functions:~n ~p.",
				[ CallbackMod, length( FunIds ), FunIds ] )
		end ),

	% Done last, as needing at least the callback_module option to be ready.
	%
	% (we want to be able to fetch from an executed shell built-in command any
	% updated shell state and/or bindings)
	%
	% Returns {'value', Result, NewBindings}:
	LocalFunHandler = fun( FName, ASTArgs, Bndngs ) ->

		FArgCount = length( ASTArgs ),

		trace_utils:debug_fmt( "Local fun handler called for function ~ts, "
			"with the following ~B direct AST arguments: ~p, "
			"while bindings are:~n ~p.", [ FName, length( ASTArgs ), ASTArgs,
										   erl_eval:bindings( Bndngs ) ] ),

		% As the shell state will be the (first) argument of built-ins:
		FId = { FName, FArgCount+1 },

		case lists:member( FId, CallbackMod:list_builtin_commands() ) of

			true ->

				% All arguments were received in AST form, so we have to
				% evaluate each of them first.
				%
				% For example VarASTName={string,1,"B"} shall be translated in
				% "B".
				%
				% We do not think folding the binding structures would matter;
				% as expr/2 returns {value, Arg, _EvalBindingStruct}:
				%
				FArgs = [ element( _ArgIdx=2, erl_eval:expr( ASTArg, Bndngs ) )
							|| ASTArg <- ASTArgs ],

				% Fetching back from this handler the latest shell state, which
				% had been bound just before the call to erl_eval:expr/3:
				% (so 'unbound' not expected)
				%
				{ value, ShState } = erl_eval:binding(
					?shell_state_binding_name, Bndngs ),

				UpShState = ShState#custom_shell_state{ bindings=Bndngs },

				FullArgs = [ UpShState | FArgs ],

				trace_utils:debug_fmt(
					"Applying ~ts:~ts/~B, with following arguments:~n ~p.",
					[ CallbackMod, FName, length( FullArgs ), FullArgs ] ),

				% By convention:
				{ NewShState, Res } = apply( CallbackMod, FName, FullArgs ),

				%trace_utils:debug_fmt(
				%   "Built-in command ~ts/~B found; result: ~p.",
				%   [ FName, FArgCount, Res ] ),

				% Bindings have possibly been updated by the shell command:
				ResBndngs = NewShState#custom_shell_state.bindings,

				% We add back the shell state to these returned bindings:
				FinalBndngs = erl_eval:add_binding(
					?shell_state_binding_name, _Value=NewShState, ResBndngs ),

				{ value, Res, FinalBndngs };

			% No other local function is legit:
			false ->
				cond_utils:if_defined( myriad_debug_shell,
					trace_utils:error_fmt(
						"No built-in shell command ~ts/~B found.",
						[ FName, FArgCount ] ) ),

				% Only way found to escape erl_eval:exprs/3 normal path:
				throw( { undef,  { FName, FArgCount } } )

		end

	end,

	ShellState#custom_shell_state{
		% Not 'value', as we need to operate on bindings:
		local_fun_handler={ eval, LocalFunHandler } };


vet_options_for_custom( _Opts=[ timestamp | T ], ShellState ) ->
	vet_options_for_custom( T,
		ShellState#custom_shell_state{ do_timestamp=true } );

vet_options_for_custom( _Opts=[ log | T ], ShellState ) ->

	DefaultLogFilename = text_utils:bin_format(
		"myriad-shell-for-~ts-on-~ts.log",
		[ text_utils:pid_to_filename( self() ),
		  time_utils:get_textual_timestamp_for_path() ] ),

	vet_options_for_custom( [ { log, DefaultLogFilename } | T ], ShellState );

vet_options_for_custom( _Opts=[ { log, AnyLogFilePath } | T ], ShellState ) ->
	BinLogFilePath = text_utils:ensure_binary( AnyLogFilePath ),
	file_utils:remove_file_if_existing( BinLogFilePath ),

	% Cannot be 'raw', as the writer will be the shell process, not the
	% caller one:
	%
	LogFile = file_utils:open( BinLogFilePath, _OpenOpts=[ write, exclusive ] ),

	trace_utils:debug_fmt( "Shell logs to be written in '~ts'.",
						   [ BinLogFilePath ] ),

	vet_options_for_custom( T, ShellState#custom_shell_state{
		log_path=BinLogFilePath, log_file=LogFile } );


vet_options_for_custom(
		_Opts=[ H={ histories, MaxCmdDepth, MaxResDepth } | T ],
		ShellState ) ->
	vet_options_for_custom( T,
		ShellState#custom_shell_state{
			cmd_history_max_depth=check_history_depth( MaxCmdDepth, H ),
			res_history_max_depth=check_history_depth( MaxResDepth, H ) } );

vet_options_for_custom( _Opts=[ no_histories | T ], ShellState ) ->
	vet_options_for_custom( T, ShellState#custom_shell_state{
		cmd_history_max_depth=0,
		res_history_max_depth=0 } );

vet_options_for_custom( _Opts=[ persistent_command_history | T ],
						ShellState ) ->

	HistPath = file_utils:join( system_utils:get_user_home_directory(),
								?persistant_command_history_filename ),

	% Intentionally no raw, exclusive, delayed_write; created in all cases:
	CmdHistFileOpts = [ write ],

	{ CmdHistQueue, CmdHistFile, InitSubCount } =
			case file_utils:is_existing_file_or_link( HistPath ) of

		true ->
			% As to be stored as binaries in shell's history:
			StoredCmds = text_utils:strings_to_binaries(
				file_utils:read_lines( HistPath ) ),

			InFileCount = length( StoredCmds ),

			trace_utils:debug_fmt( "Read ~B command(s) from persistent history "
				"in '~ts'.", [ InFileCount, HistPath ] ),

			% Feeding our history from it:
			SelectedCmds = case
					ShellState#custom_shell_state.cmd_history_max_depth of

				undefined ->
					% Unlimited, thus keeping all of them:
					StoredCmds;

				MaxDepth ->
					ToExtractCount = min( MaxDepth, InFileCount ),

					{ ExtractedCmds, _Rest } = list_utils:extract_last_elements(
						StoredCmds, ToExtractCount ),

					ExtractedCmds

			end,

			CmdHistQ = queue:from_list( SelectedCmds ),

			% Now we truncate if needed the history file, to avoid that it grows
			% indefinitely:

			ExcessCount = InFileCount - ?persistant_command_history_depth,

			CmdHFile = case ExcessCount > 0 of

				true ->
					cond_utils:if_defined( myriad_debug_shell,
						trace_utils:debug_fmt(
							"Truncating '~ts' (~B lines in excess).",
							[ HistPath, ExcessCount ] ) ),

					% Cheaper than a extract_last_elements/2 call:
					{ _PastExcessCmds, ToKeepCmds } =
						list_utils:extract_first_elements( StoredCmds,
														   ExcessCount ),

					% Would have no newlines:
					%file_utils:write_whole( HistPath, ToKeepCmds )

					CmdFile = file_utils:open( HistPath, CmdHistFileOpts ),

					[ file_utils:write_ustring( CmdFile, "~ts~n",
												[ CmdBinStr ] )
								|| CmdBinStr <- ToKeepCmds ],

					CmdFile;

				false ->
					cond_utils:if_defined( myriad_debug_shell,
						trace_utils:debug_fmt(
							"Not needing to truncate '~ts'.", [ HistPath ] ) ),

					file_utils:open( HistPath, [ append ] )

			end,

			{ CmdHistQ, CmdHFile, length( SelectedCmds ) };

		false ->
			{ queue:new(), file_utils:open( HistPath, CmdHistFileOpts ), 0 }

	end,

	vet_options_for_custom( T, ShellState#custom_shell_state{
									submission_count=InitSubCount,
									cmd_history=CmdHistQueue,
									cmd_history_file=CmdHistFile } );

vet_options_for_custom( _Opts=[ { callback_module, CallbackModule } | T ],
						ShellState ) ->

	is_atom( CallbackModule ) orelse
		throw( { non_atom_callback_module, CallbackModule } ),

	code_utils:is_beam_in_path( CallbackModule ) =/= not_found orelse
		begin
			trace_utils:error_fmt( "The shell_utils callback module '~ts' "
				"could not be found in the code path, made of (alphabetically) "
				"of: ~ts",
				[ CallbackModule, code_utils:code_path_to_string() ] ),

			throw( { shell_callback_module_not_found, CallbackModule } )

		end,

	vet_options_for_custom( T,
		ShellState#custom_shell_state{ callback_module=CallbackModule } );

vet_options_for_custom( _Opts=[ Other | _T ], _ShellState ) ->
	throw( { unexpected_shell_option, Other } ).



% (helper)
% (Histories just for a more proper error message)
check_history_depth( _MaxDepth=undefined, _Histories ) ->
	undefined;

check_history_depth( MaxDepth, _Histories )
		when is_integer( MaxDepth ) andalso MaxDepth >= 0 ->
	MaxDepth;

check_history_depth( InvMaxDepth, Histories ) ->
	throw( { invalid_history_depth, InvMaxDepth, Histories } ).



%% -doc """
%% Executes the specified command on the specified shell, and returns its result.

%% Throws an exception on error.
%% """.
%% -spec execute_command( any_string(), custom_shell_pid() ) -> command_outcome().
%% execute_command( CmdAnyStr, ShellPid ) ->
%%	CmdBinStr = text_utils:ensure_binary( CmdAnyStr ),
%%	ShellPid ! { processEntry, CmdBinStr, self() },

%%	% Blocking, so no ShellPid needs to be pattern-matched to correlate answers:
%%	receive

%%		CmdOutcome={ success, _CmdResValue, _CmdId, _MaybeTimestampBinStr } ->
%%			%CmdResValue;
%%			CmdOutcome;

%%		CmdOutcome={ error, ErrorBinStr, _CmdId, _MaybeTimestampBinStr }  ->

%%			cond_utils:if_defined( myriad_debug_shell,
%%				trace_utils:error_fmt( "Failed to execute command '~ts' "
%%					"on custom shell ~w: ~ts",
%%					[ CmdAnyStr, ShellPid, ErrorBinStr ] ),
%%				basic_utils:ignore_unused( ErrorBinStr ) ),

%%			%throw( { shell_command_failed, ErrorBinStr, ShellPid, CmdAnyStr } )
%%			CmdOutcome;

%%		CmdOutcome={ update_prompt, _NewPrompt } ->
%%			CmdOutcome

%%	end.



% Implementation helpers.


-doc "Main loop of a custom shell instance.".
% No specific initialisation needed, like 'process_flag(trap_exit, true)'.
-spec custom_shell_main_loop( custom_shell_state() ) -> no_return().
custom_shell_main_loop( ShellState ) ->

	%cond_utils:if_defined( myriad_debug_shell, trace_utils:debug_fmt(
	%   "Now being ~ts", [ custom_shell_state_to_string( ShellState ) ] ) ),

	% To test commands with proper runtime information:
	%trace_utils:debug_fmt( "Shell main loop: ~ts.",
	%	[ command_history_to_string_with_ids( ShellState ) ] ),


	% WOOPER-like conventions, except that no wooper_result is sent back:
	receive

		% Using 'processEntry' rather than for example 'processCommand' to
		% comply with the more generic text_edit interface:
		%
		{ processEntry, CmdBinStr, ClientPid } ->

			{ CmdOutcome, ProcShellState } =
				process_command_custom( CmdBinStr, ShellState ),

			% A failed command does not kill the shell:
			ClientPid ! CmdOutcome,

			custom_shell_main_loop( ProcShellState );


		% Mostly useless:
		{ getMaybeLastEntry, [], CallerPid } ->

			MaybeBinCmd = case queue:peek(
					ShellState#custom_shell_state.cmd_history ) of

				empty ->
					undefined;

				{ value, BinCmd } ->
					BinCmd

			end,

			CallerPid ! { last_entry, MaybeBinCmd },

			custom_shell_main_loop( ShellState );


		{ getMaybeEntryFromId, TargetCmdId, CallerPid } ->

			% For example [Cmd1, Cmd2, Cmd3]:
			CmdHistList = queue:to_list(
				ShellState#custom_shell_state.cmd_history ),

			CmdHistLen = length( CmdHistList ),

			LastId = ShellState#custom_shell_state.submission_count,

			CmdIdOffset = LastId - TargetCmdId + 1,

			%trace_utils:debug_fmt( "Command ids: target=~B, last=~B, "
			%   "offset=~B, hist_len=~B.",
			%   [ TargetCmdId, LastId, CmdIdOffset, CmdHistLen ] ),

			MaybeBinCmd = case CmdIdOffset > CmdHistLen of

				true ->
					undefined;

				false ->
					ListOffset = CmdHistLen - CmdIdOffset + 1,
					lists:nth( ListOffset, CmdHistList )

			end,

			CallerPid ! { target_entry, MaybeBinCmd },

			custom_shell_main_loop( ShellState );


		flushCommandHistory ->
			custom_shell_main_loop( ShellState#custom_shell_state{
				cmd_history=queue:new() } );

		flushResultHistory ->
			custom_shell_main_loop( ShellState#custom_shell_state{
				res_history=queue:new() } );


		% Returns the number of already recorded entries; to be understood in
		% this context as getCommandSubmissionCount/0:
		%
		{ getEntryCount, [], CallerPid } ->
			Count = ShellState#custom_shell_state.submission_count,
			CallerPid ! { entry_count, Count },
			custom_shell_main_loop( ShellState );


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
				"by Myriad custom shell ~w:~n ~p", [ self(), UnexpectedMsg ] ),

			custom_shell_main_loop( ShellState )

	end.



% (helper)
-spec on_prompt_update( command(), binding_struct(), custom_shell_state() ) ->
							{ command_outcome(), custom_shell_state() }.
on_prompt_update( NewPrompt, NewBindings,
		ShellState=#custom_shell_state{ submission_count=SubCount } ) ->

	% Command identifier was incremented, as a command was processed, yet a
	% prompt update does not result directly in an actual being processed:
	%
	CorrectedSubCount = SubCount - 1,

	ProcShellState = ShellState#custom_shell_state{
						submission_count=CorrectedSubCount,
						bindings=NewBindings },

	CmdOutcome = { entry_update, NewPrompt },

	{ CmdOutcome,  ProcShellState }.


% (helper)
-spec on_command_success( command(), command_result(), command_id(),
						  binding_struct(), custom_shell_state() ) ->
								{ command_outcome(), custom_shell_state() }.
on_command_success( CmdBinStr, CmdResValue, CmdId, NewBindings,
					ShellState=#custom_shell_state{
						submission_count=SubCount } ) ->

	% submission_count already incremented:
	ProcShellState = ShellState#custom_shell_state{ bindings=NewBindings },

	% Only updated on success:
	ResHistShellState = update_result_history( CmdResValue, ProcShellState ),

	MaybeTimestampBinStr =
		manage_success_log( CmdBinStr, CmdResValue, CmdId, ResHistShellState ),

	CmdOutcome = { processing_success, CmdResValue, _CmdId=SubCount+1,
				   MaybeTimestampBinStr },

	{ CmdOutcome, ResHistShellState }.


% Custom Shell commands.


-doc """
Have this custom shell process the specified command and return its outcome.
""".
-spec process_command_custom( command(), custom_shell_state() ) ->
								{ command_outcome(), custom_shell_state() }.
process_command_custom( CmdBinStr, ShellState=#custom_shell_state{
											submission_count=SubCount,
											cmd_history_file=MaybeCmdHistFile,
											bindings=Bindings } ) ->

	cond_utils:if_defined( myriad_debug_shell, trace_utils:debug_fmt(
		"Processing command '~ts'.", [ CmdBinStr ] ) ),

	NewCmdId = SubCount + 1,

	BaseShellState = ShellState#custom_shell_state{ submission_count=NewCmdId },

	% CmdBinStr recorded later so that print_command_history() will not list its
	% own call.

	% The size/depth of persistent command history is (only) managed at shell
	% startup:
	%
	MaybeCmdHistFile =:= undefined orelse
		file_utils:write_ustring( MaybeCmdHistFile, "~ts~n", [ CmdBinStr ] ),

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

					LocalFunHandler =
						BaseShellState#custom_shell_state.local_fun_handler,

					ExecBindings = erl_eval:add_binding(
						?shell_state_binding_name, _Value=BaseShellState,
						Bindings ),

					% Currently not using non-local function handlers:
					try erl_eval:exprs( ExprForms, ExecBindings,
										LocalFunHandler ) of

						{ value, _CmdRes={ update_command_prompt, NewPrompt },
						  UpdatedBindings } ->

							% Reading back the (possibly) updated shell state;
							% not expecting 'unbound':
							%
							{ value, ResShellState } = erl_eval:binding(
								?shell_state_binding_name, UpdatedBindings ),

							% Would not fail if not present:
							ResetBindings = erl_eval:del_binding(
								?shell_state_binding_name, UpdatedBindings ),

							% Will assign these bindings in state:
							on_prompt_update( NewPrompt, ResetBindings,
											  ResShellState );


						{ value, CmdRes, UpdatedBindings } ->

							% Reading back the (possibly) updated shell state;
							% not expecting 'unbound':
							%
							{ value, ResShellState } = erl_eval:binding(
								?shell_state_binding_name, UpdatedBindings ),

							CmdHistShellState = update_command_history(
								CmdBinStr, ResShellState ),

							% Would not fail if not present:
							ResetBindings = erl_eval:del_binding(
								?shell_state_binding_name, UpdatedBindings ),

							% Will assign these bindings in state:
							on_command_success( CmdBinStr, CmdRes, NewCmdId,
								ResetBindings, CmdHistShellState )

					catch Class:Reason ->

						cond_utils:if_defined( myriad_debug_shell,
							trace_utils:warning_fmt( "Evaluation error "
								"for command '~ts' by shell ~w:~n~p "
								"(class: ~ts)",
								[ CmdBinStr, self(), Reason, Class ] ),
							basic_utils:ignore_unused( Class ) ),

						ReasonBinStr = format_error( Reason ),

						MaybeTimestampBinStr = manage_error_log( CmdBinStr,
							ReasonBinStr, NewCmdId, BaseShellState ),

						CmdOutcome = { processing_error, ReasonBinStr,
									   NewCmdId+1, MaybeTimestampBinStr },

						% We record in the history of this shell a command in
						% all cases (even its syntax is wrong), so that it can
						% be edited/fixed afterwards:
						%
						CmdHistShellState = update_command_history( CmdBinStr,
							BaseShellState ),

						{ CmdOutcome, CmdHistShellState }

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

					ReasonBinStr = format_error_message(
						"parsing failed: ~ts", [ IssueDesc ] ),

					MaybeTimestampBinStr = manage_error_log( CmdBinStr,
						ReasonBinStr, NewCmdId, BaseShellState ),

					CmdOutcome = { processing_error, ReasonBinStr, NewCmdId+1,
								   MaybeTimestampBinStr },

					CmdHistShellState = update_command_history( CmdBinStr,
						BaseShellState ),

					{ CmdOutcome, CmdHistShellState }

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

			ReasonBinStr = format_error_message( "scanning failed: ~ts",
												 [ IssueDesc ] ),

			MaybeTimestampBinStr = manage_error_log( CmdBinStr, ReasonBinStr,
				NewCmdId, BaseShellState ),

			CmdOutcome = { processing_error, ReasonBinStr, NewCmdId,
						   MaybeTimestampBinStr },

			CmdHistShellState = update_command_history( CmdBinStr,
														BaseShellState ),

			{ CmdOutcome, CmdHistShellState }

	end.



% (helper)
-spec manage_success_log( command(), command_result(), command_id(),
			custom_shell_state() ) -> option( timestamp_binstring() ).
manage_success_log( _CmdBinStr, _CmdResValue, _CmdId,
					#custom_shell_state{ do_timestamp=true,
										 log_file=undefined } ) ->
	time_utils:get_bin_textual_timestamp();

manage_success_log( _CmdBinStr, _CmdResValue, _CmdId,
					#custom_shell_state{ do_timestamp=false,
										 log_file=undefined } ) ->
	undefined;

manage_success_log( CmdBinStr, CmdResValue, CmdId,
					#custom_shell_state{ do_timestamp=true,
										 log_file=LogFile } ) ->
	TimestampBinStr = time_utils:get_bin_textual_timestamp(),

	file_utils:write_ustring( LogFile,
		"[~ts] Command #~B: '~ts' -> ~p~n",
		[ TimestampBinStr, CmdId, CmdBinStr, CmdResValue ] ),

	TimestampBinStr;

manage_success_log( CmdBinStr, CmdResValue, CmdId,
					#custom_shell_state{ do_timestamp=false,
										 log_file=LogFile } ) ->
	file_utils:write_ustring( LogFile,
		"Command #~B '~ts' -> ~p~n", [ CmdId, CmdBinStr, CmdResValue ] ),

	undefined.




% (helper)
-spec manage_error_log( command(), command_error(), command_id(),
			custom_shell_state() ) -> option( timestamp_binstring() ).
manage_error_log( _CmdBinStr, _ReasonBinStr, _CmdId,
				  #custom_shell_state{ do_timestamp=true,
									   log_file=undefined } ) ->
	time_utils:get_bin_textual_timestamp();

manage_error_log( _CmdBinStr, _ReasonBinStr, _CmdId,
				  #custom_shell_state{ do_timestamp=false,
									   log_file=undefined } ) ->
	undefined;

manage_error_log( CmdBinStr, ReasonBinStr, CmdId,
				  #custom_shell_state{ do_timestamp=true,
									   log_file=LogFile } ) ->
	TimestampBinStr = time_utils:get_bin_textual_timestamp(),

	file_utils:write_ustring( LogFile,
		"[~ts] Evaluation failed for command #~B '~ts': ~ts.~n",
		[ TimestampBinStr, CmdId, CmdBinStr, ReasonBinStr ] ),

	TimestampBinStr;

manage_error_log( CmdBinStr, ReasonBinStr, CmdId,
				  #custom_shell_state{ do_timestamp=false,
									   log_file=LogFile } ) ->
	file_utils:write_ustring( LogFile,
		"Evaluation failed for command #~B '~ts': ~ts.~n",
		[ CmdId, CmdBinStr, ReasonBinStr ] ),

	undefined.




-doc "Updates the command history.".
-spec update_command_history( command(), custom_shell_state() ) ->
											custom_shell_state().
update_command_history( _Cmd, ShellState=#custom_shell_state{
									cmd_history_max_depth=0 } ) ->
	ShellState;

update_command_history( Cmd, ShellState=#custom_shell_state{
									cmd_history_max_depth=undefined,
									cmd_history=CmdHistQ } ) ->

	% No length limit:
	NewCmdHistQ = queue:in( Cmd, CmdHistQ ),

	ShellState#custom_shell_state{ cmd_history= NewCmdHistQ };


update_command_history( Cmd, ShellState=#custom_shell_state{
									cmd_history_max_depth=HDepth,
									cmd_history=CmdHistQ } ) ->
	DropCmdHistQ = case queue:len( CmdHistQ ) of

		HDepth ->
			% Full, thus dropping first (never expected to be empty):
			% { { _ValueAtom, _FirstHItem }, ShrunkCmdHistQ } =
			%  queue:out( CmdHistQ ),
			%ShrunkCmdHistQ;
			queue:drop( CmdHistQ );

		_ ->
			% Not full yet:
			CmdHistQ

	end,

	NewCmdHistQ = queue:in( Cmd, DropCmdHistQ ),

	ShellState#custom_shell_state{ cmd_history=NewCmdHistQ }.



-doc "Updates the result history.".
update_result_history( _Res, ShellState=#custom_shell_state{
									res_history_max_depth=0 } ) ->
	ShellState;

update_result_history( Res, ShellState=#custom_shell_state{
									res_history_max_depth=undefined,
									res_history=ResHistQ } ) ->

	% No length limit:
	NewResHistQ = queue:in( Res, ResHistQ ),

	ShellState#custom_shell_state{ res_history= NewResHistQ};


update_result_history( Res, ShellState=#custom_shell_state{
									res_history_max_depth=HDepth,
									res_history=ResHistQ } ) ->
	DropResHistQ = case queue:len( ResHistQ ) of

		HDepth ->
			% Full, thus dropping first (never expected to be empty):
			%{ { _ValueAtom, _FirstHItem }, ShrunkResHistQ } =
			%  queue:out( ResHistQ ),
			%ShrunkResHistQ;
			queue:drop( ResHistQ );

		_ ->
			% Not full yet:
			ResHistQ

	end,

	NewResHistQ = queue:in( Res, DropResHistQ ),

	ShellState#custom_shell_state{ res_history=NewResHistQ }.





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

		{ processEntry, CmdBinStr, ClientPid } ->

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



% Helpers:


-spec format_error( eval_error_term() ) -> bin_string().
format_error( E ) ->
	format_error_message( get_error_message( E ) ).


-spec get_error_message( eval_error_term() ) -> ustring().
get_error_message( { unbound, Var } ) ->
	text_utils:format( "variable '~ts' is not bound", [ Var ] );

get_error_message( { badmatch, Value } ) ->
	text_utils:format( "bad match, right-hand side value is actually: ~p",
					   [ Value ] );

% At least our version of undef:
get_error_message( { undef, { FunctionName, FunArity } } ) ->
	text_utils:format( "function ~ts/~B is not defined",
					   [ FunctionName, FunArity ] );

get_error_message( { invalid_variable_name, VarName } ) ->
	text_utils:format( "variable name '~p' is invalid (not a plain string)",
					   [ VarName ] );

get_error_message( Other ) ->
	text_utils:format( "~p", [ Other ] ).



-spec format_error_message( ustring() ) -> bin_string().
format_error_message( Msg ) ->
	text_utils:bin_format( "*** Error: ~ts.", [ Msg ] ).


-spec format_error_message( format_string(), format_values() ) -> bin_string().
format_error_message( Format, Values ) ->
	format_error_message( text_utils:format( Format, Values ) ).



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
								cmd_history_max_depth=CmdHistMaxDepth,
								res_history_max_depth=ResHistMaxDepth,
								cmd_history=CmdHistory,
								res_history=ResHistory,
								bindings=BindingStruct,
								callback_module=CallbackMod },
							  _Verbose=true ) ->
	CmdHistStr = case CmdHistMaxDepth of

		0 ->
			"no command history";

		undefined ->

			text_utils:format( "an unlimited  ~ts",
							   [ command_history_to_string( CmdHistory ) ] );

		_ ->
			text_utils:format( "a ~B-deep ~ts",
				[ CmdHistMaxDepth, command_history_to_string( CmdHistory ) ] )

	end,

	ResHistStr = case ResHistMaxDepth of

		0 ->
			"no result history";

		undefined ->

			text_utils:format( "an unlimited  ~ts",
							   [ result_history_to_string( ResHistory ) ] );

		_ ->
			text_utils:format( "a ~B-deep ~ts",
				[ ResHistMaxDepth, result_history_to_string( ResHistory ) ] )

	end,

	text_utils:format( "custom shell ~w, relying on the ~ts callback module, "
		"with ~ts and ~B commands already submitted, with ~ts and ~ts",
		[ self(), CallbackMod, bindings_to_string( BindingStruct ), SubCount,
		  CmdHistStr, ResHistStr ] );


custom_shell_state_to_string( #custom_shell_state{
								submission_count=SubCount,
								bindings=BindingStruct },
							  _Verbose=false ) ->
	text_utils:format( "custom shell with ~B bindings, and ~B commands already "
		"submitted",
		[ length( erl_eval:bindings( BindingStruct ) ), SubCount ] ).




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





% Section for helpers directly called by shell callbacks:


-doc """
Returns a textual description of the command history from the specified shell
state, with command identifiers specified (useful to repeat commands).
""".
-spec command_history_to_string_with_ids( custom_shell_state() ) -> ustring().
command_history_to_string_with_ids( #custom_shell_state{
										submission_count=SubCount,
										cmd_history_max_depth=CmdHMaxD,
										cmd_history=CmdQ } ) ->

	case queue:len( CmdQ ) of

		0 ->
			"Empty command history";

		1 ->
			Cmd = queue:get( CmdQ ),
			text_utils:format(
				"History of a single command (out of up to ~B): #~B was '~ts'.",
				[ CmdHMaxD, SubCount, Cmd ] );

		CmdCount ->
			Ids = lists:seq( SubCount - CmdCount, SubCount - 1 ),
			Cmds = queue:to_list( CmdQ ),

			%trace_utils:debug_fmt( "Ids = ~p, Cmds = ~p.",
			%                       [ Ids, Cmds ] ),

			IdCmds = lists:zip( Ids, Cmds ),

			Strs = [ text_utils:format( "command #~B was: '~ts'", [ Id, Cmd ] )
						|| { Id, Cmd } <- IdCmds ],

			text_utils:format( "History of ~B (out of up to ~B) commands: ~ts",
				[ CmdCount, CmdHMaxD, text_utils:strings_to_string( Strs ) ] )

	end.


-doc """
Returns a textual description of the result history from the specified shell
state, with result identifiers specified (useful to featch past results).
""".
-spec result_history_to_string_with_ids( custom_shell_state() ) -> ustring().
result_history_to_string_with_ids( #custom_shell_state{
										submission_count=SubCount,
										res_history_max_depth=ResHMaxD,
										res_history=ResQ } ) ->

	trace_utils:debug_fmt( "SubCount = ~p, ResHMaxD = ~p, ResQ = ~p",
						   [ SubCount, ResHMaxD, ResQ ] ),

	case queue:len( ResQ ) of

		0 ->
			"Empty result history";

		1 ->
			Res = queue:get( ResQ ),
			text_utils:format(
				"History of a single result (out of ~B): #~B -> '~ts'.",
				[ SubCount, ResHMaxD, Res ] );

		ResCount ->
			Ids = lists:seq( SubCount - ResCount, SubCount-1 ),
			Ress = queue:to_list( ResQ ),

			%trace_utils:debug_fmt( "Ids = ~p, Ress = ~p.",
			%                       [ Ids, Ress ] ),

			IdRess = lists:zip( Ids, Ress ),

			Strs = [ text_utils:format( "result #~B: ~p", [ Id, Res ] )
						|| { Id, Res } <- IdRess ],

			text_utils:format( "History of ~B (out of ~B) results: ~ts",
				[ ResCount, ResHMaxD, text_utils:strings_to_string( Strs ) ] )

	end.



-doc """
Returns the command of the specified identifier (if it is still in command
history), so that it can be evaluated again.
""".
-spec recall_command( custom_shell_state(), command_id() ) ->
		message() | { 'update_command_prompt', command_str() }.
recall_command( _ShellState=#custom_shell_state{ submission_count=SubCount,
						cmd_history=CmdQ }, CmdId ) when CmdId < SubCount ->

	QLen = queue:len( CmdQ ),

	% Index in the list corresponding to that queue:
	Index = QLen - SubCount + CmdId + 1,

	case Index < 1 of

		true ->
			text_utils:format( "Command #~B is not in history anymore.",
							   [ CmdId ] );

		false ->
			% Thus Index >=1; Index <= QLen as SubCount >= CmdId, so in range:
			CmdStr = lists:nth( Index, queue:to_list( CmdQ ) ),
			%text_utils:format( "Command #~B was: '~ts'.", [ CmdId, CmdStr ] )
			{ update_command_prompt, CmdStr }

	end;

% Here CmdId > SubCount:
recall_command( #custom_shell_state{
		submission_count=SubCount }, CmdId ) ->
	text_utils:format( "Command #~B would not be prior to current one (#~B).",
					   [ CmdId, SubCount ] ).




% Section for various other helpers:

-doc "Returns a textual description of the specified command history.".
-spec command_history_to_string( command_history() ) -> ustring().
command_history_to_string( History ) ->

	case queue:len( History ) of

		0 ->
			"empty command history";

		_ ->
			Strs = [ text_utils:format( "command '~ts'", [ HE ] )
						|| HE <- queue:to_list( History ) ],

			text_utils:format( "command history corresponding to: ~ts",
				[ text_utils:strings_to_enumerated_string( Strs ) ] )

	end.






-doc "Returns a textual description of the specified result history.".
-spec result_history_to_string( result_history() ) -> ustring().
result_history_to_string( History ) ->

	case queue:len( History ) of

		0 ->
			"empty result history";

		_ ->
			Strs = [ text_utils:format_ellipsed( "result ~p", [ HE ] )
						|| HE <- queue:to_list( History ) ],

			text_utils:format( "result history corresponding to: ~ts",
				[ text_utils:strings_to_enumerated_string( Strs ) ] )

	end.





% Binding-related section.


-doc """
Filters the specified binding structure, typically on behalf of shell commands,
so that they access only to the legit bindings.
""".
-spec filter_bindings( binding_struct() ) -> [ binding() ].
filter_bindings( BindingStruct ) ->
	[ P || P={N,_V} <- erl_eval:bindings( BindingStruct ),
		   N =/= ?shell_state_binding_name ].




-doc "Returns a textual description of the full specified binding structure.".
-spec bindings_to_string( binding_struct() ) -> ustring().
bindings_to_string( BindingStruct ) ->
	case erl_eval:bindings( BindingStruct ) of

		[] ->
			"no binding";

		[ Binding ] ->
			text_utils:format( "a single binding: ~ts",
							   [ binding_to_string( Binding ) ] );

		Bindings ->
			text_utils:format( "~B bindings: ~ts",
				[ length( Bindings ),
				  text_utils:strings_to_string( [ binding_to_string( B )
						|| B <- lists:sort( Bindings ) ] ) ] )

	end.



-doc """
Returns a textual command-level description of the specified, filtered, binding
structure.
""".
-spec bindings_to_command_string( binding_struct() ) -> ustring().
bindings_to_command_string( BindingStruct ) ->

	case filter_bindings( BindingStruct ) of

		[] ->
			"no binding defined";

		[ Binding ] ->
			text_utils:format( "a single binding defined: ~ts",
							   [ binding_to_string( Binding ) ] );

		Bindings ->
			text_utils:format( "~B bindings defined: ~ts",
				[ length( Bindings ),
				  text_utils:strings_to_string( [ binding_to_string( B )
						|| B <- lists:sort( Bindings ) ] ) ] )

	end.



-doc "Returns a textual description of the specified bindings.".
-spec binding_to_string( binding() ) -> ustring().
binding_to_string( _Binding={ N, V } ) ->
	text_utils:format( "variable '~ts' has for value ~p", [ N, V ] ).



% Section for helper functions that may be used by all kinds of shell
% interfaces, for a better centralisation thereof.
