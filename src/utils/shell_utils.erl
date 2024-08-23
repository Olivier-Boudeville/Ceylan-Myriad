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



-export([ start_shell/0, start_integrated_shell/0 ]).


% To enable spawns of them:
-export([ start_integrated_helper/0 ]).



-doc "The PID of a (Myriad) shell process.".
-type shell_pid() :: pid().


-doc "The PID of a shell client process.".
-type client_pid() :: pid().


-doc "A command to submit to a shell.".
-type command() :: bin_string().

-doc "The outcome of a command submitted to a shell.".
-type result() :: bin_string().


-export_type([ shell_pid/0, client_pid/0, command/0, result/0 ]).



% Local type:

-record( shell_state, {

	} ).



-doc "The state of a shell instance.".
-type shell_state() :: #shell_state{}.



% Type shorthands:

-type bin_string() :: text_utils:bin_string().




% Implementation notes:
%
% Perhaps that a smart use of the built-in 'shell' module could have sufficed.
%
% We see two approaches in order to implement such a separate shell:
%
% - (A) define our own version of it, from scratch, based on our own
% read/scan/eval loop
%
% - (B) plug in the group/user/shell architecture ("integrated shell")
%
%
%
% See also:
%
% - https://erlangforums.com/t/adding-repl-like-feature-to-a-graphical-erlang-application/3795
%
% - for approach B:
% https://erlang.org/pipermail/erlang-questions/2008-September/038476.html


% For myriad_spawn_link/1:
-include("spawn_utils.hrl").




% Section for our own shell (approach A).


-doc """
Starts a shell process according to approach A, returns its PID.
""".
-spec start_shell() -> shell_pid().
start_shell() ->

	ShellPid = ?myriad_spawn_link(
		fun() ->
			shell_main_loop()
		end ),

	cond_utils:if_defined( myriad_debug_shell,
		trace_utils:debug_fmt( "Started shell ~w.", [ ShellPid ] ) ),

	ShellPid.



-doc "Main loop of a shell instance (approach A).".
-spec shell_main_loop() -> no_return().
shell_main_loop() ->

	% WOOPER-like conventions:

	receive


		{ processCommand, CmdBinStr, ClientPid } ->

			cond_utils:if_defined( myriad_debug_shell, trace_utils:debug_fmt(
				"Processing command '~ts' on behalf of client ~w.",
				[ CmdBinStr, ClientPid ] ) ),

			CmdStr = text_utils:binary_to_string( CmdBinStr ),

			case erl_scan:string( CmdStr ) of

				{ ok, Tokens, EndLocation } ->

					cond_utils:if_defined( myriad_debug_shell,
						trace_utils:debug_fmt( "Scanned tokens '~p' "
							"(end location: ~p)", [ Tokens, EndLocation ] ),

					case erl_parse:parse_exprs( Tokens ) of

						{ ok, [ Form ] } ->
							cond_utils:if_defined( myriad_debug_shell,
								trace_utils:debug_fmt(
									"Parsed following form: '~p'",
									[ Form ] ) );




|
				{ error, ErrorInfo, ErrorLocation } ->

	{value, Value, NewBindings} = erl_eval:expr(Form, Bindings),
	{ok, Value, NewBindings}.


		AnyMsg ->
			trace_utils:warning_fmt( "Shell received unexpected message: ~p.",
									 [ AnyMsg ] ),

			shell_main_loop()

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
