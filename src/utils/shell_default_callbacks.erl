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
% Creation date: Saturday, November 2, 2024.

-module(shell_default_callbacks).

-moduledoc """
This module is the default implementation module offered by Myriad for the
callbacks of its custom shell (defined in the shell_utils module).

Defined separately from shell_utils for clarity and to showcase an actual
uncoupling.

The current module provides the default shell built-in functions.

It may be called in turn by any user-supplied shell callback module, for default
implementations.
""".


% Facilities at the level of this callback module:
-export([ list_builtin_commands/0 ]).



% Shell built-in commands:
%
% (if updating this export, update list_builtin_commands/0 accordingly)
%
-export([ list_bindings/1, print_bindings/1,
		  clear_bindings/1, clear_binding/2,
		  print_command_history/1, print_result_history/1,
		  repeat_command/2, clear_commands/1, clear_results/1,
		  set_command_history_depth/2, set_result_history_depth/2 ]).





% Type shorthands:

-type count() :: basic_utils:count().

-type shell_state() :: shell_utils:custom_shell_state().

-type command_id() :: shell_utils:command_id().

-type command_result() :: shell_utils:command_result().


-type binding() :: shell_utils:binding().
-type variable_name() :: shell_utils:variable_name().

-type state_only() :: { shell_state(), 'void' }.

-type function_id() :: meta_utils:function_id().



% Implementation notes:
%
% A shell built-in command is in the following form: `f(ShellState ::
% shell_state(), Param1, Param2, ...  ) -> {shell_state(), Result}` where Result
% (possibly 'void' if none applies) is returned to the caller (the updated shell
% state is just extracted and reused internally, transparently).


-doc """
Lists the name and arity for each of the shell built-in commands supported by
this callback module.
""".
-spec list_builtin_commands() -> [ function_id() ].
list_builtin_commands() ->
	% See the corresponding export:
	[ { list_bindings, 1 }, { print_bindings, 1 },
	  { clear_bindings, 1 }, { clear_binding, 2 },
	  { print_command_history, 1 }, { print_result_history, 1 },
	  { repeat_command, 2 },
	  { clear_commands, 1 }, { clear_results, 1 },
	  { set_command_history_depth, 2 }, { set_result_history_depth, 2 } ].



% Implementation of the shell built-in commands:


-doc "Lists (as terms) the current variable bindings.".
-spec list_bindings( shell_state() ) -> { shell_state(), [ binding() ] }.
list_bindings( ShellState ) ->
	trace_utils:debug_fmt( "Listing bindings." ),
	{ ShellState, [] }.


-doc "Prints on the console the current variable bindings.".
-spec print_bindings( shell_state() ) -> state_only().
print_bindings( ShellState ) ->
	{ ShellState, void }.



-doc "Clears all variable bindings.".
-spec clear_bindings( shell_state() ) -> state_only().
clear_bindings( ShellState ) ->
	{ ShellState, void }.


-doc "Clears the binding of the specified variable.".
-spec clear_binding( shell_state(), variable_name() ) -> state_only().
clear_binding( ShellState, _VarName ) ->
	{ ShellState, void }.



-doc "Displays the current history of commands.".
-spec print_command_history( shell_state() ) -> state_only().
print_command_history( ShellState ) ->
	{ ShellState, void }.


-doc "Displays the current history of results.".
-spec print_result_history( shell_state() ) -> state_only().
print_result_history( ShellState ) ->
	{ ShellState, void }.



-doc """
Re-evaluates the command of the specified identifier (if it is still in command
history).
""".
-spec repeat_command( shell_state(), command_id() ) ->
								{ shell_state(), command_result() }.
repeat_command( ShellState, CmdId ) ->
	{ ShellState, CmdId }.


-doc "Clears the full history of commands.".
-spec clear_commands( shell_state() ) -> state_only().
clear_commands( ShellState ) ->
	{ ShellState, void }.


-doc "Clears the full history of command results.".
-spec clear_results( shell_state() ) -> state_only().
clear_results( ShellState ) ->
	{ ShellState, void }.


-doc "Sets the depth of the command history.".
-spec set_command_history_depth( shell_state(), count() ) -> state_only().
set_command_history_depth( ShellState, _NewDepth ) ->
	{ ShellState, void }.


-doc "Sets the depth of the result history.".
-spec set_result_history_depth( shell_state(), count() ) -> state_only().
set_result_history_depth( ShellState, _NewDepth ) ->
	{ ShellState, void }.
