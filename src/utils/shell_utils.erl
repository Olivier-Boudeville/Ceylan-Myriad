% Copyright (C) 2020-2020 Olivier Boudeville
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


% Gathering of various convenient facilities regarding the management of the
% shells and command lines (ex: specified arguments).
%
% See shell_utils_test.erl for the corresponding test.
%
-module(shell_utils).



% Section for command-line facilities:
-export([ protect_from_shell/1 ]).



% The command-line is mostly managed like init:get_argument/1.


% To designate command-line arguments that are specified directly, not in the
% context of any specific command-line option.
%
% Note that such option-less arguments should thus come first on the
% command-line, otherwise they would be included in the list associated to the
% last processed option.
%
-define( no_option_key, '(none)' ).


% The name of a command-line option (ex: '-color', for an actual option that is
% "--color"); note the special value ?no_option_key that corresponds to
% option-less arguments.
%
% (a "flag", for the init standard module)
%
-type command_line_option() :: atom() | ?no_option_key.


% An unitary value specified in link to a command-line option:
-type command_line_value() :: text_utils:ustring().


% The command-line values specified after an occurrence of a given option (ex:
% ["blue", "red"]):
%
-type command_line_values() :: [ command_line_value() ].


% The association between a command-line option and the various values
% associated to its various occurrences.
%
% Ex: if arguments were "--color blue red [...] --color yellow", then the
% corresponding argument entry is { '-color', [ [ "blue", "red" ], [ "yellow" ]
% ] } (i.e. with, associated to a command-line option, a list whose elements are
% *lists* of strings; in their order on the command-line).
%
% Note that keys are atoms (with one leading dash removed), and it is advisable
% to use only the executable_utils module support rather than mixing and
% matching it with the one of the 'init' module (different keys).
%
-type command_line_argument() ::
		% Yes, a *list* of command-line valueS:
		{ command_line_option(), [ command_line_values() ] }.



% A table storing command-line user (plain, i.e arguments specified after either
% "--" or, preferably, "-extra") arguments conveniently (a bit like getopt), in
% a format exactly in the spirit of init:get_arguments/0, allowing to record
% repeated options, possibly each time specified with a series of values.
%
% Useful to manage arguments more easily, and also to handle uniformly the
% arguments specified for erl-based executions and escript ones alike.
%
-type argument_table() ::
		list_table:list_table( command_line_option(), [ command_line_values() ] ).


-export_type([ command_line_option/0, command_line_value/0,
			   command_line_argument/0, argument_table/0 ]).


% Command-line argument section:
-export([ get_argument_table/0,
		  get_argument_table_from_strings/1,
		  generate_argument_table/1,

		  get_command_arguments_for_option/1,
		  get_optionless_command_arguments/0,

		  extract_command_arguments_for_option/1,
		  extract_command_arguments_for_option/2,

		  extract_optionless_command_arguments/0,
		  extract_optionless_command_arguments/1,

		  argument_table_to_string/1 ]).



% Section for command-line facilities.


% Protects specified argument from shell parsing, typically if specifying a
% filename including a single quote.
%
-spec protect_from_shell( text_utils:ustring() ) -> text_utils:ustring().
protect_from_shell( ArgumentString ) ->

	% Not sufficient (ex: "echo 'aaa\'bbb'"):
	%text_utils:protect_from_shell( ArgumentString ).

	protect_from_shell_helper( ArgumentString, _Acc=[] ).


% (helper)
protect_from_shell_helper( _Text=[], Acc ) ->
	lists:reverse( Acc );

protect_from_shell_helper( _Text=[ $' | T ], Acc ) ->
	% As will be reversed:
	protect_from_shell_helper( T, "''\\'" ++ Acc );

protect_from_shell_helper( _Text=[ C | T ], Acc ) ->
	protect_from_shell_helper( T, [ C | Acc ] ).





% Command-line argument section.


% Returns a canonical argument table, obtained from the user command-line
% arguments supplied to the interpreter.
%
% Note:
%
% - only the arguments specified on the command-line after the '-extra' marker
% will be taken into account; ex:
%    make ui_run CMD_LINE_OPT="-a -extra some_value -b --use-ui-backend text_ui"
% (here "-a" and, of course, "-extra", will be ignored)
%
% - this function is to be called in the context of a standard erl execution (as
% opposed to an escript one, which shall use script_utils:get_arguments/1)
%
-spec get_argument_table() -> argument_table().
get_argument_table() ->

	% We do not want to include the VM-specific arguments (such as -noshell,
	% -pz, etc.); use, in the command-line, '-extra', before arguments to
	% consider as plain ones:
	%
	%Args = init:get_arguments(),
	Args = init:get_plain_arguments(),

	%trace_utils:debug_fmt( "Arguments obtained by get_argument_table/0: ~p.",
	%					   [ Args ] ),

	% To convert a list of strings into per-option list of values:
	get_argument_table_from_strings( Args ).



% Returns the specified command-line arguments (simply transmitted as a list of
% the corresponding strings) once transformed into our "canonical", more
% convenient form, which is quite similar to the one used by Erlang for its
% user/system flags (i.e. for all its non-plain options).
%
% In this form, options start with a dash, may have any number of arguments, and
% may be specified more than once in the command-line; non-option arguments are
% collected as well (refer to the no_option_key define).
%
% Note: switches to the Unicode encoding (ex: use "~tp" then).
%
-spec get_argument_table_from_strings( [ string() ] ) ->
											 argument_table().
get_argument_table_from_strings( ArgStrings ) ->

	%trace_utils:debug_fmt( "Creating argument table from: ~p.",
	%                       [ ArgStrings ] ),

	% Useful side-effect, difficult to troubleshoot:
	system_utils:force_unicode_support(),

	get_arguments_from_strings( ArgStrings, _OptionTable=list_table:new() ).



% (helper)
get_arguments_from_strings( _Args=[], OptionTable ) ->
	%trace_utils:debug_fmt( "Option table returned: ~p.", [ OptionTable ] ),
	OptionTable;

% The first option is detected, removing its initial dash:
get_arguments_from_strings( _Args=[ [ $- | Option ] | T ], OptionTable ) ->
	manage_option( Option, _RemainingArgs=T, OptionTable );

% Apparently can happen (ex: with releases run with erlexec):
get_arguments_from_strings( _Args=[ _Dropped="" | T ], OptionTable ) ->
	trace_utils:warning( "Dropping an empty argument." ),
	get_arguments_from_strings( T, OptionTable );

% Here an initial argument does not start with a dash, hence is collected as a
% non-option argument (unlike done by init:get_arguments/0):
%
get_arguments_from_strings( Args, OptionTable ) ->

	% This may happen in a legit manner if for example wanting to establish if
	% in batch mode (hence by calling is_batch/0) from a release, thus run with
	% erlexec [...] console [...]:
	%

	% Used to be dropped:
	%trace_utils:warning_fmt( "Dropping non-option initial argument '~s'.",
	%						 [ Dropped ] ),

	%code_utils:display_stacktrace(),
	%throw( { dropped, Dropped } ),

	% Now collected thanks to:
	manage_option( _Option=?no_option_key, Args, OptionTable ).



% (helper)
%
% (no_option_key being already an atom)
%
manage_option( OptionAtom, RemainingArgs, OptionTable )
  when is_atom( OptionAtom ) ->

	{ OptValues, NextOptionInfo } =
		collect_values_for_option( RemainingArgs, _AccValues=[] ),

	% This option may already be registered in the table:
	%
	% (like list_utils:append_to_entry/3 except values are added on the right,
	% thus in-order, rather than at the head)
	%
	Key = OptionAtom,
	NewOptionTable = case lists:keytake( Key, _N=1, OptionTable ) of

		{ value, { _Key, ListValue }, ShrunkTable } ->
			[ { Key, list_utils:append_at_end( OptValues, ListValue ) }
			  | ShrunkTable ];

		false ->
			[ { Key, [ OptValues ] } | OptionTable ]

	end,

	case NextOptionInfo of

		none ->
			NewOptionTable;

		{ NextOption, NextArgs } ->
			manage_option( NextOption, NextArgs, NewOptionTable )

	end;

% Normal options come as strings:
manage_option( Option, RemainingArgs, OptionTable ) ->
	OptionAtom = text_utils:string_to_atom( Option ),
	manage_option( OptionAtom, RemainingArgs, OptionTable ).



% (helper)
%
% All arguments processed here:
collect_values_for_option( _Args=[], AccValues ) ->
	{ lists:reverse( AccValues ), _NextOption=none };

% New option detected:
collect_values_for_option( _Args=[ [ $- | Option ] | T ], AccValues ) ->
	{ lists:reverse( AccValues ), _NextOption={ Option, T } };

% Still accumulating arguments for the current option:
collect_values_for_option( _Args=[ OptValue | T ], AccValues ) ->
	collect_values_for_option( T, [ OptValue | AccValues ] ).




% Returns a canonical argument table, obtained from the specified single string
% containing all options, verbatim ; ex: "--color red --set-foo".
%
% Note: useful for testing, to introduce specific command lines.
%
-spec generate_argument_table( string() ) -> argument_table().
generate_argument_table( ArgString ) ->

	CommandLineArgs = text_utils:split_per_element( ArgString,
													_Delimiters=[ $ ] ),

	get_argument_table_from_strings( CommandLineArgs ).



% Returns, if this option was specified on the command-line, the in-order list
% of the various (lists of) values (if any; no value at all being specified for
% an option resulting thus in [ [] ]) associated to the specified option; if
% this option was not specified on the command-line, returns 'undefined'.
%
% Note: generally the extract_command_arguments_for_option/{1,2} functions are more relevant
% to use.
%
-spec get_command_arguments_for_option( command_line_option() ) ->
								  maybe( [ command_line_values() ] ).
get_command_arguments_for_option( Option ) ->

	ArgumentTable = get_argument_table(),

	list_table:get_value_with_defaults( _K=Option, _DefaultValue=undefined,
										ArgumentTable ).



% Returns the in-order list of the arguments that were directly (i.e. not in the
% context of an option) specified on the command-line.
%
% Note: generally the extract_non_option_command_argument/{0,1} functions are
% more relevant to use.
%
-spec get_optionless_command_arguments() -> command_line_values().
get_optionless_command_arguments() ->

	ArgumentTable = get_argument_table(),

	% Not wanting here a list of lists of strings:
	[ Args ] = list_table:get_value_with_defaults( _K=?no_option_key,
			_DefaultValue=[ [] ], ArgumentTable ),

	Args.



% Extracts, for specified command-line option (if any was specified; otherwise
% returns 'undefined') its various in-order lists of associated values, from the
% arguments specified to this executable.
%
% Returns a pair made of these lists of (lists of) values and of the shrunk
% corresponding argument table.
%
% Note: a value set to 'undefined' means that the specified option is not in the
% specified table, whereas a value set to [ [] ] means that this option is in
% the table, yet that no parameter has been specified for it.
%
-spec extract_command_arguments_for_option( command_line_option() ) ->
		  { maybe( [ command_line_values() ] ), argument_table() }.
extract_command_arguments_for_option( Option ) ->

	ArgumentTable = get_argument_table(),

	extract_command_arguments_for_option( Option, ArgumentTable ).



% Extracts, for specified command-line option (if any was specified; otherwise
% returns 'undefined') its various in-order lists of associated values, from the
% specified argument table.
%
% Returns a pair made of these lists of (lists of) values and of the shrunk
% corresponding argument table.
%
% Note: a value set to 'undefined' means that the specified option is not in the
% specified table, whereas a value set to [ [] ] means that this option is in
% the table, yet that no parameter has been specified for it.
%
-spec extract_command_arguments_for_option( command_line_option(), argument_table() ) ->
			  { maybe( [ command_line_values() ] ), argument_table() }.
extract_command_arguments_for_option( Option, ArgumentTable ) ->
	list_table:extract_entry_with_defaults( _K=Option, _DefaultValue=undefined,
											ArgumentTable ).



% Extracts the in-order list of the arguments that were directly (i.e. not in
% the context of an option) specified on the command-line for this executable.
%
% Returns a pair made of these lists of values and of the shrunk corresponding
% argument table.
%
-spec extract_optionless_command_arguments() ->
		  { maybe( [ command_line_values() ] ), argument_table() }.
extract_optionless_command_arguments() ->

	ArgumentTable = get_argument_table(),

	extract_optionless_command_arguments( ArgumentTable ).



% Extracts, for specified command-line option (if any was specified; otherwise
% returns 'undefined') its various in-order lists of associated values, from the
% specified argument table.
%
% Returns a pair made of these lists of (lists of) values and of the shrunk
% corresponding argument table.
%
% Note: a value set to 'undefined' means that the specified option is not in the
% specified table, whereas a value set to [ [] ] means that this option is in
% the table, yet that no parameter has been specified for it.
%
-spec extract_optionless_command_arguments( argument_table() ) ->
			  { maybe( [ command_line_values() ] ), argument_table() }.
extract_optionless_command_arguments( ArgumentTable ) ->

	% Not wanting here a list of lists of strings:
	case list_table:extract_entry_with_defaults( _K=?no_option_key,
							_DefaultValue=undefined, ArgumentTable ) of

		undefined ->
			{ undefined, ArgumentTable };

		{ [ Args ], ShrunkArgTable } ->
			% Not wanting here a list of lists of strings:
			{ Args, ShrunkArgTable }

	end.



% Returns a textual representation of the specified argument table.
-spec argument_table_to_string( argument_table() ) -> string().
argument_table_to_string( ArgTable ) ->

	% No-op:
	case list_table:enumerate( ArgTable ) of

		[] ->
			"no command-line argument specified";

		ArgPairs ->
			ArgStrings = [ option_pair_to_string( Option, ArgumentLists )
						   || { Option, ArgumentLists } <- ArgPairs ],

			text_utils:format( "~B command-line argument(s) specified "
				"(listed alphabetically): ~s", [ length( ArgPairs ),
						 text_utils:strings_to_sorted_string( ArgStrings ) ] )

	end.


% (helper)
option_pair_to_string( _Option=?no_option_key, [ Arguments ] ) ->
	text_utils:format( "option-less arguments: ~p", [ Arguments ] );

option_pair_to_string( Option, _ArgumentLists=[ [] ] ) ->
	% No value:
	text_utils:format( "option '-~s'", [ Option ] );

option_pair_to_string( Option, ArgumentLists ) ->
	text_utils:format( "option '-~s', with argument lists: ~p",
					   [ Option, ArgumentLists ] ).
