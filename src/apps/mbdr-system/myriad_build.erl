% Copyright (C) 2020-2025 Olivier Boudeville
%
% Released as LGPL software.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: 2020.

-module(myriad_build).

% Should be hidden, as not useful to list in APIs (and mostly empty currently):
-moduledoc """
An experimental (not functional yet) module to **explore alternate build
systems**.

We finally stick to our make-based build system that we found more suitable than
 (for example) rebar3.

Better here than in `myriad-build.escript` to benefit from a more user-friendly
debugging.
""".


-define( exec_name, "myriad-build.escript" ).


-export([ run/0, main/1 ]).



-doc "Typically for testing.".
-spec run() -> void().
run() ->
	ArgTable = cmd_line_utils:get_argument_table(),
	main( ArgTable ).


% Defaults:


-doc "Returns the usage information of the corresponding application.".
-spec get_usage() -> void().
get_usage() ->
	text_utils:format( "Usage: ~ts MBDR_PROJECT_FILE.mbdr"
		"[-h|--help]~n"
		"  Builds specified MBDR project, where:~n"
		" - MBDR_PROJECT_FILE.mbdr is a MBDR file corresponding to the project "
		"that shall be built~n", [ ?exec_name ] ).



-doc """
Sole entry point for this buid service, either triggered by `run/0` or by the
associated escript.
""".
-spec main( cmd_line_utils:argument_table() ) -> void().
main( ArgTable ) ->

	%trace_utils:debug_fmt( "Original script-specific arguments: ~ts",
	%   [ cmd_line_utils:argument_table_to_string( ArgTable ) ] ),

	[ %InteractiveRefKey,
	  HelpRefKey ] =
		[ %'-interactive',
		  '-help' ],

	% Standardises command-line options:
	MergedTable = list_table:merge_in_keys( [
			%{ InteractiveRefKey, [ 'i' ] },
			{ HelpRefKey, [ 'h' ] } ], ArgTable ),

	%trace_utils:debug_fmt( "Canonicalized script-specific arguments: ~ts",
	%   [ cmd_line_utils:argument_table_to_string( MergedTable ) ] ),

	list_table:has_entry( HelpRefKey, MergedTable ) andalso display_usage(),

	%{ IsInteractive, InterTable } = case
	% list_table:extract_entry_with_default( InteractiveRefKey,
	% _DefaultInter=false, MergedTable ) of
	%
	%   { [], ShrunkTable } ->
	%       { true, ShrunkTable };
	%
	%   P={ false, _ShrunkTable } ->
	%       P
	%
	%end,

	%trace_utils:debug_fmt( "Interactive: ~ts", [ IsInteractive ] ),

	%ProjectFileValue = case list_table:lookup_entry( _Key=fixme, fixme ) of

	ResultingTable = MergedTable,

	case list_table:keys( ResultingTable ) of

		[] ->
			ok;

		UnexpectedOpts ->
			trace_utils:error_fmt( "Unexpected user input: ~ts~n~ts",
				[ cmd_line_utils:argument_table_to_string( ResultingTable ),
				  get_usage() ] ),
			throw( { unexpected_command_line_options, UnexpectedOpts } )

	end,

	trace_utils:notice( "Stopping now." ),

	basic_utils:stop( _ErrorCode=0 ).



-doc "Displays the usage of this service, and stops (with no error).".
display_usage() ->
	io:format( get_usage(), [] ),
	basic_utils:stop( _ErrorCode=0 ).
