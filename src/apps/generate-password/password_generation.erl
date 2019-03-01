% Copyright (C) 2016-2019 Olivier Boudeville
%
% Transferred from generate-password.escript to benefit from a more
% user-friendly debugging.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Released as LGPL software.
%
-module(password_generation).


% Implementation notes:
%

-define( exec_name, "generate-password.escript.escript" ).


-export([ run/0, main/1 ]).

% Typically for testing:
-spec run() -> void().
run() ->
	ArgTable = executable_utils:get_argument_table(),
	main( ArgTable ).


% Defaults:

-define( default_length, 16 ).
-define( default_alphabet, default ).


-spec get_usage() -> basic_utils:void().
get_usage() ->
	"Usage: ?exec_name [-i|--interactive] [-l LEN|--length LEN] "
		"[-a ALPHABET|--alphabet ALPHABET] [-h|--help]~n"
		"  Generates a suitable password, where:~n"
		"    - LEN is the (exact) number of characters to generate for "
		"this password [default: ?default_length]~n"
		"    - ALPHABET designates the set of characters to draw from, among:~n"
		"       * 'default': alphanumeric letters, all cases [A-Za-z0-9]~n"
		"       * 'extended': 'default' + basic punctuation~n"
		"       * 'full': 'default' + all punctuation~n".




% Sole entry point for this generation service, either triggered by run/0 or by
% the associated escript.
%
-spec main( executable_utils:argument_table() ) -> void().
main( ArgTable ) ->

	%trace_utils:debug_fmt( "Original script-specific arguments: ~s",
	%	   [ executable_utils:argument_table_to_string( ArgTable ) ] ),

	[ InteractiveRefKey, LengthRefKey, AlphaRefKey, HelpRefKey ] =
		[ '-interactive', '-length', '-alphabet', '-help' ],

	% Standardises command-line options:
	MergedTable = list_table:merge_in_keys( [
			{ InteractiveRefKey, [ 'i' ] },
			{ LengthRefKey, [ 'l' ] },
			{ AlphaRefKey, [ 'a' ] },
			{ HelpRefKey, [ 'h' ] } ], ArgTable ),

	%trace_utils:debug_fmt( "Canonicalized script-specific arguments: ~s",
	%	   [ executable_utils:argument_table_to_string( MergedTable ) ] ),


	case list_table:hasEntry( HelpRefKey, MergedTable ) of

		true ->
			display_usage();

		false ->
			ok

	end,

	{ IsInteractive, InterTable } = case list_table:extractEntryWithDefaults(
				 InteractiveRefKey, _DefaultInter=false, MergedTable ) of

		{ [], ShrunkTable } ->
			{ true, ShrunkTable };

		P={ false, _ShrunkTable } ->
			P

	end,

	trace_utils:debug_fmt( "Interactive: ~s", [ IsInteractive ] ),

	{ [ LengthString ], LenTable } = list_table:extractEntryWithDefaults(
			LengthRefKey, _LenDefault=[ ?default_length ], InterTable ),

	Length = text_utils:string_to_integer( LengthString ),

	trace_utils:debug_fmt( "Length: ~B", [ Length ] ),

	{ [ AlphabetStringSpec ], AlphaTable } = list_table:extractEntryWithDefaults(
			AlphaRefKey, _AlphaDefault=[ ?default_alphabet ], LenTable ),

	AlphabetSpec = text_utils:string_to_atom( AlphabetStringSpec ),

	trace_utils:debug_fmt( "Alphabet spec: ~s", [ AlphabetSpec ] ),

	case list_table:keys( AlphaTable ) of

		[] ->
			ok;

		UnexpectedOpts ->
			trace_utils:error_fmt(
			  "Unexpected command-line option(s) specified: ~s",
			  [ text_utils:atoms_to_string( UnexpectedOpts ) ] )

	end,

	Alphabet = get_alphabet( AlphabetSpec ),

	trace_utils:debug_fmt( "Input alphabet corresponding to spec ~p: "
						   "'~w' (i.e. '~s').",
						   [ AlphabetSpec, Alphabet, Alphabet] ),

	basic_utils:stop( _ErrorCode=0 ).


% Displays the usage of this service, and stops (with no error).
display_usage() ->
	trace_utils:info_fmt(  get_usage(), [] ),
	basic_utils:stop( _ErrorCode=0 ).


% Returns the corresponding alphabet, based on its spec, expressed as an atom
% (ex: 'numeric' for all numeric literals) or as a list thereof.
%
get_alphabet( AlphabetSpecs ) when is_list( AlphabetSpecs ) ->
	list_utils:flatten_once( [ get_alphabet( A ) || A <- AlphabetSpecs ] );

get_alphabet( _AlphabetSpec=default ) ->
	get_alphabet( [ lower_case, upper_case, numeric ] );

get_alphabet( _AlphabetSpec=extended ) ->
	get_alphabet( [ lower_case, upper_case, numeric, basic_punctuation ] );

get_alphabet( _AlphabetSpec=full ) ->
	get_alphabet( [ lower_case, upper_case, numeric, basic_punctuation,
					extra_punctuation ] );

get_alphabet( _AlphabetSpec=lower_case ) ->
	lists:seq( $a, $z );

get_alphabet( _AlphabetSpec=upper_case ) ->
	lists:seq( $A, $Z );

get_alphabet( _AlphabetSpec=numeric ) ->
	lists:seq( 0, 9 );

get_alphabet( _AlphabetSpec=basic_punctuation ) ->
	[ $[, $], $(, $), ${, $}, $:, $,, $;, $-, $_, $., $!, $? ];

get_alphabet( _AlphabetSpec=extra_punctuation ) ->
	[ $", $', $@, $ , $/, $&, $$, $*, $\\, $^, $%, $=, $+, $| ].
