% Copyright (C) 2016-2018 Olivier Boudeville
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
% Creation date: Wednesday, May 2, 2018



% This is the most basic, line-based monochrome textual interface, directly in
% raw text with no cursor control.
%
% See:
%
% - text_ui_test.erl for the corresponding test
% - term_ui.erl for a more advanced text interface
% - gui.erl for a graphical counterpart
%
% See also: trace_utils.erl for another kind of output.
%
-module(text_ui).



% Implementation notes:
%
% In this very specific case, we use the process dictionary to avoid having to
% keep around a UI-state variable in all calls.
%
% So most of the time the UI state is implicit; counterpart functions with an
% explicit state are also provided (ex: if having a large number of UI
% operations to perform in a row), in which case they are to return an updated
% state.


% Basic UI operations.
%
-export([ start/0, start/1,

		  display/1, display/2, display/3,

		  display_numbered_list/2, display_numbered_list/3,

		  display_error/1, display_error/2, display_error/3,

		  display_error_numbered_list/2, display_error_numbered_list/3,

		  add_separation/0, add_separation/1,

		  get_text/2, get_text_as_integer/2, get_text_as_maybe_integer/2,
		  read_text_as_integer/2,

		  choose_designated_item/1, choose_designated_item/2,
		  choose_designated_item/3,

		  choose_numbered_item/1, choose_numbered_item/2,
		  choose_numbered_item/3,

		  choose_numbered_item_with_default/2,
		  choose_numbered_item_with_default/3,
		  choose_numbered_item_with_default/4,

		  trace/1, trace/2,

		  stop/0, stop/1,

		  to_string/0, to_string/1 ]).



-record( text_ui_state, {
		   log_console = false :: boolean(),
		   log_file = undefined :: maybe( file_utils:file() ),
		   settings :: setting_table()
}).

-type ui_state() :: #text_ui_state{}.



% For common, transverse defines:
-include("ui.hrl").


-export_type([ ui_state/0 ]).



% An I/O device, either standard_io, standard_error, a registered name, or a pid
% handling I/O protocols (returned from file:open/2):
%
-type channel() :: io:device().

-define( error_prefix, "~n [error] " ).
-define( error_suffix, "~n" ).




% Starts the UI with default settings.
%
% Stores the corresponding state in the process dictionary, yet returns as well
% that state, for any explicit later operation.
%
-spec start() -> ui_state().
start() ->
	start( _Opts=[] ).



% Starts the UI with specified settings.
%
% Stores the corresponding state in the process dictionary, yet returns as well
% that state, for any explicit later operation.
%
-spec start( ui_options() ) -> ui_state().
start( Options ) ->
	BlankUIState = #text_ui_state{},
	start( Options, BlankUIState ).


% (non-exported helper)
start( _Options=[], UIState ) ->

	% Check:
	undefined = process_dictionary:put( ?ui_name_key, ?MODULE ),

	% No prior state expected:
	case process_dictionary:put( ?ui_state_key, UIState ) of

		undefined ->
			ok;

		_ ->
			throw( text_ui_already_started )

	end,
	UIState;

start( _Options=[ log_file | T ], UIState ) ->
	start( [ { log_file, "ui.log" } | T ], UIState );

start( _Options=[ { log_file, Filename } | T ], UIState ) ->
	LogFile = file_utils:open( Filename, [ write, exclusive ] ),
	file_utils:write( LogFile, "Starting text UI.\n" ),
	NewUIState = UIState#text_ui_state{ log_file=LogFile },
	start( T, NewUIState );

start( SingleElem, UIState ) ->
	start( [ SingleElem ], UIState ).




% Displays specified text, as a normal message, based on an implicit state.
%
-spec display( text() ) -> void().
display( Text ) ->
	display( Text, get_state() ).


% Displays specified text, as a normal message, based on an explicit state.
%
% Displays specified formatted text, as a normal message, based on an implicit
% state.
%
-spec display( text(), ui_state() ) -> ui_state();
			 ( text_utils:format_string(), [ term() ] ) -> void().
display( Text, UIState ) when is_record( UIState, text_ui_state ) ->
	display_helper( _Channel=standard_io, Text, UIState );

display( FormatString, Values ) ->
	display( FormatString, Values, get_state() ).



% Displays specified formatted text, as a normal message, based on an explicit
% state.
%
-spec display( text_utils:format_string(), [ term() ], ui_state() ) ->
					 ui_state().
display( FormatString, Values, UIState ) ->
	display_helper( _Channel=standard_io, FormatString, Values, UIState ).



% Displays in-order the items of specified list, as a normal message, based on
% an implicit state.
%
-spec display_numbered_list( label(), [ text() ] ) -> void().
display_numbered_list( Label, Lines ) ->
	display_numbered_list( Label, Lines, get_state() ).


% Displays in-order the items of specified list, as a normal message, based on
% an explicit state.
%
-spec display_numbered_list( label(), [ text() ], ui_state() ) -> ui_state().
display_numbered_list( Label, Lines, UIState ) ->
	LineStrings = text_utils:strings_to_enumerated_string( Lines ),
	display_helper( _Channel=standard_io, "~s~s", [ Label, LineStrings ],
					UIState ).



% Displays specified text, as an error message, based on an implicit state.
%
-spec display_error( text() ) -> void().
display_error( Text ) ->
	display_error( Text, get_state() ).



% Displays specified text, as an error message, based on an explicit state.
%
% Displays specified formatted text, as an error message, based on an implicit
% state.
%
-spec display_error( text(), ui_state() ) -> ui_state();
				   ( text_utils:format_string(), [ term() ] ) -> void().
display_error( Text, UIState ) when is_record( UIState, text_ui_state ) ->
	display_helper( standard_error, ?error_prefix ++ Text ++ ?error_suffix,
					UIState );

display_error( FormatString, Values ) ->
	display_error( FormatString, Values, get_state() ).



% Displays specified formatted text, as an error message, based on an explicit
% state.
%
-spec display_error( text_utils:format_string(), [ term() ], ui_state() ) ->
					 ui_state().
display_error( FormatString, Values, UIState ) ->
	display_helper( standard_error,
					?error_prefix ++ FormatString ++ ?error_suffix, Values,
					UIState ).



% Displays in-order the items of specified list, as an error message, based on
% an implicit state.
%
-spec display_error_numbered_list( label(), [ text() ] ) -> void().
display_error_numbered_list( Label, Lines ) ->
	display_error_numbered_list( Label, Lines, get_state() ).



% Displays in-order the items of specified list, as an error message, based on
% an explicit state.
%
-spec display_error_numbered_list( label(), [ text() ], ui_state() ) ->
										 ui_state().
display_error_numbered_list( Label, Lines, UIState ) ->
	LineStrings = text_utils:strings_to_enumerated_string( Lines ),
	display_helper( _Channel=standard_error,
					?error_prefix ++ "~s~s" ++ ?error_suffix,
					[ Label, LineStrings ], UIState ).



% Adds a default separation between previous and next content, based on an
% implicit state.
%
-spec add_separation() -> void().
add_separation() ->
	add_separation( get_state() ).


% Adds a default separation between previous and next content, based on an
% explicit state.
%
-spec add_separation( ui_state() ) -> ui_state().
add_separation( UIState ) ->
	display( _Text="", UIState ).




% Returns the user-entered text, based on an implicit state.
%
% (const)
%
-spec get_text( prompt() ) -> text().
get_text( Prompt ) ->
	text_utils:remove_ending_carriage_return( io:get_line( Prompt ) ).


% Returns the user-entered text, based on an explicit state.
%
% (const)
%
-spec get_text( prompt(), ui_state() ) -> text().
get_text( Prompt, _UIState ) ->
	get_text( Prompt ).



% Returns the user-entered text, once translated to an integer, based on an
% implicit state.
%
% (const)
%
-spec get_text_as_integer( prompt() ) -> text().
get_text_as_integer( Prompt ) ->

	Text = get_text( Prompt ),

	text_utils:string_to_integer( Text ).


% Returns the user-entered text, once translated to an integer, based on an
% explicit state.
%
% (const)
%
-spec get_text_as_integer( prompt(), ui_state() ) -> text().
get_text_as_integer( Prompt, _UIState ) ->
	get_text_as_integer( Prompt ).



% Returns the user-entered text, once translated to an integer, based on an
% implicit state, prompting the user until a valid input is obtained.
%
% (const)
%
-spec read_text_as_integer( prompt() ) -> text().
read_text_as_integer( Prompt ) ->

	Text = get_text( Prompt ),

	case text_utils:try_string_to_integer( Text ) of

		undefined ->
			read_text_as_integer( Prompt );

		I ->
			I

	end.



% Returns the user-entered text, once translated to an integer, based on an
% explicit state.
%
% (const)
%
-spec read_text_as_integer( prompt(), ui_state() ) -> text().
read_text_as_integer( Prompt, _UIState ) ->
	read_text_as_integer( Prompt ).



% Returns the user-entered text (if any), once translated to an integer, based
% on an implicit state.
%
% (const)
%
-spec get_text_as_maybe_integer( prompt() ) -> maybe( text() ).
get_text_as_maybe_integer( Prompt ) ->

	case get_text( Prompt ) of

		"" ->
			undefined;

		Text ->
			text_utils:string_to_integer( Text )

	end.


% Returns the user-entered text (if any), once translated to an integer, based
% on an explicit state.
%
% (const)
%
-spec get_text_as_maybe_integer( prompt(), ui_state() ) -> maybe( text() ).
get_text_as_maybe_integer( Prompt, _UIState ) ->
	get_text_as_maybe_integer( Prompt ).



% Returns the user-entered text, once translated to an integer, based on an
% implicit state, prompting the user until a valid input is obtained: either a
% string that resolves to an (then returned) integer, or an empty string (then
% returning 'undefined').
%
% (const)
%
-spec read_text_as_maybe_integer( prompt() ) -> maybe( text() ).
read_text_as_maybe_integer( Prompt ) ->

	case get_text( Prompt ) of

		"" ->
			undefined;

		Text ->
			case text_utils:try_string_to_integer( Text ) of

				undefined ->
					read_text_as_integer( Prompt );

				I ->
					I

			end

	end.



% Returns the user-entered text, once translated to an integer, based on an
% explicit state.
%
% (const)
%
-spec read_text_as_maybe_integer( prompt(), ui_state() ) -> text().
read_text_as_maybe_integer( Prompt, _UIState ) ->
	read_text_as_maybe_integer( Prompt ).



% Selects, using a default prompt, an item among the specified ones, and returns
% its designator, based on an implicit state.
%
% (const)
%
-spec choose_designated_item( [ choice_element() ] ) -> choice_designator().
choose_designated_item( Choices ) ->
	choose_designated_item( Choices, get_state() ).


% Selects, using a default prompt, an item among the specified ones, and returns
% its designator, based on an explicit state.
%
% Selects, based on an implicit state, using the specified label, an item among
% the specified ones, and returns its designator.
%
% (const)
%
-spec choose_designated_item( [ choice_element() ], ui_state() ) ->
									choice_designator();
							( label(), [ choice_element() ] ) ->
									choice_designator().
choose_designated_item( Choices, UIState )
  when is_record( UIState, text_ui_state ) ->

	Prompt = text_utils:format( "Select among these ~B choices:",
								[ length( Choices ) ] ),

	choose_designated_item( Prompt, Choices, UIState );

choose_designated_item( Label, Choices ) ->
	choose_designated_item( Label, Choices, get_state() ).



% Selects, based on an explicit state, using the specified label, an item among
% the specified ones, and returns its designator.
%
% (const)
%
-spec choose_designated_item( label(), [ choice_element() ], ui_state() ) ->
									choice_designator().
choose_designated_item( Label, Choices, UIState ) ->

	{ Designators, Texts } = lists:unzip( Choices ),

	ChoiceCount = length( Choices ),

	{ _FinalCount, NumberedText } = lists:foldl(
					 fun( Text, { Count, AccText } ) ->

						NewText = text_utils:format( "[~B] ~s",
													 [ Count, Text ] ),

						NewAccText = [ NewText | AccText ],

						{ Count+1, NewAccText }

					 end,
					 _Acc0= { 1, [] },
					 _List=Texts ),

	Text = text_utils:strings_to_string(
			 lists:reverse( NumberedText ), _Bullet=" " ),

	FullLabel = text_utils:format( "~s~s~nChoice> ", [ Label, Text ] ),

	case read_text_as_integer( FullLabel, UIState ) of

		{ parsing_failed, Input } ->
			display_error( "Input shall be an integer (not ~s).",
						   [ Input ], UIState ),
			choose_designated_item( Label, Choices, UIState );


		N when N < 1 ->
			display_error( "Specified choice shall be at least 1 (not ~B).",
						   [ N ], UIState ),
			%throw( { invalid_choice, too_low, N } );
			choose_designated_item( Label, Choices, UIState );

		N when N > ChoiceCount ->
			display_error(
			  "Specified choice shall not be greater than ~B (not ~B).",
			  [ ChoiceCount, N ], UIState ),
			%throw( { invalid_choice, too_high, N } );
			choose_designated_item( Label, Choices, UIState );

		N ->
			lists:nth( N, Designators )

	end.



% Selects, based on an implicit state, using a default label, an item among the
% specified ones, and returns its index.
%
-spec choose_numbered_item( [ choice_element() ] ) ->  choice_index().
choose_numbered_item( Choices ) ->
	choose_numbered_item( Choices, get_state() ).


% Selects, based on an explicit state, using a default label, an item among the
% specified ones, and returns its index.
%
% Selects, based on an implicit state, using the specified label, an item among
% the specified ones, and returns its index.
%
-spec choose_numbered_item( [ choice_element() ], ui_state() ) ->
								  choice_index();
						  ( label(), [ choice_element() ] ) -> choice_index().
choose_numbered_item( Choices, UIState )
  when is_record( UIState, text_ui_state ) ->

	Label = text_utils:format( "Select among these ~B choices:",
								[ length( Choices ) ] ),

	choose_numbered_item( Label, Choices, UIState );

choose_numbered_item( Label, Choices ) ->
	choose_numbered_item( Label, Choices, get_state() ).



% Selects, based on an explicit state, using the specified label, an item among
% the specified ones, and returns its index.
%
-spec choose_numbered_item( label(), [ choice_element() ], ui_state() ) ->
								  choice_index().
choose_numbered_item( Label, Choices, UIState ) ->

	ChoiceCount = length( Choices ),

	{ _FinalCount, NumberedText } = lists:foldl(
					 fun( Text, { Count, AccText } ) ->

						NewText = text_utils:format( "[~B] ~s",
													 [ Count, Text ] ),

						NewAccText = [ NewText | AccText ],

						{ Count+1, NewAccText }

					 end,
					 _Acc0= { 1, [] },
					 _List=Choices ),

	Text = text_utils:strings_to_string(
			 lists:reverse( NumberedText ), _Bullet=" " ),

	FullLabel = text_utils:format( "~s~s~nChoice> ", [ Label, Text ] ),

	SelectedNumber = get_text_as_integer( FullLabel, UIState ),

	%trace_utils:format( "Selected: ~B", [ SelectedNumber ] ),

	case SelectedNumber of

		N when N < 1 ->
			display_error( "Specified choice shall be at least 1 (not ~B).",
						   [ N ], UIState ),
			%throw( { invalid_choice, too_low, N } );
			choose_numbered_item( Label, Choices, UIState );

		N when N > ChoiceCount ->
			display_error(
			  "Specified choice shall not be greater than ~B (not ~B).",
			  [ ChoiceCount, N ], UIState ),
			%throw( { invalid_choice, too_high, N } );
			choose_numbered_item( Label, Choices, UIState );

		N ->
			N

	end.



% Selects, based on an implicit state, using a default label, an item among the
% specified ones, and returns its index.
%
-spec choose_numbered_item_with_default( [ choice_element() ],
										 choice_index() ) -> choice_index().
choose_numbered_item_with_default( Choices, DefaultChoiceIndex ) ->
	choose_numbered_item_with_default( Choices, DefaultChoiceIndex,
									   get_state() ).



% Selects, based on an explicit state, using a default label, an item among the
% specified ones, and returns its index.
%
% Selects, based on an implicit state, using the specified label and default
% item, an item among the specified ones, and returns its index.
%
-spec choose_numbered_item_with_default( [ choice_element() ], choice_index(),
										 ui_state() ) -> choice_index();
									   ( label(), [ choice_element() ],
										 maybe( choice_index() ) ) ->
											   choice_index().
choose_numbered_item_with_default( Choices, DefaultChoiceIndex, UIState )
  when is_record( UIState, text_ui_state ) ->

	Label = text_utils:format( "Select among these ~B choices:",
								[ length( Choices ) ] ),

	choose_numbered_item_with_default( Label, Choices, DefaultChoiceIndex,
									   UIState );

choose_numbered_item_with_default( Label, Choices, DefaultChoiceIndex ) ->
	choose_numbered_item_with_default( Label, Choices, DefaultChoiceIndex,
									   get_state() ).



% Selects, based on an explicit state, using the specified label and default
% item, an item among the specified ones, and returns its index.
%
-spec choose_numbered_item_with_default( label(), [ choice_element() ],
			maybe( choice_index() ), ui_state() ) -> choice_index().
choose_numbered_item_with_default( Label, Choices, DefaultChoiceIndex,
								   UIState ) ->

	ChoiceCount = length( Choices ),

	case DefaultChoiceIndex =/= undefined andalso DefaultChoiceIndex > 0
		andalso DefaultChoiceIndex =< ChoiceCount of

		true ->
			ok;

		false ->
			throw( { invalid_default_index, DefaultChoiceIndex } )

	end,

	{ _FinalCount, NumberedText } = lists:foldl(
					 fun( Text, { Count, AccText } ) ->

						NewText = text_utils:format( "[~B] ~s",
													 [ Count, Text ] ),

						NewAccText = [ NewText | AccText ],

						{ Count+1, NewAccText }

					 end,
					 _Acc0= { 1, [] },
					 _List=Choices ),

	Text = text_utils:strings_to_string(
			 lists:reverse( NumberedText ), _Bullet=" " ),

	FullLabel = text_utils:format( "~s~s~nChoice [default: ~B]> ",
								   [ Label, Text, DefaultChoiceIndex ] ),

	case read_text_as_maybe_integer( FullLabel, UIState ) of

		% Default:
		undefined ->
			DefaultChoiceIndex;

		N when N < 1 ->
			display_error( "Specified choice shall be at least 1 (not ~B).",
						   [ N ],
						   UIState ),
			%throw( { invalid_choice, too_low, N } );
			choose_numbered_item_with_default( Label, Choices,
											   DefaultChoiceIndex, UIState );

		N when N > ChoiceCount ->
			display_error( "Specified choice shall not be greater than ~B "
						   "(not ~B).", [ ChoiceCount, N ], UIState ),
			%throw( { invalid_choice, too_high, N } );
			choose_numbered_item_with_default( Label, Choices,
											   DefaultChoiceIndex, UIState );

		N ->
			N

	end.



% Traces specified status string, based on an explicit state.
%
-spec trace( string(), ui_state() ) -> void().
trace( Text, UIState ) ->
	display( "[trace] " ++ Text ++ "\n", UIState ).


% Traces specified status string, based on an implicit state.
%
-spec trace( string() ) -> void().
trace( Text ) ->
	trace( Text, get_state() ).



% Stops the UI.
%
-spec stop() -> void().
stop() ->

	stop( get_state() ).



% Stops the UI.
%
-spec stop( ui_state() ) -> void().
stop( #text_ui_state{ log_file=undefined } ) ->
	stop_helper();


stop( #text_ui_state{ log_file=LogFile } ) ->
	file_utils:write( LogFile, "Stopping UI.\n" ),
	file_utils:close( LogFile ),
	stop_helper().


stop_helper() ->
	[ process_dictionary:remove( Key )
	  || Key <- [ ?ui_name_key, ?ui_state_key ] ].



% Helper section.


% Returns the current UI state.
%
% (helper)
%
-spec get_state() -> ui_state().
get_state() ->

	case process_dictionary:get( ?ui_state_key ) of

		undefined ->
			throw( text_ui_not_started );

		UIState ->
			UIState

	end.




% Displays specified text, on specified channel.
%
% (helper)
%
-spec display_helper( channel(), text(), ui_state() ) -> ui_state().
display_helper( Channel, Text, UIState ) ->
	display_helper( Channel, Text, _Values=[], UIState ).



% Displays specified formatted text, on specified channel.
%
-spec display_helper( channel(), text_utils:format_string(), [ term() ],
					  ui_state() ) -> ui_state().
display_helper( Channel, FormatString, Values, UIState ) ->

	%trace_utils:debug_fmt( "Displaying, on channel '~p', '~p', with '~p'.",
	%				   [ Channel, FormatString, Values ] ),

	io:format( Channel, FormatString ++ "~n", Values ),
	UIState.



% Returns a textual description of the (implicit) UI state.
%
-spec to_string() -> string().
to_string() ->
	to_string( get_state() ).


% Returns a textual description of the specified UI state.
%
-spec to_string( ui_state() ) -> string().
to_string( #text_ui_state{ log_console=LogConsole,
						   log_file=LogFile }) ->

	ConsoleString = case LogConsole of

		true ->
			"";

		false ->
			"not"

	end,

	FileString = case LogFile of

		undefined ->
			"not using a log file";

		_ ->
			text_utils:format( "using log file '~s'", [ LogFile ] )

	end,

	text_utils:format( "text_ui interface, ~s writing logs on console, "
					   "and ~s", [ ConsoleString, FileString ] ).
