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
% Creation date: Monday, February 15, 2010.


% Gathering of various facilities for User Interfaces (graphical or not).
%
% The base interface proposed here is purely textual.
%
% See ui_test.erl for the corresponding test.
%
% See gui.erl for a graphical counterpart.
%
% See also: trace_utils.erl for another kind of output.
%
-module(ui).


% We tried to use 'dialog' or 'whiptail' to provide a nice text interface, but
% neither seems to work with Erlang (regardless of how we run them and the
% options given to the interpreter).

% Zenity could be used as well, not to mention a plain use of the wxWidgets
% module, 'wx', however we do not want here to depend on a graphical interface
% (such as X), as servers often do not have such a feature.


% Implementation notes:


% Basic UI operations.
%
-export([ start/0, start/1,
		  display/2, display/3, display_numbered_list/3,
		  display_error/2, display_error/3, display_error_numbered_list/3,

		  add_separation/1,

		  get_text/2, get_text_as_integer/2, get_text_as_maybe_integer/2,

		  choose_designated_item/2, choose_designated_item/3,
		  choose_numbered_item/2, choose_numbered_item/3,
		  choose_numbered_item_with_default/3, choose_numbered_item_with_default/4,

		  trace/2,
		  stop/1 ]).


-type text() :: text_utils:string().

-type label() :: text().
-type prompt() :: text().


% The text of a choice:
-type choice_text() :: text().


% Designator of a choice (ex: regardless of the choice labels, locales, etc.):
-type choice_designator() :: atom().


% The index of a choice (starting at 1):
-type choice_index() :: basic_utils:count().


-type choice_element() :: { choice_designator(), choice_text() }.


-record( ui_state, {
		   log_console = false :: boolean(),
		   log_file = undefined :: maybe( file_utils:file() )
}).


-type ui_state() :: #ui_state{}.

-type ui_options() :: [ any() ].



-export_type([ text/0, label/0, ui_state/0, ui_options/0,
			   choice_designator/0, choice_index/0, choice_element/0 ]).


% An I/O device, either standard_io, standard_error, a registered name, or a pid
% handling I/O protocols (returned from file:open/2):
%
-type channel() :: io:device().

-define( error_prefix, "~n [error] " ).
-define( error_suffix, "~n" ).



% Starts the UI with default settings.
%
-spec start() -> ui_state().
start() ->
	start( _Opts=[] ).



% Starts the UI with specified settings.
%
-spec start( ui_options() ) -> ui_state().
start( Options ) ->
	BlankState = #ui_state{},
	start( Options, BlankState ).


% (helper)
start( _Options=[], State ) ->
	State;

start( _Options=[ log_file | T ], State ) ->
	start( [ { log_file, "ui.log" } | T ], State );

start( _Options=[ { log_file, Filename } | T ], State ) ->
	LogFile = file_utils:open( Filename, [ write, exclusive ] ),
	file_utils:write( LogFile, "Starting UI.\n" ),
	NewState = State#ui_state{ log_file=LogFile },
	start( T, NewState );

start( SingleElem, State ) ->
	start( [ SingleElem ], State ).




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




% Displays specified text, as a normal message.
%
-spec display( text(), ui_state() ) -> ui_state().
display( Text, UIState ) ->
	display_helper( _Channel=standard_io, Text, UIState ).



% Displays specified formatted text, as a normal message.
%
-spec display( text_utils:format_string(), [ term() ], ui_state() ) ->
					 ui_state().
display( FormatString, Values, UIState ) ->
	display_helper( _Channel=standard_io, FormatString, Values, UIState ).


% Displays in-order the items of specified list, as a normal message.
%
-spec display_numbered_list( label(), [ text() ], ui_state() ) -> ui_state().
display_numbered_list( Label, Lines, UIState ) ->
	LineStrings = text_utils:strings_to_enumerated_string( Lines ),
	display_helper( _Channel=standard_io, "~s~s", [ Label, LineStrings ],
					UIState ).


% Displays specified text, as an error message.
%
-spec display_error( text(), ui_state() ) -> ui_state().
display_error( Text, UIState ) ->
	display_helper( standard_error, ?error_prefix ++ Text ++ ?error_suffix,
					UIState ).



% Displays specified formatted text, as an error message.
%
-spec display_error( text_utils:format_string(), [ term() ], ui_state() ) ->
					 ui_state().
display_error( FormatString, Values, UIState ) ->
	display_helper( standard_error,
					?error_prefix ++ FormatString ++ ?error_suffix, Values,
					UIState ).


% Displays in-order the items of specified list, as an error message.
%
-spec display_error_numbered_list( label(), [ text() ], ui_state() ) -> ui_state().
display_error_numbered_list( Label, Lines, UIState ) ->
	LineStrings = text_utils:strings_to_enumerated_string( Lines ),
	display_helper( _Channel=standard_error, ?error_prefix ++ "~s~s" ++ ?error_suffix,
					[ Label, LineStrings ], UIState ).


% Adds a default separation between previous and next content.
%
-spec add_separation( ui_state() ) -> ui_state().
add_separation( UIState ) ->
	display( _Text="", UIState ).




% Returns the user-entered text.
%
-spec get_text( prompt(), ui_state() ) -> text().
get_text( Prompt, _UIState ) ->
	text_utils:remove_ending_carriage_return( io:get_line( Prompt ) ).



% Returns the user-entered text, once translated to an integer.
%
-spec get_text_as_integer( prompt(), ui_state() ) -> text().
get_text_as_integer( Prompt, UIState ) ->

	Text = get_text( Prompt, UIState ),

	text_utils:string_to_integer( Text ).



% Returns the user-entered text (if any), once translated to an integer.
%
-spec get_text_as_maybe_integer( prompt(), ui_state() ) -> maybe( text() ).
get_text_as_maybe_integer( Prompt, UIState ) ->

	case get_text( Prompt, UIState ) of

		"" ->
			undefined;

		Text ->
			text_utils:string_to_integer( Text )

end.



% Selects, using a default prompt, an item among the specified ones, and returns
% its designator.
%
-spec choose_designated_item( [ choice_element() ], ui_state() ) -> choice_designator().
choose_designated_item( Choices, UIState ) ->

	Prompt = text_utils:format( "Select among these ~B choices:",
								[ length( Choices ) ] ),

	choose_designated_item( Prompt, Choices, UIState ).



% Selects, using the specified label, an item among the specified ones, and returns
% its designator.
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

	SelectedNumber = get_text_as_integer( FullLabel, UIState ),

	%trace_utils:format( "Selected: ~B", [ SelectedNumber ] ),

	case SelectedNumber of

		N when N < 1 ->
			display_error( "Specified choice shall be at least 1 (not ~B).", [ N ],
						   UIState ),
			throw( { invalid_choice, too_low, N } );

		N when N > ChoiceCount ->
			display_error( "Specified choice shall not be greater than ~B (not ~B).",
						   [ ChoiceCount, N ], UIState ),
			throw( { invalid_choice, too_high, N } );

		N ->
			lists:nth( N, Designators )

	end.




% Selects, using a default label, an item among the specified ones, and returns
% its index.
%
-spec choose_numbered_item( [ choice_element() ], ui_state() ) ->
								  choice_index().
choose_numbered_item( Choices, UIState ) ->

	Label = text_utils:format( "Select among these ~B choices:",
								[ length( Choices ) ] ),

	choose_numbered_item( Label, Choices, UIState ).



% Selects, using the specified label, an item among the specified ones, and returns
% its index.
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
			display_error( "Specified choice shall be at least 1 (not ~B).", [ N ],
						   UIState ),
			throw( { invalid_choice, too_low, N } );

		N when N > ChoiceCount ->
			display_error( "Specified choice shall not be greater than ~B (not ~B).",
						   [ ChoiceCount, N ], UIState ),
			throw( { invalid_choice, too_high, N } );

		N ->
			N

	end.




% Selects, using a default label, an item among the specified ones, and returns
% its index.
%
-spec choose_numbered_item_with_default( [ choice_element() ], choice_index(),
										 ui_state() ) -> choice_index().
choose_numbered_item_with_default( Choices, DefaultChoiceIndex, UIState ) ->

	Label = text_utils:format( "Select among these ~B choices:",
								[ length( Choices ) ] ),

	choose_numbered_item_with_default( Label, Choices, DefaultChoiceIndex,
									   UIState ).




% Selects, using the specified label and default item, an item among the
% specified ones, and returns its index.
%
-spec choose_numbered_item_with_default( label(), [ choice_element() ],
			maybe( choice_index() ), ui_state() ) -> choice_index().
choose_numbered_item_with_default( Label, Choices, DefaultChoiceIndex, UIState ) ->

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

	SelectedNumber = get_text_as_maybe_integer( FullLabel, UIState ),

	%trace_utils:format( "Selected: ~p", [ SelectedNumber ] ),

	case SelectedNumber of

		undefined ->
			DefaultChoiceIndex;

		N when N < 1 ->
			display_error( "Specified choice shall be at least 1 (not ~B).", [ N ],
						   UIState ),
			throw( { invalid_choice, too_low, N } );

		N when N > ChoiceCount ->
			display_error( "Specified choice shall not be greater than ~B (not ~B).",
						   [ ChoiceCount, N ], UIState ),
			throw( { invalid_choice, too_high, N } );

		N ->
			N

	end.



% Traces specified status string.
%
-spec trace( string(), ui_state() ) -> ui_state().
trace( Text, UIState ) ->
	display( "[trace] " ++ Text ++ "\n", UIState ).




% Stops the UI.
%
-spec stop( ui_state() ) -> void().
stop( _UIState=#ui_state{ log_file=undefined } ) ->
	ok;

stop( _UIState=#ui_state{ log_file=LogFile } ) ->
	file_utils:write( LogFile, "Stopping UI.\n" ),
	file_utils:close( LogFile ).
