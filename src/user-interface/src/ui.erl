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
% Creation date: Saturday, May 5, 2018


% Aggregates all code transverse to UI backends.
%
% See:
%
% - text_ui_test.erl for the corresponding test
% - term_ui.erl for a more advanced text interface
% - gui.erl for a graphical counterpart
%
% See also: trace_utils.erl for another kind of output.
%
-module(ui).




% Usage notes:
%
% By default, this module will do its best to select the most suitable backend,
% i.e. the most advanced among the available ones.
%
% One may select, from the command-line, a given backend thanks to the following
% option: --use-ui-backend BACKEND_NAME, where BACKEND_NAME is the name of the
% associated backend (ex: text_ui, term_ui or gui).
%
% See examples like merge-tree.escript (merge_utils.erl) allowing the user to
% override the actual backend.

% Note that the actual option starts with one extra dash (thus two):
-define( ui_backend_opt, '-use-ui-backend' ).


% Implementation notes:
%
% Each backend is to store its current state into a specific state record (ex:
% term_ui_state()), kept under a backend-specific key (see ui_name_key) in the
% process dictionary.
%
% Among the fields of these backend records, one is the settings table (see the
% setting_table()). It allows the developer to specify all kinds of settings
% (ex: default window size), which may or may not be accommodated by a given
% backend.


-include("ui.hrl").


-export([ start/0, start/1, start/2 ]).


% Directly forwarded section:
%
-export([ set/1, set/2, unset/1,

		  display/1, display/2,

		  display_numbered_list/2,

		  display_error/1, display_error/2,

		  display_error_numbered_list/2,

		  add_separation/0,

		  get_text/2, get_text_as_integer/2, get_text_as_maybe_integer/2,
		  read_text_as_integer/2, read_text_as_maybe_integer/2,

		  ask_yes_no/2,

		  choose_designated_item/1, choose_designated_item/2,
		  choose_designated_item/3,

		  choose_numbered_item/1, choose_numbered_item/2,
		  choose_numbered_item/3,

		  choose_numbered_item_with_default/2,
		  choose_numbered_item_with_default/3,
		  choose_numbered_item_with_default/4,

		  set_setting/2, set_setting/3,
		  set_settings/1, set_settings/2,

		  unset_setting/1, unset_setting/2,
		  get_setting/1

]).


-export([ trace/1, trace/2,

		  clear/0,

		  stop/0,

		  settings_to_string/1 ]).


% Typically text_ui_state() | term_ui_state() | ...
%
-type ui_state() :: any().



% Starts the UI with default settings.
%
-spec start() -> void().
start() ->
	start( _Opts=[] ).



% Starts the UI with specified settings, and returns the command-line arguments
% expurged from any UI-related option (as an argument table).
%
% Stores the corresponding state in the process dictionary.
%
-spec start( ui_options() ) -> executable_utils:argument_table().
start( Options ) ->

	% Here, no argument table is specified, fetching it (thus supposedly not
	% running as an escript):
	%
	start( Options, executable_utils:get_argument_table() ).



% Starts the UI with specified settings, and returns the command-line arguments
% expurged from any UI-related option (as an argument table).
%
% Stores the corresponding state in the process dictionary.
%
-spec start( ui_options(), executable_utils:argument_table() ) ->
				   executable_utils:argument_table().
start( Options, ArgumentTable ) ->

	% Just a check:
	case process_dictionary:get( ?ui_name_key ) of

		undefined ->
			ok;

		AlreadyUI ->
			throw( { ui_already_started, AlreadyUI } )

	end,

	% With the leading dash removed:
	OptName = ?ui_backend_opt,

	{ BackendModuleName, RemainingArgTable } =
		case executable_utils:extract_command_argument( OptName,
														ArgumentTable ) of

		{ [], ArgTable } ->
			% No backend specified, determining it:
			{ get_best_ui_backend(), ArgTable };

		{ [ BackendName ], OtherArgTable } ->

			trace_utils:debug_fmt( "Following backend was specified: '~s'.",
								   [ BackendName ] ),

			BackendModName = text_utils:string_to_atom( BackendName ),

			case code_utils:is_beam_in_path( BackendModName ) of

				not_found ->
					trace_utils:error_fmt( "No BEAM file found in code path "
										   "for user-specified UI backend module "
										   "'~s'.", [ BackendModName ] ),
					throw( { ui_backend_module_not_found, BackendModName } );

				[ SinglePath ] ->
					trace_utils:debug_fmt( "UI backend module found as '~s'.",
										   [ SinglePath ] ),
					{ BackendModName, OtherArgTable };

				MultiplePaths ->
					throw( { multiple_ui_backend_modules_found, MultiplePaths } )

			end;

		{ OtherValues, _OtherArgTable } ->
			throw( { invalid_ui_options, OtherValues } )


	end,

	trace_utils:debug_fmt( "The '~s' backend module has been selected.",
								   [ BackendModuleName ] ),

	% Expects this backend to register its name and state in the process
	% dictionary:
	%
	BackendModuleName:start( Options ),

	% Pass along the unexploited command-line arguments:
	RemainingArgTable.




% Directly forwarded to the backend section:
% (a parse transform could help)


% Sets specified UI setting.
%
-spec set( ui_setting_key(), ui_setting_value() ) -> void().
set( SettingKey, SettingValue ) ->

	UIModule = get_backend_name(),

	UIModule:set( SettingKey, SettingValue ).


% Sets specified UI settings.
%
-spec set( [ ui_setting_entry() ] ) -> void().
set( SettingEntries ) ->

	UIModule = get_backend_name(),

	UIModule:set( SettingEntries ).



% Unsets specified UI setting.
%
-spec unset( [ ui_setting_key() ] | ui_setting_key() ) -> void().
unset( SettingElement ) ->

	UIModule = get_backend_name(),

	UIModule:unset( SettingElement ).



% Displays specified text, as a normal message.
%
% Note: all types of quotes are allowed in specified text.
%
-spec display( text() ) -> void().
display( Text ) ->

	UIModule = get_backend_name(),

	UIModule:display( Text ).



% Displays specified formatted text, as a normal message.
%
-spec display( text_utils:format_string(), [ term() ] ) -> void().
display( FormatString, Values ) ->

	UIModule = get_backend_name(),

	UIModule:display( FormatString, Values ).


% Displays in-order the items of specified list, as a normal message.
%
-spec display_numbered_list( label(), [ text() ] ) -> void().
display_numbered_list( Label, Lines ) ->

	UIModule = get_backend_name(),

	UIModule:display_numbered_list( Label, Lines ).




% Displays specified text, as an error message.
%
-spec display_error( text() ) -> void().
display_error( Text ) ->

	UIModule = get_backend_name(),

	UIModule:display_error( Text ).



% Displays specified formatted text, as an error message.
%
-spec display_error( text_utils:format_string(), [ term() ] ) -> void().
display_error( FormatString, Values ) ->

	UIModule = get_backend_name(),

	UIModule:display_error( FormatString, Values ).



% Displays in-order the items of specified list, as an error message.
%
-spec display_error_numbered_list( label(), [ text() ] ) -> void().
display_error_numbered_list( Label, Lines ) ->

	UIModule = get_backend_name(),

	UIModule:display_error_numbered_list( Label, Lines ).



% Adds a default separation between previous and next content.
%
-spec add_separation() -> void().
add_separation() ->

	UIModule = get_backend_name(),

	UIModule:add_separation().




% Returns the user-entered text.
%
% (const)
%
-spec get_text( prompt(), ui_state() ) -> text().
get_text( Prompt, UIState ) ->

	UIModule = get_backend_name(),

	UIModule:get_text( Prompt, UIState ).



% Returns the user-entered text, once translated to an integer, based on an
% implicit state.
%
% (const)
%
-spec get_text_as_integer( prompt(), ui_state() ) -> text().
get_text_as_integer( Prompt, UIState ) ->

	UIModule = get_backend_name(),

	UIModule:get_text_as_integer( Prompt, UIState ).



% Returns the user-entered text, once translated to an integer, based on an
% implicit state, prompting the user until a valid input is obtained.
%
% (const)
%
-spec read_text_as_integer( prompt(), ui_state() ) -> text().
read_text_as_integer( Prompt, UIState ) ->

	UIModule = get_backend_name(),

	UIModule:read_text_as_integer( Prompt, UIState ).



% Returns the user-entered text (if any), once translated to an integer.
%
% (const)
%
-spec get_text_as_maybe_integer( prompt(), ui_state() ) -> maybe( text() ).
get_text_as_maybe_integer( Prompt, UIState ) ->

	UIModule = get_backend_name(),

	UIModule:get_text_as_maybe_integer( Prompt, UIState ).



% Returns the user-entered text, once translated to an integer, prompting the
% user until a valid input is obtained: either a string that resolves to an
% (then returned) integer, or an empty string (then returning 'undefined').
%
% (const)
%
-spec read_text_as_maybe_integer( prompt(), ui_state() ) -> maybe( text() ).
read_text_as_maybe_integer( Prompt, UIState ) ->

	UIModule = get_backend_name(),

	UIModule:read_text_as_maybe_integer( Prompt, UIState ).



% Displays specified prompt, let the user choose between two options, "yes" and
% "no" (with specified default option), and returns that choice.
%
-spec ask_yes_no( prompt(), binary_choice() ) -> binary_choice().
ask_yes_no( Prompt, BinaryDefault ) ->

	UIModule = get_backend_name(),

	UIModule:ask_yes_no( Prompt, BinaryDefault ).



% Selects, using a default prompt, an item among the specified ones (comprising,
% for each, an internal designator and a text), and returns its designator.
%
% (const)
%
-spec choose_designated_item( [ choice_element() ] ) -> choice_designator().
choose_designated_item( Choices ) ->

	UIModule = get_backend_name(),

	UIModule:choose_designated_item( Choices ).



% Selects, using specified prompt, an item among the specified ones (comprising,
% for each, an internal designator and a text), and returns its designator.
%
% (const)
%
choose_designated_item( Label, Choices ) ->

	UIModule = get_backend_name(),

	UIModule:choose_designated_item( Label, Choices ).



% Selects, based on an explicit state, using the specified label, an item among
% the specified ones (comprising, for each, an internal designator and a text),
% and returns its designator.
%
% (const)
%
-spec choose_designated_item( label(), [ choice_element() ], ui_state() ) ->
									choice_designator().
choose_designated_item( Label, Choices, UIState ) ->

	UIModule = get_backend_name(),

	UIModule:choose_designated_item( Label, Choices, UIState ).



% Selects, based on an implicit state, using a default label, an item among the
% specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
-spec choose_numbered_item( [ choice_element() ] ) ->  choice_index().
choose_numbered_item( Choices ) ->

	UIModule = get_backend_name(),

	UIModule:choose_numbered_item( Choices ).



% Selects, based on an explicit state, using a default label, an item among the
% specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
% Selects, based on an implicit state, using the specified label, an item among
% the specified ones, and returns its index.
%
-spec choose_numbered_item( [ choice_element() ], ui_state() ) ->
								  choice_index();
						  ( label(), [ choice_element() ] ) -> choice_index().
choose_numbered_item( Choices, UIState ) ->

	UIModule = get_backend_name(),

	UIModule:choose_numbered_item( Choices, UIState ).


% Selects, based on an explicit state, using the specified label, an item among
% the specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
-spec choose_numbered_item( label(), [ choice_element() ], ui_state() ) ->
								  choice_index().
choose_numbered_item( Label, Choices, UIState ) ->

	UIModule = get_backend_name(),

	UIModule:choose_numbered_item( Label, Choices, UIState ).


% Selects, based on an implicit state, using a default label, an item among the
% specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
-spec choose_numbered_item_with_default( [ choice_element() ],
										 choice_index() ) -> choice_index().
choose_numbered_item_with_default( Choices, DefaultChoiceIndex ) ->

	UIModule = get_backend_name(),

	UIModule:choose_numbered_item_with_default( Choices, DefaultChoiceIndex ).



% Selects, based on an explicit state, using a default label, an item among the
% specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
% Selects, based on an implicit state, using the specified label and default
% item, an item among the specified ones, and returns its index.
%
-spec choose_numbered_item_with_default( [ choice_element() ], choice_index(),
										 ui_state() ) -> choice_index();
									   ( label(), [ choice_element() ],
										 maybe( choice_index() ) ) ->
											   choice_index().
choose_numbered_item_with_default( Choices, DefaultChoiceIndex, UIState ) ->

	UIModule = get_backend_name(),

	UIModule:choose_numbered_item_with_default( Choices, DefaultChoiceIndex,
												UIState ).



% Selects, based on an explicit state, using the specified label and default
% item, an item among the specified ones (specified as direct text, with no
% specific designator provided), and returns its index.
%
-spec choose_numbered_item_with_default( label(), [ choice_element() ],
			maybe( choice_index() ), ui_state() ) -> choice_index().
choose_numbered_item_with_default( Label, Choices, DefaultChoiceIndex,
								   UIState ) ->

	UIModule = get_backend_name(),

	UIModule:choose_numbered_item_with_default( Label, Choices,
												DefaultChoiceIndex, UIState ).



% Sets the specified setting to specified value, in the (implicit) UI state.
%
-spec set_setting( ui_setting_key(), ui_setting_value() ) -> void().
set_setting( SettingKey, SettingValue ) ->

	UIModule = get_backend_name(),

	UIModule:set_setting( SettingKey, SettingValue ).



% Sets the specified setting to specified value, in the specified UI state.
%
-spec set_setting( ui_setting_key(), ui_setting_value(), ui_state() ) ->
						 ui_state().
set_setting( SettingKey, SettingValue, UIState ) ->

	UIModule = get_backend_name(),

	UIModule:set_setting( SettingKey, SettingValue, UIState ).



% Sets the specified settings to specified values, in the (implicit) UI state.
%
-spec set_settings( [ ui_setting_entry() ] ) -> void().
set_settings( SettingEntries ) ->

	UIModule = get_backend_name(),

	UIModule:set_settings( SettingEntries ).



% Sets the specified settings to specified values, in the specified UI state.
%
-spec set_settings( [ ui_setting_entry() ], ui_state() ) -> ui_state().
set_settings( SettingEntries, UIState ) ->

	UIModule = get_backend_name(),

	UIModule:set_settings( SettingEntries, UIState ).



% Unsets specified setting, in the (implicit) UI state.
%
-spec unset_setting( ui_setting_key() ) -> void().
unset_setting( SettingKey ) ->

	UIModule = get_backend_name(),

	UIModule:unset_setting( SettingKey ).



% Unsets specified setting, in the specified UI state.
%
-spec unset_setting( ui_setting_key() , ui_state()) -> void().
unset_setting( SettingKey, UIState ) ->

	UIModule = get_backend_name(),

	UIModule:unset_setting( SettingKey, UIState ).



% Returns the value (if any) associated, in the (implicit) UI state, to the
% specified setting.
%
-spec get_setting( ui_setting_key() ) -> maybe( ui_setting_value() ).
get_setting( SettingKey ) ->

	UIModule = get_backend_name(),

	UIModule:get_setting( SettingKey ).



% End of forward section.



% Traces specified status string, by displaying it, and possibly logging it.
%
-spec trace( string() ) -> void().
trace( Message ) ->

	UIModule = get_backend_name(),

	UIModule:trace( Message ).


% Displays and logs specified formatted text.
%
-spec trace( text_utils:format_string(), [ term() ] ) -> void().
trace( FormatString, Values ) ->

	UIModule = get_backend_name(),

	UIModule:trace( FormatString, Values ).



% Clears the interface.
%
-spec clear() -> void().
clear() ->

	UIModule = get_backend_name(),

	UIModule:clear().



% Stops the UI.
%
-spec stop() -> void().
stop() ->
	UIModuleName = get_backend_name(),
	UIModuleName:stop().



% Returns the module name of the current UI backend.
%
% (helper)
%
-spec get_backend_name() -> basic_utils:module_name().
get_backend_name() ->

	case process_dictionary:get( ?ui_name_key ) of

		undefined ->
			throw( no_backend_started );

		ModName ->
			ModName

	end.



% Returns the most suitable UI backend found.
%
-spec get_best_ui_backend() -> basic_utils:module_name().
get_best_ui_backend() ->

	case gui:is_available() of

		true ->
			gui;

		false ->
			case term_ui:is_available() of

				true ->
					term_ui;

				false ->
					text_ui

			end

	end.



% Returns a textual description of specified setting table.
%
-spec settings_to_string( setting_table() ) -> text_utils:ustring().
settings_to_string( SettingTable ) ->
	case ?ui_table:size( SettingTable ) of

		0 ->
			"having no setting recorded";

		Count ->

			SetPairs = lists:sort( ?ui_table:enumerate( SettingTable ) ),

			SetStrings = [ text_utils:format( "'~s' set to ~p",
									  [ K, V ] ) || { K, V } <- SetPairs ],

			text_utils:format( "with ~B settings recorded: ~s",
					   [ Count, text_utils:strings_to_string( SetStrings ) ] )

	end.
