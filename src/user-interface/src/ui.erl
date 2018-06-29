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


-export([ start/0, start/1,

		  set/1, set/2, unset/1,

		  display/1,

		  trace/1, trace/2,

		  stop/0,

		  settings_to_string/1 ]).



% Starts the UI with default settings.
%
-spec start() -> void().
start() ->
	start( _Opts=[] ).



% Starts the UI with specified settings, and returns the command-line arguments
% expurged from any UI-related option.
%
% Stores the corresponding state in the process dictionary.
%
-spec start( ui_options() ) -> [ executable_utils:command_line_argument() ].
start( Options ) ->

	% Just a check:
	case process_dictionary:get( ?ui_name_key ) of

		undefined ->
			ok;

		AlreadyUI ->
			throw( { ui_already_started, AlreadyUI } )

	end,

	% With the leading dash removed:
	OptName = ?ui_backend_opt,

	{ BackendModuleName, RemainingArgs } =
		case executable_utils:extract_command_argument( OptName ) of

		{ [], Args } ->
			% No backend specified, determining it:
			{ get_best_ui_backend(), Args };

		{ [ BackendName ], OtherArgs } ->

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
					{ BackendModName, OtherArgs };

				MultiplePaths ->
					throw( { multiple_ui_backend_modules_found, MultiplePaths } )

			end;

		{ OtherValues, _OtherArgs } ->
			throw( { invalid_ui_options, OtherValues } )


	end,

	trace_utils:debug_fmt( "The '~s' backend module has been selected.",
								   [ BackendModuleName ] ),

	% Expects this backend to register its name and state in the process
	% dictionary:
	%
	BackendModuleName:start( Options ),

	% Pass along the unexploited command-line arguments:
	RemainingArgs.



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
-spec display( text() ) -> void().
display( Text ) ->

	UIModule = get_backend_name(),

	UIModule:display( Text ).



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
-spec settings_to_string( setting_table() ) -> text_utils:string().
settings_to_string( SettingTable ) ->
	case ?ui_table:size( SettingTable ) of

		0 ->
			"having no setting recorded";

		Count ->
			text_utils:format( "with ~B settings recorded: ~p",
							   [ Count, ?ui_table:enumerate( SettingTable ) ] )

	end.
