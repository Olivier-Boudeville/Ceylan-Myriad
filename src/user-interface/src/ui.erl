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
% By default, this module will do its best to selec the most suitable backend,
% i.e. the most advanced among the available ones.
%
% One may select, from the command-line, a given backend thanks to the following
% option: --use-ui-backend BACKEND_NAME, where BACKEND_NAME is the name of the
% associated backend (ex: text_ui, term_ui or gui).



% Implementation notes:
%


-include("ui.hrl").


-export([ start/0, start/1,
		  stop/0 ]).



% Starts the UI with default settings.
%
-spec start() -> void().
start() ->
	start( _Opts=[] ).



% Starts the UI with specified settings.
%
% Stores the corresponding state in the process dictionary, yet returns as well
% that state, for any explicit later operation.
%
-spec start( ui_options() ) -> void().
start( Options ) ->

	% Just a check:
	case process_dictionary:get( ?ui_name_key ) of

		undefined ->
			ok;

		AlreadyUI ->
			throw( { ui_already_started, AlreadyUI } )

	end,

	% With the leading dash removed:
	OptName ='-use-ui-backend',

	BackendModuleName = case init:get_argument( OptName ) of

		{ ok, [ [ BackendName ] ] } when is_list( BackendName ) ->
			trace_utils:debug_fmt( "Following backend was specified: '~s'.",
								   [ BackendName ] ),

			BackendModName = text_utils:string_to_atom( BackendName ),

			case code_utils:is_beam_in_path( BackendModName ) of

				not_found ->
					trace_utils:error_fmt( "No BEAM file found in code path "
										   "for user-specified UI module "
										   "'~s'.", [ BackendModName ] ),
					throw( { ui_module_not_found, BackendModName } );

				[ SinglePath ] ->
					trace_utils:debug_fmt( "UI module found as '~s'.",
										   [ SinglePath ] ),
					BackendModName;

				MultiplePaths ->
					throw( { multiple_ui_module_found, MultiplePaths } )

			end;

		{ ok, InvalidOpts } ->
			throw( { invalid_ui_options, InvalidOpts } );


		error ->
			% No backend specified, determining it:
			get_best_ui_backend()

	end,

	trace_utils:debug_fmt( "The '~s' backend has been selected.",
								   [ BackendModuleName ] ),

	% Expects this backend to register its name and state in the process
	% dictionary:
	%
	BackendModuleName:start( Options ).



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
