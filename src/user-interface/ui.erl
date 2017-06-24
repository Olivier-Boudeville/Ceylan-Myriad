% Copyright (C) 2016-2017 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Monday, February 15, 2010.


% Gathering of various facilities for User Interfaces (UI or not).
%
% The base interface proposed here is purely textual.
%
% See ui_test.erl for the corresponding test.
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
-export([ start/0, start/1, display/2, display/3, display_numbered_list/3,
		  trace/2, stop/1 ]).

-record( ui_state, {

		   log_console = false :: boolean(),
		   log_file = undefined :: 'undefined' | file_utils:file()

}).

-type ui_state() :: #ui_state{}.

-type ui_options() :: [ any() ].


% Index of a choice, starting at 1:
-type choice_index() :: pos_integer().


-export_type([ ui_state/0, ui_options/0, choice_index/0 ]).


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
	start( T, NewState ).


% Displays specified string.
%
-spec display( string(), ui_state() ) -> ui_state().
display( Text, UIState ) ->
	display( Text, _Values=[], UIState ).



% Displays specified format string.
%
-spec display( text_utils:format_string(), [ term() ], ui_state() ) ->
					 ui_state().
display( FormatString, Values, UIState ) ->
	io:format( FormatString, Values ),
	UIState.



% Displays in-order the items of specified list and returns the index (starting
% at 1) of the user-selected one.
%
-spec display_numbered_list( string(), [ string() ], ui_state() ) -> 
								   choice_index().
display_numbered_list( Label, Choices, UIState ) ->
	StringItems = text_utils:strings_to_enumerated_string( Choices ),
	display( Label ++ "\n" ++ StringItems, UIState ).



% Traces specified status string.
%
-spec trace( string(), ui_state() ) -> ui_state().
trace( Text, UIState ) ->
	display( "[trace] " ++ Text ++ "\n", UIState ).


% Stops the UI.
%
-spec stop( ui_state() ) -> basic_utils:void().
stop( _UIState=#ui_state{ log_file=undefined } ) ->
	ok;

stop( _UIState=#ui_state{ log_file=LogFile } ) ->
	file_utils:write( LogFile, "Stopping UI.\n" ),
	file_utils:close( LogFile ).
