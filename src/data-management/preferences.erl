% Copyright (C) 2013-2017 Olivier Boudeville
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
% Creation date: Thursday, October 31, 2013
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)



% Service dedicated to the management of user-defined preferences.
%
% A preferences element is designated by a key (an atom), associated to a value
% (that can be any term).
%
% Preferences can be stored in file(s).
%
% This is typically a way of storing durable information in one's user account
% in a transverse way compared to programs and versions thereof, and of sharing
% them conveniently (ex: for passwords, settings).
%
% The format of preferences is a series of Erlang terms as strings, separated by
% dots (i.e. the format understood by file:consult/1).
%
% Example of content of a preference file:
% """
% { my_first_color, red }.
% { myheight, 1.80 }.
% """
%
% (of course without the quotes and the leading ampersands)
%
-module(preferences).


-export([ start/0, start/1, get/1, get/2, set/2, set/3, to_string/0,
		  to_string/1, get_default_preferences_path/0,
		  is_preferences_default_file_available/0,
		  check_preferences_default_file/0,
		  stop/0, stop/1 ]).


-type registration_name() :: atom(). % Private type


-type key() :: atom().


% Can be 'undefined' (no difference between a non-registered key and a key
% registered to 'undefined'):
%
-type value() :: table:value().


-type entry() :: table:entry().
-type entries() :: table:entries().


-export_type([ key/0, value/0, entry/0, entries/0 ]).



% Implementation notes:
%
% Preferences are managed through a singleton, globally registered process,
% maintaining an associative table whose content can be defined programmatically
% and/or thanks to data files.

% There is a potential race condition for the starting of this service: a
% process could trigger its creation while its creation is in progress, due to
% an earlier trigger.



% Default name for global registration:
%-define( default_preferences_server_name, ceylan_preferences_server ).


% Name of the default preferences file (searched at the root of the user
% account):
-define( default_preferences_filename, ".ceylan-erlang-preferences.txt" ).



% Ensures that, if not done already, the preferences service is started and
% initialised immediately, if wanting an explicit start rather than one implied
% by the use of an operation onto it.
%
% Returns in any case the PID of the corresponding preferences server.
%
-spec start() -> pid().
start() ->
	start( get_default_preferences_path() ).

-spec start( file_utils:file_name() ) -> pid().
start( FileName ) ->

	RegistrationName = get_registration_name( FileName ),

	case naming_utils:is_registered( RegistrationName, global ) of

		not_registered ->

			% A goal is to acquire the "lock" (the global name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			% No sensible link to be created here, so we must beware of a silent
			% crash of this server:
			%
			spawn( fun() -> server_main_run( CallerPid, RegistrationName,
											 FileName )
				   end ),

			receive

				{ preference_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end.



% Returns the value associated to specified key in the preferences (if any),
% otherwise 'undefined'.
%
-spec get( key() ) -> value().
get( Key ) ->
	get( Key, get_default_preferences_path() ).

-spec get( key(), file_utils:file_name() ) -> value().
get( Key, FileName ) ->

	ServerPid = start( FileName ),

	ServerPid ! { get_preference, Key, self() },

	receive

		{ notify_preference, V } ->
			V

	end.



% Associates, in current preferences, specified value to specified key (possibly
% overwriting any previous value).
%
-spec set( key(), value() ) -> basic_utils:void().
set( Key, Value ) ->
	set( Key, Value, get_default_preferences_path() ).

-spec set( key(), value(), file_utils:file_name() ) -> basic_utils:void().
set( Key, Value, FileName ) ->

	ServerPid = start( FileName ),

	ServerPid ! { set_preference, Key, Value }.




%  Returns a textual description of the preferences server (if any).
%
-spec to_string() -> string().
to_string() ->
	to_string( get_default_preferences_path() ).

-spec to_string( file_utils:file_name() ) -> string().
to_string( FileName ) ->

	RegistrationName = get_registration_name( FileName ),

	case naming_utils:is_registered( RegistrationName, global ) of

		not_registered ->
			"no preferences server is running";

		ServerPid ->
			ServerPid ! { to_string, self() },

			receive

				{ notify_preferences_status, PrefString } ->
					PrefString

			end

	end.



% Returns the full, absolute path to the default preferences filename.
%
-spec get_default_preferences_path() -> file_utils:path().
get_default_preferences_path() ->
	file_utils:join( system_utils:get_user_home_directory(),
					 ?default_preferences_filename ).



% Returns the automatic naming used for registering the process (deduced from
% the preferences filename).
%
-spec get_registration_name( file_utils:file_name() ) -> registration_name().
get_registration_name( FilePath ) ->
	CoreFileName = file_utils:remove_upper_levels_and_extension( FilePath ),
	RegistrationName = file_utils:path_to_variable_name( CoreFileName, "" ),
	text_utils:string_to_atom( RegistrationName ).



% Returns whether the default preferences file is available and its full path.
%
-spec is_preferences_default_file_available() ->
			{ boolean(), file_utils:path() }.
is_preferences_default_file_available() ->

	PrefFile = get_default_preferences_path(),

	Res = file_utils:is_existing_file_or_link( PrefFile ),

	{ Res, PrefFile }.



% Checks that the default preferences file exists, throws an exception
% otherwise.
%
-spec check_preferences_default_file() -> basic_utils:void().
check_preferences_default_file() ->

	case is_preferences_default_file_available() of

		{ true, _FilePath } ->
			ok;

		{ false, FilePath } ->
			throw( { no_default_preferences_file_found, FilePath } )

	end.



% Stops (asynchronously) the preferences server, if it is running.
%
% Never fails.
%
-spec stop() -> basic_utils:void().
stop() ->
	stop( get_default_preferences_path() ).


-spec stop( file_utils:file_name() ) -> basic_utils:void().
stop( FileName ) ->

	RegistrationName = get_registration_name( FileName ),

	case naming_utils:is_registered( RegistrationName, global ) of

		not_registered ->
			ok;

		Pid ->
			Pid ! stop

	end.



% Section for the preferences server itself.


% Launcher of the preference server.
%
server_main_run( SpawnerPid, RegistrationName, FileName ) ->

	case naming_utils:register_or_return_registered( RegistrationName,
													global_only ) of

		registered ->

			% We gain the shared name, we are the one and only server:
			EmptyTable = table:new(),

			FinalTable = case file_utils:is_existing_file_or_link( FileName ) of

				true ->
					add_preferences_from( FileName, EmptyTable );

				false ->
					io:format( "No preferences file found "
							   "(searched for '~s').~n", [ FileName ] ),
					EmptyTable

			end,

			% Spawner could already know that PID in this case:
			SpawnerPid ! { preference_server_pid, self() },

			% Never returns:
			server_main_loop( FinalTable );


		ServerPid ->
			% Notifies and terminates:
			SpawnerPid ! { preference_server_pid, ServerPid }

	end.



% Main loop of the preferences server.
%
server_main_loop( Table ) ->

	%io:format( "Waiting for preferences-related request, "
	%			"having ~B recorded preferences.~n",
	%			[ table:getEntryCount( Table ) ] ),

	receive

		{ get_preference, Key, SenderPid } ->

			Answer = case table:lookupEntry( Key, Table ) of

				key_not_found ->
					undefined;

				{ value, V } ->
					V

			end,

			SenderPid ! { notify_preference, Answer },

			server_main_loop( Table );


		{ set_preference, Key, Value } ->

			NewTable = table:addEntry( Key, Value, Table ),

			server_main_loop( NewTable );


		{ to_string, SenderPid } ->

			Res = case table:enumerate( Table ) of

				[] ->
					"no preferences recorded";

				L ->

					% Enforces a consistent order:
					Strings = [ io_lib:format( "~p: ~p", [ K, V ] )
								|| { K, V } <- lists:sort( L ) ],

					io_lib:format( "~B preferences recorded:~s~n",
								   [ length( L ),
								 text_utils:string_list_to_string( Strings ) ] )

			end,

			SenderPid ! { notify_preferences_status, Res },

			server_main_loop( Table );

		stop ->
			%io:format( "Stopping preferences server.~n" ),
			stopped

	end.



% Helper functions.


% Adds preferences found in specified file into specified table, and returns it.
%
% (helper)
%
add_preferences_from( Filename, Table ) ->

	case file:consult( Filename ) of

		{ ok, Entries } ->

			case check_entries( Entries ) of

				ok ->
					NewTable = table:addEntries( Entries, Table ),
					%io:format( "Loaded from preferences file '~s' "
					%           "following entries:~s",
					% [ PrefFilename, table:toString( NewTable ) ] ),
				   %io:format( "Preferences file '~s' loaded.~n",
				   %	[ Filename ] ),
				   NewTable;

				ErrorString ->
					io:format( "Error when reading preferences file '~s' (~s), "
							   "no preferences read.~n",
							   [ Filename, ErrorString ] ),
					Table

			end;


		{ error, { Line, _Mod, Term } } ->
			FlattenError = io_lib:format( "~p", [ Term ] ),
			io:format( "Error in preferences file '~s' at line ~B (~s), "
					   "no preferences read.~n",
					   [ Filename, Line, FlattenError ] ),
			Table;


		{ error, Reason } ->
			io:format( "Error when reading preferences file '~s' (~p), "
					   "no preferences read.~n", [ Filename, Reason ] ),
			Table

	end.



% Checks specified entries.
%
check_entries( _Entries=[] ) ->
	ok;

check_entries( _Entries=[ { K, _V } | T ] ) when is_atom( K ) ->
	check_entries( T );

check_entries( _Entries=[ { K, _V } | _T ] ) ->
	io_lib:format( "key '~p' is not an atom", [ K ] );

check_entries( _Entries=[ E | _T ] ) ->
	io_lib:format( "entry '~p' is not a key/value pair", [ E ] ).
