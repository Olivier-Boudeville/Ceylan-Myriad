% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Sunday, February 27, 2022.


% @doc Service dedicated to the <b>management of application environments</b>.
%
% An application environment is a server-like process that stores static or
% dynamic information (possibly initialised from an ETF file), as key/value
% entries (not unlike an ETS table), on behalf of an application or of a subset
% of its components, and make it available to client processes.
%
% An environment entry is designated by a key (an atom), associated to a value
% (that can be any term). No difference is made between a non-registered key and
% a key registered to 'undefined'.
%
% Environments hold application-specific or component-specific data, obtained
% from any source (file included) - they may also start blank and be exclusively
% fed by the application or the components. Environments are used afterwards to
% maintain these pieces of data (read/write), before possibly storing them on
% file at application exit or component stop.
%
% As a whole, an environment server can be seen as a process holding state
% information meant to be potentially common to various processes of a given
% application or component.
%
% Environment data can be read from or written to file(s) in the ETF format,
% hence as a series of Erlang terms as strings, each ended with a dot (i.e. it
% is the basic, standard format understood by `file:consult/1').
%
% Example of content of an environment file:
% ```
% {my_first_color, red}.
% {myHeight, 1.80}.
% {'My name', "Sylvester the cat"}.
% '''
%
% The server process corresponding to an environment is locally registered; as a
% consequence it can be designated either directly through its PID or through
% its conventional (atom) registration name (ex:
% `environment:get(my_first_color, foobar_env_server'). No specific global
% registration of that server is made here.
%
% A (single) explicit start (with one of the start* functions) shall be
% preferred to implicit ones (typically directly thanks to the get* functions)
% to avoid any risk of race conditions (should multiple processes attempt
% concurrently to create the same environment server), and also to be able to
% request that it is also linked to the calling process.
%
% For faster accesses (not involving any inter-process messaging), and if
% considering that their changes are rather infrequent (or never happening), at
% least some entries managed by an environment server may be cached directly in
% client processes.
%
% In this case the process dictionary of these clients is used, and when
% updating from a client process a cached key, the corresponding environment
% server is updated in turn. However any other client process caching that key
% will not be aware of this change until it requests an update to this
% environment server.
%
% A specific case of environment corresponds to the user preferences. See our
% preferences module for that.
%
% Refer to https://myriad.esperide.org/#etf for more information.
%
-module(environment).


-export([ start/1, start/2, start_link/1, start_link/2,
		  get_server/1,
		  get/2, get/3, set/2, set/3, set/4,
		  cache/2, sync/1, store/1, store/2,
		  to_string/1,
		  stop/1 ]).


-type environment_pid() :: pid().
% The PID of an environment server.

-type environment_designator() :: environment_pid() | registration_name().
% The two standard ways according to which an environment server can be
% designated: either directly thanks to its PID or to the name under which it is
% locally registered.


-type key() :: atom().


-type value() :: table:value().
% Can be 'undefined' (no difference between a non-registered key and a key
% registered to 'undefined').


-type entry() :: table:entry().
-type entries() :: table:entries().


-export_type([ environment_pid/0, environment_designator/0,
			   key/0, value/0, entry/0, entries/0 ]).


% For myriad_spawn*:
-include("spawn_utils.hrl").


% The name of the key in the process dictionary corresponding to environment
% information, notably for caching:
%
-define( environment_dictionary_key, 'myriad_environment_cache' ).



% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type registration_name() :: naming_utils:registration_name().
% The name under which an environment  server can be (locally) registered.

-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().
-type any_file_path() :: file_utils:any_file_path().



% Implementation notes:
%
% Each environment server is managed through a singleton, locally registered
% process, maintaining an associative table whose content can be defined
% programmatically and/or thanks to data files.

% There is a slight potential race condition with the implicit starting of this
% service: a process could trigger its creation while its creation is in
% progress, due to an earlier trigger.

% Some accessors accept both a registration name and a file path, whereas the
% former could be deduced from the latter. The idea is to avoid unnecessary
% conversions on potentially frequent operations.


% When using caching, the corresponding cached entries for a given environment
% will be stored in the process dictionary (of the client process using that
% feature), under the dictionary key designated by the
% environment_dictionary_key define.
%
% The value associated to this dictionary key is a AllEnvTable table associating
% to each cached environment (designated by the PID of its server) the table of
% its cached entries, i.e. AllEnvTable :: list_table(EnvPid, EnvCacheTable ::
% table()); AllEnvTable is a list_table as it is expected to contain only very
% few entries, while EnvCacheTable may cache many entries and thus is a table().



% @doc Ensures explicitly that, if not running already, a specified environment
% server is started.
%
% If a name is specified: if no server already registered it, starts it with a
% blank state.
%
% If instead a filename is specified: if a server with a deriving name is not
% already registered, starts a corresponding server and initialises it with the
% corresponding file content.
%
% Does not link any started environment server to the calling process.
%
% Returns in any case the PID of the corresponding environment server, already
% existing or not, blank or not.
%
-spec start( registration_name() | file_path() ) -> environment_pid().
start( ServerName ) when is_atom( ServerName ) ->
	case naming_utils:is_registered( ServerName, local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			% No link to be created here, so one must beware of a silent crash
			% of this server:
			%
			?myriad_spawn(
				fun() ->
					server_run( CallerPid, ServerName )
				end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end;

start( FilePath ) when is_list( FilePath ) ->

	RegistrationName = get_registration_name( FilePath ),

	case naming_utils:is_registered( RegistrationName, local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:string_to_binary( FilePath ),

			% No link to be created here, so one must beware of a silent crash
			% of this server:
			%
			?myriad_spawn(
				fun() ->
					server_run( CallerPid, RegistrationName, BinFilePath )
				end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end.



% @doc Ensures explicitly that, if not running already, a specified environment
% server is started and linked.
%
% If a name is specified: if no server already registered it, starts it with a
% blank state, and links that server to the calling process.
%
% If instead a filename is specified, if a server with a deriving name is not
% already registered, starts a corresponding server, links it to the calling
% process and initialises it with the file content.
%
% Returns in any case the PID of the corresponding environment server, already
% existing or not, blank or not.
%
-spec start_link( registration_name() | file_path() ) -> environment_pid().
start_link( FilePath ) when is_list( FilePath ) ->

	RegistrationName = get_registration_name( FilePath ),

	case naming_utils:is_registered( RegistrationName, local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:string_to_binary( FilePath ),

			?myriad_spawn_link( fun() ->
				server_run( CallerPid, RegistrationName, BinFilePath )
								end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end;

start_link( ServerName ) when is_atom( ServerName ) ->
	case naming_utils:is_registered( ServerName, local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			?myriad_spawn_link(
				fun() ->
					server_run( CallerPid, ServerName )
				end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end.



% @doc Ensures explicitly that, if not running already, an environment server is
% started with the specified registration name, and based on the specified file.
%
% If a server with the specified name is not already registered, starts a
% corresponding server and initialises it with the content of the specified
% file.
%
% Does not link the started environment server to the calling process.
%
% Returns in any case the PID of the corresponding environment server, already
% existing or not.
%
-spec start( registration_name(), file_path() ) -> environment_pid().
start( ServerName, FilePath )
			when is_atom( ServerName ) andalso is_list( FilePath ) ->

	case naming_utils:is_registered( ServerName, local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:string_to_binary( FilePath ),

			% No link to be created here, so we must beware of a silent crash of
			% this server:
			%
			?myriad_spawn(
				fun() ->
					server_run( CallerPid, ServerName, BinFilePath )
				end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end.



% @doc Ensures explicitly that, if not running already, an environment server is
% started and linked with the specified registration name, and based on the
% specified file.
%
% If a server with the specified name is not already registered, starts a
% corresponding server, links it to the calling process and initialises it with
% the content of the specified file.
%
% Returns in any case the PID of the corresponding environment server, already
% existing or not.
%
-spec start_link( registration_name(), file_path() ) -> environment_pid().
start_link( ServerName, FilePath )
			when is_atom( ServerName ) andalso is_list( FilePath ) ->

	case naming_utils:is_registered( ServerName, local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:string_to_binary( FilePath ),

			?myriad_spawn_link(
				fun() ->
					server_run( CallerPid, ServerName, BinFilePath )
				end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end.



% @doc Returns the PID of a supposedly already-running environment server,
% specified based on either its name or content filename.
%
-spec get_server( registration_name() | file_path() ) -> environment_pid().
get_server( ServerName ) when is_atom( ServerName ) ->

	case naming_utils:is_registered( ServerName, local ) of

		not_registered ->
			throw( { environment_server_not_registered, ServerName } );

		SrvPid ->
			SrvPid

	end;

get_server( FilePath ) when is_list( FilePath ) ->

	ServerName = get_registration_name( FilePath ),

	case naming_utils:is_registered( ServerName, local ) of

		not_registered ->
			throw( { environment_server_not_registered, ServerName,
					 FilePath } );

		SrvPid ->
			SrvPid

	end.



%-spec declare_cached_keys( maybe_list( key() ), environment_designator() ) ->
%declare_cached_keys() ->



% @doc Returns the value associated to each of the specified key(s) in the
% environment (if any), otherwise 'undefined', based on the specified
% environment file (and possibly launching a corresponding, non-linked,
% environment server if needed) or on the specified PID of an already-running
% environment server.
%
% Any cached key will be read from the local process cache, not from the
% environment server.
%
% Examples:
%  "Hello!" = environment:get(hello, "/var/foobar.etf")
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], "/var/foobar.etf")
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], MyEnvServerPid)
%
-spec get( maybe_list( key() ), environment_designator() | file_path() ) ->
										maybe_list( maybe( value() ) ).
get( KeyMaybes, FilePath ) when is_list( FilePath ) ->
	EnvSrvPid = start( FilePath ),
	get( KeyMaybes, EnvSrvPid );

get( Key, EnvDesignator ) when is_atom( Key ) ->
	hd( get( [ Key ], EnvDesignator ) );

% A designator is either a PID or a registration atom:
get( Keys, EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	get( Keys, EnvPid );

get( Keys, EnvPid ) when is_list( Keys ) ->
	case process_dictionary:get( ?environment_dictionary_key ) of

		undefined ->
			% No caching wanted or applied, so necessarily a message-based
			% request to the server:
			%
			get_from_environment( Keys, EnvPid );

		AllEnvTable ->
			case list_table:lookup_entry( EnvPid, AllEnvTable ) of

				% Caching used, but not for that environment; server needed:
				key_not_found ->
					get_from_environment( Keys, EnvPid );

				% Caching activated, including for this environment, at least
				% partially:
				%
				EnvCacheTable ->
					% Having to select the remote entries needed (if any):
					DictCachedKeys = table:keys( EnvCacheTable ),
					case list_utils:difference( Keys, DictCachedKeys ) of

						% All entries in cache, returning (in order) their
						% value:
						%
						[] ->
							table:values( Keys );

						% Some will have to be requested from the server (and
						% still not be cached):
						%
						LackingKeys ->
							LackingValues =
								get_from_environment( LackingKeys, EnvPid ),

							aggregate_values( Keys, LackingKeys, LackingValues,
											  EnvCacheTable, _AccValues=[] )

					end

			end

	end.



% (helper)
-spec aggregate_values( [ key() ], [ key() ], [ value() ], table(),
						[ value() ] ) -> [ value() ].
% Matches for Lacking* out of safety (not necessary):
aggregate_values( _TargetKeys=[], _LackingKeys=[], _LackingValues=[],
				  _CacheTable, AccValues ) ->
	lists:reverse( AccValues );

% Current key is a lacking one:
aggregate_values( _TargetKeys=[ K | Tt ], _LackingKeys=[ K | Tl ],
				  _LackingValues=[ V | Tv ], CacheTable, Acc ) ->
	aggregate_values( Tt, Tl, Tv, CacheTable, _NewAcc=[ V | Acc ] );

% Current key is in cache:
aggregate_values( _TargetKeys=[ K | Tt ], LackingKeys, LackingValues,
				  CacheTable, Acc ) ->
	V = table:get_value( K, CacheTable ),
	aggregate_values( Tt, LackingKeys, LackingValues, CacheTable, [ V | Acc ] ).



% @doc Returns the value associated to each of the specified key(s) in the
% environment (if any), otherwise 'undefined', based on the specified
% registration name: uses any server registered with that name, otherwise uses
% the specified filename to start a corresponding server.
%
% Any cached key will be read from the local process cache, not from the
% environment server.
%
% Examples:
%  "Hello!" = environment:get(hello, "/var/foobar.etf")
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], "/var/foobar.etf")
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], MyEnvServerPid)
%
-spec get( maybe_list( key() ), registration_name(), file_path() ) ->
										maybe_list( maybe( value() ) ).
get( KeyMaybes, ServerName, FilePath ) ->
	EnvSrvPid = case naming_utils:is_registered( ServerName, local ) of

		not_registered ->
			start( FilePath );

		ServerPid ->
			ServerPid

	end,
	get( KeyMaybes, EnvSrvPid ).



% @doc Gets the specified entries unconditionally from the environment server
% (not taking into account any cache).
%
% (helper)
%
-spec get( maybe_list( key() ), environment_designator() ) ->
										maybe_list( maybe( value() ) ).
get_from_environment( KeyMaybes, EnvDesignator ) ->
	EnvDesignator ! { get_environment, KeyMaybes, self() },
	receive

		{ notify_environment, ValueMaybes } ->
			ValueMaybes

	end.



% @doc Sets the specified key/value pairs (possibly overwriting any previous
% values) in the specified environment, based on the specified environment file
% (and possibly launching a corresponding environment server if needed) or on
% the designated already-running environment server (specified by registration
% name or PID).
%
% Any cached key will be updated in the local process cache, in addition to the
% environment server.
%
-spec set( [ entry() ], environment_designator() | file_path() ) -> void().
set( Entries, FilePath ) when is_list( FilePath ) ->
	EnvPid = start( FilePath ),
	set( Entries, EnvPid );

set( Entries, EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	set( Entries, EnvPid );

set( Entries, EnvPid ) when is_list( Entries ) ->
	EnvPid ! { set_environment, Entries },

	EnvDictKey = ?environment_dictionary_key,
	case process_dictionary:get( EnvDictKey ) of

		undefined ->
			% No caching wanted or applied, no cache to update:
			ok;

		AllEnvTable ->
			case list_table:lookup_entry( EnvPid, AllEnvTable ) of

				% Caching used, but not for that environment; thus we are done:
				key_not_found ->
					ok;

				% Caching activated, including for this environment, at least
				% partially:
				%
				EnvCacheTable ->

					NewEnvCacheTable =
						table:add_entries( Entries, EnvCacheTable ),

					NewAllEnvTable = list_table:add_entry( EnvPid,
						NewEnvCacheTable, AllEnvTable ),

					process_dictionary:put( EnvDictKey, NewAllEnvTable )


			end

	end.



% @doc Associates, in the specified environment, the specified value to the
% specified key (possibly overwriting any previous value), based on the
% specified environment file (and possibly launching a corresponding environment
% server if needed) or on the designated already-running environment server
% (specified by registration name or PID).
%
-spec set( key(), value(), environment_designator() | file_path() ) -> void().
set( Key, Value, Any ) ->
	set( [ { Key, Value } ], Any ).



% @doc Associates, in the specified environment, the specified value to the
% specified key (possibly overwriting any previous value), based on the
% specified registration name: uses any server registered with that name,
% otherwise uses the specified filename to start a corresponding server.
%
-spec set( key(), value(), registration_name(), file_path() ) -> void().
set( Key, Value, ServerName, FilePath ) ->
	EnvSrvPid = case naming_utils:is_registered( ServerName, local ) of

		not_registered ->
			start( FilePath );

		ServerPid ->
			ServerPid

	end,
	set( Key, Value, EnvSrvPid ).



% @doc Requests the calling process to cache also, from now on, the entries
% corresponding to the specified key(s).
%
% Any next setting by this process of one of these cached keys will update its
% local cache as well as the specified environment server; as a consequence,
% here the cache is supposed to start consistent with its server; and only the
% entries not already in cache are requested to the server.
%
-spec cache( maybe_list( key() ), environment_designator()  ) -> void().
cache( Key, EnvDesignator ) when is_atom( Key ) ->
	cache( [ Key ], EnvDesignator );


cache( Keys, EnvRegName ) when is_atom( EnvRegName ) ->
	% As a PID will be needed as key for the environment cache:
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	cache( Keys, EnvPid );


cache( Keys, EnvPid ) when is_list( Keys ) ->
	EnvDictKey = ?environment_dictionary_key,
	{ LackingKeys, EnvCacheTable, AllEnvTable } =
			case process_dictionary:get( EnvDictKey ) of

		undefined ->
			% All then:
			{ Keys, table:new(), list_table:new() };

		PrevAllEnvTable ->
			case list_table:lookup_entry( EnvPid, PrevAllEnvTable ) of

				key_not_found ->
					% Same as before, environment not cached yet:
					{ Keys, table:new(), PrevAllEnvTable };

				{ value, PrevEnvCacheTable } ->
					% Having to add all keys not already present in the cache
					% table:
					%
					DictCachedKeys = table:keys( PrevEnvCacheTable ),
					LackingK = list_utils:difference( Keys, DictCachedKeys ),
					{ LackingK, PrevEnvCacheTable, PrevAllEnvTable }

			end

	end,

	NewEnvCacheTable = case LackingKeys of

		[] ->
			EnvCacheTable;

		_ ->
			LackingValues = get( LackingKeys, EnvPid ),
			LackingEntries = lists:zip( LackingKeys, LackingValues ),
			table:add_entries( LackingEntries, EnvCacheTable )

	end,

	NewAllEnvTable =
		list_table:add_entry( EnvPid, NewEnvCacheTable, AllEnvTable ),
	process_dictionary:put( EnvDictKey, NewAllEnvTable ).



% @doc Synchronises the local cache (if any) from the specified environment
% server.
%
-spec sync( environment_designator() ) -> void().
sync( EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	sync( EnvPid );

sync( EnvPid ) ->
	EnvDictKey = ?environment_dictionary_key,
	case process_dictionary:get( EnvDictKey ) of

		undefined ->
			% No caching wanted or applied, nothing to sync:
			ok;

		AllEnvTable ->
			case list_table:lookup_entry( EnvPid, AllEnvTable ) of

				% Caching used, but not for that environment; thus we are done:
				key_not_found ->
					ok;

				% Caching activated, including for this environment, at least
				% partially:
				%
				EnvCacheTable ->
					AllCachedKeys = table:keys( EnvCacheTable ),
					AllValues = get_from_environment( AllCachedKeys, EnvPid ),
					AllCachedEntries = lists:zip( AllCachedKeys, AllValues ),
					NewEnvCacheTable = table:new( AllCachedEntries ),
					NewAllEnvTable = list_table:add_entry( EnvPid,
											NewEnvCacheTable, AllEnvTable ),
					process_dictionary:put( EnvDictKey, NewAllEnvTable )

			end

	end.



% @doc Stores (asynchronously) the current state of the specified environment in
% the file whence it supposedly was loaded initially.
%
-spec store( environment_designator() ) -> void().
store( EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	store( EnvPid );

store( EnvPid ) ->
	EnvPid ! store.


% @doc Stores (asynchronously) the current state of the specified environment in
% the specified file, regardless of any file from which it was loaded.
%
-spec store( environment_designator(), any_file_path() ) -> void().
store( EnvRegName, TargetFilePath ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	store( EnvPid, TargetFilePath );

store( EnvPid, TargetFilePath ) ->
	BinTargetFilePath = text_utils:ensure_binary( TargetFilePath ),
	EnvPid ! { store, BinTargetFilePath }.



% @doc Returns a textual description of the environment server (if any)
% specified from a designator thereof or from its environment file.
%
-spec to_string( environment_designator() | file_path() ) -> ustring().
to_string( FilePath ) when is_list( FilePath ) ->

	EnvRegAtom = get_registration_name( FilePath ),

	to_string( EnvRegAtom );

% Then supposed to be existing:
to_string( EnvSrvPid ) when is_pid( EnvSrvPid ) ->

	EnvSrvPid ! { to_string, self() },

	receive

		{ notify_environment_status, PrefString } ->
			PrefString

	end;

to_string( EnvRegAtom ) when is_atom( EnvRegAtom ) ->

	case naming_utils:is_registered( EnvRegAtom, local ) of

		not_registered ->
			"environment server not running";

		EnvSrvPid->
			to_string( EnvSrvPid )

	end.



% @doc Returns the automatic naming used for registering an environment server,
% as deduced from the specified environment filename.
%
% For example, the registration name associated to the
% "/var/opt/foobar.application.etf" environment filename is
% 'foobar_application'.
%
-spec get_registration_name( file_path() ) -> registration_name().
get_registration_name( FilePath ) ->
	CoreFilePath = file_utils:remove_upper_levels_and_extension( FilePath ),

	RegistrationName =
		file_utils:path_to_variable_name( CoreFilePath, _Prefix="" ),

	text_utils:string_to_atom( RegistrationName ).



% @doc Stops (asynchronously) the environment server designated by the specified
% registration name or file, if it is running.
%
% Never fails.
%
-spec stop( environment_designator() | file_path() ) -> void().
stop( FilePath ) when is_list( FilePath ) ->

	EnvRegAtom = get_registration_name( FilePath ),

	stop( EnvRegAtom );

stop( EnvDesignator ) ->
	EnvDesignator ! stop.




% Section for the environment server itself.


% Launcher of the environment server, to start with a blank state.
-spec server_run( pid(), registration_name() ) -> no_return().
server_run( SpawnerPid, RegistrationName ) ->

	trace_utils:debug_fmt( "Spawning a blank environment server named '~ts' "
		"from ~w.", [ RegistrationName, SpawnerPid ] ),

	case naming_utils:register_or_return_registered( RegistrationName,
													 local_only ) of

		registered ->

			% We gained the shared name, we are the one and only server for that
			% name.

			EmptyTable = table:new(),

			% Spawner could already know that PID in this case:
			SpawnerPid ! { environment_server_pid, self() },

			% Never returns:
			server_main_loop( EmptyTable, _MaybeBinFilePath=undefined );


		ServerPid ->
			% Notifies and terminates:
			SpawnerPid ! { environment_server_pid, ServerPid }

	end.



% Launcher of the environment server, to be initialised with the specified file.
-spec server_run( pid(), registration_name(), bin_string() ) -> no_return().
server_run( SpawnerPid, RegistrationName, BinFilePath ) ->

	trace_utils:debug_fmt( "Spawning environment server named '~ts' "
		"from ~w, based on file '~ts'.",
		[ RegistrationName, SpawnerPid, BinFilePath ] ),

	case naming_utils:register_or_return_registered( RegistrationName,
													 local_only ) of

		registered ->

			% We gained the shared name, we are the one and only server for that
			% name.

			EmptyTable = table:new(),

			FinalTable =
					case file_utils:is_existing_file_or_link( BinFilePath ) of

				true ->
					add_environment_from( BinFilePath, EmptyTable );

				false ->
					trace_bridge:info_fmt( "No environment file found "
						"(searched for '~ts').~n", [ BinFilePath ] ),
					EmptyTable

			end,

			% Spawner could already know that PID in this case:
			SpawnerPid ! { environment_server_pid, self() },

			% Never returns:
			server_main_loop( FinalTable, BinFilePath );


		ServerPid ->
			% Notifies and terminates:
			SpawnerPid ! { environment_server_pid, ServerPid }

	end.



% (helper)
get_value_maybes( Key, Table ) when is_atom( Key ) ->
	case table:lookup_entry( Key, Table ) of

		key_not_found ->
			undefined;

		{ value, V } ->
			V
	end;

get_value_maybes( Keys, Table ) ->
	get_value_maybes( Keys, Table, _Acc=[] ).



% (helper)
get_value_maybes( _Keys=[], _Table, Acc ) ->
	lists:reverse( Acc );

get_value_maybes( _Keys=[ K | T ], Table, Acc ) ->
	V = get_value_maybes( K, Table ),
	get_value_maybes( T, Table, [ V | Acc ] ).



% Main loop of the environment server.
-spec server_main_loop( table(), maybe( bin_file_path() ) ) -> no_return().
server_main_loop( Table, MaybeBinFilePath ) ->

	%trace_bridge:debug_fmt( "Waiting for environment-related request, "
	%    "having ~B recorded environment.", [ table:size( Table ) ] ),

	receive

		{ get_environment, KeyMaybes, SenderPid } ->

			Answer = get_value_maybes( KeyMaybes, Table ),

			SenderPid ! { notify_environment, Answer },

			server_main_loop( Table, MaybeBinFilePath );


		{ set_environment, Key, Value } ->

			NewTable = table:add_entry( Key, Value, Table ),

			server_main_loop( NewTable, MaybeBinFilePath );


		{ set_environment, Entries } ->

			NewTable = table:add_entries( Entries, Table ),

			server_main_loop( NewTable, MaybeBinFilePath );


		store ->
			case MaybeBinFilePath of

				undefined ->
					trace_utils:warning_fmt( "Store request for environment "
						"server ~w ignored (no file path set).", [ self() ] );

				BinFilePath ->
					file_utils:write_etf_file( table:enumerate( Table ),
											   BinFilePath )

			end;


		{ store, BinTargetFilePath } ->
			file_utils:write_etf_file( table:enumerate( Table ),
									   BinTargetFilePath );


		{ to_string, SenderPid } ->

			Res = case table:enumerate( Table ) of

				[] ->
					"no environment recorded";

				L ->

					% Enforces a consistent order:
					Strings = [ text_utils:format( "~p: ~p", [ K, V ] )
								|| { K, V } <- lists:sort( L ) ],

					text_utils:format( "~B environment recorded: ~ts~n",
						[ length( L ),
						  text_utils:strings_to_string( Strings ) ] )

			end,

			SenderPid ! { notify_environment_status, Res },

			server_main_loop( Table, MaybeBinFilePath );

		stop ->
			%trace_bridge:debug_fmt( "Stopping environment server ~w.",
			%                        [ self() ] ),
			stopped

	end.



% Helper functions.


% Adds the environment found in specified file into the specified table, and
% returns it.
%
% (helper)
%
add_environment_from( FilePath, Table ) ->

	case file:consult( FilePath ) of

		{ ok, Entries } ->

			case check_entries( Entries ) of

				ok ->
					NewTable = table:add_entries( Entries, Table ),

					%trace_bridge:debug_fmt( "Loaded from environment file "
					%    "'~ts' following entries: ~ts",
					%    [ PrefFilePath, table:to_string( NewTable ) ] ),

				   %trace_bridge:debug_fmt( "Environment file '~ts' loaded.",
				   %                        [ FilePath ] ),

				   NewTable;

				ErrorString ->
					trace_bridge:error_fmt( "Error when reading environment "
						"file '~ts' (~ts), no environment read.",
						[ FilePath, ErrorString ] ),
					Table

			end;


		{ error, { Line, _Mod, Term } } ->
			FlattenError = text_utils:format( "~p", [ Term ] ),
			trace_bridge:error_fmt( "Error in environment file '~ts' "
				"at line ~B (~ts), no environment read.",
				[ FilePath, Line, FlattenError ] ),
			Table;


		{ error, Reason } ->
			trace_bridge:error_fmt( "Error when reading environment file "
				"'~ts' (~p), no environment read.", [ FilePath, Reason ] ),
			Table

	end.



% Checks specified entries.
check_entries( _Entries=[] ) ->
	ok;

check_entries( _Entries=[ { K, _V } | T ] ) when is_atom( K ) ->
	check_entries( T );

check_entries( _Entries=[ { K, _V } | _T ] ) ->
	text_utils:format( "key '~p' is not an atom", [ K ] );

check_entries( _Entries=[ E | _T ] ) ->
	text_utils:format( "entry '~p' is not a key/value pair", [ E ] ).
