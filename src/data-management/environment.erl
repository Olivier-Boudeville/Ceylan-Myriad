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
% of its components, and makes it available to client processes.
%
% An environment entry is designated by a key (an atom), associated to a value
% (that can be any term) in a pair, which is designated as an entry. No
% difference is made between a non-registered key and a key registered to
% 'undefined'.
%
% Environments hold application-specific or component-specific data, obtained
% from any source (file included); they may also start blank and be exclusively
% fed at runtime by the application or the components. Environments are used
% afterwards to maintain these pieces of data (read/write), before possibly
% storing them on file at application exit or component stop.
%
% As a whole, an environment server can be seen as a process holding state
% information meant to be potentially common to various processes of a given
% application or component.
%
% Environment data can be read from or written to file(s) in the ETF format,
% hence as a series of Erlang terms written as strings, each ending with a dot
% (i.e. it is the basic, standard format understood by `file:consult/1').
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
% its conventional (atom) registration name (like in
% `environment:get(my_first_color, foobar_env_server'). No specific global
% registration of that server is made here.
%
% A (single) explicit start (with one of the start* functions) shall be
% preferred to implicit ones (typically triggered thanks to the get* functions)
% to avoid any risk of race conditions (should multiple processes attempt
% concurrently to create the same environment server), and also to be able to
% request that it is also linked to the calling process.
%
% An environment is best designated as a PID, otherwise as a registered name,
% otherwise from any filename that it uses.


% About the caching of environment entries:

% For faster accesses (not involving any inter-process messaging), and if
% considering that their changes are rather infrequent (or never happening), at
% least some entries managed by an environment server may be cached directly in
% client processes.
%
% In this case, the process dictionary of these clients is used to store the
% cached entries, and when updating a cached key from a client process the
% corresponding environment server is updated in turn. However any other client
% process caching that key will not be aware of this change until it requests an
% update to this environment server.
%
% As soon as a key must be cached, its value is set in the cache; there is thus
% always a value associated to a cache key (not a maybe-value), and thus cached
% values may be 'undefined').
%
% Multiple environments may be used concurrently. A specific case of environment
% corresponds to the user preferences. See our preferences module for that.
%
% Refer to https://myriad.esperide.org/#etf for more information.
%
-module(environment).


-export([ start/1, start/2, start_link/1, start_link/2,
		  get_server/1,
		  get/2, get/3, set/2, set/3, set/4,
		  cache/2, sync/1, store/1, store/2,
		  to_string/1, to_string/0,
		  stop/1 ]).


-type env_pid() :: pid().
% The PID of an environment server.

-type env_reg_name() :: registration_name().
% The registration name of an environment server.


-type env_designator() :: env_pid() | env_reg_name().
% The two standard ways according to which an environment server can be
% designated: either directly thanks to its PID or to the name under which it is
% locally registered.


-type key() :: atom().


-type value() :: table:value().
% Can be 'undefined' (no difference between a non-registered key and a key
% registered to 'undefined').


-type entry() :: table:entry().
-type entries() :: table:entries().


-export_type([ env_pid/0, env_reg_name/0, env_designator/0,
			   key/0, value/0, entry/0, entries/0 ]).


% For myriad_spawn*:
-include("spawn_utils.hrl").


% The name of the key in the process dictionary corresponding to environment
% information, notably for caching:
%
-define( env_dictionary_key, 'myriad_environment_cache' ).


% A list_table, as it is expected to reference only very few environments:
-type all_env_table() :: list_table( env_pid(), env_info() ).
% Corresponds to the value associated to the env_dictionary_key key in
% the process dictionary of a process using environment caching.


-type env_info() :: { env_reg_name(), env_cache_table() }.
% Information stored regarding an environment, typically to be cached in the
% process dictionary in a all_env_table() table, and indexed by the PID of the
% corresponding environment server.


-type env_cache_table() :: table( atom(), term() ).
% A table storing the (local) cached entries for a given environment.


% Just for silencing:
-export_type([ all_env_table/0 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type env_reg_name() :: naming_utils:env_reg_name().
% The name under which an environment server can be (locally) registered.

-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().
-type any_file_path() :: file_utils:any_file_path().

-type list_table( K, V ) :: list_table:list_table( K, V ).



% Implementation notes:
%
% Each environment server is managed through a locally registered process,
% maintaining an associative table whose content can be defined programmatically
% and/or thanks to data files.

% There is a slight potential race condition with the implicit starting of this
% service: a process could trigger its creation whereas it is already in
% progress, due to an earlier trigger.

% Some accessors accept both a registration name and a file path, whereas the
% former could be deduced from the latter. The idea is to avoid, in the case of
% potentially frequent operations, unnecessary conversions.


% When using caching, the corresponding cached entries for a given environment
% will be stored in the process dictionary (of each client process using that
% feature), under the dictionary key designated by the
% env_dictionary_key define.
%
% The value associated to this dictionary key is a AllEnvTable ::
% all_env_table() table associating to each cached environment (designated by
% the PID of its server) the registration name of that server and a table of its
% cached entries.



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
-spec start( env_reg_name() | file_path() ) -> env_pid().
start( ServerName ) when is_atom( ServerName ) ->
	case naming_utils:is_registered( ServerName, local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			% No link to be created here, so one must beware of any silent crash
			% of this server:
			%
			?myriad_spawn( fun() -> server_run( CallerPid, ServerName ) end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end;


start( FilePath ) when is_list( FilePath ) ->

	RegistrationName = get_env_reg_name_from( FilePath ),

	case naming_utils:is_registered( RegistrationName, local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:string_to_binary( FilePath ),

			% No link to be created here, so one must beware of any silent crash
			% of this server:
			%
			?myriad_spawn( fun() -> server_run( CallerPid, RegistrationName,
												BinFilePath ) end ),

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
-spec start_link( env_reg_name() | file_path() ) -> env_pid().
start_link( FilePath ) when is_list( FilePath ) ->

	RegistrationName = get_env_reg_name_from( FilePath ),

	case naming_utils:is_registered( RegistrationName, local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:string_to_binary( FilePath ),

			?myriad_spawn_link( fun() ->
				server_run( CallerPid, RegistrationName, BinFilePath ) end ),

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
				fun() -> server_run( CallerPid, ServerName ) end ),

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
-spec start( env_reg_name(), any_file_path() ) -> env_pid().
start( ServerName, AnyFilePath ) when is_atom( ServerName ) ->

	case naming_utils:is_registered( ServerName, local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:ensure_binary( AnyFilePath ),

			% No link to be created here, so we must beware of any silent crash
			% of this server:
			%
			?myriad_spawn(
				fun() -> server_run( CallerPid, ServerName, BinFilePath ) end ),

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
-spec start_link( env_reg_name(), any_file_path() ) -> env_pid().
start_link( ServerName, AnyFilePath ) when is_atom( ServerName ) ->

	case naming_utils:is_registered( ServerName, local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:ensure_binary( AnyFilePath ),

			?myriad_spawn_link(
				fun() -> server_run( CallerPid, ServerName, BinFilePath ) end ),

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
-spec get_server( env_reg_name() | file_path() ) -> env_pid().
get_server( ServerName ) when is_atom( ServerName ) ->

	case naming_utils:is_registered( ServerName, local ) of

		not_registered ->
			throw( { environment_server_not_registered, ServerName } );

		SrvPid ->
			SrvPid

	end;

get_server( FilePath ) when is_list( FilePath ) ->

	ServerName = get_env_reg_name_from( FilePath ),

	case naming_utils:is_registered( ServerName, local ) of

		not_registered ->
			throw( { environment_server_not_registered, ServerName,
					 FilePath } );

		SrvPid ->
			SrvPid

	end.




% @doc Returns the value associated to each of the specified key(s) in the
% environment (if any), otherwise 'undefined', based on the specified
% environment file (and possibly launching a corresponding, non-linked,
% environment server if needed) or on an already-running environment server
% (specified as a PID or a registration name).
%
% Any cached key will be read from the local process cache, not from the
% environment server.
%
% Examples:
%
%  "Hello!" = environment:get(hello, "/var/foobar.etf")
%
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], "/var/foobar.etf")
%
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], my_env_server_name)
%
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], MyEnvServerPid)
%
-spec get( maybe_list( key() ), env_designator() | file_path() ) ->
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
	case process_dictionary:get( ?env_dictionary_key ) of

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
				% for some keys:
				%
				{ value, { _EnvRegName, EnvCacheTable } } ->
					% Having to select the remote entries needed (if any):
					DictCachedKeys = table:keys( EnvCacheTable ),
					case list_utils:difference( Keys, DictCachedKeys ) of

						% All entries in cache; returning (in order) their
						% value directly:
						%
						[] ->
							table:get_values( Keys, EnvCacheTable );

						% At least some will have to be requested from the
						% server (and will still not be cached):
						%
						LackingKeys ->
							LackingValues =
								get_from_environment( LackingKeys, EnvPid ),

							aggregate_values( Keys, LackingKeys, LackingValues,
											  EnvCacheTable, _AccValues=[] )

					end

			end

	end.



% Merges cached entries with the specified, immediate ones.
%
% (helper)
-spec aggregate_values( [ key() ], [ key() ], [ value() ], table(),
						[ value() ] ) -> [ value() ].
% Checks that Immediate* are also empty out of safety (not necessary):
aggregate_values( _TargetKeys=[], _ImmediateKeys=[], _ImmediateValues=[],
				  _CacheTable, AccValues ) ->
	lists:reverse( AccValues );

% Current key is an immediate one:
aggregate_values( _TargetKeys=[ K | Tt ], _ImmediateKeys=[ K | Tl ],
				  _ImmediateValues=[ V | Tv ], CacheTable, Acc ) ->
	aggregate_values( Tt, Tl, Tv, CacheTable, _NewAcc=[ V | Acc ] );

% Current key is thus in cache:
aggregate_values( _TargetKeys=[ K | Tt ], ImmediateKeys, ImmediateValues,
				  CacheTable, Acc ) ->
	V = table:get_value( K, CacheTable ),
	aggregate_values( Tt, ImmediateKeys, ImmediateValues, CacheTable,
					  _NewAcc=[ V | Acc ] ).



% @doc Returns the value associated to each of the specified key(s) in the
% environment (if any), otherwise 'undefined', based on the specified
% registration name: uses any server registered with that name, otherwise uses
% the specified filename to start a corresponding server.
%
% Any cached key will be read from the local process cache, not from the
% environment server.
%
% Examples:
%
%  "Hello!" = environment:get(hello, my_env_server_name, "/var/foobar.etf")
%
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], my_env_server_name,
%                      "/var/foobar.etf")
%
-spec get( maybe_list( key() ), env_reg_name(), file_path() ) ->
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
-spec get( maybe_list( maybe_list( key() ) ), env_designator() ) ->
										maybe_list( maybe( value() ) ).
get_from_environment( _KeyMaybes=[], _EnvDesignator ) ->
	[];

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
-spec set( [ entry() ], env_designator() | file_path() ) -> void().
set( Entries, FilePath ) when is_list( FilePath ) ->
	EnvPid = start( FilePath ),
	set( Entries, EnvPid );

set( Entries, EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	set( Entries, EnvPid );

set( Entries, EnvPid ) when is_list( Entries ) ->
	EnvPid ! { set_environment, Entries },

	EnvDictKey = ?env_dictionary_key,
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
				% for some keys:
				%
				{ value, { EnvRegName, EnvCacheTable } } ->

					% We update only the cached keys:
					NewEnvCacheTable =
						table:update_existing_entries( Entries, EnvCacheTable ),

					NewAllEnvTable = list_table:add_entry( EnvPid,
						{ EnvRegName, NewEnvCacheTable }, AllEnvTable ),

					process_dictionary:put( EnvDictKey, NewAllEnvTable )

			end

	end.



% @doc Associates, in the specified environment, the specified value to the
% specified key (possibly overwriting any previous value), based on the
% specified environment file (and possibly launching a corresponding environment
% server if needed) or on the designated already-running environment server
% (specified by registration name or PID).
%
-spec set( key(), value(), env_designator() | file_path() ) -> void().
set( Key, Value, AnyEnvElem ) ->
	set( [ { Key, Value } ], AnyEnvElem ).



% @doc Associates, in the specified environment, the specified value to the
% specified key (possibly overwriting any previous value), based on the
% specified registration name: uses any server registered with that name,
% otherwise uses the specified filename to start a corresponding server.
%
-spec set( key(), value(), env_reg_name(), file_path() ) -> void().
set( Key, Value, ServerName, FilePath ) ->
	EnvSrvPid = case naming_utils:is_registered( ServerName, local ) of

		not_registered ->
			start( FilePath );

		ServerPid ->
			ServerPid

	end,
	set( [ { Key, Value } ], EnvSrvPid ).



% @doc Requests the calling process to cache the entries corresponding to the
% specified key(s), which will thus be appropriately synchronised with the
% server from now on, in addition to any already cached keys.
%
% Either single keys or full entries can be specified there. Both will lead the
% corresponding keys to be cached, yet a single key, if it is not already cached
% (otherwise, it will be ignored), will trigger its value to be fetched from the
% enviroment server whereas the value of a full entry will be cached and also
% sent to the environment server.
%
% Any next setting by this process of one of these cached keys will update its
% local cache as well as the specified environment server; as a consequence,
% here the cache is expected to start consistent with its server; afterwards by
% default only the entries not already in cache will be requested from the
% server.
%
-spec cache( maybe_list( key() | entry() ), env_reg_name()  ) -> void().
cache( Key, EnvRegName ) when is_atom( Key ) ->
	cache( [ Key ], EnvRegName );

cache( Entry, EnvRegName ) when is_tuple( Entry ) ->
	cache( [ Entry ], EnvRegName );

% No direct env_pid() allowed, as we want to store the registration name as
% well:
%
cache( KeysOrEntries, EnvRegName ) when is_atom( EnvRegName ) ->

	cond_utils:if_defined( myriad_debug_environments,
		trace_utils:debug_fmt( "Client ~w caching, "
			"regarding environment ~ts:~n~p",
			[ self(), EnvRegName, KeysOrEntries ] ) ),

	% As a PID will be needed (at least as key for the environment cache):
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),

	% Separates single keys from entries:
	{ ToCacheKeys, ToCacheEntries } = lists:foldl(
		fun( E={ _K, _V }, { AccK, AccE } ) ->
			{ AccK, [ E | AccE ] };

		   ( K, { AccK, AccE } ) ->
			{ [ K | AccK ], AccE }

		end,
		_Acc0={ _AccK0=[], _AccE0=[] },
		_List=KeysOrEntries ),

	EnvDictKey = ?env_dictionary_key,

	% We have to determine the new keys to cache whose values must be fetched
	% from server:
	%
	{ SingleKeys, Entries, EnvCacheTable, AllEnvTable } =
			case process_dictionary:get( EnvDictKey ) of

		undefined ->
			% All these new elements shall then be cached:
			cond_utils:if_defined( myriad_debug_environments,
				trace_utils:debug( "No environment was cached." ) ),

			{ ToCacheKeys, ToCacheEntries, table:new(), list_table:new() };

		PrevAllEnvTable ->
			case list_table:lookup_entry( EnvPid, PrevAllEnvTable ) of

				key_not_found ->
					% Quite same as before, this environment was not cached yet:
					cond_utils:if_defined( myriad_debug_environments,
						trace_utils:debug_fmt( "Environment ~ts "
							"(server: PID: ~w) was not cached.",
							[ EnvRegName, EnvPid ] ) ),

					{ ToCacheKeys, ToCacheEntries, table:new(),
					  PrevAllEnvTable };

				{ value, { _EnvRegName, PrevEnvCacheTable } } ->
					cond_utils:if_defined( myriad_debug_environments,
						trace_utils:debug_fmt( "Updating cache for environment "
							"~ts (server: PID: ~w): ~ts", [ EnvRegName, EnvPid,
								table:to_string( PrevEnvCacheTable ) ] ) ),

					% Having to add all single keys not already present in the
					% cache table:
					%
					DictCachedKeys = table:keys( PrevEnvCacheTable ),

					NewToCacheKeys =
						list_utils:difference( ToCacheKeys,	DictCachedKeys ),

					{ NewToCacheKeys, ToCacheEntries, PrevEnvCacheTable,
					  PrevAllEnvTable }

			end

	end,

	% First the environment must be aware of these new entries:
	EnvPid ! { set_environment, Entries },

	% And the local cache as well:
	WithEntriesEnvCacheTable = table:add_entries( Entries, EnvCacheTable ),

	% Then update based on the single keys:
	NewEnvCacheTable = case SingleKeys of

		[] ->
			WithEntriesEnvCacheTable;

		_ ->
			SingleValues = get_from_environment( SingleKeys, EnvPid ),
			SingleEntries = lists:zip( SingleKeys, SingleValues ),
			table:add_entries( SingleEntries, WithEntriesEnvCacheTable )

	end,

	%trace_utils:debug_fmt( "New cache for environment ~ts (server: ~w): ~ts",
	%   [ EnvRegName, EnvPid, table:to_string( NewEnvCacheTable ) ] ),

	NewAllEnvTable = list_table:add_entry( EnvPid,
		{ EnvRegName, NewEnvCacheTable }, AllEnvTable ),

	%trace_utils:debug_fmt( "New environment table for client ~w: ~ts",
	%   [ self(), list_table:to_string( NewAllEnvTable ) ] ),

	process_dictionary:put( EnvDictKey, NewAllEnvTable ).



% @doc Synchronises the local cache (if any) from the specified environment
% server, assuming the server entries are references (that is more recent than
% the cached ones).
%
-spec sync( env_designator() ) -> void().
sync( EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	sync( EnvPid );

sync( EnvPid ) ->
	EnvDictKey = ?env_dictionary_key,
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
				{ value, { EnvRegName, EnvCacheTable } } ->

					AllCachedKeys = table:keys( EnvCacheTable ),

					AllValues = get_from_environment( AllCachedKeys, EnvPid ),

					AllCachedEntries = lists:zip( AllCachedKeys, AllValues ),

					NewEnvCacheTable = table:new( AllCachedEntries ),

					NewAllEnvTable = list_table:add_entry( EnvPid,
						{ EnvRegName, NewEnvCacheTable }, AllEnvTable ),

					process_dictionary:put( EnvDictKey, NewAllEnvTable )

			end

	end.



% @doc Stores (asynchronously) the current state of the specified environment in
% the file whence it supposedly was loaded initially.
%
-spec store( env_designator() ) -> void().
store( EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	store( EnvPid );

store( EnvPid ) ->
	EnvPid ! store.



% @doc Stores (asynchronously) the current state of the specified environment in
% the specified file, regardless of any file from which it was loaded.
%
% The specified path becomes the reference one.
%
-spec store( env_designator(), any_file_path() ) -> void().
store( EnvRegName, TargetFilePath ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	store( EnvPid, TargetFilePath );

store( EnvPid, TargetFilePath ) ->
	BinTargetFilePath = text_utils:ensure_binary( TargetFilePath ),
	EnvPid ! { store, BinTargetFilePath }.



% @doc Returns a textual description of the environment server (if any)
% specified from a designator thereof or from its environment file.
%
-spec to_string( env_designator() | file_path() ) -> ustring().
to_string( FilePath ) when is_list( FilePath ) ->

	EnvRegAtom = get_env_reg_name_from( FilePath ),

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



% @doc Returns a textual description of the environments known of the calling
% process, based on its cache (if any).
%
-spec to_string() -> ustring().
to_string() ->
	case process_dictionary:get( ?env_dictionary_key ) of

		undefined ->
			"no caching of environments";

		AllEnvTable ->
			case list_table:enumerate( AllEnvTable ) of

				[] ->
					"no environment cached";

				[ { EnvPid, EnvInfo } ] ->
					text_utils:format( "a single environment cached, ~ts",
						[ env_info_to_string( EnvInfo, EnvPid ) ] );

				EnvPairs ->
					text_utils:format( "~B environments cached: ~ts",
						[ length( EnvPairs ), text_utils:strings_to_string(
							[ env_info_to_string( EInf, EPid )
							  || { EInf, EPid } <- EnvPairs ] ) ] )


			end

	end.




-spec env_info_to_string( env_info(), env_pid() ) -> ustring().
env_info_to_string( _EnvInfo={ EnvRegName, EnvCacheTable }, EnvPid ) ->
	text_utils:format( "environment '~ts' (whose server is ~w), caching ~ts",
		[ EnvRegName, EnvPid, env_cache_to_string( EnvCacheTable ) ] ).


-spec env_cache_to_string( env_cache_table() ) -> ustring().
env_cache_to_string( EnvCacheTable ) ->
	case table:enumerate( EnvCacheTable ) of

		[] ->
			"no entry";

		[ { K, V } ] ->
			text_utils:format( "a single entry, whose key ~p is associated "
							   "to value ~p", [ K, V ] );

		Entries ->
			text_utils:format( "~B entries: ~ts", [ length( Entries ),
				text_utils:strings_to_string( [ text_utils:format(
					"key '~ts' associated to value ~p", [ K, V ] )
						|| { K, V} <- Entries ], _IdentLevel=2 ) ] )

	end.



% @doc Returns the automatic naming used for registering an environment server,
% as deduced from the specified environment filename.
%
% For example, the registration name associated to the
% "/var/opt/foobar.application.etf" environment filename is
% 'foobar_application'.
%
-spec get_env_reg_name_from( file_path() ) -> env_reg_name().
get_env_reg_name_from( FilePath ) ->
	CoreFilePath = file_utils:remove_upper_levels_and_extension( FilePath ),

	RegistrationName =
		file_utils:path_to_variable_name( CoreFilePath, _Prefix="" ),

	text_utils:string_to_atom( RegistrationName ).



% @doc Stops (asynchronously) the environment server designated by the specified
% registration name or file, if it is running.
%
% Never fails.
%
-spec stop( env_designator() | file_path() ) -> void().
stop( FilePath ) when is_list( FilePath ) ->

	EnvRegAtom = get_env_reg_name_from( FilePath ),

	stop( EnvRegAtom );

stop( EnvDesignator ) ->
	EnvDesignator ! stop.




% Section for the environment server itself.


% Launcher of the environment server, to start with a blank state.
-spec server_run( pid(), env_reg_name() ) -> no_return().
server_run( SpawnerPid, RegistrationName ) ->

	cond_utils:if_defined( myriad_debug_environments, trace_utils:debug_fmt(
		"Spawning a blank environment server named '~ts' from ~w.",
		[ RegistrationName, SpawnerPid ] ) ),

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
-spec server_run( pid(), env_reg_name(), bin_string() ) -> no_return().
server_run( SpawnerPid, RegistrationName, BinFilePath ) ->

	cond_utils:if_defined( myriad_debug_environments, trace_utils:debug_fmt(
		"Spawning environment server named '~ts' from ~w, based on file '~ts'.",
		[ RegistrationName, SpawnerPid, BinFilePath ] ) ),

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

	% Short:
	%trace_bridge:debug_fmt( "Waiting for environment-related request, "
	%    "while having ~B recorded entries.", [ table:size( Table ) ] ),

	% Detailed:
	%trace_bridge:debug_fmt( "Waiting for environment-related request, "
	%   "storing a ~ts.", [ table:to_string( Table ) ] ),

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

					cond_utils:if_defined( myriad_debug_environments,
						trace_utils:debug_fmt( "Storing environment state "
							"in current file '~ts'.", [ BinFilePath ] ) ),

					file_utils:write_etf_file( table:enumerate( Table ),
											   BinFilePath )

			end,
			server_main_loop( Table, MaybeBinFilePath );


		{ store, BinTargetFilePath } ->
			cond_utils:if_defined( myriad_debug_environments,
				trace_utils:debug_fmt( "Storing environment state "
					"in specified file '~ts'.", [ BinTargetFilePath ] ) ),
			file_utils:write_etf_file( table:enumerate( Table ),
									   BinTargetFilePath ),
			server_main_loop( Table, BinTargetFilePath );


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
