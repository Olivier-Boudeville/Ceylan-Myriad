% Copyright (C) 2016-2021 Olivier Boudeville
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
% Creation date: Wednesday, June 8, 2016



% @doc Gathering of various facilities regarding the management of <b>SQL
% databases</b>.
%
% The current implementation is mostly geared towards the use of:
%
% - `SQLite3' (an optional dependency), through the <a
% href="https://github.com/alexeyr/erlang-sqlite3">erlang-sqlite3</a> binding
%
% - `PostgreSQL', through either its command-line client (psql; the least
% recommended approach) or the epgsql [https://github.com/epgsql/epgsql] binding
%
% See `sql_support_test.erl' for the corresponding test.
%
-module(sql_support).



% Design notes:
%
% To manage SQL databases, we rely on:
%
% - either SQLite3 and the erlang-sqlite3 binding,
% expected to be already built and installed, in ~/Software/erlang-sqlite3
%
% - or PostgreSQL and the epgsql binding, expected to be already built and
% installed, in ~/Software/epgsql



% For the various records involved:
-include("sql_support.hrl").

-if(myriad_sql_backend =:= sqlite3).

% For the various types defined:
-include_lib("sqlite3.hrl").

-endif.


-if(myriad_sql_backend =:= postgresql).

% For the various types defined:
-include_lib("epgsql.hrl").

-endif.



-type database_host_name() :: net_utils:string_host_name().
% Designates the hostname on which a target database instance is running.
% Ex: "baz.foobar.org."


-type database_port() :: net_utils:tcp_port().
% The (TCP) port at which the target database instance is running.
% Ex: 5432.


-type database_name() :: ustring().
% The name of the target database instance. Ex: "acme_stock_db".


-type database_connection_settings() :: #database_connection_settings{}.
% The settings needed to designate a database instance.



-type database_user_name() :: ustring().
% The name of a database user. Ex: "john_smith".

-type database_user_password() :: ustring().
% The password of a database user.

-type database_user_settings() :: #database_user_settings{}.
% The settings corresponding to a given database user.


-type table_name() :: ustring().
% A table of a database contains columns and rows.


-type record_name() :: ustring().
% A record (also called a 'row' or a 'tuple') represents a single, implicitly
% structured data item in a table.


-type field_name() :: ustring().
% A field (also designated as 'column' or 'attribute') represents a set of data
% values of a particular type, one value for each record of the table.


-type backend_name() :: 'sqlite3' | 'postgresql'.
% The supported SQL backends (as atom names).


% A PID for epgsql:
%-opaque connection() :: any().
-type connection() :: any().
% Designates an (opaque, and backend-dependent) connection handle.


-export_type([ database_host_name/0, database_port/0, database_name/0,
			   database_connection_settings/0,

			   database_user_name/0, database_user_password/0,
			   database_user_settings/0,

			   table_name/0, record_name/0, field_name/0,

			   backend_name/0, connection/0 ]).



-export([ list_possible_backend_names/0, has_backend/1, get_backend_name/0,
		  start/0,
		  connect/2, connect/3,
		  %extract_field/6,
		  close/1,
		  stop/0 ]).




% Shorthands:

-type base_status() :: basic_utils:base_status().

-type ustring() :: text_utils:ustring().

-type time_out() :: time_utils:time_out().



% General support section, regarding the SQL service itself.



% @doc Returns the various known SQL backend names (in general; not telling
% whether they are available locally or not).
%
-spec list_possible_backend_names() -> [ backend_name() ].
list_possible_backend_names() ->
	[ sqlite3, postgresql ].



% @doc Tells whether, from the point of view of Myriad, the specified SQL
% backend is available (that is currently and locally, at runtime).
%
-spec has_backend( backend_name() ) -> boolean().
has_backend( BackendName ) ->

	% The availability of a given backend is determined on whether its main
	% module can be found in the code path:
	%
	BackendModule = case BackendName of

		sqlite3 ->
			sqlite3;

		postgresql ->
			epgsql

	end,

	case code_utils:is_beam_in_path( BackendModule ) of

		not_found ->
			false;

		[ _SinglePath ] ->
			true;

		MultiplePaths ->
			trace_utils:warning_fmt( "Multiple '~ts' modules found for the SQL "
				"backend ~ts, in: ~ts", [ BackendModule, BackendName,
					text_utils:strings_to_string( MultiplePaths ) ] ),
			true

	end.



% @doc Returns the type of the currently used (build-time) SQL backend (if any).
%
-spec get_backend_name() -> maybe( backend_name() ).
get_backend_name() ->
	cond_utils:switch_set_to( myriad_sql_backend, [

		{ sqlite3, sqlite3 },

		{ postgresql, postgresql },

		{ none, undefined } ],

		% Default token:
		none ).



% @doc Starts (checks and inits) the SQL service support.
-spec start() -> void().
start() ->
	cond_utils:switch_set_to( myriad_sql_backend, [

		{ sqlite3,
			% We have to secure the erlang-sqlite3 binding, nevertheless nothing
			% special is needed (ex: finding ebin/sqlite3.beam results in
			% finding automatically priv/sqlite3_drv.so).

			trace_utils:debug( "Starting the SQL support, using SQLite3." )
		},

		{ postgresql,
			trace_utils:debug( "Starting the SQL support, using PostgreSQL." )
		},

		{ none, throw( no_myriad_sql_backend_enabled ) } ],

		% Default token:
		none ).



% @doc Connects to the specified database, with a default time-out.
-spec connect( database_connection_settings(), database_user_settings() ) ->
							fallible( connection() ).
connect( ConnSettings, UserSettings ) ->
	connect( ConnSettings, UserSettings, _TimeOutMs=5000 ).


% @doc Connects to the specified database, with specified time-out.
-spec connect( database_connection_settings(), database_user_settings(),
			   time_out() ) -> fallible( connection() ).
connect( ConnSettings=#database_connection_settings{ host_name=HostnameStr,
													 port=MaybePort,
													 name=DbNameStr },
		 UserSettings=#database_user_settings{ user_name=UserName,
											   user_password=UserPassword },
		 TimeOut ) ->

	ActualPort = case MaybePort of

		undefined ->
			?default_database_port;

		Port ->
			Port

	end,

	ConnRes = cond_utils:switch_set_to( myriad_sql_backend, [

		{ sqlite3,
			begin
				basic_utils:ignore_unused(
					[ ConnSettings, HostnameStr, MaybePort, DbNameStr,
					  UserSettings, UserName, UserPassword, TimeOut,
					  ActualPort ] ),
				{ ok, myriad_sqlite3_connection }
			 end },

		{ postgresql,
			epgsql:connect( _Opts=#{ host => HostnameStr,
									port => ActualPort,
									username => UserName,
									password => UserPassword,
									database => DbNameStr,
									timeout => TimeOut } ) },

		{ none,
			begin
				basic_utils:ignore_unused(
					[ ConnSettings, HostnameStr, MaybePort, DbNameStr,
					  UserSettings, UserName, UserPassword, TimeOut,
					  ActualPort ] ),
				throw( no_myriad_sql_backend_enabled )
			 end } ],

		% Default:
		none ),

	cond_utils:if_defined( myriad_debug_sql_support,
		trace_utils:debug_fmt( "Connection attempt to:~n - ~p~n"
			" - as ~p~n reported: ~p",
			[ ConnSettings, UserSettings, ConnRes ] ) ),

	ConnRes.



% @doc Closes the specified database connection.
-spec close( connection() ) -> base_status().
close( Conn ) ->

	cond_utils:if_defined( myriad_debug_sql_support,
		trace_utils:debug_fmt( "Closing connection ~p.", [ Conn ] ) ),

	cond_utils:switch_set_to( myriad_sql_backend, [

		{ sqlite3,
			begin
				basic_utils:ignore_unused( Conn ),
				throw( to_do )
			end },

		{ postgresql, epgsql:close( Conn ) },

		{ none,
			begin
				basic_utils:ignore_unused( Conn ),
				throw( no_myriad_sql_backend_enabled )
			end } ],

		% Default:
		none ).


% @doc Extracts specified field of specified table.
%-spec extract_field( ) ->
%extract_field() ->


% @doc Stops the SQL support.
-spec stop() -> void().
stop() ->
	cond_utils:if_defined( myriad_debug_sql_support,
						   trace_utils:debug( "Stopping the SQL support." ),
						   ok ).
