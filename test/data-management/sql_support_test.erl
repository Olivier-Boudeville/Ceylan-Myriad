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



% @doc Unit tests for the <b>SQL support</b>, based on available backend(s).
%
% See the sql_support tested module.
%
-module(sql_support_test).



% For the database_connection_settings and database_user_settings records:
-include("sql_support.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").



% If using the sqlite3 backend
%
% One may use 'make sql_support_run && sqlitebrowser sql_support_test.sqlite3'
% to have a look at the SQLite result (if having commented out the file removal
% at the end).



% @doc Returns the connection and user settings (if any) to apply to this test.
get_test_settings() ->

	% Optional; will look-up the default '~/.ceylan-settings.etf' preference
	% file:
	%
	preferences:start_link(),

	% Depending on the context, any of the preferences can be 'undefined':

	[ DbHostname, DbPort, DbName ] = preferences:get(
		[ myriad_test_sql_database_host, myriad_test_sql_database_port,
		  myriad_test_sql_database_name ] ),

	[ DbUserName, DbUserPassword ] = preferences:get(
		[ myriad_test_sql_user_name, myriad_test_sql_user_password ] ),

	% Testing the preferences that are required:
	case basic_utils:are_all_defined(
			[ DbHostname, DbName, DbUserName, DbUserPassword ] ) of

		true ->
			ConnSettings = #database_connection_settings{ host_name=DbHostname,
														  port=DbPort,
														  name=DbName },

			UserSettings = #database_user_settings{
								user_name=DbUserName,
								user_password=DbUserPassword },

			{ ConnSettings, UserSettings };

		false ->
			undefined

	end.



% To be integrated in sql_support API.
test_sqlite3() ->

	test_facilities:display( "Testing the direct use of the SQLite3 backend." ),

	% Preparing all elements necessary for the dataset creation:

	DatabaseName = "sql_support_test",

	DatabaseFilename = DatabaseName ++ ".sqlite3",

	test_facilities:display( "We will first create a new SQLite3 database, "
							 "in '~ts'.", [ DatabaseFilename ] ),

	% Prior tests may have left it:
	file_utils:remove_file_if_existing( DatabaseFilename ),

	{ ok, DbPid } = sqlite3:open( my_db, [ { file, DatabaseFilename } ] ),

	% Shall be empty at first:
	[] = sqlite3:list_tables( DbPid ),

	% Directly inspired from sqlite3_test:basic_functionality/0:
	test_facilities:display( "Creating a table named 'my_table'." ),

	% Four columns defined:
	ColumnInfo = [ { id,   integer, [ {primary_key, [ asc, autoincrement ]} ] },
				   { name, text,    [ not_null, unique ] },
				   { age,  integer, [ not_null ] },
				   { wage, integer } ],

	ok = sqlite3:create_table( DbPid, my_table, ColumnInfo ),


	test_facilities:display( "Checking table list." ),
	[ my_table, sqlite_sequence ] = sqlite3:list_tables( DbPid ),

	ColumnInfo = sqlite3:table_info( DbPid, my_table ),

	test_facilities:display( "Writing a few rows in this table." ),

	% First writing:
	{ rowid, 1 } = sqlite3:write( DbPid, my_table,
						[ {name,"abby"}, {age, 20}, {<<"wage">>, 2000} ] ),

	% Second one:
	MargeEntry = [ {name, "marge"}, {age, 30}, {wage, 2000} ],
	{ rowid, 2 } = sqlite3:write( DbPid, my_table, MargeEntry ),

	% Only one should be accepted:
	{ error, 19, _ } = sqlite3:write( DbPid, my_table, MargeEntry ),

	Columns = [ "id", "name", "age", "wage" ],

	AbbyRow = { 1, <<"abby">>, 20, 2000 },
	AllRows = [ AbbyRow, { 2, <<"marge">>, 30, 2000 } ],

	test_facilities:display( "Checking the content table now." ),
	ExpectedContent = [ {columns,Columns}, {rows,AllRows} ],

	ExpectedContent = sqlite3:sql_exec( DbPid, "select * from my_table;" ),

	ExpectedContent = sqlite3:read_all( DbPid, my_table ),

	% Many tests could be added here.

	test_facilities:display( "Closing database." ),
	ok = sqlite3:close( DbPid ),

	file_utils:remove_file( DatabaseFilename ).



test_myriad_sql_support() ->

	test_facilities:display(
		"Testing the actual use of our sql_support module." ),

	case get_test_settings() of

		undefined ->
			test_facilities:display( "No sufficient settings found in "
				"preferences, no test done." );

		{ ConnSettings, UserSettings } ->
			test_sql( ConnSettings, UserSettings )

	end.



test_sql( ConnSettings, UserSettings ) ->

	test_facilities:display( "Connecting:~n - to: ~p~n - as: ~p",
							 [ ConnSettings, UserSettings ] ),

	Conn = case sql_support:connect( ConnSettings, UserSettings ) of

		{ ok, C } ->
			C;

		{ error, Reason } ->
			throw( { connection_failed, Reason, ConnSettings, UserSettings } )

	end,

	test_facilities:display( "Connection established (~p).", [ Conn ] ),

	ok = sql_support:close( Conn ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing availability of known backends: ~ts",
		[ text_utils:strings_to_string( [ text_utils:format( "~ts: ~ts",
				[ BN, sql_support:has_backend( BN ) ] )
					|| BN <- sql_support:list_possible_backend_names() ] ) ] ),

	BackendType = sql_support:get_backend_name(),

	test_facilities:display( "Starting SQL support with backend '~ts'.",
							 [ BackendType ] ),

	sql_support:start(),

	case BackendType of

		sqlite3 ->
			test_sqlite3();

		_ ->
			test_myriad_sql_support()

	end,

	test_facilities:display( "Stopping SQL support." ),
	sql_support:stop(),


	test_facilities:stop().
