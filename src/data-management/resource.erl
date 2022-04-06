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
% Creation date: Sunday, April 3, 2022.


% @doc Gathering of facilities for the management of any kind of <b>data
% resources</b>, typically a content (ex: image, sound) read from file, or
% generated as a term, and to be stored in a suitable <b>resource
% referential</b>, possibly made available thanks to a <b>resource server</b>.
%
-module(resource).


-export([ create_referential/0, create_referential/1,
		  get/2, has/2, register/3, remove/2, get_path/2,
		  referential_to_string/1, resource_type_to_string/1,
		  create_server/0, create_server/1,
		  create_linked_server/0, create_linked_server/1 ]).



-type resource() :: term().
% Any resource, often as a binary term (e.g. if loaded from file).


-type resource_file_id() :: bin_file_path().
% An identifier of a file resource, that is a resource that can be read directly
% from a filesystem, as a path either absolute or relative to an (implicit)
% resource root directory. Ex: <<"images/hello.png">>.


-type resource_file_id_string() :: file_path().
% A (plain) string version of a file resource identifier, possibly more
% convenient for the user.


-type any_resource_file_id() :: resource_file_id() | resource_file_id_string().
% Any kind of file resource identifier.


-type resource_logical_id() :: atom().
% A logical, user-introduced resource identifier (e.g. 'my_splash_content'),
% which does not correspond (at least directly) to a resource that can be read
% directly from a filesystem; it is thus opaque, explicitly registered by the
% user, and refer to any term (ex: a generated bitmap).
%
% Using a symbol (an atom) so that it cannot be mixed up with an actual path.


-type resource_id() :: any_resource_file_id() | resource_logical_id().
% Any type of resource identifier.


-type resource_table( I ) :: table( I, resource() ).
% A table referencing resources, based on keys of the specified type.


-type resource_table() :: resource_table( resource_id() ).
% A table referencing resources, based on any kind of resource keys.


% For the resource_referential record:
-include("resource.hrl").

-type resource_referential() :: #resource_referential{}.
% A resource referential, storing resources based on their identifier.
% It is a term in the current process.


-type resource_server_pid() :: pid().
% The PID of a resource server.


-type resource_holder() :: resource_referential() | resource_server_pid().
% Any container of resources.


-export_type([ resource/0,
			   resource_file_id/0, resource_file_id_string/0,
			   any_resource_file_id/0,
			   resource_logical_id/0,
			   resource_id/0,
			   resource_table/0, resource_table/1,
			   resource_referential/0,
			   resource_holder/0 ]).



% Implementation notes:
%
% While a resource referential acts only at the level of a given process, a
% resource server allows to share resources between processes. Thanks to the
% sharing of (large-enough) binaries, the operation makes sense.


% Shorthands:

-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().
-type bin_directory_path() :: file_utils:bin_directory_path().
-type any_directory_path() :: file_utils:any_directory_path().

-type ustring() :: text_utils:ustring().




% Resource Referential section.


% @doc Returns an empty referential, not anchored to a specific root directory.
-spec create_referential() -> resource_referential().
create_referential() ->
	#resource_referential{ root_directory=undefined,
						   table=table:new() }.



% @doc Returns an empty referential that is to operate from the specified root
% directory. File-based resources can then be declared implicitly relatively to
% this directory.
%
-spec create_referential( any_directory_path() ) -> resource_referential().
create_referential( AnyRootDir ) ->

	AbsRootDir = file_utils:ensure_path_is_absolute( AnyRootDir ),

	file_utils:is_existing_directory_or_link( AbsRootDir )
		orelse throw( { non_existing_resource_directory, AbsRootDir } ),

	#resource_referential{
		root_directory=text_utils:ensure_binary( AbsRootDir ),
		table=table:new() }.




% Resource Server section.


% @doc Creates a resource server, and returns its PID.
-spec create_server() -> resource_server_pid().
create_server() ->
	spawn( fun() -> server_init() end ).


% @doc Creates a resource server anchored at the specified root directory, and
% returns its PID.
%
-spec create_server( any_directory_path() ) -> resource_server_pid().
create_server( AnyRootDir ) ->
	spawn( fun() -> server_init( text_utils:ensure_binary( AnyRootDir ) ) end ).



% @doc Creates a linked resource server, and returns its PID.
-spec create_linked_server() -> resource_server_pid().
create_linked_server() ->
	spawn( fun() -> server_init() end ).


% @doc Creates a linked resource server anchored at the specified root
% directory, and returns its PID.
%
-spec create_linked_server( any_directory_path() ) -> resource_server_pid().
create_linked_server( AnyRootDir ) ->
	spawn( fun() -> server_init( text_utils:ensure_binary( AnyRootDir ) ) end ).




% Resource holder API: applies both to referentials and servers.


% @doc Returns the resource content corresponding to the specified (file or
% logical) identifier, based on the specified resource holder.
%
-spec get( resource_id(), resource_referential() ) ->
				{ resource(), resource_referential() };
		 ( resource_id(), resource_server_pid() ) -> resource().
get( RscIdStr, RscHolder ) when is_list( RscIdStr ) ->
	get( text_utils:string_to_binary( RscIdStr ), RscHolder );

% From here, RscId is either binary string or atom:
get( BinRscFileId, RscRef=#resource_referential{ table=RscTable } )
								when is_binary( BinRscFileId ) ->

	case table:lookup_entry( BinRscFileId, RscTable ) of

		{ value, Rsc } ->
			{ Rsc, RscRef };

		key_not_found ->
			BinRscPath = case RscRef#resource_referential.root_directory of

				undefined ->
					BinRscFileId;

				BinRootDir ->
					file_utils:bin_join( BinRootDir, BinRscFileId )

			end,

			file_utils:is_existing_file_or_link( BinRscPath ) orelse
				throw( { resource_not_found,
						 text_utils:binary_to_string( BinRscPath ) } ),

			BinRsc = file_utils:read_whole( BinRscPath ),

			NewRscTable = table:add_entry( BinRscFileId, BinRsc, RscTable ),

			NewRscRef = RscRef#resource_referential{ table=NewRscTable },

			{ BinRsc, NewRscRef }

	end;

get( RscId, RscRef=#resource_referential{ table=RscTable } )
										when is_atom( RscId ) ->

	% For atom identifiers, no resource is ever loaded: either the resource is
	% already registered and thus returned, or this operation fails.
	% Referential is thus const.

	case table:lookup_entry( RscId, RscTable ) of

		{ value, Rsc } ->
			{ Rsc, RscRef };

		key_not_found ->
			throw( { resource_not_found, RscId } )

	end;

% Binary string or atom:
get( RscId, RscSrvPid ) when is_pid( RscSrvPid ) ->
	RscSrvPid ! { get, RscId, self() },
	receive

		{ notifyResource, Rsc } ->
			Rsc

	end.



% @doc Returns whether the resource content corresponding to the specified
% identifier is registered in the specified resource holder.
%
-spec has( resource_id(), resource_holder() ) -> boolean().
% As strings are tolerated:
has( RscIdStr, RscHolder ) when is_list( RscIdStr )->
	has( text_utils:string_to_binary( RscIdStr ), RscHolder );

% Binary string or atom from here:
has( RscId, #resource_referential{ table=RscTable } ) ->
	table:has_entry( RscId, RscTable );

has( RscId, RscSrvPid ) ->
	RscSrvPid ! { has, RscId, self() },
	receive

		{ notifyResourceAvailability, Bool } ->
			Bool

	end.



% @doc Registers explicitly the specified resource, based on the specified
% logical (atom-based) identifier, in the specified resource holder.
%
% Any resource previously registered under the same identifier will be replaced.
%
-spec register( resource_logical_id(), resource(), resource_referential() ) ->
											resource_referential();
			  ( resource_logical_id(), resource(), resource_server_pid() ) ->
											void().
% RscLogId must be an atom:
register( RscLogId, Rsc, RscRef=#resource_referential{ table=RscTable } )
										when is_atom( RscLogId ) ->
	NewRscTable = table:add_entry( RscLogId, Rsc, RscTable ),
	RscRef#resource_referential{ table=NewRscTable };

register( RscLogId, Rsc, RscSrvPid ) when is_atom( RscLogId ) ->
	RscSrvPid ! { register, [ RscLogId, Rsc ] }.




% @doc Removes the specified resource from the specified resource holder..
%
% This resource is just unregistered; for example file-based ones are not
% deleted from the filesystem.
%
-spec remove( any_resource_file_id(), resource_referential() ) ->
										resource_referential();
			( any_resource_file_id(), resource_server_pid() ) -> void().
% As strings are tolerated:
remove( RscIdStr, RscHolder ) when is_list( RscIdStr )->
	remove( text_utils:string_to_binary( RscIdStr ), RscHolder );

% Binary string or atom from here:
remove( RscId, Ref=#resource_referential{ table=RscTable } ) ->
	case table:has_entry( RscId, RscTable ) of

		true ->
			NewRscTable = table:remove_entry( RscId, RscTable ),
			Ref#resource_referential{ table=NewRscTable };

		false ->
			throw( { non_existing_resource, RscId } )

	end;

remove( RscId, RscSrvPid ) ->
	RscSrvPid ! { remove, RscId }.



% @doc Returns the full, absolute path to the file-based resource specified
% through its identifier in the specified resource holder, provided it is
% actually registered.
%
-spec get_path( any_resource_file_id(), resource_holder() ) -> bin_file_path().
% As strings are tolerated:
get_path( RscFileIdStr, RscRef ) when is_list( RscFileIdStr ) ->
	get_path( text_utils:string_to_binary( RscFileIdStr ), RscRef );

% Binary string expected:
get_path( BinRscFileId, #resource_referential{ root_directory=MaybeBinRootDir,
											   table=RscTable } )
								when is_binary( BinRscFileId ) ->

	case table:has_entry( BinRscFileId, RscTable ) of

		true ->
			case MaybeBinRootDir of

				undefined ->
					BinRscFileId;

				BinRootDir ->
					file_utils:bin_join( BinRootDir, BinRscFileId )

			end;

		key_not_found ->
			throw( { resource_not_found, BinRscFileId } )

	end;

get_path( BinRscFileId, RscSrvPid ) when is_binary( BinRscFileId ) ->
	RscSrvPid ! { getPath, BinRscFileId, self() },
	receive

		{ notifyResourcePath, BinPath } ->
			BinPath

	end.



% @doc Returns a textual description of the specified referential.
-spec referential_to_string( resource_referential() ) -> ustring().
referential_to_string( #resource_referential{ root_directory=undefined,
											  table=Rsctable } ) ->
	text_utils:format( "resource referential, not anchored to a specific "
		"root directory, storing ~ts",
		[ resource_table_to_string( Rsctable ) ] );

referential_to_string( #resource_referential{ root_directory=BinRootDir,
											  table=Rsctable } ) ->
	text_utils:format( "resource referential whose root directory is '~ts', "
		"storing ~ts", [ BinRootDir, resource_table_to_string( Rsctable ) ] ).



% @doc Returns a textual description of the specified resource table.
-spec resource_table_to_string( resource_table() ) -> ustring().
resource_table_to_string( Rsctable ) ->
	case table:keys( Rsctable ) of

		[] ->
			"no resource";

		[ RscId ] ->
			text_utils:format( "a single ~ts resource, '~ts'",
							   [ resource_type_to_string( RscId ), RscId ] );

		RscIds ->
			text_utils:format( "~B resources: ~ts", [ length( RscIds ),
				text_utils:strings_to_string( [ text_utils:format(
					"~ts resource '~ts'",
					[ resource_type_to_string( RI ), RI ] )
						|| RI <- RscIds ] ) ] )

	end.



% @doc Returns a textual description of the type of the specified resource.
-spec resource_type_to_string( resource_id() ) -> ustring().
resource_type_to_string( RscId ) when is_atom( RscId ) ->
	"logical";

resource_type_to_string( RscId )
							when is_list( RscId ) orelse is_binary( RscId ) ->
	"file".











% Resource server implementation.

-spec server_init() -> no_return().
server_init() ->
	InitialRef = create_referential(),
	server_main_loop( InitialRef ).


-spec server_init( bin_directory_path() ) -> no_return().
server_init( BinRootDir ) ->
	InitialRef = create_referential( BinRootDir ),
	server_main_loop( InitialRef ).



% Message-based interactions, made according to conventions similar to the
% Ceylan-WOOPER ones.
%
% When an operation fails, no answer is sent to the caller, and the resource
% server fails. As a result, it is better to create linked instances thereof.
%
% (helper)
-spec server_main_loop( resource_referential() ) -> no_return().
server_main_loop( RscRef ) ->

	receive

		% Request:
		{ get, RscId, SenderPid } ->
			{ Rsc, NewRscRef } = get( RscId, RscRef ),
			SenderPid ! { notifyResource, Rsc },
			server_main_loop( NewRscRef );

		% Request:
		{ has, RscId, SenderPid } ->
			Bool = has( RscId, RscRef ),
			SenderPid ! { notifyResourceAvailability, Bool },
			server_main_loop( RscRef );

		% Oneway:
		{ register, [ RscLogId, Rsc ] } ->
			NewRscRef = register( RscLogId, Rsc, RscRef ),
			server_main_loop( NewRscRef );

		% Oneway:
		{ remove, RscId } ->
			NewRscRef = remove( RscId, RscRef ),
			server_main_loop( NewRscRef );

		% Request:
		{ getPath, RscFileId, SenderPid } ->
			BinRscPath = get_path( RscFileId, RscRef ),
			SenderPid ! { notifyResourcePath, BinRscPath },
			server_main_loop( RscRef );

		terminate ->
			ok;

	Other ->
		trace_utils:warning_fmt( "Resource server ~w ignoring message ~p.",
								 [ self(), Other ] ),
		server_main_loop( RscRef )

	end.
