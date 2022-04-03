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


% @doc Gathering of facilities for the management of any kind of
% <b>resources</b>, typically a content (ex: image, sound) read from file.
%
-module(resource).


-export([ create_referential/1, get/2, referential_to_string/1 ]).



-type resource() :: binary().
% Any (loaded) resource, as binary term.


-type resource_file_id() :: bin_file_path().
% An identifier of a resource, as a path relative to an (implicit) resource root
% directory. Ex: <<"images/hello.png">>.


-type resource_table( I ) :: table( I, resource() ).
% A table referencing resources, based on keys of the specified type.


-type resource_table() :: resource_table( resource_file_id() ).
% A table referencing resources, based on file-based keys.


% For the resource_referential record:
-include("resource.hrl").

-type resource_referential() :: #resource_referential{}.


-export_type([ resource/0, resource_file_id/0,
			   resource_table/0, resource_table/1,
			   resource_referential/0 ]).





% Implementation notes:
%


% Shorthands:

-type bin_file_path() :: file_utils:bin_file_path().
-type any_directory_path() :: file_utils:any_directory_path().

-type ustring() :: text_utils:ustring().



% @doc Returns an empty referential that is to operate from the specified root
% directory.
%
-spec create_referential( any_directory_path() ) -> resource_referential().
create_referential( AnyRootDir ) ->
	BinRootDir = text_utils:ensure_binary( AnyRootDir ),

	file_utils:is_existing_directory_or_link( BinRootDir )
		orelse throw( { non_existing_resource_directory, AnyRootDir } ),

	#resource_referential{ root_directory=BinRootDir,
						   table=table:new() }.



% @doc Returns the resource content corresponding to the specified identifier,
% together with a possibly updated referential.
%
-spec get( resource_file_id(), resource_referential() ) ->
				{ resource(), resource_referential() }.
get( RscFileId, RscRef=#resource_referential{ table=RscTable } ) ->
	case table:lookup_entry( RscFileId, RscTable ) of

		{ value, BinRsc } ->
			{ BinRsc, RscRef };

		key_not_found ->
			BinRscPath = file_utils:bin_join(
				RscRef#resource_referential.root_directory, RscFileId ),

			file_utils:is_existing_file_or_link( BinRscPath ) orelse
				throw( { resource_not_found,
						 text_utils:binary_to_string( BinRscPath ) } ),

			BinRsc = file_utils:read_whole( BinRscPath ),

			NewRscTable = table:add_entry( RscFileId, BinRsc, RscTable ),

			NewRscRef = RscRef#resource_referential{ table=NewRscTable },

			{ BinRsc, NewRscRef }

	end.



% @doc Returns a textual description of the specified referential.
-spec referential_to_string( resource_referential() ) -> ustring().
referential_to_string( #resource_referential{ root_directory=BinRootDir,
											  table=Rsctable } ) ->

	RscStr = case table:keys( Rsctable ) of

		[] ->
			"no resource";

		[ RscId ] ->
			text_utils:format( "a single resource, '~ts'", [ RscId ] );

		RscIds ->
			text_utils:format( "~B resources: ~ts", [ length( RscIds ),
				text_utils:strings_to_string( RscIds ) ] )

	end,

	text_utils:format( "resource referential whose root directory is '~ts', "
		"storing ~ts", [ BinRootDir, RscStr ] ).
