% Copyright (C) 2023-2024 Olivier Boudeville
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
% Creation date: Wednesday, December 20, 2023.


% @doc Unit tests for the gephi_support toolbox.
%
% See the gephi_support tested module.
%
-module(gephi_support_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Silencing:
-export([ launch_server_unconditionally/0, launch_server_if_needed/0 ]).


-define( gephi_project_path, "test-project.gephi" ).

%-define( gephi_workspace, "Test workspace" ).
-define( gephi_workspace, "siclone" ).


launch_server_unconditionally() ->

	test_facilities:display(
		"Testing server management, by launching it unconditionally." ),

	% Before, a source ProjectPath may be copied, and UserDir may be removed:
	gephi_support:launch_server( ?gephi_project_path ),

	SrvInfo = gephi_support:get_server_info( ?gephi_workspace ),

	case gephi_support:wait_server( SrvInfo ) of

		true ->
			SrvInfo;

		false ->
			throw( gephi_test_server_not_found )

	end.



launch_server_if_needed() ->

	test_facilities:display(
		"Testing server management, by launching it if needed." ),

	SrvInfo = gephi_support:get_server_info( ?gephi_workspace ),

	% Before, a source ProjectPath may be copied, and UserDir may be removed:
	gephi_support:launch_server_if_needed( ?gephi_project_path, SrvInfo ),

	SrvInfo.



test_server( SrvInfo ) ->

	test_facilities:display( "Testing the client-side API." ),

	gephi_support:start(),


	test_facilities:display( "Testing first nodes." ),

	FirstNodeId = "myriad-node-id-1",

	FirstNodeLabel = text_utils:format(
		"I am the label of the node whose identifier is '~ts'.",
		[ FirstNodeId ] ),

	gephi_support:add_node( FirstNodeId, FirstNodeLabel, SrvInfo ),


	FirstNodePropertyId = "My first node property",

	FirstTimestamp = "40.0",

	gephi_support:update_node_property( FirstNodeId, FirstNodePropertyId,
		_PropertyValue=25, FirstTimestamp, SrvInfo ),


	SecondNodePropertyId = "My second node property",

	gephi_support:update_node_property( FirstNodeId, SecondNodePropertyId,
		_PropValue=10, SrvInfo ),


	SecondTimestamp = "41.0",

	gephi_support:update_node_property( FirstNodeId, FirstNodePropertyId,
		_PValue=26, SecondTimestamp, SrvInfo ),


	SecondNodeId = "myriad-node-id-2",

	SecondNodeLabel = text_utils:format(
		"I am the label of the node whose identifier is '~ts'.",
		[ SecondNodeId ] ),

	gephi_support:add_node( SecondNodeId, SecondNodeLabel, SrvInfo ),


	test_facilities:display( "Testing second edges." ),

	FirstEdgeId = "myriad-edge-id-1",
	FirstEdgePropertyId = "My first edge property",

	gephi_support:add_edge( FirstEdgeId, FirstNodeId, SecondNodeId,
							_IsDirected=true, SrvInfo ),

	gephi_support:update_edge_property( FirstEdgeId, FirstEdgePropertyId,
		_EPValue=-2, SrvInfo ),

	SecondEdgePropertyId = "My second edge property",
	gephi_support:update_edge_property( FirstEdgeId, SecondEdgePropertyId,
		_EPV=-1, SecondTimestamp, SrvInfo ),

	gephi_support:stop().




-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	%SrvInfo = launch_server(),
	SrvInfo = launch_server_if_needed(),

	test_facilities:display( "Server information: ~ts.",
		[ gephi_support:server_info_to_string( SrvInfo ) ] ),

	test_server( SrvInfo ),

	test_facilities:stop().
