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


% @doc Support for the <b>Gephi</b> facilities.
%
% Gephi is an open source tool to visualise and explore all kinds of graphs and
% networks.
%
% See gephi_support_test.erl for the corresponding test, and the Gephi official
% website [https://gephi.org/] for further information.
%
% For the vast majority of the services defined here, an instance of a Gephi
% server is expected to have been configured and launched beforehand, both
% according to our conventions.
%
-module(gephi_support).


-type gephi_server_host() :: net_utils:possibly_local_hostname().
% To designate the host of a Gephi server.

-type bin_gephi_server_host() :: net_utils:possibly_local_bin_hostname().
% To designate internally the host of a Gephi server.


-type gephi_server_port() :: net_utils:tcp_port().
% A (TCP) port at which a Gephi instance may run.


-type project_path() :: file_path().
% A user-specified path to a Gephi project file, from which a project name may
% be deduced.
%
% For example the "/tmp/foo/my_project.gephi" path is to correspond to the
% "my_project" project.


-type project_name() :: ustring().
% The name of a Gephi project, typically one that shall be loaded. For example,
% a "Foobar" project name would refer to a "Foobar.gephi" project file.


-type bin_project_name() :: bin_string().
% The name of a Gephi project, typically one that shall be loaded. For example,
% a `<<"Foobar">>' project name would refer to a "Foobar.gephi" project file.


-type any_project_name() :: project_name() | bin_project_name().
% The name of a Gephi project, typically one that shall be loaded.



-type gephi_workspace() :: ustring().
% The name of a Gephi workspace (for example "Siclone"), in a project.

-type bin_gephi_workspace() :: bin_string().
% The name of a Gephi workspace, in a project.

-type any_gephi_workspace() :: bin_string().
% The name of a Gephi workspace, in a project.


-type base_url() :: bin_url().
% A precomputed base URL, used to send requests more efficiently.


-record( gephi_server_info, {

	% The host (possibly localhost) on which a suitable Gephi server is expected
	% to run:
	%
	host :: bin_gephi_server_host(),

	% The TCP port on which a suitable Gephi server is expected to run:
	port :: gephi_server_port(),

	% The workspace to interact with, on this Gephi server:
	workspace :: bin_gephi_workspace(),

	% The preprocessed base URL used to trigger calls to this Gephi server:
	base_url :: base_url() } ).

-type gephi_server_info() :: #gephi_server_info{}.
% Information to designate an instance of a Gephi server.


-type element_id() :: any_string().
% The identifier of a graph element.
%
% This is a node, an edge or a property.


-type node_id() :: element_id().
% The identifier of a graph node.

-type edge_id() :: element_id().
% The identifier of a graph edge.

-type property_id() :: element_id().
% The identifier of a node property.


-type element_label() :: any_string().
% A label that can be associated to a graph element.


-type graph_value() :: boolean() | number() | ustring().
% A value associated to a given timestamp in a graph.
%
% It would be interesting to determine whether other datatypes can be accepted
% (e.g. booleans, non-scalar types).


-type timestamp() :: float().
% Possibly in [0.0, 1.0].


-type server_availability() :: 'ok' | 'time_out'.
% Availability outcome regarding a Gephi server lookup.


-export_type([ gephi_server_host/0, bin_gephi_server_host/0,
			   gephi_server_port/0,

			   project_path/0,
			   project_name/0, bin_project_name/0, any_project_name/0,

			   gephi_workspace/0, bin_gephi_workspace/0, any_gephi_workspace/0,

			   base_url/0,
			   gephi_server_info/0,

			   element_id/0, node_id/0, edge_id/0, property_id/0,
			   element_label/0, graph_value/0, timestamp/0,
			   server_availability/0 ]).


% Server management:
-export([ is_available/0,
		  launch_server/1, launch_server/2,
		  launch_server_if_needed/2, launch_server_if_needed/3,
		  wait_server/1, wait_server/2,
		  clean_up_server/2 ]).


% Client-side API:
-export([ get_server_default_tcp_port/0, get_gephi_extension/0,
		  get_project_name_from_path/1,

		  start/0, stop/0,
		  get_server_info/1, get_server_info/2, get_server_info/3,
		  server_info_to_string/1,

		  add_node/3, update_node_property/4, update_node_property/5,
		  add_edge/5, update_edge_property/4, update_edge_property/5 ]).




% Gephi installation:
%
% Our convention is to have Gephi installed in GEPHI_ROOT=~/Software/gephi, in
% which a 'gephi-current-install' symbolic link is to point to an actual sibling
% installation directory corresponding to a given version of Gephi, like:
% 'gephi-0.9.10'.
%
% For example:
% $ tree -d -L 1 ~/Software/gephi/
% ~/Software/gephi/
% ├── gephi-0.9.5
% ├── gephi-0.9.6
% ├── gephi-0.9.10
% └── gephi-current-install -> gephi-0.9.10
%
% Then, gephi can be run in all cases as
% '~/Software/gephi/gephi-current-install/bin/gephi'.
%
% So the installation boils down to selecting the latest stable version of Gephi
% from https://gephi.org/users/download/ (we recommend to avoid the 0.9.6
% version), to download a corresponding gephi-x.y.z-linux-x64.tar.gz archive in
% ~/Software/gephi/, to extract it (e.g. 'tar xvf
% gephi-x.y.z-linux-x64.tar.gz'), and to create/update a sibling
% gephi-current-install symbolic link pointing to it.
%
% The ~/Software/gephi/gephi-current-install/bin directory shall then preferably
% be set in the PATH environment variable for good (e.g. in one's ~/.bashrc).


% Gephi configuration
%
% The following steps are needed:
%
%  - installing the 'Graph Streaming' plugin (listed in Tools -> Plugins ->
%  'Available Plugins'), for example in version 1.0.3
%
%  - configuring a correct TCP port to be listened to by the Gephi server (see
%  the 'Graph streaming server subsection')
%
%  - defining and loading a suitable project file (e.g. 'foobar.gephi')


% Usage notes

% For Gephi use:
%
% A Gephi instance may open a project, which may comprises multiple workspaces.
% Such Gephi instance may additionally launch a graph streaming server.
%
% Launching a suitable Gephi server requires an host (by default the local one)
% and a port (see the gephi_default_tcp_port define for defaults).
%
% Initially Gephi could not be fully launched only from the command-line, as a
% Gephi Master Server had to be started and a project file (typically bearing a
% *.gephi extension) had be selected.
%
% The launch_server/{1,2} functions automate the launching, and the
% wait_server/{1,2} ones will wait until the Gephi server is available (or a
% time-out is reached).
%
% Apparently, depending on some circumstances, if a Gephi instance is already
% running, launching another one may just set the focus on that initial
% instance; then, as a consequence, on a given host, the risk of having
% conflicting instances is low (especially if they are to operate on the same
% TCP port, as up to one of them can be started).
%
% However at least in some other cases, each launch resulted in a new
% application window to appear, only the first one displaying (receiving) the
% sent data. As a consequence, we introduced the launch_if_needed/* functions.
%
% Moreover, using 'killall java' to kill Gephi is effective, yet it does not
% seem instantaneous; for example 'killall java; make gephi_support_run' may
% fail, presumably because the test looked up successfully a Gephi instance
% being terminated then tried to interact with it, but it does not exist anymore
% then.
%
% See also: executable_utils:get_default_graph_stream_tool_{name,path}/0.


% For data files:
%
% We recommend the use of the GraphML graph format, and relying on the
% '.graphml' file extension for that.



% For Gephi-related troubleshooting:
%
% - use the Window -> Output menu item in order to display a log window
%
% - apparently in some cases a Gephi server can become unresponsive; one may use
% 'killall java' to clear any prior instance thereof
%
% - command-line testing can be done for example with:
%    $ wget --post-data "XXX" "localhost:8090/myproject?operation=updateGraph"
%
% For example "XXX" can be a JSON document like:
% "{"cn":{"$NODEID":{"$NAME":"$VALUE"}}}".


% Graph streaming server subsection.
%
% A modified version of Gephi has been devised in order to automatically launch
% a (properly-configured) graph streaming server. Yet these steps can be taken
% care of manually, typically from a vanilla version of Gephi: the user may
% first select a project, thanks to the File -> 'Open' or 'Open Recent' (a
% *.gephi file) menu item.
%
% Then the corresponding Gephi server must be started, its TCP port being
% assigned by selecting first Window -> Streaming, then, on the 'Streaming' tab,
% by clicking the 'Settings' button and, regarding the 'HTTP Server Settings'
% panel, setting the 'Port' to the desired value (the gephi_default_tcp_port
% define is recommended for that).
%
% To start a Gephi server, select in the 'Streaming' tab the 'Master' -> 'Master
% Server' entry, and right-click on it to select 'Start'.
%
% The red point shall become green, and the corresponding TCP port shall be
% listened to; for example: 'ss -an | grep 8080' ('ss' superseding 'netstat')
% may now return information like: 'tcp6 0 0 :::8080 :::* LISTEN 1028/java'.



% Implementation notes:
%
% The communication with the Gephi server of interest can be asynchronous
% (fastest, yet prone to race conditions) or not.
%
% Synchronous operations do not return for example the identifiers involved, as
% a single calling process is generally involved. By itself, at least most
% operations on Gephi are synchronous due to the HTTP (POST) conventions.
%
% A JSON parser is prepared, yet at least most of the requests are best cooked
% directly as strings, rather than to be translated from a term to a JSON
% document.
%
% To model graph values changing over time, either time interval or timestamps
% can be used. Time intervals have a clear semantics and can factor information,
% yet are not very suitable for data streams, for which no time corresponding to
% the end of validity of a value is known.
%
% Buffering would imply a larger memory footprint and lead to values being
% reported lately (a constant value becoming visible only at the end of a
% session) and/or to be sampled, based on arbitrary points. So instead
% timestamps are used.
%
% Then a time is associated to a value. For that the GraphStream plugin has been
% modified in order to:
%  - use values associated to a timestamp through a JETTY server
%  - be launched and configured automatically from a suitable script
%
% If for a property a value is not defined at a given time, our convention is to
% retain the last displayed value for it. That way, browsing backward in time
% will retain the (past) value at t+x rather than the (future) value at t-x.
%
% If for a property multiple values are set at a given time, a choice can be
% made (first, last, min, max, mean, etc.).

% With these conventions, in the Gephi interface, the global "Timestamp"
% property (column) is not used. Our timestamps (as floating-point numbers) are
% stored together with each value taken by a property, as
% <[TIMESTAMP_1,VALUE_1]; [TIMESTAMP_2,VALUE_2]>.




% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type extension() :: file_utils:extension().
-type file_path() :: file_utils:file_path().
-type directory_path() :: file_utils:directory_path().
-type any_directory_path() :: file_utils:any_directory_path().

-type time_out() :: time_utils:time_out().

-type tcp_port() :: net_utils:tcp_port().


-type bin_url() :: web_utils:bin_url().
-type body() :: web_utils:body().



% The (TCP, non-privileged) port at which a Gephi instance runs by default:
-define( gephi_default_tcp_port, 8080 ).


% The file extension of a Gephi project:
-define( gephi_project_extension, "gephi" ).





% Section for Gephi server-side support, in charge of managing (typically
% launching) a Gephi server.


% @doc Tells whether Gephi is available, that is whether its server may be
% available.
%
-spec is_available() -> boolean().
is_available() ->
	executable_utils:lookup_executable(
		executable_utils:get_default_graph_stream_tool_name() ) =/= false.



% @doc Launches in the background (on the local host) a Gephi server, relying on
% the specified project file, using the default user directory.
%
% Note that the server may not be immediately available.
%
-spec launch_server( project_path() ) -> void().
launch_server( ProjectPath ) ->
	launch_server( ProjectPath, _DefaultUserDir="myriad-gephi-user-directory" ).



% @doc Launches in the background (on the local host) a Gephi server, relying on
% the specified user directory and project file.
%
% Note that the server may not be immediately available.
%
-spec launch_server( project_path(), any_directory_path() ) -> void().
launch_server( ProjectPath, UserDir ) ->

	trace_bridge:debug_fmt( "Launching a Gephi server with project path '~ts'.",
						   [ ProjectPath ] ),

	case executable_utils:get_default_graph_stream_tool_name() of

		"gephi" ->
			ok;

		Other ->
			throw( { graph_stream_tool_not_gephi, Other } )

	end,

	GephiPath = executable_utils:get_default_graph_stream_tool_path(),

	% Otherwise 'Cannot find java.' even if in the PATH:

	JavaExecPath = executable_utils:get_default_java_runtime(),

	file_utils:is_existing_file_or_link( ProjectPath ) orelse
		throw( { gephi_project_file_not_found, ProjectPath } ),

	% We have to workaround some Gephi bugs:

	% Apparently project files might be corrupted; using a constant, reference
	% one:

	ActualProjectPath = get_workaround_project_path( ProjectPath ),

	% Overwriting if needed:
	file_utils:copy_file( ProjectPath, ActualProjectPath ),

	% User directories may also become a problem:
	file_utils:remove_directory_if_existing( UserDir ),

	% Remove the trailing "bin/java":
	JavaHome = file_utils:get_base_path( JavaExecPath, _Depth=2 ),

	Args = [ "--jdkhome", JavaHome, "--userdir", UserDir,
			 "--branding", "gephi", "--nosplash", ActualProjectPath ],

	system_utils:run_background_executable( _ExecPath=GephiPath, Args ).



% @doc Launches in the background (on the local host) a Gephi server, relying on
% the specified project file, using the default user directory, only if needed,
% that is if no prior instance thereof is detected.
%
% Note that the server may not be immediately available.
%
-spec launch_server_if_needed( project_path(), gephi_server_info() ) -> void().
launch_server_if_needed( ProjectPath, SrvInfo ) ->
	launch_server_if_needed( ProjectPath,
		_DefaultUserDir="myriad-gephi-user-directory", SrvInfo ).



% @doc Launches in the background (on the local host) a Gephi server, relying on
% the specified user directory and project file, only if needed, that is if no
% prior instance thereof is detected.
%
% Note that the server may not be immediately available.
%
-spec launch_server_if_needed( project_path(), any_directory_path(),
							   gephi_server_info() ) -> void().
launch_server_if_needed( ProjectPath, UserDir, SrvInfo ) ->
	wait_server( SrvInfo, _Timeout=10 ) orelse
		begin
			launch_server( ProjectPath, UserDir ),
			wait_server( SrvInfo ) orelse
				throw( gephi_server_not_found )
		end.


% Note that net_utils:ping/1 could be used to check that any remote server
% responds.


% @doc Waits until the designated (Gephi) server seems available, using a
% default time-out: returns whether it was found available.
%
-spec wait_server( gephi_server_info() ) -> boolean().
wait_server( SrvInfo ) ->
	wait_server( SrvInfo, _DefTimeout=8000 ).


% @doc Waits until the designated (Gephi) server seems available, within the
% specified time-out: returns whether it was found available.
%
-spec wait_server( gephi_server_info(), time_out() ) -> boolean().
wait_server( #gephi_server_info{ host=BinHostname, port=SrvPort }, Timeout ) ->
	net_utils:is_service_running_at( BinHostname, SrvPort, Timeout ).



% @doc Cleans-up the filesystem context of the Gephi server.
-spec clean_up_server( project_path(), any_directory_path() ) -> void().
clean_up_server( ProjectPath, UserDir ) ->
	ActualProjectPath = get_workaround_project_path( ProjectPath ),
	file_utils:remove_file_if_existing( ActualProjectPath ),
	file_utils:remove_directory_if_existing( UserDir ).



-spec get_workaround_project_path( any_directory_path() ) -> directory_path().
get_workaround_project_path( ProjectPath ) ->

	{ BaseProjPath, ProjFilename } = file_utils:split_path( ProjectPath ),

	ActualProjFilename =
		text_utils:format( ".myriad-tmp-~ts", [ ProjFilename ] ),

	file_utils:join( BaseProjPath, ActualProjFilename ).





% Section for Gephi client-side support, for which a Gephi server is expected to
% run.



% @doc Returns the default TCP port expected to be used by a Gephi server.
-spec get_server_default_tcp_port() -> tcp_port().
get_server_default_tcp_port() ->
	?gephi_default_tcp_port.


% @doc Returns the file extension that Gephi projects are expected to use.
-spec get_gephi_extension() -> extension().
get_gephi_extension() ->
	?gephi_project_extension.


% @doc Deduces the Gephi project name from the path of the specified project
% file.
%
-spec get_project_name_from_path( project_path() ) -> project_name().
get_project_name_from_path( ProjectPath ) ->

	ProjectFilename = file_utils:get_last_path_element( ProjectPath ),

	file_utils:remove_extension( ProjectFilename, ?gephi_project_extension ).



% @doc Starts the Gephi (client-side) support.
-spec start() -> void().
start() ->

	% We have to secure here a JSON parser and web facilities; the target Gephi
	% server itself is to defined later, as not having them coupled is a lot
	% more flexible.

	% Runtime resolution of path:
	{ JSONParserName, _JSONResolvablePath, JSONResolvedPath } =
		json_utils:get_parser_name_paths(),

	code_utils:declare_beam_directory( JSONResolvedPath ),

	json_utils:start_parser( JSONParserName ),

	% HTTP (POST) requests will have to be made to the Gephi server:
	web_utils:start().



% @doc Stops the Gephi (client-side) support.
-spec stop() -> void().
stop() ->
	web_utils:stop(),

	% Side-effect: an extra BEAM directory remains declared.
	json_utils:stop_parser().



% @doc Returns relevant information to connect to the specified workspace of a
% Gephi server expected to run on the local host, on the default port.
%
-spec get_server_info( any_gephi_workspace() ) -> gephi_server_info().
get_server_info( Workspace ) ->
	get_server_info( _Hostname=localhost, Workspace ).


% @doc Returns relevant information to connect to the specified workspace of the
% specified Gephi server, expected to run on the specified host, on the default
% port.
%
-spec get_server_info( gephi_server_host(), any_gephi_workspace() ) ->
										gephi_server_info().
get_server_info( Hostname, Workspace ) ->
	get_server_info( Hostname, ?gephi_default_tcp_port, Workspace ).


% @doc Returns relevant information to connect to the specified workspace of the
% specified Gephi server, expected to run on the specified host, on the
% specified port.
%
-spec get_server_info( gephi_server_host(), gephi_server_port(),
					   any_gephi_workspace() ) -> gephi_server_info().
get_server_info( Hostname, ServerPort, Workspace ) ->
	get_server_info( Hostname, ServerPort, Workspace,
					 _DoCheckServer=false ).


% @doc Returns relevant information to connect to the specified workspace of the
% specified Gephi server, expected to run on the specified host, on the
% specified port.
%
% If requested, the availability of the specified host will be checked (with
% ping), provided it is not the local one.
%
-spec get_server_info( gephi_server_host(), gephi_server_port(),
		any_gephi_workspace(), boolean() ) -> gephi_server_info().
get_server_info( Hostname, ServerPort, Workspace, DoCheckServer )
						when is_integer( ServerPort ) ->

	InternHostname = case Hostname of

		localhost ->
			localhost;

		H ->
			DoCheckServer andalso
				begin
					net_utils:ping( H ) orelse
						begin
							trace_bridge:warning_fmt(
								"Unable to ping Gephi host '~ts'.", [ H ] ),
							throw( { unavailable_gephi_host, H } )
						end
				end,

			text_utils:ensure_binary( H )

	end,

	InternWorkspace = text_utils:ensure_binary( Workspace ),

	BaseUrl = text_utils:bin_format( "http://~ts:~B/~ts?operation=updateGraph",
		[ InternHostname, ServerPort, InternWorkspace ] ),

	#gephi_server_info{ host=InternHostname,
						port=ServerPort,
						workspace=InternWorkspace,
						base_url=BaseUrl }.



% @doc Returns a textual description of the specified server information.
-spec server_info_to_string( gephi_server_info() ) -> ustring().
server_info_to_string( #gephi_server_info{ host=Hostname,
										   port=ServerPort,
										   workspace=Workspace } ) ->
										   %base_url=BaseUrl } ) ->
	text_utils:format( "Gephi server running on ~ts, port #~B, "
		"using workspace '~ts'", [ Hostname, ServerPort, Workspace ] ).



% Subsection for client-side operations onto the server.


% @doc Adds a node whose identifier and label are specified, in the context of
% the specified Gephi instance.
%
-spec add_node( node_id(), element_label(), gephi_server_info() ) -> void().
add_node( NodeId, NodeLabel, SrvInfo ) ->

	cond_utils:if_defined( myriad_debug_graph,
		trace_bridge:debug_fmt( "Adding node id '~ts' with label '~ts'.",
							   [ NodeId, NodeLabel ] ) ),

	% "an": add node
	JSONContent = text_utils:format( "{\"an\":{\"~ts\":{\"label\":\"~ts\"}}}",
									 [ NodeId, NodeLabel ] ),

	send_post( JSONContent, SrvInfo ).



% @doc Updates the specified property of the specified node to the specified
% constant (timestamp-less) value.
%
-spec update_node_property( node_id(), property_id(), graph_value(),
							gephi_server_info() ) -> void().
update_node_property( NodeId, PropertyId, PropertyValue, SrvInfo ) ->

	cond_utils:if_defined( myriad_debug_graph,
		trace_bridge:debug_fmt( "Updating for node id '~ts' the property '~ts' "
			"to constant value '~p'.",
			[ NodeId, PropertyId, PropertyValue ] ) ),

	% "cn": change node
	JSONContent = text_utils:format( "{\"cn\":{\"~ts\":{\"~ts\":~p}}}",
									 [ NodeId, PropertyId, PropertyValue ] ),

	send_post( JSONContent, SrvInfo ).



% @doc Updates the specified property of the specified node to the specified
% value for the specified timestamp.
%
-spec update_node_property( node_id(), property_id(), graph_value(),
							timestamp(), gephi_server_info() ) -> void().
update_node_property( NodeId, PropertyId, PropertyValue, Timestamp, SrvInfo ) ->

	cond_utils:if_defined( myriad_debug_graph,
		trace_bridge:debug_fmt( "Updating for node id '~ts' the property '~ts' "
			"to value '~p' at timestamp ~p.",
			[ NodeId, PropertyId, PropertyValue, Timestamp ] ) ),

	% "cn": change node
	JSONContent = text_utils:format(
		"{\"t\":\"~ts\", \"cn\":{\"~ts\":{\"~ts\":~p}}}",
		[ Timestamp, NodeId, PropertyId, PropertyValue ] ),

	send_post( JSONContent, SrvInfo ).



% @doc Adds an edge whose identifier is specified, together with the identifiers
% of the first node and the second one, telling whether it is a directed edge
% (from first node to second one), in the context of the specified Gephi
% instance.
%
-spec add_edge( edge_id(), node_id(), node_id(), boolean(),
				gephi_server_info() ) -> void().
add_edge( EdgeId, FirstNodeId, SecondNodeId, IsDirected, SrvInfo ) ->

	cond_utils:if_defined( myriad_debug_graph,
		trace_bridge:debug_fmt( "Adding a (directed: ~ts) edge of id '~ts' "
			"between nodes '~ts' and '~ts'.",
			[ IsDirected, EdgeId, FirstNodeId, SecondNodeId ] ) ),

	% "ae": add edge
	JSONContent = text_utils:format(
		"{\"ae\":{\"~ts\":{\"source\":\"~ts\", \"target\":\"~ts\", "
		"\"directed\": ~ts}}}",
		[ EdgeId, FirstNodeId, SecondNodeId, IsDirected ] ),

	send_post( JSONContent, SrvInfo ).



% @doc Updates the specified property of the specified edge to the specified
% constant (timestamp-less) value.
%
-spec update_edge_property( edge_id(), property_id(), graph_value(),
							gephi_server_info() ) -> void().
update_edge_property( EdgeId, PropertyId, PropertyValue, SrvInfo ) ->

	cond_utils:if_defined( myriad_debug_graph,
		trace_bridge:debug_fmt( "Updating for edge id '~ts' the property '~ts' "
			"to constant value '~p'.",
			[ EdgeId, PropertyId, PropertyValue ] ) ),

	% "ce": change edge
	JSONContent = text_utils:format( "{\"cn\":{\"~ts\":{\"~ts\":~p}}}",
									 [ EdgeId, PropertyId, PropertyValue ] ),

	send_post( JSONContent, SrvInfo ).



% @doc Updates the specified property of the specified edge to the specified
% value for the specified timestamp.
%
-spec update_edge_property( edge_id(), property_id(), graph_value(),
							timestamp(), gephi_server_info() ) -> void().
update_edge_property( EdgeId, PropertyId, PropertyValue, Timestamp, SrvInfo ) ->

	cond_utils:if_defined( myriad_debug_graph,
		trace_bridge:debug_fmt( "Updating for edge id '~ts' the property '~ts' "
			"to value '~p' at timestamp ~p.",
			[ EdgeId, PropertyId, PropertyValue, Timestamp ] ) ),

	% "ce": change edge
	JSONContent = text_utils:format(
		"{\"t\":\"~ts\", \"cn\":{\"~ts\":{\"~ts\":~p}}}",
		[ Timestamp, EdgeId, PropertyId, PropertyValue ] ),

	send_post( JSONContent, SrvInfo ).







% Helper section.


% @doc Sends a POST HTTP "request" with the specified content to the specified
% server.
%
% The content is generally a JSON document.
%
% The returned body is generally a JSON document as well. Actually it is at
% least usually the input body, in a reinterpreted form,
% e.g. `<<"{\"an\":{\"myriad-node-id-1\":{\"label\":\"I am the label of the node
% whose identifier is 'myriad-node-id-1'.\"}}}\r\n">>'.
%
% Throws an exception if the sending failed.
%
-spec send_post( body(), gephi_server_info() ) -> body().
send_post( Body, #gephi_server_info{ base_url=BaseUrl } ) ->

	%trace_bridge:debug_fmt( "Body: ~p.", [ Body ] ),

	case web_utils:post( _Uri=BaseUrl, _Header=[], _HttpOptions=[], Body ) of

		{ error, Reason } ->
			throw( { send_post_failed, Reason } );

		% Example of headers:
		%
		% #{ <<"connection">> => <<"close">>,
		%    <<"content-type">> => <<"text/plain">>,
		%    <<"date">> => <<"Thu, 04 Jan 2024 12:16:59 GMT">>,
		%    <<"last-modified">> => <<"Thu, 04 Jan 2024 12:16:59 GMT">>,
		%    <<"server">> => <<"Gephi/0.7 alpha4">>}
		%
		{ _HTTPStatusCode=200, _Headers, BinBody } ->
			%trace_bridge:debug_fmt( "Send POST to Gephi server succeeded "
			%   "(got headers: ~p, ~nbody: ~p).", [ Headers, BinBody ] ),
			BinBody

	end.
