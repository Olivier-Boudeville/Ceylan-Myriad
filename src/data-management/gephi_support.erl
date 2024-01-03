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
% website https://gephi.org/ for further information.
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
% For example the '/tmp/foo/my_project.gephi' path is to correspond to the
% 'my_project' project.
%
%

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

	% The workspace to interact with on this Gephi server:
	workspace :: bin_gephi_workspace(),

	% The preprocessed base URL used to trigger calls to this Gephi server:
	base_url :: base_url() } ).

-type gephi_server_info() :: #gephi_server_info{}.
% To designate an instance of a Gephi server.


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


-type graph_value() :: float().
% A value associated to a given timestamp in a graph.
%
% It would be interesting to determine whether other datatypes can be accepted
% (e.g. booleans, integers, non-scalar types).


-type time_factor() :: float().
% Typically in [0.0, 1.0].



-export_type([ gephi_server_host/0, bin_gephi_server_host/0,
			   gephi_server_port/0,

			   project_path/0,
			   project_name/0, bin_project_name/0, any_project_name/0,

			   gephi_workspace/0, bin_gephi_workspace/0, any_gephi_workspace/0,

			   base_url/0,
			   gephi_server_info/0,

			   element_id/0, node_id/0, edge_id/0, property_id/0,
			   element_label/0, graph_value/0, time_factor/0 ]).


-export([ start/0, stop/0,
		  get_server_info/1, get_server_info/2, get_server_info/3,
		  server_info_to_string/1 ]).


% Gephi installation:
%
% Our convention is to have Gephi installed in GEPHI_ROOT=~/Software/gephi, in
% which a 'gephi-current-install' symbolic link is to point to an actual sibling
% installation directory with a version, like: 'gephi-0.9.10'; for example:
%
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
% gephi-x.y.z-linux-x64.tar.gz'), and to create a sibling gephi-current-install
% symbolic link pointing to it.
%
% The ~/Software/gephi/gephi-current-install/bin directory shall then preferably
% be set in the PATH environment variable for good (e.g. in one's .bashrc).


% Gephi configuration:
%
% The following steps are needed:
%  - installing the 'Graph Streaming' plugin (listed in Tools -> Plugins ->
%  'Available Plugins'), for example in version 1.0.3
%  - configuring a correct TCP port to be listened to by the Gephi server (see
%  the comments of run_gephi/0 for that)
%  - defining and loading a suitable project file (e.g. 'foobar.gephi')


% Usage notes

% For Gephi use:
%
% Refer to the comments in run_gephi/0.
%
% Launching a suitable Gephi server requires an host (by default the local one)
% and a port (see the gephi_default_tcp_port define for defaults).
%
% Initially Gephi could not be fully launched only from the command-line, as a
% Gephi Master Server had to be started and a project file (typically bearing a
% *.gephi extension) had be selected.
%
% The run_gephi/* functions automate the launching as much as currently
% possible.
%
% If a Gephi instance is already running, launching another one just sets the
% focus on that initial instance; as a consequence, on a given host, the risk of
% having conflicting instances is low (especially if they are to operate on the
% same TCP port, as up to one can be started).
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
% For example "XXX" can be a JSON document JSON like
% "{"cn":{"$NODEID":{"$NAME":"$VALUE"}}}".

% A Gephi server could be detected and waited for, by polling its expected TCP
% port (especially useful to wait for the launch of the tool).


% Implementation notes:
%
% The communication with the Gephi server of interest may be asynchronous
% (fastest, yet prone to race conditions) or not.
%
% Synchronous operations do not return for example the identifiers involved, as
% a single calling process is generally involved.




% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type file_path() :: file_utils:file_path().

-type bin_url() :: web_utils:bin_url().
-type body() :: web_utils:body().



% The (TCP, non-privileged) port at which a Gephi instance runs by default:
-define( gephi_default_tcp_port, 8080 ).


% The file extension of a Gephi project:
-define( gephi_project_extension, "gephi" ).



% @doc Starts the Gephi support.
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

	% HTTP POST requests will have to be made to the Gephi server:
	web_utils:start().


% @doc Stops the Gephi support.
-spec stop() -> void().
stop() ->
	web_utils:stop(),

	% Side-effect: extra BEAM directory declared.
	json_utils:stop_parser().



% @doc Returns relevant information to connect to the specified workspace of a
% Gephi server expected to run on the local host on the default port.
%
-spec get_server_info( any_gephi_workspace() ) -> gephi_server_info().
get_server_info( Workspace ) ->
	get_server_info( _Hostname=localhost, Workspace ).


% @doc Returns relevant information to connect to the specified workspace of the
% specified Gephi server, expected to run on the specified host on the default
% port.
%
-spec get_server_info( gephi_server_host(), any_gephi_workspace() ) ->
										gephi_server_info().
get_server_info( Hostname, Workspace ) ->
	get_server_info( Hostname, ?gephi_default_tcp_port, Workspace ).


% @doc Returns relevant information to connect to the specified workspace of the
% specified Gephi server, expected to run on the specified host on the specified
% port.
%
-spec get_server_info( gephi_server_host(), gephi_server_port(),
					   any_gephi_workspace() ) -> gephi_server_info().
get_server_info( Hostname, ServerPort, Workspace ) ->
	get_server_info( Hostname, ServerPort, Workspace,
					 _DoCheckServer=false ).


% @doc Returns relevant information to connect to the specified workspace of the
% specified Gephi server, expected to run on the specified host on the specified
% port.
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
					net_utils:ping( BinHostname ) orelse
						begin
							trace_bridge:warning_fmt(
								"Unable to ping Gephi host '~ts'.", [ H ] )
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




% Helper section.


% @doc Sends a POST HTTP "request" with the specified content to the specified
% server.
%
% The content is generally a JSON document.
%
% Throws an exception if the sending failed.
%
-spec send_post( body(), gephi_server_info() ) -> body().
send_post( Body, #gephi_server_info{ base_url=BaseUrl } ) ->
	case web_utils:post( _Uri=BaseUrl, _Header=[], _HttpOptions=[], Body ) of

		{ error, Reason } ->
			throw( { send_post_failed, Reason } );

		{ _HTTPStatusCode=200, Headers, BinBody } ->
			trace_utils:debug_fmt(
				"Send POST succeeded (headers: ~p, body: ~w).",
				[ Headers, BinBody ] ),
			BinBody

	end.
