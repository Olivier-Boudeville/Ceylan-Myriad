% Copyright (C) 2003-2013 Olivier Boudeville
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: July 1, 2007.


% Gathering of various convenient facilities.
% See net_utils_test.erl for the corresponding test.
-module(net_utils).




% Hostname-related functions.
-export([ ping/1, reverse_lookup/1, localhost/0 ]).


% Node-related functions.
-export([ localnode/0, get_all_connected_nodes/0,
		 check_node_availability/1, check_node_availability/2,
		 get_node_naming_mode/0, get_naming_compliant_hostname/2,
		 generate_valid_node_name_from/1, get_fully_qualified_node_name/3,
		 shutdown_node/1 ]).


% Net-related command line options.
-export([ get_cookie_option/0, get_epmd_option/1, get_node_name_option/2,
		 get_tcp_port_range_option/1, get_basic_node_launching_command/5 ]).


% Address-related functions.
-export([ ipv4_to_string/1, ipv4_to_string/2 ]).




% Hostname-related functions.


% Pings specified hostname, and returns true iff it could be ping'd.
% Note: command-line based call, used that way as there is no ICMP stack.
% A port could be used also.
ping(Hostname) when is_list(Hostname) ->

	Command = "if ping " ++ Hostname ++ " -q -c 1 1>/dev/null 2>&1; "
		"then echo ping_ok ; else echo ping_failed ; fi",

	%io:format( "Ping command: ~s~n.", [Command] ),

	case os:cmd( Command ) of

		"ping_ok\n" ->
			true ;

		"ping_failed\n" ->
			false

	end.



% Returns an appropriate DNS name for the local host, or throws an exception.
localhost() ->
	% Depending on the node being launched with either:
	%  - no network name or a short name
	%  - a long name
	% net_adm:localhost() may return respectively "XXX.domain.com" or
	% "XXX.localdomain", both of which are not proper hostnames.
	% On the other hand, "hostname -f" might return 'localhost.localdomain'.
	% Most reliable (ending carriage return must be removed):
	case text_utils:remove_ending_carriage_return( os:cmd( "hostname -f" ) ) of

		"localhost" ->
			throw( could_not_determine_localhost );

		"localhost.localdomain" ->
			throw( could_not_determine_localhost );

		Other ->
			Other

	end.



% Returns a string specifying the DNS name corresponding to the specified IP
% address {N1,N2,N3,N4}.
reverse_lookup( IPAddress ) ->
	Command = "host -W 1 " ++ ipv4_to_string(IPAddress) ++ " 2>/dev/null",
	Res = os:cmd( Command ),
	%io:format( "Host command: ~s, result: ~s.~n", [Command,Res] ),
	case string:tokens( Res," " ) of

		[ _ArpaString, "domain", "name", "pointer", Domain ] ->
			% Removes ending ".~n":
			string:sub_string( Domain, 1, length(Domain)-2 );

		_Other  ->
			unknown_dns

	end.




% Node-related functions.


% Returns the name of the local node, as an atom.
%
% It is either a specific node name, or the atom 'local_node' (preferred to
% 'nonode@nohost').
localnode() ->

	case node() of

		nonode@nohost ->
			local_node;

		OtherNodeName ->
			% Could be XX@myhost.example.com:
			OtherNodeName

	end.



% Returns the list of all connected nodes (each being designated by an atom,
% like 'foo@bar.org'), including the local node.
get_all_connected_nodes() ->
	[node()|nodes()].



% Returns whether specified Erlang node is available, waiting a bit (up to 3.1
% seconds) should the node need some time to come up.
%
% Nodename can be an atom or a string.
%
% Performs a fixed number of attempts with some exponential waiting in-between,
% in case the node is being launched in the background.
%
% Durations are in milliseconds, maximum waiting time is 3.1 seconds.
%
% Allows to return as soon as possible.
check_node_availability( Nodename ) ->
	check_node_availability( Nodename, with_waiting ).



% Returns whether specified Erlang node is available:

% - Nodename is an atom or a string corresponding to the name of the target node
%
% - Timing is either 'immediate' or 'with_waiting'. If 'immediate', the target
% node will be deemed available or not as soon as the first and only ping
% attempted returns a result.  If ' with_waiting', a fixed number of attempts
% with some exponential waiting in-between will be performed.
%
% This is useful so that, if the node is being launched in the background, it is
% waited for while returning as soon as possible.
%
% Durations are in milliseconds, maximum waiting time is:
%
%  - 3.1  seconds if initial attempt count is 5
%  - 6.3  seconds if initial attempt count is 6
%  - 12.7 seconds if initial attempt count is 7
%  - 25.5 seconds if initial attempt count is 8
%
% The safe setting seems an attempt count of 6.
check_node_availability( Nodename, Timing ) when is_list(Nodename) ->
	check_node_availability( list_to_atom(Nodename), Timing ) ;

check_node_availability( Nodename, _Timing=immediate )
		when is_atom(Nodename) ->

	case net_adm:ping( Nodename ) of

		pong ->
			true ;

		pang ->
			false

	end;

check_node_availability( Nodename, _Timing=with_waiting )
		when is_atom(Nodename) ->

	%io:format( "check_node_availability of node '~s' with waiting.~n",
	%		  [Nodename] ),

	check_node_availability( Nodename, _AttemptCount=6, _InitialDuration=100 ).


check_node_availability( _AtomNodename, _Count=0, _CurrentDuration ) ->
	false ;

check_node_availability( AtomNodename, AttemptCount, CurrentDuration ) ->

	case net_adm:ping( AtomNodename ) of

		pong ->
			true ;

		pang ->
			%io:format( "Sleeping for ~B ms.~n", [CurrentDuration] ),
			timer:sleep(CurrentDuration),
			check_node_availability( AtomNodename, AttemptCount-1,
				2*CurrentDuration )

	end.



% Returns the naming mode of this node, either 'short_name' or 'long_name'.
get_node_naming_mode() ->

	% We determine the mode based on the returned node name:
	% (ex: 'foo@bar' vs 'foo@bar.baz.org')
	[_Node,Host] = string:tokens( atom_to_list(node()), "@" ),
	case length( string:tokens( Host, "." ) ) of

		1 ->
			short_name;

		TwoOrMore when TwoOrMore > 1 ->
			long_name

	end.



% Returns a transformed version of the specified hostname so that it is
% compliant with the specified node naming convention.
%
% For example, if the short_name convention is specified, then a "bar.baz.org"
% hostname will result into "bar".
get_naming_compliant_hostname( Hostname, short_name ) ->
	hd( string:tokens( Hostname, "." ) );

get_naming_compliant_hostname( Hostname, long_name ) ->
	Hostname.


% Returns a name that is a legal name for an Erlang node, forged from specified
% one.
generate_valid_node_name_from( Name ) when is_list(Name) ->

	% Replaces each series of spaces (' '), lower than ('<'), greater than
	% ('>'), comma (','), left ('(') and right (')') parentheses, single (''')
	% and double ('"') quotes, forward ('/') and backward ('\') slashes,
	% ampersand ('&'), tilde ('~'), sharp ('#'), at sign ('@'), all other kinds
	% of brackets ('{', '}', '[', ']'), pipe ('|'), dollar ('$'), star ('*'),
	% marks ('?' and '!'), plus ('+'), other punctation signs (';', '.' and ':')
	% by exactly one underscore:
	%
	% (see also: file_utils:convert_to_filename/1)
	re:replace( lists:flatten(Name),
			   "( |<|>|,|\\(|\\)|'|\"|/|\\\\|\&|~|"
			   "#|@|{|}|\\[|\\]|\\||\\$|\\*|\\?|!|\\+|;|\\.|:)+", "_",
		 [global,{return, list}] ).



% Returns the full name of a node, which has to be used to target it from
% another node, with respect to the specified node naming conventions.
%
% Ex: for a node name 'foo', a hostname 'bar.org', with short names, we may
% specify 'foo@bar' to target the corresponding node with these conventions (not
% a mere 'foo', neither 'foo@bar.org').
%
% Both parameters must be plain strings.
get_fully_qualified_node_name( NodeName, Hostname, NodeNamingMode ) ->
	NodeName ++ "@"
		++ get_naming_compliant_hostname( Hostname, NodeNamingMode ).



% Shutdowns specified node, and returns only when it cannot be ping'ed anymore:
% it is a safe and synchronous operation.
%
% Throws an exception if not able to terminate it.
shutdown_node(Nodename) when is_list(Nodename) ->
	shutdown_node( list_to_atom(Nodename) );

shutdown_node(Nodename)	when is_atom(Nodename) ->
	rpc:cast( Nodename, erlang, halt, [] ),
	wait_unavailable( Nodename, _AttemptCount=5, _Duration=100 ).


wait_unavailable( Nodename, _AttemptCount=0, _Duration ) ->
	throw( {node_not_terminating,Nodename} );

wait_unavailable( Nodename, AttemptCount, Duration ) ->
	case net_adm:ping( Nodename ) of

		pong ->
			timer:sleep(Duration),
			wait_unavailable( Nodename, AttemptCount-1, 2*Duration );

		pang ->
			% Safety delay to ensure the node had time to fully shut down and
			% to unregister from everything:
			timer:sleep(500),
			ok

	end.




% Net-related command line options.


% Returns the command-line option (a plain string) to be used to run a new
% Erlang node with the same cookie as the current node, whether or not it is
% alive.
get_cookie_option() ->
	case erlang:get_cookie() of

		nocookie ->
			"";

		Cookie ->
			"-setcookie '" ++ atom_to_list( Cookie ) ++ "'"

	end.



% Returns the command-line option (a plain string) to be used to run a new
% Erlang node with the specified EPMD port specification, with can be either the
% 'undefined' atom or the port number.
%
% Note that if a non-default EPMD port is specified for a new node, this implies
% that the current node usually has to itself respect the same non-standard
% convention (ex: see the FIREWALL_OPT make option in common/GNUmakevars.inc),
% otherwise available nodes will not be found.
%
get_epmd_option( undefined ) ->
	"";

get_epmd_option( EpmdPort ) when is_integer(EpmdPort) ->
	io_lib:format( "ERL_EPMD_PORT=~B", [EpmdPort] ).



% Returns the command-line option (a plain string) to be used to run a new
% Erlang node with the node name and node naming mode (short or long name).
get_node_name_option( NodeName, NodeNamingMode ) ->

	NodeNameOption = case NodeNamingMode of

		  short_name ->
			 "-sname";

		  long_name ->
			 "-name"

	end,
	NodeNameOption ++ " " ++ NodeName.



% Returns the command-line option (a plain string) to be used to run a new
% Erlang node with the specified TCP port restriction, with can be either the
% 'no_restriction' atom or a pair of integers {MinTCPPort,MaxTCPPort}.
%
% If using a specific TCP/IP port range for a new node, the current node may
% have to respect this constraint as well (see the FIREWALL_OPT make option in
% common/GNUmakevars.inc), otherwise inter-node communication could fail.
%
get_tcp_port_range_option( no_restriction ) ->
	"";

get_tcp_port_range_option( {MinTCPPort,MaxTCPPort} ) when is_integer(MinTCPPort)
		   andalso is_integer(MaxTCPPort) andalso MinTCPPort < MaxTCPPort ->

	io_lib:format( " -kernel inet_dist_listen_min ~B inet_dist_listen_max ~B ",
				  [ MinTCPPort, MaxTCPPort ] ).



% Returns a plain string corresponding to a basic command-line command that can
% be used to launch an Erlang node (interpreter) with the specified settings.
get_basic_node_launching_command( NodeName, NodeNamingMode, EpmdSettings,
								TCPSettings, AdditionalOptions ) ->

	% May end up with a command-line option similar to:
	% ERL_EPMD_PORT=754 erl -setcookie 'foobar' -sname hello
	% -kernel inet_dist_listen_min 10000 inet_dist_listen_max 14000
	% -noshell -smp auto +K true +A 8 +P 400000

	text_utils:join( _Separator=" ", [
			get_epmd_option(EpmdSettings),
			executable_utils:get_default_erlang_interpreter(),
			get_cookie_option(),
			get_node_name_option( NodeName, NodeNamingMode ),
			get_tcp_port_range_option( TCPSettings ),
			AdditionalOptions ] ).



% Address-related functions.


% Returns a string describing the specified IPv4 address.
ipv4_to_string( {N1,N2,N3,N4} ) ->
	lists:flatten( io_lib:format( "~B.~B.~B.~B", [N1,N2,N3,N4] ) ).


% Returns a string describing the specified IPv4 address and port.
ipv4_to_string( {N1,N2,N3,N4}, Port ) ->
	lists:flatten( io_lib:format( "~B.~B.~B.~B:~B", [N1,N2,N3,N4,Port] ) ).
