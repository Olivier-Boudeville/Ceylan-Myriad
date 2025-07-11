% Copyright (C) 2007-2025 Olivier Boudeville
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
% Creation date: July 1, 2007.

-module(monitor_utils).

-moduledoc """
Gathering of various facilities related to the **monitoring of processes, ports,
time changes or nodes**.

See `monitor_utils_test.erl` for the corresponding test.
""".



% Monitoring section.


-doc "Not allowed to be shortened into a local `reference/0` type.".
-type monitor_reference() :: reference().



-doc "The types of language elements that can be monitored.".
-type monitored_element_type() :: 'process' | 'port' | 'clock'.



% (not exported yet by the 'erlang' module)
% -type monitored_process() :: erlang:monitor_process_identifier().
-doc "Designates an Erlang process being monitored.".
-type monitored_process() :: pid() | registered_process_identifier().



-doc "Designates a registered Erlang process.".
-type registered_process_identifier() ::
		registered_name() | { registered_name(), node() }.


% (not exported yet by the 'erlang' module)
%-type monitored_port() :: erlang:monitor_port_identifier().
-doc "Designates an Erlang port being monitored.".
-type monitored_port() :: port() | registered_name().



-doc "To monitor time offsets.".
-type monitored_clock() :: 'clock_service'.



-doc "An actual element being monitored.".
-type monitored_element() :: monitored_process() | monitored_port()
						   | monitored_clock().



-doc """
This information may be:

- the exit reason of the process

- or `noproc` (process or port did not exist at the time of monitor creation)

- or `noconnection` (no connection to the node where the monitored process
resides)
""".
-type monitor_info() :: basic_utils:exit_reason() | 'noproc' | 'noconnection'.



-doc "See `net_kernel:monitor_nodes/2` for more information.".
-type monitor_node_info() :: list_table:list_table().



-doc "Options to monitor a node.".
-type monitor_node_option() :: { 'node_type', net_utils:node_type() }
							 | 'nodedown_reason'.


-export_type([ monitor_reference/0, monitored_element_type/0,
			   monitored_process/0, monitored_port/0, monitored_clock/0,
			   monitored_element/0, monitor_info/0, monitor_node_info/0,
			   monitor_node_option/0 ]).


-export([ monitor_nodes/1, monitor_nodes/2 ]).



% Type shorthands:

-type registered_name() :: naming_utils:registration_name().



-doc """
Subscribes or unsubscribes the calling process to node status change messages.

See `net_kernel:monitor_nodes/2` for more information.
""".
-spec monitor_nodes( boolean() ) -> void().
monitor_nodes( DoStartNewSubscription ) ->
	monitor_nodes( DoStartNewSubscription, _Options=[] ).



-doc """
Subscribes or unsubscribes the calling process to node status change messages.

See `net_kernel:monitor_nodes/2` for more information.
""".
-spec monitor_nodes( boolean(), [ monitor_node_option() ] ) -> void().
monitor_nodes( DoStartNewSubscription, Options ) ->

	case net_kernel:monitor_nodes( DoStartNewSubscription, Options ) of

		ok ->
			ok;

		Error ->
			throw( { node_monitoring_failed, Error, DoStartNewSubscription,
					 Options } )

	end.
