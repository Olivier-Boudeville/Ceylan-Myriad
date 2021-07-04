:raw-latex:`\pagebreak`

-------------------
``Myriad`` Gotchas
-------------------


Header dependencies
==================

Only a very basic dependency between header files (``*.hrl``) and implementation files (``*.erl``) is managed.

As expected, if ``X.hrl`` changed, ``X.beam`` will be recompiled whether or not ``X.erl`` changed. However, any ``Y.erl`` that would include ``X.hrl`` would not be automatically recompiled.

Typically, when in doubt after having modified a record in a header file, just run ``make rebuild`` from the root of that layer (build is fast anyway, as quite parallel).



About the ``table`` module
==========================

This is a pseudo module, which is not meant to exist as such (no ``table.erl``, no ``table.beam``).

The ``Myriad`` parse transform replaces references to the ``table`` module by references to the ``map_hashtable`` module. See `table transformations`_ for more information.




Enabling the Interconnection of Erlang nodes
============================================

This is not a Myriad gotcha per se, but rather an Erlang one.

Way too often, for obscure reasons Erlang nodes fail to connect to each other (especially with long names), and little to no information is available to diagnose the issue.



Safety Measures
---------------

In order to maximise the chances that nodes are able to ``net_adm:ping/1`` successfully each other:

- at least for testing, run VMs spawned with preferably the same **version** of Erlang
- ensure that they rely on the same **EPMD** (TCP) port (default Erlang one is ``4369``, while Myriad default one is ``4506``); check for example that all launched nodes of interest can be seen with: ``epmd -port 4506 -names``
- check that they use the same **cookie**, either from the start (use the ``-setcookie MY_COOKIE`` command-line option) or after having changed it after the VM was launched
- ensure that no **firewall** gets in the way; one may take inspiration for example from our `iptables.rules-FullDisabling.sh <https://github.com/Olivier-Boudeville/Ceylan-Hull/blob/master/iptables.rules-FullDisabling.sh>`_ script
- finally check that the local **DNS resolution** complies with the surprisingly picky constraints demanded by the Erlang VM

For this last point, ``/etc/hosts`` is often the scene of the disaster. If your hostname is ``hurricane`` and your domain is ``foobar.org``, then a line like the following one is known to work (whereas many variations of it may be deemed "incorrrect")::

  127.0.0.1  hurricane.foobar.org hurricane localhost.localdomain localhost

provided of course that, still in that file, you have not also a declaration such as::

  192.168.0.5 hurricane.foobar.org hurricane

(setting one's IP shall better be done in one's profile in ``/etc/netctl``, right?)



Testing & Troubleshooting
-------------------------

In order to **quick-check** whether long-name connectivity is available and to rule out the most obvious culprits, open two terminals.

In the first::

 # Check (with root permissions) that the firewall rules are safe; for example:
 $ iptables -nL
 Chain INPUT (policy ACCEPT)
 target     prot opt source               destination

 Chain FORWARD (policy ACCEPT)
 target     prot opt source               destination

 Chain OUTPUT (policy ACCEPT)
 target     prot opt source               destination

 # Just to be on the safer side for this test:
 $ killall beam.smp epmd

 # Then launch the target first node:
 $ ERL_EPMD_PORT=4032 erl -name n1 -setcookie aa
 Erlang/OTP 23 [erts-11.1.4] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

 Eshell V11.1.4  (abort with ^G)
 (n1@hurricane.foobar.org)1>


In the second terminal, try to find the previous node::

 $ ERL_EPMD_PORT=4032 erl -name n2 -setcookie aa
 Erlang/OTP 23 [erts-11.1.4] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

 Eshell V11.1.4  (abort with ^G)
 (n2@hurricane.foobar.org)1> net_adm:ping('n1@hurricane.foobar.org').
 pong


If you see ``pang`` here, run to the nearest altar and make a sacrifice to any Distribution God you may believe in (Norse ones being presumably the most effective here), and apply the hints listed in the `Enabling the Interconnection of Erlang nodes`_ section.
