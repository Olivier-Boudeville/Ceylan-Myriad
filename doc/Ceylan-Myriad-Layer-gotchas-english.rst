:raw-latex:`\pagebreak`

----------------------------------
``Myriad``-related Troubleshooting
----------------------------------


Header/Module Dependencies
==========================

Only a very basic dependency between header files (``*.hrl``) and implementation files (``*.erl``) is managed.

As expected, if ``X.hrl`` changed, ``X.beam`` will be recompiled whether or not ``X.erl`` changed. However, any ``Y.erl`` that would include ``X.hrl`` would not be automatically recompiled.

Typically, when in doubt after having modified a record in a header file, just run ``make rebuild`` from the root of that layer (build is fast anyway, as quite parallel).



Third-Party Dependencies
========================

Let's suppose we have an application named ``Foo`` that relies on Myriad.

``Foo`` may define additional dependencies, which may be:

- either mandatory or optional
- needed starting from build-time (e.g. if relying on their headers and/or modules - including parse-transforms), or only at runtime

For a given **optional** dependency (e.g. regarding JSON [#]_), a USE make variable is to be defined in the layer that introduced this dependency (e.g. ``USE_JSON``, introduced by Myriad, therefore to be listed in its ``GNUmakevars.inc``). This variable allows to have our native build system register the associated additional include and ebin directories.

.. [#] JSON is just an example, knowing moreover that now Erlang provides a built-in JSON support on which we rely by default.

The first step to enable such a dependency (e.g. the JSON support) is to set its USE variable to ``true`` (e.g. ``USE_JSON = true``), as it is expected to be disabled by default. Depending on the service at hand, a specific (non-builtin) backend may have also to be selected (e.g. either ``USE_JSX = true`` or ``USE_JIFFY = true`` to select a suitable JSON parser).

Finally, some supports may depend on others (e.g. enabling ``USE_REST`` will enable ``USE_JSON`` in turn).



Runtime-only Third-Party Dependencies
-------------------------------------

The dependencies discussed here have to be found only when *executing* one's application; they can be installed:

- either manually, in which case the location of their ``ebin`` directory (typically an absolute path then) shall be specified in the code path (see, in ``GNUmakevars.inc``, the ``JSX_SOFTWARE_BASE`` make variable for an example)
- or thanks to rebar, in which case they shall obey the same rules as the `Build-time Third-Party Dependencies`_ discussed below



Build-time Third-Party Dependencies
-----------------------------------

Myriad does not have such dependencies, but layers above in the software stack (like a layer that would be named ``Foo``) may.

To have such dependencies (e.g., just for the sake of example, let's suppose that the ``jsx`` JSON parser defined header files that one wants to include) *installed* as well when building one's project (e.g. ``Foo``), one may rely on rebar, and list them in the project's ``foo/conf/rebar.config.template`` file (e.g. ``{deps, [bar, jsx]}.``) from which the actual ``rebar.config`` is to be generated (use the ``make set-rebar-conf`` target for that).

The actual compilation will be done by our native build system in all cases, either directly (when running ``make all``) or when using ``rebar compile`` (rebar hooks will then ensure that in practice the application is compiled with our native rules anyway). Therefore appropriate make variables (e.g. ``JSX_REBAR_BASE``, in ``myriad/GNUmakevars.inc``) shall be defined so that the corresponding BEAM files installed through rebar can be found in this native context as well (through the ``BEAM_DIRS`` make variable).

Finally, such dependencies may or may not be listed in the ``deps`` entry of the  ``conf/foo.app.src`` file [#]_, depending on whether they are optional or not.

.. [#] After having edited this file, run ``make create-app-file`` afterwards in order to have the three other versions of it properly generated (namely ``./_build/lib/foo/ebin/foo.app``, ``ebin/foo.app`` and ``src/foo.app.src``).



Myriad-level Third-Party Dependencies
-------------------------------------

Myriad as such has no mandatory dependency (except Erlang itself of course), but *optional* ones may be enabled, for:

- a basic `JSON <https://en.wikipedia.org/wiki/JSON>`_ support (see our `json_utils <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/data-management/json_utils.erl>`_ module), thanks to a suitable actual JSON parser: any available built-in `json <https://www.erlang.org/doc/apps/stdlib/json.html>`_ one, otherwise `jsx <https://github.com/talentdeficit/jsx/>`_ or `jiffy <https://github.com/davisp/jiffy>`_; note that the detection and use of these parsers are done transparently at runtime, hence none of them is a declared dependency of Myriad, which will adapt to any choice made by the overall application that includes both Myriad and one of such parsers (provided, as mentioned above, that the proper ``USE_*`` make variables are set)

- a first-level support of the `HDF5 <https://www.hdfgroup.org/HDF5/>`_ file format (see our `hdf5_support <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/data-management/hdf5_support.erl>`_ module), based on - and thus requiring - the `enhanced fork <https://github.com/Olivier-Boudeville-EDF/erlhdf5>`_ that we made of `erlhdf5 <https://github.com/RomanShestakov/erlhdf5>`_

- `Python <https://en.wikipedia.org/wiki/Python_(programming_language)>`_ (see our `python_utils <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/utils/python_utils.erl>`_ module), thanks to `erlport <https://github.com/hdima/erlport>`_

- `SQLite <https://en.wikipedia.org/wiki/SQLite>`_ (see our `sql_support <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/data-management/sql_support.erl>`_ module), thanks to the SQLite 3 Erlang binding that we retained, `erlang-sqlite3 <https://github.com/alexeyr/erlang-sqlite3.git>`_


.. _`jsx install`:

As an example, let's suppose that we need a JSON support and that we want to rely on the ``jsx`` parser (our previous default choice) for that.

If applying our conventions, supposing that Erlang and Rebar3 are already installed (otherwise refer to the `getting Erlang`_ and `getting Rebar3`_ sections), ``jsx`` may be installed with:

.. code:: bash

 $ mkdir -p ~/Software/jsx
 $ cd ~/Software/jsx
 $ git clone https://github.com/talentdeficit/jsx.git
 $ ln -s jsx jsx-current-install
 $ cd jsx/
 $ rebar3 compile && rebar3 eunit

.. $ ln -s _build/default/lib/jsx/ebin



About the ``table`` module
==========================

This is a pseudo module, which is not meant to exist as such (no ``table.erl``, no ``table.beam``).

The ``Myriad`` parse transform replaces references to the ``table`` module by (generally) references to the ``map_hashtable`` module. See `table transformations`_ for more information.



Enabling the Interconnection of Erlang nodes
============================================

This is not a Myriad gotcha per se, but rather an Erlang one, so we documented it in `this section <http://howtos.esperide.org/Erlang.html#general-information>`_ of our Erlang HOWTO.

Regarding the **EPMD** (TCP) port, the default Erlang one is ``4369``, while Myriad default one is ``4506``. Check for example that all launched nodes of interest can be seen with: ``epmd -port 4506 -names``.



Using the Erlang Shell for Debugging
====================================

It may be convenient to run an Erlang shell in order to investigate and fix issues.

One may execute ``make shell`` to launch a shell that is parameterised so that all modules of all layers (hence having Myriad from Myriad) are in its code path.

The `built-in shell commands <https://erlang.org/doc/man/shell.html#shell-commands>`_ are then very convenient, notably:

- ``v(-1)`` to get the *result* of the last command
- less relevant in a Myriad context: ``c(my_module)`` to compile (if possible with default settings - thus notably with no parse transform involved)  and (re)load the specified module
- ``l(my_module)`` to (re)load the specified module; useful when it has to be recompiled by Myriad (typically thanks to a ``make`` issued in another terminal)

Do not mix up this last command with ``rl(XXX)``, which does not perform a module reload but prints a record definition (and will not complain if given an unrelated module name, thus not reloading anything...).
