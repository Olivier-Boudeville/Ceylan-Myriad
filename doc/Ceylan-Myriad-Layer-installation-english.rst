

--------------
Getting Myriad
--------------


.. _prerequisites:

Prerequisites
=============

The **operating system** is supposed to be any not-so-old ``GNU/Linux`` distribution [#]_.

People reported uses of Myriad on ``macOS``, yet no extensive testing has been done there.

Whereas Erlang supports ``Windows`` and we tried to be as cross-platform as possible, even with tools like ``MSYS2`` / ``MinGW-w64`` we suppose quite a lot of special cases would have to be addressed (patches welcome, though!).

.. [#] For what it is worth, we prefer `Arch Linux <https://www.archlinux.org/>`_, but this does not really matter here.


The main tool prerequisite is of course having the `Erlang <http://erlang.org>`_ environment available, in its ``23.0`` version [#]_ or more recent.

.. [#] Most probably that older versions of Erlang would be more than sufficient in order to build Myriad (possibly at the expense of minor changes in a few calls to standard modules having been deprecated since then). It is just that in general we prefer to stick to the latest stable versions of software such as Erlang, and advise you to do so.


There are various ways of obtaining it (from your distribution, from prebuilt packages, directly from the sources), one of which being the `install-erlang.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/install-erlang.sh>`_ script that we devised.

A simple use of it is:

.. code:: bash

 $ ./install-erlang.sh --doc-install --generate-plt


One may execute ``./install-erlang.sh --help`` for more guidance about how to configure it, notably in order to enable all modules of interest (``crypto``, ``wx``, etc.).



Getting Myriad's Sources
========================

This is pretty straightforward, based on the `project repository <https://github.com/Olivier-Boudeville/Ceylan-Myriad>`_ hosted by Github:

.. code:: bash

 $ git clone https://github.com/Olivier-Boudeville/Ceylan-Myriad.git myriad

This should download in your current directory the full Myriad repository. For OTP compliance, using for such a clone its short name (``myriad``) rather than its long one (``Ceylan-Myriad``) is recommended.

The Myriad ``master`` branch is meant to stick to the latest stable version: we try to ensure that this main line always stays functional (sorry for the pun). Evolutions are to take place in feature branches and to be merged only when ready.


.. _build:

Building Myriad
===============

If a relevant Erlang installation is available, this is as simple as:

.. code:: bash

 $ cd myriad
 $ make all


The parallel build of the whole layer (services and tests alike) shall complete successfully (if it is not the case, see our support_ section).

One may just run ``make`` by itself in order to list the main available options.



.. _testing:

Testing Myriad
==============

Just run, still from the ``myriad`` directory:

.. code:: bash

 $ make test

The testing shall complete successfully (if it is not the case, see our support_ section).



.. _`type-checking`:

Type-checking Myriad
====================

As Myriad is (by default) to enable debug information with a key-based protection of the resulting BEAM files, one should first have such key defined.

One way of doing so is, if wanted, to update the default key (see ``DEBUG_INFO_KEY`` in ``GNUmakevars.inc``) and to write in on disk (ex: ``make write-debug-key-file``), and to rebuild Myriad accordingly afterwards (ex: ``make rebuild``).

Then, still from the ``myriad`` root directory:

.. code:: bash

 $ make generate-local-plt self-check-against-plt

It will trigger a full type-checking of Myriad, done thanks to `Dialyzer <http://erlang.org/doc/man/dialyzer.html>`_.

This time-consuming phase will complete with a rather long list of notifications. Help us reducing it! These messages are numerous, but we do not think that most of them are so worrying.

Finally, to trigger in one go a full rebuild, testing and type-checking, one may run:

.. code:: bash

 $ make check




:raw-html:`<a name="otp"></a>`

.. _`otp-build`:

OTP Build
=========

We felt that OTP build tools and Emakefiles were not expressive enough for our needs: as mentioned in `Building Myriad`_, a full, rather complete/complex/powerful build system based on `GNU make <https://www.gnu.org/software/make/manual/make.html>`_ is used by Ceylan-Myriad natively instead.

It allows to introduce all the generic rules we wanted, to define many conditional settings, to walk through an arbitrarily nested source tree, to integrate within a layered stack (notably alongside some other ``Ceylan-*`` libraries that depend on Ceylan-Myriad) and to perform a multi-stage build to accommodate the compilation and use of parse-transforms, with their own set of prerequisites.

However, to better integrate with other Erlang developments (which are mostly OTP-compliant), we added the (optional) possibility of generating a Myriad *OTP library application* out of the build tree, ready to be integrated into an (OTP) *release* and to be available as an Hex *package*. For that we rely on `rebar3 <https://www.rebar3.org/>`_, `relx <https://github.com/erlware/relx>`_ and `hex <https://hex.pm/>`_.


OTP Application
---------------

Myriad is not an *active* OTP application, and as such does not rely on, or provides, services running in the background; so no supervision tree or ``gen_server`` is involved here, just a *library* application ready for OTP integration [#]_.

.. [#] Speaking of OTP, in development mode, ``proc_lib``-based spawns used to be enabled, yet this led to longer error messages that were not that useful; see ``spawn_utils.hrl`` if wanting to re-enable them.


There are `various ways <https://www.rebar3.org/docs/getting-started>`_  for obtaining ``rebar3``; we prefer::

  $ cd ~/Software && git clone https://github.com/erlang/rebar3.git
	  && cd rebar3 && ./bootstrap


From the root of a Myriad clone, to obtain the Ceylan-Myriad library *application*, one just has to enter::

 $ make rebar3-application

It will trigger ``rebar3``, resulting [#]_ in a full, OTP-compliant build tree created in ``_build`` (including a properly-generated ``_build/default/lib/myriad/ebin/myriad.app`` file), and more generally in a proper OTP application.

.. [#] The operation was previously done through a rebar pre-compile hook, so that the our native build system could be relied upon before injecting the produced BEAMs into rebar's ``_build`` tree. Because of extraneous, failing recompilations being triggered by rebar, now we rely on a build system parallel to - and directly inspired by - our native one, directly done from within rebar (once properly triggered by our user-oriented Make targets).

As a result, the OTP application support can be tested from the root of an (already-built, with ``make rebar3-application``) Myriad source tree::

 $ cd src/utils
 $ make myriad_otp_application_run
		Running unitary test myriad_otp_application_run (third form) from
		   myriad_otp_application_test

 --> Testing module myriad_otp_application_test.

 Starting the Myriad application.
 Myriad version: {1,0,11}.
 Current user name: 'stallone'.
 Stopping the Myriad application.
 Successful end of test of the Myriad application.
 =INFO REPORT==== 18-Jul-2019::22:37:24.779037 ===
	application: myriad
	exited: stopped
	type: temporary

 --> Successful end of test.

 (test finished, interpreter halted)


This support can be also tested manually, directly through the build tree used by rebar3; from the root of Myriad, after having run ``make rebar3-application``::

 $ erl -pz _build/default/lib/myriad/ebin/
 Erlang/OTP 22 [erts-10.4] [source] [64-bit] [smp:8:8] [...]

 Eshell V10.4  (abort with ^G)
 1> application:start(myriad).
 ok
 2> text_utils:format( "Hello ~s", [ world ] ).
 "Hello world"
 3> application:stop(myriad).
 =INFO REPORT==== 18-Jul-2019::22:47:36.429804 ===
	application: myriad
	exited: stopped
	type: temporary


When needing to include a Myriad header file (taking ``spawn_utils.hrl`` as an example) in one's code, OTP conventions mandate using::

 -include_lib("myriad/include/spawn_utils.hrl").

rather than::

 -include("spawn_utils.hrl").



OTP Release
-----------

Quite similarly, to obtain a Ceylan-Myriad OTP *release* (`relx <https://github.com/erlware/relx>`_ being used in the background), possibly for a given profile like ``default`` (development mode) or ``prod`` (production mode) - refer to ``REBAR_PROFILE`` in ``GNUmakevars.inc``, one just has to run, from the root of Myriad::

 $ make rebar3-release


Hex Package
-----------

The `hex <https://hex.pm/>`_ package manager relies on mix, which is commonly installed with `Elixir <https://elixir-lang.org/>`_ (another language built on top of the Erlang VM).

.. comment  As an example on Arch Linux, to obtain hex, one would do the following:: $ pacman -S elixir

Thanks to the rebar3 integration with the ``rebar3_hex`` plugin specified in Myriad's (generated) `rebar.config <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/rebar.config>`_, ``hex`` will be automatically installed and set up.

By following the publishing guidelines (`[1] <https://hex.pm/docs/rebar3_publish>`_, `[2] <https://www.rebar3.org/docs/publishing-packages>`_), we were able to publish `Hex packages for Myriad <https://hex.pm/packages/myriad>`_ that can be freely used. And there was much rejoicing!

One just has to specify for example ``{deps,[myriad]}.`` in one's ``rebar.config``, and that's it.


For more details, one may have a look at:

- `rebar.config.template <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/rebar.config.template>`_, the general rebar configuration file used when generating the Myriad OTP application and release
- `rebar-for-hex.config.template <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/rebar-for-hex.config.template>`_, to generate a corresponding Hex package for Myriad (whose structure and conventions is quite different from the previous OTP elements)
- `rebar-for-testing.config.template <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/rebar-for-testing.config.template>`_, the simplest test of the previous Hex package: an empty rebar project having for sole dependency that Hex package


Other OTP-related Make Targets of Interest
------------------------------------------

To populate/update the OTP build tree (by default, from the GIT root, for example ``_build/default/lib/myriad/`` for Myriad) of the current Ceylan layer, one may use::

 $ make rebar3-compile

(this is especially useful in order to be able to use directly, from an OTP application, changes just performed in a Ceylan-based layer)


To update both the OTP build tree and the local ebin directory of each Ceylan layer on which the current layer depends, use::

 $ make rebar3-local-update

(note this will be a no-op from Myriad, as it does not depend on any Ceylan layer)


To publish an Hex package (once the proper version number has been set in ``GNUmakevars.inc``, see ``MYRIAD_VERSION``)::

 $ make rebar3-hex-publish


To test such a package::

 $ make test-hex-package


To populate directly the OTP local build tree with the Ceylan dependencies located alongside the current install (not useful for Myriad - which depends on none, but useful for upper layers) rather than fetching them through Hex (otherwise may more Hex packages would have to be published for testing during development)::

 $ make rebar3-local-update

Many more targets are defined in `GNUmakerules-explicit.inc <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/GNUmakerules-explicit.inc>`_.
