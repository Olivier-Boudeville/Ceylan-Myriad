

--------------
Getting Myriad
--------------

.. _prerequisite:

Prerequisites
=============

The **operating system** is supposed to be any not-so-old ``GNU/Linux`` distribution [#]_.

People reported uses of Myriad on ``macOS``, yet no extensive testing has been done there.

Whereas Erlang supports ``Windows`` and we tried to be as cross-platform as possible, even with tools like ``MSYS2`` / ``MinGW-w64`` we suppose quite a lot of special cases would have to be addressed (patches welcome, though!).

.. [#] For what it is worth, we prefer `Arch Linux <https://www.archlinux.org/>`_, but this does not really matter here.

The main tool prerequisite is of course having the `Erlang <http://erlang.org>`_ environment available, in its ``22.0`` version or more recent.

There are various ways of obtaining it (from your distribution, from prebuilt packages, directly from the sources), one of which being our `install-erlang.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/install-erlang.sh>`_ script; see its ``--help`` option for more guidance.



Getting the Sources
===================

This is pretty straightforward, based on the `project repository <https://github.com/Olivier-Boudeville/Ceylan-Myriad>`_ hosted by Github:

.. code:: bash

 $ git clone https://github.com/Olivier-Boudeville/Ceylan-Myriad.git

This should download in your current directory the full Myriad repository.



.. _build:

Building Myriad
===============

This is as simple as:

.. code:: bash

 $ cd Ceylan-Myriad
 $ make all


The parallel build of the whole layer (services and tests alike) shall complete successfully (if it is not the case, see our support_ section).

One may just run ``make`` by itself in order to list the main available options.



.. _testing:

Testing Myriad
==============

Just run, still from the ``Ceylan-Myriad`` directory:

.. code:: bash

 $ make test

The testing shall complete successfully (if it is not the case, see our support_ section).



.. _`type-checking`:

Type-checking Myriad
====================

As Myriad is (by default) to enable debug information with a key-based protection of the resulting BEAM files, one should first have such key defined.

One way of doing so is, if wanted, to update the default key (see ``DEBUG_INFO_KEY`` in ``GNUmakevars.inc``) and to write in on disk (ex: ``make write-debug-key-file``), and to rebuild Myriad accordingly afterwards (ex: ``make rebuild``).

Then, still from the ``Ceylan-Myriad`` root directory:

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

It allows to define all the generic rules we wanted, to define many conditional settings, to walk through an arbitrarily nested source tree, to integrate within a layered stack (notably alongside some other ``Ceylan-*`` libraries that depend on Ceylan-Myriad) and to perform a multi-stage build to accommodate the compilation and use of parse-transforms, with their own set of prerequisites.

However, to better integrate with other Erlang developments (which are mostly OTP-compliant), we added the possibility of generating a Myriad *OTP library application* out of the build tree, ready to be integrated into an *(OTP) release* and to be available (soon!) as an Hex package. For that we rely on `rebar3 <https://www.rebar3.org/>`_, `relx <https://github.com/erlware/relx>`_ and `hex <https://hex.pm/>`_.


OTP Application
---------------

Myriad is not an *active* OTP application, and as such does not rely on, or provides, services running in the background; so no supervision tree or ``gen_server`` is involved here, just a library application ready for OTP integration. In development mode, ``proc_lib``-based spawns are done.

There are `various ways <https://www.rebar3.org/docs/getting-started>`_  for obtaining ``rebar3``; we prefer::

  $ cd ~/Software && git clone https://github.com/erlang/rebar3.git
	  && cd rebar3 && ./bootstrap


From the root of a Myriad clone, to obtain the Ceylan-Myriad library application, one just has to enter::

 $ make rebar3-application

It will trigger ``rebar3`` that, through a hook, will trigger at the right step the relevant Myriad Make-based targets, resulting in a full, OTP-compliant build tree created in ``_build`` (including a properly-generated ``_build/default/lib/myriad/ebin/myriad.app`` file), and more generally in a proper OTP application.

The OTP application support can be tested through a (compiled) source tree; from the root of Myriad::

 $ cd src/utils
 $ make otp_application_run
		Running unitary test otp_application_run (third form) from otp_application_test

 --> Testing module otp_application_test.

 Starting the Myriad application.
 Myriad version: {1,0,0}.
 Current user name: 'stallone'.
 Stopping the Myriad application.
 Successful end of test of the Myriad application.
 =INFO REPORT==== 18-Jul-2019::22:37:24.779037 ===
	application: myriad
	exited: stopped
	type: temporary

 --> Successful end of test.

 (test finished, interpreter halted)


It can be also tested manually, directly through the build tree used by rebar3; from the root of Myriad::

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



OTP Release
-----------

Quite similarly, to obtain a Ceylan-Myriad OTP release, possibly for a given profile like ``default`` (development mode) or ``prod`` (production mode) - refer to ``REBAR_PROFILE`` in ``GNUmakevars.inc``, one just has to run::

 $ make rebar3-release


Hex Package
-----------

The `hex <https://hex.pm/>`_ package manager relies on mix, which is commonly installed with `Elixir <https://elixir-lang.org/>`_ (another language built on top of the Erlang VM).

.. comment  As an example on Arch Linux, to obtain hex, one would do the following:: $ pacman -S elixir

Thanks to the rebar3 integration with the ``rebar3_hex`` plugin specified in Myriad's `rebar.config <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/rebar.config>`_, hex will be automatically installed and set up.

Following the publishing guidelines (`[1] <https://hex.pm/docs/rebar3_publish>`_, `[2] <https://www.rebar3.org/docs/publishing-packages>`_), after having created a proper hex user, trying to run ``rebar3 hex publish`` in our case resulted in a strange behaviour::

 Select application(s):
 ------------
 A) All
 [1-0] or (A)ll  ("A")>


and then the command just finished with no specific error.

We will retry later when these tools will have made more progress.
