

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

The main tool prerequisite is of course having the `Erlang <http://erlang.org>`_ environment available, in its ``21.0`` version or more recent.

There are various ways of obtaining it (from your distribution, from prebuilt packages, directly from the sources), one of which being our `install-erlang.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/install-erlang.sh>`_ script; see its ``--help`` option for more guidance.


Getting the Sources
===================

This is pretty straightforward:

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



.. _`otp-build`:

OTP Build
=========

We felt that OTP build tools and Emakefiles were not expressive enough for our needs: as mentioned in `Building Myriad`_, a full, rather complete/complex/powerful build system based on `GNU make <https://www.gnu.org/software/make/manual/make.html>`_ is used by Ceylan-Myriad natively instead.

It allows to define all the generic rules we wanted, to define many conditional settings, to walk through an arbitrarily nested source tree, to integrate within a layered stack (notably some other ``Ceylan-*`` libraries) and to perform a multi-stage build to accommodate the compilation and use of parse-transforms, with their own set of prerequisites.

However, to better integrate with other Erlang developments (which are mostly OTP-compliant), we added the possibility of generating a Myriad OTP library out of the build tree. For that we relied on `rebar3 <https://www.rebar3.org/>`_ and `hex <https://hex.pm/>`_.

From the root of a Myriad clone, one just has to enter::

 $ make rebar3-compile

Then a full, OTP-compliant build tree is created in ``_build``, including a properly generated ``.app`` file.
