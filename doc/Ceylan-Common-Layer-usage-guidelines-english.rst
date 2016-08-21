

----------------
Usage Guidelines
----------------


License
=======

The ``Common`` layer is licensed by its author (Olivier Boudeville) under a disjunctive tri-license, giving you the choice of one of the three following sets of free software/open source licensing terms:

- `Mozilla Public License <http://www.mozilla.org/MPL/MPL-1.1.html>`_ (MPL), version 1.1 or later (very close to the `Erlang Public License <http://www.erlang.org/EPLICENSE>`_, except aspects regarding Ericsson and/or the Swedish law)

- `GNU General Public License <http://www.gnu.org/licenses/gpl-3.0.html>`_ (GPL), version 3.0 or later

- `GNU Lesser General Public License <http://www.gnu.org/licenses/lgpl.html>`_ (LGPL), version 3.0 or later


This allows the use of the ``Common`` code in as wide a variety of software projects as possible, while still maintaining copyleft on this code.

Being triple-licensed means that someone (the licensee) who modifies and/or distributes it can choose which of the available sets of licence terms he is operating under.

Enhancements are expected to be back-contributed, so that everyone can benefit from them.



About Layers
============

The ``Common`` services are to be used by this layer itself (for its inner workings), and, more importantly, are to be re-used, specialised and enriched by all layers built on top of it.

The general rule is that a layer may depend on (i.e. make use of) layers *below* it, but cannot refer to any layer *above* it (it should be literally unaware of their existence).

So, in a bottom-up view, a typical software stack begins with the operating system (typically GNU/Linux), then `Erlang/OTP <http://erlang/org>`_, then ``Common``, then any layer(s) built on top of them.

Of course a given layer does not mask the layers below; for example programs using the ``Common`` layer typically use also a lot the services brought by the `Erlang base libraries <http://erlang.org/erldoc>`_.



Recommended Usage & Contribution
================================

When developing Ceylan-based code, if needing a service already provided by this ``Common`` layer, it is strongly advised to use that service and, possibly, expand or enrich it, with backward compatibility in mind.

If such a service is not provided by the current version of the layer, yet being deemed generic enough, then it should preferably be directly added to the relevant part of the library and then called from the code that was needing it.

Of course, contributions of all sorts are welcome.

We do our best to test at least lightly each element provided. All services offered by ``foo.erl`` are thus expected to be tested in the companion ``foo_test.erl`` file, in the same directory. Once there, running this test is as simple as executing::

 $ make foo_run

Note that however we have not reached the discipline level of an exhaustive ``eunit`` test suite for each service (most of them being almost trivial).

The `Dialyzer <http://erlang.org/doc/apps/dialyzer/dialyzer_chapter.html>`_ static analysis tool is regularly run on the code-base (see the ``generate-local-plt`` and ``self-check-against-plt`` generic Make targets for that).
