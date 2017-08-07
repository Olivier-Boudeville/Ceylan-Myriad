
:raw-latex:`\pagebreak`

General Build Structure
=======================

Various elements are defined at the ``Common`` level to set-up an appropriate build, based on `GNU Make <http://www.gnu.org/software/make/manual/make.html>`_.

This includes:

- a set of pre-defined Make **variables**, describing various settings that will be reused by generic rules (ex: to compile modules with relevant flags, to create source archives, to install an application, to manage the various paths, to perform test checking, to generate archives, installs and releases, etc.); these variables are defined in ``common/src/GNUmakevars.inc``

- a set of generic **rules**, to compile and run various modules and tests, to generate various elements of documentation, etc.; these rules are defined in ``common/GNUmakerules.inc``

- **examples** of minimal Make files, that mostly specify the relative base path and only refer to the generic variables and rules; see ``common/src/GNUmakefile`` as an example

These build facilities are designed to be enriched in turn by all layers above, which may add or override variables and rules.

An example of that is the ``WOOPER`` layer, which is directly built on top of ``Common`` (and itself a base layer for other layers and applications).
