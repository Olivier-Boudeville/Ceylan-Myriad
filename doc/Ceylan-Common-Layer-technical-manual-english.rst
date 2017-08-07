========================================
Technical Manual of the ``Common`` Layer
========================================


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


:Author: Olivier Boudeville
:Contact: olivier.boudeville@esperide.com
:Creation Date: Wednesday, August 11, 2010
:Lastly Updated on: Wednesday, August 2, 2017


:Status: Work in progress
:Version: 0.2.6
:Dedication: Users and maintainers of the ``Common`` layer.
:Abstract:

	The role of the ``Common`` layer (part of the Ceylan project) is to gather all Erlang general-purpose base constructs that we found useful for Erlang developments.

	We present here a short overview of these services, to introduce them to newcomers.
	The next level of information is to read the corresponding source files, which are intensely commented and generally straightforward.


.. meta::
   :keywords: Common, generic, general-purpose, helper code, library, layer



:raw-latex:`\pagebreak`

.. contents:: Table of Contents
	:depth: 2

.. section-numbering::




.. include:: Ceylan-Common-Layer-overview-and-context-english.rst

.. include:: Ceylan-Common-Layer-usage-guidelines-english.rst


:raw-latex:`\pagebreak`


.. include:: Ceylan-Common-Layer-list-of-services-english.rst

.. include:: Ceylan-Common-Layer-build-english.rst

.. include:: Ceylan-Common-Layer-general-settings-english.rst

.. include:: Ceylan-Common-Layer-maths-services-english.rst

.. include:: Ceylan-Common-Layer-data-management-services-english.rst

.. include:: Ceylan-Common-Layer-gui-english.rst

.. include:: Ceylan-Common-Layer-helper-scripts-english.rst

.. include:: Ceylan-Common-Layer-utility-toolbox-english.rst

.. include:: Ceylan-Common-Layer-management-of-units-english.rst

.. include:: Ceylan-Common-Layer-sql-support-english.rst


.. include:: Ceylan-Common-Layer-gotchas-english.rst




:raw-latex:`\pagebreak`

----------
Final Word
----------

Each time that you need a basic service that:

- seems neither provided by the Erlang `built-in modules <http://erlang.org/doc/man_index.html>`_ nor by this ``Common`` layer
- is generic-enough, simple and requires no special prerequisite

please either enrich our ``*_utils.erl`` helpers, or add new general services!
