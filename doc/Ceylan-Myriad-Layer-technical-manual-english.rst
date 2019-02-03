.. _Top:


.. title:: Welcome to the Ceylan-Myriad documentation

.. comment stylesheet specified through GNUmakefile


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex

.. comment Would appear too late, can only be an be used only in preamble:
.. comment :raw-latex:`\usepackage{graphicx}`
.. comment As a result, in this document at least a '.. figure:: XXXX' must
.. exist, otherwise: 'Undefined control sequence \includegraphics.'.


:raw-html:`<a name="myriad_top"></a>`

:raw-html:`<div class="banner"><p><em>Myriad documentation</em> <a href="http://myriad.esperide.org">browse latest</a> <a href="https://olivier-boudeville.github.io/Ceylan-Myriad/myriad.html">browse mirror</a> <a href="myriad.pdf">get PDF</a> <a href="#myriad_top">go to top</a> <a href="#myriad_bottom">go to bottom</a> <a href="mailto:about(dash)myriad(at)esperide(dot)com?subject=[Ceylan-Myriad]%20Remark">mail us</a></p></div>`



:raw-html:`<center><img src="myriad-title.png" width="70%"></img></center>`
:raw-latex:`\includegraphics[scale=0.25]{myriad-title.png}`



===============================================
Technical Manual of the ``Ceylan-Myriad`` Layer
===============================================


:Organisation: Copyright (C) 2008-2019 Olivier Boudeville
:Contact: about (dash) myriad (at) esperide (dot) com
:Creation Date: Wednesday, August 11, 2010
:Lastly Updated: Sunday, February 3, 2019
:Status: Work in progress
:Version: 1.2.1
:Dedication: Users and maintainers of the ``Myriad`` layer.
:Abstract:

	The role of the `Myriad <http://myriad.esperide.org/>`_ layer (part of the `Ceylan <https://github.com/Olivier-Boudeville/Ceylan>`_ project) is to gather all `Erlang <http://erlang.org>`_ general-purpose base constructs that we found useful for (Erlang-based) developments.

	We present here a short overview of these services, to introduce them to newcomers.
	The next level of information is to read the corresponding `source files <https://github.com/Olivier-Boudeville/Ceylan-Myriad>`_, which are intensely commented and generally straightforward.


.. meta::
   :keywords: Myriad, generic, general-purpose, helper code, library, layer



:raw-latex:`\pagebreak`

.. contents:: Table of Contents
	:depth: 32

.. comment To avoid, otherwise title with a '1.':.. section-numbering::




.. include:: Ceylan-Myriad-Layer-overview-and-context-english.rst

.. include:: Ceylan-Myriad-Layer-usage-guidelines-english.rst

.. include:: Ceylan-Myriad-Layer-installation-english.rst


:raw-latex:`\pagebreak`


.. include:: Ceylan-Myriad-Layer-list-of-services-english.rst

.. include:: Ceylan-Myriad-Layer-build-english.rst

.. include:: Ceylan-Myriad-Layer-general-settings-english.rst

.. include:: Ceylan-Myriad-Layer-maths-services-english.rst

.. include:: Ceylan-Myriad-Layer-data-management-services-english.rst

.. include:: Ceylan-Myriad-Layer-conditioned-code-english.rst

.. include:: Ceylan-Myriad-Layer-ui-english.rst

.. include:: Ceylan-Myriad-Layer-helper-scripts-english.rst

.. include:: Ceylan-Myriad-Layer-utility-toolbox-english.rst

.. include:: Ceylan-Myriad-Layer-metaprogramming-english.rst

.. include:: Ceylan-Myriad-Layer-management-of-units-english.rst

.. include:: Ceylan-Myriad-Layer-sql-support-english.rst

.. include:: Ceylan-Myriad-Layer-gotchas-english.rst

.. include:: Ceylan-Myriad-Layer-support-english.rst



:raw-latex:`\pagebreak`

---------------------------
Contributions & Ending Word
---------------------------

Each time that you need a basic service that:

- seems neither provided by the Erlang `built-in modules <http://erlang.org/doc/man_index.html>`_ nor by this ``Myriad`` layer
- is generic-enough, simple and requires no special prerequisite

please either enrich our ``*_utils.erl`` helpers, or add new general services!



In such a case, we would prefer that, in contributed code:

- Myriad code style is, as much as possible, respected (regarding naming, spacing, code/comments/blank line ratios, etc.)
- lines stop no later than their 80th character
- whitespaces be removed (ex: one may use the ``whitespace.el`` Emacs mode)

Thanks in advance, and have fun with Myriad!

.. comment Mostly added to ensure there is at least one figure directive,
.. otherwise the LateX graphic support will not be included:

.. figure:: myriad-title.png
   :alt: Myriad logo
   :scale: 25

:raw-html:`<a name="myriad_bottom"></a>`
