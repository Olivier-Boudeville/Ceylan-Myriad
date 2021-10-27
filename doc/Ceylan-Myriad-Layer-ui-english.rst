:raw-latex:`\pagebreak`

.. _`user interface`:
.. _`graphical user interface`:
.. _`MyriadGUI`:


MyriadGUI: Helpers For User Interface Programming
=================================================

Some services have been defined, in ``myriad/src/user-interface``, in order to handle more easily interactions with the user, i.e. to provide a user interface.

.. Note The user-interface services, as a whole, are currently *not* functional. A rewriting thereof as been started yet has not completed yet.

The spirit of **MyriadGUI** is to offer, as much as possible, a high-level API (refer to the ``ui`` module) that can be seamlessly instrumented at runtime by different backends, depending on availability (ex: is this dependency installed?) and context (ex: is the program running in a terminal, or are graphical outputs possible?).

Unless the user forces the use of a given backend, the most advanced one that is locally available will be automatically selected.

An objective is to shelter user code from:

- the actual UI backend that will be selected ultimately on a given host
- the rise and fall of the various backends (thinking for example to ``gs`` having been quickly deprecated in favour of ``wx``); the idea is then that any new backend could be integrated, with *little to no change* in already-written code relying on MyriadGUI

Of course not all features of all backends can be integrated (they have not the same expressivity, a common base must be enforced [#]_) and creating a uniform interface over all sorts of vastly different ways of displaying and interacting with the user would require a lot of efforts. So MyriadGUI follows a pragmatic route: defining first basic, relevant, user-centric conventions and services able to cover most needs and to federate (hopefully) most backends, and to incrementally augment its implementation coverage on a per-need basis. As a consequence, efforts have been made so that adding any lacking element can be done at low cost.

.. [#] Yet optional, additional features may be defined, and each backend may choose to provide them or ignore them.


Various Flavours of User Interfaces
-----------------------------------

Such a user interface may be:

- either **text-only**, within a console, relying either on the very basic ``text_ui`` (for raw text) or its more advanced ``term_ui`` counterpart (for terminal-based outputs, with colours and text-based widgets)
- or **graphical**, with ``gui``

Text-based user interfaces are quite useful, as they are lightweight, incur few dependencies (if any), and can be used with headless remote servers (``text_ui`` and ``term_ui`` work well through SSH, and require no X server nor mouse).

As for graphical-based user interfaces, they are the richest, most usual, and generally the most convenient, user-friendly interfaces.

The user interfaces provided by Myriad are stateful, they rely on a **state** that can be:

- either ``explicit``, in a functional way; thus having to be carried in all calls
- or ``implicit``, using - for that very specific need only - the process dictionary (even if we try to stay away of it as much as possible)

We tested the two approaches and preferred the latter (implicit) one, which was found considerably more flexible and thus finally fully superseded the (former) explicit one.

We made our best so that a lower-level API interface (relying on a more basic backend) is **strictly included** in the higher-level ones (ex: ``term_ui`` adds concepts - like the one of window or box - to the line-based ``text_ui``; similarly ``gui`` is richer than ``term_ui``), in order that any program using a given user interface may use any of the next, upper ones as well (provided implicit states are used), in the following order: the ``text_ui`` API is included in the one of ``term_ui``, which is itself included in the one of ``gui``.

We also defined the **settings table**, which is a table gathering all the settings specified by the developer, which the current user interface does its best to accommodate.

Thanks to these "Matryoshka" APIs and the settings table, the definition of a more generic ``ui`` interface has been possible. It selects automatically, based on available local software dependencies, **the most advanced available backend**, with the most relevant settings.

For example a relevant backend will be automatically selected by:

.. code:: bash

 $ cd test/user-interface/src
 $ make ui_run


On the other hand, if wanting to select a specified backend:

.. code:: bash

 $ make ui_run CMD_LINE_OPT="--use-ui-backend term_ui"

(see the corresponding `GNUmakefile <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/test/user-interface/src/GNUmakefile>`_ for more information)



Raw Text User Interface: ``text_ui``
------------------------------------

This is the most basic, line-based monochrome textual interface, directly in raw text with no cursor control.

It is located in ``{src,test}/user-interface/textual``; see ``text_ui.erl`` for its implementation, and ``text_ui_test.erl`` for an example of its use.



Terminal Text User Interface: ``term_ui``
-----------------------------------------

This is a more advanced textual interface than the previous one, with colors, dialog boxes, support of locales, etc., based on `dialog <https://en.wikipedia.org/wiki/Dialog_(software)>`_ (possibly `whiptail <https://en.wikipedia.org/wiki/Newt_(programming_library)>`_ could be supported as well). Such backend of course must be available on the execution host then.

For example, to secure these prerequisites:

.. code:: bash

 # On Arch Linux:
 $ pacman -S dialog

 # On Debian-like distros:
 $ apt-get install dialog


It is located in ``{src,test}/user-interface/textual``; see ``term_ui.erl`` for its implementation, and ``term_ui_test.erl`` for an example of its use.



Graphical User Interface: ``gui``
---------------------------------


For Classical 2D Applications
.............................

This interface relied initially on ``gs`` (now deprecated), now on `wx <http://erlang.org/doc/man/wx.html>`_ (a port of `wxWidgets <https://www.wxwidgets.org/>`_), maybe later in HTML 5 [#]_. For the base dialogs, `Zenity <https://en.wikipedia.org/wiki/Zenity>`_ could have been an option.

.. [#] Possibly relying some day for that on the `Nitrogen web framework <http://nitrogenproject.com/>`_ or on `N2O <https://ws.n2o.dev/>`_.


.. Note:: ``gui`` does not adhere yet to the ``ui`` conventions, but it will ultimately will. Currently it just offers a graphical API (on top of ``wx``).

.. Note GUI services are currently being reworked, to provide a ``gs``-like concurrent API while relying underneath on ``wx``, with some additions (such as canvases).


The goal is to provide a small, lightweight API (including message types) that are higher-level than ``wx``, and do not depend on any particular GUI backend (such as ``wx``, ``gs``, etc.; so none of their includes, records, types or functions leak in the user realm), to avoid that user programs become obsolete too quickly because of the UI backend they rely on.

So for example the messages received by the user programs do not mention ``wx``, and respect only MyriadGUI conventions. These conventions are in line with the `WOOPER ones <https://wooper.esperide.org/#method-invocation>`_, enabling (in a fully optional manner) the user code to rely on WOOPER if wanted.

The usual mode of operation is the following:

1. From a user process (a test, an application, etc.), the GUI support is first started, with ``gui:start/{0,1}``
2. Then the various widgets (windows, frames, panels, buttons, etc.) are created (ex: thanks to ``MainFrame = gui:create_frame(...``) and the user process subscribes to the events it is interested in (as a combination of an event type and a widget-as-an-event-emitter; for example:

.. code:: erlang

 gui:subscribe_to_events({onWindowClosed, MainFrame}))

3. The user process also triggers any relevant operation (ex: clearing widgets, setting various parameters), generally shows at least a main frame and records the GUI state that it needs for future use (typically containing at least the MyriadGUI references of the widgets that it created)
4. Then the user process enters its own (GUI-specific) main loop, from which it will receive the events that it subscribed to, and to which it will react by performing application-specific operations and/or GUI-related operations (creating, modifying, deleting widgets). Generally at least one condition is defined in order to leave that main loop and stop the GUI (``gui:stop/0``)

Refer to `lorenz_test.erl <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/test/user-interface/graphical/lorenz_test.erl>`_ test as a full, executable usage example thereof.


Defining ``gui`` as an interface between the user code and a backend also allows to enrich said backend [#]_.

.. [#] For example, we needed to operate on a plain canvas, whereas ``wx`` (as we understand it) offers only panels with bitmaps (with ``wxDC``, ``wxWindowDC``, ``wxMemoryDC``, etc.), with no possibility to subclass them in order to add them features. So MyriadGUI transparently introduced ``gui_canvas`` to offer extended canvas services.


These services are located in ``{src,test}/user-interface/graphical`` (see ``gui.erl``, ``gui_color.erl``, ``gui_text.erl``, ``gui_canvas.erl``, etc.), with a few tests (``gui_test.erl``, ``lorenz_test.erl``) and will be enriched over time, on a per-need basis.


Related information of interest:

- if receiving errors about ``{badarg,"This"}``, like in:

.. code:: erlang

 {'_wxe_error_',710,{wxDC,setPen,2},{badarg,"This"}}

it is probably the sign that an attempt was done to perform an operation on an already-deallocated wx object

- wxErlang: `Getting started <https://arifishaq.files.wordpress.com/2017/12/wxerlang-getting-started.pdf>`_ and `Speeding up <https://arifishaq.files.wordpress.com/2018/04/wxerlang-speeding-up.pdf>`_, by Arif Ishaq

- Doug Edmunds' `wxerlang workups <http://wxerlang.dougedmunds.com/>`_

.. comment 404: - http://www.idiom.com/~turner/wxtut/wxwidgets.html



For 3D Applications
...................

The ``wx`` services also include a support for `OpenGL <https://en.wikipedia.org/wiki/OpenGL>`_. It can be easily tested by running ``wx:demo()`` from any Erlang shell and selecting then ``gl`` in the left example menu.

We gather a few helpers in ``gui_opengl``. This topic directly relates to Myriad's `spatial services and conventions`_ and to its support of the `glTf file format`_.


