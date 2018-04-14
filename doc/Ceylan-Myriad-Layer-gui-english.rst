:raw-latex:`\pagebreak`

Helpers For Graphical User Interface Programming
================================================

Services have been defined, in ``myriad/src/user-interface``, in order to handle more easily some elements regarding user interface (either graphical, with ``gui.erl``, ``gui_color.erl``, ``gui_text.erl``, ``gui_canvas.erl``,  etc., or textual, with ``ui.erl``), with a few tests (``gui_test.erl``, ``lorenz_test.erl``).

They relied initially on `gs <http://erlang.org/doc/man/gs.html>`_, now on `wx <http://erlang.org/doc/man/wx.html>`_ (a port of wxWidgets), maybe later in HTML 5.

.. Note:: GUI services are currently being reworked, to provide a ``gs``-like concurrent API while relying underneath on ``wx``, with some additions (such as canvases).

Related information of interest:

- `wxErlang: Getting Started <https://arifishaq.files.wordpress.com/2017/12/wxerlang-getting-started.pdf>`_
- http://www.idiom.com/~turner/wxtut/wxwidgets.html
- http://wxerlang.dougedmunds.com/
