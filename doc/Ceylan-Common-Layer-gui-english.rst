
.. comment :raw-latex:`\pagebreak`

Helpers For Graphical User Interface Programming
================================================

Services have been defined, in ``common/src/user-interface``, in order to handle more easily some elements regarding user interface (either graphical, with ``gui.erl``, ``gui_color.erl``, ``gui_text.erl``, ``gui_canvas.erl``,  etc., or textual, with ``ui.erl``), with a few tests (``gui_test.erl``, ``lorenz_test.erl``).

They relied initially on `gs <http://erlang.org/doc/man/gs.html>`_, now on `wx <http://erlang.org/doc/man/wx.html>`_ (a port of wxWidgets).

Information of interest:

- `https://arifishaq.files.wordpress.com/2017/12/wxerlang-getting-started.pdf`_: wxErlang: Getting Started
- http://www.idiom.com/~turner/wxtut/wxwidgets.html
- http://wxerlang.dougedmunds.com/
