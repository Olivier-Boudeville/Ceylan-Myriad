
=========
MyriadGUI
=========


--------
Overview
--------

``MyriadGUI`` is a small, lightweight graphical user interface in `Erlang <http://www.erlang.org>`_.

It is built on top of `wx <http://erlang.org/doc/man/wx.html>`_, which itself encapsulates `wxWidgets <https://www.wxwidgets.org/>`_.


-------------------------
Implementation Principles
-------------------------

The goal is to provide an API (including message types) that are higher-level than ``wx``, and do not depend on any particular GUI backend (such as ``wx``, ``gs``, etc.) to avoid that user programs become obsolete too quickly.

So for example the messages received by the user programs shall not mention ``wx``, and they should take the form of `WOOPER <https://github.com/Olivier-Boudeville/Ceylan-WOOPER>`_ messages to allow for user code that would rely on WOOPER.

