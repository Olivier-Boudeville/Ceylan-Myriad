
:raw-latex:`\pagebreak`

.. _toolbox:


Utility Toolbox
===============

This is the **core** of the ``Myriad`` library: a toolbox comprising many helper functions (with their tests), defined in the ``Ceylan-Myriad/src/utils`` directory, often providing enhanced, more specialised services compared to the ones offered by the Erlang standard libraries.

These helpers (code and typing information) are thematically aggregated in modules that are generally suffixed by ``_utils``, and include:

- many **basic, general-purpose services**, defined in ``basic_utils.erl``, regarding:

 - the base types we defined
 - process registration
 - notifications
 - message handling
 - many miscellaneous functions

- **cipher**-related facilities (basic, a bit exotic chained symmetric encryptions, notably with Mealy machines), in ``cipher_utils.erl``
- functions to manage Erlang **compiled BEAM code** (``code_utils.erl``)
- services to manage the **execution of other programs** (``executable_utils.erl``), to:

  - locate said executables
  - to execute functional services (ex: display a PDF) regardless of the actual executable involved
  - to handle more easily command-line arguments (a bit like ``getopt``), regardless of the interpreter or escript context

- helpers for **file-based** I/O operations (``file_utils.erl``)
- a very basic support of **Finite State Machines** (``fsm_utils.{e,h}rl``)
- a few operations defined on **graphs** (``graph_utils.erl``, with ``find_breadth_first/{3,4}``)
- extra operations defined on **lists** (``list_utils.erl``), including rings
- support for **network**-related concerns (``net_utils.erl.{e,h}rl``)
- services to offer **randomness** (``random.erl``), with regard to various sources (the Erlang built-in algorithm, ``crypto``, newer ones like ``exsplus`` - our current default, ``exs64`` and ``exs1024``), for seeding, drawing, etc.
- very little support of **RDF** operations, standing for `Resource Description Framework <https://en.wikipedia.org/wiki/Resource_Description_Framework>`_ (``rdf_utils.erl``)
- facilities to perform **REST calls** (``rest_utils.erl``), using built-in ``httpc`` and ``http_client``, and possibly a JSON parser, `jsx <https://github.com/talentdeficit/jsx/>`_
- elements for the sending of **SMS** (``sms_utils.erl``), based either on third-party providers providing REST APIs, or via a mobile phone (typically connected thanks to a USB cable)
- support for operations at the **operating-system** level (``system_utils.{e,h}rl``)
- services to handle **text** (``text_utils.erl``)
- functions to manage **time** information (``time_utils.erl``)
- a few helpers to ease the writing of `escripts <http://erlang.org/doc/man/escript.html>`_ relying on the Myriad layer (``script_utils.erl``)
- services about all kinds of **units** (``unit_utils.erl``); refer to the `Management of Units`_ section below for more information
- very basic facilities for **applications**, in ``app_facilities.{e,h}rl`` with an example (``most_basic_example_app.erl``)
