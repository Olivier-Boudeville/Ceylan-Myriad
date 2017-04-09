
:raw-latex:`\pagebreak`

Utility Toolbox
===============

This is the **core** of the ``Commmon`` library: a toolbox comprising many helper functions (with their tests), defined in the ``common/src/utils`` directory, often providing enhanced, more specialised services compared to the ones offered by the Erlang standard library.

These helpers (code and typing information) are thematically aggregated in modules that are generally suffixed by ``_utils``, and include:

- many **basic, general-purpose services**, defined in ``basic_utils.erl``, regarding:

 - the base types we defined
 - process registration
 - notifications
 - message handling
 - many miscellaneous functions

- **cipher**-related facilities (basic, a bit exotic chained symmetric encryptions, notably with Mealy machines), in ``cipher_utils.erl``
- functions to manage Erlang **compiled BEAM code** (``code_utils.erl``)
- services to manage the **execution of other programs** (``executable_utils.erl``)
- helpers for **file-based** I/O operations (``file_utils.erl``)
- a very basic support of **Finite State Machines** (``fsm_utils.{e,h}rl``)
- a few operations defined on **graphs** (``graph_utils.erl``, with ``find_breadth_first/{3,4}``)
- extra operations defined on **lists** (``list_utils.erl``), including rings
- elements to manage a sort of **metaprogrammation** in Erlang, based on *parse transforms*: ``meta_utils.{e,}rl``, ``common_parse_transform.erl``, ``example_parse_transform.erl``
- support for **network**-related concerns (``net_utils.erl.{e,h}rl``)
- services to offer **randomness** (``random.erl``), with regard to various sources (the Erlang built-in algorithm, ``crypto``, newer ones like ``exsplus`` - our current default, ``exs64`` and ``exs1024``), for seeding, drawing, etc.
- very little support of **RDF** operations, standing for `Resource Description Framework <https://en.wikipedia.org/wiki/Resource_Description_Framework>`_ (``rdf_utils.erl``)
- facilities to perform **REST calls** (``rest_utils.erl``), using built-in ``httpc`` and ``http_client``, and possibly a JSON parser, `jsx <https://github.com/talentdeficit/jsx/>`_
- elements for the sending of **SMS** (``sms_utils.erl``), based on third-party providers providing REST interfaces
- support for operations at the **operating-system** level (``system_utils.{e,h}rl``)
- services to handle **text** (``text_utils.erl``)
- functions to manage **time** information (``time_utils.erl``)
- a few helpers to ease the writing of `escripts <http://erlang.org/doc/man/escript.html>`_ relying on the Common layer (``script_utils.erl``)
- services about all kinds of **units** (``unit_utils.erl``); refer to the `Management of Units`_ section below for more information
- very basic facilities for **applications**, in ``app_facilities.{e,h}rl`` with an example (``most_basic_example_app.erl``)
