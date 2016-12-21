

:raw-latex:`\pagebreak`


Data-Management Services
========================

Some generic data-structures, in addition to the ones provided built-in with Erlang, are defined in ``common/src/data-management``, notably:

- a set of associative tables, with a rather complete interface (to create,
  update, enrich, search, query, list, map, fold, merge, display, etc.) and
  various implementations thereof, tests and benchmarks, in::

  {hash,lazy,list,tracked,map}_table.erl

- a ``table`` pseudo-module to abstract them out from the user's point of view; note that this is a fully virtual module, in the sense that neither ``table.erl`` nor ``table.beam`` exist (the ``Common`` parse transform replaces a call to the ``table`` module by, currently, a call to the ``map_table`` module; so, in order to consult the ``table`` API, please refer to ``map_table.erl``)
- a way of generating a read-only associative table whose key/value pairs can be read from any number (potentially extremely large) of readers very efficiently (``const_table.erl``)
- a specific support for other datatypes (``pair.erl``, ``option_list.erl``, ``preferences.erl``, ``tree.erl``)
- a first-level, optional support of the `HDF5 <https://www.hdfgroup.org/HDF5/>`_ file format (based on, and thus requiring, the `enhanced fork <https://github.com/Olivier-Boudeville-EDF/erlhdf5>`_ we made of `erlhdf5 <https://github.com/RomanShestakov/erlhdf5>`_)
