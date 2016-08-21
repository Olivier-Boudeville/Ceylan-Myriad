

:raw-latex:`\pagebreak`


Data-Management Services
========================

Some generic data-structures, in addition to the ones provided built-in with Erlang, are defined in ``common/src/data-management``, notably:

- a set of associative tables, with various implementations, tests and benchmarks, in::

  {hash,lazy,list,tracked,map}_table.erl

- a pseudo-table to abstract them out from the user's point of view (``table.erl``); note that now this is now a fully virtual module, in the sense that neither ``table.erl`` nor ``table.beam`` exist (the ``Common`` parse transform replaces a call to the ``table`` module by, currently, a call to the ``map_table`` module)
- a way of generating a read-only associative table whose key/value pairs can be read from any number (potentially extremely large) of readers very efficiently (``const_table.erl``)
- a specific support for other datatypes (``pair.erl``, ``option_list.erl``, ``preferences.erl``, ``tree.erl``)
- a first-level, optional support of the `HDF5 <https://www.hdfgroup.org/HDF5/>`_ file format (based on, and thus requiring, the `enhanced fork <https://github.com/Olivier-Boudeville-EDF/erlhdf5>`_ we made of `erlhdf5 <https://github.com/RomanShestakov/erlhdf5>`_)
