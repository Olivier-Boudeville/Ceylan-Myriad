:raw-latex:`\pagebreak`

``Common`` Gotchas
==================


About the ``table`` module
--------------------------

This is a pseudo module, which is not meant to exist as such (no ``table.erl``, no ``table.beam``).

The ``Common`` parse transform replaces references to the ``table`` module by references to the ``map_table`` module (currently the most efficient table implementation we determined).
