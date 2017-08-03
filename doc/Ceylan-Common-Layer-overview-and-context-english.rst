:raw-latex:`\pagebreak`


------------------
Overview & Context
------------------

When using any programming language, there are always **recurring patterns** that prove useful.

Instead of writing them again and again, we prefer to gather them all in a **low-level layer** (mostly a **code library**), in their most convenient, reliable, efficient version, together with their specification, documentation and testing.

This layer provides its (generally lightweight, simple) services just on top of the language, as a small (comprising currently about 38k lines), thin layer.

These services tend to stay away from introducing any new dependency. Should a key, generic service need a third-party prerequisite (ex: library to manage a complex data format, or to process specific data), that dependency should be made fully optional [#]_ (then disabling the corresponding service if not available).

.. [#] One may refer to what we did respectively for HDF5 and for JSON parsers in the context of REST support, with the ``USE_HDF5`` and ``USE_REST`` Make variables.

.. comment Line count computed with: wc -l $(find . -name '*.?rl')`

As a consequence, for the Ceylan project, the first level of the software stack we use relies on this ``Common`` layer - whose official, more specific name is the ``Ceylan-Myriad`` layer.
