:raw-latex:`\pagebreak`

All-Purpose Helper Scripts
==========================

A small set of scripts has been defined, in ``myriad/src/scripts``, in order to help:

- finding in (Erlang) source code **type definitions** (``find-type-definition.sh``) and **function specifications** (``find-function-specification.sh``)
- **benchmarking** Erlang code: ``benchmark-command.escript``, ``benchmark-command.sh``, ``etop.sh``
- generating **documentation**: ``generate-docutils.sh``, ``generate-pdf-from-rst.sh``
- supporting **explicit typing**: ``list-available-types.sh``, ``add-deduced-type-specs.escript``
- evaluating Erlang **code size**: ``make-code-stats.sh``
- **running** Erlang programs: ``launch-erl.sh``, i.e. the (non-trivial) script that is automatically called by all our execution rules (i.e. we always run our Erlang programs through it)
- parsing **XML** thanks to ``xmerl``: ``show-xml-file.escript``

To be added: merging facilities (upcoming ``merge-tree.escript``)
