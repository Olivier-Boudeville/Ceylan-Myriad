.. comment :raw-latex:`\pagebreak`

All-Purpose Helper Scripts
==========================

A small set of scripts has been defined, in ``common/src/scripts``, in order to help:

- benchmarking Erlang code: ``benchmark-command.escript``, ``benchmark-command.sh``, ``etop.sh``
- generating documentation: ``generate-docutils.sh``, ``generate-pdf-from-rst.sh``
- supporting explicit typing: ``list-available-types.sh``, ``add-deduced-type-specs.escript``
- evaluation Erlang code size: ``make-code-stats.sh``
- running Erlang programs: ``launch-erl.sh``, i.e. the (non-trivial) script that is automatically called by all our execution rules (i.e. we always run our Erlang programs through it)
