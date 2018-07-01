#!/bin/sh


# Main launcher of the merge tool (knowing that the escript-based version
# misbehave in terms of term_ui).

# Any argument(s) specified to this script shall be interpreted as a plain,
# extra one:
#
make -s merge_exec CMD_LINE_OPT="-extra $*"
