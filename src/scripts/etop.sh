#!/bin/sh

USAGE="  "`basename $0`" [ -node NODE_NAME ] [ -setcookie COOKIE ]: shows the activity of Erlang processes on an Erlang nodes.

	Example: etop.sh -node foobar@baz.org -setcookie a_cookie
"

# Ex for testing: erl -name foobar@baz.org

etop_base=`which erl|sed 's|bin/erl$|lib/erlang/lib/observer-|1'`

actual_etop_base=`ls ${etop_base}* -d|tail -n 1`

#echo "actual_etop_base = $actual_etop_base"

etop="${actual_etop_base}/priv/bin/etop"

${etop} -interval 1 -output graphical -lines 20 $*

