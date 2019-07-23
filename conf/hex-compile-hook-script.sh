#!/bin/sh

usage="$(basename $0): a script for the compilation to happen correctly when creating an hex package."

# See the pre_hooks key in rebar-for-hex.config.

echo "  Hex compile hook script started: preparing for package..."

for f in priv/GNUmake* ; do ln -sf $f ; done

make all && echo "  End of hex compile hook script"
