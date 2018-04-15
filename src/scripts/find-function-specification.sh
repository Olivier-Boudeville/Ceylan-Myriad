#!/bin/sh


USAGE="  Usage: $(basename $0) A_FUNCTION_NAME [A_DIR]\nAttempts to find the type specification for the specified Erlang function (even if the name is partial and/or includes an arity) from specified directory (if any), otherwise from current one."

base_dir=$(pwd)

function_name="$1"

if [ $# -eq 2 ] ; then

	base_dir="$2"

	if [ ! -d "${base_dir}" ] ; then

		echo "  Error, specified base search directory (${base_dir}) does not exist." 1>&2
		exit 10

	fi

	shift

fi

if [ ! $# -eq 1 ] ; then

	echo -e "$USAGE" 1>&2
	exit 5

fi

# Remove any arity:
actual_function_name=$(echo ${function_name} | sed 's|/.*$||1')

echo "Looking for the (Erlang) type specification for the function '${actual_function_name}' from ${base_dir}..."
echo

# DUMMY to force the display of the corresponding file.
#
# '{actual_function_name}', not '{actual_function_name}(', so that partial
# function names can still be found.
#
cd ${base_dir} && find . -name '*.?rl' -exec /bin/grep -e "[[:space:]]\?-spec[[:space:]]\+${actual_function_name}" DUMMY '{}' ';' 2>/dev/null
