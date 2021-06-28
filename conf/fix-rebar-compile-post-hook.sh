#!/bin/sh

usage="Usage: $(basename $0) PROJECT_NAME [VERBOSE_MODE:0|1]"

project_name="$1"

# Not verbose by default (1):
#verbose=1
verbose=0

if [ -n "$2" ]; then
	verbose="$2"
fi


if [ -z "${project_name}" ]; then

	echo "  Error, project name not set." 1>&2

	exit 5

fi

# Removes the side effects in source tree of conf/fix-rebar-compile-pre-hook.sh.

echo
echo "Fixing rebar post-build for ${project_name}, from $(pwd)."


#do_name_back=0
do_name_back=1

# Renames back hidden sources only if requested:
#
# (probably not a good idea, knowing that the timestamps of the source files
# will become by design more recent than their BEAM files, leading to rebar3
# probably triggering another unexpected attempt of building them)
#
if [ $do_name_back -eq 0 ]; then

	to_rename=$(find src test -name '*.erl-hidden')

	[ $verbose -eq 1 ] || echo "Renaming back ${to_rename}"

	for f in ${to_rename}; do

		corrected_f="$(echo $f | sed 's|\.erl-hidden$|.erl|1')"
		/bin/mv -f $f "${corrected_f}"

	done

fi


if [ $verbose -eq 0 ]; then

	echo "Result from $(pwd):"

	# To list paths:
	for d in $(/bin/ls -d _build/default/lib/*); do

		echo " - in $d: "
		/bin/ls -l "$d"
		echo

	done

fi


echo "Rebar post-build fixed for ${project_name}."
