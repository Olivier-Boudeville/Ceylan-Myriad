#!/bin/sh

usage="Usage: $(basename $0) PROJECT_NAME [VERBOSE_MODE:0|1]"

# Removes the side effects in source tree of counterpart script
# conf/fix-rebar-compile-pre-hook.sh.

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


echo
echo "Fixing rebar post-build for ${project_name}, from $(pwd)."


helper_script="$(dirname $0)/fix-rebar-hook-helper.sh"

if [ ! -f "${helper_script}" ]; then

	echo "  Error, helper script ('${helper_script}') not found." 1>&2

	exit 8

fi

. "${helper_script}"


# Sets following variables depending on context, project being either the actual
# build target or just a (direct or not) dependency thereof:
#
# - target_base_dir: the target base project directory where elements (notably
# BEAM files) shall be produced
#
# - role: tells whether this project is the actual build target, a normal
#  dependency or a checkout
#
determine_build_context


do_name_back=0
#do_name_back=1

# Renames back hidden sources only if requested:
#
# (probably not a good idea, knowing that the timestamps of the source files
# will become by design more recent than their BEAM files, leading to rebar3
# probably triggering another unexpected attempt of building them; yet otherwise
# we have: "make: *** No rule to make target 'src/X.erl', needed by
# 'src/X.beam'.  Stop."; so we have to set back the erl files and thus recreate
# their BEAM counterpart files - even if currently it results in a double build
# of them)
#
if [ $do_name_back -eq 0 ]; then

	# Order matters again:

	headers_to_rename=$(find src test include -name '*.hrl-hidden')

	[ $verbose -eq 1 ] || echo "  Renaming back headers ${headers_to_rename}"

	for f in ${headers_to_rename}; do

		corrected_f="$(echo $f | sed 's|\.hrl-hidden$|.hrl|1')"
		/bin/mv -f "$f" "${corrected_f}"

	done


	sources_to_rename=$(find src test -name '*.erl-hidden')

	[ $verbose -eq 1 ] || echo "  Renaming back sources ${sources_to_rename}"

	for f in ${sources_to_rename}; do

		corrected_f="$(echo $f | sed 's|\.erl-hidden$|.erl|1')"
		/bin/mv -f "$f" "${corrected_f}"

	done


	beams_to_rename=$(find ebin -name '*.beam-hidden')

	[ $verbose -eq 1 ] || echo "  Renaming back BEAMs ${beams_to_rename}"

	for f in ${beams_to_rename}; do

		corrected_f="$(echo $f | sed 's|\.beam-hidden$|.erl|1')"
		/bin/mv -f "$f" "${corrected_f}"

	done


	echo "Rebuilding the whole again"
	make -s all 1>/dev/null

	# Update our local ebin accordingly:
	make -s copy-beams-to-ebin

	# Do the same for rebar3 conventions; hopefully sufficient:
	/bin/cp -f ebin/* "${target_base_dir}/ebin"

fi


if [ $verbose -eq 0 ]; then

	echo "Final build result for ${project_name} from $(pwd):"

	tree

	# To list paths:
	#for d in $(/bin/ls -d _build/default/lib/*); do

	#   echo " - in $d: "
	#   /bin/ls -l "$d"
	#   echo

	#done

fi


echo "Rebar post-build fixed for ${project_name}."
