#!/bin/sh

usage="Usage: $(basename $0) PROJECT_NAME [VERBOSE_MODE:0|1]"

# Script defined for convenience and reliability.

# Recreates a proper rebar3 landscape, based on our build, so that rebar3 will
# not attempt (and fail) to recreate BEAM files that are already correct as they
# are.


# Even if copying the relevant header/source/BEAM files with relevant
# timestamps, in some cases, for some reason, rebar will still find it relevant
# to try to rebuild some of them. So we have to hide the corresponding sources
# as well...

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
echo "Fixing rebar pre-build for ${project_name}"



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


echo "For ${project_name}, building all first, from $(pwd) (role: ${role}):"

[ $verbose -eq 1 ] || make -s info-context

# 'tree' may not be available:
[ $verbose -eq 1 ] || tree


make -s all #1>/dev/null


# We used not to fix anything by default; now we do the opposite, and in all
# cases (for all roles: build target or dependency):


# Actually needed apparently (even if they just copy a file onto itself, they at
# least update its timestamp in the process, like a touch), otherwise another
# silly attempt of rebuild will be done by rebar3:
#
fix_sources=0

fix_headers=0

# To copy ebin content:
fix_beams=0



echo "Copying build-related elements in the '${target_base_dir}' target tree."

# Transforming a potentially nested hierarchy (tree) into a flat directory:
# (operation order matters, as it allows proper timestamp ordering for make)
#
# Note that 'ebin' is bound to be an actual directory, yet (probably if dev_mode
# has been set to true in rebar.config), 'include', 'priv' and 'src' may be
# symlinks to their original versions (in the source tree).
#
# As they could be nested (being trees rather than flat directories), we remove
# any pre-existing target directory and replace it with a flat copy of our own.


if [ $fix_headers -eq 0 ]; then

	target_header_dir="${target_base_dir}/include"

	if [ -L "${target_header_dir}" ]; then

		echo "Replacing the ${target_header_dir} symlink by an actual directory."
		/bin/rm -f "${target_header_dir}"
		mkdir "${target_header_dir}"

	elif [ ! -d "${target_header_dir}" ]; then

		echo "Creating the non-existing ${target_header_dir} directory."
		mkdir "${target_header_dir}"

	else

		# This may happen when multiple attempts of build are performed:
		echo "(${target_header_dir} directory already existing)"

		echo "Warning: unexpected target ${target_header_dir}: $(ls -l ${target_header_dir})" 1>&2

		#exit 5

	fi

	all_headers=$(find src test include -name '*.hrl' 2>/dev/null)

	if [ $verbose -eq 0 ]; then
		echo "  Copying all headers to ${target_header_dir}: ${all_headers}"
	else
		echo "  Copying all headers to ${target_header_dir}"
	fi

	for f in ${all_headers}; do

		# We do not care if it is a copy of a file onto itself, a touch-like
		# operation is anyway strictly needed (however a copy of a file to
		# itself does not update its timestamp):
		#
		/bin/cp -f $f "${target_header_dir}/" #2>/dev/null
		/bin/mv -f $f $f-hidden

	done

else

	echo "(not fixing headers)"

fi



if [ $fix_sources -eq 0 ]; then

	target_src_dir="${target_base_dir}/src"

	if [ -L "${target_src_dir}" ]; then

		echo "Replacing the ${target_src_dir} symlink by an actual directory."
		/bin/rm -f "${target_src_dir}"
		mkdir "${target_src_dir}"

	elif [ ! -d "${target_src_dir}" ]; then

		echo "Creating the non-existing ${target_src_dir} directory."
		mkdir "${target_src_dir}"

	else

		# This may happen when multiple attempts of build are performed:
		echo "(${target_src_dir} directory already existing)"

		echo "Warning: unexpected target ${target_src_dir}: $(ls -l ${target_src_dir})" 1>&2

		#exit 6

	fi

	all_srcs=$(find src test -name '*.erl' 2>/dev/null)

	if [ $verbose -eq 0 ]; then
		echo "  Copying all sources to ${target_src_dir} then hiding the original ones: ${all_srcs}"
	else
		echo "  Copying all sources to ${target_src_dir} then hiding the original ones"
	fi

	for f in ${all_srcs}; do

		# We do not care if it is a copy of a file onto itself, a touch-like
		# operation is anyway strictly needed:
		#
		/bin/cp -f $f "${target_src_dir}/" #2>/dev/null
		/bin/mv -f $f $f-hidden

	done

else

	echo "(not fixing sources)"

fi


if [ $fix_beams -eq 0 ]; then

	target_beam_dir="${target_base_dir}/ebin"


	if [ -L "${target_beam_dir}" ]; then

		echo "Replacing the ${target_beam_dir} symlink by an actual directory."
		/bin/rm -f "${target_beam_dir}"
		mkdir "${target_beam_dir}"

	elif [ ! -d "${target_beam_dir}" ]; then

		echo "Creating the non-existing ${target_beam_dir} directory."
		mkdir "${target_beam_dir}"

	else

		# This may happen when multiple attempts of build are performed:
		echo "(${target_beam_dir} directory already existing)"

		echo "Warning: unexpected target ${target_beam_dir}: $(ls -l ${target_beam_dir})" 1>&2

		exit 7

	fi


	all_beams=$(find src test -name '*.beam' 2>/dev/null)

	if [ $verbose -eq 0 ]; then
		echo "  Copying all BEAM files to ${target_beam_dir}: ${all_beams}"
	else
		echo "  Copying all BEAM files to ${target_beam_dir}"
	fi

	for f in ${all_beams}; do
		/bin/cp -f $f ${target_beam_dir}
		/bin/mv -f $f $f-hidden
	done

else

	echo "(not fixing BEAM files)"

fi


[ $verbose -eq 1 ] || (echo "Final content for ${project_name} from $(pwd):"; tree "${target_base_dir}")


echo "Rebar pre-build fixed for ${project_name}."
