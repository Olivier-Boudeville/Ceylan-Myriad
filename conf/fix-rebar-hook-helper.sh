# This file is an helper used by the fix-rebar-compile-{pre,post}-hook.sh
# scripts in order to define common functions once.
#
# As such it is not executable, as it is only meant to be sourced.


build_target_role=1
normal_dependency_role=2
checkout_role=3


# Sets following variables depending on context, project being either the actual
# build target or just a (direct or not) dependency thereof:
#
# - target_base_dir: the target base project directory where elements (notably
# BEAM files) shall be produced
#
# - role: tells whether this project is the actual build target, a normal
#  dependency or a checkout
#
determine_build_context()
{

	# Element locations vary depending on whether this project is the actual
	# target to build, or a mere dependency, or in a checkout.

	target_base_dir="$(pwd)/_build/default/lib/${project_name}"

	if [ -d "${target_base_dir}" ]; then

		echo "Project ${project_name} is detected as being the direct actual build target (not a dependency or a checkout), as '${target_base_dir}' exists."

		role=${build_target_role}

	else

		echo "(base directory of project ${project_name} is not '${target_base_dir}')"

		target_base_dir="$(realpath $(pwd)/../${project_name})"

		if [ ! -d "${target_base_dir}" ]; then

			echo "(base directory of project ${project_name} is not '${target_base_dir}' either)"

			echo "  Error, target directory not found for ${project_name}." 1>&2

			exit 10

		fi

		echo "Project ${project_name} is detected as being built as a dependency (in ${target_base_dir})."

		role=${normal_dependency_role}

	fi

}
