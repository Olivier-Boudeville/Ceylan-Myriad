# Prerequisite: COMMON_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Ceylan-common
endif


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = 0.1
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = common
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(COMMON_TOP)
endif


ARCHIVE_ROOT := $$HOME/Archive/$(PROJECT_NAME)


ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME).tar.bz2


ARCHIVE_BRANCH_FILE = $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME)-branch-$(shell basename `pwd`).tar.bz2



SNAPSHOT_ARCHIVE_FILE := $(ARCHIVE_ROOT)/`date +'%Y%m%d'`-$(PROJECT_NAME)-snapshot.tar.bz2


PROJECT_RELEASE_BASE        := $(PROJECT_NAME)-$(PROJECT_VERSION)

PROJECT_RELEASE_ARCHIVE_ZIP := $(PROJECT_RELEASE_BASE).zip
PROJECT_RELEASE_ARCHIVE_BZ2 := $(PROJECT_RELEASE_BASE).tar.bz2



# Installation section.


# If wanting to install that package, following installation prefix is used.
# INSTALLATION_PREFIX should be defined on a per-project basis, or be
# overridden on the command line (ex: 'make install INSTALLATION_PREFIX=/tmp'):
ifndef INSTALLATION_PREFIX
	INSTALLATION_PREFIX = $$HOME/Software
endif


PACKAGE_INSTALLATION_PREFIX = $(INSTALLATION_PREFIX)/$(PACKAGE_NAME)


# Could rely on the permissions instead:
EXEC_TO_INSTALL := `find $(PACKAGE_TOP) -name '*.sh' -a ! -name '*.exe'`


BEAMS_TO_INSTALL := `find $(PACKAGE_TOP) -name examples -prune -o \( -name '*.beam' -a ! -name '*_test.beam' -a ! -name 'class_Test*.beam' \) -a -print`

SOURCES_TO_INSTALL := `find $(PACKAGE_TOP) -name examples -prune -o \( -name '*.erl' -a ! -name '*_test.erl' -a ! -name 'class_Test*.erl' \) -a -print`


EXAMPLES_TO_INSTALL_BEAMS := `[ ! -d examples ] || find $(PACKAGE_TOP)/examples -name '*.beam'`

EXAMPLES_TO_INSTALL_SRC := `[ ! -d examples ] || find $(PACKAGE_TOP)/examples -name '*.erl'`


INCLUDES_TO_INSTALL := `find $(PACKAGE_TOP) -name '*.hrl'`


TESTS_TO_INSTALL_BEAMS := `find $(PACKAGE_TOP) -name '*_test.beam' -o -name 'class_Test*.beam'`

TESTS_TO_INSTALL_SRC := `find $(PACKAGE_TOP) -name '*_test.erl' -o -name 'class_Test*.erl'`


DOC_TO_INSTALL := `find $(PACKAGE_TOP) -name tmp-rst -prune -o \( -type f -a \( -name '*.pdf' -o -name '*.html' -o -name '*.png' \) \)|grep -v tmp-rst`



# Source section.
COMMON_SRC := $(COMMON_TOP)/src

# BEAM path section.
COMMON_BEAM := $(COMMON_TOP)/src

# Include path section.
COMMON_INC = -I$(COMMON_SRC)


INC += $(foreach dir,$(BEAM_DIRS),-I$(dir))


# Erlang environment section.

ERLANG_ROOT     := ~/Software/Erlang/Erlang-current-install

ERLANG_COMPILER := `PATH=$(ERLANG_ROOT)/bin:$$PATH which erlc`


WARNING_LEVEL := 2

ERLANG_COMPILER_OPT_DEBUG := +debug_info

# The number of available cores on this computer:
CORE_COUNT := `grep -c processor /proc/cpuinfo`

ERLANG_INTERPRETER := `PATH=$(ERLANG_ROOT)/bin:$$PATH which erl`

ERLANG_INTERPRETER_OPT_DEBUG := -init_debug -smp auto


ifndef FQDN

#FQDN    := `host \`hostname\` | awk '{ print $$1 }' | head -n 1`
FQDN    := `hostname -f`

endif


ifndef VM_TEST_NAME

VM_NAME := ceylan_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := ceylan_test

endif


#DISTRIBUTED_NODE_OPT := -sname $(VM_NAME) -setcookie $(PROJECT_NAME)
DISTRIBUTED_NODE_OPT := -name $(VM_NAME)@$(FQDN) -setcookie $(PROJECT_NAME)


STARTUP_FUNCTION := run

BEAM_DIRS += . $(COMMON_BEAM)/data-management $(COMMON_BEAM)/maths \
		   $(COMMON_BEAM)/user-interface $(COMMON_BEAM)/utils


ERL_LAUNCHER = $(COMMON_SRC)/scripts/launch-erl.sh


# Add '+native' if native-code compilation is supported.
#
# Removed options:
#
# - ("+{warn_format,2}" would trigger unwanted useless warnings about format
# strings
#
# - -v/+verbose causes inlining to be too verbose, ex:
# "Old inliner: threshold=0 functions=[{setAttribute,3},.."
#
ERLANG_COMPILER_OPT += -b beam $(INC) -W$(WARNING_LEVEL)        \
	$(ERLANG_COMPILER_OPT_DEBUG) +bin_opt_info +warn_export_all \
	+warn_export_vars +warn_shadow_vars +warn_obsolete_guards   \
	+warn_unused_import	+warnings_as_errors                     \
	"+{debug_info_key,\"Ceylan\"}"


# +v (verbose) option removed, as only active if the interpreter has been
# compiled in debug mode (otherwise a warning is displayed at each execution).
# (note: variable generally not used, as using the ERL_LAUNCHER script instead)
ERLANG_INTERPRETER_OPT := -pz $(BEAM_DIRS) $(DISTRIBUTED_NODE_OPT)



# 'BEAM_PATH_OPT = --beam-paths $(BEAM_DIRS)' would be shorter, but would
# prevent adding other options to the launcher afterwards:
BEAM_PATH_OPT = $(foreach dir,$(BEAM_DIRS),--beam-dir $(dir))

# Removed:
#  --ln $(VM_TEST_NAME)
#  -c $(PROJECT_NAME), as not specifying it on the command line allows to
# select the one in ~/.erlang.cookie (if any), which is better
#
# OVERRIDDEN_OPT left for the user.
ERL_PARAMETERIZED_LAUNCHER = $(ERL_LAUNCHER) -v --fqdn $(FQDN) \
						   $(BEAM_PATH_OPT) $(OVERRIDDEN_OPT)

INTERNAL_OPTIONS := -start-verbatim-options $(INTERNAL_OPTIONS)
