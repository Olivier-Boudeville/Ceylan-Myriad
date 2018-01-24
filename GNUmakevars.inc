# Prerequisite: COMMON_TOP must be set.


# Project section.


# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME := Ceylan-Myriad
endif


COMMON_VERSION := 0.1

# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(COMMON_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME := Ceylan-Myriad
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP := $(COMMON_TOP)
endif


ARCHIVE_ROOT := $$HOME/Archives/$(PROJECT_NAME)


ARCHIVE_FILE := $(ARCHIVE_ROOT)/$$(date +'%Y%m%d')-$(PROJECT_NAME).tar.xz


VCS_ARCHIVE_FILE := $(ARCHIVE_ROOT)/$$(date +'%Y%m%d')-$(PROJECT_NAME).git-bundle


ARCHIVE_BRANCH_FILE := $(ARCHIVE_ROOT)/$$(date +'%Y%m%d')-$(PROJECT_NAME)-branch-$(shell basename $$(pwd)).tar.xz


SNAPSHOT_ARCHIVE_FILE := $(ARCHIVE_ROOT)/$$(date +'%Y%m%d')-$(PROJECT_NAME)-snapshot.tar.xz


PROJECT_RELEASE_BASE        := $(PROJECT_NAME)-$(PROJECT_VERSION)

PROJECT_RELEASE_ARCHIVE_ZIP := $(PROJECT_RELEASE_BASE).zip
PROJECT_RELEASE_ARCHIVE_BZ2 := $(PROJECT_RELEASE_BASE).tar.bz2
PROJECT_RELEASE_ARCHIVE_XZ  := $(PROJECT_RELEASE_BASE).tar.xz




# Installation section.


# If wanting to install that package, following installation prefix is used.
#
# INSTALLATION_PREFIX should be defined on a per-project basis, or be overridden
# on the command line (ex: 'make install INSTALLATION_PREFIX=/tmp'):

ifndef INSTALLATION_PREFIX

  INSTALLATION_PREFIX := $$HOME/Software

endif


PACKAGE_INSTALLATION_PREFIX := $(INSTALLATION_PREFIX)/$(PACKAGE_NAME)


# Could rely on the permissions instead:
#
# (not taking symbolic links to avoid duplicate targets)
#
EXEC_TO_INSTALL := $$(find $(PACKAGE_TOP) -name '*.sh' -a ! -name '*.exe' -a ! -type l)



BEAMS_TO_INSTALL := $$(find $(PACKAGE_TOP) -name examples -prune -o \( -name '*.beam' -a ! -name '*_test.beam' -a ! -name 'class_Test*.beam' \) -a -print)

SOURCES_TO_INSTALL := $$(find $(PACKAGE_TOP) -name examples -prune -o \( -name '*.erl' -a ! -name '*_test.erl' -a ! -name 'class_Test*.erl' \) -a -print)


EXAMPLES_TO_INSTALL_BEAMS := $$([ ! -d examples ] || find $(PACKAGE_TOP)/examples -name '*.beam')

EXAMPLES_TO_INSTALL_SRC := $$([ ! -d examples ] || find $(PACKAGE_TOP)/examples -name '*.erl')


INCLUDES_TO_INSTALL := $$(find $(PACKAGE_TOP) -name '*.hrl')


TESTS_TO_INSTALL_BEAMS := $$(find $(PACKAGE_TOP) -name '*_test.beam' -o -name 'class_Test*.beam')

TESTS_TO_INSTALL_SRC := $$(find $(PACKAGE_TOP) -name '*_test.erl' -o -name 'class_Test*.erl')


DOC_TO_INSTALL := $$(find $(PACKAGE_TOP) -name tmp-rst -prune -o \( -type f -a \( -name '*.pdf' -o -name '*.html' \) \) | grep -v tmp-rst)



# Install locations:

BIN_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/bin
EBIN_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/ebin
SRC_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/src
INCLUDE_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/include
EXAMPLES_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/examples
TEST_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/test
DOC_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/doc

DEFAULT_PRODUCTION_INSTALLATION_PREFIX := /usr/local/$(PACKAGE_NAME)


# Source section.
COMMON_SRC := $(COMMON_TOP)/src

# BEAM path section.
COMMON_BEAM := $(COMMON_TOP)/src

# Include path section.
COMMON_INC := -I$(COMMON_SRC)

# Contributions path section:
#COMMON_CONTRIB := $(COMMON_TOP)/contrib


# BEAM_DIRS are relative paths of BEAM files, ABS_BEAM_DIRS are the
# corresponding absolute paths.

INC += $(foreach dir,$(BEAM_DIRS),-I$(dir))


# To know when the including makefile is the sole root one:
#
ifndef ROOT_MAKEFILE

  ROOT_MAKEFILE := false

endif



# Extra prerequisite includes:

# Optional services, relying on third-party prerequisites:
#
# (for them, we do not force the corresponding variable here, in order to let
# the upper layers be able to decide; if not, the defaults apply)


# For the reading and writing of large, often scientific-oriented data:
#USE_HDF5 := false
#USE_HDF5 := true

# To gain the ability of acting as a client of a REST service:
#USE_REST := false
#USE_REST := true

# For SQL database operations, based on SQLite 3 operations:
#USE_SQLITE := false
#USE_SQLITE := true

# For using an Erlang/Python binding, based on the ErlPort library:
#USE_PYTHON_BINDING := false
#USE_PYTHON_BINDING := true

# For using an Erlang/Java binding, based on the JInterface library:
#USE_JAVA_BINDING := false
#USE_JAVA_BINDING := true



# Deactivated by default, as requires specific prerequisites (the HDF5 library
# and the erlhdf5 binding).
#
ifndef USE_HDF5

  USE_HDF5 := false

endif


ifeq ($(USE_HDF5),true)

  # If using erlhdf5 (for the hdf5_support module):

  ERLHDF5_BASE := $$HOME/Software/erlhdf5

  INC += -I$(ERLHDF5_BASE)/include

  # (ebin/erlhdf5.beam and priv/erlhdf5.so found at runtime)

endif


# Deactivated by default, as requires specific prerequisites (the jsx JSON
# parser, cf https://github.com/talentdeficit/jsx).
#
ifndef USE_REST

  USE_REST := false

endif


ifeq ($(USE_REST),true)

  JSX_BASE := $$HOME/Software/jsx/jsx-current-install

  INC += -I$(JSX_BASE)/include

  # ($(JSX_BASE)/ebin/*.beam found at runtime)

  ABS_BEAM_DIRS += $(JSX_BASE)/ebin/

endif



# Deactivated by default, as requires specific prerequisites:
#
# - the SQLite 3 library, ex: sqlite3 and sqlite3-dev packages in Debian, sqlite
#    on Arch Linux
# - the SQLite 3 Erlang binding we retained, erlang-sqlite3 (cf
#    https://github.com/alexeyr/erlang-sqlite3.git)
#
ifndef USE_SQLITE

  USE_SQLITE := false

endif


ifeq ($(USE_SQLITE),true)

  SQLITE3_BASE := $$HOME/Software/erlang-sqlite3

  INC += -I$(SQLITE3_BASE)/include

  # ($(SQLITE3_BASE)/ebin/*.beam found at runtime)

  ABS_BEAM_DIRS += $(SQLITE3_BASE)/ebin/

endif



# Deactivated by default, as requires specific prerequisites:
#
# - Python (binding tested with v3.6+ only)
# - the ErlPort library (cf https://github.com/hdima/erlport)
#
#
ifndef USE_PYTHON_BINDING

  USE_PYTHON_BINDING := false

endif

ifeq ($(USE_PYTHON_BINDING),true)

  ERLPORT_BASE := $$HOME/Software/ErlPort/ErlPort-current-install

  INC += -I$(ERLPORT_BASE)/src

  # ($(ERLPORT_BASE)/ebin/*.beam found at runtime)

  ABS_BEAM_DIRS += $(ERLPORT_BASE)/ebin

endif



# Deactivated by default, as requires specific prerequisites:
#
# - Java (binding tested with 8.0+ only)
# - the JInterface library, bundled with the Erlang runtime (cf
#   http://erlang.org/doc/man/jinterface.html)
#
ifndef USE_JAVA_BINDING

  USE_JAVA_BINDING := false

endif

ifeq ($(USE_JAVA_BINDING),true)

  # The path pointing to the target JInterface JAR (OtpErlang.jar), only needed
  # at compilation-time:
  #
  # Ex: cd [...]/Erlang-current-install/lib/erlang &&
  #     ln -s lib/jinterface-1.8.1 jinterface-current-install
  #
  JINTERFACE_ROOT = $(ERLANG_ROOT)/lib/erlang/jinterface

  CLASSPATH_OPT = -classpath $(JINTERFACE_ROOT)/priv/OtpErlang.jar

  JAVAC_OPT = $(CLASSPATH_OPT)

  # Only useful for manual testing:
  JAVA_OPT = $(CLASSPATH_OPT):.

endif


# Unless specified otherwise, the including makefile is not the one used as the
# base of a given layer (hence, for example, it is not expected to build in
# parallel):
#
ifndef BASE_MAKEFILE

  BASE_MAKEFILE := false

endif



# Overall settings section.

#EXECUTION_TARGET := production

ifndef EXECUTION_TARGET

  # Other possible value: production
  EXECUTION_TARGET := development

endif


# We have to define a symbol specific to the 'production' execution target
# (exec_target_is_production) instead of relying only on a execution_target
# variable which could be set either to 'development' or 'production', as the
# Erlang preprocessor allows only to test whether a symbol is defined (with
# -ifdef), we cannot test its value.
#
# So code is to use '-ifdef(target_production_exec)' instead of something like
# '-ifdef(execution_target,production)'.

ifeq ($(EXECUTION_TARGET),development)


  #$(info Execution target is development)

  ENABLE_DEBUG := true
  ENABLE_NATIVE_COMPILATION := false

  # No exec_target_is_production defined here:
  ERLANG_COMPILER_EXEC_TARGET_OPT :=

  ERLANG_COMPILER_DEBUG_OPT := -Ddebug_mode_is_enabled


else ifeq ($(EXECUTION_TARGET),production)


  #$(info Execution target is production)

  ENABLE_DEBUG := false

  # Native compilation may be enabled - with care:
  ENABLE_NATIVE_COMPILATION := true

  # Problem with orddict:erase/2; lists:fold/3, etc., thus switched off:
  #ENABLE_NATIVE_COMPILATION := false

  ERLANG_COMPILER_EXEC_TARGET_OPT := -Dexec_target_is_production
  ERLANG_COMPILER_DEBUG_OPT :=

else


  $(error Invalid execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')


endif


# Enforce default values:


ifndef ENABLE_DEBUG

  ENABLE_DEBUG := true

endif


ifndef ENABLE_NATIVE_COMPILATION

  ENABLE_NATIVE_COMPILATION := false

endif


# Apply settings:

# Tells which set of settings should be applied (allows for conditional code):
ERLANG_COMPILER_EXECUTION_TARGET_OPT += -Dexecution_target=$(EXECUTION_TARGET)

ifeq ($(ENABLE_NATIVE_COMPILATION),true)
	ERLANG_COMPILER_NATIVE_COMPILATION_OPT = +native
else
	ERLANG_COMPILER_NATIVE_COMPILATION_OPT =
endif






# Erlang environment section.

ERLANG_USUAL_ROOT := ~/Software/Erlang/Erlang-current-install

ERLANG_COMPILER := $$(PATH=$(ERLANG_USUAL_ROOT)/bin:$$PATH which erlc 2>/dev/null)


ERLANG_BIN_ROOT := $(shell dirname $(ERLANG_COMPILER) )

ERLANG_ROOT := $(shell dirname $(ERLANG_BIN_ROOT) )


# Note that at least some versions of dialyzer (ex: the one shipped with 17.1)
# will not deference anymore symlinks (if Erlang.plt pointing to Erlang-17.1.plt
# for example, it will not be found; this is managed in our installation
# script):
#
ERLANG_PLT_FILE := $(ERLANG_ROOT)/Erlang.plt


WARNING_LEVEL := 2

# Used even if not in debug mode:
ERLANG_COMPILER_OPT_DEBUG := +debug_info


# The number of available cores on this computer:
#
# (set to 1 in order to restore a sequential build)
#
CORE_COUNT := $$(grep -c processor /proc/cpuinfo)
#CORE_COUNT := 1

ERLANG_INTERPRETER := $$(PATH=$(ERLANG_USUAL_ROOT)/bin:$$PATH which erl 2>/dev/null)


# Not using '-smp auto' anymore, as the SMP mode is needed even with a single
# core if GUI (WxWindows) is to be used:
#
ERLANG_INTERPRETER_OPT_DEBUG := -init_debug -smp


# Finally not useful:
#ifndef FQDN

  #FQDN    := $$(host `hostname` | awk '{ print $$1 }' | head -n 1)
  FQDN    := $$(hostname -f)

		#FQDN_OPT= "--fqdn $(FQDN)"

#endif




ifndef VM_NAME

  VM_NAME := ceylan_debug

endif


ifndef VM_TEST_NAME

  VM_TEST_NAME := ceylan_test

endif


ifndef COOKIE

  COOKIE := "ceylan_myriad_cookie"

endif


COOKIE_OPT = -c $(COOKIE)

#DISTRIBUTED_NODE_OPT := -sname $(VM_NAME) -setcookie $(COOKIE)
DISTRIBUTED_NODE_OPT := -name $(VM_NAME)@$(FQDN) -setcookie $(COOKIE)


STARTUP_FUNCTION := run

BEAM_DIRS += $(COMMON_BEAM_DIRS)


# So that they can be fetched from outside
# (see the 'list-beam-dirs' target)
#
COMMON_BEAM_DIRS = $(COMMON_BEAM)/data-management \
				   $(COMMON_BEAM)/maths           \
				   $(COMMON_BEAM)/user-interface  \
				   $(COMMON_BEAM)/utils



ERL_LAUNCHER = $(COMMON_SRC)/scripts/launch-erl.sh



# Compiler section.


# Modules needed for the bootstrapping for others (hence to be built first, and
# not parse-transformed; typically used by parse-transforms)
#
# (see the 'Bootstrap section' in GNUmakerules.inc for their special-cased
# build)
#
ifndef BOOTSTRAP_MODULES

# We list here all prerequisites of the 'Common' parse-transform:
#  - basic_utils: to provide the basic services
#  - id_utils: to sort the forms
#  - meta_utils: to support the metaprogramming needs
#  - ast_utils: to handle the AST
#  - ast_scan: to scan the AST
#  - text_utils: to display debugging information regarding the transform
#  - map_hashtable: to manage the module information
#
BOOTSTRAP_MODULES = $(COMMON_TOP)/src/utils/basic_utils.beam             \
					$(COMMON_TOP)/src/utils/id_utils.beam                \
					$(COMMON_TOP)/src/utils/meta_utils.beam              \
					$(COMMON_TOP)/src/utils/ast_utils.beam               \
					$(COMMON_TOP)/src/utils/ast_scan.beam                \
					$(COMMON_TOP)/src/utils/type_utils.beam              \
					$(COMMON_TOP)/src/utils/text_utils.beam              \
					$(COMMON_TOP)/src/data-management/map_hashtable.beam \

endif


## Parse-transform related section.


# List here all the directories expected to contain parse transforms:
#
# (to be overridden by each upper layer)
#
ifndef PARSE_TRANSFORM_DIRS

PARSE_TRANSFORM_DIRS = $(COMMON_TOP)/src/utils

endif


# Base compilation options that should be used for all modules (third-party
# included):
#
# -v/+verbose causes inlining to be too verbose, ex:
# "Old inliner: threshold=0 functions=[{setAttribute,3},.."
#
# (src/utils is specified as all modules - parse-transformed or not - need to be
# able to find the BEAMs of parse-transforms)
#
ERLANG_COMPILER_BASE_OPT += -b beam                                   \
						  +\"{debug_info_key,\\\"Ceylan-Myriad\\\"}\" \
						  -pz $(COMMON_TOP)/src/utils


# To apply a parse transform, not only it must be already compiled, but also,
# when compiling the module to be transformed, the BEAM of the transform must be
# found:
#
# (add -P to generate, when compiling X.erl, a X.P file listing the transformed
#  sources that are actually compiled)
#
# We conditionally defined this option, as upper layers may want to trigger
# their own parse transforms *instead* (probably that will call directly, by
# themselves, at the Erlang level, this 'Common' parse transform):
#
ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT

# We need src/utils and src/data-management in the path, as the 'Common' parse
# transform uses meta_utils that uses map_hashtable (which is thus bootstrapped
# as well).

# Note: if, when compiling a module, the following error message is issued:
# "undefined parse transform 'common_parse_transform'", then it is most probably
# that the code of the parse transform actually calls, directly or not, a
# non-existing code (typically a function of a module to be built later)
#
ERLANG_COMPILER_PARSE_TRANSFORM_OPT = \
					-pz $(COMMON_TOP)/src/utils                 \
					-pz $(COMMON_TOP)/src/data-management       \
					"+{parse_transform,common_parse_transform}"

endif


# Warning-related options (may or may not be included):
# "+{warn_format,2}" would trigger unwanted useless warnings about format
# strings
ERLANG_COMPILER_WARNING_OPT += -W$(WARNING_LEVEL) +warn_export_all  \
	+warn_export_vars +warn_shadow_vars +warn_obsolete_guards       \
	+warn_unused_import +warnings_as_errors

# Once type specs will have been added: +warn_untyped_record +warn_missing_spec


# No general inlining is enforced, as:
#
# - the induced gain is not obvious, compared to the non-inlined version
#
# - inlining seems to be triggered only through a in-file directive, whereas no
# general include is used here
#
# Otherwise we would have added, on a per-file basis:
# -compile(inline).
# -compile( {inline_size,64} ).


# +bin_opt_info removed, as even efficient binary handling yields "warning"
# messages ("OPTIMIZED: creation of sub binary delayed"), which are treated as
# errors afterwards.
#
# Instead 'export ERL_COMPILER_OPTIONS=bin_opt_info' shall be punctually used.
#
ERLANG_COMPILER_OPT_BASE += $(ERLANG_COMPILER_BASE_OPT) $(INC)           \
	$(ERLANG_COMPILER_EXEC_TARGET_OPT) $(ERLANG_COMPILER_DEBUG_OPT)      \
	$(ERLANG_COMPILER_NATIVE_COMPILATION_OPT)                            \
	$(ERLANG_COMPILER_WARNING_OPT)                                       \
	$(ERLANG_COMPILER_OPT_DEBUG)


# To compile parse transforms themselves:
#
ERLANG_COMPILER_OPT_FOR_PT += $(ERLANG_COMPILER_OPT_BASE)


# For all modules that are not parse transforms (hence that may be
# parse-transformed):
#
ERLANG_COMPILER_OPT += $(ERLANG_COMPILER_OPT_BASE)

# To centralise all options but the parse transform trigger:
ERLANG_COMPILER_OPT_NO_PT := $(ERLANG_COMPILER_OPT)

ERLANG_COMPILER_OPT += $(ERLANG_COMPILER_PARSE_TRANSFORM_OPT)


# For the Java binding; expecting Java 8 or higher:
JAVAC := $$(which javac)

# Optional variable, used for testing only (never directly executed from the
# shell):
#
JAVA := $$(which java)



# Runtime settings.



# +v (verbose) option removed, as only active if the interpreter has been
# compiled in debug mode (otherwise a warning is displayed at each execution).
#
# (note: variable generally not used, as using the ERL_LAUNCHER script instead)
#
ERLANG_INTERPRETER_OPT := -pz $(BEAM_DIRS) $(DISTRIBUTED_NODE_OPT)




# Erlang Default (only used in a node that is distributed):
#EPMD_PORT := 4369

# 'Common' default, to force correct port management:
#
# Note: if modifying this port definition, upper layers may have to modify that
# accordingly.
#
# Notably, if a VM is ultimately started with the --nn option (i.e. not
# distributed), enabling distribution at runtime (see
# net_utils:enable_distribution/2) will lead this VM to expect an EPMD at the
# port specified here (thus this information is taken into account in all cases,
# even when starting non-distributed)
#
# Always test with a prior 'killall epmd' on all hosts involved, and use 'epmd
# -d -port XXXX' to check whether there is an EPMD at that port (this is the
# case iff 'there is already a epmd running at port XXXX' is output)
#
EPMD_PORT := 4506
#EPMD_PORT := 4514


# We can force the launched VM to use TCP/IP connections in a range of ports
# enabled by the firewalls and/or to rely on a non-standard EPMD port:
#
# (note that, should the user code launch other VMs and/or have to communicate
# with other VMs, the corresponding settings will have to match these ones -
# think to all upper layers which may rely implicitly on that port information)
#
#FIREWALL_OPT :=
FIREWALL_OPT := --tcp-range 50000 55000 --epmd-port $(EPMD_PORT)


ifdef MAX_PROCESS_COUNT
	MAX_PROCESS_OPT := --max-process-count $(MAX_PROCESS_COUNT)
endif


ifdef ASYNC_THREAD_COUNT
	ASYNC_THREAD_OPT := --async-thread-count $(ASYNC_THREAD_COUNT)
endif





# 'BEAM_PATH_OPT = --beam-paths $(BEAM_DIRS)' would be shorter, but would
# prevent adding other options to the launcher afterwards:
# (now we use absolute directories - thanks to 'pwd' - instead of relative ones,
# as, otherwise, as soon as a process changes the current working directory, any
# further loading of a user-defined module, for example if used then for the
# first time, would fail).
BEAM_PATH_OPT = $(foreach dir,$(BEAM_DIRS),--beam-dir $$(pwd)/$(dir)) \
				$(foreach dir,$(ABS_BEAM_DIRS),--beam-dir $(dir))



# Increases the distribution buffer busy limit (default is 1024):
# See http://erlang.org/doc/man/erl.html#+zdbbl.
#
# 1024*1024 = 1048576
BUSY_LIMIT_OPT := --busy-limit 1048576

SCALE_OPT := $(BUSY_LIMIT_OPT)


# Removed:
#
#  --ln $(VM_TEST_NAME)
#
#  -c $(PROJECT_NAME), as not specifying it on the command line allows to
# select the one in ~/.erlang.cookie (if any), which is better
#
#  -v, as the verbose mode for the launcher led to too many lines being output,
# now that user-defined code paths are absolute, thus significantly longer
#
# OVERRIDDEN_OPT left for the user.
ERL_PARAMETERIZED_LAUNCHER = $(ERL_LAUNCHER) $(FQDN_OPT) $(FIREWALL_OPT)  \
							 $(MAX_PROCESS_OPT) $(ASYNC_THREAD_OPT)       \
							 $(BEAM_PATH_OPT) $(SCALE_OPT) $(COOKIE_OPT)  \
							 $(OVERRIDDEN_OPT)


# Previously the short names were preferred, as they seemed to resist to
# non-existing or, more probably, improperly configured, DNS systems.
#
# However other issues may arise with them (ex: an inappropriate domain being
# taken by Erlang from /etc/resolv.conf), so we went back to the (supposedly
# more precise) long names.
#
# Then we realized that some use cases required the node name to be changed at
# runtime, implying the node should not be defined initially as a distributed
# one (hence the --nn option).
#
# This setting can also be overridden from the command-line (variable used in
# generic rules):
#
# (use net_utils:get_node_naming_mode/0 to detect the naming mode at runtime)
#
ifndef NODE_NAMING
  NODE_NAMING="--ln"
  #NODE_NAMING="--sn"
  #NODE_NAMING="--nn"
endif


# Options used internally, added automatically and transparently:
INTERNAL_OPTIONS := -start-verbatim-options $(INTERNAL_OPTIONS)


DIALYZER := $$(PATH=$(ERLANG_USUAL_ROOT)/bin:$$PATH which dialyzer 2>/dev/null)


# Options for Dialyzer, see http://www.erlang.org/doc/man/dialyzer.html:
# Could be added: -Wunderspecs (obsolete: -Wbehaviours)
DIALYZER_OPT := -Wrace_conditions


# To use a generic rule to produce it (unless already defined):
ifndef PLT_FILE
  PLT_FILE := $(PACKAGE_TOP)/$(PACKAGE_NAME).plt
endif

# To use a generic rule to produce it (unless already defined):
ifndef TYPE_LIST_FILE
  TYPE_LIST_FILE := $(PACKAGE_TOP)/declared-types-in-$(PACKAGE_NAME).txt
endif


# For later reuse in the next layer of the software stack:
COMMON_PLT_FILE := $(COMMON_TOP)/common.plt


# Shorthand:
BATCH_OPT = 'CMD_LINE_OPT="--batch"'


# This is the merged PLT of the level just below in the software stack.
#
# For 'Common', it is the PLT of Erlang/OTP (possibly enriched by
# prerequisites):
#
ifndef PREDECESSOR_PLT
  PREDECESSOR_PLT := $(ERLANG_PLT_FILE)
endif


MAKE_CODE_STATS := $(COMMON_TOP)/src/scripts/make-code-stats.sh
