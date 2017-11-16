# From here all variables are expected to be updated, as first all the variables
# of englobing projects should be set, then only rules based on that variables
# should be defined.



# Now defining the generic rules operating on the variables.

ALL_ERL_FILES = $(wildcard *.erl)



# Excluded sources, as depending on prerequisites:


ifeq ($(USE_HDF5),false)

  EXCLUDED_SOURCES := $(EXCLUDED_SOURCES) hdf5_support.erl      \
										  hdf5_support_test.erl \
										  raw_hdf5_test.erl

endif


ifeq ($(USE_REST),false)

  EXCLUDED_SOURCES := $(EXCLUDED_SOURCES) rest_support.erl rest_support_test.erl

endif


ifeq ($(USE_SQLITE),false)

  EXCLUDED_SOURCES := $(EXCLUDED_SOURCES) sql_support.erl sql_support_test.erl

endif



ERL_FILES = $(filter-out $(EXCLUDED_SOURCES), $(ALL_ERL_FILES))


# Includes test beams:
BEAM_FILES = $(patsubst %.erl,%.beam,$(ERL_FILES))


# Excludes the tests depending on prerequisites:
TEST_SOURCES = $(filter-out $(EXCLUDED_SOURCES), $(wildcard *_test.erl))

TEST_TARGETS = $(patsubst %.erl,%,$(TEST_SOURCES))


# Regarding dependency management.

# One can just ignore dependencies and rebuild "blindly" every module in current
# subtree.

# One just has not to forget to issue 'make' from a right location, before
# running one's Erlang program.

# Another option is to track dependencies explicitly.

# We found no existing way of managing dependencies automatically, thus they
# have to be declared by hand.

# We want these user-specified dependencies to work with the generic rules to
# build beam files and to run programs.

# For example, 'make MyObject_run' would check MyObject_test.beam and its
# dependencies before running it.

# A generic rule like: '%.beam: %.erl' would not take into account dependencies.

# A generic rule like: '%.beam: %.erl %_dependencies' with a definition like
# 'MyObject_dependencies: A.beam moduleB.beam' would work (and modules not
# depending on others could be managed with a second generic rule: '%.beam:
# %.erl'), but generic targets, as defined in GNUmakerules.inc, seem to be
# *always* rebuilt, not depending on their .PHONY status.

# In our case, MyObject.beam (and all its prerequisites!) would thus always be
# rebuilt, even if no change at all was operated on the corresponding sources,
# which would not be acceptable.

# Thus we stick from now to the basic strategy: always issue a global 'make' at
# the root of the source before running a program, otherwise be doomed (changes
# not taken into account at run-time, as not recompiled).


.PHONY: all all-pre-hook all-parallel-recurse all-recurse rebuild			\
	test test-recurse doc-recurse build-tests launch						\
	install-all install-announce install-ebin install-bin install-src		\
	install-include install-examples install-test install-doc				\
	info-install info-install-locations info-install-content                \
	generate-list-of-erlang-types generate-list-of-local-types              \
	generate-local-plt self-check-against-plt check check-hook help-erl     \
	clean clean-erlang clean-python clean-database clean-recurse            \
	clean-outputs real-clean info info-files $(MODULES_DIRS)



# Note: long lines in rules should not be word-wrapped (with \), as their
# displaying on the console would print them, leading to poorly dense outputs,
# that are not pleasing to the eye.


# Defining the BASE_MAKEFILE variable in top makefiles allows to define a
# specific (parallel) 'all' rule for them, while other makefiles just recurse
# accordingly:
ifeq ($(BASE_MAKEFILE),true)

# CORE_COUNT defined in common/GNUmakevars.inc:
all: all-pre-hook all-parse-transforms
	@echo "   Building all, in parallel over $(CORE_COUNT) core(s), from "$(PWD) #$$(basename $(PWD))
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m &&  \
		$(MAKE) -s all-recurse -j $(CORE_COUNT) && cd .. ; else echo "     (directory $$m skipped)" ; \
	fi ) ; then exit 1; fi ; done

else

all: all-recurse-pre-hook all-recurse $(BEAM_FILES)

endif



all-parse-transforms: $(BOOTSTRAP_MODULES) make-all-parse-transforms


make-all-parse-transforms:
	@echo "   Building first all layer-specific parse transforms"
	@for d in $(PARSE_TRANSFORM_DIRS) ; do ( cd $$d && \
	$(MAKE) -s local-parse-transforms ) ; done


LOCAL_TRANSFORM_SRC = $(wildcard *_parse_transform.erl)

LOCAL_TRANSFORM_BEAM = $(patsubst %.erl,%.beam,$(LOCAL_TRANSFORM_SRC))


# It is necessary to compile parse-transforms from their directory, as they may
# use bootstrapped BEAMs or lower layers, so the _TOP variable must be correct
# in order the -pz directories to be relevant:
#
local-parse-transforms: $(LOCAL_TRANSFORM_BEAM)



# all*-pre-hook targets allow to insert arbitrary rules *before* other specified
# prerequisite targets, at each level.
#
# Meant to be enriched with prerequisite targets by above layers:

# Called once per layer, when starting compiling it:
all-pre-hook:



# Called once per subdirectory of the current layer, when starting compiling it:
all-recurse-pre-hook:


# This rule is not used by default, as we use the -j option from the root
# makefile, letting then 'make' use its job server for that task:
all-parallel-recurse:
	@echo "   Building all on parallel over $(CORE_COUNT) cores (in "$(PWD) #$$(basename $(PWD))
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s all -j $(CORE_COUNT) CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ; else echo "	  (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done



all-recurse: $(BEAM_FILES)
	@echo "   Building all in "$(PWD) #$$(basename $(PWD))
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s all CMD_LINE_OPT="${CMD_LINE_OPT}" USE_HDF5=$(USE_HDF5) USE_REST=$(USE_REST) USE_SQLITE=$(USE_SQLITE) && cd .. ; else echo "	 (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done



# We must first go into the directory in which the source of the parse transform
# lies, otherwise the BEAM file will be created on the current (ex: root)
# directory:
#
%_parse_transform.beam: %_parse_transform.erl
	@echo "		Compiling parse transform $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_PT) -o $@ $<


rebuild: clean all



# Bootstrap section.


# Bootstrapped modules must be special-cased, as they should not be built with
# parse-transforms (by design not available yet) and the resulting BEAM must be
# produced in the right directory (not at the root).
#
# Exactly the same processing applies to each of them.


$(COMMON_TOP)/src/utils/meta_utils.beam: $(COMMON_TOP)/src/utils/meta_utils.erl
	@echo "		Compiling bootstrap module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_PT) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )


$(COMMON_TOP)/src/utils/text_utils.beam: $(COMMON_TOP)/src/utils/text_utils.erl
	@echo "		Compiling bootstrap module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_PT) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )


$(COMMON_TOP)/src/data-management/map_hashtable.beam: $(COMMON_TOP)/src/data-management/map_hashtable.erl
	@echo "		Compiling bootstrap module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_PT) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )



# Standard general rules:


%.beam: %.erl %.hrl
	@echo "		Compiling module with header $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $<


%.beam: %.erl
	@echo "		Compiling module $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT) -o $@ $<



# Applications are meant to be executed, whereas tests are meant to be run: they
# have different suffixes (_app vs _test) and ways of being launched (make
# X_exec / make X_run) otherwise, for example, my_server_app.erl would shadow
# my_server_test.erl if an ambiguous 'make my_server_run' was issued.



# Application section.


# 'X_exec' becomes 'X_app:exec()':


%_exec: %_app.beam %.beam
	@echo "		Executing application $^ (first form)"
	$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$$USER --eval $$(echo $@|sed 's|_exec$$|_app:exec()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)


%_exec: %_app.beam
	@echo "		Executing application $^ (second form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$$USER --eval $$(echo $@|sed 's|_exec$$|_app:exec()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)



# To run in the background (somewhat like a daemon):
#
# Note: the specified long name (with $(NODE_NAMING)) has its '_background'
# suffix removed so that the node name of an executable does not depend from how
# it is launched, otherwise clients needing to connect to that node would
# themselves depend on that.

%_exec_background: %_app.beam %.beam
	@echo "		Executing application $^ in the background (first form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@|sed 's|_background$$||1')-$$USER --background --eval $$(echo $@|sed 's|_exec_background$$|_app:exec()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT) &


%_exec_background: %_app.beam
	@echo "		Executing application $^ in the background (second form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@|sed 's|_background$$||1')-$$USER --background --eval $$(echo $@|sed 's|_exec_background$$|_app:exec()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT) &


# The two sets of rules below are functional, yet they use very different
# solutions (run_erl/eval) that can tested independently.


# To run an application as a service.
#
# Exactly like when running as %_exec_background, except services are shared
# hence their node name must no depend on any user name; they are directly named
# as their core target (ex: 'make X_exec_service' results in a node named
# 'X@LOCALHOSTNAME').
#
# (these rules uses run_erl, that runs in the background)


%_exec_service: %_app.beam %.beam
	@echo "		Executing application $^ as a service (first form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@|sed 's|_exec_service$$||1') --daemon --eval $$(echo $@|sed 's|_exec_service$$|_app:exec()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)


%_exec_service: %_app.beam
	@echo "		Executing application $^ as a service (second form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@|sed 's|_exec_service$$||1') --daemon --eval $$(echo $@|sed 's|_exec_service$$|_app:exec()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)



# Interactive variants, for service debugging.
#
# These rules used to rely on a standard 'erl -eval' put in the background, with
# the --background option, i.e so that the VM was run in detached mode (yet
# without run_erl).
#
# There was also a final '&', leading to a crash ("*** Terminating erlang...")
# most probably because the VM was twice set in background.
#
# One may get rid in all cases of that '&', and the --background option may or
# may not be added. Its main drawback is that no output or activity is then
#  shown on the console.
#
%_exec_service_debug: %_app.beam %.beam
	@echo "		Executing application $^ as a debugged service (first form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@|sed 's|_exec_service_debug$$||1') --eval $$(echo $@|sed 's|_exec_service_debug$$|_app:exec()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)


%_exec_service_debug: %_app.beam
	@echo "		Executing application $^ as a debugged service (second form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@|sed 's|_exec_service_debug$$||1') --eval $$(echo $@|sed 's|_exec_service_debug$$|_app:exec()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)



# Test section.


#%_test: %.beam %_test.beam
#	@echo "    Running test function $(STARTUP_FUNCTION) in module $@"
#	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION)


#%_test: %_test.beam
#	@echo "    Running test function $(STARTUP_FUNCTION) in module $@"
#	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION)



%_test: %.beam %_test.beam
		@# Even just a comment is needed here, to force rebuild.


%_test: %_test.beam
		@# Even just a comment is needed here, to force rebuild.


%_interactive_test: %.beam %_interactive_test.beam
	@echo "		Executing interactively test function "
	"$(STARTUP_FUNCTION) in module $@"
	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION) $(INTERNAL_OPTIONS)


%_batch_test: %.beam %_batch_test.beam
	@echo "		Executing non-interactively test function "
	"$(STARTUP_FUNCTION) in module $@"
	@$(ERLANG_INTERPRETER) $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION) $(INTERNAL_OPTIONS)



# _integration prefix added not to match instead of the next rule.
# %_integration_dependencies target is a phony target, so that test dependencies
# can be specified.
%_integration_run: %_integration_test.beam %_integration_dependencies
	@echo "		Running integration test $@ from $^ "
	"with $(ERL_PARAMETERIZED_LAUNCHER)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$$USER --eval $$(echo $@|sed 's|_run$$|_test:run()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)



# To run on a cluster is basically running with no shell nor interactive input,
# but without being put in the background (no ending '&', no detached option,
# etc.)


%_cluster_run: %_test.beam %.beam
	@echo "		Executing test $^ for the cluster (first form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@|sed 's|_cluster_run$$|_run|1')-$$USER --non-interactive --eval $$(echo $@|sed 's|_cluster_run$$|_test:run()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)


%_cluster_run: %_test.beam
	@echo "		Executing test $^ for the cluster (second form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@|sed 's|_cluster_run$$|_run|1')-$$USER --non-interactive --eval $$(echo $@|sed 's|_cluster_run$$|_test:run()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)




# 'X_run' becomes 'X_test:run()':


%_run: %_test %_test_dependencies
	@echo "		Running unitary test $@ (first form) from $^, with $(ERL_PARAMETERIZED_LAUNCHER)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$$USER --eval $$(echo $@|sed 's|_run$$|_test:run()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)


%_run: %_test %.beam
	@echo "		Running unitary test $@ (second form) from $^"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$$USER --eval $$(echo $@|sed 's|_run$$|_test:run()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)


# Again and again one will remove the '@' to inspect the actual command-line:
%_run: %_test
	@echo "		Running unitary test $@ (third form) from $^"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$$USER --eval $$(echo $@|sed 's|_run$$|_test:run()|1') $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)


%_shell: %_test
	@echo "		Launching a shell for test $@ from $^"
	@echo "  (one may then enter: '$<:run().' to execute the test)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$$USER  $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)


test: all test-recurse
	@for t in $(TEST_TARGETS); do if ! $(MAKE) -s $$(echo $$t|sed 's|_test$$|_run|1') CMD_LINE_OPT="${CMD_LINE_OPT} --batch"; then echo "Test failed." 1>&2 ; exit 1; fi ; done


test-recurse:
	@echo "   Testing all in $$(basename $(PWD))"
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s test CMD_LINE_OPT="${CMD_LINE_OPT} --batch" && cd .. ; else echo "	  (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done


# Best placed here rather than in GNUmakerules-docutils.inc:
doc: doc-recurse

doc-recurse:
	@echo "   Preparing documentation in "$(PWD) #$$(basename $(PWD))
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s doc CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ; else echo "	 (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done



# Not used any more now that all beams are always built:
build-tests: $(BEAM_FILES)
	@for t in $(TEST_TARGETS); do $(MAKE) -s $$t CMD_LINE_OPT="${CMD_LINE_OPT}"; done


launch: Emakefile
	@echo "		Launching interpreter with default Ceylan settings"
	@${ERL_PARAMETERIZED_LAUNCHER} $(NODE_NAMING) test_shell-$$USER $(INTERNAL_OPTIONS) $(CMD_LINE_OPT)




# Installation section.


# For a development-ready install:
#
install-package: all doc install-all


# For a production-ready install:
#
#
install-prod-standard: all
	@sudo $(MAKE) install-prod PACKAGE_INSTALLATION_PREFIX=$(DEFAULT_PRODUCTION_INSTALLATION_PREFIX)



# In a production setting, We do not want to generate documentation in
# production or to install extra elements (such as tests, examples, etc.):
#
install-prod: all install-prod-all




install-all: install-announce install-bin install-ebin install-src \
	install-include install-examples install-test install-doc


install-prod-all: install-prod-announce install-bin install-ebin \
	install-src install-include



install-announce:
	@echo " Installing this package ($(PACKAGE_NAME)) for development \
	in $(PACKAGE_INSTALLATION_PREFIX)"


install-prod-announce:
	@echo "  Installing this package ($(PACKAGE_NAME)) for production \
	in $(PACKAGE_INSTALLATION_PREFIX)"


# The -L option for cp is used to force the copy of the file which is pointed to
# by a symbolic link, instead of copying that link, which is generally relative
# and broken if copied in an installation 'as is'.


install-bin:
	@if [ -n "$(EXEC_TO_INSTALL)" ] ; then \
	mkdir -p $(BIN_INSTALLATION_PREFIX) ; \
	/bin/cp $(EXEC_TO_INSTALL) $(BIN_INSTALLATION_PREFIX) ; fi


install-ebin:
	@if [ -n "$(BEAMS_TO_INSTALL)" ] ; then \
	mkdir -p $(EBIN_INSTALLATION_PREFIX) ; \
	/bin/cp -L $(BEAMS_TO_INSTALL) $(EBIN_INSTALLATION_PREFIX) ; fi


install-src:
	@if [ -n "$(SOURCES_TO_INSTALL)" ] ; then \
	mkdir -p $(SRC_INSTALLATION_PREFIX) ; \
	/bin/cp -L $(SOURCES_TO_INSTALL) $(SRC_INSTALLATION_PREFIX) ; fi


install-include:
	@if [ -n "$(INCLUDES_TO_INSTALL)" ] ; then \
	mkdir -p $(INCLUDE_INSTALLATION_PREFIX) ; \
	/bin/cp -L $(INCLUDES_TO_INSTALL) \
	$(INCLUDE_INSTALLATION_PREFIX) ; fi


install-examples:
	@if [ -n "$(EXAMPLES_TO_INSTALL_BEAMS)" ] ; then \
	mkdir -p $(EXAMPLES_INSTALLATION_PREFIX)/ebin ; \
	/bin/cp -L $(EXAMPLES_TO_INSTALL_BEAMS) \
	$(EXAMPLES_INSTALLATION_PREFIX)/ebin ; fi
	@if [ -n "$(EXAMPLES_TO_INSTALL_SRC)" ] ; then \
	mkdir -p $(EXAMPLES_INSTALLATION_PREFIX)/src ; \
	/bin/cp -L $(EXAMPLES_TO_INSTALL_SRC) \
	$(EXAMPLES_INSTALLATION_PREFIX)/src ; fi


install-test:
	@if [ -n "$(TESTS_TO_INSTALL_BEAMS)" ] ; then \
	mkdir -p $(TEST_INSTALLATION_PREFIX)/ebin ; \
	cp -L $(TESTS_TO_INSTALL_BEAMS) \
	$(TEST_INSTALLATION_PREFIX)/ebin ; fi
	@if [ -n "$(TESTS_TO_INSTALL_SRC)" ] ; then \
	mkdir -p $(TEST_INSTALLATION_PREFIX)/src ; \
	cp -L $(TESTS_TO_INSTALL_SRC) \
	$(TEST_INSTALLATION_PREFIX)/src ; fi


install-doc:
	@if [ -n "$(DOC_TO_INSTALL)" ] ; then \
	mkdir -p $(DOC_INSTALLATION_PREFIX) ; \
	/bin/cp -r -L $(DOC_TO_INSTALL) $(DOC_INSTALLATION_PREFIX) ; fi



info-install: info-install-locations info-install-content


info-install-locations:
	@echo   "PACKAGE_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)"
	@echo   "BIN_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/bin"
	@echo   "EBIN_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/ebin"
	@echo   "SRC_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/src"
	@echo   "INCLUDE_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/include"
	@echo   "EXAMPLES_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/examples"
	@echo   "TEST_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/test"
	@echo   "DOC_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/doc"
	@echo   "DEFAULT_PRODUCTION_INSTALLATION_PREFIX = $(DEFAULT_PRODUCTION_INSTALLATION_PREFIX)"


info-install-content:
	@echo "  EXEC_TO_INSTALL = $(EXEC_TO_INSTALL)"
	@echo "  BEAMS_TO_INSTALL = $(BEAMS_TO_INSTALL)"
	@echo "  SOURCES_TO_INSTALL = $(SOURCES_TO_INSTALL)"
	@echo "  INCLUDES_TO_INSTALL = $(INCLUDES_TO_INSTALL)"
	@echo "  EXAMPLES_TO_INSTALL_BEAMS = $(EXAMPLES_TO_INSTALL_BEAMS)"
	@echo "  EXAMPLES_TO_INSTALL_SRC = $(EXAMPLES_TO_INSTALL_SRC)"
	@echo "  TESTS_TO_INSTALL_BEAMS = $(TESTS_TO_INSTALL_BEAMS)"
	@echo "  TESTS_TO_INSTALL_SRC = $(TESTS_TO_INSTALL_SRC)"
	@echo "  DOC_TO_INSTALL = $(DOC_TO_INSTALL)"



# Allows to generate a file listing all the types defined in the specified
# Erlang source tree.
#
# Ex: 'make generate-list-of-erlang-types ERLANG_SOURCE_ROOT=~/otp_src_R15B/'
#
generate-list-of-erlang-types:
	@if [ -z "$$ERLANG_SOURCE_ROOT" ] ; then echo "  Error, no ERLANG_SOURCE_ROOT variable specified." ; exit 15 ; fi ; if [ ! -d "$$ERLANG_SOURCE_ROOT" ] ; then echo "  Error, specified directory (ERLANG_SOURCE_ROOT=$$ERLANG_SOURCE_ROOT) does not exist." ; exit 16 ; fi ; target_file="declared-types-in-Erlang.txt" ; erl="$(ERLANG_INTERPRETER)" ; target_path=$$(echo $$erl|sed "s|/bin/erl$$|/$$target_file|1") ; echo "  Generating the list of types declared in the Erlang runtime..." &&  $(COMMON_TOP)/src/scripts/list-available-types.sh "$$ERLANG_SOURCE_ROOT" > $$target_path && echo "  Types have been collected in '$$target_path'."



generate-list-of-local-types:
	@echo "    Listing all types defined in package '$(PACKAGE_NAME)', in the $$(basename $(TYPE_LIST_FILE)) file"
	@$(COMMON_TOP)/src/scripts/list-available-types.sh > $(TYPE_LIST_FILE)



$(PLT_FILE): generate-local-plt


# Overriding BASE_PLT is useful to take into account third-party prerequisites:
ifndef BASE_PLT

  BASE_PLT := $(PREDECESSOR_PLT)

endif


# We do not want to issue an error if only warnings (code: 2) were emitted:
generate-local-plt: add-prerequisite-plts
	@echo "    Generating PLT for $(PACKAGE_NAME) ($(PLT_FILE)), based on $(BASE_PLT)"
	@$(DIALYZER) --add_to_plt --output_plt $(PLT_FILE) -r $(PACKAGE_TOP)/src --plt $(BASE_PLT); if [ $$? -eq 1 ] ; then exit 1 ; else exit 0 ; fi


# Made to be enriched (with child targets) on a per-layer basis:
add-prerequisite-plts:
	@echo "Prerequisite PLTs added."



self-check-against-plt: all $(PLT_FILE)
	@echo "    Checking $(PACKAGE_NAME) against its PLT ($(PLT_FILE))"
	@$(DIALYZER) $(DIALYZER_OPT) --plt $(PLT_FILE) -r .


# Far easier target name to remember:
# (hook added to block this target if not relevant from the current location)
#
check: check-hook clean all test generate-local-plt self-check-against-plt


check_hook:

%.plt: %.beam
	@echo "    Checking module '$<' against relevant PLT ($(PLT_FILE))"
	@$(DIALYZER) $(DIALYZER_OPT) --plt $(PLT_FILE) $<


help-erl:
	@echo "To test hello.erl: 'erl', then 'c(hello).'," \
		"then 'hello:world().', then CTRL-C CTRL-C"


clean: clean-erlang clean-python clean-database clean-override clean-recurse


clean-erlang:
	@echo "    Cleaning all in "$(PWD) #$$(basename $(PWD))
	-@/bin/rm -f *.beam *.jam erl_crash.dump erlang.log.*


# We can have a Python binding:
clean-python:
	-@/bin/rm -rf '__pycache__'


clean-database:
	-@/bin/rm -rf Mnesia.*@*


# This target has been added in order to allow the packages making use of
# 'Common' to define their specific cleaning rules.
#
# They could have defined in their own GNUmakerules.inc something like:
#	clean: clean-mypackage
#	clean-mypackage:
#	  -@/bin/rm foo.mypackage
# but then this 'clean' target would become the default one, whereas we expect
# the 'all' default target of 'Common' to be triggered.
# Thus we finally allowed upper packages to make their cleaning thanks to:
# FILES_TO_CLEAN += foo.mypackage in their GNUmakevars.inc
clean-override:
	@#echo "FILES_TO_CLEAN = $(FILES_TO_CLEAN)"
	-@/bin/rm -f $(FILES_TO_CLEAN)


ifeq ($(ROOT_MAKEFILE),true)

# More talkative:
clean-recurse:
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then      \
	echo "   Cleaning in '$$(basename $$m)' layer" ; cd $$m &&            \
	$(MAKE) -s clean CMD_LINE_OPT="${CMD_LINE_OPT}" &&   \
	cd .. ; else echo "		(directory $$m skipped)" ; fi ) ; then \
	exit 1; fi ; done

else

clean-recurse:
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then      \
	cd $$m && $(MAKE) -s clean CMD_LINE_OPT="${CMD_LINE_OPT}" &&   \
	cd .. ; else echo "		(directory $$m skipped)" ; fi ) ; then \
	exit 1; fi ; done

endif


clean-outputs:


# To be enriched by user layers:
real-clean: clean real-clean-recurse

real-clean-recurse:
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then      \
	cd $$m && $(MAKE) -s real-clean CMD_LINE_OPT="${CMD_LINE_OPT}" &&   \
	cd .. ; else echo "		(directory $$m skipped)" ; fi ) ; then \
	exit 1; fi ; done



info: info-files
	@echo "FQDN                   = $(FQDN)"
	@echo "BEAM_DIRS              = $(BEAM_DIRS)"
	@echo "BEAM_PATH_OPT          = $(BEAM_PATH_OPT)"
	@echo "ARCHIVE_FILE           = $(ARCHIVE_FILE)"
	@echo "ERLANG_INTERPRETER     = $(ERLANG_INTERPRETER)"
	@echo "ERLANG_INTERPRETER_OPT = $(ERLANG_INTERPRETER_OPT)"
	@echo "ERLANG_SRC             = $(ERLANG_SRC)"
	@echo "VM_TEST_NAME           = $(VM_TEST_NAME)"
	@echo "PROJECT_NAME           = $(PROJECT_NAME)"
	@echo "PROJECT_VERSION        = $(PROJECT_VERSION)"
	@echo "PACKAGE_NAME           = $(PACKAGE_NAME)"
	@echo "PLT_FILE               = $(PLT_FILE)"
	@echo "PREDECESSOR_PLT        = $(PREDECESSOR_PLT)"


info-files:
	@echo "ERL_FILES       = $(ERL_FILES)"
	@echo "BEAM_FILES      = $(BEAM_FILES)"
	@echo "TEST_SOURCES    = $(TEST_SOURCES)"
	@echo "TEST_TARGETS    = $(TEST_TARGETS)"
	@echo "ERLANG_PLT_FILE = $(ERLANG_PLT_FILE)"
	@echo "PLT_FILE        = $(PLT_FILE)"



include $(COMMON_TOP)/doc/GNUmakerules-docutils.inc
