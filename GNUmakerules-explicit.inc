# Here are gathered explicit (immediate, static) specific rules for Myriad.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: MYRIAD_TOP must be set.


.PHONY: .PHONY-myriad-explicit

.PHONY-myriad-explicit:  all all-pre-hook all-parallel-recurse all-recurse     \
			  all-parse-transforms bootstrap-modules make-all-parse-transforms \
			  build-debug rebuild test test-recurse doc-recurse build-tests    \
			  launch install-all install-announce install-ebin install-bin     \
			  install-src install-include install-examples install-test        \
			  install-doc info-install info-install-locations                  \
			  info-install-content                                             \
			  rebar3-application rebar3-release rebar3-development-release     \
			  rebar3-production-release rebar3-update                          \
			  rebar3-compile rebar3-create-tree rebar3-populate-build-tree     \
			  rebar3-create-app-file rebar3-copy-beams                         \
			  rebar3-clean rebar3-real-clean rebar-test-clean                  \
			  rebar3-cache-clean rebar3-info rebar3-hex-publish                \
			  test-hex-package                                                 \
			  generate-list-of-erlang-types generate-list-of-local-types       \
			  generate-local-plt self-check-against-plt write-debug-key-file   \
			  check check-hook help-erl                                        \
			  clean clean-erlang clean-python clean-database clean-recurse     \
			  clean-outputs clean-ast-debug real-clean track                   \
			  info info-files info-meta info-erlang info-python info-java


#dummy:
#	@echo "You are executing the default, do-nothing make rule. You may want specify an actual target instead (ex: 'make foo')."



all:


# Defining the BASE_MAKEFILE variable in top makefiles allows to define a
# specific (parallel) 'all' rule for them, while other makefiles just recurse
# accordingly:
#
ifeq ($(BASE_MAKEFILE),true)

# CORE_COUNT defined in myriad/GNUmakevars.inc:
all: all-pre-hook all-parse-transforms
	@echo "   Building all, in parallel over $(CORE_COUNT) core(s), from $(PWD)"
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && \
		$(MAKE) -s all-recurse -j $(CORE_COUNT) && cd .. ; else echo "     (directory $$m skipped)" ; \
	fi ) ; then exit 1; fi ; done


clean:
	-@/bin/rm -f rebar.config
	-@/bin/rm -rf _build/


else

all: all-recurse-pre-hook all-recurse $(BEAM_FILES) $(JAVA_CLASS_FILES)

endif


all-parse-transforms: bootstrap-modules make-all-parse-transforms

bootstrap-modules: $(BOOTSTRAP_MODULES)


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
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s all -j $(CORE_COUNT) CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ; else echo "    (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done


# We used to define '$(BEAM_FILES)' as prerequisites to this target; however
# then the "Building all" message was output after its directory was processed,
# not before.
#
# To avoid triggering the dummy target should no BEAM file be found, inner make
# command is conditional.
#
all-recurse: $(BEAM_FILES)
	@echo "   Building all in "$(PWD) #$$(basename $(PWD))
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s all CMD_LINE_OPT="${CMD_LINE_OPT}" USE_HDF5=$(USE_HDF5) USE_REST=$(USE_REST) USE_SQLITE=$(USE_SQLITE) && cd .. ; else echo "  (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done



# To interpret more easily compilation errors during development:
build-debug:
	@$(MAKE) -s all CORE_COUNT=1 | less


rebuild: clean all




# Bootstrap section.


# Bootstrapped modules must be special-cased, as they should not be built with
# parse-transforms (by design not available yet) and the resulting BEAM must be
# produced in the right directory (not at the root).
#
# Exactly the same processing applies to each of them.
#
# See also: in GNUmakevars.inc, the BOOTSTRAP_MODULES variable.

# We cherry-pick modules in various source directories, and select all modules
# in 'src/meta', which are then built generically.

# A difficulty is that we have to generate each BEAM file in the same directory
# as their source (hence the 'cd $$(dirname $@)'), however the automatically-set
# MYRIAD_TOP variable points only to the root of Myriad. As a result, paths
# (typically include ones) will not be correct, so we added BOOTSTRAP_INCLUDES
# to insert the correct ones.


# Note:
#
# - these rules are rather convoluted, as apparently the -o command-line option
#	of erlc ignores any path prior to the target, and thus writes the resulting
#	BEAM into the current directory; we have thus to go in the target directory
#	first
#
# - maybe these (identical) rules can be factored with just different sources
#	and targets


# In 'utils':

$(MYRIAD_TOP)/src/utils/basic_utils.beam: $(MYRIAD_TOP)/src/utils/basic_utils.erl
	@echo "		Compiling bootstrap module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )


$(MYRIAD_TOP)/src/utils/text_utils.beam: $(MYRIAD_TOP)/src/utils/text_utils.erl
	@echo "		Compiling bootstrap module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )


$(MYRIAD_TOP)/src/utils/id_utils.beam: $(MYRIAD_TOP)/src/utils/id_utils.erl
	@echo "		Compiling bootstrap module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )



# In 'meta':

# Real parse-transform-specific rules:
# (actually this is an automatic rule, but that is not a problem here)
#
$(MYRIAD_TOP)/src/meta/%.beam: $(MYRIAD_TOP)/src/meta/%.erl
	@echo "		Compiling bootstrap meta module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )



# In 'data-management':

#$(MYRIAD_TOP)/src/data-management/list_table.beam: $(MYRIAD_TOP)/src/data-management/list_table.erl
#	@echo "		Compiling bootstrap module $<"
#	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )


$(MYRIAD_TOP)/src/data-management/map_hashtable.beam: $(MYRIAD_TOP)/src/data-management/map_hashtable.erl
	@echo "		Compiling bootstrap module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )




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
	@${ERL_PARAMETERIZED_LAUNCHER} $(NODE_NAMING) test_shell-$$USER $(EXEC_INTERNAL_OPTIONS) $(CMD_LINE_OPT)




# Installation section.
#

# The goal here is to perform an installation of this layer in a prefixed
# directory (mostly regardless of OTP conventions; see the rebar3-* targets for
# that).


# For a development-ready install:
install-package: all doc install-all


# For a production-ready install:
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



# Section for the support of a rebar3+relx+hex compliant build.


# The two main targets for application and release:


# Apparently, a pre-existing rebar.lock file might cause version dependencies
# not to be respected (ex: sticking to an older hex version of a dependency,
# whereas a newer one was specified), so we remove it when appropriate.


rebar3-application: rebar3-clean rebar3-update set-base-rebar-conf
	@echo "  Triggering the overall creation by rebar3 of an OTP application corresponding to $(PROJECT_NAME)"
	-@/bin/rm -f rebar.lock 2>/dev/null
	@$(REBAR_DEBUG) rebar3 compile
	@$(MAKE) -s remove-rebar-conf


rebar3-release: rebar3-development-release


# If making a rebar application or release, a symlink will suffice:
set-base-rebar-conf:
	@echo "  Setting rebar base configuration"
	@/bin/rm -f rebar.config 2>/dev/null
	@cd conf && $(MAKE) -s rebar.config && cd .. && ln -sf conf/rebar.config


# If publishing an hex package (conf/ not included in the package, hence no
# symlink shall point to it, hence an actual copy of rebar configuration file):
#
set-hex-rebar-conf:
	@echo "  Setting rebar hex configuration"
	@/bin/rm -f rebar.config 2>/dev/null
	@cd conf && $(MAKE) -s rebar-for-hex.config && /bin/cp -f rebar-for-hex.config ../rebar.config


# Either a symlink or a copy (in the case of hex):
remove-rebar-conf:
	@echo "  Removing rebar configuration"
	@/bin/rm -f rebar.config


# Default is development:
rebar3-development-release: rebar3-clean rebar3-update set-base-rebar-conf
	@echo "  Triggering the overall creation by rebar3 of a (relx-based) development release corresponding to $(PROJECT_NAME)"
	@$(REBAR_DEBUG) rebar3 release
	@$(MAKE) -s remove-rebar-conf


# Supposedly more functional/flexible than 'rebar3 release -d false' or 'rebar3
# release -n prod':
#
rebar3-production-release: rebar3-clean rebar3-update set-base-rebar-conf
	@echo "  Triggering the overall creation by rebar3 of a (relx-based) production release corresponding to $(PROJECT_NAME)"
	@$(REBAR_DEBUG) rebar3 as prod release
	@$(MAKE) -s remove-rebar-conf


# Needed to update local package information according to (server-side) hex,
# otherwise rebar will stick to obsolve package versions (ex: if having just
# published a new version of a dependency)
#
rebar3-update:
	@echo "  Updating rebar (hex package repository)"
	@$(REBAR_DEBUG) rebar3 update


# Targets called by hooks specified to rebar3 through the rebar.config file:


# This overall target is called (thanks to a pre-hook) when running: 'rebar3
# compile'
#
# (note: we copy sources before compiling and copying BEAM files so that rebar3
# will not try to recompile modules by itself)
#
rebar3-compile:
	@echo "  Preparing $(PROJECT_NAME) for a rebar3-compliant build (in $(REBAR_BUILD_DIR))"
	@$(MAKE) -s set-base-rebar-conf rebar3-clean rebar3-create-tree rebar3-populate-build-tree rebar3-create-app-file all rebar3-copy-beams REBAR_PROFILE=$(REBAR_PROFILE) REBAR_BUILD_DIR=$(REBAR_BUILD_DIR)


# See for more information:
# http://erlang.org/doc/design_principles/applications.html#directory-structure
#
# Note: we now create a symlink directly pointing to the real 'src' tree,
# instead of copying the source files as a whole in the build tree. It is almost
# mandatory, as the URL above tells us that this src directory in _build should
# never be deeper than one level (whereas we have more levels than that), and
# apparently if rebar3 is not able to locate, for an ebin/ X.beam, a X.erl, it
# will attempt to recompile it (and obviously fail).
#
# See https://www.rebar3.org/discuss/5d1c82951e0cbe00121f12db for more details.
#
rebar3-create-tree:
	@echo "  Creating rebar3 build tree in $(REBAR_BUILD_DIR)"
	@mkdir -p $(REBAR_BUILD_DIR)/lib/$(REBAR3_PROJECT_NAME)/{priv/examples,include,doc,test,ebin}
	@cd $(REBAR_BUILD_DIR)/lib/$(REBAR3_PROJECT_NAME)/ && ln -sf ../../../../src


# Currently, 'priv' is not specifically populated, and all header files
# (including internal ones) are copied to 'include':
#
# (note that the sources of conditionally-compiled modules like sql_support are
# intentionally left out, otherwise the release as a whole will be considered as
# an unreachable_package: 'Dependency XXX is specified as a dependency but is
# not reachable by the system.'; they are also explicitly excluded in the
# exclude_files section of the .app.src file)
#
# The symptom can be better seen with REBAR_DEBUG=1 above; for example:
#
# ===> Resolving OTP Applications from directories:
# [...]
#
# ===> Missing beam file sql_support
#  [...]/_build/default/lib/myriad/ebin/sql_support.beam
#
# Solution: not listing such conditional BEAM files among the modules in the
# .app.src file.
#
rebar3-populate-build-tree:
	@echo "  Copying sources in rebar3 build tree"
	@true "Using a src symlink, hence disabled: find src -name '*.erl' -a ! -name '*_test.erl' -a ! -name sql_support.erl -a ! -name hdf5_support.erl -exec /bin/cp '{}' $(REBAR_BUILD_DIR)/lib/$(REBAR3_PROJECT_NAME)/src/ ';'"
	@# As tests may be in a separate 'tests' directory for example, and we do not want to select the content of _build once copied, in order to be idempotent:
	@find src tests -name '*_test.erl' -exec /bin/cp '{}' $(REBAR_BUILD_DIR)/lib/$(REBAR3_PROJECT_NAME)/test/ ';' 1>/dev/null 2>&1 | true
	@find src -name '*.hrl' -exec /bin/cp '{}' $(REBAR_BUILD_DIR)/lib/$(REBAR3_PROJECT_NAME)/include/ ';'
	@find examples -name '*rl' -exec /bin/cp '{}' $(REBAR_BUILD_DIR)/lib/$(REBAR3_PROJECT_NAME)/priv/examples ';' 1>/dev/null 2>&1 | true
	@/bin/cp LICENSE README.md $(REBAR_BUILD_DIR)/lib/$(REBAR3_PROJECT_NAME)/doc


# For modules, remove paths, extension, replace newlines with commas, and remove
# the last one:
#
# Not specifically removing source files, each .app.src does that thanks its
# 'exclude_files' entry, with MODULES_FOR_REBAR3:
#
# -a ! -name sql_support.erl -a ! -name hdf5_support.erl
#
rebar3-create-app-file:
	@echo "  Generating rebar3 $(REBAR3_PROJECT_NAME).app file"
	@# This target may be called for hex for example:
	@mkdir -p $(REBAR_BUILD_DIR)/lib/$(REBAR3_PROJECT_NAME)/ebin
	@all_modules=$$(find src -name '*.erl' -a ! -name '*_test.erl' $(REBAR3_FIND_SRC_OPT) -exec echo '{}' ';' | sed 's|.*/||1' | sed 's|.erl$$||1' | tr '\n' ','| sed 's|,|, |g' | sed 's|, $$||1' | sort ) ; cat conf/$(REBAR3_PROJECT_NAME).app.src | sed "s|VERSION_FOR_REBAR3|$(VERSION_FOR_REBAR3)|1" | sed "s|MODULES_FOR_REBAR3|$$all_modules|1" > $(REBAR_BUILD_DIR)/lib/$(REBAR3_PROJECT_NAME)/ebin/$(REBAR3_PROJECT_NAME).app


# Currently excluding test BEAMs, for a smaller size:
rebar3-copy-beams:
	@echo "  Copying non-test BEAM modules in rebar3 build tree"
	@find src -name '*.beam' -a ! -name '*_test.beam' -exec /bin/cp '{}' $(REBAR_BUILD_DIR)/lib/$(REBAR3_PROJECT_NAME)/ebin/ ';'


rebar3-clean:
	@echo "  Removing any prior rebar3 build tree for this project ($(REBAR_BUILD_BASE))"
	-@/bin/rm -rf $(REBAR_BUILD_BASE) 2>/dev/null


rebar3-real-clean: rebar-test-clean rebar3-cache-clean
	@echo "  Removing any prior rebar3 build root ($(REBAR_BUILD_ROOT)), hence for all profiles"
	-@/bin/rm -rf $(REBAR_BUILD_ROOT)


rebar-test-clean:
	@echo  "  Cleaning rebar3+hex test directory $(REBAR_TEST_DIR)"
	-@/bin/rm -rf $(REBAR_TEST_DIR)/_build
	-@/bin/rm -f $(REBAR_TEST_DIR)/rebar.lock


# Otherwise could reuse wrong cached versions (ex: 1.O.6.tar instead of
# 1.0.7.tar), or a former version of a recently-submitted package:
# (actually triggering the rebar3-update target should suffice)
#
rebar3-cache-clean:
	@echo "  Removing rebar3 cache for $(REBAR3_PROJECT_NAME)"
	-@/bin/rm -f $$HOME/.cache/rebar3/hex/hexpm/packages/$(REBAR3_PROJECT_NAME)-*


rebar3-info:
	@echo "REBAR_DEBUG = $(REBAR_DEBUG)"
	@echo "REBAR_PROFILE = $(REBAR_PROFILE)"
	@echo "REBAR_BUILD_ROOT = $(REBAR_BUILD_ROOT)"
	@echo "REBAR_BUILD_DIR = $(REBAR_BUILD_DIR)"
	@echo "REBAR3_FIND_SRC_EXCLUDES = $(REBAR3_FIND_SRC_EXCLUDES)"
	@echo "REBAR3_FIND_SRC_OPT = $(REBAR3_FIND_SRC_OPT)"
	@echo "REBAR_TEST_DIR = $(REBAR_TEST_DIR)"
	@echo "REBAR_3_EXEC = $(REBAR_3_EXEC)"
	@echo "VERSION_FOR_REBAR3 = $(VERSION_FOR_REBAR3)"


# Changes have to be applied for hex packages, which are very different from
# applications or releases.

# The simplest studying approach was to use the build of another hex package
# relying on Myriad to test how relevant is its package: 'make
# rebar3-application' done for WOOPER (which depends on Myriad, specified here
# as an hex dependency) shows notably that a
# ~/.cache/rebar3/hex/hexpm/packages/myriad-x.y.z.tar file is downloaded, which
# contains the VERSION, CHECKSUM, metadata.config and contents.tar.gz files.
#
# This last archive shows that the whole Myriad source tree is preserved, in its
# original, nested form, except for the root GNUmake* files (as a result, no
# rebar3 _build tree is used at all). So Myriad should copy them in priv/ and
# the compile hook is to copy them from priv/ to the root of the extracted
# package build tree.
#
# So, apparently:
#
# - a temporary priv/ is to be used
#
# - BEAMs shall not be included in the archive, as hex packages are source
#   containers, not binary ones
#
# - a correct, non-template, src/$(REBAR3_PROJECT_NAME).app.src is needed
#
# To publish a package, 'rebar3 hex user auth' is expected to have been executed
# at least once on the publishing computer.
#
rebar3-hex-publish: rebar3-clean clean set-hex-rebar-conf rebar3-create-app-file
	@/bin/cp -f $(REBAR_BUILD_BASE)/ebin/$(REBAR3_PROJECT_NAME).app src/$(REBAR3_PROJECT_NAME).app.src
	@mkdir -p priv/hex-packaging/{root,doc}
	@/bin/cp -f $(MYRIAD_TOP)/conf/hex-compile-hook-script.sh priv/hex-packaging/
	@/bin/cp -f GNUmake* priv/hex-packaging/root
	@/bin/cp -f doc/GNUmake* priv/hex-packaging/doc
	@$(REBAR_DEBUG) rebar3 hex publish
	@/bin/rm -rf priv/
	@/bin/rm -f src/$(REBAR3_PROJECT_NAME).app.src
	@$(MAKE) -s rebar3-clean
	@$(MAKE) -s remove-rebar-conf


test-hex-package: rebar-test-clean
	@echo "  Testing $(PROJECT_NAME) hex package ('$(REBAR3_PROJECT_NAME)' package, version $(VERSION_FOR_REBAR3)) in $(REBAR_TEST_DIR) using $(REBAR_3_EXEC)"
	@cd conf && $(MAKE) -s rebar-for-testing.config && /bin/cp -f rebar-for-testing.config $(REBAR_TEST_DIR)/rebar.config
	@cd $(REBAR_TEST_DIR) && $(REBAR_3_EXEC) update && $(REBAR_3_EXEC) compile


# Allows to generate a file listing all the types defined in the specified
# Erlang source tree.
#
# Ex: 'make generate-list-of-erlang-types ERLANG_SOURCE_ROOT=~/otp_src_R15B/'
#
generate-list-of-erlang-types:
	@if [ -z "$$ERLANG_SOURCE_ROOT" ] ; then echo "  Error, no ERLANG_SOURCE_ROOT variable specified." ; exit 15 ; fi ; if [ ! -d "$$ERLANG_SOURCE_ROOT" ] ; then echo "  Error, specified directory (ERLANG_SOURCE_ROOT=$$ERLANG_SOURCE_ROOT) does not exist." ; exit 16 ; fi ; target_file="declared-types-in-Erlang.txt" ; erl="$(ERLANG_INTERPRETER)" ; target_path=$$(echo $$erl|sed "s|/bin/erl$$|/$$target_file|1") ; echo "  Generating the list of types declared in the Erlang runtime..." &&  $(MYRIAD_TOP)/src/scripts/list-available-types.sh "$$ERLANG_SOURCE_ROOT" > $$target_path && echo "  Types have been collected in '$$target_path'."



generate-list-of-local-types:
	@echo "    Listing all types defined in package '$(PACKAGE_NAME)', in the $$(basename $(TYPE_LIST_FILE)) file"
	@$(MYRIAD_TOP)/src/scripts/list-available-types.sh > $(TYPE_LIST_FILE)


# Yes, this is an explicit, not automatic, rule:
$(PLT_FILE): generate-local-plt


# We do not want to issue an error if only warnings (code: 2) were emitted:
generate-local-plt: add-prerequisite-plts
	@echo "    Generating PLT for $(PACKAGE_NAME) ($(PLT_FILE)), based on $(BASE_PLT)"
	@$(DIALYZER) --add_to_plt --output_plt $(PLT_FILE) -r $(PACKAGE_TOP)/src --plt $(BASE_PLT); if [ $$? -eq 1 ] ; then exit 1 ; else exit 0 ; fi


# Made to be enriched (with child targets) on a per-layer basis:
add-prerequisite-plts:
	@echo "Prerequisite PLTs added."


# If Dialyzer complains that it cannot get Core Erlang code for a set of BEAM
# files (ex: with "Recompile with +debug_info or analyze starting from source
# code"), then the element lacking is most probably not the +debug_info switch
# when compiling these BEAMs, but the debug_info key when running Dialyzer
# itself.
#
# Such a key can be for example specified in ~/.erlang.crypt, as:
# [{debug_info, des3_cbc, [], "Ceylan-Myriad"}].
#
# See also the write-debug-key-file target.
#
self-check-against-plt: all $(PLT_FILE)
	@echo " Checking $(PACKAGE_NAME) against its PLT ($(PLT_FILE))"
	@$(DIALYZER) $(DIALYZER_OPT) --plt $(PLT_FILE) -r .


# Stores on disk the current debug key for BEAMs, so that tools like Dialyzer
# can determine it:
#
write-debug-key-file:
	@echo "[{debug_info, des3_cbc, [], \""$(DEBUG_INFO_KEY)"\"}]." > ~/.erlang.crypt



# Far easier target name to remember:
# (hook added to block this target if not relevant from the current location)
#
check: check-hook clean all test generate-local-plt self-check-against-plt


check-hook:


help-erl:
	@echo "To test hello.erl: 'erl', then 'c(hello).'," \
		"then 'hello:world().', then CTRL-C CTRL-C"


clean: clean-erlang clean-python clean-java clean-database clean-override \
	   clean-recurse


clean-erlang:
	@echo "   Cleaning all in "$(PWD) #$$(basename $(PWD))
	-@/bin/rm -f *.beam *.jam erl_crash.dump erlang.log.*


# We can have a Python binding:
clean-python:
	-@/bin/rm -rf '__pycache__'


# We can have a Java binding:
clean-java:
	-@/bin/rm -f *.class


clean-database:
	-@/bin/rm -rf Mnesia.*@*


# This target has been added in order to allow the packages making use of
# 'Myriad' to define their specific cleaning rules.
#
# They could have defined in their own GNUmakerules-explicit.inc something like:
#	clean: clean-mypackage
#	clean-mypackage:
#	  -@/bin/rm foo.mypackage
# but then this 'clean' target would become the default one, whereas we expect
# the 'all' default target of 'Myriad' to be triggered.
# Thus we finally allowed upper packages to specify their cleaning thanks to:
# 'FILES_TO_CLEAN += foo.mypackage' in their GNUmakevars.inc
clean-override:
	@#echo "FILES_TO_CLEAN = $(FILES_TO_CLEAN)"
	-@/bin/rm -f $(FILES_TO_CLEAN)


ifeq ($(ROOT_MAKEFILE),true)

# More talkative:
clean-recurse:
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then      \
	echo "  Cleaning in '$$(basename $$m)' layer" ; cd $$m &&            \
	$(MAKE) -s clean CMD_LINE_OPT="${CMD_LINE_OPT}" &&   \
	cd .. ; else echo "    (directory $$m skipped)" ; fi ) ; then \
	exit 1; fi ; done

else

clean-recurse:
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then      \
	cd $$m && $(MAKE) -s clean CMD_LINE_OPT="${CMD_LINE_OPT}" &&   \
	cd .. ; else echo "    (directory $$m skipped)" ; fi ) ; then \
	exit 1; fi ; done

endif


# Removes the debug files created about ASTs:
clean-ast-debug: clean-ast-debug-myriad

clean-ast-debug-myriad:
	@find . -name 'Myriad-*put-AST*.txt' -exec /bin/rm -f '{}' ';'


clean-outputs:


# To be enriched by user layers:
real-clean: clean clean-ast-debug real-clean-recurse rebar3-real-clean

real-clean-recurse:
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then      \
	cd $$m && $(MAKE) -s real-clean CMD_LINE_OPT="${CMD_LINE_OPT}" &&   \
	cd .. ; else echo "     (directory $$m skipped)" ; fi ) ; then \
	exit 1; fi ; done


# We prefer tracking the HTML version thereof (rather than the PDF one):
track:
	@$(DOC_TRACKER) $(OVERALL_DOCUMENT_SOURCE)


info: info-files
	@echo "FQDN                   = $(FQDN)"
	@echo "BEAM_DIRS              = $(BEAM_DIRS)"
	@echo "BEAM_PATH_OPT          = $(BEAM_PATH_OPT)"
	@echo "ARCHIVE_FILE           = $(ARCHIVE_FILE)"
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


info-meta:
	@echo "META_SRC_FILES = $(META_SRC_FILES)"
	@echo "META_BEAM_FILES = $(META_BEAM_FILES)"


info-erlang:
	@echo "ERLANG_INTERPRETER     = $(ERLANG_INTERPRETER)"
	@echo "ERLANG_INTERPRETER_OPT = $(ERLANG_INTERPRETER_OPT)"
	@echo "ERLANG_ROOT            = $(ERLANG_ROOT)"
	@echo "ERLANG_SRC             = $(ERLANG_SRC)"
	@echo "VM_TEST_NAME           = $(VM_TEST_NAME)"


# No Python source or binary files to be specifically managed by the build
# infrastructure itself.
#
info-python:
	@echo "USE_PYTHON_BINDING = $(USE_PYTHON_BINDING)"


info-java:
	@echo "USE_JAVA_BINDING = $(USE_JAVA_BINDING)"
	@echo "JAVA_FILES = $(JAVA_FILES)"
	@echo "JAVA_CLASS_FILES = $(JAVA_CLASS_FILES)"
	@echo "JINTERFACE_ROOT = $(JINTERFACE_ROOT)"
	@echo "JAVAC_OPT = $(JAVAC_OPT)"


# Put here, as defining explicit targets:
include $(MYRIAD_TOP)/doc/GNUmakerules-docutils.inc
