COMMON_TOP = .


.PHONY: register-version-in-header register-common info-paths info-settings

#MODULES_DIRS = contrib src doc
MODULES_DIRS = src doc


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE := true


include $(COMMON_TOP)/GNUmakesettings.inc


# The base PLT is not the predecessor one (i.e. the Erlang PLT) as prerequisites
# may have to be taken into account, and have already started feeding PLT_FILE:
#
BASE_PLT := $(PLT_FILE)


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ] ; then \
	echo "Error, no version file defined." 1>&2 ; exit 50 ; else \
	$(MAKE) register-common ; fi


register-common:
	@echo "-define( common_version, \"$(COMMON_VERSION)\" )." >> $(VERSION_FILE)


add-prerequisite-plts: prepare-base-plt \
					   add-erlhdf5-plt add-jsx-plt add-sqlite3-plt


# So that in all cases we start by the same PLT name:
prepare-base-plt:
	@echo "Copying predecessor PLT $(PREDECESSOR_PLT) to $(PLT_FILE)"
	@/bin/cp -f $(PREDECESSOR_PLT) $(PLT_FILE)


# First prerequisite operates on predecessor (here, the Erlang PLT):
add-erlhdf5-plt:
	@if [ "$(USE_HDF5)" == "true" ] ; then echo "   Generating PLT for prerequisite erlhdf5" ; $(DIALYZER) --add_to_plt --output_plt $(PLT_FILE) -r $(ERLHDF5_BASE)/ebin --plt $(PLT_FILE); if [ $$? -eq 1 ] ; then exit 1 ; fi ; else echo "(no PLT determined for non-available erlhdf5 prerequisite; unknown functions in the erlhdf5 module will be found)" ; fi


# From the second, operating on the current PLT:
add-jsx-plt:
	@if [ "$(USE_REST)" == "true" ] ; then echo "   Generating PLT for prerequisite jsx" ; $(DIALYZER) --add_to_plt --output_plt $(PLT_FILE) -r $(JSX_BASE)/ebin --plt $(PLT_FILE); if [ $$? -eq 1 ] ; then exit 1 ; fi ; else echo "(no PLT determined for non-available jsx prerequisite; unknown functions in the jsx module will be found)" ; fi


add-sqlite3-plt:
	@if [ "$(USE_REST)" == "true" ] ; then echo "   Generating PLT for prerequisite sqlite3" ; $(DIALYZER) --add_to_plt --output_plt $(PLT_FILE) -r $(SQLITE3_BASE)/ebin --plt $(PLT_FILE); if [ $$? -eq 1 ] ; then exit 1 ; fi ; else echo "(no PLT determined for non-available sqlite3 prerequisite; unknown functions in the sqlite3 module will be found)" ; fi


info-paths:
	@echo "BEAM_PATH_OPT = $(BEAM_PATH_OPT)"


info-settings:
	@echo "ERLANG_COMPILER_OPT = $(ERLANG_COMPILER_OPT)"
	@echo "USE_HDF5   = $(USE_HDF5)"
	@echo "USE_REST   = $(USE_REST)"
	@echo "USE_SQLITE = $(USE_SQLITE)"
