MYRIAD_TOP = ..


.PHONY: info-rebar info-replacements clean-local clean-rebar


# Typically to generate various flavours (normal, for testing, etc.) of
# rebar.config files.
#
# Note that:
# - the Hex version, rebar-for-hex.config.template, is now useless
# - this rule is not centralised in a GNUmakerules-automatic file for all
# layers, as substitutions are layer-specific
# - 'rebar.config' is created in this 'conf' directory; any 'rebar.config' at
# the root of this package has not been affected
#
%: %.template $(MYRIAD_TOP)/GNUmakevars.inc
	@echo "  Generating $@ from $< (VERSION_FOR_REBAR3 being $(VERSION_FOR_REBAR3))"
	@cat $< | sed "s|\"VERSION_FOR_REBAR3\"|\"$(VERSION_FOR_REBAR3)\"|g" | sed "s|FIRST_FILES_FOR_REBAR|$(REBAR_FIRST_FILES)|g" | sed "s|DEBUG_KEY_FOR_REBAR|$(DEBUG_INFO_KEY)|g"| sed "s|INCS_FOR_REBAR|$(REBAR_INCS)|g" > $@


info-rebar:
	@echo "REBAR_FIRST_FILES = $(REBAR_FIRST_FILES)"
	@echo "REBAR_INCS = $(REBAR_INCS)"


info-replacements:
	@echo "VERSION_FOR_REBAR3 = $(VERSION_FOR_REBAR3)"
	@echo "REBAR_FIRST_FILES = $(REBAR_FIRST_FILES)"
	@echo "DEBUG_INFO_KEY = $(DEBUG_INFO_KEY)"
	@echo "REBAR_INCS = $(REBAR_INCS)"


clean: clean-local


clean-local: clean-rebar
	-@/bin/rm -f $(REBAR_CONF_FILES)


# Removed: -@/bin/rm -rf priv/
clean-rebar:
	-@/bin/rm -f rebar.config


include $(MYRIAD_TOP)/GNUmakesettings.inc
