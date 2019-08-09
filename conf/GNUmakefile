MYRIAD_TOP = ..


.PHONY: info-rebar clean-local clean-rebar


# Typically to generate various flavours (normal, for Hex, for testing, etc.) of
# rebar.config files:
#
# (note: not centralised in a GNUmakerules-automatic file for all layers, as
# substitutions are layer-specific)
#
%: %.template
	@echo "  Generating $@ from $< (VERSION_FOR_REBAR3 being $(VERSION_FOR_REBAR3))"
	@cat $< | sed "s|\"VERSION_FOR_REBAR3\"|\"$(VERSION_FOR_REBAR3)\"|g" | sed "s|FIRST_FILES_FOR_REBAR|$(REBAR_FIRST_FILES)|g"  | sed "s|INCS_FOR_REBAR|$(REBAR_INCS)|g" > $@


info-rebar:
	@echo "REBAR_FIRST_FILES = $(REBAR_FIRST_FILES)"
	@echo "REBAR_INCS = $(REBAR_INCS)"


clean: clean-local


clean-local: clean-rebar
	-@/bin/rm -f $(REBAR_CONF_FILES)


clean-rebar:
	-@/bin/rm -f rebar.config
	-@/bin/rm -rf priv/



include $(MYRIAD_TOP)/GNUmakesettings.inc
