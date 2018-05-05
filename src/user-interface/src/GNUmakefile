MYRIAD_TOP = ../../..

.PHONY: all test-local



MODULES_DIRS = textual graphical


all:

test: test-local


# In addition to parameter-less test:
test-local:
	@$(MAKE) ui_run CMD_LINE_OPT="--use-ui-backend text_ui"
	@$(MAKE) ui_run CMD_LINE_OPT="--use-ui-backend term_ui"
	@$(MAKE) ui_run CMD_LINE_OPT="--use-ui-backend gui"
	@if ! $(MAKE) ui_run CMD_LINE_OPT="--use-ui-backend non_existing_gui" 2>/dev/null ; then exit 0; else exit 5 ; fi


include $(MYRIAD_TOP)/GNUmakesettings.inc
