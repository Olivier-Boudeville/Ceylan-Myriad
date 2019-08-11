MYRIAD_TOP = ../../..

.PHONY: all test-interactive



MODULES_DIRS = textual graphical


all:

test:


# In addition to parameter-less, batch test:
test-interactive:
	@for b in text_ui term_ui gui ; do echo ; echo "With backend '$$b' selected:" ; $(MAKE) -s ui_run CMD_LINE_OPT="-extra --use-ui-backend $$b" ; done
	@if ! $(MAKE) ui_run CMD_LINE_OPT="-extra --use-ui-backend non_existing_gui" 2>/dev/null ; then exit 0; else exit 5 ; fi


include $(MYRIAD_TOP)/GNUmakesettings.inc
