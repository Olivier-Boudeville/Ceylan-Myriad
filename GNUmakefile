COMMON_TOP = .

MODULES_DIRS = src doc

all:
	@echo "   Building all, in parallel over $(CORE_COUNT) core(s), from "$(PWD) #`basename $(PWD)`
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m &&  \
		$(MAKE) -s all-recurse -j $(CORE_COUNT) && cd .. ; else echo "     (directory $$m skipped)" ; \
	fi ) ; then exit 1; fi ; done


include $(COMMON_TOP)/GNUmakesettings.inc
