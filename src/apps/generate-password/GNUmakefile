MYRIAD_TOP = ../../..

.PHONY: make-test-tree test-local test-local-scan run-test-tree \
		debug run-direct clean-test-tree

GEN_SCRIPT := ./generate-password.escript


all:



run-direct: password_generation.beam generate_password_exec


run-param: password_generation.beam
	@$(MAKE) generate_password_exec CMD_LINE_OPT="-h"


help:
	@$(GEN_SCRIPT) --help


clean:


include $(MYRIAD_TOP)/GNUmakesettings.inc
