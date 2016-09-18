COMMON_TOP = ../..


.PHONY: run scan merge

# Examples:
SOURCE_DIR := "../a"
TARGET_DIR := "../b"


all:

run: merge_utils.beam
	@$(ERL_PARAMETERIZED_LAUNCHER) --eval 'merge_utils:run()'


scan: merge_utils.beam
	@$(ERL_PARAMETERIZED_LAUNCHER) --eval "merge_utils:scan('$(SOURCE_DIR)')"

test-scan:
	-@/bin/rm -f $(SOURCE_DIR)/.merge-tree.cache
	@$(MAKE) scan


merge: merge_utils.beam
	@$(ERL_PARAMETERIZED_LAUNCHER) --eval "merge_utils:merge('$(SOURCE_DIR)',('$(TARGET_DIR)')"

test-merge:
	-@/bin/rm -f $(SOURCE_DIR)/.merge-tree.cache
	-@/bin/rm -f $(TARGET_DIR)/.merge-tree.cache
	@$(MAKE) merge



include $(COMMON_TOP)/GNUmakesettings.inc
