MYRIAD_TOP = ../../..

.PHONY: make-test-tree test-local test-initial-scan test-extra-scan test-uniquify \
		make-test-tree run-test-tree process-test-log


# Previously run as an escript, but due to an issue with term_ui (i.e. dialog),
# now run as a shell script:
#
#MERGE_SCRIPT := ./merge-tree.escript
MERGE_SCRIPT := ./merge.sh


TEST_TREE := "myriad-example-tree"

TEST_LOG_FILE := "merge-tree.log"

UI_BACKEND := text_ui
#UI_BACKEND := term_ui



all:

make-test-tree: clean-test-tree
	@echo " Creating test tree..."
	@mkdir -p $(TEST_TREE)/foo/bar
	@mkdir $(TEST_TREE)/foo/baz $(TEST_TREE)/buz
	@echo "Content #1" > $(TEST_TREE)/foo/bar/a.txt
	@echo "Content #1" > $(TEST_TREE)/buz/b.txt
	@echo "Content #2" > $(TEST_TREE)/buz/a.txt
	@echo "Content #3" > $(TEST_TREE)/foo/bar/c.txt
	@echo "Content #1" > $(TEST_TREE)/foo/baz/a.txt
	@echo "Content #2" > $(TEST_TREE)/foo/baz/c.txt
	@tree $(TEST_TREE)


test-local: test-initial-scan test-extra-scan


# Meant to create a cache file:
test-initial-scan: make-test-tree run-test-tree process-test-log

# Meant to perform a light check of the pre-existing cache file:
test-extra-scan: run-test-tree process-test-log

test-uniquify:
	@$(MAKE) -s all && $(MERGE_SCRIPT) --uniquify $(TEST_TREE) --use-ui-backend $(UI_BACKEND)


run-test-tree:
	@$(MAKE) -s all && $(MERGE_SCRIPT) --scan $(TEST_TREE) --use-ui-backend $(UI_BACKEND)


debug:
	@/bin/rm -f "$(TEST_LOG_FILE)" ; $(MAKE) -s test process-test-log


process-test-log:
	@if [ -f "$(TEST_LOG_FILE)" ] ; then echo ; echo "#### Content of $(TEST_LOG_FILE) follows:" ; more "$(TEST_LOG_FILE)" ; /bin/rm -f "$(TEST_LOG_FILE)" ; fi


run-direct: merge_exec


help:
	@$(MERGE_SCRIPT) --help --use-ui-backend $(UI_BACKEND)


clean: clean-test-tree


clean-test-tree:
	@echo "   Cleaning test tree $(TEST_TREE)"
	-@/bin/rm -rf $(TEST_TREE)


include $(MYRIAD_TOP)/GNUmakesettings.inc
