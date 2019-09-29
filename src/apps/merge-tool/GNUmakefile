MYRIAD_TOP = ../../..


.PHONY: test-local test-initial-scan test-extra-scan test-uniquify           \
		make-test-trees run-test-tree clean-test-trees                       \
		post-test process-test-log reset-keyboard-mode


# Previously run as an escript, but due to an issue with term_ui (i.e. dialog),
# now run as a shell script:
#
#MERGE_SCRIPT := ./merge-tree.escript
MERGE_SCRIPT := ./merge.sh


TEST_CONTENT_TREE := "myriad-example-tree"

TEST_REFERENCE_TREE := "myriad-reference-tree"

TEST_TREES := $(TEST_CONTENT_TREE) $(TEST_REFERENCE_TREE)


TEST_LOG_FILE := "merge-tree.log"

UI_BACKEND := text_ui
#UI_BACKEND := term_ui



all:


test-local: test-initial-scan test-extra-scan


# Meant to create a cache file:
test-initial-scan: make-test-trees pre-test run-test-tree post-test

# Meant to perform a light check of the pre-existing cache file:
test-extra-scan: pre-test run-test-tree post-test


test-uniquify: pre-test make-test-trees
	@$(MERGE_SCRIPT) --uniquify $(TEST_CONTENT_TREE) --use-ui-backend $(UI_BACKEND) ; $(MAKE) -s post-test


test-merge: pre-test make-test-trees
	@$(MERGE_SCRIPT) --input $(TEST_CONTENT_TREE) --reference $(TEST_REFERENCE_TREE) ; $(MAKE) -s post-test


# Reference starts empty in this test:
make-test-trees: clean-test-trees
	@echo " Creating test tree..."
	@mkdir -p $(TEST_CONTENT_TREE)/foo/bar
	@mkdir $(TEST_CONTENT_TREE)/foo/baz $(TEST_CONTENT_TREE)/buz
	@echo "Content #1" > $(TEST_CONTENT_TREE)/foo/bar/a.txt
	@echo "Content #1" > $(TEST_CONTENT_TREE)/buz/b.txt
	@echo "Content #2" > $(TEST_CONTENT_TREE)/buz/a.txt
	@echo "Content #3" > $(TEST_CONTENT_TREE)/foo/bar/c.txt
	@echo "Content #1" > $(TEST_CONTENT_TREE)/foo/baz/a.txt
	@echo "Content #2" > $(TEST_CONTENT_TREE)/foo/baz/c.txt
	@tree $(TEST_CONTENT_TREE)
	@mkdir $(TEST_REFERENCE_TREE)
	@echo


pre-test:
	@$(MAKE) -s all
	@/bin/rm -f "$(TEST_LOG_FILE)" || true


run-test-tree:
	@echo "  Running a scan of $(TEST_CONTENT_TREE)"
	@$(MERGE_SCRIPT) --scan $(TEST_CONTENT_TREE) --use-ui-backend $(UI_BACKEND)


debug:
	@/bin/rm -f "$(TEST_LOG_FILE)" ; $(MAKE) -s test post-test



run-direct: merge_exec


post-test: process-test-log reset-keyboard-mode

process-test-log:
	@if [ -f "$(TEST_LOG_FILE)" ] ; then echo ; echo "#### Content of $(TEST_LOG_FILE) follows:" ; more "$(TEST_LOG_FILE)" ; fi


# Some configurations seem to screw up the terminal (weirdly, most keystrokes
# are then lost)
#
# (note: script found in Ceylan-Hull; still not sufficient here apparently)
#
reset-keyboard-mode:
	@reset-keyboard-mode.sh


help:
	@$(MERGE_SCRIPT) --help --use-ui-backend $(UI_BACKEND)


clean: clean-test-trees


clean-test-trees:
	@echo "   Cleaning test trees $(TEST_TREES)"
	-@/bin/rm -rf $(TEST_TREES)


include $(MYRIAD_TOP)/GNUmakesettings.inc
