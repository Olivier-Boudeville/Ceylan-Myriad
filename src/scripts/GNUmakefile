MYRIAD_TOP = ../..

.PHONY: make-test-tree test-local test-local-scan run-test-tree debug \
		clean-test-tree


TEST_TREE := "myriad-example-tree"

TEST_LOG_FILE := "merge-tree.log"


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


test: test-local


test-local: test-local-scan


test-local-scan: make-test-tree run-test-tree


run-test-tree:
	@$(MAKE) -s all && ./merge-tree.escript --scan $(TEST_TREE) --use-ui-backend text_ui


debug:
	@/bin/rm -f "$(TEST_LOG_FILE)" ; $(MAKE) -s test ; if [ -e "$(TEST_LOG_FILE)" ]; then echo ; echo "#### Content of $(TEST_LOG_FILE) follows:" ; more "$(TEST_LOG_FILE)" ; fi


clean: clean-test-tree


clean-test-tree:
	@echo " Cleaning test tree $(TEST_TREE)"
	-@/bin/rm -rf $(TEST_TREE)


include $(MYRIAD_TOP)/GNUmakesettings.inc
