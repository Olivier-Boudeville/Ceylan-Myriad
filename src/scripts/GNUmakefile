MYRIAD_TOP = ../..

.PHONY: make-test-tree test-local test-local-scan run-test-tree clean-test-tree


TEST_TREE := "myriad-example-tree"

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
	@tree $(TEST_TREE)


test: test-local


test-local: test-local-scan


test-local-scan: make-test-tree run-test-tree


run-test-tree:
	@$(MAKE) -s all && ./merge-tree.escript --scan $(TEST_TREE) CMD_LINE_OPT="--use-ui-backend text_ui"



clean: clean-test-tree


clean-test-tree:
	@echo " Cleaning test tree $(TEST_TREE)"
	-@/bin/rm -rf $(TEST_TREE)


include $(MYRIAD_TOP)/GNUmakesettings.inc
