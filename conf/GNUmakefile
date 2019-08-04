MYRIAD_TOP = ..

.PHONY: test-hex-package clean-for-rebarinfo-rebar


REBAR_CONF_FILES := rebar.config rebar-for-hex.config rebar-for-testing.config

TEST_DIR := /tmp


# To install rebar3: https://github.com/erlang/rebar3#getting-started
#
# Typically obtained thanks to : mkdir -p ~/Software && cd ~/Software && git
# clone https://github.com/erlang/rebar3.git && cd rebar3 && ./bootstrap
#
REBAR_3_EXEC = $$(which rebar3 2>/dev/null)


%: %.template
	@echo "  Generating $@ from $< (VERSION_FOR_REBAR3 being $(VERSION_FOR_REBAR3))"
	@cat $< | sed "s|\"VERSION_FOR_REBAR3\"|\"$(VERSION_FOR_REBAR3)\"|g" > $@


test-hex-package: clean-for-rebar rebar-for-testing.config
	@echo "  Testing $(PROJECT_NAME) hex package ('$(REBAR3_PROJECT_NAME)' package, version $(VERSION_FOR_REBAR3)) in $(TEST_DIR) using $(REBAR_3_EXEC)"
	@/bin/cp -f rebar-for-testing.config $(TEST_DIR)/rebar.config
	@cd $(TEST_DIR) && $(REBAR_3_EXEC) update && $(REBAR_3_EXEC) compile


clean:
	-@/bin/rm -f $(REBAR_CONF_FILES)


clean-for-rebar: clean-rebar-cache
	-@$(MAKE) -s clean
	-@/bin/rm -rf $(TEST_DIR)/_build
	-@/bin/rm -f $(TEST_DIR)/rebar.lock


# Otherwise could take wrong cached versions (ex: 1.O.6.tar instead of
# 1.0.7.tar), or former version of a recently-submitted package:
#
clean-rebar-cache:
	-@/bin/rm -f $$HOME/.cache/rebar3/hex/hexpm/packages/$(REBAR3_PROJECT_NAME)-*


info-rebar:
	@echo "TEST_DIR = $(TEST_DIR)"
	@echo "REBAR_3_EXEC = $(REBAR_3_EXEC)"
	@echo "VERSION_FOR_REBAR3 = $(VERSION_FOR_REBAR3)"


include $(MYRIAD_TOP)/GNUmakesettings.inc
