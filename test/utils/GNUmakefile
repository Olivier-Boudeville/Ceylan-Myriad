MYRIAD_TOP = ../..

.PHONY: test-local prepare-otp-test test-distributions clean-distribution-files


# So that it remains the default target:
all:

test: test-local

test-local: prepare-otp-test
	@$(MAKE) -s most_basic_example_exec


prepare-otp-test:
	@cd $(MYRIAD_TOP) && $(MAKE) -s rebar3-create-app-file


test-distributions: clean-distribution-files
	-@killall eog; $(MAKE) -s random_utils_run


clean: clean-distribution-files

clean-distribution-files:
	-@/bin/rm -f test_*.dat compare_test_pdf_sampling_for_*.p test_*.png 2>/dev/null


include $(MYRIAD_TOP)/GNUmakesettings.inc
