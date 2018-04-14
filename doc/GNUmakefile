MYRIAD_TOP = ..

.PHONY: all test uml-diagram-test


OVERALL_DOCUMENT_SOURCE = Ceylan-Myriad-Layer-technical-manual-english.rst


# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' to generate the manual of the 'Myriad' layer."


# As a side-effect, early checking that the 'dot' tool will be available:
test: uml-diagram-test

uml-diagram-test:
	@$(MAKE) uml_class_diagram_example.png


DOCUTILS_TOP = .

include $(MYRIAD_TOP)/GNUmakesettings.inc
