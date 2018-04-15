MYRIAD_TOP = ..


DOCUTILS_TOP = $(MYRIAD_TOP)

DOC_BASE_NAME := Ceylan-Myriad-Layer-technical-manual-english

DOC_PDF_NAME := $(DOC_BASE_NAME).pdf

OVERALL_DOCUMENT_TARGET = $(DOC_BASE_NAME).rst

DOC_GENERATED_FILES = $(DOC_PDF_NAME) $(DOC_BASE_NAME).html

DOC_FILES = $(DOC_GENERATED_FILES)


# The current, operational version of Myriad:
CURRENT_VERSION := 1.0

CURRENT_MYRIAD_DOC := $(DOC_PDF_NAME)

PROJECT_CSS := pygments-default.css,myriad.css


WEB_CONTENT = *.html *.css *.png $(DOC_PDF_NAME)


.PHONY: all test uml-diagram-test


OVERALL_DOCUMENT_SOURCE = $(DOC_BASE_NAME).rst


# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' to generate the manual of the 'Myriad' layer."


doc:


# Already generates by default the PDF:
full-doc: $(DOC_BASE_NAME).html

# Create a separate repository for the GitHub pages branch:
clone-mirror:
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/Ceylan-Myriad.git $(shell basename $(MYRIAD_WEB_MIRROR_BRANCH)) && git checkout gh-pages


export-doc: clean-doc full-doc export-to-official export-to-mirror


export-to-official:
	@echo "   Exporting Myriad documentation to official website"
	@/bin/scp $(SP) $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/Myriad/


export-to-mirror:
	@echo "   Exporting Myriad documentation to mirror website in $(MYRIAD_WEB_MIRROR_BRANCH)"
	@/bin/cp -f $(WEB_CONTENT) $(MYRIAD_WEB_MIRROR_BRANCH) && cd $(MYRIAD_WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "Myriad doc updated." && git push


info-web:
	@echo "WEB_CONTENT = $(WEB_CONTENT)"
	@echo "MYRIAD_WEB_MIRROR_BRANCH = $(MYRIAD_WEB_MIRROR_BRANCH)"



# As a side-effect, early checking that the 'dot' tool will be available:
test: uml-diagram-test

uml-diagram-test:
	@$(MAKE) uml_class_diagram_example.png


DOCUTILS_TOP = .

include $(MYRIAD_TOP)/GNUmakesettings.inc
