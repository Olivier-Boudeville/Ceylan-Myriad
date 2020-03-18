MYRIAD_TOP = ..


.PHONY: all doc full-doc html clone-mirror                        \
		export-doc export-to-official export-to-mirror            \
		clean clean-doc real-clean info-web test uml-diagram-test


DOC_BASE_NAME := Ceylan-Myriad-Layer-technical-manual-english

OVERALL_DOCUMENT_SOURCE = $(DOC_BASE_NAME).rst

DOC_PDF_NAME := $(DOC_BASE_NAME).pdf

HTML_TARGET = $(DOC_BASE_NAME).html

DOC_GENERATED_FILES = $(DOC_PDF_NAME) $(HTML_TARGET)

DOC_FILES = $(DOC_GENERATED_FILES)


# The current, operational version of Myriad:
CURRENT_VERSION := 1.0

CURRENT_MYRIAD_DOC := $(DOC_BASE_NAME)-$(CURRENT_VERSION).pdf

PROJECT_CSS := pygments-default.css,myriad.css

PROJECT_ICON := myriad-icon.png

WEB_CONTENT = *.html *.css *.png *.pdf

# Read from the user's environment:
GITHUB_PAGES_BASE := $(shell basename "$(MYRIAD_WEB_MIRROR_BRANCH)")



# Default do-nothing target:
doc:

all:
	@echo "  Run 'make full-doc' to generate the manual of the 'Myriad' layer."


full-doc: $(DOC_BASE_NAME).pdf $(HTML_TARGET)
	@/bin/ln -s $(HTML_TARGET) myriad.html
	@/bin/ln -s myriad.html index.html
	@/bin/ln -s $(DOC_PDF_NAME) myriad.pdf


html: clean-doc $(HTML_TARGET)


# Creates a separate repository for the GitHub pages branch:
# (please then remove all initial content of that branch)
#
clone-mirror:
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/Ceylan-Myriad.git $(GITHUB_PAGES_BASE) && cd $(GITHUB_PAGES_BASE) && git checkout -b gh-pages


export-doc: clean-doc full-doc export-to-official export-to-mirror


# Drawback: scp will not preserve symlinks, it will duplicate files instead.

# rsync would not, but requires a different source layout (copies a full tree,
# not a set of files), so current not used:
# rsync -avze "ssh $(SP)" $(SOME_WEB_TREE) $(WEB_SRV):$(WEB_ROOT)/Myriad/
#
export-to-official: $(DOC_GENERATED_FILES)
	@echo "   Exporting Myriad documentation to official website ($(WEB_SRV))"
	@/bin/scp $(SP) $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/Myriad/


export-to-mirror: $(DOC_GENERATED_FILES)
	@echo "   Exporting Myriad documentation to mirror website in $(MYRIAD_WEB_MIRROR_BRANCH)"
	@/bin/cp -f $(WEB_CONTENT) $(MYRIAD_WEB_MIRROR_BRANCH) && cd $(MYRIAD_WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "Myriad doc updated." && git push && git pull --ff-only



clean: clean-doc


clean-doc:
	@echo "     Cleaning documentation"
	-@/bin/rm -f *.aux *.log *.maf *.mtc* *.stc* *.tex *.toc \
	$(CURRENT_MYRIAD_DOC) $(DOC_GENERATED_FILES)


# These three elements are symbolic links:
real-clean: clean
	-@/bin/rm -f index.html myriad.html myriad.pdf


info-web:
	@echo "HTML_TARGET = $(HTML_TARGET)"
	@echo "SP = $(SP)"
	@echo "WEB_CONTENT = $(WEB_CONTENT)"
	@echo "WEB_SRV = $(WEB_SRV)"
	@echo "WEB_ROOT = $(WEB_ROOT)"
	@echo "MYRIAD_WEB_MIRROR_BRANCH = $(MYRIAD_WEB_MIRROR_BRANCH)"
	@echo "GITHUB_PAGES_BASE = $(GITHUB_PAGES_BASE)"


# As a side-effect, early checking that the 'dot' tool will be available:
test: uml-diagram-test


uml-diagram-test:
	@$(MAKE) uml_class_diagram_example.png


DOCUTILS_TOP = .

include $(MYRIAD_TOP)/GNUmakesettings.inc
