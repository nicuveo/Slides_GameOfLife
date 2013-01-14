##
## Makefile for haskell_pres
## Made by nicuveo <crucuny@gmail.com>
##


## GENERAL INFOS

IMG_DIR   = ./img/
BUILD_DIR = ./build/

PDF       = gol.pdf
PDF_DEPS  = Main.lhs Matrix.lhs Game.lhs Parser.lhs theme.tex $(wildcard img/*)
PDF_CMD   = pdflatex
PDF_FLAGS = -interaction nonstopmode -output-directory $(BUILD_DIR)

EXE       = gol
EXE_DEPS  = Main.lhs Matrix.lhs Game.lhs Parser.lhs
EXE_CMD   = ghc
EXE_FLAGS = --make -odir $(BUILD_DIR) -hidir $(BUILD_DIR) -tmpdir $(BUILD_DIR)

TARBALL   = gol

CLEAN = '*~' '\#*' '.\#*' '.DS_Store' '*.log' '*.aux' '*.toc' '*.nav' '*.out' '*.snm' '*.vrb' '*.o' '*.hi'




## OUTPUT

all: $(PDF) $(EXE);

$(PDF): $(PDF_DEPS) Main.pdf
	mv $(BUILD_DIR)/Main.pdf $@

$(EXE): $(EXE_DEPS)
	$(EXE_CMD) $(EXE_FLAGS) $< -o $@



# CLEANING

clean:
	for f in $(CLEAN) ; do find . -name "$$f" | xargs rm -f ; done

distclean: clean
	rm -f $(TARBALL) $(PDF) $(EXE)

dist tar tarball: distclean
	tar --transform 's,^,$(TARBALL)/,' -cvjf $(TARBALL).tar.bz2 --exclude=".svn" *



## SPECIFIC

%.pdf: %.tex
	$(PDF_CMD) $(PDF_FLAGS) $*.tex && $(PDF_CMD) $(PDF_FLAGS) $*.tex

%.pdf: %.lhs
	$(PDF_CMD) $(PDF_FLAGS) $*.lhs && $(PDF_CMD) $(PDF_FLAGS) $*.lhs

%.pdf:
	@echo "Unable to find $*.tex or $*.lhs, aborting."
	@exit 2



## SPECIALS

.PRECIOUS: %.pdf ;
.INTERMEDIATE: Main.pdf ;
