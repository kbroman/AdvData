LEC = 18_rpack

#FIGS =

all: ../docs/$(LEC).pdf ../docs/$(LEC)_notes.pdf ../docs/18_rpack_demo.md

include ../Makefile_lectures.mk

../docs/18_rpack_demo.md: 18_rpack_demo.md
	cp $< $@

#Figs/nested_bootstrap_1.pdf: R/nested_bootstrap.R
#	cd R;R CMD BATCH $(R_OPTS) $(<F)
