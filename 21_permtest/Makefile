LEC = 21_permtest

FIGS = Figs/experiment.pdf \
	   Figs/sel_geno.png

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/sel_geno.png: R/sel_geno.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
