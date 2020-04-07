LEC = 21_permtest

FIGS = Figs/experiment.pdf \
	   Figs/sel_geno.png \
	   Figs/geno_and_pheno.pdf

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/sel_geno.png: R/sel_geno.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/geno_and_pheno.pdf: R/qtl_figs.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
