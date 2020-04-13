LEC = 23_modelbuilding

FIGS = Figs/pheno.pdf \
	   Figs/geneticmap.pdf \
	   Figs/genodata.pdf \
	   Figs/alod.pdf \
	   Figs/meffects.pdf

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/meffects.pdf: R/effects_fig.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
