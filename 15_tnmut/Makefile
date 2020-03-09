LEC = 15_mtb

FIGS = Figs/mt598.pdf \
	   Figs/himar.pdf \
	   Figs/mut6.pdf

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/mut6.pdf: R/mut_figs.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
