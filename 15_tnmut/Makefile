LEC = 15_mtb

FIGS = Figs/mt598.pdf \
	   Figs/himar.pdf

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
