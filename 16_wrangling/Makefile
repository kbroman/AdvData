LEC = 16_wrangling

FIGS=Figs/il3.pdf \
	 Figs/adipose_weight.pdf \
	 Figs/body_weight.pdf


include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
