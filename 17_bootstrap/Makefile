LEC = 17_bootstrap

FIGS = Figs/hyper_lod.pdf

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/hyper_lod.pdf: R/hyper_fig.R R/my.plot.scanone.R R/bootlod.RData R/hyper_analysis.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
