LEC = 12_collab_eda

FIGS = Figs/attiedo_results.pdf

include ../Makefile_lectures.mk

Figs/attiedo_results.pdf: R/attiedo_results_fig.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
