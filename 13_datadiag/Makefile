LEC = 13_datadiag

FIGS = Figs/missing_data_per_sample.pdf

include ../Makefile_lectures.mk

Figs/missing_data_per_sample.pdf: R/missing_data_per_sample.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
