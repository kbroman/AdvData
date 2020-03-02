LEC = 13_datadiag

FIGS = Figs/missing_data_per_sample.pdf \
	   Figs/ave_snp_int_XY.pdf

include ../Makefile_lectures.mk

Figs/missing_data_per_sample.pdf: R/missing_data_per_sample.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/ave_snp_int_XY.pdf: R/snp_int_xy.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
