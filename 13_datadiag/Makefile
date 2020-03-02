LEC = 13_datadiag

FIGS = Figs/missing_data_per_sample.pdf \
	   Figs/ave_snp_int_XY.pdf \
	   Figs/percent_matching_geno.pdf

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
