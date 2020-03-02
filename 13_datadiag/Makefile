LEC = 13_datadiag

FIGS = Figs/missing_data_per_sample.pdf \
	   Figs/ave_snp_int_XY.pdf \
	   Figs/percent_matching_geno.pdf \
	   Figs/gve_scheme_1.pdf \
	   Figs/gve_dist_matrix.png \
	   Figs/microbiome_scheme.pdf

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/gve_scheme_1.pdf: R/gve_mixup_scheme.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/gve_dist_matrix.png: R/gve_results.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
