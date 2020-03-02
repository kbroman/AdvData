LEC = 13_datadiag

FIGS = Figs/missing_data_per_sample.pdf \
	   Figs/ave_snp_int_XY.pdf \
	   Figs/percent_matching_geno.pdf \
	   Figs/gve_scheme_1.pdf \
	   Figs/gve_dist_matrix.png \
	   Figs/microbiome_scheme.pdf \
	   Figs/microbiome_best_vs_self.pdf \
	   Tabs/dna361_mb361_table.tex \
	   Figs/array_int.pdf

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/gve_scheme_1.pdf: R/gve_mixup_scheme.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/gve_dist_matrix.png: R/gve_results.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/microbiome_best_vs_self.pdf: R/microbiome_figs.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Tabs/dna361_mb361_table.tex: R/microbiome_tables.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/array_int.pdf: R/sample_qc.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
