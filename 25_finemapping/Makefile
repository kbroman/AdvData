LEC = 25_finemapping

FIGS = Figs/plot-eqtl.pdf \
	   Figs/chr6_lod.pdf \
	   Figs/islet_c6_pca.pdf \
	   Figs/islet_c6_geno_C.pdf \
	   Figs/ldapca_islet6.pdf \
	   Figs/effects_islet6.pdf \
	   Figs/formal_islet6.pdf \
	   Figs/intercross.pdf \
	   Figs/lodcurve_insulin.pdf \
	   Figs/nonrec_illustrationA.pdf \
	   Figs/effect_illustrationA.pdf

include ../Makefile_lectures.mk

Figs/plot-eqtl.pdf: R/plot-eqtl.R R/my_ploteQTL.R
	cd $(<D); R CMD BATCH $(R_opt) $(<F)

Figs/chr6_lod.pdf: R/chr6_lod.R
	cd $(<D); R CMD BATCH $(R_opt) $(<F)

Figs/islet_c6_pca.pdf: R/islet_c6_pca.R
	cd $(<D); R CMD BATCH $(R_opt) $(<F)

Figs/islet_c6_geno_C.pdf: R/islet_c6_geno.R R/myplotgeno.R
	cd $(<D); R CMD BATCH $(R_opt) $(<F)

Figs/ldapca_islet6.pdf: R/lda_pca.R R/my_plottrans.R
	cd R;R CMD BATCH $(<F)

Figs/effects_islet6.pdf: R/effects.R R/my_plotGenetpattern.R
	cd R;R CMD BATCH $(<F)

Figs/formal_islet6.pdf: R/formal.R R/my_plot_formal.R
	cd R;R CMD BATCH $(<F)

Figs/intercross.pdf: R/intercross.R
	cd R;R CMD BATCH $(<F)

Figs/lodcurve_insulin.pdf: R/lodcurve_insulin.R
	cd R;R CMD BATCH $(<F)

Figs/nonrec_illustrationA.pdf: R/nonrec_illustration.R
	cd R;R CMD BATCH $(<F)

Figs/effect_illustrationA.pdf: R/effect_illustration.R
	cd R;R CMD BATCH $(<F)
