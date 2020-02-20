LEC = 07_qtlspike

include ../Makefile_lectures.mk

FIGS=		 Figs/intercross.pdf \
			 Figs/lodcurve_insulin_with_effects.pdf \
			 Figs/pheno.pdf \
			 Figs/genodata.pdf \
			 Figs/geneticmap.pdf \
			 Figs/anova.pdf \
			 Figs/backcross.pdf \
			 Figs/genoprob1.pdf \
			 Figs/mixtures.pdf \
			 Figs/listeria_hist.pdf \
			 Figs/listeria_lod.pdf \
			 Figs/listeria_effects.pdf \
			 Figs/listeria_stdlod.pdf

Figs/lodcurve_insulin_with_effects.pdf: R/lodcurve_insulin.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/genoprob1.pdf: R/genoprob_fig.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/%.pdf: R/%_fig.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
