LEC = 03_sims

FIGS=		 Figs/intercross.pdf \
			 Figs/lodcurve_insulin_with_effects.pdf \
			 Figs/congenic.pdf \
			 Figs/ail.pdf \
			 Figs/rilines.pdf \
			 Figs/riself.pdf \
			 Figs/ri8.pdf \
			 Figs/ri8self.pdf \
			 Figs/hs.pdf \
			 Figs/ri8genome1.pdf \
			 Figs/basic_genetics.pdf \
			 Figs/rf_by_sim.pdf \
			 Figs/ngen99.pdf \
			 Figs/threepoints.pdf \
			 Figs/coincidence_ni.pdf \
			 Figs/3pt_symmetry.pdf \
			 Figs/3pt_markov1.pdf

include ../Makefile_lectures.mk

Figs/lodcurve_insulin_with_effects.pdf: R/lodcurve_insulin.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/%.pdf: R/%_fig.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/ri8genome1.pdf: R/ri8_genome_fig.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/ngen99.pdf: R/genome_plots.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/coincidence_ni.pdf: R/coincidence_figs.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/3pt_markov1.pdf: R/3pt_markov.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/3pt_symmetry.pdf: R/3pt_symmetry.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
