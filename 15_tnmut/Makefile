LEC = 15_tnmut

FIGS = Figs/mt598.pdf \
	   Figs/himar.pdf \
	   Figs/mut6.pdf \
	   Figs/numTAs.pdf \
	   Figs/mcmc13.pdf \
	   Figs/circlefig.pdf \
	   Figs/overlap.pdf \
	   Figs/operon.pdf \
	   Figs/unif_v_binom.pdf \
	   Figs/prob_essential.pdf \
	   Figs/sim_fig.pdf

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/mut6.pdf: R/mut_figs.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/mcmc13.pdf: R/mcmc_example.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/prob_essential.pdf: R/mtb_figs.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
