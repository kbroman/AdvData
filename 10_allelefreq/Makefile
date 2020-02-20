LEC = 10_allelefreq

include ../Makefile_lectures.mk

FIGS=		 Figs/gh_results_good.pdf \
			 Figs/sibpairs.pdf \
			 Figs/mixture_univariate.pdf \
			 Figs/sib_chr.pdf

Figs/gh_results_good.pdf: R/plot_gh_results.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/sibpairs.pdf: R/plot_sibships.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/mixture_univariate.pdf: R/plot_mixtures.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/sib_chr.pdf: R/plot_sib_chromosomes.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
