LEC = 23_modelbuilding

FIGS = Figs/pheno.pdf \
	   Figs/geneticmap.pdf \
	   Figs/genodata.pdf \
	   Figs/alod.pdf \
	   Figs/meffects.pdf \
	   Figs/epistasis_bc.pdf \
	   Figs/alod_c4.pdf \
	   Figs/models.pdf \
	   Figs/hyper_models1.pdf \
	   Figs/lod_profile.pdf

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/meffects.pdf: R/effects_fig.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/epistasis_bc.pdf: R/epistasis_fig.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/hyper_models1.pdf: R/hyper_models.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
