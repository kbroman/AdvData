LEC = 17_bootstrap

FIGS = Figs/hyper_lod.pdf \
	   Figs/coveraget.pdf \
	   Figs/mle1.pdf \
	   Figs/bootstrap_illu_1.pdf \
	   Figs/nested_bootstrap_1.pdf

all: web ../docs/$(LEC)_example.html

include ../Makefile_lectures.mk

Figs/hyper_lod.pdf: R/hyper_fig.R R/my.plot.scanone.R R/bootlod.RData R/hyper_analysis.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/coveraget.pdf: R/coverage-plots.R R/boot-summary.RData
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/mle1.pdf: R/hist-plot.R R/boot-summary.RData
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/bootstrap_illu_1.pdf: R/bootstrap_illu.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/nested_bootstrap_1.pdf: R/nested_bootstrap.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

$(LEC)_example.html: $(LEC)_example.Rmd
	R -e 'rmarkdown::render("$<")'

../docs/$(LEC)_example.html: $(LEC)_example.html
	cp $< $@
