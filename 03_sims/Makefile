STEM = 03_sims
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

../docs/$(STEM).pdf: $(STEM).pdf
	cp $< $@

$(STEM).pdf: $(STEM).tex ../LaTeX/header.tex \
			 Figs/intercross.pdf \
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
			 Figs/ngen99.pdf
	xelatex $<

Figs/lodcurve_insulin_with_effects.pdf: R/lodcurve_insulin.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/%.pdf: R/%_fig.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/ri8genome1.pdf: R/ri8_genome_fig.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/ngen99.pdf: R/genome_plots.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
