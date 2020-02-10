STEM = 07_qtlspike
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

../docs/$(STEM).pdf: $(STEM).pdf
	cp $< $@

$(STEM).pdf: $(STEM).tex ../LaTeX/header.tex \
			 Figs/intercross.pdf \
			 Figs/lodcurve_insulin_with_effects.pdf \
			 Figs/pheno.pdf \
			 Figs/genodata.pdf \
			 Figs/geneticmap.pdf \
			 Figs/anova.pdf
	xelatex $<

Figs/lodcurve_insulin_with_effects.pdf: R/lodcurve_insulin.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/%.pdf: R/%_fig.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
