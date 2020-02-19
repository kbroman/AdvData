STEM = 10_allelefreq
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

../docs/$(STEM).pdf: $(STEM).pdf
	cp $< $@

$(STEM).pdf: $(STEM).tex ../LaTeX/header.tex Figs/gh_results_good.pdf
	xelatex $<

Figs/gh_results_good.pdf: R/plot_gh_results.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
