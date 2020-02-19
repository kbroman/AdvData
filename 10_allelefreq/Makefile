STEM = 10_allelefreq
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

../docs/$(STEM).pdf: $(STEM).pdf
	cp $< $@

$(STEM).pdf: $(STEM).tex ../LaTeX/header.tex Figs/gh_results.pdf
	xelatex $<

Figs/gh_results.pdf: R/gh_results.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
