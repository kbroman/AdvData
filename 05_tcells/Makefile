STEM = 05_tcells
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

../docs/$(STEM).pdf: $(STEM).pdf
	cp $< $@

$(STEM).pdf: $(STEM).tex ../LaTeX/header.tex \
			 Figs/mean_response.pdf \
			 Figs/model_distribution.pdf
	xelatex $<

Figs/mean_response.pdf: R/plot_mean_response.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/model_distribution.pdf: R/plot_model.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
