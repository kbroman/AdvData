STEM = 05_tcells
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

../docs/$(STEM).pdf: $(STEM).pdf
	cp $< $@

$(STEM).pdf: $(STEM).tex ../LaTeX/header.tex \
			 Figs/mean_response.pdf \
			 Figs/model_distribution.pdf \
			 Figs/em_loglik_wrong.pdf \
			 Figs/em_loglik_right.pdf \
			 Figs/multiple_modes.pdf
	xelatex $<

Figs/mean_response.pdf: R/plot_mean_response.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/model_distribution.pdf: R/plot_model.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/em_loglik_wrong.pdf: R/npem_wrong.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/em_loglik_right.pdf: R/npem_right.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/multiple_modes.pdf: R/starting_pts.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
