STEM = 02_reprores
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

../docs/$(STEM).pdf: $(STEM).pdf
	cp $< $@

$(STEM).pdf: $(STEM).tex ../LaTeX/header.tex Figs/data_dict.pdf
	xelatex $<

Figs/data_dict.pdf: R/spreadsheets.R
	cd $(<D);R $(R_OPTS) -e "source('$(<F)')"
