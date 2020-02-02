STEM = 05_tcells
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

../docs/$(STEM).pdf: $(STEM).pdf
	cp $< $@

$(STEM).pdf: $(STEM).tex ../LaTeX/header.tex
	xelatex $<
