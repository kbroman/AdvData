# Common portion of the Makefile for each lecture

WEBDIR=../docs
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

web: $(WEBDIR)/$(LEC).pdf $(WEBDIR)/$(LEC)_notes.pdf

$(WEBDIR)/%: %
	cp $< $@

$(LEC).pdf: $(LEC).tex ../LaTeX/header.tex $(FIGS)
	xelatex $^

$(LEC)_notes.pdf: $(LEC)_notes.tex ../LaTeX/header.tex $(FIGS)
	xelatex $<
	pdfnup $@ --nup 1x2 --paper letterpaper --frame true --scale 0.9
	mv $(LEC)_notes-1x2.pdf $@

$(LEC)_notes.tex: $(LEC).tex ../Ruby/createVersionWithNotes.rb
	../Ruby/createVersionWithNotes.rb $< $@

clean:
	rm *.aux *.log *.nav *.out *.snm *.toc *.vrb
