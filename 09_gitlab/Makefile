all: ../docs/09_git_lab.html ../docs/09_git_lab.pdf

../docs/%: %
	cp $< $@

09_git_lab.html: git_lab.md
	R -e "rmarkdown::render('$(<F)', '$(@F)', output_format='html_document')"

09_git_lab.pdf: git_lab.md
	R -e "rmarkdown::render('$(<F)', '$(@F)', output_format='pdf_document')"
