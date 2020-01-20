R_OPTS=--no-save --no-restore --no-init-file --no-site-file

01_intro.pdf: 01_intro.tex ../LaTeX/header.tex Figs/autozygosity.pdf Figs/ceph_pedigrees.pdf
	xelatex $^

Figs/autozygosity.pdf: R/autozyg_fig.R R/chrompic_chr6_fam884.txt
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/ceph_pedigrees.pdf: R/ceph_pedigrees.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
