LEC=01_intro

include ../Makefile_lectures.mk

FIGS=Figs/autozygosity.pdf \
	 Figs/ceph_pedigrees.pdf \
	 Figs/chr22rev.pdf

Figs/autozygosity.pdf: R/autozyg_fig.R R/chrompic_chr6_fam884.txt
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/ceph_pedigrees.pdf: R/ceph_pedigrees.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Figs/chr22rev.pdf: R/chr22_rev.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
