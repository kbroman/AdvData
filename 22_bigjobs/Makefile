LEC = 22_bigjobs

#FIGS =

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
