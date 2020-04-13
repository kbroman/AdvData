LEC = 24_knitrpapers

#FIGS =

include ../Makefile_lectures.mk

Figs/%.pdf: R/%.R
	cd R;R CMD BATCH $(R_OPTS) $(<F)
