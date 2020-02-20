LEC=02_reprores

include ../Makefile_lectures.mk

FIGS=Figs/data_dict.pdf

Figs/data_dict.pdf: R/spreadsheets.R
	cd $(<D);R $(R_OPTS) -e "source('$(<F)')"
