LEC = 27_software

FIGS = Figs/scale_fig1.pdf \
	   Figs/batches_fig1.pdf \
	   Figs/rqtl_lines_code.pdf \
	   Figs/rqtl2_scan.pdf

include ../Makefile_lectures.mk

Figs/scale_fig1.pdf: R/scale_fig.R
	cd $(<D);R $(R_OPTS) -e "source('$(<F)')"

Figs/batches_fig1.pdf: R/batches_fig.R
	cd $(<D);R $(R_OPTS) -e "source('$(<F)')"

Figs/rqtl_lines_code.pdf: R/colors.R Data/lines_code_by_version.csv R/rqtl_lines_code.R
	cd R;R CMD BATCH rqtl_lines_code.R

Data/lines_code_by_version.csv: Perl/grab_lines_code.pl Data/versions.txt
	cd Perl;grab_lines_code.pl

Figs/rqtl2_scan.pdf: R/rqtl2_figs.R R/colors.R
	cd $(<D);R $(R_OPTS) -e "source('$(<F)')"
