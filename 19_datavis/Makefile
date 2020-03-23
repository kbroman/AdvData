LEC = 19_datavis

FIGS = Figs/fig1a.png Figs/fig3a.png Figs/fig5a.png Figs/fig2a_rev.png Crashes/crashes.pdf Crashes/crashes_scatter.pdf Crashes/Figs/chalabi-dearmona-distracted.png Crashes/Figs/chalabi-dearmona-speeding.png Crashes/Figs/chalabi-dearmona-drinking.png Crashes/crashes_errors.pdf Figs/piecharts.pdf Figs/ease_comparisons_1.pdf Figs/which_comparison.pdf

include ../Makefile_lectures.mk

Figs/fig1a.png: R/fig1.R
	cd R;R CMD BATCH fig1.R fig1.Rout

Figs/fig3a.png: R/fig3.R
	cd R;R CMD BATCH fig3.R fig3.Rout

Figs/fig5a.png: R/fig5.R
	cd R;R CMD BATCH fig5.R fig5.Rout

Figs/fig2a_rev.png: R/fig2.R
	cd R;R CMD BATCH fig2.R fig2.Rout

Crashes/crashes.pdf: Crashes/plot.R Crashes/proper_data.R
	cd $(<D);R CMD BATCH $(<F)

Crashes/crashes_scatter.pdf: Crashes/scatterplots.R Crashes/proper_data.R
	cd $(<D);R CMD BATCH $(<F)

Crashes/crashes_errors.pdf: Crashes/compare_measurements.R Crashes/data.R Crashes/proper_data.R
	cd $(<D);R CMD BATCH $(<F)

Crashes/Figs/%.png:
	cd $(@D);wget 'https://espnfivethirtyeight.files.wordpress.com/2014/10/$(@F)'
	touch $@

Figs/piecharts.pdf: R/piecharts.R
	cd $(<D);R CMD BATCH $(<F)

Figs/ease_comparisons_1.pdf: R/ease_comparisons.R
	cd $(<D);R CMD BATCH $(<F)

Figs/%.pdf: R/%.R
	cd $(<D);R CMD BATCH $(<F)
