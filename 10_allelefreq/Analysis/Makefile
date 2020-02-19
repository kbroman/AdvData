R_OPTS=--no-save --no-restore --no-init-file --no-site-file

R/gh_results.rds: R/run_ghp_all.R Ghdata/aprpros22_rev.loc
	cd R;R CMD BATCH $(R_OPTS) $(<F)

Ghdata/aprpros22_rev.loc: R/edit_allele_freq.R Ghdata/aprpros22.loc
	cd R;R CMD BATCH $(R_OPTS) $(<F)
