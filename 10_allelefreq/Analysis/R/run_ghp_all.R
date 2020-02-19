# run genehunter-plus for all chromosomes

source("run_ghp.R")

library(parallel)

for(rev_freq in c(FALSE, TRUE)) { # whether to use original or revised allele frequencies
    result <- mclapply(1:22, function(chr) run_ghp(rev_freq, chr), mc.cores=detectCores())
    freq <- ifelse(rev_freq, "_rev", "")

    result <- do.call("rbind", result)

    output_file <- paste0("gh_results", freq, ".rds")
    saveRDS(result, output_file)
}
