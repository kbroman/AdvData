# plot genehunter results

library(qtl2convert)
library(qtl2)
library(broman)

for(i in 1:2) {

    if(i==1) {
        input <- "../Analysis/R/gh_results.rds"
        output <- "../Figs/gh_results_good.pdf"
    } else {
        input <- "../Analysis/R/gh_results_rev.rds"
        output <- "../Figs/gh_results_bad.pdf"
    }

    gh <- readRDS(input)
    rownames(gh) <- paste0("marker", 1:nrow(gh))

    gh$marker <- rownames(gh)
    map <- map_df_to_list(gh, pos_column="position")

    gh_results <- as.matrix(gh[,3:5])
    gh_results[,2] <- -log10(gh_results[,2])
    class(gh_results) <- "scan1"
    pdf(output, width=9.75, height=5.5, pointsize=12)
    par(mar=c(3.1, 3.1, 0.6, 0.6))
    plot_scan1(gh_results, map, lodcolumn=2, ylab=expression(paste(-log[10]," P-value")),
               altcol=brocolors("web")["green"], ylim=c(0, 6.8),
               mgp.y=c(1.6, 0.3, 0), mgp.x=c(1.8, 0.2, 0))
    dev.off()
}
