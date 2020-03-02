# percent matching genotypes between pairs

library(qtl2)

file <- "_cache/compare_geno.rds"
if(file.exists(file)) {
    cg <- readRDS(file)
} else {
    cg <- compare_geno(do, cores=0)
    saveRDS(cg, file)
}
prop_match_fig <- function(label=FALSE)
{
    par(mar=c(6.1, 1.1, 1.1, 1.1), cex=1.7, cex.lab=1.3, cex.axis=1.3)
    cg_upper <- cg[upper.tri(cg)]*100
    hist(cg_upper, breaks=seq(0, 100, length=201),
         xlab="Percent matching genotypes", ylab="", yaxt="n", main="")

    rug(sort(cg_upper)[seq(1, length(cg_upper), 10)], col="violetred")
    rug(cg_upper[cg_upper < 25 | cg_upper > 60],  col="violetred")

    if(!label) return()

    textpos <- median(cg_upper[cg_upper < 35])
    text(textpos, 4000, "DO306 / DO308", col="darkslateblue")
    arrows(textpos, 3000, textpos, 1000, length=0.15, col="violetred", lwd=2)
}

pdf("../Figs/percent_matching_geno.pdf", height=5.5, width=9.75, pointsize=8)
prop_match_fig(FALSE)
dev.off()

pdf("../Figs/percent_matching_geno_labeled.pdf", height=5.5, width=9.75, pointsize=8)
prop_match_fig(TRUE)
dev.off()
