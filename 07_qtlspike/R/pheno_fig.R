library(qtl)
library(broman)


cache_file <- "_cache/sug.rds"
if(file.exists(cache_file)) {
    sug <- readRDS(cache_file)
} else {
    sug <- read.cross("csv", "https://rqtl.org", "sug.csv", genotypes=c("CC", "CB", "BB"))
    saveRDS(sug, cache_file)
}

pdf(file="../Figs/pheno.pdf", width=9.75, height=4.8, pointsize=12, onefile=TRUE)
layout(cbind(c(1,2),c(3,3)))
par(mar=c(5.1, 1.1, 1.1, 1.1))
hist(sug$pheno$bw, breaks=34, main="", yaxt="n", ylab="", xlab="Body weight")
hist(sug$pheno$heart_wt, breaks=34, main="", yaxt="n", ylab="", xlab="Heart weight")

par(mar=c(5.1, 4.1, 1.1, 1.1), las=1)
plot(sug$pheno[,c("bw", "heart_wt")], xlab="Body weight", ylab="Heart weight",
     pch=21, bg="lightblue")
rug(sug$pheno$bw, side=1, col="slateblue", ticksize=0.02)
rug(sug$pheno$heart_wt, side=2, col="slateblue", ticksize=0.02)

dev.off()
