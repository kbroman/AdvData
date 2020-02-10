library(qtl)

cache_file <- "_cache/sug.rds"
if(file.exists(cache_file)) {
    sug <- readRDS(cache_file)
} else {
    sug <- read.cross("csv", "https://rqtl.org", "sug.csv", genotypes=c("CC", "CB", "BB"))
    saveRDS(sug, cache_file)
}

library(broman)
color <- brocolors("crayons")[c("Cornflower", "Inchworm", "Blush")]

bgcolor <- "white"

pdf(file="../Figs/genodata.pdf", width=9.75, height=5.5, pointsize=12, onefile=TRUE)
par(mar=c(5.1, 5.1, 2.1, 1.1), las=1)
g <- pull.geno(sug)
g[is.na(g)] <- 0
n <- ncol(g)+(-1:0)
me <- apply(sug$pheno[,c("heart_wt", "bw")], 1, mean)
image(1:totmar(sug), 1:nind(sug), t(g)[,order(me)], col=c(bgcolor,color),
      xlab="Markers", ylab="Individuals")
nm <- nmar(sug)
abline(v=cumsum(nm)+0.5, col="white")
u <- par("usr")
abline(h=u[3:4], v=u[1:2], col="white")
pos <- (cumsum(nm) + cumsum(c(0,nm[-length(nm)])))/2+0.5
axis(side=3, at=pos, labels=rep("", length(pos)))
text(pos, u[4]+diff(u[3:4])*0.04, names(sug$geno), xpd=TRUE)
dev.off()
