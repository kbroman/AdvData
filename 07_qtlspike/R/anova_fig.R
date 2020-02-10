library(qtl)

cache_file <- "_cache/sug.rds"
if(file.exists(cache_file)) {
    sug <- readRDS(cache_file)
} else {
    sug <- read.cross("csv", "https://rqtl.org", "sug.csv", genotypes=c("CC", "CB", "BB"))
    saveRDS(sug, cache_file)
}

pdf(file="../Figs/anova.pdf", width=5, height=5.2, pointsize=12, onefile=TRUE)

par(mfrow=c(1,2))
par(mar=c(5.1, 4.1, 1.1, 0.1))

y <- sug$pheno[,1]
x <- pull.geno(fill.geno(subset(sug, chr=c(3,7))))[,c("D7MIT31", "D3MIT19")]
z <- runif(length(y), -0.2, 0.2)
par(las=1)
plot(x[,1]+z, y, ylab=expression(paste(log[2], " liver")),
     xlab="Genotype at D7Mit31", xaxt="n", pch=21, bg="lightblue")
axis(side=1, at=1:3, labels=c("BB","BC","CC"))
me <- tapply(y, x[,1], mean, na.rm=TRUE)
segments(1:3-0.25, me, 1:3+0.25, me, lwd=3, col="violetred")



par(mar=c(5.1,2.1,1.1,2.1))
plot((x[,2]+z), y, ylab="", yaxt="n",
     xlab="Genotype at D3Mit19", xaxt="n", pch=21, bg="lightblue")
axis(side=1, at=1:3, labels=c("BB","BC","CC"))

me <- tapply(y, x[,2], mean, na.rm=TRUE)
segments(1:3-0.25, me, 1:3+0.25, me, lwd=3, col="violetred")



dev.off()
