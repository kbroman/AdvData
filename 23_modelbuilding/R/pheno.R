library(qtl)
data(hyper)

pdf(file="../Figs/pheno.pdf", width=10, height=4.5, pointsize=12)

par(mar=c(5.1,1.1,1.1,1.1))

hist(hyper$pheno[,1], breaks=seq(82, 128.5, by=1.5),
     main="", yaxt="n", ylab="", xlab="Blood pressure")

dev.off()
