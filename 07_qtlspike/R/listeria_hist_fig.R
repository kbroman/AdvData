library(qtl)
data(listeria)

pdf("../Figs/listeria_hist.pdf", height=5, width=9.75, pointsize=18)
par(mar=c(4.1, 0.6, 0.6, 0.6))
hist(listeria$pheno[,1], breaks=seq(0, 264, by=4),
     xlab="Survival time (hours)", ylab="", yaxt="n", main="")
u <- par("usr")
text(20, sum(u[3:4]*c(0.25, 0.75)), adj=c(0, 0.5),
     "~1/3 of 116 mice survived to 264 hrs")
dev.off()
