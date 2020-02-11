library(qtl)
data(listeria)

# transform to log2 scale
listeria$pheno[,1] <- log2(listeria$pheno[,1])

# genome scan
listeria <- calc.genoprob(listeria, step=1, err=0.002)
out <- scanone(listeria, model="2part", upper=TRUE)


pdf("../Figs/listeria_lod.pdf", height=5.5, width=9.75, pointsize=18)
par(mar=c(4.1, 4.1, 0.6, 0.6))
plot(out, lod=1:3, col=c("black", "slateblue", "violetred"), ylab="LOD score")
legend("topright", lwd=2, col=c("black", "slateblue", "violetred"),
       c(expression(paste(pi, ", ", mu)),  expression(pi), expression(mu)))
dev.off()
