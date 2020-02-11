library(qtl)
data(listeria)

# transform to log2 scale
listeria$pheno[,1] <- log2(listeria$pheno[,1])

# genome scan
listeria <- calc.genoprob(listeria, step=1, err=0.002)
out <- scanone(listeria)
out_2p <- scanone(listeria, model="2part", upper=TRUE)

ymx <- max(c(out[,3], out_2p[,3]))
yli <- c(0, ymx)

pdf("../Figs/listeria_stdlod.pdf", height=5.5, width=9.75, pointsize=18)
par(mar=c(4.1, 4.1, 0.6, 0.6))
plot(out, ylab="LOD score", ylim=yli)
dev.off()

pdf("../Figs/listeria_stdlodB.pdf", height=5.5, width=9.75, pointsize=18)
par(mar=c(4.1, 4.1, 0.6, 0.6))
plot(out_2p, col="gray70", ylim=yli, ylab="LOD score")
plot(out, add=TRUE)
dev.off()
