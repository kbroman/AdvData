library(qtl)

png("../Figs/sel_geno.png", height=800, width=1600, pointsize=32)
data(hyper)
par(mar=c(4.1,4.1,2.1,1.1))
geno.image(hyper, reorder=1, ylab="Mice", main="")
dev.off()
