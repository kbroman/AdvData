library(qtl)

png("../Figs/sel_geno.png", height=800, width=1600, pointsize=24)
data(hyper)
geno.image(hyper, reorder=1, ylab="Mice", main="")
dev.off()
