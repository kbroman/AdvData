library(qtl)
source("my_plot_map.R")
data(hyper)

pdf(file="../Figs/geneticmap.pdf", width=10, height=5.5, pointsize=12)
par(mar=c(5.1, 5.1, 1.1, 1.1))
plot.map(hyper, main="")
dev.off()
