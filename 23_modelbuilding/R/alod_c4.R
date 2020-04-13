library(qtl)
blue <- "slateblue"
pink <- "violetred"
gray <- "gray87"

file <- "Rcache/alod_c4.RData"
if(file.exists(file)) {
  load(file)
} else {
  data(hyper)
  g <- fill.geno(hyper)$geno[[4]]$data[,"D4Mit164"]
  hyper <- calc.genoprob(hyper, step=1, err=0.01)

  out.c4 <- scanone(hyper, phe=1, addcovar=g)
  save(out.c4, g, file=file, compress=TRUE)
}

load("Rcache/alod.RData")

pdf(file="../Figs/alod_c4.pdf", width=10, height=5.5, pointsize=14)
par(mar=c(5.1, 5.1, 1.1, 2.6))

plot(out, out.c4, col=c(blue,pink), ylab="LOD score", chr=1:19, bandcol=gray)
legend("topright", lwd=2, col=c(blue, pink), c("Interval mapping","Control for chr 4"))

dev.off()
