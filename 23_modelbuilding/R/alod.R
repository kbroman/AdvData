library(qtl)

file <- "Rcache/alod.RData"
if(file.exists(file)) {
  load(file)
} else {
  data(hyper)
  hyper <- calc.genoprob(hyper, step=1, err=0.01)

  out <- scanone(hyper, phe=1)
  save(out, hyper, file=file, compress=TRUE)
}



for(i in 0:9) {
  file <- paste("Rcache/perm1_0", i, ".RData", sep="")
  if(file.exists(file)) {
    attach(file)
    if(i==0) op <- operm
    else op <- c(op, operm)
    detach(2)
  }
}


blue <- "slateblue"
pink <- "violetred"
gray <- "gray87"

pdf(file="../Figs/alod.pdf", width=10, height=5.5, pointsize=12)
par(mar=c(5.1, 5.1, 1.1, 2.6))

qu <- quantile(op, 0.95)
plot(out, col=blue, ylab="LOD score", chr=1:19, bandcol=gray)
abline(h=qu, col=pink, lty=2)
axis(side=4, at=qu, label="5%", las=1, col.axis=pink, col=pink, tick=FALSE, line=-0.5)
dev.off()
