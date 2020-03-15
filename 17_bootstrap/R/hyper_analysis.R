library(qtl)

data(hyper)
hyper <- subset(hyper, chr=4)
d <- diff(hyper$geno[[1]]$map)+1e-6
hyper$geno[[1]]$map[-1] <- cumsum(d)
hyper <- calc.genoprob(hyper, step=1)

out <- scanone(hyper)

 load bootstrap results
b <- matrix(ncol=2, nrow=1250*8)
for(i in 0:7) {
  attach(paste0("boot0", i, ".RData"))
  b[1:1250+1250*i,] <- boot
  if(i==0) dimnames(b) <- dimnames(boot)
  detach(2)
}
boot <- b
rm(b)

map <- hyper$geno[[1]]$map
