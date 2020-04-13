library(qtl)
source("colors.R")

data(hyper)

pdf(file="../Figs/genodata.pdf", width=10, height=5.5, pointsize=12)

par(mar=c(5.1, 5.1, 2.1, 1.1), las=1)
g <- pull.geno(hyper)
g[is.na(g)] <- 0
phe <- hyper$pheno[,1]
image(1:totmar(hyper), 1:nind(hyper), t(g)[,order(phe)], col=c("white", color[c(1,4,2)]),
      xlab="Markers", ylab="Individuals")
nm <- nmar(hyper)
abline(v=cumsum(nm)+0.5)
u <- par("usr")
abline(h=u[3:4], v=u[1:2])
pos <- (cumsum(nm) + cumsum(c(0,nm[-length(nm)])))/2+0.5
axis(side=3, at=pos, labels=rep("", length(pos)))
text(pos, u[4]+diff(u[3:4])*0.04, names(hyper$geno), xpd=TRUE)
dev.off()
