library(qtl)
library(lineup)
library(broman)

color <- brocolors("crayons")[c("Cornflower", "Tickle Me Pink", "Robin's Egg Blue")]


file <- "_cache/insulin_lod.RData"
if(file.exists(file)) {
  load(file)
} else {
  attach("~/Projects/Attie/GoldStandard/FinalData/aligned_geno_with_pmap.RData")
  attach("~/Projects/Attie/GoldStandard/FinalData/lipomics_final_rev2.RData")
  phe <- "INSULIN (ng/ml) 10 wk"

  ids <- findCommonID(f2g$pheno$MouseNum, lipomics$MouseNum)

  f2g <- f2g[,ids$first]
  f2g$pheno$insulin <- log10(lipomics[ids$second,phe])

  f2g <- calc.genoprob(f2g, step=1, err=0.002, map.function="c-f")

  sex <- as.numeric(f2g$pheno$Sex)-1
  out <- scanone(f2g, phe="insulin", method="hk", addcovar=sex)

  operm <- scanone(f2g, phe="insulin", method="hk", addcovar=sex, n.perm=1000, n.cluster=24)

  g <- pull.geno(fill.geno(f2g[2,], err=0.002, map.function="c-f"))[,rownames(max(out))]
  gnames <- getgenonames(class(f2g)[1], "A", cross.attr=attributes(f2g))
  g <- factor(gnames[g], levels=gnames)
  y <- f2g$pheno$insulin

  me <- tapply(y, g, mean, na.rm=TRUE)
  ci <- matrix(unlist(tapply(y, g, function(a) t.test(a)$conf.int)), nrow=2)
  dimnames(ci) <- list(c("lo", "hi"),gnames)

  save(out, g, y, me, ci, operm, file=file)
}


bw <- TRUE
incl_effects <- TRUE

fgcolor <- ifelse(bw, "black", "white")

file <- "../Figs/lodcurve_insulin"
if(incl_effects) file <- paste0(file, "_with_effects")
file <- paste0(file, ".pdf")


pdf(file, width=9.75, height=5, pointsize=14)
if(bw) {
    par(fg="black", col="black", col.axis="black", col.lab="black", bg="white")
} else {
    par(fg=fgcolor, col=fgcolor, col.axis=fgcolor, col.lab=fgcolor, bg=bgcolor)
}
par(mar=c(5.1,4.1,0.6,0.6))
if(bw) color[1] <- "slateblue"
plot(out, col=color[1], ylab="LOD score",
     bandcol=ifelse(bw, "gray92", "gray18"))


abline(h=quantile(operm, 0.95), lty=2, col=color[2])

if(!incl_effects) next  # skip the rest if no effects

yd <- 1
xl <- xaxisloc.scanone(out, c(4,7), c(0,0))
yl <- c(max(out, chr=5:7)[,3]+yd, par("usr")[4]-yd*0.1)
mx <- max(out)
mx.x <- xaxisloc.scanone(out, mx[[1]], mx[[2]])
mx.y <- mx[[3]]

rect(xl[1], yl[1], xl[2], yl[2], border=fgcolor, col=ifelse(bw, "white", NA))

for(y in yl)
  segments(mx.x, mx.y, xl[1], y, lty=2, col=ifelse(bw, "gray", "gray70"), lend=1, ljoin=1)
xat <- seq(xl[1], xl[2], len=7)[c(2,4,6)]
for(x in xat)
    segments(x, yl[1], x, yl[2], lwd=5, col=ifelse(bw, "gray90", "gray20"), lend=1, ljoin=1)
text(xat, rep(yl[1]-yd*0.35, length(xat)), names(me))

library(scales)
yaxlab <- yax <- pretty(range(ci))
me <- rescale(me, to=yl + c(1,-1)*diff(yl)*0.06, from=range(ci))
yax <- rescale(yax, to=yl + c(1,-1)*diff(yl)*0.06, from=range(ci))
ci <- rescale(ci, to=yl + c(1,-1)*diff(yl)*0.06, from=range(ci))

yaxlab <- yaxlab[yax > yl[1] & yax < yl[2]]
yax <- yax[yax > yl[1] & yax < yl[2]]

xw <- diff(xat)[1]


for(y in yax)
  segments(xl[1], y, xl[2], y, col=ifelse(bw, "gray40", "gray40"), lend=1, ljoin=1)
text(xl[2]+xw*0.12, yax, myround(yaxlab, 1), adj=c(0, 0.5), cex=0.8)

segments(xat-xw*0.1, me, xat+xw*0.1, me, col=color[3], lwd=2, lend=1, ljoin=1)
segments(xat, ci[1,], xat, ci[2,], col=color[3], lwd=2, lend=1, ljoin=1)
for(i in 1:2)
  segments(xat-xw*0.05, ci[i,], xat+xw*0.05, ci[i,], col=color[3], lwd=2, lend=1, ljoin=1)

rect(xl[1], yl[1], xl[2], yl[2], border=fgcolor)

dev.off()
#}
#}
