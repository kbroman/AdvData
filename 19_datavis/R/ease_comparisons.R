library(broman)
bgcolor <- "white"
fgcolor <- "black"
gray <- "gray80"


pdf("../Figs/ease_comparisons_1.pdf", height=5.5, width=9.75, pointsize=12)
par(mfrow=c(1,2))
set.seed(11341)
me <- c(10, 15, 17, 12, 16, 18)
se <- runif(length(me), 0.5, 1)
me <- me[c(1,4,2,5,3,6)]
lo <- me - se
hi <- me + se
xd <- 0.2
xat <- c(1, 2, 3)
xat <- sort(c(xat - xd, xat + xd))
par(bg=bgcolor, fg=fgcolor, col=fgcolor, las=1, col.axis=fgcolor,
    col.lab=fgcolor)
par(mar=c(8.1,4.1,1.1,1.1))
plot(0, 0, type="n", xlab="", xaxt="n", ylab="Phenotype",
     ylim=range(c(lo, hi))+c(-1, +1), xlim=range(xat)+c(-xd, xd)*2,
     xaxs="i", yaxs="i")
abline(h=pretty(c(lo, hi)), col=gray)
segments(xat, lo, xat, hi, lwd=2)
segments(xat-xd/8, lo, xat+xd/8, lo, lwd=2)
segments(xat-xd/8, hi, xat+xd/8, hi, lwd=2)
segments(xat-xd/4, me, xat+xd/4, me, lwd=2)
axis(side=1, at=xat, rep(c("Female", "Male"), 3), cex.axis=0.7,
     fg=fgcolor, col.axis=fgcolor)
axis(side=1, at=1:3, c("AA", "AB", "BB"), col.axis=fgcolor,
     tick=FALSE, line=2, cex.axis=1.3)


me <- me[c(1,3,5,2,4,6)]
se <- se[c(1,3,5,2,4,6)]
lo <- me - se
hi <- me + se
xd <- 0.2
xat <- c(0.3, 0.6, 0.9, 1.5, 1.8, 2.1)
plot(0, 0, type="n", xlab="", xaxt="n", ylab="Phenotype",
     ylim=range(c(lo, hi))+c(-1, +1), xlim=range(xat)+c(-xd, xd)*2,
     xaxs="i", yaxs="i")
abline(h=pretty(c(lo, hi)), col=gray)
segments(xat, lo, xat, hi, lwd=2)
segments(xat-xd/8, lo, xat+xd/8, lo, lwd=2)
segments(xat-xd/8, hi, xat+xd/8, hi, lwd=2)
segments(xat-xd/4, me, xat+xd/4, me, lwd=2)
axis(side=1, at=xat, rep(c("AA", "AB", "BB"), 2),
     fg=fgcolor, col.axis=fgcolor)
axis(side=1, at=c(0.6, 1.8), c("Female", "Male"), col.axis=fgcolor,
     tick=FALSE, line=2, cex.axis=1.3)
dev.off()






pdf("../Figs/ease_comparisons_2.pdf", height=5.5, width=9.75, pointsize=12)
par(mfrow=c(1,2))
set.seed(11341)
me <- c(10, 15, 17, 12, 16, 18)
se <- runif(length(me), 0.5, 1)
me <- me[c(1,4,2,5,3,6)]
lo <- me - se
hi <- me + se
xd <- 0.2
xat <- c(1, 2, 3)
xat <- sort(c(xat - xd, xat + xd))
pink <- brocolors("web")["fuchsia"]
blue <- brocolors("web")["blue"]
col <- rep(c(pink, blue), 3)
par(bg=bgcolor, fg=fgcolor, col=fgcolor, col.axis=fgcolor, col.lab=fgcolor,
    las=1)
par(mar=c(8.1,4.1,1.1,1.1))
plot(0, 0, type="n", xlab="", xaxt="n", ylab="Phenotype",
     ylim=range(c(lo, hi))+c(-1, +1), xlim=range(xat)+c(-xd, xd)*2,
     xaxs="i", yaxs="i")
abline(h=pretty(c(lo, hi)), col=gray)
segments(xat, lo, xat, hi, lwd=2, col=col)
segments(xat-xd/8, lo, xat+xd/8, lo, lwd=2, col=col)
segments(xat-xd/8, hi, xat+xd/8, hi, lwd=2, col=col)
segments(xat-xd/4, me, xat+xd/4, me, lwd=2, col=col)
axis(side=1, at=xat, rep("", 6), cex.axis=0.7)
for(i in seq(along=xat))
  axis(side=1, at=xat[i], rep(c("Female", "Male"), 3)[i], cex.axis=0.7,
       col=fgcolor, col.axis=col[i], tick=FALSE)
axis(side=1, at=1:3, c("AA", "AB", "BB"),
     tick=FALSE, line=2, cex.axis=1.3)


me <- me[c(1,3,5,2,4,6)]
se <- se[c(1,3,5,2,4,6)]
lo <- me - se
hi <- me + se
xd <- 0.2
xat <- c(0.3, 0.6, 0.9, 1.5, 1.8, 2.1)
col <- rep(c(pink, blue), rep(3, 2))
plot(0, 0, type="n", xlab="", xaxt="n", ylab="Phenotype",
     ylim=range(c(lo, hi))+c(-1, +1), xlim=range(xat)+c(-xd, xd)*2,
     xaxs="i", yaxs="i")
abline(h=pretty(c(lo, hi)), col=gray)
segments(xat, lo, xat, hi, lwd=2, col=col)
segments(xat-xd/8, lo, xat+xd/8, lo, lwd=2, col=col)
segments(xat-xd/8, hi, xat+xd/8, hi, lwd=2, col=col)
segments(xat-xd/4, me, xat+xd/4, me, lwd=2, col=col)
axis(side=1, at=xat, rep(c("AA", "AB", "BB"), 2),
     col=fgcolor, col.axis=fgcolor)
for(i in 1:2)
  axis(side=1, at=c(0.6, 1.8)[i], c("Female", "Male")[i], col.axis=col[c(1,4)][i],
       tick=FALSE, line=2, cex.axis=1.3)
dev.off()
