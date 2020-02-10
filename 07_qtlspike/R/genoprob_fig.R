library(qtl)
data(hyper)

color <- "violetred"

pdf(file="../Figs/genoprob1.pdf", width=9.5, height=2, pointsize=16, onefile=TRUE)
par(mar=rep(0.1,4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0,100), ylim=c(60,110), xaxs="i", yaxs="i")
segments(5,95,95,95, lwd=2)
data(hyper)
m <- hyper$geno[[8]]$map
m[3] <- m[3]-5
m[4] <- m[4]+5
x <- (m-min(m))/diff(range(m))*90+5
segments(5,95,95,95, lwd=2)
segments(x, 92, x, 98, lwd=2)
text(x, 85, c("A","A","A","A","A","B"))
x2 <- 48
points(x2, 100, col=color[1], lwd=2, pch=6, cex=1.3)
arrows(x2, 70, x2, 80, len=0.1, lwd=2, col=color[1])
text(x2, 68, "?", col=color[1], adj=c(0.5,1), cex=1.2)
dev.off()

pdf(file="../Figs/genoprob2.pdf", width=9.5, height=2, pointsize=16, onefile=TRUE)
par(mar=rep(0.1,4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0,100), ylim=c(60,110), xaxs="i", yaxs="i")
segments(5,95,95,95, lwd=2)
data(hyper)
m <- hyper$geno[[8]]$map
m[3] <- m[3]-5
m[4] <- m[4]+5
x <- (m-min(m))/diff(range(m))*90+5
segments(5,95,95,95, lwd=2)
segments(x, 92, x, 98, lwd=2)
text(x, 85, c("A","A","A","A","A","B"))
x2 <- 48
points(x2, 100, col=color[1], lwd=2, pch=6, cex=1.3)
arrows(x2, 70, x2, 80, len=0.1, lwd=2, col=color[1])
text(x2, 68, "?", col=color[1], adj=c(0.5,1), cex=1.2)
points(x[3:4], c(85,85), lwd=2, cex=3, col=color[1])
dev.off()

pdf(file="../Figs/genoprob3.pdf", width=9.5, height=2, pointsize=16, onefile=TRUE)
par(mar=rep(0.1,4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0,100), ylim=c(60,110), xaxs="i", yaxs="i")
segments(5,95,95,95, lwd=2)
data(hyper)
m <- hyper$geno[[8]]$map
m[3] <- m[3]-5
m[4] <- m[4]+5
x <- (m-min(m))/diff(range(m))*90+5
segments(5,95,95,95, lwd=2)
segments(x, 92, x, 98, lwd=2)
text(x, 85, c("B","A","A","-","B","A"))
x2 <- 48
points(x2, 100, col=color[1], lwd=2, pch=6, cex=1.3)
arrows(x2, 70, x2, 80, len=0.1, lwd=2, col=color[1])
text(x2, 68, "?", col=color[1], adj=c(0.5,1), cex=1.2)
dev.off()

pdf(file="../Figs/genoprob4.pdf", width=9.5, height=2, pointsize=16, onefile=TRUE)
par(mar=rep(0.1,4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0,100), ylim=c(60,110), xaxs="i", yaxs="i")
segments(5,95,95,95, lwd=2)
data(hyper)
m <- hyper$geno[[8]]$map
m[3] <- m[3]-5
m[4] <- m[4]+5
x <- (m-min(m))/diff(range(m))*90+5
segments(5,95,95,95, lwd=2)
segments(x, 92, x, 98, lwd=2)
text(x, 85, c("B","A","A","-","B","A"))
x2 <- 48
points(x2, 100, col=color[1], lwd=2, pch=6, cex=1.3)
arrows(x2, 70, x2, 80, len=0.1, lwd=2, col=color[1])
text(x2, 68, "?", col=color[1], adj=c(0.5,1), cex=1.2)
points(x[c(3,5)], c(85,85), lwd=2, cex=3, col=color[1])
dev.off()

pdf(file="../Figs/genoprob5.pdf", width=9.5, height=2, pointsize=16, onefile=TRUE)
par(mar=rep(0.1,4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0,100), ylim=c(60,110), xaxs="i", yaxs="i")
segments(5,95,95,95, lwd=2)
data(hyper)
m <- hyper$geno[[8]]$map
m[3] <- m[3]-5
m[4] <- m[4]+5
x <- (m-min(m))/diff(range(m))*90+5
segments(5,95,95,95, lwd=2)
segments(x, 92, x, 98, lwd=2)
text(x, 85, c("A","B","-","B","A","A"))
x2 <- 48
points(x2, 100, col=color[1], lwd=2, pch=6, cex=1.3)
arrows(x2, 70, x2, 80, len=0.1, lwd=2, col=color[1])
text(x2, 68, "?", col=color[1], adj=c(0.5,1), cex=1.2)
dev.off()

pdf(file="../Figs/genoprob6.pdf", width=9.5, height=2, pointsize=16, onefile=TRUE)
par(mar=rep(0.1,4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0,100), ylim=c(60,110), xaxs="i", yaxs="i")
segments(5,95,95,95, lwd=2)
data(hyper)
m <- hyper$geno[[8]]$map
m[3] <- m[3]-5
m[4] <- m[4]+5
x <- (m-min(m))/diff(range(m))*90+5
segments(5,95,95,95, lwd=2)
segments(x, 92, x, 98, lwd=2)
text(x, 85, c("A","B","-","B","A","A"))
x2 <- 48
points(x2, 100, col=color[1], lwd=2, pch=6, cex=1.3)
arrows(x2, 70, x2, 80, len=0.1, lwd=2, col=color[1])
text(x2, 68, "?", col=color[1], adj=c(0.5,1), cex=1.2)
points(x[c(2,4)], c(85,85), lwd=2, cex=3, col=color[1])
dev.off()
