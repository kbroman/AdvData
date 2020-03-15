# illustrations of the bootstrap

library(broman)
blue <- brocolors("web")["blue"]
pink <- brocolors("web")["fuchsia"]
purple <- brocolors("web")["purple"]

m <- 5
df <- 6
n <- 1000
samp <- rchisq(n, df)*m
mx <- qchisq(0.999, df)*m
mx <- max(mx, max(samp))
set.seed(20200315)
true_qu <- qchisq(0.95, df)*m

pdf("../Figs/bootstrap_illu.pdf", height=5.5, width=9.75, pointsize=18)
layout(cbind(c(rep(1,5), 2, rep(3,5)), c(rep(4,3), 5, rep(6,3), 7, rep(8,3))),
       height=rep(1, 11), width=c(6,5))



# population distribution
par(mar=c(3.1, 1.1, 0.1, 1.1))
x <- seq(0, mx, len=251)
y <- dchisq(x/m, df)/m
plot(x, y, type="l", lwd=2, yaxs="i", yaxt="n", ylab="", xlab="", xaxs="i", bty="n",
     ylim=c(0, max(y)*1.05), xlim=c(0, mx*1.05))
y_true_qu <- dchisq(true_qu/m, df)/m
segments(true_qu, 0, true_qu, y_true_qu, col=pink, lwd=3)
text(true_qu, y_true_qu*1.25, expression(theta), col=pink, cex=1.3, adj=c(0.5, 0))
u <- par("usr")
text(sum(u[1:2]*c(0.3,0.7)), sum(u[3:4]*c(0.3,0.7)),
     "Population", col=purple, cex=1.4)


# arrow
par(mar=rep(0,4))
plot(0,0,xlim=c(0, 100), ylim=c(0,100),
     xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
     xlab="", ylab="", type="n")
arrows(40, 100, 40, 0, len=0.15, col=blue, lwd=2)
text(45, 50, "Sample n=1000", col=blue, adj=c(0, 0.5), cex=1.3)

# sample
par(mar=c(3.1, 1.1, 0.1, 1.1))
hist(samp, breaks=seq(0, mx, len=61), main="", yaxt="n", xlab="", ylab="", xaxs="i")
u <- par("usr")
est_qu <- quantile(samp, 0.95)
segments(est_qu, 0, est_qu, diff(u[3:4])*0.2, lwd=3, col=pink)
text(est_qu, diff(u[3:4])*0.25, expression(hat(theta)), col=pink, cex=1.3, adj=c(0.5, 0))
text(sum(u[1:2]*c(0.3,0.7)), sum(u[3:4]*c(0.3,0.7)),
     "Data", col=purple, cex=1.4)






par(mar=c(3.1, 1.1, 0.1, 1.1))
x <- seq(0, mx, len=251)
y <- dchisq(x/m, df)/m
plot(x, y, type="l", lwd=2, yaxs="i", yaxt="n", ylab="", xlab="", xaxs="i", bty="n",
     ylim=c(0, max(y)*1.05), xlim=c(0, mx*1.05))
u <- par("usr")
text(sum(u[1:2]*c(0.2,0.8)), sum(u[3:4]*c(0.3,0.7)),
     "Population", col=purple, cex=1.2)

# arrow
par(mar=rep(0,4))
plot(0,0,xlim=c(0, 100), ylim=c(0,100),
     xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
     xlab="", ylab="", type="n")
arrows(40, 100, 40, 0, len=0.15, col=blue, lwd=2)
text(45, 50, "Sample n=1000", col=blue, adj=c(0, 0.5), cex=1.3)

# sample
par(mar=c(3.1, 1.1, 0.1, 1.1))
new_samp <- rchisq(n, df)*m
new_samp <- new_samp[new_samp <= mx]
hist(new_samp, breaks=seq(0, mx, len=61), main="", yaxt="n", xlab="", ylab="", xaxs="i")
est_qu <- quantile(new_samp, 0.95)
u <- par("usr")
segments(est_qu, 0, est_qu, diff(u[3:4])*0.2, lwd=3, col=pink)
text(est_qu, diff(u[3:4])*0.25, expression(hat(theta)), col=pink, cex=1.3, adj=c(0.5, 0))
text(sum(u[1:2]*c(0.2,0.8)), sum(u[3:4]*c(0.3,0.7)),
     "Simulated data", col=purple, cex=1.2)

# arrow
par(mar=rep(0,4))
plot(0,0,xlim=c(0, 100), ylim=c(0,100),
     xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
     xlab="", ylab="", type="n")
arrows(40, 100, 40, 0, len=0.15, col=blue, lwd=2)
text(45, 50, "[Repeat 1000 times]", col=blue, adj=c(0, 0.5), cex=1.3)

# bootstrap results
qu_boot <- replicate(1000, quantile(rchisq(n, df)*m, 0.95))

par(mar=c(3.1, 1.1, 0.1, 1.1))
hist(qu_boot, breaks=seq(0, mx, len=121), main="", yaxt="n", xlab="", ylab="", xaxs="i")
u <- par("usr")
text(sum(u[1:2]*c(0.2,0.8)), sum(u[3:4]*c(0.3,0.7)),
     "Distribution of\nEstimate", col=purple, cex=1.2)


dev.off()
