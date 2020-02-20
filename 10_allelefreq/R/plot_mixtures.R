# plot mixtures

library(broman)

# first univariate

pdf("../Figs/mixture_univariate.pdf", width=9.75, height=5.5, pointsize=12)

x <- seq(45, 90, len=251)
y1 <- dnorm(x, 63, 5)*0.6
y2 <- dnorm(x, 69, 6)*0.4

par(mar=c(2.1, 2.1, 0.6, 0.6))
grayplot(0,0,type="n", xlab="", ylab="", yat=NA, hlines=pretty(c(y1, y2)),
         xlim=range(x), ylim=c(0, max(c(y1, y2))*1.05), xaxs="i", yaxs="i")
lines(x, y1, col=brocolors("web")["purple"], lwd=2)
lines(x, y2, col=brocolors("web")["green"], lwd=2)
dev.off()


pdf("../Figs/mixture_bivariate.pdf", width=5.5, height=5.5, pointsize=12)

par(mar=c(1.6, 1.6, 0.6, 0.6), pty="s")
x <- rmvn(200, c(40,60), 3*cbind(c(5, 3), c(3,5)))
y <- rmvn(400, c(50,50), 3*cbind(c(5, -3), c(-3, 5)))
z <- rmvn(200, c(60,40), 3*cbind(c(5, 0), c(0, 5)))
li <- range(c(x, y, z))
grayplot(x[,1], x[,2], xlim=li, ylim=li, xlab="", ylab="",
         bg=brocolors("web")["orange"], mgp=c(0, 0.2, 0))
points(y, bg=brocolors("web")["teal"], pch=21)
points(z, bg=brocolors("web")["maroon"], pch=21)

dev.off()
