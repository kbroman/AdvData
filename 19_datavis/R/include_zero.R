library(broman)
bgcolor <- "white"
fgcolor <- "black"
gray <- "gray80"
pink <- brocolors("web")["fuchsia"]
blue <- brocolors("web")["blue"]
green <- brocolors("web")["green"]

pdf("../Figs/include_zero.pdf", height=5.2, width=10, pointsize=10)
par(mfrow=c(1,2), bg=bgcolor, las=1, fg=fgcolor,
    col.lab=fgcolor, col.axis=fgcolor)
par(mar=c(4.1,4.6,1.1, 1.1))
rate <- c(96.5, 98.1, 99.2)
b <- barplot(rate, col=c(blue, green, pink), ylab="Detection rate (%)",
             ylim=c(0, 120), col.lab=fgcolor, yaxs="i",
             border=c(blue, green, pink), xlab="Method",
             names.arg=rep("", 3), cex.lab=1.5, cex.axis=1.5)
u <- par("usr")
rect(u[1], u[3], u[2], u[4], border=fgcolor)
axis(side=1, at=b, LETTERS[1:3], col.axis=fgcolor, col=fgcolor,
     cex.axis=1.5, tick=FALSE)
text(b, rate+5, paste0(rate, "%"))

plot(0, 0, type="n", ylim=c(95, 100), yaxs="i",
     xlim=c(0.5, 3.5), xaxt="n", xlab="Method",
     cex.lab=1.5, cex.axis=1.5, ylab="Detection rate (%)")
abline(h=96:99, col=gray)
x <- 1:3
xd <- 0.2
se <- sqrt(rate*(100-rate)/1000)
segments(x, rate-se, x, rate+se, lwd=2, col=c(blue, green, pink))
segments(x-xd/2, rate-se, x+xd/2, rate-se, lwd=2, col=c(blue, green, pink))
segments(x-xd/2, rate+se, x+xd/2, rate+se, lwd=2, col=c(blue, green, pink))
segments(x-xd, rate, x+xd, rate, lwd=3, col=c(blue, green, pink))
axis(side=1, at=1:3, LETTERS[1:3], cex.axis=1.5)
dev.off()
