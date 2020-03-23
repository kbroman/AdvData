bgcolor <- "white"
fgcolor <- "black"
library(broman)
blue <- brocolors("web")["blue"]
gray <- "gray80"


pdf("../Figs/alphabet.pdf", height=5.2, width=10, pointsize=12)

load("../Data/health.RData")

par(mfrow=c(1,2), bg=bgcolor, las=1, fg=fgcolor,
    col.lab=fgcolor, col.axis=fgcolor,
    mar=c(5.1, 10.1, 1.1, 1.1))

plot(0, 0, type="n", xlab="Health care spending (% GDP)",
     ylab="", yaxt="n", xlim=c(0, max(health[,2])*1.05), xaxs="i",
     ylim=c(nrow(health), 0)+0.5, yaxs="i")
axis(side=2, at=1:25, health[,1])
abline(v=seq(5, 15, by=5), col=gray)
abline(h=seq(1, 25, by=5), col=blue)
points(health[,2], 1:25, pch=16)
axis(side=2, at=seq(1, 25, by=5), rep("", 5), col.ticks=blue)

o <- order(health[,2], decreasing=TRUE)
plot(0, 0, type="n", xlab="Health care spending (% GDP)",
     ylab="", yaxt="n", xlim=c(0, max(health[,2])*1.05), xaxs="i",
     ylim=c(nrow(health), 0)+0.5, yaxs="i")
axis(side=2, at=1:25, health[o,1])
abline(v=seq(5, 15, by=5), col=gray)
abline(h=seq(1, 25, by=5), col=blue)
points(health[o,2], 1:25, pch=16)
axis(side=2, at=seq(1, 25, by=5), rep("", 5), col.ticks=blue)
dev.off()
