# plots of rf vs R for 8-way RIL by sibling mating, from simulations

library(broman)

color <- c(rgb(102,203,254,maxColorValue=255), # light blue
           rgb(254,  0,128,maxColorValue=255), # dark pink
           rgb(102,254,102,maxColorValue=255), # light green
           rgb(128,  0,128,maxColorValue=255), # dark purple
           rgb(203,102,254,maxColorValue=255), # light purple
           rgb(254,203,102,maxColorValue=255), # yellow/gold
           rgb(  0,128,128,maxColorValue=255), # dark green
           rgb(102,102,102,maxColorValue=255)) # gray

# text
color2 <- c(rgb(255, 255, 102, maxColorValue=255), # yellow
            rgb(102, 204, 255, maxColorValue=255), # light blue
            rgb(255, 102, 255, maxColorValue=255)) # pink


textcolor <- c(yellow=rgb(1, 1, 0.7),
               blue=rgb(0.4,0.8,1),
               darkblue=rgb(0, 0.25, 0.5),
               pink=rgb(1,0.7,1),
               hotpink=rgb(1,0,0.6))



# just the points
pdf("../Figs/rf_by_sim.pdf", width=9.75, height=5.5, pointsize=16, onefile=TRUE)
par(mar=c(4.1, 4.1, 1.1, 1.1),las=1,bty="n")
load("ri8_onestep_rev.RData.gz")
rf <- as.numeric(names(onestep))
wh <- (rf*1000 == round(rf*1000))

xat <- seq(0, 0.5, by=0.05)
yat <- seq(0, 0.8, by=0.1)
grayplot(rf[wh], onestep[wh], xlab="recombination fraction", ylab="Pr ( recombination in RIL )",
         xat=xat, yat=yat,
         hlines=yat, vlines=xat, ylim=c(0, 7/8+0.02), xaxs="i", yaxs="i",
         pch=21, col="black", bg=color[1], xlim=c(0, 0.5),
         mgp.x=c(2.1, 0.4, 0), mgp.y=c(2.6, 0.4, 0))
mtext(side=2, "", line=2.6, col=textcolor["blue"], las=0)
abline(h=7/8, col=color2[3], lwd=2)
mtext(side=2, at=7/8, "7/8", col=color2[3], line=0.4)

dev.off()







# with the general ar/(1+br) formula and curve
pdf("../Figs/rf_by_sim_genformula.pdf", width=9.75, height=5.5, pointsize=16, onefile=TRUE)
par(mar=c(4.1, 4.1, 1.1, 1.1),las=1,bty="n")

grayplot(rf[wh], onestep[wh], xlab="recombination fraction", ylab="Pr ( recombination in RIL )",
         xat=xat, yat=yat,
         hlines=yat, vlines=xat, ylim=c(0, 7/8+0.02), xaxs="i", yaxs="i",
         pch=21, col="black", bg=color[1], xlim=c(0, 0.5),
         mgp.x=c(2.1, 0.4, 0), mgp.y=c(2.6, 0.4, 0))
abline(h=7/8, col=color2[3], lwd=2)
mtext(side=2, at=7/8, "7/8", col=color2[3], line=0.4)

rect(0.305, 0.403, 0.445, 0.497, col="gray90", border="gray90", alpha=0.7)
text(0.31, 0.45, "R = a r / ( 1 + b r )", col="black", adj=c(0, 0.5))
text(0.436, 0.45, "?", col=color[2])
dev.off()



# with additional data points
pdf("../Figs/rf_by_sim_moredata.pdf", width=9.75, height=5.5, pointsize=16, onefile=TRUE)
par(mar=c(4.1, 4.1, 1.1, 1.1),las=1,bty="n")

grayplot(rf[wh], onestep[wh], xlab="recombination fraction", ylab="Pr ( recombination in RIL )",
         xat=xat, yat=yat,
         hlines=yat, vlines=xat, ylim=c(0, 7/8+0.02), xaxs="i", yaxs="i",
         pch=21, col="black", bg=color[1], xlim=c(0, 0.5),
         mgp.x=c(2.1, 0.4, 0), mgp.y=c(2.6, 0.4, 0))
abline(h=7/8, col=color2[3], lwd=2)
mtext(side=2, at=7/8, "7/8", col=color2[3], line=0.4)

x <- seq(0, 0.5, length=251)
lines(x, 7*x/(1+6*x), col=color2[1], lwd=2)
points(rf[wh], onestep[wh], col="black", bg=color[1], pch=21)
points(rf[!wh], onestep[!wh], col="black", bg=color[3], pch=21, cex=0.6)

rect(0.305, 0.403, 0.445, 0.497, col="gray90", border="gray90", alpha=0.7)
text(0.32, 0.45, "R = 7 r / ( 1 + 6 r )", col="black", adj=c(0, 0.5))

dev.off()


load("ri8_onestep_rev.RData.gz")
rf <- as.numeric(names(onestep))
wh <- (rf*1000 == round(rf*1000))
dat <- data.frame(rf=rf[wh], R=onestep[wh])
out <- nls(R ~ a*rf/(1+b*rf), data=dat, start=list(a=4, b=6))
summary(out)
out <- nls(R ~ a*rf/(1+b*rf), data=dat, start=list(a=2, b=2))
summary(out)
