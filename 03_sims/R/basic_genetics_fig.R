##############################
# RI lines
##############################

color <- c(rgb(102,203,254,maxColorValue=255),
           rgb(254,  0,128,maxColorValue=255),
           broman::brocolors("web")["green"])

pdf("../Figs/basic_genetics.pdf", width=9.75, height=3.5, pointsize=24, onefile=TRUE)

par(mar=rep(0.1,4),las=1,bty="n")
plot(0,0,xlim=c(-10,190),ylim=c(-82.5,75),xaxt="n",yaxt="n",xlab="",ylab="",type="n",
     xaxs="i", yaxs="i")

text(c(10,10), c(30,60), c("A","A"), col=color[1])
segments(15, 20, 15, 70, lwd=2)
text(c(20,20), c(30,60), c("B","B"), col=color[2])

arrows(30, 45, 50, 45, lwd=2, len=0.1)

x <- 70
text(c(x,x), c(30,60), c("A","A"), col=color[1])
segments(x+5, 20, x+5, 70, lwd=2)
text(x+2.5, -10, expression((1-r)/2), adj=c(0.5, 0), col=color[3], cex=0.8)

x <- 100
text(c(x,x), c(30,60), c("B","B"), col=color[2])
segments(x+5, 20, x+5, 70, lwd=2)
text(x+2.5, -10, expression((1-r)/2), adj=c(0.5, 0), col=color[3], cex=0.8)

x <- 130
text(c(x,x), c(30,60), c("B","A"), col=color[2:1])
segments(x+5, 20, x+5, 70, lwd=2)
text(x+2.5, -10, expression(r/2), adj=c(0.5, 0), col=color[3], cex=0.8)

x <- 160
text(c(x,x), c(30,60), c("A","B"), col=color[1:2])
segments(x+5, 20, x+5, 70, lwd=2)
text(x+2.5, -10, expression(r/2), adj=c(0.5, 0), col=color[3], cex=0.8)

text(115, -75, expression(paste(r, " is the \"recombination fraction\"")), col=color[3], adj=c(0.5,0))


dev.off()
