color <- broman::brocolors("web")["blue"]

pdf("../Figs/threepoints.pdf", width=2.5, height=1,
      pointsize=16, onefile=TRUE)

par(mar=rep(0.1,4),las=1,bty="n")
plot(0,0,xlim=c(0,100),ylim=c(3,6),xaxt="n",yaxt="n",xlab="",ylab="",type="n",
     xaxs="i",yaxs="i")

x <- c(5,95)
y <- c(3,3.5,4,5)
segments(x[1],y[2],x[2],y[2],lwd=3, lend=1, ljoin=1)
segments(x[1],y[1],x[1],y[3],lwd=3, lend=1, ljoin=1)
segments(x[2],y[1],x[2],y[3],lwd=3, lend=1, ljoin=1)
segments(mean(x),y[1],mean(x),y[3],lwd=3, lend=1, ljoin=1)
text(x, rep(y[4],2), c("1","3"), col=color[1], cex=2)
text(mean(x), y[4], c("2"), col=color[1], cex=2)

dev.off()
