pink <- "violetred"
blue <- "slateblue"
bgcolor <- "white"

##############################
# base model
##############################
pdf(file="../Figs/hyper_models1.pdf", width=10, height=5.5, pointsize=20)

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
text(x,y,c("1","4","6","15"))

text(110, 60, "LOD = 23.1", adj=c(0, 0.5), col=blue)

dev.off()

##############################
# drop chr 1
##############################
pdf(file="../Figs/hyper_models2.pdf", width=10, height=5.5, pointsize=20)

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
text(x,y,c("1","4","6","15"))

text(110, 60, "LOD = 23.1", adj=c(0, 0.5), col=blue)
text(53, 70, "6.3", col=pink, adj=c(1,0.5))

text(2, 0, expression(paste(T[m], " = 2.69")), adj=c(0,0), col=pink)
text(24, 0, expression(paste(T[i]^H, " = 2.62")), adj=c(0,0), col=blue)
text(46, 0, expression(paste(T[i]^L, " = 1.19")), adj=c(0,0), col=blue)
text(68, 0, expression(paste(T[m] + T[i]^H, " = 5.31")), adj=c(0,0), col=blue)
text(98, 0, expression(paste(T[m] + T[i]^L, " = 3.88")), adj=c(0,0), col=blue)
text(128, 0, expression(paste(2*T[m], " = 5.38")), adj=c(0,0), col=blue)
dev.off()

##############################
# drop one at a time
##############################
for(i in 1:5){
pdf(file=paste("../Figs/hyper_models2", letters[i], ".pdf", sep=""),
    width=10, height=5.5, pointsize=20)

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
text(x,y,c("1","4","6","15"))

if(i==1) {
  text(53, 70, "6.3", col=pink, adj=c(1,0.5))
} else if (i==2){
  text(97, 70, "12.2", col=pink, adj=c(0,0.5))
} else if (i==3) {
  text(53, 50, "7.9", col=pink, adj=c(1,0.5))
} else if (i==4){
  text(97, 50, "7.1", col=pink, adj=c(0,0.5))
} else if (i==5) {
  text(75, 45.3, "5.7", col=pink, adj=c(0.5,0.5))
}
u <- par("usr")
text(2, 0, expression(paste(T[m], " = 2.69")), adj=c(0,0), col=c(pink,pink,blue,blue,blue)[i])
text(24, 0, expression(paste(T[i]^H, " = 2.62")), adj=c(0,0), col=blue)
text(46, 0, expression(paste(T[i]^L, " = 1.19")), adj=c(0,0), col=c(blue,blue,blue,blue,pink)[i])
text(68, 0, expression(paste(T[m] + T[i]^H, " = 5.31")), adj=c(0,0), col=blue)
text(98, 0, expression(paste(T[m] + T[i]^L, " = 3.88")), adj=c(0,0), col=c(blue,blue,pink,pink,blue)[i])
text(128, 0, expression(paste(2*T[m], " = 5.38")), adj=c(0,0), col=blue)

dev.off()
}

##############################
# drop one at a time
##############################
pdf(file="../Figs/hyper_models3.pdf", width=10, height=5.5, pointsize=20)

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
text(x,y,c("1","4","6","15"))

text(53, 70, "6.3", col=pink, adj=c(1,0.5))
text(97, 70, "12.2", col=pink, adj=c(0,0.5))
text(53, 50, "7.9", col=pink, adj=c(1,0.5))
text(97, 50, "7.1", col=pink, adj=c(0,0.5))
text(75, 45.3, "5.7", col=pink, adj=c(0.5,0.5))

u <- par("usr")
text(2, 0, expression(paste(T[m], " = 2.69")), adj=c(0,0), col=pink)
text(24, 0, expression(paste(T[i]^H, " = 2.62")), adj=c(0,0), col=blue)
text(46, 0, expression(paste(T[i]^L, " = 1.19")), adj=c(0,0), col=pink)
text(68, 0, expression(paste(T[m] + T[i]^H, " = 5.31")), adj=c(0,0), col=blue)
text(98, 0, expression(paste(T[m] + T[i]^L, " = 3.88")), adj=c(0,0), col=pink)
text(128, 0, expression(paste(2*T[m], " = 5.38")), adj=c(0,0), col=blue)

dev.off()

##############################
# another interaction?
##############################
int1 <- c(1,1,1,2,2)
int2 <- c(2,3,4,3,4)
lod <- c("0.6","0.1","0.4","0.2","0.0")

for(i in 1:5) {

  pdf(file=paste("../Figs/hyper_models", i+3, ".pdf", sep=""),
      width=10, height=5.5, pointsize=20)

  par(mar=rep(0.1,4))
  plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
       xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

  x <- c(60, 90, 60, 90)
  y <- c(70, 70, 50, 50)
  segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
  segments(x[int1[i]], y[int1[i]], x[int2[i]], y[int2[i]], lwd=2, ljoin=1, lend=1,
           col=blue)
  points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
  points(x, y, pch=1, cex=4, lwd=2)
  text(x,y,c("1","4","6","15"))
  text(97, 60, lod[i], col=pink, adj=c(0,0.5))


  u <- par("usr")
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19    ",
#    T[m] + T[i]^H, " = 5.31    ", T[m] + T[i]^L, " = 3.88")),
#     adj=c(0,0), col=blue)
#  text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19")),
#       adj=c(0,0), col=blue)
text(2, 0, expression(paste(T[m], " = 2.69")), adj=c(0,0), col=blue)
text(24, 0, expression(paste(T[i]^H, " = 2.62")), adj=c(0,0), col=c(blue,pink,pink,pink,pink)[i])
text(46, 0, expression(paste(T[i]^L, " = 1.19")), adj=c(0,0), col=c(pink,blue,blue,blue,blue)[i])
text(68, 0, expression(paste(T[m] + T[i]^H, " = 5.31")), adj=c(0,0), col=blue)
text(98, 0, expression(paste(T[m] + T[i]^L, " = 3.88")), adj=c(0,0), col=blue)
text(128, 0, expression(paste(2*T[m], " = 5.38")), adj=c(0,0), col=blue)

  dev.off()
}

##############################
# another QTL?
##############################
pdf(file=paste("../Figs/hyper_models", 9, ".pdf", sep=""),
    width=10, height=5.5, pointsize=20)

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
text(x,y,c("1","4","6","15"))

xx <- c(40,40,40)
yy <- c(80,60,40)
points(xx, yy, pch=1, cex=4, lwd=2, col=blue)
text(xx, yy, c("1b","2","5"), col=blue)
text(xx-7, yy, c("1.87", "1.52","1.62"), col=pink, adj=c(1,0.5))

u <- par("usr")
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19    ",
#    T[m] + T[i]^H, " = 5.31    ", T[m] + T[i]^L, " = 3.88")),
#     adj=c(0,0), col=blue)
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19")),
#     adj=c(0,0), col=blue)
text(2, 0, expression(paste(T[m], " = 2.69")), adj=c(0,0), col=pink)
text(24, 0, expression(paste(T[i]^H, " = 2.62")), adj=c(0,0), col=blue)
text(46, 0, expression(paste(T[i]^L, " = 1.19")), adj=c(0,0), col=blue)
text(68, 0, expression(paste(T[m] + T[i]^H, " = 5.31")), adj=c(0,0), col=blue)
text(98, 0, expression(paste(T[m] + T[i]^L, " = 3.88")), adj=c(0,0), col=blue)
text(128, 0, expression(paste(2*T[m], " = 5.38")), adj=c(0,0), col=blue)

dev.off()

##############################
# another interacting QTL?
##############################
pdf(file=paste("../Figs/hyper_models", 10, ".pdf", sep=""),
    width=10, height=5.5, pointsize=20)

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
xx <- 110; yy <- 40
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
segments(x[4], y[4], xx, yy, lwd=2, ljoin=1, lend=1, col=blue)
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
points(xx, yy, pch=16, cex=4, lwd=2, col=bgcolor)
points(xx, yy, pch=1, cex=4, lwd=2, col=blue)
text(x,y,c("1","4","6","15"))
text(xx,yy,"7", col=blue)
text(xx,yy+6, "2.66", col=pink, adj=c(0.5,0))

u <- par("usr")
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19    ",
#    T[m] + T[i]^H, " = 5.31    ", T[m] + T[i]^L, " = 3.88")),
#     adj=c(0,0), col=blue)
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19")),
#     adj=c(0,0), col=blue)
text(2, 0, expression(paste(T[m], " = 2.69")), adj=c(0,0), col=blue)
text(24, 0, expression(paste(T[i]^H, " = 2.62")), adj=c(0,0), col=blue)
text(46, 0, expression(paste(T[i]^L, " = 1.19")), adj=c(0,0), col=blue)
text(68, 0, expression(paste(T[m] + T[i]^H, " = 5.31")), adj=c(0,0), col=pink)
text(98, 0, expression(paste(T[m] + T[i]^L, " = 3.88")), adj=c(0,0), col=blue)
text(128, 0, expression(paste(2*T[m], " = 5.38")), adj=c(0,0), col=blue)

dev.off()

##############################
# another interacting QTL?
##############################
pdf(file=paste("../Figs/hyper_models", 11, ".pdf", sep=""),
    width=10, height=5.5, pointsize=20)

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
xx <- 40; yy <- 40
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
segments(x[3], y[3], xx, yy, lwd=2, ljoin=1, lend=1, col=blue)
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
points(xx, yy, pch=16, cex=4, lwd=2, col=bgcolor)
points(xx, yy, pch=1, cex=4, lwd=2, col=blue)
text(x,y,c("1","4","6","15"))
text(xx,yy,"4b", col=blue)
text(xx,yy+6, "2.82", col=pink, adj=c(0.5,0))


u <- par("usr")
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19    ",
#    T[m] + T[i]^H, " = 5.31    ", T[m] + T[i]^L, " = 3.88")),
#     adj=c(0,0), col=blue)
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19")),
#     adj=c(0,0), col=blue)
text(2, 0, expression(paste(T[m], " = 2.69")), adj=c(0,0), col=blue)
text(24, 0, expression(paste(T[i]^H, " = 2.62")), adj=c(0,0), col=blue)
text(46, 0, expression(paste(T[i]^L, " = 1.19")), adj=c(0,0), col=blue)
text(68, 0, expression(paste(T[m] + T[i]^H, " = 5.31")), adj=c(0,0), col=pink)
text(98, 0, expression(paste(T[m] + T[i]^L, " = 3.88")), adj=c(0,0), col=blue)
text(128, 0, expression(paste(2*T[m], " = 5.38")), adj=c(0,0), col=blue)

dev.off()

######################################################################
# add another pair of QTL?
######################################################################
pdf(file=paste("../Figs/hyper_models", 12, ".pdf", sep=""),
    width=10, height=5.5, pointsize=20)

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
xx <- 45; yy <- 40; xxd <- 9
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
#segments(x[3], y[3], xx, yy, lwd=2, ljoin=1, lend=1, col=blue)
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
points(xx, yy, pch=16, cex=4, lwd=2, col=bgcolor)
points(xx-xxd, yy, pch=16, cex=4, lwd=2, col=bgcolor)
points(xx, yy, pch=1, cex=4, lwd=2, col=blue)
points(xx-xxd, yy, pch=1, cex=4, lwd=2, col=blue)
text(x,y,c("1","4","6","15"))
text(xx,yy,"3b", col=blue)
text(xx-xxd,yy,"3a", col=blue)
text(xx-xxd/2,yy+6, "4.60", col=pink, adj=c(0.5,0))


u <- par("usr")
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19    ",
#    T[m] + T[i]^H, " = 5.31    ", T[m] + T[i]^L, " = 3.88")),
#     adj=c(0,0), col=blue)
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19")),
#     adj=c(0,0), col=blue)
text(2, 0, expression(paste(T[m], " = 2.69")), adj=c(0,0), col=blue)
text(24, 0, expression(paste(T[i]^H, " = 2.62")), adj=c(0,0), col=blue)
text(46, 0, expression(paste(T[i]^L, " = 1.19")), adj=c(0,0), col=blue)
text(68, 0, expression(paste(T[m] + T[i]^H, " = 5.31")), adj=c(0,0), col=blue)
text(98, 0, expression(paste(T[m] + T[i]^L, " = 3.88")), adj=c(0,0), col=blue)
text(128, 0, expression(paste(2*T[m], " = 5.38")), adj=c(0,0), col=pink)

dev.off()
