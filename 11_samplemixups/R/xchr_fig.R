######################################################################
# illustration of the X chromosome
######################################################################

source("meiosis_func.R")

bgcolor <- "white"
color <- broman::brocolors("crayons")[c("Denim", "Blush")]

pdf("../Figs/xchr_fig.pdf", width=5, height=6.5, pointsize=12, onefile=TRUE)
par(mar=rep(0.1,4), lwd=3, bty="n")
plot(0, 0, type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(125,700), ylim=c(-40,480))

rect(c(300,328),c(480,480),c(310,338),c(385,385),col=color[1],border=color[1], lend=1, ljoin=1)
rect(c(526,554),c(480,480),c(536,564),c(385,456),col=color[2],border=color[2], lend=1, ljoin=1)

points(432,440,pch=4,cex=2.5,lwd=2)
segments(432,400,432,340,lwd=2)
segments(319,340,545,340,lwd=2)
arrows(c(319,545),c(340,340),c(319,545),c(300,300),lwd=2,len=0.1)
#arrows(432,400,432,340,lwd=2,len=0.1)

text(200,(480+385)/2,expression(BTBR),cex=1.5,adj=c(0.5,0.5), col=color[1])
text(634,(480+385)/2,expression(B6),cex=1.5,adj=c(0.5,0.5), col=color[2])

rect(300,287,310,192,col=color[1],border=color[1], lend=1, ljoin=1)
rect(328,287,338,192,col=color[2],border=color[2], lend=1, ljoin=1)
rect(526,287,536,192,col=color[1],border=color[1], lend=1, ljoin=1)
rect(554,287,564,263,col=color[2],border=color[2], lend=1, ljoin=1)

points(432,247,pch=4,cex=2.5,lwd=2)
segments(432,208,432,147,lwd=2)
segments(319,147,545,147,lwd=2)
arrows(c(319,545),c(147,147),c(319,545),c(107,107),lwd=2,len=0.1)


text(200,(287+192)/2,expression(F[1]),cex=1.5,adj=c(0.5,0.5))

f1 <- create.par(100,c(1,2))
set.seed(20200223)
f2 <- vector("list",2)
for(i in 1:2) f2[[3-i]] <- cross(f1,f1,m=2,obl=TRUE)

xloc <- 300
mult <- 95/f2[[1]]$mat[1,ncol(f2[[1]]$mat)]
for(i in 1:2) {
    rect(xloc,0,xloc+10,95,   col=color[1],border=color[1], lend=1, ljoin=1)
    if(i==1)
      rect(xloc+28,0,xloc+38,95,col=color[1],border=color[1], lend=1, ljoin=1)
    else
      rect(xloc+28,71,xloc+38,95,col=color[2],border=color[2], lend=1, ljoin=1)

  f2m <- f2[[i]]$mat
  for(j in 2:ncol(f2m)) {
    if(f2m[2,j]==2) {
        rect(xloc,f2m[1,j]*mult,xloc+10,f2m[1,j-1]*mult,col=color[2],border=color[2], lend=1, ljoin=1)
    }
  }
  xloc <- xloc+226
}
text(200,95/2,expression(F[2]),cex=1.5,adj=c(0.5,0.5))

text(319,-35, "Female", cex=1.5)
text(319+226,-35, "Male", cex=1.5)



dev.off()
