##############################
# backcross_fig.R: Illustration of a backcross
##############################

library(simcross)
library(broman)
color <- brocolors("crayons")[c("Cornflower", "Blush")]

pdf(file="../Figs/backcross.pdf", width=9.75, height=5, pointsize=14, onefile=TRUE)

par(mar=rep(0.1,4),las=1,bty="n")
plot(0,0,xlim=c(0,864),ylim=c(0,480),xaxt="n",yaxt="n",xlab="",ylab="",type="n")
xd <- 2

rect(c(300,328),c(480,480),c(310,338),c(385,385), lend=1, ljoin=1,
     col=color[1], border=color[1])
rect(c(526,554),c(480,480),c(536,564),c(385,385),
     lend=1, ljoin=1, col=color[2], border=color[2])

points(432,440,pch=4,cex=2.5, lwd=2)
arrows(432, 400, 432, 300, len=0.1, lwd=2)

text(300-25,(480+385)/2,expression(P[1]),cex=2,adj=c(1,0.5))
text(564+25,(480+385)/2,expression(P[2]),cex=2,adj=c(0,0.5))

rect(413,287,423,192, lend=1, ljoin=1, col=color[1], border=color[1])
rect(441,287,451,192,col=color[2], border=color[2], lend=1, ljoin=1)

rect(413-176,287,423-176,192, lend=1, ljoin=1, col=color[1], border=color[1])
rect(441-176,287,451-176,192, lend=1, ljoin=1, col=color[1], border=color[1])

points(344,247,pch=4,cex=2.5, lwd=2)
segments(344,207,344,147, lwd=2)
segments(57,147,849,147, lwd=2)
arrows(seq(57,849,by=88),rep(147,10),seq(57,849,by=88),rep(107,10),len=0.1, lwd=2)

text(451+25,(287+192)/2,expression(F[1]),cex=2,adj=c(0,0.5))
text(413-176-25,(287+192)/2,expression(P[1]),cex=2,adj=c(1,0.5))

f1 <- create_parent(100,c(1,2))
set.seed(99020)
f2 <- vector("list",10)
for(i in 1:10) f2[[i]] <- cross(f1,f1,m=10,obl=TRUE)

xloc <- 38
mult <- 95/f2[[1]]$pat$locations[length(f2[[1]]$pat$locations)]
for(i in 1:10) {
  rect(xloc,0,xloc+10,95, lend=1, ljoin=1, col=color[1], border=color[1])
  rect(xloc+28,0,xloc+38,95, lend=1, ljoin=1, col=color[1], border=color[1])
  f2p <- c(0,f2[[i]]$pat$locations)
  start <- sample(2:3, 1)
  if(start <= length(f2p)) {
      for(j in seq(start, length(f2p), by=2)) {
          rect(xloc+28,f2p[j]*mult,xloc+38,f2p[j-1]*mult,
               col=color[2], border=color[2], lend=1, ljoin=1)
      }
  }
  xloc <- xloc+38+50
}
text(38-20,95/2,"BC",cex=2,adj=c(1,0.5))
dev.off()
