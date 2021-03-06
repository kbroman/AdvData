library(broman)
red <- brocolors("web")["red"]
green <- brocolors("web")["green"]
blue <- brocolors("web")["blue"]

set.seed(20200310)
source("mtb_analysis.R")

pdf("../Figs/mcmc1.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
#for(i in 1:75)
#  points(rep(i,50),1:50,col=c(red,green)[out$output[i,mywh]+1],pch=16)
dev.off()

pdf("../Figs/mcmc2.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
points(rep(1,49),2:50,col=red,pch=16)
points(1,1)
arrows(5,1,2,1,lwd=2,len=0.1)
text(6,1,"?",font=2,cex=1.6)
dev.off()

pdf("../Figs/mcmc3.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
points(rep(1,48),3:50,col=red,pch=16)
points(1,1,col=c(red,green)[out$output[1,mywh[1]]+1],pch=16)
points(1,2)
arrows(5,2,2,2,lwd=2,len=0.1)
text(6,2,"?",font=2,cex=1.6)
dev.off()

pdf("../Figs/mcmc4.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
points(rep(1,47),4:50,col=red,pch=16)
points(rep(1,2),1:2,col=c(red,green)[out$output[1,mywh[1:2]]+1],pch=16)
points(1,3)
arrows(5,3,2,3,lwd=2,len=0.1)
text(6,3,"?",font=2,cex=1.6)
dev.off()

pdf("../Figs/mcmc5.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
points(rep(1,46),5:50,col=red,pch=16)
points(rep(1,3),1:3,col=c(red,green)[out$output[1,mywh[1:3]]+1],pch=16)
points(1,4)
arrows(5,4,2,4,lwd=2,len=0.1)
text(6,4,"?",font=2,cex=1.6)
dev.off()

pdf("../Figs/mcmc6.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
points(rep(1,45),6:50,col=red,pch=16)
points(rep(1,4),1:4,col=c(red,green)[out$output[1,mywh[1:4]]+1],pch=16)
points(1,5)
arrows(5,5,2,5,lwd=2,len=0.1)
text(6,5,"?",font=2,cex=1.6)
dev.off()

pdf("../Figs/mcmc7.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
points(rep(1,44),7:50,col=red,pch=16)
points(rep(1,5),1:5,col=c(red,green)[out$output[1,mywh[1:5]]+1],pch=16)
points(1,6)
arrows(5,6,2,6,lwd=2,len=0.1)
text(6,6,"?",font=2,cex=1.6)
dev.off()

pdf("../Figs/mcmc8.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
points(rep(1,50),1:50,col=c(red,green)[out$output[1,mywh]+1],pch=16)
dev.off()

pdf("../Figs/mcmc9.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
for(i in 1:2)
  points(rep(i,50),1:50,col=c(red,green)[out$output[i,mywh]+1],pch=16)
dev.off()

pdf("../Figs/mcmc10.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
for(i in 1:3)
  points(rep(i,50),1:50,col=c(red,green)[out$output[i,mywh]+1],pch=16)
dev.off()

pdf("../Figs/mcmc11.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
for(i in 1:4)
  points(rep(i,50),1:50,col=c(red,green)[out$output[i,mywh]+1],pch=16)
dev.off()

pdf("../Figs/mcmc12.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
for(i in 1:75)
  points(rep(i,50),1:50,col=c(red,green)[out$output[i,mywh]+1],pch=16)
dev.off()

pdf("../Figs/mcmc13.pdf", height=5.5,width=9.75)
par(mar=c(0.1,0.1,0.1,0.1))
plot(rep(0,50),1:50,col=red,pch=16,xlim=c(-0.5,75.5),xlab="",ylab="",
     xaxt="n",yaxt="n",bty="n")
for(i in 1:75)
  points(rep(i,50),1:50,col=c(red,green)[out$output[i,mywh]+1],pch=16)
rect(-0.6,21.5,75.6,22.5,lwd=2)
arrows(-20,22,-1,22,len=0.1,lwd=2,col=blue)
dev.off()
