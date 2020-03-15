library(qtl)
source("my.plot.scanone.R")
source("hyper_analysis.R")

pink <- rgb(255, 0.4*256-1, 255, maxColorValue=255)
blue <- rgb(0.4*256-1,  0.8*256-1, 255, maxColorValue=255)
green <- broman::brocolors("web")["green"]
purple <- broman::brocolors("web")["purple"]
orange <- broman::brocolors("web")["orange"]


pdf("../Figs/hyper_lod.pdf", height=5.5, width=9.75, pointsize=14)

layout(rbind(1,2),heights=c(5,4))
par(mar=c(0.6,4.1,0.6,0.1),xaxs="i",yaxs="i")
plot(out,ylim=c(0,8.5))
title(ylab="LOD score")
u <- par("usr")

li <- lodint(out,"4")
segments(li[2,2],li[2,3],40,li[2,3],lwd=2,col=pink,lty=2)
segments(li[1,2],li[1,3],40,li[1,3],lwd=2,col=pink,lty=2)
arrows(40,li[2,3]-0.2,40,li[1,3]+0.2,len=0.1,lwd=2,col=pink)
text(42,mean(c(li[2,3],li[1,3])),"1.5", cex=0.8, adj=c(0,0.5), col=pink)
u <- par("usr")
segments(li[1,2],u[3]+diff(u[3:4])*0.09,
         li[3,2],u[3]+diff(u[3:4])*0.09,col=pink,lwd=2)
for(i in c(1,3))
  segments(li[i,2],u[3]+diff(u[3:4])*0.07,
           li[i,2],u[3]+diff(u[3:4])*0.11,col=pink,lwd=2)
text(34,u[3]+diff(u[3:4])*0.09, "1.5-LOD support interval", adj=c(0,0.5),
     cex=0.8, col=pink)

axis(side=1)
mtext(side=1, xpd=TRUE, line=2.5, "Position (cM)")
dev.off()



pdf("../Figs/hyper_bayes.pdf", height=5.5, width=9.75, pointsize=14)

layout(rbind(1,2),heights=c(5,4))
par(mar=c(0.6,4.1,0.6,0.1),xaxs="i",yaxs="i")
temp <- out
temp[,3] <- 10^temp[,3]
names(temp)[3] <- ""
plot(temp, yaxt="n", ylim=c(0,1.29e8))
mtext(side=2, line=1, expression(10^LOD), las=1)
u <- par("usr")

axis(side=1)
mtext(side=1, xpd=TRUE, line=2.5, "Position (cM)")

li <- bayesint(out, "4")

segments(li[1,2],u[3]+diff(u[3:4])*0.09,
         li[3,2],u[3]+diff(u[3:4])*0.09,col=blue,lwd=2)
for(i in c(1,3))
  segments(li[i,2],u[3]+diff(u[3:4])*0.07,
           li[i,2],u[3]+diff(u[3:4])*0.11,col=blue,lwd=2)
text(34,u[3]+diff(u[3:4])*0.09+0.75, "95% Bayes credible interval", adj=c(0,0.5),
     cex=0.8, col=blue)


dev.off()



pdf("../Figs/hyper_boot.pdf", height=5.5, width=9.75, pointsize=14)

layout(rbind(1,2),heights=c(5,4))
par(mar=c(0.6,4.1,0.6,0.1),xaxs="i",yaxs="i")
load("bootlod.RData")
z <- c(16,20,24)
plot(out, lty=2,
     ylim=c(0,8.5))
title(ylab="LOD score")

u <- par("usr")

for(i in 1:3)
  lines(out[,2], bootlod[,z[i]], col=c(orange,green,purple)[i], lwd=2)


par(mar=c(5.1,4.1,0.1,0.1),xaxs="i",yaxs="i")
hist(boot[,1],breaks=seq(-0.125,74.375,by=0.25),xlim=c(0,74.3),bty="o",
     xlab="Position (cM)",ylab="",yaxt="n",ylim=c(0,6705),main="")

u <- par("usr")
abline(v=c(0,74.3))
u <- par("usr")
abline(h=u[4], v=u[1:2])
qu <- quantile(boot[,1],c(0.025,0.975))
segments(qu[1],u[4]-diff(u[3:4])*0.04*2,
         qu[2],u[4]-diff(u[3:4])*0.04*2,lwd=2)
for(i in 1:2)
  segments(qu[i],u[4]-diff(u[3:4])*0.02*2,
           qu[i],u[4]-diff(u[3:4])*0.06*2,lwd=2)
text(34,u[4]-diff(u[3:4])*0.04*2, "95% bootstrap confidence interval", adj=c(0,0.5),
     cex=0.8)

dev.off()
