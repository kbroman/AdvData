load("sims.RData")
library(broman)
blue <- brocolors("web")["blue"]
red <- brocolors("web")["red"]

# fig 7: simulation results
pdf("../Figs/sim_fig.pdf",height=5.5,width=9.75)
par(mar=c(3.6,4.1,2.1,2.1),mfrow=c(2,1),las=1)
yl <- range(c(sim.bias[1,]-2*sim.bias[2,],sim.bias[1,]+2*sim.bias[2,]),na.rm=TRUE)
yl <- c(-1.1,0.2)
plot(0,0,type="n",xaxt="n",ylim=yl,xlim=c(0.5,12.5),
     xlab="",ylab="Bias")
u <- par("usr")
first <- u[1];last <- u[2]
x <- seq(first+(last-first)/24,last-(last-first)/24,len=12)
segments(x,u[3],x,u[3]-diff(u[3:4])*0.05,xpd=TRUE)
text(x,u[3]-diff(u[3:4])*0.12,as.character(rep(c(750,1500,3000,4500),3)),xpd=TRUE)
abline(v=c(mean(x[4:5]),mean(x[8:9])))
abline(h=0,lty=2)
text(mean(x[2:3]),u[4]+diff(u[3:4])*0.08,"25% essential",xpd=TRUE)
text(mean(x[6:7]),u[4]+diff(u[3:4])*0.08,"50% essential",xpd=TRUE)
text(mean(x[10:11]),u[4]+diff(u[3:4])*0.08,"75% essential",xpd=TRUE)
segments(x,sim.bias[1,]-2*sim.bias[2,],x,sim.bias[1,]+2*sim.bias[2,],lwd=2)
segments(x-0.2,sim.bias[1,],x+0.2,sim.bias[1,],lwd=2)
segments(x-0.1,sim.bias[1,]-2*sim.bias[2,],x+0.1,sim.bias[1,]-2*sim.bias[2,],lwd=2)
segments(x-0.1,sim.bias[1,]+2*sim.bias[2,],x+0.1,sim.bias[1,]+2*sim.bias[2,],lwd=2)
text(u[1]-diff(u[1:2])*0.09,u[4]+diff(u[3:4])*0.13,"Bias",font=2,xpd=TRUE,cex=1.7,
     col=blue,adj=c(0,0.5))
title(xlab="No. mutants", mgp=c(1.8, 0, 0))

yl <- range(c(sim.cov[2:3,]),na.rm=TRUE)
#yl <- c(91.5,97.5)
plot(0,0,type="n",xaxt="n",ylim=yl,xlim=c(0.5,12.5),
     xlab="",ylab="Interval coverage (%)")
u <- par("usr")
first <- u[1];last <- u[2]
x <- seq(first+(last-first)/24,last-(last-first)/24,len=12)
segments(x,u[3],x,u[3]-diff(u[3:4])*0.05,xpd=TRUE)
text(x,u[3]-diff(u[3:4])*0.12,as.character(rep(c(750,1500,3000,4500),3)),xpd=TRUE)
abline(v=c(mean(x[4:5]),mean(x[8:9])))
abline(h=95,lty=2)
segments(x,sim.cov[2,],x,sim.cov[3,],lwd=2)
segments(x-0.2,sim.cov[1,],x+0.2,sim.cov[1,],lwd=2)
segments(x-0.1,sim.cov[2,],x+0.1,sim.cov[2,],lwd=2)
segments(x-0.1,sim.cov[3,],x+0.1,sim.cov[3,],lwd=2)
text(mean(x[2:3]),u[4]+diff(u[3:4])*0.08,"25% essential",xpd=TRUE)
text(mean(x[6:7]),u[4]+diff(u[3:4])*0.08,"50% essential",xpd=TRUE)
text(mean(x[10:11]),u[4]+diff(u[3:4])*0.08,"75% essential",xpd=TRUE)
text(u[1]-diff(u[1:2])*0.09,u[4]+diff(u[3:4])*0.17,"Coverage",font=2,xpd=TRUE,cex=1.7,
     col=blue,adj=c(0,0.5))
text(u[2],u[3]-diff(u[3:4])*0.32,"Based on 1000 simulations",col=red,xpd=TRUE,
     cex=1.5, adj=c(1,0.5))
title(xlab="No. mutants", mgp=c(1.8, 0, 0))
dev.off()
