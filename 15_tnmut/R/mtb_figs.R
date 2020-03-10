library(broman)
blue <- brocolors("web")["blue"]

source("mtb_analysis.R")

pdf("../Figs/mtb_mcmc.pdf",height=5.5,width=9.75)
par(las=1,mar=c(5.1,5.1,3.1,5.1))
plot(0:1000,c(3435,out$n.essential)/42.04,type="l",ylim=c(0,100),
     ylab="Percent essential", xlab="MCMC steps",lwd=2, yaxs="i")
abline(h=seq(0, 100, by=10), lty=2)
text(500,75,"Overall:  35%   (28 - 41%)",adj=c(0,0.5),cex=1.8,font=2,col=blue)
dev.off()

pdf("../Figs/mtb_hist.pdf", height=5.5,width=9.75)
par(las=1,mar=c(5.1,5.1,0.1,5.1))
hist(finalres$total/4204*100, xlab="", main="",ylab="",yaxt="n",
     breaks=seq(0,100,by=1),xaxt="n")
mtext(side=1,line=2, "Percent essential genes", cex=1.7)
x <- seq(0,100,by=10)
u <- par("usr")
segments(x,0,x,u[3]/2,lwd=2,xpd=TRUE)
text(x,1.3*u[3],as.character(x),cex=1.3,xpd=TRUE)
text(55, u[3]+diff(u[3:4])*0.8, "Based on every 50th of",
     cex=1.6, col=blue, adj=c(0,0.5))
text(60, u[3]+diff(u[3:4])*0.74, "500,000 Gibbs steps",
     cex=1.6, col=blue, adj=c(0,0.5))
dev.off()



pdf("../Figs/prob_essential.pdf", height=5.5,width=9.75)
par(las=1,mar=c(5.1,5.1,3.1,0.6))
p <- finalres[[2]]
p[p==0] <- p[p==0] + runif(sum(p==0),-0.01, 0.01)
plot(numTAs, p*100, ylim=c(-5,100), xlab="Number of TAs in proximal portion of gene",
     ylab="Probability gene is essential",type="n", yaxs="i")
abline(h=seq(0,100,by=10),lty=2)
points(numTAs, p*100, cex=0.6)
dev.off()
