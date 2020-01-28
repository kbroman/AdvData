######################################################################
# figure 5: Markov property for 3pts in 8-way RILs
######################################################################

attach("ril_3pt.RData")
source("study_probs.R")
mark8AI[,-(1:2)] <- log2(mark8AI[,-(1:2)])
mark8ANI[,-(1:2)] <- log2(mark8ANI[,-(1:2)])

color <- c("black",
           rgb(102,203,254,maxColorValue=255),
           rgb(254,  0,128,maxColorValue=255),
           rgb(203,102,254,maxColorValue=255),
           rgb(102,254,102,maxColorValue=255),
           rgb(128,  0,128,maxColorValue=255),
           rgb(254,203,102,maxColorValue=255),
           rgb(  0,128,128,maxColorValue=255),
           rgb(102,102,102,maxColorValue=255))


pdf("../Figs/3pt_markov1.pdf", width=9.75, height=5.3,
    pointsize=16, onefile=TRUE)
par(mar=c(5.1,4.1,0.6,0.6),las=1)
plot(0,0,type="n",ylab="",xlab="",
     xlim=c(0,0.5),ylim=c(-4,2.64))
mtext(side=2,expression(paste(log[2]," probability ratio")),
      line=2.3,las=0)
mtext(side=1,"recombination fraction",line=2.5)
mtext(side=3,"Pr(xAA | xA-) / Pr(-AA | -A-)",line=0.5)
mtext(side=3,adj=0,line=0.5,"A",font=2)
wh <- c(1:3,5)
for(i in 4:1) {
  lines(mark8AI[c(1,i+2)],lwd=2,lty=2,col=color[wh[i]])
  lines(mark8ANI[c(1,i+2)],lwd=2,col=color[wh[i]])
}
u <- par("usr")
legend(u[2],u[3],yjust=0,xjust=1,paste("x = ",LETTERS[wh]),
       lwd=2,col=color[wh],y.intersp=0.9)
dev.off()

pdf("../Figs/3pt_markov2.pdf", width=9.75, height=5.3,
    pointsize=16, onefile=TRUE)
par(mar=c(5.1,4.1,0.6,0.6),las=1)
plot(0,0,type="n",ylab="",xlab="",
     xlim=c(0,0.5),ylim=c(-4,2.64))
#abline(h=0,lty=3,col="gray45",lwd=2)
mtext(side=2,expression(paste(log[2]," probability ratio")),line=2.3,las=0)
mtext(side=1,"recombination fraction",line=2.5)
mtext(side=3,"Pr(xBA | xB-) / Pr(-BA | -B-)",line=0.5)
mtext(side=3,adj=0,line=0.5,"B",font=2)
wh <- c(1:3,5)
for(i in 4:1) {
  lines(mark8AI[c(1,i+6)],lwd=2,lty=2,col=color[wh[i]])
  lines(mark8ANI[c(1,i+6)],lwd=2,col=color[wh[i]])
}
u <- par("usr")
legend(u[2],u[3],yjust=0,xjust=1,paste("x = ",LETTERS[wh]),
       lwd=2,col=color[wh],y.intersp=0.9)
dev.off()

pdf("../Figs/3pt_markov3.pdf", width=9.75, height=5.3,
    pointsize=16, onefile=TRUE)
par(mar=c(5.1,4.1,0.6,0.6),las=1)
plot(0,0,type="n",ylab="",xlab="",
     xlim=c(0,0.5),ylim=c(-4,2.64))
#abline(h=0,lty=3,col="gray45",lwd=2)
mtext(side=2,expression(paste(log[2]," probability ratio")),line=2.3,las=0)
mtext(side=1,"recombination fraction",line=2.5)
mtext(side=3,"Pr(xCA | xC-) / Pr(-CA | -C-)",line=0.5)
mtext(side=3,adj=0,line=0.5,"C",font=2)
wh <- 1:5
for(i in 5:1) {
  lines(mark8AI[c(1,i+10)],lwd=2,lty=2,col=color[wh[i]])
  lines(mark8ANI[c(1,i+10)],lwd=2,col=color[wh[i]])
}
u <- par("usr")
let <- LETTERS[wh]
let[3] <- paste(let[3:4],collapse="/")
let <- let[-4]
legend(u[2],u[3],yjust=0,xjust=1,paste("x = ",let),
       lwd=2,col=color[wh[-4]],ncol=1,y.intersp=0.9)
dev.off()

pdf("../Figs/3pt_markov4.pdf", width=9.75, height=5.3,
    pointsize=16, onefile=TRUE)
par(mar=c(5.1,4.1,0.6,0.6),las=1)
plot(0,0,type="n",ylab="",xlab="",
     xlim=c(0,0.5),ylim=c(-4,2.64))
#abline(h=0,lty=3,col="gray45",lwd=2)
mtext(side=2,expression(paste(log[2]," probability ratio")),line=2.3,las=0)
mtext(side=1,"recombination fraction",line=2.5)
mtext(side=3,"Pr(xEA | xE-) / Pr(-EA | -E-)",line=0.5)
mtext(side=3,adj=0,line=0.5,"D",font=2)
wh <- c(1:3,5:7)
for(i in 6:1) {
  lines(mark8AI[c(1,i+15)],lwd=2,lty=2,col=color[wh[i]])
  lines(mark8ANI[c(1,i+15)],lwd=2,col=color[wh[i]])
}
u <- par("usr")
let <- LETTERS[wh]
let[4] <- paste(let[4:5],collapse="/")
let <- let[-5]
legend(u[2],u[3],yjust=0,xjust=1,paste("x = ",let),
       lwd=2,col=color[wh[-5]],ncol=1,y.intersp=0.9)
dev.off()
rm(list=ls())
detach(2)
