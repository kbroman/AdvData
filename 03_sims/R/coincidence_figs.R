attach("coincidence.RData.gz")

r <- c(seq(0,0.0025,len=26)[-c(1:3,26)],seq(0,0.5,length=201)[-c(1,201)])

color <- c(rgb(102,203,254,maxColorValue=255),
           rgb(254,102,254,maxColorValue=255),
           rgb(102,254,102,maxColorValue=255),
           rgb(254,203,102,maxColorValue=255)) # yellow

pdf("../Figs/coincidence_ni.pdf", width=9.75, height=5.5,
      pointsize=16, onefile=TRUE)
par(mar=c(5.1,4.1,2.1,1.1),las=1)
plot(c(r,0.5),c(Cself[,1],1), ylim=c(0,2),xlab="r",ylab="Coincidence",
     las=1,type="l",lwd=2,col=color[1], xaxs="i", yaxs="i")
#title(main="No interference", col.main=color[4])

abline(h=1,lty=3)
segments(min(r),1,0.5,1,lwd=2)
lines(c(r,0.5),c(Csib[,1],1),lwd=2,col=color[2])
u <- par("usr")
legend(u[2], u[4], lty=1, lwd=2, xjust=1, col=c("black",color[1],color[2]),
       legend=c("Meiosis","RILs by selfing","RILs by sib-mating"))
dev.off()



pdf("../Figs/coincidence_i.pdf", width=9.75, height=5.5,
      pointsize=16, onefile=TRUE)
par(mar=c(5.1,4.1,2.1,1.1),las=1)
r <- c(seq(0,0.0025,len=26)[-c(1:3,26)],seq(0,0.5,length=201)[-c(1,201)])
Rsib <- 4*r/(1+6*r)
Rself <- 2*r/(1+2*r)
plot(c(r,0.5),c(Cself[,1],1),ylim=c(0,2),xlab="r",ylab="Coincidence",las=1,
     type="l",lwd=2,col=color[1], xaxs="i", yaxs="i")
abline(h=1,lty=3)
#segments(min(r),1,0.5,1,lwd=2,col="#000070")
segments(min(r),1,0.5,1,lwd=2)
lines(c(r,0.5),c(Csib[,1],1),lwd=2,col=color[2])
lines(c(r,0.5),c(Cf1[,"11.3"],1),lwd=2,col="black",lty=2)
lines(c(r,0.5),c(Cself[,"11.3"],1),lwd=2,col=color[1],lty=2)
lines(c(r,0.5),c(Csib[,"11.3"],1),lwd=2,col=color[2],lty=2)
u <- par("usr")
legend(u[2], u[4], lty=c(1,1,1,1,1,2), lwd=2, xjust=1,
       col=c("black",color[1],color[2], "white","black","black"),
       legend=c("Meiosis","RILs by selfing","RILs by sib-mating","",
         "No interference", "Mouse interference"), cex=0.9)
dev.off()


######################################################################


attach("coinc_ri8.RData.gz")
attach("coinc_8way_numerically.RData")
oNI <- c(R8ANI[length(R8ANI)],co8ANI[length(co8ANI)])
oI <- c(R8AI[length(R8AI)],co8AI[length(co8AI)])
coinc <- co8ANI
coincI <- co8AI
R <- R8ANI
RI <- R8AI
coinc <- coinc[order(R)]
coincI <- coincI[order(RI)]
R <- sort(R)
RI <- sort(RI)

pdf("../Figs/coincidence_8way.pdf", width=9.75, height=5.5,
      pointsize=16, onefile=TRUE)
par(mar=c(5.1,4.1,2.1,1.1),las=1)
r <- c(seq(0,0.0025,len=26)[-c(1:3,26)],seq(0,0.5,length=201)[-c(1,201)])
Rsib <- 4*r/(1+6*r)
Rself <- 2*r/(1+2*r)
plot(c(r,0.5),c(Cself[,1],1),ylim=c(0,2),xlab="r",ylab="Coincidence",las=1,
     type="l",lwd=2,col=color[1],xlim=c(0,0.5), xaxs="i", yaxs="i")
abline(h=1,lty=3)
segments(min(r),1,0.5,1,lwd=2)
lines(c(r,0.5),c(Csib[,1],1),lwd=2,col=color[2])
lines(c(r,0.5),c(Cf1[,"11.3"],1),lwd=2,col="black",lty=2)
lines(c(r,0.5),c(Cself[,"11.3"],1),lwd=2,col=color[1],lty=2)
lines(c(r,0.5),c(Csib[,"11.3"],1),lwd=2,col=color[2],lty=2)

rt <- R/(7-6*R)
rit <- RI/(7-6*RI)

lines(rt,coinc,col=color[3],lwd=2)
lines(rit,coincI,col=color[3],lwd=2,lty=2)
u <- par("usr")
legend(u[2], u[4], lty=c(1,1,1,1,1,1,2), lwd=2, xjust=1,
       col=c("black",color[1],color[2], color[3],"white","black","black"),
       legend=c("Meiosis","2-way RILs by selfing","2-way RILs by sib-mating","8-way RILs by sib-mating",
         "","No interference","Mouse interference"),cex=0.7)
dev.off()
