######################################################################
# figure 4: symmetry in 3pts in 8-way RILs
######################################################################

attach("ril_3pt.RData")
source("study_probs.R")

color <- c("black",rgb(102,203,254,maxColorValue=255),
           rgb(254,102,254,maxColorValue=255),
           rgb(102,254,102,maxColorValue=255))

rI <- sym8AI[,1]
rNI <- sym8ANI[,1]
tempI <- sym8AI
tempNI <- sym8ANI
tempNI[,4:6] <- sym8ANI[,4:6] / (sym8ANI[,4]+sym8ANI[,5]*2+sym8ANI[,6]*4)
tempI[,4:6] <- sym8AI[,4:6] / (sym8AI[,4]+sym8AI[,5]*2+sym8AI[,6]*4)

pdf("../Figs/3pt_symmetry.pdf", width=9.75, height=5.3,
    pointsize=16, onefile=TRUE)
par(mar=c(5.1,4.1,0.6,0.6),las=1)
plot(0,0,type="n",ylab="",xlab="",
     xlim=c(0,0.5),ylim=c(0,0.2))#ylim=c(0,1/8))
#lines(rNI,sym8ANI[,4],lwd=2,col=color[1])
#lines(rNI,sym8ANI[,5],lwd=2,col=color[2])
#lines(rNI,sym8ANI[,6],lwd=2,col=color[3])
#lines(rI,sym8AI[,4],lwd=2,lty=2,col=color[1])
#lines(rI,sym8AI[,5],lwd=2,col=color[2],lty=2)
#lines(rI,sym8AI[,6],lwd=2,col=color[3],lty=2)

abline(h=1/7,lty=3,col="gray")

lines(rNI,tempNI[,4],lwd=2,col=color[1])
lines(rNI,tempNI[,5],lwd=2,col=color[2])
lines(rNI,tempNI[,6],lwd=2,col=color[3])
lines(rI,tempI[,4],lwd=2,lty=2,col=color[1])
lines(rI,tempI[,5],lwd=2,col=color[2],lty=2)
lines(rI,tempI[,6],lwd=2,col=color[3],lty=2)

#mtext(side=2,"Pr(AxA | A-A)",line=3.1,las=0)
mtext(side=2,expression(paste("Pr(",M[2],"=x | ", M[1], "=A, ", M[2] !=A, ", ", M[3], "=A)")),line=3.1,las=0)
mtext(side=1,"recombination fraction",line=2.5)

u <- par("usr")
legend(u[2],u[3],xjust=1,yjust=0,
       c(paste("x =",LETTERS[c(2,3,5)]),"","No interference","Positive interference"),
       lwd=2,col=c(color[1],color[2],color[3],"white","black","black"),
       lty=c(1,1,1,1,1,2))

dev.off()
