source("colors.R")

pdf(file="../Figs/epistasis_bc.pdf", width=10, height=5.5, pointsize=12)

par(mar=c(6.1,5.1,5.1,1.1),las=1,mfrow=c(1,2))
plot(0,0,type="n",xlab="",xaxt="n",xlim=c(0.7,3),ylim=c(0,100),
     ylab="Ave. phenotype")
mtext(side=3,"Additive", cex=1.5, line=1.5)
points(c(1,2,1,2),c(10,40,60,90),cex=1.4,lwd=2)
  segments(c(1,1),c(10,60),c(2,2),c(40,90),lwd=2,col=color[1:2])
u <- par("usr")
segments(c(1,2),u[3],c(1,2),u[3]-diff(u[3:4])*0.04,xpd=TRUE)
text(c(1,2),u[3]-diff(u[3:4])*0.12,c("A","H"),cex=1.3,xpd=TRUE)
text(1.5,u[3]-diff(u[3:4])*0.24,"QTL 1",cex=1.4,xpd=TRUE)
text(2.2,c(40,90),c("A","H"),cex=1.3,col=color[1:2])
text(2.4,65,"QTL 2",cex=1.4,adj=0)


plot(0,0,type="n",xlab="",xaxt="n",xlim=c(0.7,3),ylim=c(0,100),
     ylab="Ave. phenotype")
mtext(side=3,"Epistatic", cex=1.5, line=1.5)
points(c(1,2,1,2),c(10,40,25,90),cex=1.4,lwd=2)
segments(c(1,1),c(10,25),c(2,2),c(40,90),lwd=2,col=color[1:2])
u <- par("usr")
segments(c(1,2),u[3],c(1,2),u[3]-diff(u[3:4])*0.04,xpd=TRUE)
text(c(1,2),u[3]-diff(u[3:4])*0.12,c("A","H"),cex=1.3,xpd=TRUE)
text(1.5,u[3]-diff(u[3:4])*0.24,"QTL 1",cex=1.4,xpd=TRUE)
text(2.2,c(40,90),c("A","H"),cex=1.3,col=color[1:2])
text(2.4,65,"QTL 2",cex=1.4,adj=0)
dev.off()



pdf(file="../Figs/epistasis_f2.pdf", width=10, height=5.5, pointsize=12)
par(mar=c(6.1,5.1,5.1,1.1),las=1,mfrow=c(1,2))
plot(0,0,type="n",xlab="",xaxt="n",xlim=c(0.7,3),ylim=c(0,100),
     ylab="Ave. phenotype")
mtext(side=3,"Additive", cex=1.5, line=1.5)
points(rep(c(1,1.5,2),3),c(10,20,60,20,30,70,40,50,90),cex=1.4,lwd=2)
segments(c(1,1,1),c(10,20,40),c(1.5,1.5,1.5),c(20,30,50),
         lwd=2,col=color[c(1,2,4)])
segments(c(2,2,2),c(60,70,90),c(1.5,1.5,1.5),c(20,30,50),
         lwd=2,col=color[c(1,2,4)])
u <- par("usr")
segments(c(1,1.5,2),u[3],c(1,1.5,2),u[3]-diff(u[3:4])*0.04,xpd=TRUE)
text(c(1,1.5,2),u[3]-diff(u[3:4])*0.12,c("A","H","B"),cex=1.3,xpd=TRUE)
text(1.5,u[3]-diff(u[3:4])*0.24,"QTL 1",cex=1.4,xpd=TRUE)
text(2.2,c(60,70,90),c("A","H","B"),cex=1.3,col=color[c(1,2,4)])
text(2.4,75,"QTL 2",cex=1.4,adj=0)


plot(0,0,type="n",xlab="",xaxt="n",xlim=c(0.7,3),ylim=c(0,100),
     ylab="Ave. phenotype")
mtext(side=3,"Epistatic", cex=1.5, line=1.5)
points(rep(c(1,1.5,2),3),c(10,20,0,20,30,30,40,50,90),cex=1.4,lwd=2)
segments(c(1,1,1),c(10,20,40),c(1.5,1.5,1.5),c(20,30,50),
         lwd=2,col=color[c(1,2,4)])
segments(c(2,2,2),c(0,30,90),c(1.5,1.5,1.5),c(20,30,50),
         lwd=2,col=color[c(1,2,4)])
u <- par("usr")
segments(c(1,1.5,2),u[3],c(1,1.5,2),u[3]-diff(u[3:4])*0.04,xpd=TRUE)
text(c(1,1.5,2),u[3]-diff(u[3:4])*0.12,c("A","H","B"),cex=1.3,xpd=TRUE)
text(1.5,u[3]-diff(u[3:4])*0.24,"QTL 1",cex=1.4,xpd=TRUE)
text(2.2,c(0,30,90),c("A","H","B"),cex=1.3,col=color[c(1,2,4)])
text(2.4,75,"QTL 2",cex=1.4,adj=0)
dev.off()
