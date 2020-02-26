load("genome_for_talk.RData")

color <- broman::brocolors("web")[c("blue", "red", "green", "yellow")]

color <- color[c(4,1,4,4,4,2,4,3)]
######################################################################
# number of generations to complete achieve 99% fixation
######################################################################
pdf("../Figs/ngen99.pdf", width=9.75, height=5,
    pointsize=16, onefile=TRUE)
par(mar=c(4.6,0.1,0.1,0.1),las=1)
plot(0,0,type="n",xlim=c(-0.5,40.5),ylim=c(0,max(ngen99tab)),
     xlab="No. generations", ylab="", yaxt="n")
x <- 0:46
for(i in c(2,6,8)) {
  if(i==2 || i==6)
    y <- c(0,ngen99tab[,i],0,0)
  else
    y <- c(0,0,0,ngen99tab[,i])

  segments(x-0.5,y,x+0.5,y,lwd=2,col=color[i])
  segments(x[-length(x)]+0.5,y[-1],x[-length(x)]+0.5,y[-length(x)],lwd=2,
           col=color[i])
}
u <- par("usr")
legend(u[2],u[4],xjust=1,c("2-way selfing","2-way sib-mating",
                   "8-way sib-mating"),col=color[c(2,6,8)],lwd=2,cex=1.2)
text(8.5,2800,"mean = 8.0",cex=1.2,col=color[2],font=2,adj=c(0,0.5))
text(19.5,1250,"mean = 23.5",cex=1.2,col=color[6],font=2,adj=c(1,0.5))
text(28.5,1000,"mean = 26.7",cex=1.2,col=color[8],font=2,adj=c(0,0.5))
dev.off()



######################################################################
# number of generations to complete achieve 100% fixation
######################################################################
pdf("../Figs/ngen_fix.pdf", width=9.75, height=5,
    pointsize=16, onefile=TRUE)
par(mar=c(4.6,0.1,0.1,0.1),las=1)
plot(0,0,type="n",xlim=c(-0.5,62.5),ylim=c(0,2250),
     xlab="No. generations", ylab="", yaxt="n")
x <- 0:80
for(i in c(2,6,8)) {
  if(i==2 || i==6)
    y <- c(0,ngentab[,i],0,0)
  else
    y <- c(0,0,0,ngentab[,i])

  segments(x-0.5,y,x+0.5,y,lwd=2,col=color[i])
  segments(x[-length(x)]+0.5,y[-1],x[-length(x)]+0.5,y[-length(x)],lwd=2,
           col=color[i])
}
u <- par("usr")
legend(u[2],u[4],xjust=1,c("2-way selfing","2-way sib-mating",
                   "8-way sib-mating"),col=color[c(2,6,8)],lwd=2,cex=1.2)
text(10.5,1800,"mean = 10.5",cex=1.2,col=color[2],font=2,adj=c(0,0.5))
text(29,725,"mean = 35.6",cex=1.2,col=color[6],font=2,adj=c(1,0.5))
text(42.5,500,"mean = 38.9",cex=1.2,col=color[8],font=2,adj=c(0,0.5))
dev.off()

######################################################################
# proportion heterozygous
######################################################################
pdf("../Figs/prophet.pdf", width=9.75, height=5,
    pointsize=16, onefile=TRUE)
par(mar=c(4.6,4.1,0.1,0.1),las=1)
plot(0,0,type="n",xlim=c(-0.5,40.5),ylim=c(0,20),
     xlab="No. generations", ylab="Percent not yet fixed")
x <- 1:81
for(i in c(2,6,8)) {
  if(i==2 || i==6)  {
    y <- c(1,aveprophet[[i]])*100
    z <- c(1,prophet95[[i]])*100
  }
  else  {
    y <- c(1,1,1,aveprophet[[i]])*100
    z <- c(1,1,1,prophet95[[i]])*100
  }

  if(length(y) < length(x)) y <- c(y,rep(0,length(x)-length(y)))
  if(length(z) < length(x)) z <- c(z,rep(0,length(x)-length(z)))

  lines(x,y,lwd=2,col=color[i])
  lines(x,z,lwd=2,col=color[i], lty = 2)
}
u <- par("usr")
legend(u[2],u[4],xjust=1,c("2-way selfing","2-way sib-mating",
                   "8-way sib-mating","","Average","95th percentile"),
       col=c(color[c(2,6,8)],"white","black","black"),lwd=2,cex=1.2,
       lty=c(1,1,1,1,1,2))
dev.off()

######################################################################
# distribution of no. segments
######################################################################
pdf("../Figs/nseg.pdf", width=9.75, height=5,
    pointsize=16, onefile=TRUE)
par(mar=c(4.6,0.1,0.1,0.1),las=1)
plot(0,0,type="n",xlim=c(29.5,183.5),ylim=c(0,741),
     xlab="No. segments", ylab="", yaxt="n")
x <- 30:183
for(i in c(2,6,8)) {
#  j <- 2 - i %% 2
  segments(x-0.5,nsegtab[,i],x+0.5,nsegtab[,i],lwd=2,col=color[i])#,lty=j)
  segments(x[-length(x)]+0.5,nsegtab[-1,i],x[-length(x)]+0.5,
           nsegtab[-length(x),i],lwd=2, col=color[i])#,lty=j)
}
u <- par("usr")
legend(u[2],u[4],xjust=1,c("2-way selfing","2-way sib-mating",
                   "8-way sib-mating"),col=color[c(2,6,8)],lwd=2,cex=1.2)
text(57,700,"mean = 52.7",cex=1.2,col=color[2],font=2,adj=c(0,0.5))
text(91,430,"mean = 85.3",cex=1.2,col=color[6],font=2,adj=c(0,0.5))
text(139,380,"mean = 134.4",cex=1.2,col=color[8],font=2,adj=c(0,0.5))
dev.off()

######################################################################
# distribution of no. breakpoints
######################################################################
pdf("../Figs/nbrks.pdf", width=9.75, height=5,
    pointsize=16, onefile=TRUE)
par(mar=c(4.6,0.1,0.1,0.1),las=1)
plot(0,0,type="n",xlim=c(29.5-20,183.5-20),ylim=c(0,741),
     xlab="No. breakpoints", ylab="", yaxt="n")
x <- (30:183)-20
for(i in c(2,6,8)) {
#  j <- 2 - i %% 2
  segments(x-0.5,nsegtab[,i],x+0.5,nsegtab[,i],lwd=2,col=color[i])#,lty=j)
  segments(x[-length(x)]+0.5,nsegtab[-1,i],x[-length(x)]+0.5,
           nsegtab[-length(x),i],lwd=2, col=color[i])#,lty=j)
}
u <- par("usr")
legend(u[2],u[4],xjust=1,c("2-way selfing","2-way sib-mating",
                   "8-way sib-mating"),col=color[c(2,6,8)],lwd=2,cex=1.2)
text(37,700,"mean = 32.7",cex=1.2,col=color[2],font=2,adj=c(0,0.5))
text(71,430,"mean = 65.3",cex=1.2,col=color[6],font=2,adj=c(0,0.5))
text(119,380,"mean = 114.4",cex=1.2,col=color[8],font=2,adj=c(0,0.5))
dev.off()



######################################################################
# distribution of segment lengths
######################################################################
pdf("../Figs/lseg.pdf", width=9.75, height=5,
    pointsize=16, onefile=TRUE)
par(mar=c(4.6,0.1,0.1,0.1),las=1)
plot(0,0,type="n",xlim=c(0,127),ylim=c(0,11500),
     xlab="Segment lengths (cM)", ylab="", yaxt="n")
x <- seq(0.1,127,by=0.1);x <- x - x[1]/2
for(i in c(2,6,8)) {
  segments(x-x[1],lsegtab[,i],x+x[1],lsegtab[,i],lwd=2,col=color[i])#,lty=j)
  segments(x[-length(x)]+x[1],lsegtab[-1,i],x[-length(x)]+x[1],
           lsegtab[-length(x),i],lwd=2, col=color[i])#,lty=j)
}
u <- par("usr")
legend(u[2],u[4],xjust=1,c("2-way selfing","2-way sib-mating",
                   "8-way sib-mating"),col=color[c(2,6,8)],lwd=2,cex=1.2)
text(0,100,"median = 23.7 cM",cex=1.2,col=color[2],font=2,adj=c(0,0.5))
text(15,4000,"median = 12.9 cM",cex=1.2,col=color[6],font=2,adj=c(0,0.5))
text(5,10400,"median = 8.5 cM",cex=1.2,col=color[8],font=2,adj=c(0,0.5))
arrows(90,5000,82,4000,lwd=2,len=0.1)
text(91,5200,"Two chromosomes",adj=c(0,0.5),font=2)
arrows(106.5,3800,98,2800,lwd=2,len=0.1)
text(107.5,4000,"X chromosome",adj=c(0,0.5),font=2)
dev.off()

######################################################################
# probability chr is intact
######################################################################
pdf("../Figs/probintact.pdf", width=9.75, height=5,
    pointsize=16, onefile=TRUE)
par(mar=c(4.6,4.1,0.1,0.1),las=1)
plot(0,0,type="n",xlim=c(0,127),ylim=c(0,1),
     xlab="Length of segment (cM)", ylab="Probability segment inherited intact")
for(i in c(2,6,8)) {
  y <- sort(firstonc1[[i]])
  n <- sum(y==127)
  x <- (10000:n)/10000
  y <- c(y[y<127],127)

  lines(y,x,lwd=2,col=color[i])
}
u <- par("usr")
legend(u[2],u[4],xjust=1,c("2-way selfing","2-way sib-mating",
                   "8-way sib-mating"),col=color[c(2,6,8)],lwd=2,cex=1.2)
dev.off()

######################################################################
# length of smallest segment
######################################################################
pdf("../Figs/lsmseg.pdf", width=9.75, height=5,
    pointsize=16, onefile=TRUE)
par(mar=c(4.6,0.1,0.1,0.1),las=1)
plot(0,0,type="n",xlim=c(0,2.50),ylim=c(0,2095),
     xlab="Length of smallest segment (cM)", ylab="", yaxt="n")
x <- seq(0.02,8.63,by=0.02);x <- x - x[1]/2
for(i in c(2,6,8)) {
  segments(x-x[1],lsmsegtab[,i],x+x[1],lsmsegtab[,i],lwd=2,col=color[i])#,lty=j)
  segments(x[-length(x)]+x[1],lsmsegtab[-1,i],x[-length(x)]+x[1],
           lsmsegtab[-length(x),i],lwd=2, col=color[i])#,lty=j)
}
u <- par("usr")
legend(u[2],u[4],xjust=1,c("2-way selfing","2-way sib-mating",
                   "8-way sib-mating"),col=color[c(2,6,8)],lwd=2,cex=1.2)
text(1,200,"95th %ile = 2.2 cM",cex=1.2,col=color[2],font=2,adj=c(0,0.5))
text(0.25,400,"95th %ile = 0.58 cM",cex=1.2,col=color[6],font=2,adj=c(0,0.5))
text(0.05,2000,"95th %ile = 0.26 cM",cex=1.2,col=color[8],font=2,adj=c(0,0.5))
dev.off()

######################################################################
# no. segments < 1 cM
######################################################################
pdf("../Figs/nsegsm1.pdf", width=9.75, height=5,
    pointsize=16, onefile=TRUE)
par(mar=c(4.6,0.1,0.1,0.1),las=1)
plot(0,0,type="n",xlim=c(-0.5,25.5),ylim=c(0,3392),
     xlab="No. segments < 1 cM", ylab="", yaxt="n")
x <- 0:31
for(i in c(2,6,8)) {
  segments(x-0.5,nsegsm1tab[,i],x+0.5,nsegsm1tab[,i],lwd=2,col=color[i])
  segments(x[-length(x)]+0.5,nsegsm1tab[-1,i],x[-length(x)]+0.5,
           nsegsm1tab[-length(x),i],lwd=2, col=color[i])
}
u <- par("usr")
legend(u[2],u[4],xjust=1,c("2-way selfing","2-way sib-mating",
                   "8-way sib-mating"),col=color[c(2,6,8)],lwd=2,cex=1.2)
text(2,3200,"mean = 1.4",cex=1.2,col=color[2],font=2,adj=c(0,0.5))
text(7,1400,"mean = 5.2",cex=1.2,col=color[6],font=2,adj=c(0,0.5))
text(15,800,"mean = 11.2",cex=1.2,col=color[8],font=2,adj=c(0,0.5))
dev.off()
