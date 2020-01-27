##############################
# RI lines
##############################
color <- broman::brocolors("crayons")[c("Cornflower", "Blush")]

file <-  "_cache/for_riself_fig.RData"
if(file.exists(file)) {
    load(file)
} else {
    source("meiosis_func.R")
    f1 <- create.par(100,c(1,2))
    set.seed(3312219)
    f2 <- vector("list",4)
    for(i in 1:4) f2[[i]] <- cross(f1,f1,m=10,obl=TRUE)
    f3 <- vector("list",4)
    for(i in 1:4)
        f3[[i]] <- cross(f2[[i]],f2[[i]],m=10,obl=TRUE)
    f4 <- vector("list",4)
    for(i in 1:4)
        f4[[i]] <- cross(f3[[i]],f3[[i]],m=10,obl=TRUE)
    temp <- f4
    my.ri8 <- vector("list",4)
    for(j in 1:20) {
        for(i in 1:4) {
            my.ri8[[i]] <- cross(temp[[i]],temp[[i]],m=10,obl=TRUE)
        }
        temp <- my.ri8
    }
    save(f1,f2,f3,f4,my.ri8,file=file)
}


#bitmap(file="../Figs/riself.bmp", width=9, height=5, res=288,
#       pointsize=14)
pdf("../Figs/riself.pdf", width=9.75, height=5.5, pointsize=16, onefile=TRUE)
par(mar=rep(0.1,4),las=1,bty="n")

plot(0,0,xlim=c(-30,864),ylim=c(30,480),xaxt="n",yaxt="n",xlab="",ylab="",type="n")
mtext("Selfing",side=3,line=0.8,cex=1.3)
u <- par("usr")
text(u[1],u[4]+diff(u[3:4])*0.05,"A",font=2,cex=1.3,xpd=TRUE,adj=c(0,0.5))

rect(c(306,326),c(480,480),c(312,332),c(440,440),col=color[1],border=color[1], lend=1, ljoin=1)
rect(c(532,552),c(480,480),c(538,558),c(440,440),col=color[2],border=color[2], lend=1, ljoin=1)
points(432,465,pch=4)
arrows(432,450,432,410,len=0.07)
text(300-25,460,expression(P[1]),adj=c(1,0.5),cex=1.2)
text(564+25,460,expression(P[2]),adj=c(0,0.5),cex=1.2)

rect(419,400,425,360,col=color[1],border=color[1], lend=1, ljoin=1)
rect(439,400,445,360,col=color[2],border=color[2], lend=1, ljoin=1)

text(413-25,380,expression(F[1]),adj=c(1,0.5),cex=1.2)
segments(432,360,432,350)
segments(92,350,772,350)
arrows(c(319,545,92,772),rep(350,4),c(319,545,92,772),rep(330,4),len=0.07)


xloc <- 92
mult <- 40/f2[[1]]$mat[1,ncol(f2[[1]]$mat)]
for(i in 1:4) {
  rect(xloc-13,320,xloc-7,280,col=color[1],border=color[1], lend=1, ljoin=1)
  rect(xloc+7,320,xloc+13,280,col=color[1],border=color[1], lend=1, ljoin=1)

  f2m <- f2[[i]]$mat
  for(j in 2:ncol(f2m)) {
    if(f2m[2,j]==2)
      rect(xloc-13,280+f2m[1,j]*mult,xloc-7,280+f2m[1,j-1]*mult,col=color[2],border=color[2], lend=1, ljoin=1)
  }

  f2m <- f2[[i]]$pat
  for(j in 2:ncol(f2m)) {
    if(f2m[2,j]==2)
      rect(xloc+7,280+f2m[1,j]*mult,xloc+13,280+f2m[1,j-1]*mult,col=color[2],border=color[2], lend=1, ljoin=1)
  }

  xloc <- xloc+227
}
text(38-25,300,expression(F[2]),adj=c(1,0.5),cex=1.2)
arrows(c(319,545,92,772),rep(270,4),c(319,545,92,772),rep(250,4),len=0.07)



xloc <- 92
mult <- 40/f2[[1]]$mat[1,ncol(f2[[1]]$mat)]
for(i in 1:4) {
  rect(xloc-13,240,xloc-7,200,col=color[1],border=color[1], lend=1, ljoin=1)
  rect(xloc+7,240,xloc+13,200,col=color[1],border=color[1], lend=1, ljoin=1)

  f2m <- f3[[i]]$mat
  for(j in 2:ncol(f2m)) {
    if(f2m[2,j]==2)
      rect(xloc-13,200+f2m[1,j]*mult,xloc-7,200+f2m[1,j-1]*mult,col=color[2],border=color[2], lend=1, ljoin=1)
  }

  f2m <- f3[[i]]$pat
  for(j in 2:ncol(f2m)) {
    if(f2m[2,j]==2)
      rect(xloc+7,200+f2m[1,j]*mult,xloc+13,200+f2m[1,j-1]*mult,col=color[2],border=color[2], lend=1, ljoin=1)
  }

  xloc <- xloc+227
}
text(38-25,220,expression(F[3]),adj=c(1,0.5),cex=1.2)
arrows(c(319,545,92,772),rep(190,4),c(319,545,92,772),rep(170,4),len=0.07)


xloc <- 92
mult <- 40/f2[[1]]$mat[1,ncol(f2[[1]]$mat)]
for(i in 1:4) {
  rect(xloc-13,160,xloc-7,120,col=color[1],border=color[1], lend=1, ljoin=1)
  rect(xloc+7,160,xloc+13,120,col=color[1],border=color[1], lend=1, ljoin=1)

  f2m <- f4[[i]]$mat
  for(j in 2:ncol(f2m)) {
    if(f2m[2,j]==2)
      rect(xloc-13,120+f2m[1,j]*mult,xloc-7,120+f2m[1,j-1]*mult,col=color[2],border=color[2], lend=1, ljoin=1)
  }

  f2m <- f4[[i]]$pat
  for(j in 2:ncol(f2m)) {
    if(f2m[2,j]==2)
      rect(xloc+7,120+f2m[1,j]*mult,xloc+13,120+f2m[1,j-1]*mult,col=color[2],border=color[2], lend=1, ljoin=1)
  }

#  points(xloc+54,130,pch=4,cex=1.3)


  xloc <- xloc+227
}
text(38-25,140,expression(F[4]),adj=c(1,0.5),cex=1.2)
arrows(c(319,545,92,772),rep(85,4),c(319,545,92,772),rep(80,4),len=0.07)
arrows(c(319,545,92,772),rep(110,4),c(319,545,92,772),rep(80,4),len=0.07,lty=2)

xloc <- 38
a <- 70-40
mult <- (70-a)/f2[[1]]$mat[1,ncol(f2[[1]]$mat)]
for(i in 1:4) {
  rect(xloc+54-13,70,xloc+54-7,a,col=color[1],border=color[1], lend=1, ljoin=1)
  rect(xloc+54+7,70,xloc+54+13,a,col=color[1],border=color[1], lend=1, ljoin=1)

  my.ri8m <- my.ri8[[i]]$mat
  for(j in 2:ncol(my.ri8m)) {
    if(my.ri8m[2,j]==2)
      rect(xloc+54-13,a+my.ri8m[1,j]*mult,xloc+54-7,a+my.ri8m[1,j-1]*mult,
           col=color[2],border=color[2], lend=1, ljoin=1)
  }
  my.ri8p <- my.ri8[[i]]$pat
  for(j in 2:ncol(my.ri8p)) {
    if(my.ri8p[2,j]==2)
      rect(xloc+54+7,a+my.ri8p[1,j]*mult,xloc+54+13,a+my.ri8p[1,j-1]*mult,
           col=color[2],border=color[2], lend=1, ljoin=1)
  }

  xloc <- xloc+227
}
text(38-25,(70+a)/2,expression(F[infinity]),adj=c(1,0.5),cex=1.2)
points(rep(-3,3),c(-8,0,8)+mean(c(140,(70+a)/2)),pch=16,cex=0.4)

dev.off()
