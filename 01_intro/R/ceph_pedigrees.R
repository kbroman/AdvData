######################################################################
# CEPH pedigrees
######################################################################

library(broman)
bgcolor <- "white"
text_color <- brocolors("web")["blue"]
seg_color <- brocolors("web")["red"]

pdf("../Figs/ceph_pedigrees.pdf", width=10, height=6,
    pointsize=12, onefile=TRUE)
par(mar=rep(0.1,4),lwd=1,bty="n")
plot(0,0,type="n",xlab="",ylab="",xaxt="n",yaxt="n",
     xlim=c(0,90),ylim=c(0,50))

fam.x <-
  c(3.7, 5.3, 6.7, 8.3,   4.5, 7.5,   1:11,
    18.7, 20.3, 21.7, 23.3, 19.5, 22.5,  16:26,
    33.7, 35.3, 36.7, 38.3, 34.5, 37.5, seq(31.5, 40.5),
    3.7, 5.3, 6.7, 8.3,   4.5, 7.5,   1:11,
    19.2, 20.8, 22.2, 23.8,  20, 23,  14:28,
    33.7, 35.3, 36.7, 38.3,   34.5, 37.5,  31:41,
    12.2, 13.8, 15.2, 16.8,   13, 16, 9:20,
       27, 30, 22:35)


fam.y <-
  c(c(2.75,2.75,2.75,2.75,  2,2, rep(1,11))+3.5*2,
    c(2.75,2.75,2.75,2.75,  2,2, rep(1, 11))+3.5*2,
    c(2.75,2.75,2.75,2.75,  2,2, rep(1, 10))+3.5*2,
    c(2.75,2.75,2.75,2.75,  2,2, rep(1,11))+3.5,
    c(2.75,2.75,2.75,2.75,  2,2, rep(1, 15))+3.5,
    c(2.75,2.75,2.75,2.75,  2,2, rep(1, 11))+3.5,
    c(2.75,2.75,2.75,2.75,  2,2, rep(1,12)),
    c(                      2,2, rep(1, 14)))


sex <- c(0,1,0,1,  0,1,  0,1,0,0,0,0,1,1,1,0,1,
         0,1,0,1,  0,1,  0,1,0,1,0,0,1,0,1,1,1,
         0,1,0,1,  0,1,  0,1,1,1,0,0,1,1,1,1,
         0,1,0,1,  0,1,  0,0,1,0,0,1,0,0,1,0,1,
         0,1,0,1,  0,1,  1,1,1,0,0,1,1,1,1,0,1,1,1,0,1,
         0,1,0,1,  0,1,  1,1,0,1,0,1,0,0,1,0,0,
         0,1,0,1,  0,1,  0,0,0,1,1,1,0,0,1,1,0,0,
                   0,1,  0,1,0,0,0,1,1,0,1,0,0,0,1,1)


lab <- c(15,14, 13,12, 2, 1, 3:11,16,17,
         16,15, 14,13, 2, 1, 3:12,17,
         15,14, 13,12, 2, 1, 3:11,16,
         16,15,14,13,  2, 1, 3:12,17,
         19,21,18,20,  2, 1, 3:17,
         14,13,12,11,  2, 1, 3:11,15,16,
         18,17,16,15,  2, 1, 3:14,
                       2, 1, 3:16)


couples <- list(c(1,2), c(3,4), c(5,6),
                c(18,19), c(20,21), c(22,23),
                c(35,36), c(37,38), c(39,40),
                c(51,52), c(53,54), c(55,56),
                c(68,69), c(70,71), c(72,73),
                c(89,90), c(91,92), c(93,94),
                c(106,107), c(108,109), c(110,111),
                c(124,125))
sibships <- list( 5,  6, 7:17,
                 22, 23, 24:34,
                 39, 40, 41:50,
                 55, 56, 57:67,
                 72, 73, 74:88,
                 93, 94, 95:105,
                 110,111,112:123,126:139)

famnam <- c(1331, 1332, 1347, 1362, 1413, 1416, 884, 102)
faminx <- c(1, 18, 35, 51, 68,89, 106, 124)
faminy <- c(1, 18, 35, 51, 68,89, 106, 106)
dead <- c(69,71)

u <- par("usr")
ux <- u[1:2]
uy <- u[3:4]

fam.x <- fam.x/(max(fam.x)+1)*diff(ux) + min(ux)
d <- 0.2
fam.y <- (fam.y-0.5-d)/(max(fam.y)-2*d)*diff(uy) + min(uy)

for(i in seq(along=faminx))
  text(fam.x[faminx[[i]]] - diff(u[1:2])*0.02, fam.y[faminy[[i]]],
       famnam[i], col=text_color, adj=c(1,0.5), cex=1.5)

for(i in 1:length(couples))
  segments(fam.x[couples[[i]][1]],
           fam.y[couples[[i]][1]],
           fam.x[couples[[i]][2]],
           fam.y[couples[[i]][2]], lend=2, ljoin=1)

vy <- diff(sort(unique(fam.y)))[1]
py <- 0.35

kids <- unlist(sibships)
segments(fam.x[kids],fam.y[kids],fam.x[kids],
         fam.y[kids]+vy*py, lend=2,ljoin=1)

for(i in 1:length(couples)) {
  if(length(sibships[[i]])==1)
    segments(mean(fam.x[couples[[i]]]),
             fam.y[couples[[i]]][1],
             mean(fam.x[couples[[i]]]),
             fam.y[sibships[[i]]], lend=2, ljoin=1)
  else {
    segments(mean(fam.x[couples[[i]]]),
             fam.y[couples[[i]]][1],
             mean(fam.x[couples[[i]]]),
             fam.y[couples[[i]][1]]-vy*(1-py), lend=2, ljoin=1)
    segments(min(fam.x[sibships[[i]]]),
             fam.y[couples[[i]][1]]-vy*(1-py),
             max(fam.x[sibships[[i]]]),
             fam.y[couples[[i]][1]]-vy*(1-py), lend=2, ljoin=1)
  }

}



thepch <- 1-sex
thepch2 <- thepch+15

points(fam.x,fam.y, pch=thepch2,cex=2.5, col=bgcolor)
points(fam.x,fam.y, pch=thepch, cex=2.5)
text(fam.x, fam.y, lab, cex=0.8)

dy <- 1.1
pin <- par("pin")
u <- par("usr")
dx <- dy * (diff(u[1:2])/pin[1])/(diff(u[3:4])/pin[2])

segments(fam.x[dead]-dx, fam.y[dead]-dy, fam.x[dead]+dx, fam.y[dead]+dy,
         col=seg_color, lend=1, ljoin=1)


dev.off()
