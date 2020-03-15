library(broman)
pink <- brocolors("web")["fuchsia"]
blue <- brocolors("web")["blue"]

attach("boot-summary.RData")

pdf("../Figs/mle1.pdf",height=5.5,width=9.75,pointsize=14)
k <- 1:6
for(i in 6:1) {
  tab <- (thatvt.em[1:101,i] + thatvt.em[101:1,102-i])/10000
  tab <- tab[1:41]
  k[i] <- max(tab)
}
thisk <- k

skip <- 0.07
vdrop <- 0.11
par(mfrow=c(1,1),xaxs="i",yaxs="i")
par(mar=c(3.6,1.1,0.1,1.1),bty="o")
plot(0,0,type="n",ylab="",yaxt="n",xlim=c(-0.5,40.5),ylim=c(0,sum(k)+6*skip),
     xlab="")
tot <- sum(k)+6*skip
lo <- 0
for(i in 6:1) {
  tab <- (thatvt.em[1:101,i] + thatvt.em[101:1,102-i])/10000
  tab <- tab[1:41]
  k[i] <- max(tab)
  z <- 0:40
  segments(z-0.5,tab+lo,z+0.5,tab+lo)
  segments(z-0.5,lo,z-0.5,tab+lo)
  segments(z+0.5,lo,z+0.5,tab+lo)
  abline(h=lo)
  segments(seq(0,40,by=10),lo,seq(0,40,by=10),lo-0.05,xpd=TRUE)
  x <- c(expression(paste(theta," = 0")),
         expression(paste(theta," = 1")),
         expression(paste(theta," = 2")),
         expression(paste(theta," = 3")),
         expression(paste(theta," = 4")),
         expression(paste(theta," = 5")))[i]
  text(37,lo+k[i]+skip-vdrop, x, col=pink, adj=c(0,0.5))
  points(i-1, lo, pch=17, col=pink, xpd=TRUE)
  lo <- lo + k[i]+skip
}
mtext(side=1,"Position (cM)",line=2.5)
dev.off()

pdf("../Figs/mle2.pdf",height=5.5,width=9.75,pointsize=14)
k <- 1:9
for(i in 9:1) {
  tab <- (thatvt.em[1:101,6+i] + thatvt.em[101:1,102-i-6])/10000
  tab <- tab[1:41]
  k[i] <- max(tab)
}
skip <- (tot - sum(k))/9

par(mfrow=c(1,1),xaxs="i",yaxs="i")
par(mar=c(3.6,1.1,0.1,1.1),bty="o")
plot(0,0,type="n",ylab="",yaxt="n",xlim=c(-0.5,40.5),ylim=c(0,sum(k)+9*skip),
     xlab="")
lo <- 0
for(i in 9:1) {
  tab <- (thatvt.em[1:101,6+i] + thatvt.em[101:1,102-i-6])/10000
  tab <- tab[1:41]
  k[i] <- max(tab)
  z <- 0:40
  segments(z-0.5,tab+lo,z+0.5,tab+lo)
  segments(z-0.5,lo,z-0.5,tab+lo)
  segments(z+0.5,lo,z+0.5,tab+lo)
  abline(h=lo)
  segments(seq(0,40,by=10),lo,seq(0,40,by=10),lo-0.05,xpd=TRUE)
  x <- c(expression(paste(theta," = 6")),
         expression(paste(theta," = 7")),
         expression(paste(theta," = 8")),
         expression(paste(theta," = 9")),
         expression(paste(theta," = 10")),
         expression(paste(theta," = 11")),
         expression(paste(theta," = 12")),
         expression(paste(theta," = 13")),
         expression(paste(theta," = 14")))[i]
  text(37,lo+k[i]+skip-vdrop, x, col=pink, adj=c(0,0.5))
  points(i-1+6, lo, pch=17, col=pink, xpd=TRUE)
  lo <- lo + k[i]+skip
}
mtext(side=1,"Position (cM)",line=2.5)
dev.off()

thek <- 1:51
for(i in 1:51) {
  tab <- (thatvt.em[1:101,i] + thatvt.em[101:1,102-i])/10000
  thek[i] <- max(tab)
}

pdf("../Figs/mle4.pdf",height=5.5,width=9.75,pointsize=14)
k <- 1:9
for(i in 9:1) {
  tab <- (thatvt.em[1:101,42+i] + thatvt.em[101:1,102-i-42])/10000
  tab <- tab[26:66]
  k[i] <- max(tab)
}
skip <- (tot - sum(k))/9

par(mfrow=c(1,1),xaxs="i",yaxs="i")
par(mar=c(3.6,1.1,0.1,1.1),bty="o")
plot(0,0,type="n",ylab="",yaxt="n",xlim=c(25.5,65.5),ylim=c(0,sum(k)+9*skip),
     xlab="")
lo <- 0
for(i in 9:1) {
  tab <- (thatvt.em[1:101,42+i] + thatvt.em[101:1,102-i-42])/10000
  tab <- tab[26:66]
  k[i] <- max(tab)
  z <- 25:65
  segments(z-0.5,tab+lo,z+0.5,tab+lo)
  segments(z-0.5,lo,z-0.5,tab+lo)
  segments(z+0.5,lo,z+0.5,tab+lo)
  abline(h=lo)
  segments(seq(30,60,by=10),lo,seq(30,60,by=10),lo-0.05,xpd=TRUE)
  x <- c(#expression(paste(theta," = 41")),
         expression(paste(theta," = 42")),
         expression(paste(theta," = 43")),
         expression(paste(theta," = 44")),
         expression(paste(theta," = 45")),
         expression(paste(theta," = 46")),
         expression(paste(theta," = 47")),
         expression(paste(theta," = 48")),
         expression(paste(theta," = 49")),
         expression(paste(theta," = 50")))[i]
  text(62,lo+k[i]+skip-vdrop, x, col=pink, adj=c(0,0.5))
  points(i+41, lo, pch=17, col=pink, xpd=TRUE)
  lo <- lo + k[i]+skip
}
mtext(side=1,"Position (cM)",line=2.5)
dev.off()




detach(2)
