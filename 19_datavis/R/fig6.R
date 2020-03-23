######################################################################
# Figure 6: two-color chip
######################################################################

bgcolor <- rgb(0, 0, 98, maxColorValue=255)

Data <- read.csv("rafa_data.csv",header=TRUE,comment="")[,c(73,75)]
x <- Data[,1]
y <- Data[,2]

r <- log2(x)
g <- log2(y)
m <- r-g
a <- (r+g)/2
o <- order(a)
m <- m[o]
a <- a[o]

#bitmap(file="../Figs/fig6a.bmp", width=6, height=5, res=288,
#      pointsize=12)
png(file="../Figs/fig6a.png", width=1200, height=1000, res=288,
    pointsize=12)
par(las=1,fg="white",col="white",col.axis="white",col.lab="white",
    bg=bgcolor,mar=c(5.1,5.1,0.1,1.1))
plot(a,m,lwd=2,cex=0.05,pch=16,
     xlab=expression(paste("Ave{ ",log[2], " ", R,", ",log[2], " ", G," }")),
     ylab=expression(paste(log[2]," { ",R," / ",G," }")),
     ylim=c(-3.6,3.6))
fit1 <- loess(m~a,span=2/3,degree=1)
lines(a,fit1$fitted,col="hotpink",lwd=2)
dev.off()

#bitmap(file="../Figs/fig6b.bmp", width=6, height=5, res=288,
#       pointsize=12)
png(file="../Figs/fig6b.png", width=1200, height=1000, res=288,
    pointsize=12)
par(las=1,fg="white",col="white",col.axis="white",col.lab="white",
    bg=bgcolor,mar=c(5.1,5.1,0.1,1.1))
plot(r,g,lwd=2,cex=0.05,pch=16,
     xlab=expression(paste(log[2]," ", R)),
     ylab=expression(paste(log[2]," ", G)))
abline(0,1,lwd=2,col="hotpink")
dev.off()

#bitmap(file="../Figs/fig6c.bmp", width=6, height=5, res=288,
#       pointsize=12)
png(file="../Figs/fig6c.png", width=1200, height=1000, res=288,
    pointsize=12)
par(las=1,fg="white",col="white",col.axis="white",col.lab="white",
    bg=bgcolor,mar=c(5.1,5.1,0.1,1.1))
plot(x,y,lwd=2,cex=0.05,pch=16, xlab=expression(R),ylab="")
mtext(side=2,expression(G),line=4)
abline(0,1,lwd=2,col="hotpink")
dev.off()

#bitmap(file="../Figs/fig6d.bmp", width=6, height=5, res=288,
#       pointsize=12)
png(file="../Figs/fig6d.png", width=1200, height=1000, res=288,
    pointsize=12)
par(las=1,fg="white",col="white",col.axis="white",col.lab="white",
    bg=bgcolor,mar=c(5.1,5.1,0.1,1.1))
plot(x,y,lwd=2,cex=0.05,pch=16, xlab=expression(R),ylab="")
mtext(side=2,expression(G),line=4)
dev.off()

