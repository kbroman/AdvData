######################################################################
# Operon
######################################################################
library(broman)
orange <- brocolors("web")["orange"]
green <- brocolors("web")["green"]
red <- brocolors("web")["red"]

pdf("../Figs/operon.pdf", height=1, width=9)
par(mar=rep(0.1,4),bty="n")
plot(0,0,type="n",xaxt="n",xlab="",yaxt="n",ylab="",
     ylim=c(0,10), xlim=c(0,100))
len <- c(51,88,188,87,154)
gap <- c(3,1,4,2)*5
x <- sum(c(len,gap))
len <- len/x*100; gap <- gap/x*100
start <- c(0,cumsum(len[1:4]+gap))
end <- len[1]+c(0,cumsum(len[2:5]+gap))
taloc <- c(1.79,2.41,3.57,
           10.85,15.37,16.82,18.27,20.15,21.32,
           25.88,30.19,32.78,35.76,42.12,44.11,46.04,47.62,50.81,53.59,
           59.49,61.52,65.63,68.84,70.29,70.92,71.55,
           76.14,79.03,85.00,86.66,89.46,90.45,92.35,97.33)
segments(taloc,5,taloc,6.5,lwd=2,col=orange)
arrows(start,5,end,5,lwd=4,len=0.15,col=c(green,red,green,green,green))
dev.off()
