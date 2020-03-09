# Uniform vs. binomial
library(broman)
blue <- "slateblue"
red <- "violetred"

pdf("../Figs/unif_v_binom.pdf",height=5.5, width=9.75)
plot(0,0,type="n",xlab="Number of essential genes",yaxt="n", ylab="",
     xlim=c(0,4204),ylim=c(0,0.01231))
hi <- qbinom(1-1e-5,4204,0.5)
lo <- qbinom(1e-5,4204,0.5)
segments(0,0,lo,0,lwd=2,col=red)
lines(lo:hi,dbinom(lo:hi,4204,0.5),lwd=2,col=red)
segments(hi,0,4204,0,lwd=2,col=red)
segments(0,1/4205,4204,1/4205,lwd=2,col=blue)
u <- par("usr")
legend(u[1],u[4],c("Uniform","Binomial"),lwd=2,col=c(blue,red),cex=1.5)
dev.off()
