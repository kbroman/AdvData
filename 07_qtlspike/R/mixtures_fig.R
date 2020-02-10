pdf(file="../Figs/mixtures.pdf", width=4.5, height=5.5, pointsize=12, onefile=TRUE)
par(bty="n")


par(mar=c(5.1,0.1,0.1,0.1))

x <- seq(0,100,by=0.1)
r <- (1-exp(-20/50))/2
r1 <- (1-exp(-7/50))/2
r2 <- (1-exp(-13/50))/2
p11 <- (1-r1)*(1-r2)/(1-r)
p12 <- (1-r1)*r2/r
p21 <- r1*(1-r2)/r
p22 <- r1*r2/(1-r)
plot(0,0,type="l",xlab="Phenotype",ylab="",xlim=c(20,100),ylim=c(0,5.3),yaxt="n")
a <- par("usr")
abline(h=c(0,1,2,3)*1.5 + a[3])
lines(x,(p11*dnorm(x,65,8)+(1-p11)*dnorm(x,50,8))/0.05+a[3],lwd=2)
text(rep(30,4),c(0,1,2,3)*6/4 + a[3]+0.5,c("BB/BB","BB/AB","AB/BB","AB/AB"))
lines(x,(p12*dnorm(x,65,8)+(1-p12)*dnorm(x,50,8))/0.05+1.5+a[3],lwd=2)
lines(x,p12*dnorm(x,65,8)/0.05+1.5+a[3],lwd=2,lty=2)
lines(x,(1-p12)*dnorm(x,50,8)/0.05+1.5+a[3],lwd=2,lty=2)
lines(x,(p21*dnorm(x,65,8)+(1-p21)*dnorm(x,50,8))/0.05+3+a[3],lwd=2)
lines(x,p21*dnorm(x,65,8)/0.05+3+a[3],lwd=2,lty=2)
lines(x,(1-p21)*dnorm(x,50,8)/0.05+3+a[3],lwd=2,lty=2)
lines(x,(p22*dnorm(x,65,8)+(1-p22)*dnorm(x,50,8))/0.05+4.5+a[3],lwd=2)
x <- seq(20,100,by=20)
segments(x,0.0+a[3],x,0.0+a[3]-0.12,xpd=TRUE, lend=1, ljoin=1)
segments(x,1.5+a[3],x,1.5+a[3]-0.12, lend=1, ljoin=1)
segments(x,3.0+a[3],x,3.0+a[3]-0.12, lend=1, ljoin=1)
segments(x,4.5+a[3],x,4.5+a[3]-0.12, lend=1, ljoin=1)
z <- c(0,1.5,3,4.5)
segments(50,z+a[3],50,z+a[3]-0.12,lwd=2,xpd=TRUE, lend=1, ljoin=1)
segments(65,z+a[3],65,z+a[3]-0.12,lwd=2,xpd=TRUE, lend=1, ljoin=1)
text(51,z+a[3]-0.28,expression(bold(mu[0])),xpd=TRUE,col="violetred")
text(66,z+a[3]-0.28,expression(bold(mu[1])),xpd=TRUE,col="violetred")
dev.off()
