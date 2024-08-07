load("gyanu_results_nov02.RData")
numTAs <- mydata[,1]+mydata[,3]+ c(0,mydata[-nrow(mydata),3])

# histogram of number of ta sites
pdf("../Figs/numTAs.pdf",width=8,height=3)
par(mar=c(4.1,0.1,0.1,0.1))
hist(numTAs,breaks=30,ylab="",yaxt="n",xlab="Number of TA sites per gene",main="",xaxt="n")
u <- par("usr")
x <- numTAs + runif(length(numTAs),-0.4,0.4)
segments(x,u[3]-diff(u[3:4])*0.03,x,0,xpd=TRUE)
segments(0,u[3]-diff(u[3:4])*0.03,u[2],u[3]-diff(u[3:4])*0.03,xpd=TRUE)
segments(0,0,u[2],0,xpd=TRUE)
x <- seq(0,200,by=25)
segments(x,u[3]-diff(u[3:4])*0.03,x,2*(u[3]-diff(u[3:4])*0.03),xpd=TRUE)
text(x,3*(u[3]-diff(u[3:4])*0.03),paste(x),xpd=TRUE)
dev.off()
