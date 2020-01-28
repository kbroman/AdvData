
symmetry <-
function(rmat)
{

  if(ncol(rmat)==15) { # 8-way
    rmat[,-(1:2)] <-
      t( t(rmat[,-(1:2)]) / c(8,16,8,32,16,64,32,32,16,64,32,128,64))

    newmat <- rmat[,c("AAA","ABA","ACA","AEA")]/
      apply(rmat[,c("AAA","ABA","ACA","AEA")],1,function(a) sum(a*c(1,1,2,4)))
    return(cbind(rmat[,1:2],newmat))

  }
  else if(ncol(rmat)==9) { # 4-way?
    rmat[,-(1:2)] <-
      t( t(rmat[,-(1:2)]) / c(4,8,4,16,8,16,8) )

    newmat <- rmat[,c("AAA","ABA","ACA")]/
      apply(rmat[,c("AAA","ABA","ACA")],1,function(a) sum(a*c(1,1,2)))
    return(cbind(rmat[,1:2],newmat))
  }
  else 
    stop("Coded only for 8-way and 4-way autosome")

}


markov <-
function(rmat)
{

  if(ncol(rmat)==15) { # 8-way
    rmat[,-(1:2)] <-
      t( t(rmat[,-(1:2)]) / c(8,16,8,32,16,64,32,32,16,64,32,128,64))

    r <- rmat[,1]
    z <- 8*(1+6*r)^2
    rAAA <- (1-r)^2/z     # x==y==A 
    rABB <- (1-r)*r/z     # (x==y and y!=A) or (x!=y and y==A)
    rABA <- r^2/z         # x!=y and y!=A

    newmat1 <- rmat[,c("AAA","AAB","AAC","AAE")]/cbind(rAAA,rABB,rABB,rABB)
    newmat2 <- rmat[,c("ABA","AAB","ABC","ABE")]/cbind(rABA,rABB,rABA,rABA)
    newmat3 <- rmat[,c("ACA","ACB","AAC","ABC","ACE")]/cbind(rABA,rABA,rABB,rABA,rABA)
    newmat4 <- rmat[,c("AEA","AEB","AEC","AAE","ABE","ACE")]/
      cbind(rABA,rABA,rABA,rABB,rABA,rABA)

    colnames(newmat1) <- c("AAA|AA-","BAA|BA-","CAA|CA-","EAA|EA-")
    colnames(newmat2) <- c("ABA|AB-","BBA|BB-","CBA|CB-","EBA|EB-")
    colnames(newmat3) <- c("ACA|AC-","BCA|BC-","CCA|CC-","DCA|DC-","ECA|EC-")
    colnames(newmat4) <- c("AEA|AE-","BEA|BE-","CEA|CE-","EEA|EE-","FEA|FE-","GEA|GE-")

    return(cbind(rmat[,1:2],newmat1,newmat2,newmat3,newmat4))
  }
#  else if(ncol(rmat)==9) { # 4-way?
#    rmat[,-(1:2)] <-
#      t( t(rmat[,-(1:2)]) / c(4,8,4,16,8,16,8) )
#
#    r1 <- 1/(4*(1+6*rmat[,1]))
#    r2 <- rmat[,1]/(2*(1+6*rmat[,1]))
#
#    newmat1 <- rmat[,c("AAA","AAB","AAC")]/cbind(r1,r2,r2)
#    newmat2 <- rmat[,c("ABA","AAB","ABC")]/cbind(r2,r1,r2)
#    newmat3 <- rmat[,c("ACA","ACB","AAC")]/cbind(r2,r2,r1)
#
#    colnames(newmat1) <- c("A|AA","A|BA","A|CA")
#    colnames(newmat2) <- c("A|AB","A|BB","A|CB")
#    colnames(newmat3) <- c("A|AC","A|BC","A|CC")
#
#    return(cbind(rmat[,1:2],newmat1,newmat2,newmat3))
#  }
  else
#    stop("Coded only for 8-way and 4-way autosome")
    stop("Coded only for 8-way autosome")
}

sym8AI <- symmetry(r8AI)
sym8ANI <- symmetry(r8ANI)
mark8AI <- markov(r8AI)
mark8ANI <- markov(r8ANI)


#plot(sym8AI[,c(1,4)],type="l",lwd=2,xlab="r",ylab="Pr(AxA|A-A)",ylim=c(0,1/8))
#lines(sym8AI[,c(1,5)],lwd=2,col="blue",type="l")
#lines(sym8AI[,c(1,6)],lwd=2,col="red",type="l")
#lines(sym8ANI[,c(1,4)],lwd=2,lty=2,type="l")
#lines(sym8ANI[,c(1,5)],lwd=2,col="blue",lty=2,type="l")
#lines(sym8ANI[,c(1,6)],lwd=2,col="red",lty=2,type="l")
#u <- par("usr")
#legend(u[2],u[3],xjust=1,yjust=0,paste("x =",LETTERS[c(2,3,5)]),
#       lwd=2,col=c("black","blue","red"))
#
#par(mfrow=c(2,2),mar=c(5.1,4.1,1.1,1.1))
#for(i in 0:3) {
#  lab <- c("Pr(A|xA)","Pr(A|xB)","Pr(A|xC)","Pr(A|xE)")[i+1]
#  plot(mark8ANI[,c(1,i*4+3)],type="l",lwd=2,
#       xlab="r",ylab=lab,lty=2)
#  lines(mark8ANI[,c(1,i*4+4)],lwd=2,col="blue",lty=2)
#  lines(mark8ANI[,c(1,i*4+5)],lwd=2,col="red",lty=2)
#  lines(mark8ANI[,c(1,i*4+6)],lwd=2,col="green",lty=2)
#  r <- seq(0,0.5,len=301)
#  u <- par("usr")
#  if(i==0)
#    legend(u[2],u[4],xjust=1,paste("x =",LETTERS[c(1:3,5)]),
#           col=c("black","blue","red","green"),lwd=2)
#  else
#    legend(u[2],u[3],xjust=1,yjust=0,paste("x =",LETTERS[c(1:3,5)]),
#           col=c("black","blue","red","green"),lwd=2)
#
#
#  lines(mark8AI[,c(1,i*4+3)],lwd=2,col="black")
#  lines(mark8AI[,c(1,i*4+4)],lwd=2,col="blue")
#  lines(mark8AI[,c(1,i*4+5)],lwd=2,col="red")
#  lines(mark8AI[,c(1,i*4+6)],lwd=2,col="green")
#
#  if(i==0)
#    lines(r,(1-r)/(1+6*r),col="gray",lwd=2)
#  else
#    lines(r,r/(1+6*r),col="gray",lwd=2)
#}
#par(mfrow=c(1,1))
#
#
#######################################################################
## 4-way
#sym4AI <- symmetry(r4AI)
#sym4ANI <- symmetry(r4ANI)
#plot(sym4AI[,c(1,4)],type="l",lwd=2,xlab="r",ylab="Pr(AxA|A-A)",ylim=c(0,1/4))
#lines(sym4AI[,c(1,5)],lwd=2,col="blue",type="l")
#lines(sym4ANI[,c(1,4)],lwd=2,lty=2,type="l")
#lines(sym4ANI[,c(1,5)],lwd=2,col="blue",lty=2,type="l")
#u <- par("usr")
#legend(u[2],u[3],xjust=1,yjust=0,paste("x =",LETTERS[2:3]),
#       lwd=2,col=c("black","blue"))
#
#mark4AI <- markov(r4AI)
#mark4ANI <- markov(r4ANI)
#par(mfrow=c(2,2),mar=c(5.1,4.1,1.1,1.1))
#for(i in 0:2) {
#  lab <- c("Pr(A|xA)","Pr(A|xB)","Pr(A|xC)")[i+1]
#  plot(mark4ANI[,c(1,i*3+3)],type="l",lwd=2,
#       xlab="r",ylab=lab,lty=2)
#  lines(mark4ANI[,c(1,i*3+4)],lwd=2,col="blue",lty=2)
#  lines(mark4ANI[,c(1,i*3+5)],lwd=2,col="red",lty=2)
#  r <- seq(0,0.5,len=301)
#  u <- par("usr")
#  if(i==0)
#    legend(u[2],u[4],xjust=1,paste("x =",LETTERS[c(1:3)]),
#           col=c("black","blue","red"),lwd=2)
#  else
#    legend(u[2],u[3],xjust=1,yjust=0,paste("x =",LETTERS[c(1:3)]),
#           col=c("black","blue","red"),lwd=2)
#
#
#  lines(mark4AI[,c(1,i*3+3)],lwd=2,col="black")
#  lines(mark4AI[,c(1,i*3+4)],lwd=2,col="blue")
#  lines(mark4AI[,c(1,i*3+5)],lwd=2,col="red")
#
#  if(i==0)
#    lines(r,1/(1+6*r),col="gray",lwd=2)
#  else
#    lines(r,2*r/(1+6*r),col="gray",lwd=2)
#}
#par(mfrow=c(1,1))
