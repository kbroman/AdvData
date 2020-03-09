######################################################################
# gene overlap
######################################################################
library(broman)
orange <- brocolors("web")["orange"]
red <- brocolors("web")["red"]
blue <- brocolors("web")["blue"]
purple <- brocolors("web")["purple"]


pdf("../Figs/overlap.pdf", height=6, width=4.5)
par(mar=rep(0.1,4),bty="n")
plot(0,0,type="n",xaxt="n",xlab="",yaxt="n",ylab="",
     ylim=c(600,0), xlim=c(450,0))

# first pair
taloc <- c(25,55,64,70,76,100,124,142,172,183,190,220,238,244,
           280,322,373,388,391,406,412,418,424,445)
segments(taloc[taloc>150],50,taloc[taloc>150],40,lwd=2,col=orange)
segments(taloc[taloc<250],100,taloc[taloc<250],90,lwd=2,col=orange)
segments(taloc[taloc>150 & taloc<250],50,taloc[taloc>150 & taloc<250],40,
         lwd=2,col=purple)
segments(taloc[taloc>150 & taloc<250],100,taloc[taloc>150 & taloc<250],90,
         lwd=2,col=purple)
segments(450,50,210,50,lwd=4,col=blue)
arrows(210,50,150,50,lwd=4,col=red,len=0.15)
segments(250,100,50,100,lwd=4,col=blue)
arrows(50,100,0,100,lwd=4,col=red,len=0.15)

# second pair
taloc <- c(49,103,124,148,191,232,286,310,337,358,370,409,424,439)
segments(taloc[taloc>200],250,taloc[taloc>200],240,lwd=2,col=orange)
segments(taloc[taloc<260],300,taloc[taloc<260],290,lwd=2,col=orange)
segments(taloc[taloc>200 & taloc<260],250,taloc[taloc>200 & taloc<260],240,
         lwd=2,col=purple)
segments(taloc[taloc>200 & taloc<260],300,taloc[taloc>200 & taloc<260],290,
         lwd=2,col=purple)
segments(450,250,250,250,lwd=4,col=blue)
arrows(250,250,200,250,lwd=4,col=red,len=0.15)
segments(40,300,220,300,lwd=4,col=blue)
arrows(220,300,260,300,lwd=4,col=red,len=0.15)

# third pair
taloc <- c(28,48,68,96,104,124,132,140,152,160,176,208,232,248,272,296,320,
           328,340,356,384,404,426)
segments(taloc[taloc>150],450,taloc[taloc>150],440,lwd=2,col=orange)
segments(taloc[taloc<200],500,taloc[taloc<200],490,lwd=2,col=orange)
segments(taloc[taloc>150 & taloc<200],450,taloc[taloc>150 & taloc<200],440,
         lwd=2,col=purple)
segments(taloc[taloc>150 & taloc<200],500,taloc[taloc>150 & taloc<200],490,
         lwd=2,col=purple)
segments(150,450,390,450,lwd=4,col=blue)
arrows(390,450,450,450,lwd=4,col=red,len=0.15)
segments(200,500,40,500,lwd=4,col=blue)
arrows(40,500,0,500,lwd=4,col=red,len=0.15)
dev.off()
