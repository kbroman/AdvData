# make a plot of siblings' chromosomes

library(broman)
library(simcross)

pdf("../Figs/sib_chr.pdf", width=9.75, height=5.5, pointsize=12)

par(mar=rep(0.1,4))
plot(0,0,type="n", bty="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0, 100), ylim=c(0, 100))

n <- 10
x <- seq(5, 95, length=n)
xpar <- c(40, 60)
y1 <- 95
y2 <- 82
y3 <- 70

segments(xpar[1], y1, xpar[2], y1, lwd=2)
segments(x[1], y2, x[n], y2, lwd=2)
segments(x, rep(y2, n), x, rep(y3,n), lwd=2)
segments(mean(xpar), y1, mean(xpar), y2, lwd=2)

points(xpar, c(y1, y1), pch=c(1,0)+15, lwd=2, cex=8, col="white")
points(xpar, c(y1, y1), pch=c(1,0), lwd=2, cex=8)

set.seed(20200220)
sex <- sample(0:1, n, replace=TRUE)
points(x, rep(y3, n), pch=sex+15, lwd=2, cex=8, col="white")
points(x, rep(y3, n), pch=sex, lwd=2, cex=8)

col <- brocolors("web")[c("purple", "teal", "olive", "orange")]
yd <- 5
rect(xpar[1]-8, y1-yd, xpar[1]-7, y1+yd, col=col[1], border=col[1])
rect(xpar[1]-6, y1-yd, xpar[1]-5, y1+yd, col=col[2], border=col[2])
rect(xpar[2]+6, y1-yd, xpar[2]+5, y1+yd, col=col[3], border=col[3])
rect(xpar[2]+8, y1-yd, xpar[2]+7, y1+yd, col=col[4], border=col[4])

# rescale from chr position to plot position
rescale1 <- function(a) a/250 * (y3-10 - 5) + 5
rescale2 <- function(a) a/150 * (y3-10 - 5) + 5

for(i in seq_along(x)) {
    rect(x[i]-2, y3-10, x[i]-0.75, 5, col=col[1], border=col[1])
    rect(x[i]+2, y3-10, x[i]+0.75, 5, col=col[3], border=col[3])

    m1 <- sim_meiosis( create_parent(250, 1:2), m=3 )
    m2 <- sim_meiosis( create_parent(150, 1:2), m=3 )

    m1loc <- c(0, m1$locations)
    if(any(m1$alleles==2)) {
        for(j in which(m1$alleles==2)) {
            rect(x[i]-2, rescale1(m1loc[j]), x[i]-0.75, rescale1(m1loc[j+1]),
                 col=col[2], border=col[2])
        }
    }

    m2loc <- c(0, m2$locations)
    if(any(m2$alleles==2)) {
        for(j in which(m1$alleles==2)) {
            rect(x[i]+2, rescale2(m2loc[j]), x[i]+0.75, rescale2(m2loc[j+1]),
                 col=col[4], border=col[4])
        }
    }

}


dev.off()
