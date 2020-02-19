plot_sibship <-
    function(x=0, y=0, dx=5, dy=10, dy2=dy*0.6, dx3=dx*0.32, dy3=dy*0.22,
             bg="white", bg_pt=c(bg, bg, "lightblue","lightblue"),
             sib1_female=TRUE, sib2_female=FALSE, add=TRUE,
             cex=13, lwd=2,
             dead=c(FALSE, FALSE, FALSE, FALSE),
             dead_color=broman::brocolors("web")["red"])
{
    x <- c(x, x+dx, x, x+dx)
    y <- c(y, y, y-dy, y-dy)

    if(!add) {
        xlim <- range(c(x-dx/2, x+dx/2))
        ylim <- range(c(y-dy/2, y+dy/2))
        plot(0, 0, type="n", xlab="", xaxt="n", ylab="", yaxt="n", bty="n",
             xlim=xlim, ylim=ylim)
    }

    segments(x[1], y[1], x[2], y[2], lwd=lwd)
    segments(x[1], y[1]-dy2, x[2], y[2]-dy2, lwd=lwd)
    segments(x[1], y[1]-dy2, x[3], y[3], lwd=lwd)
    segments(x[2], y[2]-dy2, x[4], y[4], lwd=lwd)
    segments(mean(x[1:2]), mean(y[1:2]), mean(x[1:2]), y[1]-dy2, lwd=lwd)

    points(x, y, pch=22-c(1, 0, 1*sib1_female, 1*sib2_female),
           col=bg, cex=cex, lwd=lwd)
    points(x, y, pch=22-c(1, 0, 1*sib1_female, 1*sib2_female), cex=cex, lwd=lwd,
           bg=bg_pt)

    if(any(dead)) {
        xx <- x[dead]
        yy <- y[dead]
        segments(xx-dx3, yy-dy3,
                 xx+dx3, yy+dy3, lwd=lwd, col=dead_color)

    }

}


plot_geno <-
    function(x=0, y=0, dx=5, dy=10, dx2=dx*0.12, dy2=dy*0.4,
             geno, ...)
{
    x <- c(x, x+dx, x, x+dx)
    y <- c(y, y, y-dy, y-dy)

    text(x-dx2, y-dy2, geno[,1], ...)
    text(x+dx2, y-dy2, geno[,2], ...)
}


pdf("../Figs/sibpairs.pdf", width=9.75, height=5.5, pointsize=12)
par(mar=rep(0.1, 4))
plot(0, 0, type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0, 100), ylim=c(0,20))

set.seed(20200220)
sib1_female <- sample(c(FALSE, TRUE), 8, replace=TRUE)
sib2_female <- sample(c(FALSE, TRUE), 8, replace=TRUE)

k <- 1
for(x in seq(0, 80, length=4)) {
    for(y in c(0, 10)) {
        plot_sibship(x=3+x, y=18-y, cex=8, dy=5, dx=10,
                     sib1_female=sib1_female[k],
                     sib2_female=sib2_female[k])
        k <- k + 1
    }
}
dev.off()


pdf("../Figs/sibpairs_wdata.pdf", width=9.75, height=5.5, pointsize=12)
par(mar=rep(0.1, 4))
plot(0, 0, type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0, 100), ylim=c(0,20))

p <- c(0.07942584, 0.01339713, 0.24593301, 0.08229665, 0.02105263,
       0.41913876, 0.13301435, 0.00382775, 0.00191388)

momg <- matrix(sample(seq_along(p), 16, prob=p, replace=TRUE), ncol=2)
dadg <- matrix(sample(seq_along(p), 16, prob=p, replace=TRUE), ncol=2)

sib1 <- matrix(sample(1:2, 16, replace=TRUE), ncol=2)
sib2 <- matrix(sample(1:2, 16, replace=TRUE), ncol=2)
for(i in 1:nrow(sib1)) {
    sib1[i,1] <- momg[i,sib1[i,1]]
    sib2[i,1] <- momg[i,sib2[i,1]]
    sib1[i,2] <- dadg[i,sib1[i,2]]
    sib2[i,2] <- dadg[i,sib2[i,2]]
}

k <- 1
for(x in seq(0, 80, length=4)) {
    for(y in c(0, 10)) {
        plot_sibship(x=3+x, y=18-y, cex=8, dy=5, dx=10,
                     sib1_female=sib1_female[k],
                     sib2_female=sib2_female[k])
        plot_geno(x=3+x, y=18-y, dy=5, dx=10,
                  geno=rbind(momg[k,], dadg[k,], sib1[k,], sib2[k,]),
                  cex=1.8)

        k <- k + 1

    }
}
dev.off()


pdf("../Figs/sibpairs_nopar.pdf", width=9.75, height=5.5, pointsize=12)
par(mar=rep(0.1, 4))
plot(0, 0, type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0, 100), ylim=c(0,20))

for(x in seq(0, 80, length=4)) {
    for(y in c(0, 10)) {
        plot_sibship(x=3+x, y=18-y, cex=8, dy=5, dx=10,
                     sib1_female=FALSE, sib2_female=FALSE,
                     dead=c(TRUE,TRUE,FALSE,FALSE))
    }
}
dev.off()



momg <- dadg <- matrix(rep("", 16), ncol=2)



pdf("../Figs/sibpairs_nopar_wdata.pdf", width=9.75, height=5.5, pointsize=12)
par(mar=rep(0.1, 4))
plot(0, 0, type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0, 100), ylim=c(0,20))

k <- 1
for(x in seq(0, 80, length=4)) {
    for(y in c(0, 10)) {
        plot_sibship(x=3+x, y=18-y, cex=8, dy=5, dx=10,
                     sib1_female=FALSE, sib2_female=FALSE,
                     dead=c(TRUE,TRUE,FALSE,FALSE))
        plot_geno(x=3+x, y=18-y, dy=5, dx=10,
                  geno=rbind(momg[k,], dadg[k,], sib1[k,], sib2[k,]),
                  cex=1.8)

        k <- k + 1

    }
}
dev.off()
