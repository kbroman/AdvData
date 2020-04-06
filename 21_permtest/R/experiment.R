# illustration of experiment
library(broman) # myround()

set.seed(20200407)

green <- brocolors("web")["green"]
purple <- brocolors("web")["purple"]

pdf("../Figs/experiment.pdf", height=5, width=10, pointsize=14)
par(mar=rep(1.1, 4))
plot(0,0, type="n", xlim=c(0, 100), ylim=c(0, 105), xaxs="i", yaxs="i",
     xaxt="n", yaxt="n", bty="n")
x <- c(2, 48, 52, 98)

text(mean(x[1:2]), 102.5, "Treatment groups", cex=1.3, xpd=TRUE, font=2)
text(mean(x[3:4]), 102.5, "Responses", cex=1.3, xpd=TRUE, font=2)


dat <- list(ttt=matrix(sample(rep(c("C","T"), 3*4)), ncol=4),
            resp=matrix(rnorm(3*4*2, 20, 5), ncol=4))
dat$resp[dat$ttt=="T"] <- dat$resp[dat$ttt=="T"] + 4
dat$color <- matrix("purple", ncol=4, nrow=6)
dat$color[dat$ttt=="C"] <- "green"

for(i in 1:2) {
    xmin <- x[i*2-1]
    xmax <- x[i*2]
    ymin <- x[1]
    ymax <- x[4]
    xx <- seq(xmin, xmax, length=5)
    yy <- seq(ymin, ymax, length=7)
    segments(xx, ymin, xx, ymax, lwd=2)
    segments(xmin, yy, xmax, yy, lwd=2)

    for(ro in 1:nrow(dat$ttt)) {
        for(co in 1:ncol(dat$ttt)) {
            if(i==1) text(mean(xx[co+0:1]), mean(yy[ro+0:1]), dat$ttt[ro,co],
                          col=dat$color[ro,co])
            else text(mean(xx[co+0:1]), mean(yy[ro+0:1]), myround(dat$resp[ro,co], 1),
                          col=dat$color[ro,co])
        }
    }

}
dev.off()


dat$ttt <- factor(dat$ttt, c("C", "T"))

dir <- "_cache"
if(!dir.exists(dir)) dir.create(dir)
saveRDS(dat, file.path(dir, "experiment_data.rds"))



plot_data <-
    function(dat, show_pval=TRUE)
{
    par(mfrow=c(1,2))
    par(mar=c(3.1, 3.1, 1.1, 1.1))
    dotplot(dat$ttt, dat$resp, xlab="Response", ylab="Group", rotate=TRUE,
            mgp.x=c(1.8,0.3,0), mgp.y=c(1.8, 0.2, 0))

    plot(0,0,type="n", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
         xaxs="i", yaxs="i", bty="n")

    eff <- diff(tapply(dat$resp, dat$ttt, mean)) # 5.9
    out <- t.test(as.numeric(dat$resp) ~ dat$ttt)
    SE <- out$stderr # 2.1

    text(30, 85, expression(bar(Y)[T] - bar(Y)[C]), adj=c(1, 0.5), cex=1.5)
    text(33, 86, paste("=", myround(eff, 1)), adj=c(0, 0.5), cex=1.5)
    text(30, 72, expression(hat(SE)), adj=c(1, 0.5), cex=1.5)
    text(33, 71, paste("=", myround(SE, 1)), adj=c(0, 0.5), cex=1.5)

    text(30, 45, "t", adj=c(1, 0.5), cex=1.5)
    text(33, 45, paste("=", myround(out$stat, 2)), adj=c(0, 0.5), cex=1.5)
    if(show_pval) {
        text(99, 45, paste("P =", myround(out$p.value, 2)), adj=c(1, 0.5), cex=1.5)
    }

    text(30, 18, "95% CI", adj=c(1, 0.5), cex=1.5)
    text(33, 18, paste0("= (", myround(-out$conf.int[2], 1), ", ",
                       myround(-out$conf.int[1], 1), ")"), adj=c(0, 0.5), cex=1.5)
}


pdf("../Figs/experiment_results.pdf", height=5, width=10, pointsize=14)
plot_data(dat)
dev.off()



pdf("../Figs/perm_results1.pdf", height=5, width=10, pointsize=14)
plot_data(list(ttt=dat$ttt, resp=sample(dat$resp)),
          show_pval=FALSE)
dev.off()

pdf("../Figs/perm_results2.pdf", height=5, width=10, pointsize=14)
plot_data(list(ttt=dat$ttt, resp=sample(dat$resp)),
          show_pval=FALSE)
dev.off()

pdf("../Figs/perm_results3.pdf", height=5, width=10, pointsize=14)
plot_data(list(ttt=dat$ttt, resp=sample(dat$resp)),
          show_pval=FALSE)
dev.off()

pdf("../Figs/perm_results4.pdf", height=5, width=10, pointsize=14)
plot_data(list(ttt=dat$ttt, resp=sample(dat$resp)),
          show_pval=FALSE)
dev.off()
