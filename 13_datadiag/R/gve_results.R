# mRNA vs DNA sample mix-ups
library(qtl2)
library(broman)
library(lineup2)


load("../Data/rnaseq_mixups_results.RData")
png("../Figs/gve_dist_matrix.png", height=800, width=1400, pointsize=12)
par(mar=c(6.6,12.1,1.1, 12.1), cex=1.3, cex.axis=1.2, cex.lab=1.2)
image(1:ncol(d), 1:nrow(d), t(d),
      col=gray((0:256)/256), las=1,
      xlab="DNA sample", ylab="RNA sample")
dev.off()



gve_best_vs_self <-
    function(labels=FALSE)
{
    selfd <- get_self(d)
    bestd <- get_best(d)
    par(mar=c(6.6,12.1,1.1, 12.1), cex=1.3, cex.axis=1.2, cex.lab=1.2)
    grayplot(selfd, bestd, xlab="distance to self", ylab="minimum distance")
    if(!labels) return()

    wh <- which(selfd > 0.8)
    xadd <- yadd <- setNames(rep(0, length(wh)), names(wh))
    yadd["DO340"] <- +0.02
    yadd["DO397"] <- -0.02
    yadd["DO306"] <- -0.025
    xadd["DO306"] <- +0.05
    yadd["DO268"] <- +0.02
    yadd["DO310"] <- -0.025
    xadd["DO310"] <- +0.05
    yadd["DO360"] <- -0.01
    yadd["DO370"] <- +0.005
    for(i in seq_along(wh)) {
        text(selfd[wh[i]]-0.008+xadd[i], bestd[wh[i]]+yadd[i], names(wh)[i], adj=c(1, 0.5))
    }
}


pdf("../Figs/gve_best_vs_self.pdf",  height=5.5, width=9.75, pointsize=8)
gve_best_vs_self(FALSE)
dev.off()


pdf("../Figs/gve_best_vs_self_labeled.pdf",  height=5.5, width=9.75, pointsize=8)
gve_best_vs_self(TRUE)
dev.off()


pdf("../Figs/gve_details.pdf", height=5.5, width=9.75, pointsize=8)
label_sep <- 10
par(mfcol=c(2,3), mar=c(3.1,5.1,2.1,1.1), cex=1.3)
for(ind in paste0("DO", c(268, 269, 309, 310, 360, 370))) {
    this_d <- d[ind,]
    this_d <- this_d[order(as.numeric(sub("DO", "", names(this_d))))]
    grayplot(seq_along(this_d), this_d,
             xlab="", xaxs="i", xlim=c(-4.5, ncol(d)+5.5),
             ylab="distance", main=ind, ylim=range(d),
             xat=c(1,100, 200, 300, 400, 500), vlines=c(1,100, 200, 300, 400, 500))
    text(which.min(this_d)-10, min(this_d), names(which.min(this_d)),
         adj=c(1, 0.5), cex=1.2)
}
dev.off()
