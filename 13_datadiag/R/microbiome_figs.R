library(broman)

microbiome <- readRDS("../Data/microbiome_mixups_dist.rds")

microbiome_best_vs_self <- function(label=FALSE)
{
    par(mar=c(6.6,12.1,1.1, 12.1), cex=1.3, cex.axis=1.2, cex.lab=1.2)

    # color things
    bgcol <- setNames(rep("lightblue", length(microbiome$self)), names(microbiome$self))
    if(label) {
        mixups <- sprintf("DO%03d", c(360,370,53,54,83,85,88))
        dups <- sprintf("DO%03d", c(327,336))
        mixtures <- sprintf("DO%03d", c(329,340,343,344,346,354,359,362,   385,41))
        bad <- sprintf("DO%03d", c(357,397))
        bad_micro <- "DO174"
        weak <- c("DO205", "DO358")
        bgcol[mixups] <- "violetred"
        bgcol[bad] <- brocolors("web")["purple"]
        bgcol[mixtures] <- brocolors("web")["green"]
        bgcol[dups] <- "violetred"
        bgcol[bad_micro] <- brocolors("web")["purple"]
    }


    grayplot(microbiome$self, microbiome$best, bg=bgcol,
             xlab="distance to self", ylab="minimum distance",
             xlim=c(0, 0.22), ylim=c(0, 0.22), yaxs="i", xaxs="i")

    if(!label) return()

    par(cex=1.7)
    text(0.17, 0.145, "bad DNA", col=brocolors("web")["purple"])
    text(0.18, 0.025, "mix-ups", col="violetred")
    text(0.14, 0.09, "mixtures", col=brocolors("web")["green"], adj=c(1, 0.5))
    text(0.102, 0.048, "few reads", col=brocolors("web")["purple"], adj=c(1, 0.5))
}

pdf("../Figs/microbiome_best_vs_self.pdf", height=5.5, width=9.75, pointsize=8)
microbiome_best_vs_self(FALSE)
dev.off()


pdf("../Figs/microbiome_best_vs_self_labeled.pdf", height=5.5, width=9.75, pointsize=8)
microbiome_best_vs_self(TRUE)
dev.off()
