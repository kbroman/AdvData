# illustrations of the bootstrap

bootstrap_illu <-
function(fig=1)
{

    library(broman)
    blue <- brocolors("web")["blue"]
    pink <- brocolors("web")["fuchsia"]
    purple <- brocolors("web")["purple"]

    m <- 5
    df <- 6
    n <- 1000
    set.seed(20200315)
    samp <- rchisq(n, df)*m
    mx <- qchisq(0.999, df)*m
    mx <- max(mx, max(samp))
    true_qu <- qchisq(0.95, df)*m

    pdf(paste0("../Figs/bootstrap_illu_", fig, ".pdf"), height=5.5, width=9.75, pointsize=18)
    layout(cbind(c(rep(1,5), 2, rep(3,5)), c(rep(4,3), 5, rep(6,3), 7, rep(8,3))),
           height=rep(1, 11), width=c(6,5))

    # population distribution
    par(mar=c(3.1, 1.1, 0.1, 1.1))
    x <- seq(0, mx, len=251)
    y <- dchisq(x/m, df)/m
    plot(x, y, type="l", lwd=2, yaxs="i", yaxt="n", ylab="", xlab="", xaxs="i", bty="n",
         ylim=c(0, max(y)*1.05), xlim=c(0, mx*1.05))
    y_true_qu <- dchisq(true_qu/m, df)/m
    segments(true_qu, 0, true_qu, y_true_qu, col=pink, lwd=3)
    text(true_qu, y_true_qu*1.8, expression(theta),
         col=pink, cex=1.3, adj=c(0.5, 0.5))
    if(fig==1) {
        text(true_qu+3, y_true_qu*1.8, "= 95th percentile",
             col=pink, cex=1.3, adj=c(0, 0.5))
    }
    u <- par("usr")
    text(sum(u[1:2]*c(0.3,0.7)), sum(u[3:4]*c(0.3,0.7)),
         "Population", col=purple, cex=1.4)


    # arrow
    par(mar=rep(0,4))
    plot(0,0,xlim=c(0, 100), ylim=c(0,100),
         xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
         xlab="", ylab="", type="n")
    arrows(40, 100, 40, 0, len=0.15, col=blue, lwd=2)
    text(45, 50, "Sample n", col=blue, adj=c(0, 0.5), cex=1.3)

    # sample
    par(mar=c(3.1, 1.1, 0.1, 1.1))
    hist(samp, breaks=seq(0, mx, len=61), main="", yaxt="n", xlab="", ylab="", xaxs="i", prob=TRUE)
    u <- par("usr")
    est_qu <- quantile(samp, 0.95)
    segments(est_qu, 0, est_qu, diff(u[3:4])*0.2, lwd=3, col=pink)
    text(est_qu, diff(u[3:4])*0.25, expression(hat(theta)), col=pink, cex=1.3, adj=c(0.5, 0))
    text(sum(u[1:2]*c(0.3,0.7)), sum(u[3:4]*c(0.3,0.7)),
         "Data", col=purple, cex=1.4)




    if(fig == 1) {

        # skip two frames
        par(mar=rep(0, 4))
        plot(0,0,type="n", bty="n", xaxt="n", yaxt="n")
        par(mar=rep(0, 4))
        plot(0,0,type="n", bty="n", xaxt="n", yaxt="n")

        # plain text
        par(mar=rep(0, 4))
        plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n", xlim=c(0, 100), ylim=c(0, 100))
        text(50, 50, expression(paste("What is the standard error of ", hat(theta), "?")),
             col=blue, cex=1.3)
    }


    if(fig==3 || fig==4) { # estimate population distribution and show it on the left
        m_hat <- var(samp)/mean(samp)/2
        df_hat <- mean(samp)/m_hat

        x <- seq(0, mx, len=251)
        y_fitted <- dchisq(x/m_hat, df_hat)/m_hat

        lines(x, y_fitted, col=blue, lwd=2)
    }

    if(fig>=5) {
        par(mar=c(3.1, 1.1, 0.1, 1.1))
        hist(samp, breaks=seq(0, mx, len=61), main="", yaxt="n", xlab="", ylab="", xaxs="i", prob=TRUE)
        lines(x, y, type="l", lty=2) # true population
    }


    if(!(fig %% 2)) {

        par(mar=c(3.1, 1.1, 0.1, 1.1))
        x <- seq(0, mx, len=251)
        y <- dchisq(x/m, df)/m
        if(fig==2) {
            plot(x, y, type="l", lwd=2, yaxs="i", yaxt="n", ylab="", xlab="", xaxs="i", bty="n",
                 ylim=c(0, max(y)*1.05), xlim=c(0, mx*1.05))
            u <- par("usr")
            text(sum(u[1:2]*c(0.2,0.8)), sum(u[3:4]*c(0.3,0.7)),
                 "Population", col=purple, cex=1.2)
        }
        if(fig==4) {
            plot(x, y_fitted, type="l", lwd=2, yaxs="i", yaxt="n", ylab="", xlab="", xaxs="i", bty="n",
                 ylim=c(0, max(y)*1.05), xlim=c(0, mx*1.05), col=blue)
            lines(x, y, type="l", lty=2) # true population
            u <- par("usr")
            text(sum(u[1:2]*c(0.2,0.8)), sum(u[3:4]*c(0.3,0.7)),
                 "Estimated\nPopulation", col=purple, cex=1.2)
        }

        # arrow
        par(mar=rep(0,4))
        plot(0,0,xlim=c(0, 100), ylim=c(0,100),
             xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
             xlab="", ylab="", type="n")
        arrows(40, 100, 40, 0, len=0.15, col=blue, lwd=2)
        text(45, 50, "Sample n", col=blue, adj=c(0, 0.5), cex=1.3)

        # sample
        par(mar=c(3.1, 1.1, 0.1, 1.1))
        if(fig==2)  new_samp <- rchisq(n, df)*m
        else if(fig==4)   new_samp <- rchisq(n, df_hat)*m_hat
        else if(fig==6)   new_samp <- sample(samp, replace=TRUE)
        new_samp <- new_samp[new_samp <= mx]
        hist(new_samp, breaks=seq(0, mx, len=61), main="", yaxt="n", xlab="", ylab="", xaxs="i", prob=TRUE)
        est_qu <- quantile(new_samp, 0.95)
        u <- par("usr")
        segments(est_qu, 0, est_qu, diff(u[3:4])*0.2, lwd=3, col=pink)
        text(est_qu, diff(u[3:4])*0.25, expression(hat(theta)), col=pink, cex=1.3, adj=c(0.5, 0))
        text(sum(u[1:2]*c(0.2,0.8)), sum(u[3:4]*c(0.3,0.7)),
             "Simulated data", col=purple, cex=1.2)

        # arrow
        par(mar=rep(0,4))
        plot(0,0,xlim=c(0, 100), ylim=c(0,100),
             xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
             xlab="", ylab="", type="n")
        arrows(40, 100, 40, 0, len=0.15, col=blue, lwd=2)
        text(45, 50, "[Repeat 1000 times]", col=blue, adj=c(0, 0.5), cex=1.3)

        # bootstrap results
        if(fig==2) qu_boot <- replicate(1000, quantile(rchisq(n, df)*m, 0.95))
        else if(fig==4) qu_boot <- replicate(1000, quantile(rchisq(n, df_hat)*m_hat, 0.95))
        else if(fig==6) qu_boot <- replicate(1000, quantile(sample(samp, replace=TRUE), 0.95))

        par(mar=c(3.1, 1.1, 0.1, 1.1))
        hist(qu_boot, breaks=seq(0, mx, len=121), main="", yaxt="n", xlab="", ylab="", xaxs="i", prob=TRUE)
        u <- par("usr")
        text(sum(u[1:2]*c(0.2,0.8)), sum(u[3:4]*c(0.3,0.7)),
             "Distribution of\nEstimate", col=purple, cex=1.2)
    }

    dev.off()
}

# initial figure with just the left side
bootstrap_illu(1)

# add simulation stuff on the right
bootstrap_illu(2)

# use estimated distribution
bootstrap_illu(3)

# use estimated distribution + add simulation stuff on the right
bootstrap_illu(4)

# show empirical distribution in top-right
bootstrap_illu(5)

# use nonparametric bootstrap
bootstrap_illu(6)

####
# 1. just left side
# 2. add simulation stuff on right side
# 3. add fitted curve in lower-left
# 4. add fitted curve in top-right and draw samples from that
# 5. put empirical distribution in top-right and draw samples from that
