# illustrations of the bootstrap

library(broman)
blue <- brocolors("web")["blue"]
pink <- brocolors("web")["fuchsia"]
purple <- brocolors("web")["purple"]

nested_bootstrap <-
function(fig=1)
{


    m <- 5
    df <- 6
    n <- 1000
    set.seed(20200315)
    samp <- rchisq(n, df)*m
    mx <- qchisq(0.999, df)*m
    mx <- max(mx, max(samp))
    true_qu <- qchisq(0.95, df)*m

    pdf(paste0("../Figs/nested_bootstrap_", fig, ".pdf"), height=5.5, width=9.75, pointsize=18)
    layout(cbind(c(rep(1,5), 2, rep(3,5)), c(rep(4,3), 5, rep(6,3), 7, rep(8,3))),
           height=rep(1, 11), width=c(6,5))

    # population distribution
    par(mar=c(3.1, 1.1, 0.1, 1.1))
    hist(samp, breaks=seq(0, mx, len=61), main="", yaxt="n", xlab="", ylab="", xaxs="i", prob=TRUE)

    u <- par("usr")
    text(sum(u[1:2]*c(0.3,0.7)), sum(u[3:4]*c(0.3,0.7)),
         "Data", col=purple, cex=1.4)


    # arrow
    par(mar=rep(0,4))
    plot(0,0,xlim=c(0, 100), ylim=c(0,100),
         xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
         xlab="", ylab="", type="n")
    arrows(40, 100, 40, 0, len=0.15, col=blue, lwd=2)
    text(45, 50, "Sample n", col=blue, adj=c(0, 0.5), cex=1.3)

    # sample
    par(mar=c(3.1, 1.1, 0.1, 1.1))
    set.seed(20200316)
    new_samp <- sample(samp, replace=TRUE)
    hist(new_samp, breaks=seq(0, mx, len=61), main="", yaxt="n", xlab="", ylab="", xaxs="i", prob=TRUE)
    u <- par("usr")
    text(sum(u[1:2]*c(0.3,0.7)), sum(u[3:4]*c(0.3,0.7)),
         "Resampled data", col=purple, cex=1.4)



    if(fig > 1) {

        par(mar=c(3.1, 1.1, 0.1, 1.1))
        hist(new_samp, breaks=seq(0, mx, len=61), main="", yaxt="n", xlab="", ylab="", xaxs="i", prob=TRUE)
        u <- par("usr")
        text(sum(u[1:2]*c(0.2,0.8)), sum(u[3:4]*c(0.3,0.7)),
             "Estimated\nPopulation", col=purple, cex=1.2)

        # arrow
        par(mar=rep(0,4))
        plot(0,0,xlim=c(0, 100), ylim=c(0,100),
             xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
             xlab="", ylab="", type="n")
        arrows(40, 100, 40, 0, len=0.15, col=blue, lwd=2)
        text(45, 50, "Sample n", col=blue, adj=c(0, 0.5), cex=1.3)

        # sample
        par(mar=c(3.1, 1.1, 0.1, 1.1))
        new_samp_2 <- sample(new_samp, replace=TRUE)
        new_samp_2 <- new_samp_2[new_samp_2 <= mx]
        hist(new_samp_2, breaks=seq(0, mx, len=61), main="", yaxt="n", xlab="", ylab="", xaxs="i", prob=TRUE)
        u <- par("usr")
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
        qu_boot <- replicate(1000, quantile(sample(new_samp, replace=TRUE), 0.95))

        par(mar=c(3.1, 1.1, 0.1, 1.1))
        hist(qu_boot, breaks=seq(0, mx, len=121), main="", yaxt="n", xlab="", ylab="", xaxs="i", prob=TRUE)
        u <- par("usr")
        text(sum(u[1:2]*c(0.2,0.8)), sum(u[3:4]*c(0.3,0.7)),
             "Distribution of\nEstimate", col=purple, cex=1.2)
    }

    dev.off()
}

# initial figure with just the left side
nested_bootstrap(1)

# add simulation stuff on the right
nested_bootstrap(2)


# nested bootstrap results
file <- "_cache/nested_bootstrap.RData"
if(!dir.exists("_cache")) dir.create("_cache")
if(file.exists(file)) {
    load(file)
} else {
    library(parallel)
    cores <- detectCores()

    m <- 5
    df <- 6
    n <- 1000
    set.seed(20200315)
    samp <- rchisq(n, df)*m

    RNGkind("L'Ecuyer-CMRG")
    set.seed(20200316)

    results <- mclapply(1:1000,
                        function(i) {

        new_samp <- sample(samp, replace=TRUE)
        qu_boot <- replicate(1000, quantile(sample(new_samp, replace=TRUE), 0.95))
        list(boot_qu=quantile(new_samp, 0.95), boot_se=sd(qu_boot)) }, mc.cores=cores)

    boot_qu <- sapply(results, "[[", "boot_qu")
    boot_se <- sapply(results, "[[", "boot_se")

    save(boot_qu, boot_se, file=file)
}



pdf("../Figs/nested_bootstrap_3.pdf", height=5.5, width=6.5, pointsize=12)

par(mfrow=c(2,1))

par(mar=c(3.1, 1.1, 0.1, 1.1))
hist(boot_qu, breaks=100, main="", yaxt="n", xlab="", ylab="", xaxs="i", prob=TRUE)
u <- par("usr")
text(sum(u[1:2]*c(0.99, 0.01)), sum(u[3:4]*c(0.15,0.85)),
     "1000 simulated quantiles", adj=c(0, 0.5), col=blue, cex=1.3)
text(sum(u[1:2]*c(0.15, 0.85)), sum(u[3:4]*c(0.5,0.5)),
     paste0("SD = ", broman::myround(sd(boot_qu), 2)),
     adj=c(0.5, 0.5), col=blue, cex=1.3)


par(mar=c(3.1, 1.1, 0.1, 1.1))
hist(boot_se, breaks=60, main="", yaxt="n", xlab="", ylab="", xaxs="i", prob=TRUE)
abline(v=sd(boot_qu), col=pink, lwd=2)
abline(v=mean(boot_se), col=blue, lwd=2)
u <- par("usr")
text(sum(u[1:2]*c(0.2, 0.8)), sum(u[3:4]*c(0.5,0.5)),
     "1000 bootstrap SEs", adj=c(0.5, 0.5), col=blue, cex=1.3)
text(mean(boot_se)-0.05,  sum(u[3:4]*c(0.03,0.97)), xpd=TRUE,
     paste0("mean = ", broman::myround(mean(boot_se), 2)),
     adj=c(1, 0.5), col=blue, cex=1.3)

text(sd(boot_qu)+0.05, sum(u[3:4]*c(0.04,0.96)),
     paste0("simulated SD (above) = ", broman::myround(sd(boot_qu), 2)), col=pink, cex=1.3,
     adj=c(0, 0.5))

dev.off()
