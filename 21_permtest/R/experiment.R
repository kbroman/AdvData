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

out <- t.test(as.numeric(dat$resp) ~ dat$ttt)
t_stat <- -out$stat




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
    text(33, 45, paste("=", myround(-out$stat, 2)), adj=c(0, 0.5), cex=1.5)
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



# permutations
perm_file <- "_cache/perms.rds"
if(file.exists(perm_file)) {
    perm_t <- readRDS(perm_file)
} else {
    perm_t <- replicate(10000, t.test(sample(dat$resp) ~ dat$ttt)$stat)
    saveRDS(perm_t, perm_file)
}

pdf("../Figs/perm_hist.pdf", height=5, width=10)
par(mar=c(4.1, 1.1, 1.1, 1.1))
xmax <- max(abs(perm_t))
hist(perm_t, xlab="t statistic", ylab="", yaxt="n", main="",
     prob=TRUE, breaks=seq(-xmax, xmax, len=201))
dev.off()

pdf("../Figs/perm_hist_with_t.pdf", height=5, width=10)
par(mar=c(4.1, 1.1, 1.1, 1.1))
xmax <- max(abs(perm_t))
hist(perm_t, xlab="t statistic", ylab="", yaxt="n", main="",
     prob=TRUE, breaks=seq(-xmax, xmax, len=201))
u <- par("usr")
x <- seq(-xmax, xmax, len=251)
lines(x, dt(x, 22), lwd=2, col=green)
dev.off()

pdf("../Figs/perm_hist_with_arrow.pdf", height=5, width=10)
par(mar=c(4.1, 1.1, 1.1, 1.1))
xmax <- max(abs(perm_t))
hist(perm_t, xlab="t statistic", ylab="", yaxt="n", main="",
     prob=TRUE, breaks=seq(-xmax, xmax, len=201))
u <- par("usr")
x <- seq(-xmax, xmax, len=251)
lines(x, dt(x, 22), lwd=2, col=green)

arrows(t_stat, u[3]+diff(u[3:4])*0.25, t_stat,  u[3]+diff(u[3:4])*0.08, col=purple, lwd=3, len=0.1)
text(t_stat, u[3]+diff(u[3:4])*0.28, myround(t_stat, 2), cex=1.5, adj=c(0.5, 0), col=purple)
dev.off()




pdf("../Figs/block_experiment.pdf", height=5, width=10, pointsize=14)
par(mar=rep(1.1, 4))
plot(0,0, type="n", xlim=c(0, 100), ylim=c(0, 105), xaxs="i", yaxs="i",
     xaxt="n", yaxt="n", bty="n")
x <- c(2, 48, 52, 98)

text(mean(x[1:2]), 102.5, "Treatment groups", cex=1.3, xpd=TRUE, font=2)
text(mean(x[3:4]), 102.5, "Responses", cex=1.3, xpd=TRUE, font=2)


dat <- list(ttt=matrix(rep(c("C","T"), 3*4), ncol=4),
            resp=matrix(rnorm(3*4*2, 20, 5), ncol=4))
dat$block <- (row(dat$ttt)+1) %/% 2 + ((col(dat$ttt)-1) %/% 2)*3
ublock <- unique(dat$block)
for(i in ublock) {
    dat$ttt[dat$block==i] <- sample(dat$ttt[dat$block==i])
}
dat$resp[dat$ttt=="T"] <- dat$resp[dat$ttt=="T"] + 4
blockeff <- rnorm(max(ublock), 0, 10)
dat$resp <- dat$resp + blockeff[dat$block]
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

    xxsub <- xx[seq(1, length(xx), by=2)]
    yysub <- yy[seq(1, length(yy), by=2)]
    segments(xxsub, ymin, xxsub, ymax, lwd=4)
    segments(xmin, yysub, xmax, yysub, lwd=4)


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


### perms that are stratified by block vs not

perm_dat <-
    function(dat, by_block=TRUE)
    {
        if(by_block) {
            ublock <- unique(dat$block)
            for(i in ublock) {
                dat$ttt[dat$block==i] <- sample(dat$ttt[dat$block==i])
            }
        } else {
            dat$ttt <- matrix(sample(dat$ttt), ncol=4)
        }

        dat
    }

anal_byblock <-
    function(dat)
    {
        anova(aov(as.numeric(dat$resp) ~ as.factor(dat$ttt) + as.factor(dat$block)))[1,4]
    }


block_perms_file <- "_cache/block_perms.rds"
if(file.exists(block_perms_file)) {
    block_perms <- readRDS(block_perms_file)
} else {
    block_perms <- list(by_block=replicate(10000, anal_byblock(perm_dat(dat))),
                        overall=replicate(10000, anal_byblock(perm_dat(dat, FALSE))))

    saveRDS(block_perms, block_perms_file)
}

obs <- anal_byblock(dat)


pdf("../Figs/hist_block_perms.pdf", height=5, width=8, pointsize=14)

par(mar=c(4.1, 0.1, 1.6, 0.1), mfrow=c(2,1))

for(i in 1:2) {
    hist(block_perms[[i]], breaks=seq(0, max(unlist(block_perms)), len=401), xlim=c(0,10),
         xlab="F statistic", ylab="", yaxt="n", mgp=c(2.1, 0.8, 0),
         main=paste(c("Stratified", "Normal")[i], "permutations"))

    u <- par("usr")
    arrows(obs, u[3]+diff(u[3:4])*0.25, obs, u[3]+diff(u[3:4])*0.06, col=purple, lwd=2, len=0.1)
    text(obs, u[3]+diff(u[3:4])*0.34, myround(obs, 1), col=purple)

    text(obs+1, u[3]+diff(u[3:4])*0.34, paste0("(P = ", myround(mean(block_perms[[i]] >= obs), 3), ")"), col=purple)

}

dev.off()




pdf("../Figs/qqplot_block_perms.pdf", height=5, width=5, pointsize=14)

par(mar=c(4.1, 4.1, 0.6, 0.6))

ymx <- max(unlist(block_perms))

qqplot(block_perms[[1]], block_perms[[2]],
       xlab="F stat, stratified perms",
       ylab="F stat, normal perms",
       las=1, pch=21, bg="lightblue", cex=0.5,
       xlim=c(0, ymx), ylim=c(0, ymx))
qqline2(log2(block_perms[[1]]), log2(block_perms[[2]]), lwd=2, col=green)

dev.off()
