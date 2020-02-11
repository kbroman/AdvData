library(broman)
library(qtl)
data(listeria)

# original scale
listeria_bin <- as.numeric(listeria$pheno[,1]==264)

# transform to log2 scale
listeria$pheno[,1] <- log2(listeria$pheno[,1])

# genome scan
listeria <- calc.genoprob(listeria, step=1, err=0.002)
out <- scanone(listeria, model="2part", upper=TRUE)

chr <- c(1, 5, 13)
loc <- lapply(chr, function(a) max(out, chr=a) )
mar <- sapply(loc, function(a) find.marker(listeria, a$chr, a$pos))
g <- pull.geno(listeria)[,mar]

# sample averages + confidence intervals
mu <- apply(g, 2, function(a) {
    y <- listeria$pheno[,1]
    wh <- (!is.na(y) & y < max(y, na.rm=TRUE))
    tapply(y[wh], a[wh], mean, na.rm=TRUE)})

mu_lo <- apply(g, 2, function(a) {
    y <- listeria$pheno[,1]
    wh <- (!is.na(y) & y < max(y, na.rm=TRUE))
    tapply(y[wh], a[wh], function(a) t.test(a)$conf.int[1])})

mu_hi <- apply(g, 2, function(a) {
    y <- listeria$pheno[,1]
    wh <- (!is.na(y) & y < max(y, na.rm=TRUE))
    tapply(y[wh], a[wh], function(a) t.test(a)$conf.int[2])})


# sample averages + confidence intervals
p <- apply(g, 2, function(a) {
    y <- listeria_bin
    wh <- !is.na(y)
    tapply(y[wh], a[wh], mean, na.rm=TRUE)})

p_lo <- apply(g, 2, function(a) {
    y <- listeria_bin
    wh <- !is.na(y)
    tapply(y[wh], a[wh], function(a) binom.test(sum(a==1), length(a))$conf.int[1])})

p_hi <- apply(g, 2, function(a) {
    y <- listeria_bin
    wh <- !is.na(y)
    tapply(y[wh], a[wh], function(a) binom.test(sum(a==1), length(a))$conf.int[2])})



pdf("../Figs/listeria_effects.pdf", height=5.5, width=9.75, pointsize=12)
par(mar=c(4.1, 3.1, 2.1, 0.6), mfrow=c(2,3))

mu_lim <- c(min(mu_lo), max(mu_hi))
p_lim <- c(min(p_lo), max(p_hi))

rownames(p) <- rownames(mu) <- c("AA", "AB", "BB")

for(i in 1:3) {
    ciplot(mu[,i], lo=mu_lo[,i], hi=mu_hi[,i], ylim=mu_lim,
           xlab="", ylab="")
    title(ylab="log2 survival time", mgp=c(2.1,0,0))
    title(xlab="Genotype",mgp=c(1.8,0,0))
    title(main=paste("Chr", chr[i]))
}

for(i in 1:3) {
    ciplot(p[,i]*100, lo=p_lo[,i]*100, hi=p_hi[,i]*100, ylim=p_lim*100,
           xlab="", ylab="")
    title(ylab="Percent survived", mgp=c(2.1,0,0))
    title(xlab="Genotype",mgp=c(1.8,0,0))
    title(main=paste("Chr", chr[i]))
}

dev.off()
