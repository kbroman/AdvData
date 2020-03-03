# sample qc figs


library(qtl2)
library(qtl2fst)
library(fst)
library(broman)
library(glue)


# load data
x <- read.fst("../Data/attieDO_chrXint.fst")
y <- read.fst("../Data/attieDO_chrYint.fst")
snpint <- read.fst("../Data/attieDO_allint.fst")
do <- readRDS("../Data/attieDO_v0.rds")

# for samples in replicate, figure out which one to keep
tab <- table(do$covar$mouse)
dup_mice <- names(tab[tab>1])
dup_mice <- sprintf("DO%03d", as.numeric(sub("DO", "", dup_mice)))
nm <- n_missing(do, summary="prop")
nm_before <- nm[dup_mice]
nm_after <- nm[paste0(dup_mice, "_b")]
keep <- names(nm_before)[nm_before < nm_after]

# samples to omit
omit <- dup_mice
omit[omit %in% keep] <- paste0(omit[omit %in% keep], "_b")

x <- x[,is.na(match(names(x), omit))]
y <- y[,is.na(match(names(y), omit))]
snpint <- snpint[,is.na(match(names(snpint), omit))]
ids <- ind_ids(do)
do <- do[ids[!(ids %in% omit)],]

# clean up the names
names(x) <- sub("_b$", "", names(x))
names(y) <- sub("_b$", "", names(y))
names(snpint) <- sub("_b$", "", names(snpint))
for(i in seq_along(do$geno)) {
    rownames(do$geno[[i]]) <- sub("_b$", "", rownames(do$geno[[i]]))
}
rownames(do$covar) <- sub("_b$", "", rownames(do$covar))
rownames(do$cross_info) <- sub("_b$", "", rownames(do$cross_info))
names(do$is_female) <- sub("_b$", "", names(do$is_female))

# reorder by mouse number
x <- x[, c(1,1+order(as.numeric(sub("DO", "", names(x)[-1]))))]
y <- y[, c(1,1+order(as.numeric(sub("DO", "", names(y)[-1]))))]
snpint <- snpint[, c(1,2,2+order(as.numeric(sub("DO", "", names(snpint)[-(1:2)]))))]
do <- do[order(as.numeric(sub("DO", "", ind_ids(do)))),]




file <- "_cache/array_intensities_densities.rds"
if(file.exists(file)) {
    d <- readRDS(file)
} else {
    a <- (snpint[seq(1, nrow(snpint), by=2), -(1:2)] +
          snpint[seq(2, nrow(snpint), by=2), -(1:2)])
    a[a>5] <- 5
    d <- apply(a, 2, function(x) density(x[!is.na(x)], from=0, to=5,
                                         window=0.1, n=251, kernel="gaussian")$y)
    colnames(d) <- colnames(a)
    saveRDS(d, file)
}

pdf("../Figs/array_int.pdf", height=5.5, width=9.75, pointsize=8)
wave <- setNames(do$covar$wave, rownames(do$covar))
bad <- paste0("DO", c(306, 308, 309, 340, 357, 397, 274))
dx <- seq(0, 5, len=251)
gray <- "gray65"
layout(rbind(c(6,1,1,2,2,6),c(3,3,4,4,5,5)))
par(mar=c(5.1, 1.1, 2.1, 1.1), cex.lab=2, cex.axis=1.7, cex.main=2)
for(i in 1:5) {
    grayplot(dx, d[,1], type="n", col=gray, main=paste("wave", i),
             xlim=c(0, 5), xaxs="i", ylim=range(d)*1.03, yaxs="i",
             xlab="SNP intensity", yat=NA, hlines=pretty(range(d)), ylab="")
    for(j in names(wave)[wave==i])
        lines(dx, d[,j], col="gray")
    for(j in bad[wave[bad]==i])
        lines(dx, d[,j], col="violetred", lwd=2)
}
dev.off()



######################################################################


# pull out genotype data
g <- do.call("cbind", do$geno[1:19])
fg <- do.call("cbind", do$founder_geno[1:19])

# omit markers with any missing founder genotypes
g <- g[,colSums(fg==0)==0]
fg <- fg[,colSums(fg==0)==0]

# 0 -> NA
g[g==0] <- NA

# recode so 1 = major allele
af_fg <- colSums(fg==1)
fg[,af_fg > 4] <- 4-fg[,af_fg > 4]
g[,af_fg > 4] <- 4-g[,af_fg > 4]
af_fg <- colSums(fg==1)

# genotype frequencies by marker class
af_gind <- matrix(ncol=4, nrow=nrow(g))
dimnames(af_gind) <- list(rownames(g), 1:4)
for(i in 1:4) {
    af_gind[,i] <- rowMeans((g[,af_fg==i] - 1)/2, na.rm=TRUE)
}

af_gmar <- colMeans((g-1)/2, na.rm=TRUE)


prop_missing <- n_missing(do, "ind", "prop")*100

plot_allele_freq_byind <- function(subset=FALSE, density=FALSE)
{
    these_af <- af_gind
    if(subset) {
        these_af <- af_gind[prop_missing < 10, ]
    }

    par(mfrow=c(2,2), mar=c(5.1, 1.1, 2.1, 1.1), cex=1.5)
    for(i in 1:4) {
        if(subset) breaks <- seq(i/8-0.05, i/8+0.05, len=45) else breaks <- 80

        yli <- range(hist(1-these_af[,i], breaks=breaks, plot=FALSE)$density)

        if(density) {
            u <-par("usr")[1:2]
            u <- seq(min(breaks), max(breaks), len=251)
            dn <- dnorm(u, i/8, sqrt(i/8*(1-i/8) / sum(af_fg==i)))
            yli <- range(c(yli, dn))
        }

        hist(1-these_af[,i], breaks=breaks, ylim=yli,
             main=paste0("founder MAF = ", i, "/8"),
             yaxt="n", ylab="", xlab="Frequency of minor allele", prob=TRUE)
        arrows(i/8, par("usr")[3], i/8, 0, len=0.1, lwd=3, col="violetred")

        if(density) lines(u, dn, lwd=2, col="slateblue")
    }
}

pdf("../Figs/allele_freq_byind.pdf", height=5.5, width=9.75, pointsize=12)
plot_allele_freq_byind(FALSE)
dev.off()

pdf("../Figs/allele_freq_byind_fixrange.pdf", height=5.5, width=9.75, pointsize=12)
plot_allele_freq_byind(TRUE)
dev.off()


######################################################################

file <- "_cache/genofreq.RData"
if(file.exists(file)) {
    load(file)
} else {
    # grab data
    g <- do.call("cbind", do$geno[1:19])
    fg <- do.call("cbind", do$founder_geno[1:19])
    g[g==0] <- NA

    # drop markers with missing founder genotypes
    keep <- colSums(fg==0)==0
    g <- g[,keep]
    fg <- fg[,keep]

    # make 1 the minor allele in all cases
    n3 <-  colSums(fg==3)
    g[,n3<4] <- 4-g[,n3<4]
    fg[,n3<4] <- 4-fg[,n3<4]
    n1 <-  colSums(fg==1)

    indfreq <- rbind(rowSums(g==1, na.rm=TRUE),
                     rowSums(g==2, na.rm=TRUE),
                     rowSums(g==3, na.rm=TRUE))
    marfreq <- rbind(colSums(g==1, na.rm=TRUE),
                     colSums(g==2, na.rm=TRUE),
                     colSums(g==3, na.rm=TRUE))

    indfreq <- t(indfreq)/colSums(indfreq)
    marfreq <- t(marfreq)/colSums(marfreq)

    indfreq_by_maf <- vector("list", 4)
    for(i in 1:4) {
        gg <- g[,n1 == i]
        indfreq_by_maf[[i]] <- rbind(rowSums(gg==1, na.rm=TRUE),
                                     rowSums(gg==2, na.rm=TRUE),
                                     rowSums(gg==3, na.rm=TRUE))
        indfreq_by_maf[[i]] <- t(indfreq_by_maf[[i]])/colSums(indfreq_by_maf[[i]])
    }
    save(indfreq, marfreq, indfreq_by_maf, n1, file=file)
}

geno_freq_byind <-
function(label=FALSE)
{
    require(broman)

    mycolors <- paste0(brocolors("web")[c("purple", "green", "blue", "black")], "33")

    layout(rbind(c(5,1,1,2,2,6),c(7,3,3,4,4,8)))
    par(cex=1.5)
    for(i in 1:4) {
        par(mar=rep(0,4), bty="n")
        triplot(c("AA", "AB", "BB"))
        pts <-tripoints(indfreq_by_maf[[i]], cex=0.8, pch=16, col=mycolors[i])
        u <- par("usr")
        text(u[1:2] %*% c(0.4, 0.6), u[3:4] %*% c(0.25, 0.75),
             paste0("founder MAF = ", i, "/8"), adj=c(0, 0.5),
             col="slateblue", font=2, cex=0.9)
        if(label) {
            labels <- list(paste0("DO", c(306, 308, 340, 397)),
                           paste0("DO", c(306, 308, 340, 397)),
                           paste0("DO", c(306, 308, 340, 397, 125, 357)),
                           paste0("DO", c(308, 340, 397, 125, 357)))
            dir <- list(c(1,1,-1,1),
                        c(1,1,-1,1),
                        c(1,1,-1,1,1,-1),
                        c(1,-1,-1,1,-1))


            for(j in seq_along(labels[[i]])) {
               lab <- labels[[i]][j]
               text(pts[lab,1]+0.04*dir[[i]][j], pts[lab,2], lab, adj=c(ifelse(dir[[i]][j]==1,0,1), 0.5))
            }
        }
    }
}

pdf("../Figs/geno_freq_byind.pdf", height=5.5, width=9.75, pointsize=8)
geno_freq_byind(FALSE)
dev.off()

pdf("../Figs/geno_freq_byind_labeled.pdf", height=5.5, width=9.75, pointsize=8)
geno_freq_byind(TRUE)
dev.off()
