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

######################################################################

pdf("../Figs/het_byind.pdf", height=5.5, width=9.75, pointsize=8)
par(mfrow=c(2,2), mar=c(5.1, 1.1, 2.1, 1.1), cex=1.5)
for(i in 1:4) {
    expected <- i/8*(8-i)/8*2
    breaks <- seq(expected-0.1, expected+0.1, len=45)

    keep <- names(prop_missing)[prop_missing < 10]

    hist(indfreq_by_maf[[i]][keep,2], breaks=breaks,
         main=paste0("founder MAF = ", i, "/8"),
         yaxt="n", ylab="", xlab="Frequency of minor allele")
    arrows(expected, par("usr")[3], expected, 0, len=0.1, lwd=3, col="violetred")

}
dev.off()


######################################################################

png("../Figs/geno_prob.png", height=800, width=9.75/5.5*800, pointsize=8)
pr_fst <- readRDS("../Data/Genoprobs_fst/attie_DO500_genoprobs_v5_index.rds")
pr_fst <- replace_path(pr_fst, "../Data/Genoprobs_fst/attie_DO500_genoprobs_v5")
gmap <- readRDS("../Data/grid_gmap.rds")
pmap <- readRDS("../Data/grid_pmap.rds")
par(mar=c(5.1,5.1,2.1,0.1), cex=1.8, cex.axis=0.9, cex.lab=1.3, cex.main=1.3)
plot_genoprob(pr_fst, gmap, ind="DO361", chr=3, threshold=0.25,
              xlab="Chr 3 position (cM)", main="DO361", ylab="Genotype")
dev.off()



pdf("../Figs/geno_reconstruct.pdf", height=5.5, width=9.75, pointsize=8)
file <- "_cache/infg_nxo.RData"
if(file.exists(file)) {
    load(file)
} else {
    infg <- maxmarg(pr_fst, minprob=0.5, cores=0)
    infg_ph <<- guess_phase(do, infg, cores=0)
    n_xo <- count_xo(infg, cores=0)
    save(infg, infg_ph, n_xo, file=file)
}
par(mar=c(5.1,5.1,2.1,0.1), cex=1.8, cex.axis=1.1, cex.lab=1.3, cex.main=1.3)
plot_onegeno(infg_ph, gmap, ind="DO361", ylab="Position (cM)", main="DO361")
dev.off()

######################################################################

missing_v_nxo_fig <- function(label=FALSE, subset=FALSE)
{
    par(mar=c(5.1,5.1,1.1,1.1), cex=1.8, cex.axis=1.3, cex.lab=1.3, cex.main=1.3)
    prop_missing <- n_missing(do, "ind", "prop")*100
    tot_xo <- rowSums(n_xo)
    if(subset) {
        tot_xo <- tot_xo[prop_missing < 10]
        prop_missing <- prop_missing[prop_missing < 10]
    }
    grayplot(tot_xo, prop_missing, xlab="Number of crossovers", ylab="Percent missing data")
    if(!label) return()
    if(!subset) {
        to_lab <- names(prop_missing)[prop_missing >= 10]
    } else {
        to_lab <- names(tot_xo)[tot_xo == min(tot_xo) | tot_xo == max(tot_xo) | prop_missing > 5.9]
    }
    xadj <- setNames(rep(ifelse(subset, 5, 50), length(to_lab)), to_lab)
    yadj <- setNames(rep(0, length(to_lab)), to_lab)
    if(!subset) {
        xadj["DO340"] <- xadj["DO340"] * -1
        xadj["DO397"] <- xadj["DO397"] * -1
    } else {
        xadj["DO029"] <- 0; yadj["DO029"] <- 0.4
        xadj["DO524"] <- 0; yadj["DO524"] <- 0.4
    }
    for(i in seq_along(to_lab)) {
        text(tot_xo[to_lab[i]]+xadj[i], prop_missing[to_lab[i]]+yadj[i], to_lab[i],
             adj=c(ifelse(xadj[i] < 0, 1, ifelse(yadj[i] > 0, 0.5, 0)), 0.5))
    }
}


pdf("../Figs/missing_v_nxo.pdf", height=5.5, width=9.75, pointsize=8)
missing_v_nxo_fig(FALSE, FALSE)
dev.off()

pdf("../Figs/missing_v_nxo_labeled.pdf", height=5.5, width=9.75, pointsize=8)
missing_v_nxo_fig(TRUE, FALSE)


pdf("../Figs/missing_v_nxo_subset.pdf", height=5.5, width=9.75, pointsize=8)
missing_v_nxo_fig(FALSE, TRUE)


pdf("../Figs/missing_v_nxo_subset_labeled.pdf", height=5.5, width=9.75, pointsize=8)
missing_v_nxo_fig(TRUE, TRUE)
dev.off()

pdf("../Figs/nxo_by_generation.pdf", height=5.5, width=9.75, pointsize=8)
tot_xo <- rowSums(n_xo)
tot_xo <- tot_xo[tot_xo < 1000]
wave <- as.numeric(do$covar[names(tot_xo), "wave"])
mousenum <- as.numeric(sub("DO", "", names(tot_xo)))
o <- order(mousenum)
color <- qtl2::CCcolors[c(8,2,4,6,7)]
par(mar=c(5.1,5.1,2.1,1.1), cex=1.8, cex.axis=1.3, cex.lab=1.3, cex.main=1.3)
grayplot(seq_along(o), tot_xo[o], bg=color[wave[o]],
         xlab="Mouse number", ylab="No. crossovers", xaxs="i",
         xlim=c(-1.5,500.5))
gen <- tapply(do$covar$gen, do$covar$wave, "[", 1)
text(seq(50, 450, by=100), rep(617, 5), paste("gen", gen),
     xpd=TRUE, col=color)
dev.off()


######################################################################

errlod_file <- "_cache/errlod.rds"
if(file.exists(errlod_file)) {
    errlod <- readRDS(errlod_file)
} else {
    pr <- NULL
    for(chr in c(1:19,"X")) {
        cat(chr, "\n")
        this_pr <- readRDS(glue::glue("~/Projects/AttieDO/CalcGenoProb/attieDO_probs{chr}.rds"))
        if(is.null(pr)) pr <- this_pr
        else pr <- cbind(pr, this_pr)
    }
    errlod <- calc_errorlod(do, pr, cores=0)
    saveRDS(errlod, errlod_file)
}
errlod <- do.call("cbind", errlod)

file <- "_cache/n_err.RData"
if(file.exists(file)) {
    load(file)
} else {
    nerr_byind <- rowSums(errlod > 4)
    nerr_bymar <- colSums(errlod > 4)
    save(nerr_byind, nerr_bymar, file=file)
}


plot_geno_err_byind <- function(subset=FALSE)
{
    par(mar=c(5.1,5.1,1.1,1.1), cex=1.5, cex.lab=1.3, cex.axis=1.3)
    yval <- nerr_byind/n_typed(do)*100
    if(subset) {
        prop_missing <- n_missing(do, "ind", "prop")*100
        yval <- yval[names(prop_missing)[prop_missing < 10]]
    }
    mousenum <- as.numeric(sub("DO", "", names(yval)))
    yval <- yval[order(mousenum)]
    xval <- setNames(seq_along(yval), names(yval))

    if(!subset) ylim <- c(0, 6.2)
    else ylim <- c(0, 0.27)

    grayplot(xval, yval,
             xlab="Mouse", ylab="Percent genotyping errors",
             xlim=c(-2.5, 502.5), xaxs="i", yaxs="i", ylim=ylim,
             mgp.y=c(3.1,0.3,0))

    # labels
    to_lab <- names(yval)[yval > ifelse(subset, 0.15, 0.25)]
    xadj <- setNames(rep(1, length(to_lab)), to_lab)
    if(subset) {
        xadj["DO135"] <- -1
    }
    for(i in to_lab) text(xval[i]+3*xadj[i], yval[i], i, adj=c(ifelse(xadj[i]<0,1,0), 0.5))
}

pdf("../Figs/percent_geno_errors.pdf", height=5.5, width=9.75, pointsize=8)
plot_geno_err_byind(FALSE)
dev.off()


pdf("../Figs/percent_geno_errors_labeled.pdf", height=5.5, width=9.75, pointsize=8)
plot_geno_err_byind(TRUE)
dev.off()
