library(qtl2)
library(qtl2fst)
library(fst)
library(broman)


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


# function to plot missing data
prop_missing <- n_missing(do, "ind", "prop")*100
plot_missing_byind <- function(label=FALSE)
{
    mousenum <- as.numeric(sub("DO", "", names(prop_missing)))
    prop_missing <- prop_missing[order(mousenum)]

    par(mar=c(5.1, 4.1, 1.1, 1.1), cex=1.5, cex.axis=1.3, cex.lab=1.3)
    index <- setNames(seq_along(prop_missing), names(prop_missing))
    color <- qtl2::CCcolors[c(8,2,4,6,7)]
    wave <- as.numeric(do$covar[names(prop_missing),"wave"])

    grayplot(index, prop_missing, bg=color[wave],
             xlab="Mouse", ylab="Percent missing genotypes",
             xlim=c(0, 502), xaxs="i", mgp.x=c(1.9,0.3,0),
             ylim=c(0, 70), yaxs="i")

    if(!label) return()

    to_label <- names(prop_missing)[prop_missing > 10]
    xadj <- setNames(rep(+1, length(to_label)), to_label)
    xadj["DO357"] <- xadj["DO397"] <- -1

    for(i in to_label) {
        text(index[i]-3*xadj[i], prop_missing[i], i,
             adj=c(ifelse(xadj[i] > 0, 1, 0), 0.5))
    }
}

# plot missing
pdf("../Figs/missing_data_per_sample.pdf", height=5.5, width=9.75, pointsize=8)
plot_missing_byind(FALSE)
dev.off()


# plot missing with labels
pdf("../Figs/missing_data_per_sample_labeled.pdf", height=5.5, width=9.75, pointsize=8)
plot_missing_byind(TRUE)
dev.off()
