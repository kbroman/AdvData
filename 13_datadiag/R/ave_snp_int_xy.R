# ave snp intensity on x vs y chr to identify sex swaps
# also het vs intensity on x chr

library(qtl2)
library(qtl2fst)
library(fst)
library(broman)

##############################
# load data
##############################
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
ids <- ind_ids(do)
do <- do[ids[!(ids %in% omit)],]

# clean up the names
names(x) <- sub("_b$", "", names(x))
names(y) <- sub("_b$", "", names(y))
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


# plot snp intensity on x vs y chr
pdf("../Figs/ave_snp_int_XY.pdf", height=5.5, width=9.75, pointsize=8)
par(mar=c(6.1,5.1,1.6,1.1), cex=1.7, cex.axis=1.3, cex.lab=1.3)
xm <- colMeans(x[,-1], na.rm=TRUE)
ym <- colMeans(y[,-1], na.rm=TRUE)
purple_green <- brocolors("web")[c("purple", "green")]
grayplot(xm, ym, bg=purple_green[do$is_female + 1],
         xlab="ave X chr intensity",
         ylab="ave Y chr intensity")
mtext(side=3, "All markers", adj=0, cex=1.8)
legend("topright", pch=21, pt.bg=purple_green, c("male", "female"), bg="gray90")
dev.off()


file <- "_cache/xymarkers.RData"
if(file.exists(file)) {
    load(file)
} else {
    xp <- apply(x[,-1], 1, function(a) -log10(t.test(a ~ do$is_female)$p.value))
    yp <- apply(y[,-1], 1, function(a) -log10(t.test(a ~ do$is_female)$p.value))
    xmar <- x[xp > -log10(0.05/length(xp)),1]
    ymar <- y[yp > -log10(0.05/length(yp)),1]
    save(xmar, ymar, file=file)
}
xm <- colMeans(x[x[,1] %in% xmar, -1], na.rm=TRUE)
ym <- colMeans(y[y[,1] %in% ymar, -1], na.rm=TRUE)
prop_missing <- n_missing(do, "ind", "prop")

yint_vs_xint_selected <-
function(labels=FALSE) {
par(mar=c(6.1,5.1,1.6,1.1), cex=1.7, cex.axis=1.3, cex.lab=1.3)
grayplot(xm, ym, bg=purple_green[do$is_female + 1],
         xlab="ave X chr intensity",
         ylab="ave Y chr intensity")
mtext(side=3, adj=0, cex=1.8, "Selected markers")
legend("topright", pch=21, pt.bg=purple_green, c("male", "female"), bg="gray90")
if(!labels) return()
to_label <- c(names(sort(xm)[1:2]), # really low X intensity
              names(xm)[xm < 0.4 & ym < 0.1], # low X and Y
              names(xm)[do$is_female & ym > 0.2]) # female but high Y intensity
yadd <- setNames(rep(0.02, length(to_label)), to_label)
yadd["DO357"] <- yadd["DO357"] * -1.7
xadj <- setNames(rep(0.5, length(to_label)), to_label)
xadj["DO153"] <- 1
xadj["DO347"] <- 0
for(i in to_label)
    text(xm[i], ym[i]+yadd[i], i, adj=c(xadj[i], 0.5))
}


pdf("../Figs/ave_snp_int_XY_select.pdf", height=5.5, width=9.75, pointsize=8)
yint_vs_xint_selected(FALSE)
dev.off()


pdf("../Figs/ave_snp_int_XY_select_labeled.pdf", height=5.5, width=9.75, pointsize=8)
yint_vs_xint_selected(TRUE)
dev.off()



prop_het <- rowSums(do$geno$X == 2)/rowSums(do$geno$X != 0)

het_vs_xint <- function(labels=FALSE) {
par(mar=c(6.1,5.1,1.6,1.1), cex=1.7, cex.axis=1.3, cex.lab=1.3)
grayplot(xm, prop_het, bg=purple_green[do$is_female + 1],
         ylab="proportion het on X",
         xlab="ave X chr intensity",
         ylim=c(0,0.85), yaxs="i")
on_top <- (do$is_female & xm < 0.4)
points(xm[on_top], prop_het[on_top], pch=21, bg=purple_green[2])
legend("topright", pch=21, pt.bg=purple_green, c("male", "female"), bg="gray90")
if(!labels) return()
to_label <- c(names(xm)[xm < 0.4 & prop_het > 0.2],
              names(xm)[do$is_female & xm > 0.35 & xm < 0.4])
xadd <- 0.003*c(1,1,1,1,-2.7)
yadd <- 0.035*c(0,0,0,0,1)
xadj <- c(0,0,0,0,0.5)
for(i in seq_along(to_label))
    text(xm[to_label[i]]+xadd[i], prop_het[to_label[i]]+yadd[i], to_label[i],
         adj=c(xadj[i], 0.5))
}





pdf("../Figs/het_vs_Xint.pdf", height=5.5, width=9.75, pointsize=8)
het_vs_xint(FALSE)
dev.off()

pdf("../Figs/het_vs_Xint_labeled.pdf", height=5.5, width=9.75, pointsize=8)
het_vs_xint(TRUE)
dev.off()
