# a bunch of qtl mapping figs

library(qtl)
library(broman)
blue <- brocolors("web")["blue"]
data(fake.f2)
set.seed(20200406)

fake.f2 <- calc.genoprob(fake.f2, step=1)

out <- scanone(fake.f2, method="hk")

operm_out <- vector("list", 4)
temp <- fake.f2
for(i in 1:4) {
    temp$pheno <- temp$pheno[sample(nind(temp)),]
    operm_out[[i]] <- scanone(temp, method="hk")
}


operm_file <- "_cache/qtl_perms.rds"
if(file.exists(operm_file)) {
    operm <- readRDS(operm_file)
} else {
    operm <- scanone(fake.f2, method="hk", n.perm=1000)
    saveRDS(operm, operm_file)
}


# genotypes + phenotypes

pdf("../Figs/geno_and_pheno.pdf", height=5, width=10, pointsize=14)

layout(cbind(1,2), width=c(8,1))

par(mar=c(4.1, 3.1, 2.1, 1.1))
geno.image(fake.f2, reorder=1, main="")

par(mar=c(4.1, 0.1, 2.1, 3.1))
image(1, seq_len(nind(fake.f2)), matrix(sort(fake.f2$pheno[,1]),nrow=1),
      col=revgray(), xaxt="n", yaxt="n", xlab="", ylab="")
axis(side=4, las=1)
dev.off()


# QTL results
pdf("../Figs/qtl_scan.pdf", height=5, width=10, pointsize=14)
par(mar=c(5.1,4.1,1.1,1.1))
plot(out, bandcol="gray80", bgrect="gray93", col=blue)
dev.off()


# QTL results + permutation results
for(i in 1:4) {
    pdf(paste0("../Figs/perm_scan_", i, ".pdf"), height=5, width=10, pointsize=14)
    par(mar=c(5.1,4.1,1.1,1.1))
    plot(out, bandcol="gray80", bgrect="gray93", col=blue)
    for(j in 1:i) {
        plot(operm_out[[j]], col="gray70", add=TRUE)
    }
    plot(operm_out[[i]], col="gray40", add=TRUE)
    dev.off()
}


# permutation histogram
pdf("../Figs/qtl_perm_results.pdf", height=5, width=10, pointsize=14)
par(mar=c(5.1, 1.1, 1.1, 1.1))
hist(as.numeric(operm), breaks=seq(0, max(operm), len=2*sqrt(1000)),
     main="", ylab="", yaxt="n", xlab="Maximum LOD score")
dev.off()
