######################################################################
# Autozygosity figure
######################################################################

library(broman)
color2 <- brocolors("web")[c("silver", "purple", "green")]

x <- read.table("chrompic_chr6_fam884.txt", stringsAsFactors=FALSE)
mat <- matrix(unlist(strsplit(x[,4], "")), nrow=nrow(x), byrow=TRUE)

pdf("../Figs/autozygosity.pdf", width=10, height=6,
    pointsize=12, onefile=TRUE)
par(mar=rep(0.1,4),bty="n")
plot(0,0,type="n",xlab="",ylab="",xaxt="n",yaxt="n",
     xlim=c(-50,468),ylim=c(29,-2), xaxs="i", yaxs="i")

n <- ncol(mat)
text(-45, -1, "Maternal chromosomes", col=color2[3], cex=1.5, adj=c(0,0.5))
for(i in 1:12) {
  j <- i*2-1
  thecol <- rep(color2[1],n)
  thecol[mat[j,]=="1" | mat[j,]=="i"] <- color2[2]
  thecol[mat[j,]=="0" | mat[j,]=="o"] <- color2[3]
  segments(1:n-0.5,i,1:n+0.5,i, col=thecol, lend=1, ljoin=1, lwd=15)
  text(-25, i, x[j,2])
}

text(-45, 15, "Paternal chromosomes", col=color2[2], cex=1.5, adj=c(0,0.5))
for(i in 1:12) {
  j <- i*2
  thecol <- rep(color2[1],n)
  thecol[mat[j,]=="1" | mat[j,]=="i"] <- color2[2]
  thecol[mat[j,]=="0" | mat[j,]=="o"] <- color2[3]
  segments(1:n-0.5,i+16,1:n+0.5,i+16, col=thecol, lend=1, ljoin=1, lwd=15)
  text(-25, i+16, x[j,2])
}
dev.off()
