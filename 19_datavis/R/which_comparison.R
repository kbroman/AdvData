library(broman)
bgcolor <- "white"
fgcolor <- "black"
pink <- brocolors("web")["fuchsia"]
blue <- brocolors("web")["blue"]

pdf("../Figs/which_comparison.pdf", height=5.2, width=10, pointsize=12)
par(mfrow=c(2,3), bg=bgcolor, col=fgcolor, fg=fgcolor,
    col.axis=fgcolor, col.lab=fgcolor, las=1,
    mar=c(3.1, 3.1, 1.1, 1.1))
ht <- c(83, 114, 36, 67, 114, 154, 137)
names(ht) <- rep("", length(ht))
names(ht)[4:5] <- c("A", "B")
col <- rep(bgcolor, length(ht))
col[4:5] <- c(pink, blue)

barplot(ht, col=col, border=fgcolor, ylim=c(0, max(ht)*1.1))
u <- par("usr")
rect(u[1], u[3], u[2], u[4], border=fgcolor)


o <- c(4, 2, 3, 1, 5, 6, 7)
barplot(ht[o], col=col[o], border=fgcolor, ylim=c(0, max(ht)*1.1))
u <- par("usr")
rect(u[1], u[3], u[2], u[4], border=fgcolor)

htlist <- list(ht[o][1:4], ht[o][5:7])
collist <- list(col[o][1:4], col[o][5:7])
lablist <- list(names(ht)[o][1:4], names(ht)[o][5:7])
plot(0, 0, type="n", xaxt="n", xlab="", ylab="", xlim=c(0.5, 2.5),
     ylim=c(0, max(sapply(htlist, sum))*1.1), yaxs="i", xaxs="i")
xl <- c(0.75, 1.65)
xr <- c(1.35, 2.25)
for(i in 1:2) {
  top <- cumsum(htlist[[i]])
  bot <- cumsum(c(0, htlist[[i]]))
  for(j in seq(along=htlist[[i]])) {
    rect(xl[i], bot[j], xr[i], top[j], border=fgcolor,
         col=collist[[i]][j])
    text((xl[i] + xr[i])/2, (bot[j]+top[j])/2, lablist[[i]][j], col=bgcolor)
  }
}

htlist[[2]] <- rev(htlist[[2]])
collist[[2]] <- rev(collist[[2]])
lablist[[2]] <- rev(lablist[[2]])
plot(0, 0, type="n", xaxt="n", xlab="", ylab="", xlim=c(0.5, 2.5),
     ylim=c(0, max(sapply(htlist, sum))*1.1), yaxs="i", xaxs="i")
xl <- c(0.75, 1.65)
xr <- c(1.35, 2.25)
for(i in 1:2) {
  top <- cumsum(htlist[[i]])
  bot <- cumsum(c(0, htlist[[i]]))
  for(j in seq(along=htlist[[i]])) {
    rect(xl[i], bot[j], xr[i], top[j], border=fgcolor,
         col=collist[[i]][j])
    text((xl[i] + xr[i])/2, (bot[j]+top[j])/2, lablist[[i]][j], col=bgcolor)
  }
}

collist[[1]][2] <- collist[[2]][3]
collist[[2]][3] <- collist[[2]][2]
lablist[[1]][2] <- lablist[[2]][3]
lablist[[2]][3] <- lablist[[2]][2]
plot(0, 0, type="n", xaxt="n", xlab="", ylab="", xlim=c(0.5, 2.5),
     ylim=c(0, max(sapply(htlist, sum))*1.1), yaxs="i", xaxs="i")
xl <- c(0.75, 1.65)
xr <- c(1.35, 2.25)
for(i in 1:2) {
  top <- cumsum(htlist[[i]])
  bot <- cumsum(c(0, htlist[[i]]))
  for(j in seq(along=htlist[[i]])) {
    rect(xl[i], bot[j], xr[i], top[j], border=fgcolor,
         col=collist[[i]][j])
    text((xl[i] + xr[i])/2, (bot[j]+top[j])/2, lablist[[i]][j], col=bgcolor)
  }
}

o <- c(1,2,3,5,6,4,7)
pie(ht[o], col=col[o], border=fgcolor)
dev.off()
