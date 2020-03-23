# pie charts vs bar charts
library(plotrix)
library(RColorBrewer)

bgcolor <- "white"
fgcolor <- "black"

pdf("../MoreFigs/piecharts.pdf", height=5.5, width=8.5, pointsize=12)
x <- list(18:22, c(20,20,19,21,20))
for(i in 1:2) names(x[[i]]) <- LETTERS[seq(along=x[[i]])]

par(bg=bgcolor, fg=fgcolor)
par(mar=rep(1.1, 4))
par(mfrow=c(2,2))
for(i in 1:2)
  pie(x[[i]], LETTERS[1:5], col=brewer.pal(5, "Set2"))
dev.off()


pdf("../MoreFigs/piecharts_3d.pdf", height=5.5, width=8.5, pointsize=12)
par(bg=bgcolor, fg=fgcolor)
par(mar=rep(1.1, 4))
par(mfrow=c(2,2))
for(i in 1:2)
    pie3D(x[[i]], labels=LETTERS[1:5], explode=0.1, col=brewer.pal(5, "Set2"),
          mar=rep(1,4), labelcex=1)
dev.off()

pdf("../MoreFigs/piecharts_3d_w_bars.pdf", height=5.5, width=8.5, pointsize=12)
par(bg=bgcolor, fg=fgcolor, col.axis=fgcolor)
par(mfrow=c(2,2))
par(mar=rep(1.1, 4))
for(i in 1:2)
    pie3D(x[[i]], labels=LETTERS[1:5], explode=0.1, col=brewer.pal(5, "Set2"),
          mar=rep(1,4), labelcex=1)
par(mar=c(6.1, 4.1, 1.1, 1.1))
for(i in 1:2)
    barplot(x[[i]], names=LETTERS[1:5], col=brewer.pal(5, "Set2"), las=1,
            ylim=c(0, 22))
dev.off()


pdf("../MoreFigs/piecharts_w_bars.pdf", height=5.5, width=8.5, pointsize=12)
par(bg=bgcolor, fg=fgcolor, col.axis=fgcolor)
par(mfrow=c(2,2))
par(mar=rep(1.1, 4))
for(i in 1:2)
  pie(x[[i]], LETTERS[1:5], col=brewer.pal(5, "Set2"))
par(mar=c(6.1, 4.1, 1.1, 1.1))
for(i in 1:2)
    barplot(x[[i]], names=LETTERS[1:5], col=brewer.pal(5, "Set2"), las=1,
            ylim=c(0, 22))
dev.off()
