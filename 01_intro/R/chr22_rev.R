library(broman)

bgcolor <- "white"
text_color <- brocolors("web")["blue"]
point_color <- brocolors("web")["purple"]

pdf("../Figs/chr22rev.pdf", width=10, height=5,
    pointsize=12, onefile=TRUE)
par(mar=c(5.1,4.1,0.1,0.1),lwd=1, las=1)

x <- read.table("chr22.map")
x <- x[!is.na(x[,5]),]
x <- x[order(x[,5]),]

plot(x[,5], x[,2], type="l", lwd=2, xlab="Sequence Position (Mbp)", ylab="Genetic map position (cM)",
     yaxs="i", xaxs="i", xlim=c(15, 48.5), ylim=c(0, 63))
wh <- which(x[,1]=="UT7213")
points(x[wh,5], x[wh,2], col=bgcolor, pch=15)
points(x[wh,5], x[wh,2], lwd=2, col=point_color)
text(x[wh,5], x[wh,2]-4, "D22S534\n(UT7213)", col=text_color)

wh <- which(x[,1]=="UT1963")
points(x[wh,5], x[wh,2], col=bgcolor, pch=15)
points(x[wh,5], x[wh,2], lwd=2, col=point_color)
text(x[wh,5], x[wh,2]+4, "D22S529\n(UT1963)", col=text_color)

wh <- which(x[,1]=="UT5900")
points(x[wh,5], x[wh,2], col=bgcolor, pch=15)
points(x[wh,5], x[wh,2], lwd=2, col=point_color)
text(x[wh,5]-1, x[wh,2]-4, "D22S531\n(UT5900)", col=text_color)

wh <- which(x[,1]=="AFM331wc9")
points(x[wh,5], x[wh,2], col=bgcolor, pch=15)
points(x[wh,5], x[wh,2], lwd=2, col=point_color)
text(x[wh,5]+1, x[wh,2]-4, "D22S1175\n(AFM331wc9)", col=text_color)

dev.off()
