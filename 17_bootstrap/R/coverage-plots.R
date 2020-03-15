library(broman)
pink <- brocolors("web")["fuchsia"]
blue <- brocolors("web")["blue"]
yellow <- brocolors("web")["orage"]


attach("boot-summary.RData")

pdf("../Figs/coveraget.pdf", width=9.75, height=5.5, pointsize=14)

par(mar=c(5.1,4.1,1.1,1.1), las=1)
plot(0:100, boot95cover.vt, type="n", xlab="QTL location (cM)",
     ylab="Coverage (%)", ylim=c(89.9,100), col="black", cex=2, xaxs="i", yaxs="i")

abline(v=c(10,20,30,40,60,70,80,90), col="gray", lty=2)
abline(h=95, col="gray", lty=2)
segments(50,92,50,100, col="gray", lty=2)

lines(0:100,boot95cover.vt, lwd=2)
lines(0:100,bayes95cover.vt, col=blue,lwd=2)
lines(0:100,lod1cover.vt, col=pink,lwd=2)

legend(c(40,60),c(89.9,92), lwd=2, col=c("black",blue,pink),
       c("Bootstrap", "Bayes", "1-LOD"))
dev.off()

pdf("../Figs/coveragethat.pdf", width=9.75, height=5.5, pointsize=14)

par(mar=c(5.1,4.1,1.1,1.1), las=1)
plot(0:100, boot95cover.vthat, type="n", xlab="Estimated QTL location (cM)",
     ylab="Coverage (%)", ylim=c(89.9,100), col="black", cex=2, xaxs="i", yaxs="i")

abline(v=c(10,20,30,40,60,70,80,90), col="gray", lty=2)
abline(h=95, col="gray", lty=2)
segments(50,92,50,100, col="gray", lty=2)

lines(0:100,boot95cover.vthat, lwd=2)
lines(0:100,bayes95cover.vthat, col=blue,lwd=2)
lines(0:100,lod1cover.vthat, col=pink,lwd=2)

legend(c(40,60),c(89.9,92), lwd=2, col=c("black",blue,pink),
       c("Bootstrap", "Bayes", "1-LOD"))
dev.off()


detach(2)
