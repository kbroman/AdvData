library(plotrix)
gL <- c("Arabidopsis"=0.145, "Human" = 3.2, "Wheat"=17)

library(broman)
green <- brocolors("crayons")["Caribbean Green"]
pink <- brocolors("crayons")["Tickle Me Pink"]
bgcolor <- "white"
fgcolor <- "black"


pdf("../Figs/circles_1.pdf", height=5.2, width=10, pointsize=12)
mult <- sqrt(gL["Human"]/pi)/gL["Human"]
par(mar=rep(0.1, 4), bg=bgcolor, col=fgcolor)
plot(0,0,type="n", xlim=c(0, 25), ylim=c(0, 25), xlab="", ylab="",
     xaxt="n", yaxt="n", xaxs="i", yaxs="i")
draw.circle(20, 20, gL["Wheat"],     nv=251, lwd=2, border="wheat", col="wheat")
text(5/mult, 4/mult, "Wheat (17 Gbp)", col=fgcolor, cex=1.5)
draw.circle( 1,  1,  gL["Arabidopsis"], nv=251, lwd=2, border=green, col=green)
text(1.3, 0.5, "Arabidopsis (0.145 Gbp)", col=green, adj=c(0, 0.5))
draw.circle( 4,  7.5,  gL["Human"],   nv=251, lwd=2, border=pink, col=pink)
text(4, 6, "Human (3.2 Gbp)", col=fgcolor, adj=c(0.5, 0.5))
box()
dev.off()


pdf("../Figs/circles_2.pdf", height=5.2, width=10, pointsize=12)
mx <- 25*mult
par(mar=rep(0.1, 4), bg=bgcolor, col=fgcolor)
plot(0,0,type="n", xlim=c(0, mx), ylim=c(0, mx), xlab="", ylab="",
     xaxt="n", yaxt="n", xaxs="i", yaxs="i")
draw.circle(5, 4, sqrt(gL["Wheat"]/pi),     nv=251, lwd=2, border="wheat", col="wheat")
text(5, 4, "Wheat (17 Gbp)", col=fgcolor, cex=1.5)
draw.circle(mult*0.8, 1.8*mult, sqrt(gL["Arabidopsis"]/pi), nv=251, lwd=2, border=green, col=green)
text(1.3*mult, 0.5*mult, "Arabidopsis (0.145 Gbp)", col=green, adj=c(0, 0.5))
draw.circle(4*mult,  7.5*mult,  sqrt(gL["Human"]/pi),   nv=251, lwd=2, border=pink, col=pink)
text(4*mult, 6*mult, "Human (3.2 Gbp)", col=fgcolor, adj=c(0.5, 0.5))
box()
dev.off()

pdf("../Figs/circles_3.pdf", height=5.2, width=7.2, pointsize=12)
par(mar=c(3.1, 5.1, 0.6, 0.6), bg=bgcolor, las=1, fg=fgcolor,
    col.lab=fgcolor, col.axis=fgcolor)
b <- barplot(gL, col=c(green, pink, "wheat"), ylab="Genome size (Gbp)",
             ylim=c(0, 18), col.lab=fgcolor, yaxs="i", border=c(green, pink, "wheat"),
             names.arg=rep("", 3), cex.lab=1.5, cex.axis=1.5)
u <- par("usr")
rect(u[1], u[3], u[2], u[4], border=fgcolor)
axis(side=1, at=b, names(gL), col.axis=fgcolor, col=fgcolor,
     cex.axis=1.5, tick=FALSE)
dev.off()
