bgcolor <- "white"
fgcolor <- "black"

library(broman)
pink <- brocolors("web")["green"]
blue <- brocolors("web")["purple"]


pdf("../Figs/histograms_1.pdf", height=5.2, width=10, pointsize=12)
par(mfrow=c(2,2), bg=bgcolor, fg=fgcolor, col=fgcolor,
    col.axis=fgcolor, col.lab=fgcolor)
par(mar=c(5.1,1.1,2.1,1.1))
set.seed(85588489)
fem <- rnorm(600, 64, 2.8)
mal <- rnorm(600, 68, 2.8)
jt <- c(fem, mal)
br <- seq(min(jt), max(jt), len=51)
hist(fem, breaks=br, yaxt="n", ylab="", main="Women",
     xlab="Height (in)", col.main=pink)
hist(mal, breaks=br, yaxt="n", ylab="", main="Men",
     xlab="Height (in)", col.main=blue)
hist(mal, breaks=br, yaxt="n", ylab="", main="Men",
     xlab="Height (in)", col.main=blue)
dev.off()




pdf("../Figs/histograms_2.pdf", height=5.2, width=10, pointsize=12)
par(mfcol=c(2,2), bg=bgcolor, fg=fgcolor, col=fgcolor,
    col.axis=fgcolor, col.lab=fgcolor)
par(mar=c(5.1,1.1,2.1,1.1))
hist(fem, breaks=51, yaxt="n", ylab="", main="Women",
     xlab="Height (in)", col.main=pink)
hist(mal, breaks=51, yaxt="n", ylab="", main="Men",
     xlab="Height (in)", col.main=blue)
hist(fem, breaks=br, yaxt="n", ylab="", main="Women",
     xlab="Height (in)", col.main=pink)
hist(mal, breaks=br, yaxt="n", ylab="", main="Men",
     xlab="Height (in)", col.main=blue)
dev.off()
