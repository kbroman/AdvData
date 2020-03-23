bgcolor <- "white"
fgcolor <- "black"
library(broman)
pink <- brocolors("web")["fuchsia"]
blue <- brocolors("web")["blue"]
green <- brocolors("web")["green"]


pdf("../Figs/direct_labels.pdf", height=5.2, width=10, pointsize=12)
data(iris)
par(bg=bgcolor, fg=fgcolor, col.lab=fgcolor, las=1, col.axis=fgcolor)
par(mfrow=c(1, 2))
jitx <- runif(nrow(iris), -0.1, 0.1)
jity <- runif(nrow(iris), -0.02, 0.02)
plot(I(Petal.Width + jity) ~ I(Petal.Length + jitx), data=iris,
     bg=c(blue, green, pink)[as.numeric(Species)],
     xlab = "Petal length (cm)", ylab="Petal width (cm)", pch=21)
legend("topleft", levels(iris$Species), pt.bg=c(blue, green, pink),
       pch=21, col=fgcolor)

plot(I(Petal.Width + jity) ~ I(Petal.Length + jitx), data=iris,
     bg=c(blue, green, pink)[as.numeric(Species)], col=fgcolor,
     xlab = "Petal length (cm)", ylab="Petal width (cm)", pch=21)
text(2.2,  0.4,  levels(iris$Species)[1], adj=c(0, 0.5), col=blue)
text(3.25, 1.4,  levels(iris$Species)[2], adj=c(1, 0.5), col=green)
text(4.7,  2.2,  levels(iris$Species)[3], adj=c(1, 0.5), col=pink)
dev.off()
