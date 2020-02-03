library(npem)
library(broman)

data(p713)

file <- "_cache/model_param.rds"
if(file.exists(file)) {
    out <- readRDS(file)
} else {
    pl <- 1
    dat <- matrix(c(p713$counts[[pl]], NA, NA), ncol=4)
    dat <- as.list(as.data.frame(dat))
    dat <- lapply(dat, function(a) a[!is.na(a)])

    start <- npem.start(p713$counts[[1]])
    out <- npem.em(p713$counts[[1]], start)
    saveRDS(out, file)
}

maxk <- 5
x <- seq(0, out$ests[5] + out$ests[6]*maxk + 4*out$ests[7], len=251)

y <- matrix(ncol=maxk+1, nrow=length(x))
for(k in 0:maxk) {
    y[,k+1] <- dpois(k, out$ests[2]) * dnorm(x, out$ests[5] + out$ests[6]*k, out$ests[7])
}


pdf("../Figs/model_distribution.pdf", height=2.5, width=10)
par(mar=c(4.1, 0.6, 0.6, 0.6))
grayplot(x, rowSums(y), xlab="square-root response", ylab="", type="l",
         xlim=c(0, 80), lwd=2, xaxs="i", yaxs="i", yat=NA,
         ylim=c(0, max(rowSums(y))*1.05), mgp=c(1.1,0.1, 0))
col <- brocolors("web")[c("blue", "orange", "olive", "purple", "red", "silver")]
for(i in maxk:0) lines(x, y[,i+1], col=col[i+1], lwd=2)
lines(x, rowSums(y), lwd=2)
dev.off()
