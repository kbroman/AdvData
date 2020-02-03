library(npem)
library(broman) # for grayplot()
data(p713)

first <- cumsum(c(1, p713$n))[1:4]
last <- cumsum(p713$n)

response <- matrix(ncol=length(first)+1, nrow=length(p713$counts))
response[,1] <- p713$cells

for(i in seq_along(first)) {
    response[,i+1] <- sapply(p713$counts, function(a) mean(a[first[i]:last[i]]^2))
}


pdf("../Figs/mean_response.pdf", width=10, height=5.5)
par(mfrow=c(2,2), mar=c(4.1,3.6,2.1,0.6))
for(i in 1:4) {
    grayplot(response[,1], response[,i+1], ylim=c(0, ifelse(i>1,4500,500)), yaxs="i",
             xlab="No. cells", ylab="Average response",
             main=c("Cells only", "gD2", "gB2", "Tetox")[i]
             )
}
dev.off()
