library(npem)
library(broman)
data(p713)

y <- p713$counts[[1]]

file <- "_cache/multi_starts.RData"
if(file.exists(file)) {
    load(file)
} else {
    set.seed(20200203)
    n_start <- 1000
    starts <- mle <- matrix(nrow=n_start, ncol=7)
    ll <- rep(NA, n_start)
    for(i in 1:n_start) {
        Sys.sleep(0.5)
        starts[i,] <- npem.start(y)
        out <- npem.em(y, starts[i,], maxit=10000)
        mle[i,] <- out$ests
        ll[i] <- out$loglik
    }
    save(starts, mle, ll, file=file)
}

# find unique values

# plot of log likelihood vs attempt
pdf("../Figs/multiple_modes.pdf", height=5.5, width=10)
par(mar=c(3.1,4.1,0.6,0.6))
grayplot(ll, xlab="EM attempt", ylab="log likelihood",
         mgp.x=c(1.6, 0.1, 0), mgp.y=c(3.1, 0.3, 0))
dev.off()

# table with estimates and log likelihood
ll_round <- round(ll, 5)
u_ll <- unique(ll_round)
tab <- matrix(ncol=7+2, nrow=length(u_ll))
colnames(tab) <- c(paste0("lambda_", c("0", "D", "B", "T")),
                   "a", "b", "sig", "loglik", "n_hits")
tab[,"loglik"] <- u_ll
tab[,"n_hits"] <- sapply(u_ll, function(a) sum(ll_round==a))
mle[match(u_ll, ll_round),]
tab[,1:7] <- mle[match(u_ll, ll_round),]
tab <- round(tab,2)
tab <- tab[order(tab[,"loglik"], decreasing=TRUE), ]
xtable::xtable(tab)


pdf("../Figs/starting_pts.pdf", height=5.5, width=10)
par(mfrow=c(2,4), mar=c(3.1, 3.1, 2.1, 0.6))
for(i in 1:7) {
    grayplot(starts[,i], mle[,i],
             main=c(expression(lambda[0]),
                    expression(lambda[D]),
                    expression(lambda[B]),
                    expression(lambda[T]),
                    "a", "b", expression(sigma))[i],
             xlab="Starting point", ylab="Estimate",
             mgp.x=c(1.6, 0.1, 0), mgp.y=c(1.6, 0.3, 0))
}
dev.off()

# proportion of starts that give the best value
