library(npem)
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
# table with estimates and log likelihood
# proportion of starts that give the best value
