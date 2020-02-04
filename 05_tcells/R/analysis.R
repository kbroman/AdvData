# analysis of LDA 713 plates 1 and 2

library(npem)
data(p713)

n_rep <- 100
mle <- matrix(ncol=10, nrow=n_rep)
ll <- rep(NA, n_rep)
for(i in 1:n_rep) {
   Sys.sleep(0.5)
   start <- npem.start(unlist(p713$counts[1:2]), n.plates=2)
   out <- npem.em(unlist(p713$counts[1:2]), start, n.plates=2)
   mle[i,] <- out$ests
   ll[i] <- out$loglik
}

omle <- mle[ll==max(ll),]
