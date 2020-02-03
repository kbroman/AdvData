library(npem)
library(broman)

data(p713)

pl <- 1
dat <- matrix(c(p713$counts[[pl]], NA, NA), ncol=4)
dat <- as.list(as.data.frame(dat))
dat <- lapply(dat, function(a) a[!is.na(a)])

estep <-
    function(data, theta, maxk=20)
{
    stopifnot(length(theta) == length(data)+3)
    absig <- theta[-seq_along(data)]

    k <- 0:maxk

    ek <- data
    for(i in seq_along(data)) {
        ek[[i]] <- sapply(data[[i]], function(y) {
            d <- dpois(k, theta[i]) * dnorm(y, absig[1]+absig[2]*k, absig[3])
            sum(k*d)/sum(d)
            })
    }

    ek
}

mstep <-
    function(data, ek)
{
    lambda <- sapply(ek, mean)
    out_lm <- lm(unlist(dat) ~ unlist(ek))

    sig <- sqrt(sum(out_lm$resid^2)/length(unlist(dat)))

    c(lambda, setNames(out_lm$coef, c("a", "b")), sigma=sig)
}

loglik <-
    function(data, theta, maxk=20)
{
    stopifnot(length(theta) == length(data)+3)
    absig <- theta[-seq_along(data)]

    k <- 0:maxk

    ll <- 0
    for(i in seq_along(data)) {
        ll <- ll + sum(sapply(data[[i]], function(y)
            log(sum(dpois(k, theta[i]) * dnorm(y, absig[1]+absig[2]*k, absig[3]))) ))
    }

    ll
}

file <- "_cache/npem_wrong.rds"
if(file.exists(file)) {
    ll <- readRDS(file)
} else {
    n_step <- 25
    ll <- rep(NA, n_step)
    m <- npem.start(unlist(dat))
    ll[1] <- loglik(dat, m)
    for(i in 2:n_step) {
        e <- estep(dat, m)
        m <- mstep(dat, e)
        ll[i] <- loglik(dat, m)
    }
    saveRDS(ll, file)
}


pdf("../Figs/em_loglik_wrong.pdf", height=5.5, width=10)
par(mar=c(5.1,4.1,0.6,0.6))
grayplot(ll, xlab="EM iteration", ylab="log likelihood")
dev.off()
