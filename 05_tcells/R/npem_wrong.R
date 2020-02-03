library(npem)

data(p713)

pl <- 1
dat <- matrix(c(p713$counts[[pl]], NA, NA), ncol=4)
dat <- as.list(as.data.frame(dat))
dat <- lapply(dat, function(a) a[!is.na(a)])

start <- npem.start(p713$counts[[1]], p713$cells[1])
out <- npem.em(p713$counts[[1]], start, p713$cells[1])


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

n_step <- 100
ll <- rep(NA, n_step)
m <- npem.start(unlist(dat))
ll[1] <- loglik(dat, m)
for(i in 2:n_step) {
    e <- estep(dat, m)
    m <- mstep(dat, e)
    ll[i] <- loglik(dat, m)
}
