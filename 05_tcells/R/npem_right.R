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

    ek <- eksq <- data
    for(i in seq_along(data)) {
        for(j in seq_along(data[[i]])) {
            d <- dpois(k, theta[i]) * dnorm(data[[i]][j], absig[1]+absig[2]*k, absig[3])
            sum_d <- sum(d)
            ek[[i]][j] <- sum(k*d)/sum_d
            eksq[[i]][j] <- sum(k^2*d)/sum_d
        }
    }

    list(ek=ek, eksq=eksq)
}

mstep <-
    function(data, ek_eksq)
{
    ek <- ek_eksq[[1]]
    eksq <- ek_eksq[[2]]

    n <- length(unlist(ek))

    XpX <- cbind(c(n, sum(unlist(ek))),
                 c(sum(unlist(ek)), sum(unlist(eksq))))
    XpY <- c(sum(unlist(data)), sum(unlist(data)*unlist(ek)))

    lambda <- sapply(ek, mean)

    ab <- solve(XpX) %*% XpY

    sig <- sqrt((sum(unlist(data)^2) - t(XpY) %*% ab)/n)

    c(lambda, setNames(ab, c("a", "b")), sigma=sig)
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
