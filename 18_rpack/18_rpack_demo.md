---
layout: page
title: usethis for R packages
description: Creating an R package in RStudio using the usethis package.
---

I will demonstrate the use of the
[usethis](https://usethis.r-lib.org/) and
[devtools](https://devtools.r-lib.org/) packages to create an R
package.

1. I think it's best to start with some functions that we want to put
   in the R package. We will use a pair of functions: one for simulating
   Brownian motion, and the second for plotting the results.

   ```r
   simBrM <-
   function(n, sigma=1)
   {
       stopifnot(n>=2, sigma>0)

       x <- matrix(rnorm(n*2, 0, sigma), ncol=2)
       colnames(x) <- c("x", "y")

       apply(x, 2, cumsum)
   }

   plot_simBrM <-
   function(x, pointcolor=c("slateblue", "violetred"), ...)
   {
       stopifnot(is.matrix(x), ncol(x)>=2, nrow(x)>=2)

       if(is.null(colnames(x))) colnames(x) <- c("x", "y")

       plot(x[,1], x[,2], xlab=colnames(x)[1], ylab=colnames(x)[2],
            type="l", las=1, ...)

       if(!is.null(pointcolor)) {
           points(x[c(1,nrow(x)), 1], x[c(1,nrow(x)), 2],
                  pch=21, bg=pointcolor)
       }
   }
   ```

   Our goal is to make a package containing these functions.

2. We first use `usethis::create_package()` to create a directory for
   the package plus the skeleton of the files that are needed.
   I'm going to place it on my Desktop (because I'm just going to
   delete it later). But generally I keep my R packages in `~/Code/`.

   ```r
   library(usethis)
   create_package("~/Desktop/simBrM")
   ```
