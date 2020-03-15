plot.scanone <- 
function (x, x2, x3, chr, lodcolumn = 3, incl.markers = TRUE, 
    xlim, ylim, lty = 1, col = c("black", "blue", "red"), lwd = 2, 
    add = FALSE, gap = 25, main, mtick = c("line", "triangle"), 
    ...) 
{
    mtick <- match.arg(mtick)
    if (length(dim(x)) != 2) 
        stop("Argument x must be a matrix or data.frame.")
    if (!missing(x2) && length(dim(x2)) != 2) 
        stop("Argument x2 must be a matrix or data.frame.")
    if (!missing(x3) && length(dim(x3)) != 2) 
        stop("Argument x3 must be a matrix or data.frame.")
    if (length(lodcolumn) == 1) 
        lodcolumn <- rep(lodcolumn, 3)[1:3]
    else if (length(lodcolumn) == 2) {
        if (missing(x2)) 
            x2 <- x
        lodcolumn <- lodcolumn[c(1, 2, 3)]
    }
    else {
        if (missing(x2)) 
            x2 <- x
        if (missing(x3)) 
            x3 <- x
    }
    second <- third <- TRUE
    if (missing(x2) && missing(x3)) 
        second <- third <- FALSE
    if (missing(x3)) 
        third <- FALSE
    if (missing(x2)) 
        second <- FALSE
    out <- x[, c(1:2, lodcolumn[1])]
    if (second) 
        out2 <- x2[, c(1:2, lodcolumn[2])]
    if (third) 
        out3 <- x3[, c(1:2, lodcolumn[3])]
    if (length(lty) == 1) 
        lty <- rep(lty, 3)
    if (length(lwd) == 1) 
        lwd <- rep(lwd, 3)
    if (length(col) == 1) 
        col <- rep(col, 3)
    if (missing(chr) || length(chr) == 0) 
        chr <- unique(as.character(out[, 1]))
    else if (all(chr < 0)) {
        a <- sort(unique(out[, 1]))
        chr <- a[-match(-chr, a)]
    }
    u <- is.na(match(chr, unique(out[, 1])))
    if (all(u)) 
        stop("Chromosome(s) to plot were not matched to those in the scanone output.")
    else if (any(u)) {
        warning(paste("Chromosome(s)", chr[u], "were not found.", 
            sep = " "))
        chr <- chr[!u]
    }
    out <- out[!is.na(match(out[, 1], chr)), ]
    if (second) 
        out2 <- out2[!is.na(match(out2[, 1], chr)), ]
    if (third) 
        out3 <- out3[!is.na(match(out3[, 1], chr)), ]
    onechr <- FALSE
    if (length(chr) == 1) {
        gap <- 0
        onechr <- TRUE
    }
    temp <- grep("^c[0-9A-Za-z]+.loc-*[0-9]+", rownames(out))
    if (length(temp) == 0) 
        temp <- out
    else temp <- out[-temp, ]
    begend <- matrix(unlist(tapply(temp[, 2], temp[, 1], range)), 
        ncol = 2, byrow = TRUE)
    rownames(begend) <- unique(out[, 1])
    begend <- begend[as.character(chr), , drop = FALSE]
    len <- begend[, 2] - begend[, 1]
    start <- c(0, cumsum(len + gap)) - c(begend[, 1], 0)
    maxx <- sum(len + gap) - gap
    maxy <- max(out[, 3], na.rm = TRUE)
    if (second) 
        maxy <- max(c(maxy, out2[, 3]), na.rm = TRUE)
    if (third) 
        maxy <- max(c(maxy, out3[, 3]), na.rm = TRUE)
    old.xpd <- par("xpd")
    old.las <- par("las")
    par(xpd = FALSE, las = 1)
    on.exit(par(xpd = old.xpd, las = old.las))
    if (missing(ylim)) 
        ylim <- c(0, maxy)
    if (missing(xlim)) 
        xlim <- c(0, maxx)
    if (!add) {
        if (onechr) {
            plot(0, 0, ylim = ylim, xlim = xlim, type = "n", 
                xlab = "", ylab = "", xaxt="n",
                ...)
        }
        else {
            plot(0, 0, ylim = ylim, xlim = xlim, type = "n",
                xaxt = "n", xlab = "", ylab = "",
                ...)
        }
        if (!missing(main)) 
            title(main = main)
    }
    xtick <- NULL
    xticklabel <- NULL
    for (i in 1:length(chr)) {
        x <- out[out[, 1] == chr[i], 2] + start[i]
        y <- out[out[, 1] == chr[i], 3]
        if (length(x) == 1) {
            g <- max(gap/10, 2)
            x <- c(x - g, x, x + g)
            y <- rep(y, 3)
        }
        lines(x, y, lwd = lwd[1], lty = lty[1], col = col[1])
        if (!add && !onechr) {
            tloc <- mean(c(min(x), max(x)))
            xtick <- c(xtick, tloc)
            xticklabel <- c(xticklabel, as.character(chr[i]))
        }
        if (second) {
            x <- out2[out2[, 1] == chr[i], 2] + start[i]
            y <- out2[out2[, 1] == chr[i], 3]
            if (length(x) == 1) {
                g <- max(gap/10, 2)
                x <- c(x - g, x, x + g)
                y <- rep(y, 3)
            }
            lines(x, y, lty = lty[2], col = col[2], lwd = lwd[2])
        }
        if (third) {
            x <- out3[out3[, 1] == chr[i], 2] + start[i]
            y <- out3[out3[, 1] == chr[i], 3]
            if (length(x) == 1) {
                g <- max(gap/10, 2)
                x <- c(x - g, x, x + g)
                y <- rep(y, 3)
            }
            lines(x, y, lty = lty[3], col = col[3], lwd = lwd[3])
        }
        if (incl.markers && !add) {
            nam <- dimnames(out)[[1]][out[, 1] == chr[i]]
            wh.genoprob <- (seq(along = nam))[grep("^c[0-9A-Za-z]+.loc-*[0-9]+", 
                nam)]
            if (length(wh.genoprob) == 0) 
                wh.genoprob <- seq(along = nam)
            else wh.genoprob <- (seq(along = nam))[-wh.genoprob]
            pos <- out[out[, 1] == chr[i], 2][wh.genoprob] + 
                start[i]
            if (mtick == "line") 
                rug(pos, 0.02, quiet = TRUE, col="white")
            else {
                a <- par("usr")
                points(pos, rep(a[3] + diff(a[3:4]) * 0.04, length(pos)), 
                  pch = 17, cex = 1.5, col="white")
            }
        }
    }
#    if (!add && !onechr) 
#        axis(1, at = xtick, labels = xticklabel)
    invisible()
}
