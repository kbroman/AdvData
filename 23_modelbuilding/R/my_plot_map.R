plot.map <- 
function (x, map2, chr, horizontal = FALSE, shift = TRUE, show.marker.names = FALSE, 
    ...) 
{
    dots <- list(...)
    if ("main" %in% names(dots)) {
        themain <- dots[["main"]]
        usemaindefault <- FALSE
    }
    else usemaindefault <- TRUE
    map <- x
    if (length(class(map)) > 1 && class(map)[2] == "cross") 
        map <- pull.map(map)
    if (!missing(map2) && length(class(map2)) > 1 && class(map2)[2] == 
        "cross") 
        map2 <- pull.map(map2)
    if (class(map)[1] != "map" || (!missing(map2) && class(map2)[1] != 
        "map")) 
        stop("Input should have class \"cross\" or \"map\".")
    if (!missing(chr)) {
        map <- map[chr]
        if (!missing(map2)) 
            map2 <- map2[chr]
    }
    sex.sp <- FALSE
    if (is.matrix(map[[1]])) {
        one.map <- FALSE
        sex.sp <- TRUE
        if (!missing(map2)) {
            if (is.logical(map2)) {
                horizontal <- map2
                map2 <- lapply(map, function(a) a[2, ])
                map <- lapply(map, function(a) a[1, ])
            }
            else {
                Map1 <- lapply(map, function(a) a[1, , drop = TRUE])
                Map2 <- lapply(map, function(a) a[2, , drop = TRUE])
                Map3 <- lapply(map2, function(a) a[1, , drop = TRUE])
                Map4 <- lapply(map2, function(a) a[2, , drop = TRUE])
                old.mfrow <- par("mfrow")
                on.exit(par(mfrow = old.mfrow))
                par(mfrow = c(2, 1))
                class(Map1) <- class(Map2) <- class(Map3) <- class(Map4) <- "map"
                plot.map(Map1, Map3, horizontal = horizontal, 
                  shift = shift)
                plot.map(Map2, Map4, horizontal = horizontal, 
                  shift = shift)
                return(invisible(NULL))
            }
        }
        else {
            map2 <- lapply(map, function(a) a[2, ])
            map <- lapply(map, function(a) a[1, ])
        }
    }
    else {
        if (!missing(map2)) 
            one.map <- FALSE
        else one.map <- TRUE
    }
    if (one.map) {
        n.chr <- length(map)
        if (!show.marker.names) {
            chrpos <- 1:n.chr
            thelim <- range(chrpos) + c(-0.5, 0.5)
        }
        else {
            chrpos <- seq(1, n.chr * 2, by = 2)
            thelim <- range(chrpos) + c(-0.35, 2.35)
        }
        if (shift) 
            map <- lapply(map, function(a) a - a[1])
        maxlen <- max(unlist(lapply(map, max)))
        if (horizontal) {
            old.xpd <- par("xpd")
            old.las <- par("las")
            par(xpd = TRUE, las = 1)
            on.exit(par(xpd = old.xpd, las = old.las))
            plot(0, 0, type = "n", xlim = c(0, maxlen), ylim = rev(thelim), 
                yaxs = "i", xlab = "Location (cM)", ylab = "Chromosome", 
                yaxt = "n")
            a <- par("usr")
            for (i in 1:n.chr) {
                segments(min(map[[i]]), chrpos[i], max(map[[i]]), 
                  chrpos[i], lwd=2)
                segments(map[[i]], chrpos[i] - 0.25, map[[i]], 
                  chrpos[i] + 0.25, lwd=2)
                if (show.marker.names) 
                  text(map[[i]], chrpos[i] + 0.35, names(map[[i]]), 
                    srt = 90, adj = c(1, 0.5))
            }
            axis(side = 2, at = chrpos, labels = names(map))
        }
        else {
            old.xpd <- par("xpd")
            old.las <- par("las")
            par(xpd = TRUE, las = 1)
            on.exit(par(xpd = old.xpd, las = old.las))
            plot(0, 0, type = "n", ylim = c(maxlen, 0), xlim = thelim, 
                xaxs = "i", ylab = "Location (cM)", xlab = "Chromosome", 
                xaxt = "n")
            a <- par("usr")
            for (i in 1:n.chr) {
                segments(chrpos[i], min(map[[i]]), chrpos[i], 
                  max(map[[i]]), lwd=2)
                segments(chrpos[i] - 0.25, map[[i]], chrpos[i] + 
                  0.25, map[[i]], lwd=2)
                if (show.marker.names) 
                  text(chrpos[i] + 0.35, map[[i]], names(map[[i]]), 
                    adj = c(0, 0.5))
            }
            axis(side = 1, at = chrpos, labels = names(map))
        }
        if (usemaindefault) 
            title(main = "Genetic map")
        else if (themain != "") 
            title(main = themain)
    }
    else {
        if (is.matrix(map2[[1]])) 
            stop("Second map appears to be a sex-specific map.")
        if (length(map) != length(map2)) 
            stop("Maps have different numbers of chromosomes.")
        if (any(sapply(map, length) != sapply(map2, length))) 
            stop("Maps have different numbers of markers.")
        map1 <- map
        if (shift) {
            map1 <- lapply(map1, function(a) a - a[1])
            map2 <- lapply(map2, function(a) a - a[1])
        }
        n.chr <- length(map1)
        maxloc <- max(c(unlist(lapply(map1, max)), unlist(lapply(map2, 
            max))))
        if (!show.marker.names) {
            chrpos <- 1:n.chr
            thelim <- range(chrpos) + c(-0.5, 0.5)
        }
        else {
            chrpos <- seq(1, n.chr * 2, by = 2)
            thelim <- range(chrpos) + c(-0.4, 2.4)
        }
        if (!horizontal) {
            old.xpd <- par("xpd")
            old.las <- par("las")
            par(xpd = TRUE, las = 1)
            on.exit(par(xpd = old.xpd, las = old.las))
            plot(0, 0, type = "n", ylim = c(maxloc, 0), xlim = thelim, 
                xaxs = "i", ylab = "Location (cM)", xlab = "Chromosome", 
                xaxt = "n")
            a <- par("usr")
            for (i in 1:n.chr) {
                if (max(map2[[i]]) < max(map1[[i]])) 
                  map2[[i]] <- map2[[i]] + (max(map1[[i]]) - 
                    max(map2[[i]]))/2
                else map1[[i]] <- map1[[i]] + (max(map2[[i]]) - 
                  max(map1[[i]]))/2
                segments(chrpos[i] - 0.3, min(map1[[i]]), chrpos[i] - 
                  0.3, max(map1[[i]]), lwd=2)
                segments(chrpos[i] + 0.3, min(map2[[i]]), chrpos[i] + 
                  0.3, max(map2[[i]]), lwd=2)
                segments(chrpos[i] - 0.3, map1[[i]], chrpos[i] + 
                  0.3, map2[[i]], lwd=2)
                if (show.marker.names) 
                  text(chrpos[i] + 0.35, map2[[i]], names(map2[[i]]), 
                    adj = c(0, 0.5))
            }
            axis(side = 1, at = chrpos, labels = names(map1))
        }
        else {
            old.xpd <- par("xpd")
            old.las <- par("las")
            par(xpd = TRUE, las = 1)
            on.exit(par(xpd = old.xpd, las = old.las))
            plot(0, 0, type = "n", xlim = c(0, maxloc), ylim = rev(thelim), 
                xlab = "Location (cM)", ylab = "Chromosome", 
                yaxt = "n", yaxs = "i")
            a <- par("usr")
            for (i in 1:n.chr) {
                if (max(map2[[i]]) < max(map1[[i]])) 
                  map2[[i]] <- map2[[i]] + (max(map1[[i]]) - 
                    max(map2[[i]]))/2
                else map1[[i]] <- map1[[i]] + (max(map2[[i]]) - 
                  max(map1[[i]]))/2
                segments(min(map1[[i]]), chrpos[i] - 0.3, max(map1[[i]]), 
                  chrpos[[i]] - 0.3, lwd=2)
                segments(min(map2[[i]]), chrpos[i] + 0.3, max(map2[[i]]), 
                  chrpos[[i]] + 0.3, lwd=2)
                segments(map1[[i]], chrpos[i] - 0.3, map2[[i]], 
                  chrpos[i] + 0.3, lwd=2)
                if (show.marker.names) 
                  text(map2[[i]], chrpos[i] + 0.35, names(map2[[i]]), 
                    srt = 90, adj = c(1, 0.5))
            }
            axis(side = 2, at = chrpos, labels = names(map1))
        }
        if (usemaindefault) {
            if (!sex.sp) 
                title(main = "Comparison of genetic maps")
            else title(main = "Genetic map")
        }
        else if (themain != "") 
            title(main = themain)
    }
    invisible()
}
