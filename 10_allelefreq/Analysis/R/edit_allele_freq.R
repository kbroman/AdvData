for(i in 1:22) {
    input_file <- sprintf("../Ghdata/aprpros%02d.loc", i)
    output_file <- sprintf("../Ghdata/aprpros%02d_rev.loc", i)

    lines <- readLines(input_file)

    afreq_lines <- seq(9, length(lines)-4, by=2)

    for(j in afreq_lines) {
        vals <- as.numeric(strsplit(lines[j], "\\s+")[[1]])

        vals[1] <- vals[1] + 0.5
        if(vals[1] > 1) vals[1] <- 0.8
        vals[-1] <- vals[-1]*(1-vals[1])/sum(vals[-1])

        lines[j] <- paste(vals, collapse=" ")
    }

    cat(lines, file=output_file, sep="\n")
}
