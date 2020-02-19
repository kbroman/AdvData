# run ghp for a single chromosome

source("read_ghp_output.R")

run_ghp <-
    function(rev_freq=FALSE, chr)
{
    stopifnot(is.logical(rev_freq))
    freq <- ifelse(rev_freq, "_rev", "")
    stopifnot(chr %in% 1:22)

    # create input file
    loc_file <- sprintf("../Ghdata/aprpros%02d%s.loc", chr, freq)
    pre_file <- sprintf("../Ghdata/aprpros%02d.pre", chr)

    dir <- tempfile()
    if(!dir.exists(dir)) {
        dir.create(dir)
    }

    file.copy(loc_file, dir)
    file.copy(pre_file, dir)

    orig_dir <- getwd()
    on.exit(setwd(orig_dir))
    setwd(dir)

    input <- c(paste("load markers ", basename(loc_file)),
               "increment distance 1",
               "analysis npl",
               paste("scan pedigree ", basename(pre_file)),
               "total stat",
               "quit")
    input_file <- "gh_input.txt"
    output_file <- "gh_output.txt"
    cat(input, file=input_file, sep="\n")

    # run genehunter
    system(paste("ghp <", input_file, ">", output_file))

    this_result <- read_ghp_output(output_file)

    # remove temp directory
    unlink(dir, recursive=TRUE)

    # load results
    cbind(data.frame(chr=chr, stringsAsFactors=FALSE),
          this_result)

}
