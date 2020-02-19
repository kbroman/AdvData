read_ghp_output <-
function(file="gh_output.txt")
{
    # read output
    lines <- readLines(file)

    # take the bit after "Totalling" between "position" and first blank line
    totline <- grep("Totalling", lines)
    lines <- lines[-(1:totline)]
    posline <- grep("^position", lines)
    lines <- lines[posline:length(lines)]
    blankline <- which(lines=="")
    lines <- lines[1:(blankline[1]-1)]

    # convert to numeric matrix
    dat <- lapply(strsplit(lines, "\\s+"), function(a) a[a != ""])
    result <- matrix(as.numeric(unlist(dat[-1])), ncol=length(dat[[1]]), byrow=TRUE)
    colnames(result) <- dat[[1]]
    as.data.frame(result)
}
