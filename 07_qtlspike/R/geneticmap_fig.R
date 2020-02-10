library(qtl)
source("my_plot_map.R")


cache_file <- "_cache/sug.rds"
if(file.exists(cache_file)) {
    sug <- readRDS(cache_file)
} else {
    sug <- read.cross("csv", "https://rqtl.org", "sug.csv", genotypes=c("CC", "CB", "BB"))
    saveRDS(sug, cache_file)
}

pdf(file="../Figs/geneticmap.pdf", width=9.75, height=5.5, pointsize=12, onefile=TRUE)

par(mar=c(5.1, 5.1, 1.1, 1.1))
plot.map(sug, main="")
dev.off()
