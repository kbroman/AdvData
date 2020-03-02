library(xtable)



microbiome <- readRDS("../Data/microbiome_mixups_dist.rds")
tab <- table("Microbiome DO361"=c("A", "B"), "DNA DO361"=c("AA", "BB"))
tab[1,1] <- microbiome$k1["DO361", "DO361"]
tab[2,1] <- (microbiome$n1 - microbiome$k1)["DO361", "DO361"]
tab[1,2] <- (microbiome$n3 - microbiome$k3)["DO361", "DO361"]
tab[2,2] <- microbiome$k3["DO361", "DO361"]
print(xtable::xtable(add_commas(tab), align="rrr"),
    file="../Tabs/dna361_mb361_table.tex", floating=FALSE)






# Microbiome DO360 vs DNA DO360

tab <- table("Microbiome DO360"=c("A", "B"), "DNA DO360"=c("AA", "BB"))
tab[1,1] <- microbiome$k1["DO360", "DO360"]
tab[2,1] <- (microbiome$n1 - microbiome$k1)["DO360", "DO360"]
tab[1,2] <- (microbiome$n3 - microbiome$k3)["DO360", "DO360"]
tab[2,2] <- microbiome$k3["DO360", "DO360"]
print(xtable::xtable(add_commas(tab), align="rrr"),
    file="../Tabs/dna360_mb360_table.tex", floating=FALSE)


tab <- table("Microbiome DO360"=c("A", "B"), "DNA DO370"=c("AA", "BB"))
tab[1,1] <- microbiome$k1["DO370", "DO360"]
tab[2,1] <- (microbiome$n1 - microbiome$k1)["DO370", "DO360"]
tab[1,2] <- (microbiome$n3 - microbiome$k3)["DO370", "DO360"]
tab[2,2] <- microbiome$k3["DO370", "DO360"]
print(xtable::xtable(add_commas(tab), align="rrr"),
    file="../Tabs/dna370_mb360_table.tex", floating=FALSE)
