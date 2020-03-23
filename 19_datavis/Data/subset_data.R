gdp <- read.csv("Data/gdp.csv", comment.char="#")
wh <- which(gdp[,1] == "World")
gdp <- gdp[-(1:wh),]
wh <- which(gdp[,1] == "Austria")
countries <- as.character(gdp[!is.na(gdp$X2012) & gdp$X2012 >= gdp$X2012[wh],1])

health <- read.csv("Data/health_care_spending.csv", comment.char="#")
health <- health[!is.na(match(health[,1], countries)), c("Country.Name", "X2011")]
health[,1] <- as.character(health[,1])
save(health, file="Data/health.RData")
