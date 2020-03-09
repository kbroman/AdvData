library(negenes)

load("gyanu_results_nov02.RData")
# contains mydata, taloc, finalres

numTAs <- mydata[,1]+mydata[,3]+ c(0,mydata[-nrow(mydata),3])

hit <- as.numeric(mydata[,2]>0 | mydata[,4]>0 |
                  c(mydata[nrow(mydata),4],mydata[-nrow(mydata),4])>0)

out <- negenes(mydata[,1],mydata[,2],mydata[,3],mydata[,4],
               n.mcmc=1000,burnin=0,skip=0, ret=TRUE)
mywh <- sort(sample((1:4204)[hit==0],50))
