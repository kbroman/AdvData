##############################
# examples of RI genome
##############################

color <- qtl2::CCcolors

source("meiosis_func.R")

library(qtl)
data(map10)
L <- summary(map10)[1:20,2]
#L <- read.csv("/Users/kbroman/Projects/MouseMaps/lengths.csv")[,4]
L[L<51] <- 51
file <- "_cache/ri8_genome.RData"
if(file.exists(file)) {
    load(file)
} else {
    set.seed(122069)
    par <- vector("list",8)
    for(i in 1:8) {
        par[[i]] <- vector("list",20)
        for(j in 1:20) par[[i]][[j]] <- create.par(L[j],i)
    }
    my.ri8 <- vector("list",5)
    for(k in 1:5) {
        if(k==1) wh <- 1:8
        else wh <- sample(1:8)

        f1 <- vector("list",4)
        for(i in 1:4) {
            f1[[i]] <- vector("list",20)
            for(j in 1:20) {
                f1[[i]][[j]] <- cross(par[[wh[2*i-1]]][[j]],par[[wh[2*i]]][[j]],m=10,obl=TRUE,
                                      xchr=(j==20), male=(i %% 2))
            }
        }

        f2 <- vector("list",2)
        for(j in 1:20) {
            f2[[1]][[j]] <- cross(f1[[1]][[j]],f1[[2]][[j]],m=10,obl=TRUE,xchr=(j==20),male=FALSE)
            f2[[2]][[j]] <- cross(f1[[3]][[j]],f1[[4]][[j]],m=10,obl=TRUE,xchr=(j==20),male=TRUE)
        }
        my.ri8[[k]] <- temp <- f2
        flag <- 0; i <- 1
        while(flag == 0) {
            for(j in 1:20) {
                my.ri8[[k]][[1]][[j]] <- cross(temp[[1]][[j]],temp[[2]][[j]],m=10,obl=TRUE,xchr=(j==20),male=FALSE)
                my.ri8[[k]][[2]][[j]] <- cross(temp[[1]][[j]],temp[[2]][[j]],m=10,obl=TRUE,xchr=(j==20),male=TRUE)
            }
            temp <- my.ri8[[k]]
            if(all(sapply(my.ri8[[k]][[1]],function(a) is.null(where.het(a)))) &&
               all(sapply(my.ri8[[k]][[2]][-20],function(a) is.null(where.het(a)))) &&
               length(unlist(my.ri8[[k]][[1]][1:19])) == length(unlist(my.ri8[[k]][[2]][1:19])) &&
               all(unlist(my.ri8[[k]][[1]][1:19]) == unlist(my.ri8[[k]][[2]][1:19])) &&
               length(unlist(my.ri8[[k]][[1]][[20]]$mat)) == length(unlist(my.ri8[[k]][[2]][[20]]$mat))
               &&
               all(unlist(my.ri8[[k]][[1]][[20]]$mat) == unlist(my.ri8[[k]][[2]][[20]]$mat))) {
                flag <- 1
            }
            cat(k,i,"\n")
            i <- i + 1
        }
    }
    save(my.ri8,file=file)
}

for(k in 1:5) {
#  bitmap(file=paste("../Figs/ri8genome",".bmp",sep=as.character(k)), width=9, height=5, res=288,
#         pointsize=14)
  pdf(paste("../Figs/ri8genome",k,".pdf",sep=""), width=9.75, height=5.5,
      pointsize=16, onefile=TRUE)

  par(mar=rep(0.1,4),las=1,bty="n")
  plot(0,0,xlim=c(0,864),ylim=c(0,480),xaxt="n",yaxt="n",xlab="",ylab="",type="n")

  u <- par("usr")
  xcen <- seq(u[1]+3,u[2]-3,len=22)
  xcen <- apply(cbind(xcen[-1],xcen[-22]),1,mean)

  text(xcen,475,c(1:19,"X","Y"),cex=1.2,adj=c(0.5,1))

  temp <- my.ri8[[k]][[2]]
  mult <- 430/max(L)
  for(i in 1:20) {
    rect(xcen[i]-10,430,xcen[i]+10,430-L[i]*mult,col=color[1],border=color[1], lend=1, ljoin=1)

    for(j in 2:ncol(temp[[i]]$mat)) {
      rect(xcen[i]-10,430-temp[[i]]$mat[1,j]*mult,
           xcen[i]+10,430-temp[[i]]$mat[1,j-1]*mult,
           col=color[temp[[i]]$mat[2,j]],
           border=color[temp[[i]]$mat[2,j]], lend=1, ljoin=1)
    }

  }
  rect(xcen[21]-10,430,xcen[21]+10,430-15*mult,col=color[temp[[20]]$pat[2,1]],
       border=color[temp[[20]]$pat[2,1]], lend=1, ljoin=1)

  dev.off()
}
