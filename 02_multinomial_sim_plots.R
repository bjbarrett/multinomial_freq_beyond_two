library(rethinking)
library(RColorBrewer)
knitr::opts_chunk$set(echo = TRUE)
master <- read.csv("f_3_n_10_20_50_nsims_30_fits.csv") #load data
master1 <- read.csv("f_3_n_100_200_500_nsims_30_fits.csv") #load data
master2 <- rbind(master,master1)
master2 <- master2[,-1] # get rid of bs first column
nsims <- ncol(master2) - 3 #number of sims 

N <- sort(unique(master2$n))  ## pop size vector from data
F <-sort(unique(master2$f))  ## strength of frequency dependence from data
K <- sort(unique(master2$k))  ## number of options from data
mypalette<-brewer.pal(max(K),"Dark2")

plot.new()
par( mfrow = c( length(N) , length(K) ) ) 
par(mar=c(0,0,0,0)+.1)
for (f_i in 1:length(F)){
  for (n_i in 1:length(N)){
    for (k_i in 1:length(K)){
      z <- master2[master2$k==K[k_i],] #subset relevant k
      zm <- data.matrix(z[,1:nsims]) #extract parameter estimates
      ##plots
      ylimz <- 3*max(density(zm)$y)
      index <- which(z$n==N[n_i]) #get rows for relevant n in loop
      textz <- paste("k =",K[k_i],"; n =",N[n_i],"; f =",F[f_i] )   #title of plots
      dens( zm[index,] , col=mypalette[k_i] , main=textz ,
            xlim=c(0,8) ,ylim=c(0,1.2) , cex.main=0.8 , xlab='' , yaxt='n' , xaxt='n') #post of all sims
      curve(dlnorm(x, meanlog=0, sdlog=1), from=0, to=10 , add=TRUE , lty=3 , col=1 ) #plot prior
      #dens(exp(rnorm(1e7 , mean=0 , sd=1)) , lty=2 , add=TRUE)
      abline(v=F[f_i]) #line at true value
      seq_l <- seq(from=0 , to=0.35 , length=nsims) #vertical range to plot hpdi segs
      #f_med_order <- order(apply(zm[index,] , 2 , median)) #order of model f medians by magnitude
      f_med <- apply(zm[index,] , 2 ,  median) #actual medians
      f_hpdi <- apply(zm[index,] , 2 ,  HPDI) #actual medians
      for (i in 1:nsims) dens(zm[index,i] , add=TRUE ,  col=col.alpha(mypalette[k_i] , alpha=0.15))
      for(i in 1:nsims){
        segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] , 
                  x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , prob=0.5 , col.alpha("darkgrey" , alpha=1) , lty=1 , lw=.5)
      }
      points(f_med[order(f_med)] , seq_l , cex=0.3 ,  col=mypalette[k_i] , alpha=0.99 , pch=19)
      
      
    }
  }
}



