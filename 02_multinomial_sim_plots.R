library(rethinking)
library(RColorBrewer)
library(plyr)
library(readr)
knitr::opts_chunk$set(echo = TRUE)


####f is 3
#mydir = "/Users/sifaka/Documents/multinomial_freq_beyond_two/model_output/f_3"
mydir = "/Users/sifaka/Downloads/rstudio-export-log/f_3"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
d3 = ldply(myfiles, read_csv)

master2 <- d3[,-1] # get rid of bs first column
nsims <- ncol(master2) - 3 #number of sims 

N <- sort(unique(master2$n))  ## pop size vector from data
F <-sort(unique(master2$f))  ## strength of frequency dependence from data
K <- sort(unique(master2$k))  ## number of options from data
mypalette<-brewer.pal(max(K),"Dark2")

plot.new()
par( mfrow = c( length(N) , length(K) ) ) 
par( oma=c(4,0,0,0) +.1)
par(mar=c(0,0,0,0)+.1)
for (f_i in 1:length(F)){
  for (n_i in 1:length(N)){
    for (k_i in 1:length(K)){
      z <- master2[master2$k==K[k_i],] #subset relevant k
      zm <- data.matrix(z[,1:nsims]) #extract parameter estimates
      ##plots
      ylimz <- c(1 , 1.25 , 1.75 , 2 , 2.25 , 2.75 , 3)

      index <- which(z$n==N[n_i]) #get rows for relevant n in loop
      textz <- paste("k =",K[k_i],"; n =",N[n_i],"; f =",F[f_i] )   #title of plots
      dens( zm[index,] , col=mypalette[k_i] , xlim=c(0,8) ,ylim=c(0,3.5) , xlab='',ylab='' , xaxt='n' , yaxt='n') #post of all sims
      curve(dlnorm(x, meanlog=0, sdlog=1), from=0, to=10 , add=TRUE , lty=3 , col=1 ) #plot prior
      title(main=textz , line=-.75 , cex.main=0.8)
      #dens(exp(rnorm(1e7 , mean=0 , sd=1)) , lty=2 , add=TRUE)
      abline(v=F[f_i]) #line at true value
      seq_l <- seq(from=0 , to=1.5 , length=nsims) #vertical range to plot hpdi segs
      #f_med_order <- order(apply(zm[index,] , 2 , median)) #order of model f medians by magnitude
      f_med <- apply(zm[index,] , 2 ,  median) #actual medians
      f_hpdi <- apply(zm[index,] , 2 , HPDI) #actual medians
      #for (i in 1:nsims) dens(zm[index,i] , add=TRUE ,  col=col.alpha(mypalette[k_i] , alpha=0.15))
      for(i in 1:nsims){
        segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] , 
                  x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , col.alpha("darkgrey" , alpha=1) , lty=1 , lw=.5)
      }
      points(f_med[order(f_med)] , seq_l , cex=0.3 ,  col=mypalette[k_i] , alpha=0.99 , pch=5)
      dens( zm[index,] , col=mypalette[k_i] , add=TRUE , lw=3) #post of all sims
      axis(1, at=c(0,1,2,3,4,5,6,7,8) , cex.axis=0.8 , tck=0.01 , labels=FALSE )
      axis(1, at=c(0,1,2,3,4,5,6,7,8) , cex.axis=0.8 , tck=-0.01 , labels=FALSE )
      
      
      if(n_i==length(N)){
        axis(1, at=c(0,1,2,3,4,5,6,7,8), labels=c(0,1,2,3,4,5,6,7,8) , cex.axis=0.8 )
      }
      
    }
  }
}
mtext(side=1 ,"strength of frequency depndence", outer = TRUE, line=2.2 )

####f is 1
mydir = "/Users/sifaka/Documents/multinomial_freq_beyond_two/model_output/f_1"
mydir = "/Users/sifaka/Downloads/rstudio-export-log/f_1"

myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
d3 = ldply(myfiles, read_csv)

master2 <- d3[,-1] # get rid of bs first column
nsims <- ncol(master2) - 3 #number of sims 

N <- sort(unique(master2$n))  ## pop size vector from data
F <-sort(unique(master2$f))  ## strength of frequency dependence from data
K <- sort(unique(master2$k))  ## number of options from data
mypalette<-brewer.pal(max(K),"Dark2")

plot.new()
par( mfrow = c( length(N) , length(K) ) ) 
par( oma=c(4,0,0,0) +.1)
par(mar=c(0,0,0,0)+.1)
for (f_i in 1:length(F)){
  for (n_i in 1:length(N)){
    for (k_i in 1:length(K)){
      z <- master2[master2$k==K[k_i],] #subset relevant k
      zm <- data.matrix(z[,1:nsims]) #extract parameter estimates
      ##plots
      ylimz <- c(1 , 1.25 , 1.75 , 2 , 2.25 , 2.75 , 3)
      
      index <- which(z$n==N[n_i]) #get rows for relevant n in loop
      textz <- paste("k =",K[k_i],"; n =",N[n_i],"; f =",F[f_i] )   #title of plots
      dens( zm[index,] , col=mypalette[k_i] , xlim=c(0,6) ,ylim=c(0,3.5) , xlab='',ylab='' , xaxt='n' , yaxt='n') #post of all sims
      curve(dlnorm(x, meanlog=0, sdlog=1), from=0, to=10 , add=TRUE , lty=3 , col=1 ) #plot prior
      title(main=textz , line=-.75 , cex.main=0.8)
      #dens(exp(rnorm(1e7 , mean=0 , sd=1)) , lty=2 , add=TRUE)
      abline(v=F[f_i]) #line at true value
      seq_l <- seq(from=0 , to=1.5 , length=nsims) #vertical range to plot hpdi segs
      #f_med_order <- order(apply(zm[index,] , 2 , median)) #order of model f medians by magnitude
      f_med <- apply(zm[index,] , 2 ,  median) #actual medians
      f_hpdi <- apply(zm[index,] , 2 , HPDI) #actual medians
      for (i in 1:nsims) dens(zm[index,i] , add=TRUE ,  col=col.alpha(mypalette[k_i] , alpha=0.15))
      for(i in 1:nsims){
        segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] , 
                  x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , col.alpha("darkgrey" , alpha=1) , lty=1 , lw=.5)
      }
      points(f_med[order(f_med)] , seq_l , cex=0.3 ,  col=mypalette[k_i] , alpha=0.99 , pch=5)
      dens( zm[index,] , col=mypalette[k_i] , add=TRUE , lw=3) #post of all sims
      axis(1, at=c(0,1,2,3,4,5,6,7,8) , cex.axis=0.8 , tck=0.01 , labels=FALSE )
      axis(1, at=c(0,1,2,3,4,5,6,7,8) , cex.axis=0.8 , tck=-0.01 , labels=FALSE )
      
      
      if(n_i==length(N)){
        axis(1, at=c(0,1,2,3,4,5,6,7,8), labels=c(0,1,2,3,4,5,6,7,8) , cex.axis=0.8 )
      }
      
    }
  }
}
mtext(side=1 ,"strength of frequency depndence", outer = TRUE, line=2.2 )


####f is 0.33
mydir = "/Users/sifaka/Documents/multinomial_freq_beyond_two/model_output/f_0.33"
mydir = "/Users/sifaka/Downloads/rstudio-export-log/f_0.33"

myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
d3 = ldply(myfiles, read_csv)

master2 <- d3[,-1] # get rid of bs first column
nsims <- ncol(master2) - 3 #number of sims 

N <- sort(unique(master2$n))  ## pop size vector from data
F <-sort(unique(master2$f))  ## strength of frequency dependence from data
K <- sort(unique(master2$k))  ## number of options from data
mypalette<-brewer.pal(max(K),"Dark2")

plot.new()
par( mfrow = c( length(N) , length(K) ) ) 
par( oma=c(4,0,0,0) +.1)
par(mar=c(0,0,0,0)+.1)
for (f_i in 1:length(F)){
  for (n_i in 1:length(N)){
    for (k_i in 1:length(K)){
      z <- master2[master2$k==K[k_i],] #subset relevant k
      zm <- data.matrix(z[,1:nsims]) #extract parameter estimates
      ##plots
      ylimz <- c(1 , 1.25 , 1.75 , 2 , 2.25 , 2.75 , 3)
      
      index <- which(z$n==N[n_i]) #get rows for relevant n in loop
      textz <- paste("k =",K[k_i],"; n =",N[n_i],"; f =",F[f_i] )   #title of plots
      dens( zm[index,] , col=mypalette[k_i] , xlim=c(0,4) ,ylim=c(0,4.5) , xlab='',ylab='' , xaxt='n' , yaxt='n') #post of all sims
      curve(dlnorm(x, meanlog=0, sdlog=1), from=0, to=10 , add=TRUE , lty=3 , col=1 ) #plot prior
      title(main=textz , line=-.75 , cex.main=0.8)
      #dens(exp(rnorm(1e7 , mean=0 , sd=1)) , lty=2 , add=TRUE)
      abline(v=F[f_i]) #line at true value
      seq_l <- seq(from=0 , to=1.5 , length=nsims) #vertical range to plot hpdi segs
      #f_med_order <- order(apply(zm[index,] , 2 , median)) #order of model f medians by magnitude
      f_med <- apply(zm[index,] , 2 ,  median) #actual medians
      f_hpdi <- apply(zm[index,] , 2 , HPDI) #actual medians
      for (i in 1:nsims) dens(zm[index,i] , add=TRUE ,  col=col.alpha(mypalette[k_i] , alpha=0.15))
      for(i in 1:nsims){
        segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] , 
                  x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , col.alpha("darkgrey" , alpha=1) , lty=1 , lw=.5)
      }
      points(f_med[order(f_med)] , seq_l , cex=0.3 ,  col=mypalette[k_i] , alpha=0.99 , pch=5)
      dens( zm[index,] , col=mypalette[k_i] , add=TRUE , lw=3) #post of all sims
      axis(1, at=c(0,1,2,3,4,5,6,7,8) , cex.axis=0.8 , tck=0.01 , labels=FALSE )
      axis(1, at=c(0,1,2,3,4,5,6,7,8) , cex.axis=0.8 , tck=-0.01 , labels=FALSE )
      
      
      if(n_i==length(N)){
        axis(1, at=c(0,1,2,3,4,5,6,7,8), labels=c(0,1,2,3,4,5,6,7,8) , cex.axis=0.8 )
      }
      
    }
  }
}
mtext(side=1 ,"strength of frequency depndence", outer = TRUE, line=2.2 )

