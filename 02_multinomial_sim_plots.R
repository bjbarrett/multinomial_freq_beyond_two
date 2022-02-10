library(rethinking)
library(RColorBrewer)
library(plyr)
library(readr)
knitr::opts_chunk$set(echo = TRUE)

####f is 3
#mydir = "/Users/sifaka/Documents/multinomial_freq_beyond_two/model_output/f_3"
mydir = "model_output_log/f_3"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
d3 = ldply(myfiles, read_csv)

master2 <- d3[,-1] # get rid of bs first column
nsims <- ncol(master2) - 3 #number of sims 

N <- sort(unique(master2$n))  ## pop size vector from data
F <-sort(unique(master2$f))  ## strength of frequency dependence from data
K <- sort(unique(master2$k))  ## number of options from data
mypalette<-brewer.pal(max(K),"BuPu")
prior_dist <- exp(rnorm(3000, mean=0, sd=1 ))
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
      dens( zm[index,] , col=mypalette[k_i] , xlim=c(0,7) ,ylim=c(0,3.5) , xlab='',ylab='' , xaxt='n' , yaxt='n' , adj=2) #post of all sims
      dens(prior_dist, from=0, to=10 , add=TRUE , lty=3 , col=1 ) #plot prior
      title(main=textz , line=-.75 , cex.main=0.8)
      #dens(exp(rnorm(1e7 , mean=0 , sd=1)) , lty=2 , add=TRUE)
      abline(v=F[f_i]) #line at true value
      seq_l <- seq(from=0 , to=1.5 , length=nsims) #vertical range to plot hpdi segs
      #f_med_order <- order(apply(zm[index,] , 2 , median)) #order of model f medians by magnitude
      f_med <- apply(zm[index,] , 2 ,  median) #actual medians
      f_hpdi <- apply(zm[index,] , 2 , HPDI , prob=0.50) #actual medians
      for (i in 1:nsims) dens(zm[index,i] , add=TRUE ,  col=col.alpha(mypalette[k_i] , alpha=0.05))
      for(i in 1:nsims){
        segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] ,
                  x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , col.alpha("darkgrey" , alpha=1) , lty=1 , lw=.5)
      }
      points(f_med[order(f_med)] , seq_l , cex=0.3 ,  col=mypalette[k_i] , alpha=0.99 , pch=5)
      #dens( zm[index,] , col=mypalette[k_i] , add=TRUE , lw=3) #post of all sims
      for (i in 1:nsims) dens(zm[index,i] , add=TRUE ,  col=col.alpha(mypalette[k_i] , alpha=0.15))
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
mydir = "model_output_log/f_1"

myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
d3 = ldply(myfiles, read_csv)

master2 <- d3[,-1] # get rid of bs first column
nsims <- ncol(master2) - 3 #number of sims 

#PlotDatShit <- function(master2){
N <- sort(unique(master2$n))  ## pop size vector from data
F <-sort(unique(master2$f))  ## strength of frequency dependence from data
K <- sort(unique(master2$k))  ## number of options from data
mypalette<-brewer.pal(max(K),"Dark2")
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
      f_hpdi <- apply(zm[index,] , 2 , HPDI, prob=0.50) #actual medians
      for (i in 1:nsims) dens(zm[index,i] , add=TRUE ,  col=col.alpha(mypalette[k_i] , alpha=0.15))
      for(i in 1:nsims){
        segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] , 
                  x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , col.alpha("darkgrey" , alpha=1) , lty=1 , lw=.5)
      }
      points(f_med[order(f_med)] , seq_l , cex=0.3 ,  col=mypalette[k_i] , alpha=0.99 , pch=5)
      dens( zm[index,] , col=mypalette[k_i] , add=TRUE , lw=3 , adj=2) #post of all sims
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
      f_hpdi <- apply(zm[index,] , 2 , HPDI, prob=0.50) #actual medians
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


#####################lets do new sims
load("~/Documents/multinomial_freq_beyond_two/100simsK2345.rdata")

drops <- c("choice","n_i","obs")

d2 <- dsim2[ , !(names(dsim2) %in% drops)]
d2 <- d2[!duplicated(d2), ]
d2$entropy <- as.vector(apply(d2[,1:2] ,1, CalcEntropy))
d2$post_f_med <- as.vector(apply(post2$f ,2, median))
d2$loss <- abs(d2$f-d2$post_f_med)

d3 <- dsim3[ , !(names(dsim3) %in% drops)]
d3 <- d3[!duplicated(d3), ]
d3$entropy <- as.vector(apply(d3[,1:3] ,1, CalcEntropy))
d3$post_f_med <- as.vector(apply(post3$f ,2, median))
d3$loss <- abs(d3$f-d3$post_f_med)


d4 <- dsim4[ , !(names(dsim4) %in% drops)]
d4 <- d4[!duplicated(d4), ]
d4$entropy <- as.vector(apply(d4[,1:4] ,1, CalcEntropy))
d4$post_f_med <- as.vector(apply(post4$f ,2, median))
d4$loss <- abs(d4$f-d4$post_f_med)

d5 <- dsim5[ , !(names(dsim5) %in% drops)]
d5 <- d5[!duplicated(d5), ]
d5$entropy <- as.vector(apply(d5[,1:5] ,1, CalcEntropy))
d5$post_f_med <- as.vector(apply(post5$f ,2, median))
d5$loss <- abs(d5$f-d5$post_f_med)

mypalette <- brewer.pal(max(K),"BuPu")


d <- rbind(d2,d3,d4,d5)
post_f <- cbind(post2$log_f, post3$log_f , post4$log_f , post5$log_f)
N <- sort(unique(d$n))  ## pop size vector from data
F <-sort(unique(d$f))  ## strength of frequency dependence from data
K <- sort(unique(d$k))  ## number of options from data
mypalette<-brewer.pal(max(K),"Dark2")
nsims <- 100

plot.new()

for (f_i in 1:length(F)){
  pdf(file = paste("posts_sims_f_",F[f_i],".pdf") , width = 8, height = 8) # The height of the plot in inches
  par( mfrow = c( length(N) , length(K) ) ) 
  par( oma=c(4,0,0,0) +.1)
  par(mar=c(0,0,0,0)+.1)
  for (n_i in 1:length(N)){
    for (k_i in 1:length(K)){
      index <- which(d$f==F[f_i] & d$k==K[k_i] & d$n==N[n_i])
      textz <- paste("k =",K[k_i],"; n =",N[n_i],"; f =",F[f_i] )   #title of plots
      dens( post_f[,index] , col=mypalette[k_i] , xlim=c(-3,3) ,ylim=c(0,8) , xaxt='n' , yaxt='n') #post of all sims
      title(main=textz , line=-.75 , cex.main=0.8 , bg="white")
      abline(v=log(F[f_i]) , lw=2 , col=mypalette[k_i]) #line at true value
      abline(v=0 , lty=3) #line at conform grenze
      seq_l <- seq(from=0 , to=6 , length=nsims) #vertical range to plot hpdi segs
      #f_med_order <- order(apply(zm[index,] , 2 , median)) #order of model f medians by magnitude
      f_med <- apply(post_f[,index] , 2 ,  median) #actual medians
      f_hpdi <- apply(post_f[,index] , 2 , HPDI, prob=0.80) #actual medians
      for (i in index) dens(post_f[,i] , add=TRUE ,  col=col.alpha(mypalette[k_i] , alpha=0.15))
      # for(i in 1:nsims){
      #   segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] , 
      #             x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , col=col.alpha("darkgrey" , alpha=.9) , lty=1 , lw=.5)
      # }
      dens(rnorm(1e7 , mean=0 , sd=1) , lty=2 , add=TRUE)
      points(f_med[order(f_med)] , seq_l , cex=0.3 ,  col=c(mypalette[k_i]) , pch=5)
      
      #dens( post_f[,index] , col=mypalette[k_i] , add=TRUE , lw=3) #post of all sims
      axis(1, at=c(-4:4) , cex.axis=0.8 , tck=0.01 , labels=FALSE )
      axis(1, at=c(-4:4) , cex.axis=0.8 , tck=-0.01 , labels=FALSE )
      if(n_i==length(N)){
        axis(1, at=c(-4:4), labels=c(-4:4) , cex.axis=0.8 )
      }
    }
  }
  mtext(side=1 ,"log(f)", outer = TRUE, line=2.2 )
}

dev.off()
#plot(loss ~ entropy , data=d[d$f==3 & d$n==25 &  d$k==4,] , col=mypalette[2])


