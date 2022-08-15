library(rethinking)
library(RColorBrewer)

#load("~/Documents/multinomial_freq_beyond_two/100simsK2345_popsame.rdata")
load("~/Documents/multinomial_freq_beyond_two/100simsK2345_differentdistforagentspersim.rdata")

CalcEntropy<- function(x){
  out = -sum(ifelse(x==0, 0 , x*log(x)) )
  return(out)
}

drops <- c("choice","n_i","obs")

##2
d2 <- dsim2[ , !(names(dsim2) %in% drops)]
d2 <- d2[!duplicated(d2), ]
d2$entropy <- as.vector(apply(d2[,1:2] ,1, CalcEntropy))
d2$post_f_med <- as.vector(apply(post2$f ,2, median))
d2$loss <- abs(d2$f-d2$post_f_med)

##3
d3 <- dsim3[ , !(names(dsim3) %in% drops)]
d3 <- d3[!duplicated(d3), ]
d3$entropy <- as.vector(apply(d3[,1:3] ,1, CalcEntropy))
d3$post_f_med <- as.vector(apply(post3$f ,2, median))
d3$loss <- abs(d3$f-d3$post_f_med)

## 4
d4 <- dsim4[ , !(names(dsim4) %in% drops)]
d4 <- d4[!duplicated(d4), ]
d4$entropy <- as.vector(apply(d4[,1:4] ,1, CalcEntropy))
d4$post_f_med <- as.vector(apply(post4$f ,2, median))
d4$loss <- abs(d4$f-d4$post_f_med)

## 5
d5 <- dsim5[ , !(names(dsim5) %in% drops)]
d5 <- d5[!duplicated(d5), ]
d5$entropy <- as.vector(apply(d5[,1:5] ,1, CalcEntropy))
d5$post_f_med <- as.vector(apply(post5$f ,2, median))
d5$loss <- abs(d5$f-d5$post_f_med)

d <- rbind(d2,d3,d4,d5)

post_f <- cbind(post2$log_f, post3$log_f , post4$log_f , post5$log_f)

postposi <- function(x){sum( x > 0 ) / 2000 }
d$perc_positive <- apply( post_f , 2 , postposi )
d$sim_sd <- apply( post_f , 2 , sd )
d$med_log_f <-  apply( post_f , 2 , median )

N <- sort(unique(d$n))  ## pop size vector from data
F <-sort(unique(d$f))  ## strength of frequency dependence from data
K <- sort(unique(d$k))  ## number of options from data
nsims <- 100
pal_names=c("Oranges","Greens","Blues","Purples")

for (f_i in 1:length(F)){
  pdf(file = paste("posts_sims_f_",F[f_i],".pdf") , width = 8, height = 8) # The height of the plot in inches
  par( mfrow = c( length(N) , length(K) ) ) 
  par( oma=c(4,1,1,0) +.2)
  par(mar=c(0,0,0,0)+.1)
  for (n_i in 1:length(N)){
    for (k_i in 1:length(K)){
      mypalette <- brewer.pal(9 , pal_names[k_i])[4:9]
      index <- which(d$f==F[f_i] & d$k==K[k_i] & d$n==N[n_i])
      dens( post_f[,index] , col=mypalette[n_i] , xlim=c(-3,3) ,ylim=c(0,7) , xaxt='n' , yaxt='n') #post of all sims
      abline(v=log(F[f_i]) , lw=2 , col=mypalette[n_i]) #line at true value
      abline(v=0 , lty=3) #line at conform grenze
      seq_l <- seq(from=0 , to=6 , length=nsims) #vertical range to plot hpdi segs

      for (i in index) dens(post_f[,i] , add=TRUE ,  col=col.alpha(mypalette[n_i] , alpha=0.15))
      dens(rnorm(1e7 , mean=0 , sd=1) , lty=2 , add=TRUE)
      axis(1, at=c(-4:4) , cex.axis=0.8 , tck=0.01 , labels=FALSE )
      axis(1, at=c(-4:4) , cex.axis=0.8 , tck=-0.01 , labels=FALSE )
      if(n_i==length(N)){
        axis(1, at=c(-4:4), labels=c(-4:4) , cex.axis=0.8 )
      }
      if(n_i==1){
        mtext(paste("k =",K[k_i]) , line=0 , cex.main=1, bg="white")
      }
      if(k_i==1){
        mtext(paste("n=",N[n_i]) , line=0 , cex.main=1, bg="white" , side=2)
      }
    }
  }
  mtext(side=1 ,"posterior density of log(f)", outer = TRUE, line=2.2 )
  dev.off()
}

#plot(loss ~ entropy , data=d[d$f==3 & d$n==25 &  d$k==4,] , col=mypalette[2])


##alternative with sd bars
for (f_i in 1:length(F)){
  pdf(file = paste("posts_sims_ci_f_",F[f_i],".pdf") , width = 8, height = 8) # The height of the plot in inches
  par( mfrow = c( length(N) , length(K) ) ) 
  par( oma=c(4,1,1,0) +.2)
  par(mar=c(0,0,0,0)+.1)
  for (n_i in 1:length(N)){
    for (k_i in 1:length(K)){
      mypalette <- brewer.pal(9 , pal_names[k_i])[4:9]
      index <- which(d$f==F[f_i] & d$k==K[k_i] & d$n==N[n_i])
      plot( 0 ,0, col="white" , xlim=c(-3,3) ,ylim=c(0,4) , xaxt='n' , yaxt='n') #post of all sims
      abline(v=log(F[f_i]) , lw=2 , col=mypalette[n_i]) #line at true value
      abline(v=0 , lty=3) #line at conform grenze
      seq_l <- seq(from=0 , to=4 , length=nsims) #vertical range to plot hpdi segs
      f_med <- apply(post_f[,index] , 2 ,  median) #actual medians
      f_hpdi <- apply(post_f[,index] , 2 , HPDI, prob=30*pi/100) #actual medians

      for(i in 1:nsims){
        segments( x0=f_hpdi[,order(f_med)[i]][1] , y0=seq_l[i] ,
                  x1=f_hpdi[,order(f_med)[i]][2], y1=seq_l[i] , col=col.alpha("black" , alpha=.7) , lty=1 , lw=.5)
      }
      points(f_med[order(f_med)] , seq_l , cex=0.25 ,  col=c(mypalette[n_i]) , pch=5)
      
      axis(1, at=c(-4:4) , cex.axis=0.8 , tck=0.01 , labels=FALSE )
      axis(1, at=c(-4:4) , cex.axis=0.8 , tck=-0.01 , labels=FALSE )
      if(n_i==length(N)){
        axis(1, at=c(-4:4), labels=c(-4:4) , cex.axis=0.8 )
      }
      if(n_i==1){
        mtext(paste("k =",K[k_i]) , line=0 , cex.main=1, bg="white")
      }
      if(k_i==1){
        mtext(paste("n=",N[n_i]) , line=0 , cex.main=1, bg="white" , side=2)
      }
    }
  }
  mtext(side=1 ,"predicted value of log(f)", outer = TRUE, line=2.2 )
  dev.off()
  
}


# 
# for (f_i in 3:length(F)){
#   for (n_i in 1:length(N)){
#     dens( c(0,1) , col="white" , xlim=c(-2,3) ,ylim=c(0,3.5) , xaxt='n' , yaxt='n' ) #post of all sims
#     for (k_i in 1:length(K)){
#       index <- which(d$f==F[f_i] & d$k==K[k_i] & d$n==N[n_i])
#       textz <- paste( n =N[n_i])   #title of plots
#       dens( post_f[,index] , col=mypalette[k_i] , add=TRUE) #post of all sims
#       title(main=textz , line=-.7 , cex.main=0.8 , bg="white")
#       abline(v=log(F[f_i]) , lw=2 , col=mypalette[k_i]) #line at true value
#       #abline(v=0 , lty=3) #line at conform grenze
#       #dens(rnorm(1e7 , mean=0 , sd=1) , lty=2 , add=TRUE)
#       axis(1, at=c(-4:4), labels=c(-4:4) , cex.axis=0.8 )
# 
#     }
#   }
#   mtext(side=1 ,"posterior density of log(f)", outer = TRUE, line=2.2 )
# }

#plot % of posterior mass > 0 for each sim at each sample size, colors k , panels are samples sizes, point types correspond with type



# for (n_i in 1:length(N)){
#   plot(0,0 , xlim=c(1,100) , ylim=c(0,1) , col="white" , xlab="simulation number" , ylab="propotion of positive posterior samples of log(f)" )
#   for(f_i in 1:length(F)){
#     for (k_i in 1:length(K)){
#       index <- which(d$f==F[f_i] & d$k==K[k_i] & d$n==N[n_i])
#       yy <- d$perc_positive[index]
#       points(1:100 , yy[order(yy)]  , cex=0.5 ,  col=c(mypalette[k_i]) , pch=f_i+15 , type="b")
# 
#     }
#   }
#   textz <- paste("n =",N[n_i] )   #title of plots
#   title(main=textz , line=0.5 , cex.main=1 , bg="white")
# }
# 
# str(d)
# 
# plot(d$loss , col=mypalette[d$k] )
# 
# plot(perc_positive~med_log_f, data=d[d$f==3 & d$n==250,] , col=col.alpha(mypalette[d$k[d$f==3 & d$n==250]] , alpha=0.2) )

# postposi <- function(x){sum( x > 0 ) / 2000 }
# 
# 
# d$perc_positive <- apply( post_f , 2 , postposi )
# d$sim_sd <- apply( post_f , 2 , sd )
# d$med_log_f <-  apply( post_f , 2 , median )
# 
# 
# for (n_i in 1:length(N)){
#   plot(0,0 , xlim=c(1,100) , ylim=c(0,1) , col="white" , xlab="simulation number" , ylab="propotion of positive posterior samples of log(f)" )
#   for(f_i in 1:length(F)){
#     for (k_i in 1:length(K)){
#       index <- which(d$f==F[f_i] & d$k==K[k_i] & d$n==N[n_i])
#       yy <- d$perc_positive[index]
#       points(1:100 , yy[order(yy)]  , cex=0.5 ,  col=c(mypalette[k_i]) , pch=f_i+15 , type="b")
# 
#     }
#   }
#   textz <- paste("n =",N[n_i] )   #title of plots
#   title(main=textz , line=0.5 , cex.main=1 , bg="white")
# }
# 
# str(d)
# 
# plot(d$loss , col=mypalette[d$k])
# 
# plot(perc_positive~med_log_f, data=d[d$f==3 & d$n==250,] , col=mypalette[d$k[d$f==3 & d$n==250]])
