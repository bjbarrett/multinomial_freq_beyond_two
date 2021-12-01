library(rethinking)
library(rstan)
options(mc.cores=3) 
# datalist <- list("post_samples" = f_samples, 
#                  "f" = F[f_i], 
#                  "n" = N[n_i],
#                  "k" = K[k_i]
# )   

##prior predictive simulation of mode; for f
# rln <- rlnorm(1000, meanlog = 0, sdlog = 0.8) 
# curve(dlnorm(x, meanlog=0, sdlog=1), from=0, to=10 , add=TRUE)
# median(rln)
# N1<- seq(from=0 , to=100 , by=1)
# N2<- 100-N1
# FreqN1B4 <- N1/(N1+N2)
# FreqN1After <- rep (0,100)
# plot(FreqN1B4,FreqN1B4 , ylim=c(0,1) , xlim=c(0,1) , ylab="frequency of trait after social learning" , xlab="frequency of trait before social learning",type="n" , bty="n" , cex.lab=1.5)
# for(i in 1:length(rln) ){
#   FreqN1After <- N1^rln[i]/(N1^rln[i]+N2^rln[i])  
#   lines( FreqN1B4,FreqN1After,  col=col.alpha( "darksalmon" ,  alpha=0.05  )  , lwd=1) 
# }
#   FreqN1After <- N1^3/(N1^3+N2^3)  
#   lines( FreqN1B4,FreqN1After,  col=col.alpha( "red"   )  , lwd=3) 
#   FreqN1After <- N1^(1/3)/(N1^(1/3)+N2^(1/3))  
#   lines( FreqN1B4,FreqN1After,  col=col.alpha( "red"   )  , lwd=3) 
# 
# abline(a=0 , b=1 , lty=2)

##begin sims

  #lets do 4 population sizes 5, 10 , 20 ,50, 100, 200 ,500 ,1000
  #fix f to 1/3 , 1 and 3
  #gnomes is 2,  4 , 6 , 8
  ##simulate sl data
N <- c(5, 10 , 20 , 50 )  ## pop size
F <- c( 2) ## strength of frequency dependence
K <- c(2,3,4) ## number of options
n_sims <- 50
#stat sim and model
n_iter=2000
n_chains=3
 for (f_i in 1:length(F)){
   for (n_i in 1:length(N)){
     for (k_i in 1:length(K)){
      # f_i <- 3
      #n_i <- 3
      # k_i <- 2
      f_samples <- array(data=NA , c(n_iter*n_chains*0.5,n_sims) )
      
      ###blank list for data storage
      for (sim in 1:n_sims){
        tweak <- 0.05
        si_freq <- rep(1/K[k_i],K[k_i])
        si_freq[1] <- si_freq[1] + tweak
        si_freq[-1] <-  si_freq[-1] - tweak/(K[k_i]-1) 
        si_freq[K[k_i]+1:max(K)] <- NA
        #if( sum(si_freq[1:k_i])!=1){ print("FUCCCCKKKK, NO SUM TO ONE, DANGER!!!") }  #flag bad initial probs
        s_temp <- rep(0,K[k_i])
        dsim <- data.frame(id=0 , choice=0 , s1=0 , s2=0 , s3=0 , s4=0, sim=0 , f=0 ,n=0 , k=0) #may have to modify if K gets bigger
        therow <- 1
        #begin loop
        for(n in 1:N[n_i]){
          s_temp <- si_freq[1:K[k_i]]^F[f_i]
          pr_choose <- s_temp/sum(s_temp)
          choice <- sample( 1:K[k_i] , size=1 , prob=pr_choose)
          dsim[therow,] <- c( n , choice , si_freq[1] , si_freq[2] , si_freq[3] , si_freq[4] , sim ,F[f_i] ,N[n_i] , K[k_i])
          therow <- therow + 1
        } #n
        

        data <- list(
          K=K[k_i] ,
          N=nrow(dsim),
          choice=dsim$choice,
          s= cbind( dsim[,3:( K[k_i]+2) ] ) 
        )
        

        fit= stan( file = 'freq_dep.stan',
                   data = data ,
                   iter = n_iter,
                   chains=n_chains,
                   cores=n_chains,
                   control=list(adapt_delta=0.999) ,
                   pars=c("f"),
                   refresh=100,
                   init=0
        )


        post <- extract(fit)
        f_samples[,sim] <- post$f
  
      }#sim
      #convert to data frame as i suck at lists
      f_samples2 <- as.data.frame(f_samples)
      f_samples2$f= F[f_i]
      f_samples2$n= N[n_i]
      f_samples2$k= K[k_i]
      if(n_i==1 & f_i==1 & k_i==1){
        master <- f_samples2 
      }else{
        master <- rbind(master,f_samples2) 
      }
     } #f_i
   } #n_i
} #k_i

str(master)


dens(f_samples , show.HPDI=0.99999 , col="grey" , ylim=c(0,.7))
curve(dlnorm(x, meanlog=0, sdlog=1), from=0, to=10 , add=TRUE , lty=2 , col=1)
#for (i in 1:ncol(f_samples)) dens(f_samples[,i] , add=TRUE ,  col=col.alpha("red" , alpha=0.1))
      ## ineed to get simulated data ans posteriors stored
abline(v=1)
str(f_samples)
seq_l <- seq(from=0 , to=0.35 , length=n_sims)
f_med_order <- order(apply(f_samples , 2 , median)) 
f_med_sort <- sort(apply(f_samples , 2 , median)) 
points(f_med_sort , seq_l , cex=0.3)
for(i in 1:n_sims) segments( x0=HPDI(f_samples[,f_med_order[i]])[[1]] , y0=seq_l[i] , x1=HPDI(f_samples[,f_med_order[i]])[[2]], y1=seq_l[i] )









f_samples <- array(data=NA , c(3000,50) )
for (sim in 1:50){
  N <- 20  ## pop size
  f <- 3  ## strength of frequency dependence
  K <- 2 ## number of options
  tweak <- 0.1
  si_freq <- rep(1/K,K)
  si_freq[1] <- si_freq[1] + tweak
  si_freq[-1] <-  si_freq[-1] - tweak/(K-1) 
  if( sum(si_freq)!=1){ print("FUCCCCKKKK, NO SUM TO ONE, DANGER!!!") }  #flag bad initial probs
  s_temp <- rep(0,K)
  dsim <- data.frame(id=0 , choice=0 , s1=0 , s2=0 , s3=0 , s4=0  , s5=0 , s6=0 , sim=0)
  therow <- 1
  #begin loop
  for(n in 1:N){
    for ( k in 1:K ){ s_temp[k] <- si_freq[k]^f}
    pr_choose <- s_temp/sum(s_temp)
    choice <- sample( 1:K , size=1 , prob=pr_choose)
    dsim[therow,] <- c( n , choice , s1=si_freq[1] , si_freq[2] , NA , NA , NA , NA , sim)
    therow <- therow + 1
  }
  
  table(dsim$choice)
  ## fit model
  options(mc.cores=4) 
  library(rstan)
  library(rethinking)
  data <- list(
    K=K ,
    N=nrow(dsim), 
    choice=dsim$choice,
    s= cbind(dsim$s1 , dsim$s2 )
  )
  
  fit= stan( file = '/Users/sifaka/Documents/freq_dep.stan', 
             data = data ,
             iter = 2000, 
             warmup=1000, 
             chains=3, 
             cores=3, 
             control=list(adapt_delta=0.999) , 
             pars=c("f"), 
             refresh=100,
             init=0,
             seed=as.integer(444)
  )
  
  
  summary(fit)
  post <- extract(fit)
  post <- extract(fit)
  f_samples2[,sim] <- post$f
  # dens(post$f)
  # dens(rlnorm(1e5, meanlog = 1, sdlog = 1) , add=TRUE , lty=2 , main="2 options")
  # abline(v=f)
}


###4 options

f_samples4 <- array(data=NA , c(3000,50) )
for (sim in 1:50){
  N <- 20  ## pop size
  f <- 3  ## strength of frequency dependence
  K <- 4 ## number of options
  tweak <- 0.1
  si_freq <- rep(1/K,K)
  si_freq[1] <- si_freq[1] + tweak
  si_freq[-1] <-  si_freq[-1] - tweak/(K-1) 
  if( sum(si_freq)!=1){ print("FUCCCCKKKK, NO SUM TO ONE, DANGER!!!") }  #flag bad initial probs
  s_temp <- rep(0,K)
  dsim <- data.frame(id=0 , choice=0 , s1=0 , s2=0 , s3=0 , s4=0 , s5=0 , s6=0 , sim=0)
  therow <- 1
  #begin loop
  for(n in 1:N){
    for ( k in 1:K ){ s_temp[k] <- si_freq[k]^f}
    pr_choose <- s_temp/sum(s_temp)
    choice <- sample( 1:K , size=1 , prob=pr_choose)
    dsim[therow,] <- c( n , choice , s1=si_freq[1] , si_freq[2] , si_freq[3] , si_freq[4] , NA , NA , sim)
    therow <- therow + 1
  }
  
  ## fit model
  
  data <- list(
    K=K ,
    N=nrow(dsim), 
    choice=dsim$choice,
    s= cbind(dsim$s1 , dsim$s2 , dsim$s3, dsim$s4)
  )
  
  fit= stan( file = '/Users/sifaka/Documents/freq_dep.stan', 
             data = data ,
             iter = 2000, 
             warmup=1000, 
             chains=3, 
             cores=3, 
             control=list(adapt_delta=0.999) , 
             pars=c("f"), 
             refresh=100,
             init=0,
             seed=as.integer(444)
  )
  
  
  post <- extract(fit)
  f_samples4[,sim] <- post$f
  # dens(post$f)
  # dens(rlnorm(1e5, meanlog = 1, sdlog = 1) , add=TRUE , lty=2 , main="2 options")
  # abline(v=f)
}

###6 options

f_samples6 <- array(data=NA , c(3000,50) )
for (sim in 1:50){
  N <- 20  ## pop size
  f <- 3  ## strength of frequency dependence
  K <- 6 ## number of options
  tweak <- 0.1
  si_freq <- rep(1/K,K)
  si_freq[1] <- si_freq[1] + tweak
  si_freq[-1] <-  si_freq[-1] - tweak/(K-1) 
  if( sum(si_freq)!=1){ print("FUCCCCKKKK, NO SUM TO ONE, DANGER!!!") }  #flag bad initial probs
  s_temp <- rep(0,K)
  dsim <- data.frame(id=0 , choice=0 , s1=0 , s2=0 , s3=0 , s4=0 , sim=0)
  therow <- 1
  #begin loop
  for(n in 1:N){
    for ( k in 1:K ){ s_temp[k] <- si_freq[k]^f}
    pr_choose <- s_temp/sum(s_temp)
    choice <- sample( 1:K , size=1 , prob=pr_choose)
    dsim[therow,] <- c( n , choice , s1=si_freq[1] , si_freq[2] , si_freq[3] , si_freq[4] ,si_freq[5] , si_freq[6] , sim)
    therow <- therow + 1
  }
  
  ## fit model
  
  data <- list(
    K=K ,
    N=nrow(dsim), 
    choice=dsim$choice,
    s= cbind(dsim$s1 , dsim$s2 , dsim$s3, dsim$s4,dsim$s5, dsim$s6)
  )
  
  fit= stan( file = '/Users/sifaka/Documents/freq_dep.stan', 
             data = data ,
             iter = 2000, 
             warmup=1000, 
             chains=3, 
             cores=3, 
             control=list(adapt_delta=0.999) , 
             pars=c("f"), 
             refresh=100,
             init=0,
             seed=as.integer(444)
  )
  
  
  post <- extract(fit)
  f_samples6[,sim] <- post$f
  # dens(post$f)
  # dens(rlnorm(1e5, meanlog = 1, sdlog = 1) , add=TRUE , lty=2 , main="2 options")
  # abline(v=f)
}


###4 options

f_samples5 <- array(data=NA , c(3000,50) )
for (sim in 1:100){
  N <- 20  ## pop size
  f <- 3  ## strength of frequency dependence
  K <- 5 ## number of options
  tweak <- 0.1
  si_freq <- rep(1/K,K)
  si_freq[1] <- si_freq[1] + tweak
  si_freq[-1] <-  si_freq[-1] - tweak/(K-1) 
  if( sum(si_freq)!=1){ print("FUCCCCKKKK, NO SUM TO ONE, DANGER!!!") }  #flag bad initial probs
  s_temp <- rep(0,K)
  dsim <- data.frame(id=0 , choice=0 , s1=0 , s2=0 , s3=0 , s4=0 , s5=0 , sim=0)
  therow <- 1
  #begin loop
  for(n in 1:N){
    for ( k in 1:K ){ s_temp[k] <- si_freq[k]^f}
    pr_choose <- s_temp/sum(s_temp)
    choice <- sample( 1:K , size=1 , prob=pr_choose)
    dsim[therow,] <- c( n , choice , s1=si_freq[1] , si_freq[2] , si_freq[3] , si_freq[4] , si_freq[5] , sim)
    therow <- therow + 1
  }
  
  ## fit model
  
  data <- list(
    K=K ,
    N=nrow(dsim), 
    choice=dsim$choice,
    s= cbind(dsim$s1 , dsim$s2 , dsim$s3, dsim$s4 , dsim$s5)
  )
  
  fit= stan( file = '/Users/sifaka/Documents/freq_dep.stan', 
             data = data ,
             iter = 2000, 
             warmup=1000, 
             chains=3, 
             cores=3, 
             control=list(adapt_delta=0.999) , 
             pars=c("f"), 
             refresh=100,
             init=0,
             seed=as.integer(444)
  )
  
  
  post <- extract(fit)
  f_samples5[,sim] <- post$f
  # dens(post$f)
  # dens(rlnorm(1e5, meanlog = 1, sdlog = 1) , add=TRUE , lty=2 , main="2 options")
  # abline(v=f)
}

dens(f_samples , xlim=c(0,10))
dens(f_samples3 , add=TRUE , col="red" )
dens(f_samples4 , add=TRUE , col="orange")
dens(f_samples5 , add=TRUE , col="yellow")
