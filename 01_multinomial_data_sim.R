##prior predictive simulation of mode; for f
mod_prior <- 
  #lets do 4 population sizes 5, 10 , 20 ,50, 100, 200 ,500 ,1000
  #fix f to 1 and 2.5
  #gnomes is 2,  4 , 6 , 8
  ##simulate sl data
  f_samples2 <- array(data=NA , c(3000,50) )
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
