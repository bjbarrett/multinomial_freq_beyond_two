library(rethinking)
library(rstan)
library(gtools)

options(mc.cores=2) 

###########simulate 2
##dataframe
N <- c(5,10,25,50,100,250)  ## pop size / nobs
F <- c(1/3 , 1 , 3) ## strength of frequency dependence
K <- c(2) ## number of options
n_sims <- 100

#data sim
therow <- thesim <- 1
dsim <- data.frame(n_i=0 , choice=0 , s1=0 , s2=0 , s3=0 , s4=0 , s5=0, sim_local=0 , f=0 ,n=0 , k=0 ,sim_global=0 , obs=0) #may have to modify if K gets bigger
therow <- 1
for (f_i in 1:length(F)){
  for (k_i in 1:length(K)){
    for (n_i in 1:length(N)){
      for (sim in 1:n_sims){
        set.seed(therow)
        si_freq <- rdirichlet(1, alpha=rep(1,K[k_i]))
        si_freq[K[k_i]+1:max(K)] <- 0
        s_temp <- rep(0,K[k_i])
        s_temp <- si_freq[1:K[k_i]]^F[f_i]
        pr_choose <- s_temp/sum(s_temp)
        for(num in 1:N[n_i]){
          choice <- sample( 1:K[k_i] , size=1 , prob=pr_choose , replace=TRUE)
          dsim[therow,] <- c( num , choice , si_freq[1] , si_freq[2] , si_freq[3] , si_freq[4] , si_freq[5], sim ,F[f_i] ,N[n_i] , K[k_i] , thesim, therow)
          therow <- therow + 1
        }
        thesim <- thesim + 1
      }#sim
    } #f_i
  } #n_i
} #k_i

dsim2 <- dsim
write.csv(dsim2 , "sim100_k2_fthirds_n5102550100250.csv")

data2 <- list(
  K_i= dsim$k,
  K=max(dsim$k) ,
  N=nrow(dsim),
  choice=dsim$choice,
  s=dsim[,3:4],
  sim=dsim$sim_global,
  n_sim=max(dsim$sim_global)
)


file_name <- 'freq_dep.stan'
fit2= stan( file = file_name,
           data = data2 ,
           iter = 2000,
           chains=2,
           cores=2,
           control=list(adapt_delta=0.99) ,
           pars=c( "log_f" , "f" ),
           refresh=100,
           init=0,
           seed=44
)
post2 <- extract.samples(fit2)
precis(fit2 , pars="f" , depth=2)

###########simulate 3
K <- c(3) ## number of options

#data sim
therow <- thesim <- 1
dsim <- data.frame(n_i=0 , choice=0 , s1=0 , s2=0 , s3=0 , s4=0 , s5=0, sim_local=0 , f=0 ,n=0 , k=0 ,sim_global=0 , obs=0) #may have to modify if K gets bigger
therow <- 1
for (f_i in 1:length(F)){
  for (k_i in 1:length(K)){
    for (n_i in 1:length(N)){
      for (sim in 1:n_sims){
        set.seed(therow)
        si_freq <- rdirichlet(1, alpha=rep(1,K[k_i]))
        si_freq[K[k_i]+1:max(K)] <- 0
        s_temp <- rep(0,K[k_i])
        s_temp <- si_freq[1:K[k_i]]^F[f_i]
        pr_choose <- s_temp/sum(s_temp)
        for(num in 1:N[n_i]){
          choice <- sample( 1:K[k_i] , size=1 , prob=pr_choose , replace=TRUE)
          dsim[therow,] <- c( num , choice , si_freq[1] , si_freq[2] , si_freq[3] , si_freq[4] , si_freq[5], sim ,F[f_i] ,N[n_i] , K[k_i] , thesim, therow)
          therow <- therow + 1
        }
        thesim <- thesim + 1
      }#sim
    } #f_i
  } #n_i
} #k_i

dsim3 <- dsim
write.csv(dsim3 , "sim100_k3_fthirds_n5102550100250.csv")

data3 <- list(
  K_i= dsim$k,
  K=max(dsim$k) ,
  N=nrow(dsim),
  choice=dsim$choice,
  s=dsim[,3:5],
  sim=dsim$sim_global,
  n_sim=max(dsim$sim_global)
)


fit3= stan( file = file_name,
            data = data3 ,
            iter = 2000,
            chains=2,
            cores=2,
            control=list(adapt_delta=0.99) ,
            pars=c( "log_f" , "f" ),
            refresh=100,
            init=1,
            seed=44
)
post3 <- extract.samples(fit3)
precis(fit3 , pars="f" , depth=2)

##########simulate K is 4
K <- c(4) ## number of options

#data sim
therow <- thesim <- 1
dsim <- data.frame(n_i=0 , choice=0 , s1=0 , s2=0 , s3=0 , s4=0 , s5=0, sim_local=0 , f=0 ,n=0 , k=0 ,sim_global=0 , obs=0) #may have to modify if K gets bigger
therow <- 1
for (f_i in 1:length(F)){
  for (k_i in 1:length(K)){
    for (n_i in 1:length(N)){
      for (sim in 1:n_sims){
        set.seed(therow)
        si_freq <- rdirichlet(1, alpha=rep(1,K[k_i]))
        si_freq[K[k_i]+1:max(K)] <- 0
        s_temp <- rep(0,K[k_i])
        s_temp <- si_freq[1:K[k_i]]^F[f_i]
        pr_choose <- s_temp/sum(s_temp)
        for(num in 1:N[n_i]){
          choice <- sample( 1:K[k_i] , size=1 , prob=pr_choose , replace=TRUE)
          dsim[therow,] <- c( num , choice , si_freq[1] , si_freq[2] , si_freq[3] , si_freq[4] , si_freq[5], sim ,F[f_i] ,N[n_i] , K[k_i] , thesim, therow)
          therow <- therow + 1
        }
        thesim <- thesim + 1
      }#sim
    } #f_i
  } #n_i
} #k_i

dsim4 <- dsim
write.csv(dsim4 , "sim100_k4_fthirds_n5102550100250.csv")

data4 <- list(
  K_i= dsim$k,
  K=max(dsim$k) ,
  N=nrow(dsim),
  choice=dsim$choice,
  s=dsim[,3:6],
  sim=dsim$sim_global,
  n_sim=max(dsim$sim_global)
)


file_name <- 'freq_dep_2.stan'
fit4= stan( file = file_name,
            data = data4 ,
            iter = 2000,
            chains=2,
            cores=2,
            control=list(adapt_delta=0.99) ,
            pars=c( "log_f" , "f" ),
            refresh=100,
            init=1,
            seed=44
)
post4 <- extract.samples(fit4)
precis(fit4 , pars="f" , depth=2)

##5
##########simulate K is 4
K <- c(5) ## number of options

#data sim
therow <- thesim <- 1
dsim <- data.frame(n_i=0 , choice=0 , s1=0 , s2=0 , s3=0 , s4=0 , s5=0, sim_local=0 , f=0 ,n=0 , k=0 ,sim_global=0 , obs=0) #may have to modify if K gets bigger
therow <- 1
for (f_i in 1:length(F)){
  for (k_i in 1:length(K)){
    for (n_i in 1:length(N)){
      for (sim in 1:n_sims){
        set.seed(therow)
        si_freq <- rdirichlet(1, alpha=rep(1,K[k_i]))
        si_freq[K[k_i]+1:max(K)] <- 0
        s_temp <- rep(0,K[k_i])
        s_temp <- si_freq[1:K[k_i]]^F[f_i]
        pr_choose <- s_temp/sum(s_temp)
        for(num in 1:N[n_i]){
          choice <- sample( 1:K[k_i] , size=1 , prob=pr_choose , replace=TRUE)
          dsim[therow,] <- c( num , choice , si_freq[1] , si_freq[2] , si_freq[3] , si_freq[4] , si_freq[5], sim ,F[f_i] ,N[n_i] , K[k_i] , thesim, therow)
          therow <- therow + 1
        }
        thesim <- thesim + 1
      }#sim
    } #f_i
  } #n_i
} #k_i

dsim5 <- dsim
write.csv(dsim5 , "sim100_k5_fthirds_n5102550100250.csv")

data5 <- list(
  K_i= dsim$k,
  K=max(dsim$k) ,
  N=nrow(dsim),
  choice=dsim$choice,
  s=dsim[,3:7],
  sim=dsim$sim_global,
  n_sim=max(dsim$sim_global)
)


file_name <- 'freq_dep_2.stan'
fit5= stan( file = file_name,
            data = data5 ,
            iter = 2000,
            chains=2,
            cores=2,
            control=list(adapt_delta=0.99) ,
            pars=c( "log_f" , "f" ),
            refresh=100,
            init=0,
            seed=44
)
post5 <- extract.samples(fit5)
precis(fit5 , pars="f" , depth=2)

save(dsim2,dsim3,dsim4,dsim5,post2,post3,post4,post5 , file="100simsK2345_differentdistforagentspersim.rdata")

##lets export elements without dupes to link posteriors to sim conditions
CalcEntropy<- function(x){
  out = -sum(ifelse(x==0, 0 , x*log(x)) )
  return(out)
}

#loss is minimized at intermediate values of distributional entropy
# there are more ways to realize intermediate values of entropy as K increases
