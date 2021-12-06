library(rethinking)
library(rstan)
library(gtools)
options(mc.cores=3) 
# datalist <- list("post_samples" = f_samples, 
#                  "f" = F[f_i], 
#                  "n" = N[n_i],
#                  "k" = K[k_i]
# )   

#prior predictive simulation of mode; for f
rln <- rlnorm(1000, meanlog = 0, sdlog = 1)
median(rln)
N1<- seq(from=0 , to=100 , by=1)
N2<- 100-N1
FreqN1B4 <- N1/(N1+N2)
FreqN1After <- rep (0,100)
plot(FreqN1B4,FreqN1B4 , ylim=c(0,1) , xlim=c(0,1) , ylab="frequency of trait after social learning" , xlab="frequency of trait before social learning",type="n" , bty="n" , cex.lab=1.5)
for(i in 1:length(rln) ){
  FreqN1After <- N1^rln[i]/(N1^rln[i]+N2^rln[i])
  lines( FreqN1B4,FreqN1After,  col=col.alpha( "darksalmon" ,  alpha=0.05  )  , lwd=1)
}
  FreqN1After <- N1^3/(N1^3+N2^3)
  lines( FreqN1B4,FreqN1After,  col=col.alpha( "red"   )  , lwd=3)
  FreqN1After <- N1^(1/3)/(N1^(1/3)+N2^(1/3))
  lines( FreqN1B4,FreqN1After,  col=col.alpha( "red"   )  , lwd=3)

abline(a=0 , b=1 , lty=2)

##begin sims

  #lets do 4 population sizes 5, 10 , 20 ,50, 100, 200 ,500 ,1000
  #fix f to 1/3 , 1 and 3
  #gnomes is 2,  4 , 6 , 8
  ##simulate sl data
N <- c(10 , 25 , 50 , 100 , 250 , 500 , 1000)  ## pop size
N <- c(1000)
F <- c( 1/3 ) ## strength of frequency dependence
K <- c(2,3,4,5) ## number of options
n_sims <- 50
#stat sim and model
n_iter=1600
n_chains=3
for (f_i in 1:length(F)){
 for (n_i in 1:length(N)){
   for (k_i in 1:length(K)){
      f_samples <- array(data=NA , c(n_iter*n_chains*0.5,n_sims) )
      
      ###blank list for data storage
      for (sim in 1:n_sims){
        si_freq <- rdirichlet(1.2, alpha=rep(1,K[k_i]))
        si_freq[K[k_i]+1:max(K)] <- NA
        s_temp <- rep(0,K[k_i])
        dsim <- data.frame(id=0 , choice=0 , s1=0 , s2=0 , s3=0 , s4=0, s5=0 , sim=0 , f=0 ,n=0 , k=0) #may have to modify if K gets bigger
        therow <- 1
        #begin loop
        for(n in 1:N[n_i]){
          s_temp <- si_freq[1:K[k_i]]^F[f_i]
          pr_choose <- s_temp/sum(s_temp)
          choice <- sample( 1:K[k_i] , size=1 , prob=pr_choose)
          dsim[therow,] <- c( n , choice , si_freq[1] , si_freq[2] , si_freq[3] , si_freq[4] ,si_freq[5] , sim ,F[f_i] ,N[n_i] , K[k_i])
          therow <- therow + 1
        } #n
        

        data <- list(
          K=K[k_i] ,
          N=nrow(dsim),
          choice=dsim$choice,
          s= cbind( dsim[,3:( K[k_i]+2) ] ) 
        )
        
        file_name <- '/Users/sifaka/Documents/multinomial_freq_beyond_two/freq_dep.stan'
        fit= stan( file = file_name,
                    data = data ,
                    iter = n_iter,
                    chains=n_chains,
                    cores=n_chains,
                    control=list(adapt_delta=0.999) ,
                    pars=c( "f" ),
                    refresh=100,
                    init=1,
                    seed=122021
        )


        post <- extract(fit)
        f_samples[,sim] <- post$f
  
      }#sim
      #convert to data frame as i suck at lists
      f_samples2 <- as.data.frame(f_samples)
      f_samples2$f= F[f_i]
      f_samples2$n= N[n_i]
      f_samples2$k= K[k_i]
      #if(n_i==1 & f_i==1 & k_i==1){
        master <- f_samples2 
     # }else{
      #  master <- rbind(master,f_samples2) 
    #  }
        write.csv(master, paste0("f_",F[f_i],"_n_",N[n_i],"_k_",K[k_i],"_sims_fits.csv"))
        
     } #f_i
 } #n_i
  #write.csv(master, paste0("f_",F[f_i],"_k_",K[k_i],"_sims_fits.csv"))
} #k_i
#write.csv(master, paste0("f_",F[f_i],"_sims_fits.csv"))


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



##########################3
# 
# N <- c( 50 )  ## pop size
# F <- c( 2.5 ) ## strength of frequency dependence
# K <- c(3) ## number of options
# n_sims <- 1
# #stat sim and model
# n_iter=1000
# n_chains=3
# for (f_i in 1:length(F)){
#   for (n_i in 1:length(N)){
#     for (k_i in 1:length(K)){
#       # f_i <- 3
#       #n_i <- 3
#       # k_i <- 2
#       f_samples <- array(data=NA , c(n_iter*n_chains*0.5,n_sims) )
#       
#       ###blank list for data storage
#       for (sim in 1:n_sims){
#         tweak <- 0.05
#         si_freq <- rep(1/K[k_i],K[k_i])
#         si_freq[1] <- si_freq[1] + tweak
#         si_freq[-1] <-  si_freq[-1] - tweak/(K[k_i]-1) 
#         si_freq[K[k_i]+1:max(K)] <- NA
#         #if( sum(si_freq[1:k_i])!=1){ print("FUCCCCKKKK, NO SUM TO ONE, DANGER!!!") }  #flag bad initial probs
#         s_temp <- rep(0,K[k_i])
#         dsim <- data.frame(id=0 , choice=0 , s1=0 , s2=0 , s3=0 , s4=0, sim=0 , f=0 ,n=0 , k=0) #may have to modify if K gets bigger
#         therow <- 1
#         #begin loop
#         for(n in 1:N[n_i]){
#           s_temp <- si_freq[1:K[k_i]]^F[f_i]
#           pr_choose <- s_temp/sum(s_temp)
#           choice <- sample( 1:K[k_i] , size=1 , prob=pr_choose)
#           dsim[therow,] <- c( n , choice , si_freq[1] , si_freq[2] , si_freq[3] , si_freq[4] , sim ,F[f_i] ,N[n_i] , K[k_i])
#           therow <- therow + 1
#         } #n
#         
#         
#         data <- list(
#           K=K[k_i] ,
#           N=nrow(dsim),
#           choice=dsim$choice,
#           s= cbind( dsim[,3:( K[k_i]+2) ] ) 
#         )
#         
#         
#         fit1= stan( file = 'freq_dep.stan',
#                    data = data ,
#                    iter = n_iter,
#                    chains=n_chains,
#                    cores=n_chains,
#                    control=list(adapt_delta=0.999) ,
#                    pars=c("f"),
#                    refresh=100,
#                    init=0,
#                    seed=122021
#         )
#         
#         fit2= stan( file = 'freq_dep_log.stan',
#                     data = data ,
#                     iter = n_iter,
#                     chains=n_chains,
#                     cores=n_chains,
#                     control=list(adapt_delta=0.999) ,
#                     pars=c("log_f" , "f"),
#                     refresh=100,
#                     init=0,
#                     seed=122021
#         )
#         
#         post <- extract(fit)
#         f_samples[,sim] <- post$f
#         
#       }#sim
#       #convert to data frame as i suck at lists
#       f_samples2 <- as.data.frame(f_samples)
#       f_samples2$f= F[f_i]
#       f_samples2$n= N[n_i]
#       f_samples2$k= K[k_i]
#       if(n_i==1 & f_i==1 & k_i==1){
#         master <- f_samples2 
#       }else{
#         master <- rbind(master,f_samples2) 
#       }
#     } #f_i
#     #save(master , file="mulinom_sim.rds")
#   } #n_i
# } #k_i
# 
# ####lets generate mutlinomial random initial conditions
# 
# str(x)
# dens(x)
