library(rethinking)
library(rstan)
library(gtools)
#library(FSelector)

options(mc.cores=2) 

#prior predictive simulation of mode; for f
pdf(file = "prior_pred_acq_curve.pdf", width = 6, height = 6) # The height of the plot in inches

rln <- exp(rnorm(1000, mean = 0, sd = 1))
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
lines( FreqN1B4,FreqN1After,  col=col.alpha("red" , alpha=0.4)  , lwd=3)
FreqN1After <- N1^(1/3)/(N1^(1/3)+N2^(1/3))
lines( FreqN1B4,FreqN1After,  col=col.alpha("red" , alpha=0.4) , lwd=3)

abline(a=0 , b=1 , lty=2)

dev.off()
#prior predictive simulation of model for 5 if k=4
##begin sims

K <-3
i<-1
N1<- seq(from=0 , to=1000 , by=1)
N2 <- N3 <- (1000-N1)/(K[i]-1)
FreqN1B4 <- N1/(N1+N2+N3)
FreqN1After <- rep (0,1000)
plot(FreqN1B4,FreqN1B4 , ylim=c(0,1) , xlim=c(0,1) , ylab="frequency of trait after social learning" , xlab="frequency of trait before social learning",type="n" , bty="n" , cex.lab=1.5)
# for(i in 1:length(rln) ){
#   FreqN1After <- N1^rln[i]/(N1^rln[i]+N2^rln[i])
#   lines( FreqN1B4,FreqN1After,  col=col.alpha( "darksalmon" ,  alpha=0.05  )  , lwd=1)
# }
FreqN1After <- N1^3/(N1^3+N2^3+N3^3)
lines( FreqN1B4,FreqN1After,  col=col.alpha( "red"   )  , lwd=3)

#make n3 # times less likely than n2
N1<- seq(from=0 , to=1 , length=1000)
N2 <- (1-N1)/(K[i]-1)*.9
N3 <- 1- N1-N2
FreqN1B4 <- N1/(N1+N2+N3)
FreqN1After <- rep (0,1)
FreqN1After <- N1^3/(N1^3+N2^3+N3^3)
lines( FreqN1B4,FreqN1After,  col=col.alpha( "green"   )  , lwd=3)

abline(a=0 , b=1 , lty=2)

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


file_name <- 'freq_dep_2.stan'
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

save(dsim2,dsim3,dsim4,dsim5,post2,post3,post4,post5 , file="100simsK2345.rdata")

##lets export elements without dupes to link posteriors to sim conditions
CalcEntropy<- function(x){
  out = -sum(ifelse(x==0, 0 , x*log(x)) )
  return(out)
}

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

d <- rbind(d2,d3,d4)
post <-  post2 + post3
mypalette<-brewer.pal(3,"Dark2")

plot(loss~entropy , data=d2, col=mypalette[1] , pch=1 , xlim=c(0 ,2) , ylim=c(0,5))
points(loss~entropy , data=d3 , col=mypalette[2] , pch=1)
points(loss~entropy , data=d4 , col=mypalette[3] , pch=1)

plot((d2$entropy/max(d2$entropy)),d2$loss, col=mypalette[1] , pch=1 , xlim=c(0 ,1) , ylim=c(0,5))
points((d3$entropy/max(d3$entropy)),d3$loss, col=mypalette[2] , pch=1)
points((d4$entropy/max(d4$entropy)),d4$loss, col=mypalette[3] , pch=1)

#loss is minimized at intermediate values of distributional entropy
# there are more ways to realize intermediate values of entropy as K increases
mypalette<-brewer.pal(4,"Dark2")

pdf(file = "prob_dist_dirichlet.pdf", width = 6, height = 6) # The height of the plot in inches

K <- c(2,3,4,5)
#lets plot where all the distributions are drawn form
x <- rdirichlet(1e6, alpha=rep(1,K[1]))
dens(x , col="white" , ylim=c(0,4) , xlab="probability")
for(i in 1:4){
  x <- rdirichlet(1e5, alpha=rep(1,K[i]))
  dens(x, add=TRUE , col=mypalette[i] , lw=3 )
  abline(v=mean(x),col=mypalette[i] , lty=3 )
}
legend("topright" , legend=K , fill=mypalette , title="K" , bty='n')

dev.off()

x <- rdirichlet(1e5, alpha=rep(1,K[i]))
x <- as.vector(apply(x ,1, CalcEntropy))
dens(x , col="white" , ylim=c(0,6) , xlim=c(0,1))
for(i in 1:4){
  x <- rdirichlet(1e6, alpha=rep(1,K[i]))
  x <- as.vector(apply(x ,1, CalcEntropy))
  dens(x/max(x), add=TRUE , col=mypalette[i] , lw=3)
  #abline(v=mean(x),col=mypalette[i] , lty=3 )
}
legend("topleft" , legend=K , fill=mypalette , title="K" , bty='n')
