library(RColorBrewer)
library(gtools)
library(FSelector)
#lets simulate a vector of length K that sums to 1 to generate random frequencies.
# We chose an alpha of one to cover a variety of prob combons
K <- c(2,3,4)
#x <- rdirichlet(n_sims*10000, alpha=rep(K,K[k_i]))
mypalette<-brewer.pal(length(K),"Dark2")
#lets plot where all the distributions are drawn form
x <- rdirichlet(1e5, alpha=rep(1,K[1]))
dens(x , col="white")
for(i in 1:length(K)){
  x <- rdirichlet(1e5, alpha=rep(1.2,K[i]))
  dens(x, add=TRUE , col=mypalette[i] , lw=3)
  abline(v=mean(x),col=mypalette[i] , lty=3 )
}
legend("topright" , legend=K , fill=mypalette , title="K" , bty='n')

##bumping up alpha centers estimated around 1/k
x <- rdirichlet(n_sims*10000, alpha=rep(5,K[k_i]))
dens(x[,1] , col=colp[1] , lty=2,add=TRUE)
for(i in 2:k_i) dens(x[,2], add=TRUE , col=colp[k_i] , lty=2)
abline(v=(1/K[k_i]))

## lets calculate variance of dirchtlet

x <- rdirichlet(100, alpha=rep(1,2))
var <- apply( x , 1 , var)
max <- apply( x , 1 , max)
plot(var~max , col=mypalette[1], cex=0.8 , pch=19 , xlab="maximum observed prob" , ylab= "variance" , xlim=c(0,1) , ylim=c(0,0.6))

for(i in 1:length(K)){
  x <- rdirichlet(1e5, alpha=rep(1,K[i]))
  var(x[,2])
  var <- apply( x , 1 , var)
  max <- apply( x , 1 , max)
  points(var~max , col=mypalette[i] , cex=0.8 , pch=19)
}

####calculate entropy
entropy <- function(target) {
  freq <- table(target)/length(target)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #drop 0 to avoid NaN resulting from log2
  vec<-vec[vec>0]
  #compute entropy
  -sum(vec * log2(vec))
}


x <- rdirichlet(100, alpha=rep(1,2))
ent <- apply( x , 1 , entropy)
max <- apply( x , 1 , max)
plot(ent~max , col=mypalette[1], cex=0.8 , pch=19 , xlab="maximum observed prob" , ylab= "entropy" , xlim=c(0,1) , ylim=c(0,10))

for(i in 1:length(K)){
  x <- rdirichlet(10, alpha=rep(1,K[i]))
  ent <- apply( x , 1 , entropy)
  max <- apply( x , 1 , max)
  points(ent~max , col=mypalette[i] , cex=0.8 , pch=19)
}

### what we really want to do is for a given value of f with K=2,3, or 4 options count up all the ways that the observed data could occur
### we also want to bumb up the sample size for a given value of f
#1. generate observations of 2+3 traits
#2. simulate data showing what they would choose given a particular strength of conformity
#3. calculate the number of forking paths that would generate that observation
#4. think carefully about how this is related to entropy
q <- list()
q$q2 <- c(0.5 , 0.5 )
q$q3 <- c(1/3 , 1/3 , 1/3)
q$q4 <- c(1/4 , 1/4 , 1/4 , 1/4)
( H <- sapply( q , function(q) -sum(ifelse(q==0,0,q*log(q))) ) ) #calculate entropy
zz <- rmultinom(n=1, size=1000, prob=c(0.9,0.1))
zz/1000

# 5 agents observe this (c(0.6 , 0.4))
K <-2
F <- 2.1
N <- 20
si_freq <- rdirichlet(1, alpha=rep(1,K))
si_freq <- c(0.45,0.55)
s_temp <- si_freq[1:K]^F
pr_choose <- s_temp/sum(s_temp)
choices <- sample( 1:K , size=N , prob=pr_choose , replace=TRUE) #what are all the values of F that could give rise to this data?


p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
dens(likelihood)
plot(likelihood)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
dens(samples)

#lets try here
F <- 3
K <- 2
N <- 20
p_grid <- seq( from=0.01 , to=5 , length.out=1000 )
prior <- rep(1,1000)
#likelihood is the hard part
#what is probability that a values of f can produce this data.
#simulate data with F=3
si_freq <- c(0.4,0.6)
pr_choose <- si_freq^F/sum(si_freq^F)
choices <- sample( 1:K , size=N , prob=pr_choose , replace=TRUE) #what are all the values of F that could give rise to this data?
choices



f <- function(x) { exp(-x)/(1+exp(-x))^2 }
i <-1:10000
W <- array ( runif ( i, min = 0.00, max = 5 ), 10000 )
X <- apply ( W , 1 , FUN = f )




f <- function(x) { 
  si_freq <- c(0.7,0.3)
  (si_freq[1]^x)/sum(si_freq^x) 
  }
W <-  array( seq( from=0.01 , to=8 , length.out=1e5 ) )
X <- apply ( W , 1 , FUN = f )
dens(X) #all plausible values of Pr1
plot(sort(X)) #all plausible values of Pr1

post1 <- X/sum(X)
dens(post1)
samples <- sample( W , size=1e5 , replace=TRUE , prob=post1 )
dens(samples)

f <- function(x) { 
  si_freq <- c(0.6,0.4)
  (si_freq[1]^x)/sum(si_freq^x) 
}
W <-  array( seq( from=0.01 , to=8 , length.out=1e5 ) )
X <- apply ( W , 1 , FUN = f )
dens(X) #all plausible values of Pr1
plot(sort(X)) #all plausible values of Pr1

post1 <- X/sum(X)
dens(post1)
samples <- sample( W , size=1e5 , replace=TRUE , prob=post1 )
dens(samples)
