library(RColorBrewer)
library(gtools)
library(FSelector)
#lets simulate a vector of length K that sums to 1 to generate random frequencies.
# We chose an alpha of one to cover a variety of prob combons

#x <- rdirichlet(n_sims*10000, alpha=rep(K,K[k_i]))
mypalette<-brewer.pal(length(K),"Dark2")
#lets plot where all the distributions are drawn form
x <- rdirichlet(1e5, alpha=rep(1,K[1]) ,ylim=c(0,3))
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
