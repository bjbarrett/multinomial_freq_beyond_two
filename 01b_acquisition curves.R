library(RColorBrewer)
library(rethinking)

#prior predictive simulation of model; for k=2
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

##begin sims
pdf(file = "multi_f_acq_curve.pdf", width = 6, height = 6) # The height of the plot in inches

K <-c(2,3,4,5)
mypalette<-brewer.pal(max(K),"Set1")
plot(0,0 , ylim=c(0,1) , xlim=c(0,1) , ylab="frequency of trait after social learning" , xlab="frequency of trait before social learning",type="n" , bty="n" , cex.lab=1.5)
for(i in 1:length(K)){
  p_mat <- matrix(NA, nrow = 100, ncol = K[i])
  p_mat[,1] <- seq(from=0 , to=1 , length=100)
  p_mat[,2:K[i]] <- (1-p_mat[,1])/(K[i]-1)
  p_mat_after <- p_mat
  eff <- 3
  p_mat_after <- p_mat_after ^ eff
    for(j in 1:100){
      p_mat_after[j,] <- p_mat_after[j,]/sum(p_mat_after[j,]) 
    }
  lines( p_mat[,1],p_mat_after[,1],  col=mypalette[i]  , lwd=3)
}
abline(a=0 , b=1 , lty=2)
legend("bottomright" , legend=K ,fill=mypalette , title="f" , bty='n' , inset=.03)

dev.off()

## for K=3 lets show it across a few values of difference
#make n3 # times less likely than n2
pdf(file = "varydist_k3_acq_curve.pdf", width = 6, height = 6) # The height of the plot in inches

diff <-c(.5 , .6 , .7 , .8 , .9 , 1)
mypalette<-brewer.pal(length(diff),"Set1")
plot(0,0 , ylim=c(0,1) , xlim=c(0,1) , ylab="frequency of trait after social learning" , xlab="frequency of trait before social learning",type="n" , bty="n" , cex.lab=1.5)
for(j in 1:length(diff)){
  i <- 2 #k is 3
  p_mat <- matrix(NA, nrow = 100, ncol = K[i])
  p_mat[,1] <- seq(from=0 , to=1 , length=100)
  p_mat[,2] <- (1-p_mat[,1])*diff[j]
  p_mat[,3] <- (1-p_mat[,1])*(1-diff[j])
  p_mat_after <- p_mat
  eff <- 3
  p_mat_after <- p_mat_after ^ eff
  for(k in 1:100){
    p_mat_after[k,] <- p_mat_after[k,]/sum(p_mat_after[k,]) 
  }
  lines( p_mat[,1],p_mat_after[,1],  col=mypalette[j]  , lwd=3)
}
abline(a=0 , b=1 , lty=2)
legend("bottomright" , legend=diff ,fill=mypalette , title="x for f(2)=x*f(3)" , bty='n' , inset=.03)
dev.off()
