mydir = "/Users/sifaka/Downloads/rstudio-export-log/f_3"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE , recursive=TRUE)
d = ldply(myfiles, read_csv)
mypalette<-brewer.pal(max(K),"BuPu")

d <- d[,-1] # get rid of bs first column
nsims <- ncol(d) - 3 #number of sims 
master2 <- d
N <- sort(unique(d$n))  ## pop size vector from data
F <-sort(unique(d$f))  ## strength of frequency dependence from data
K <- sort(unique(d$k))  ## number of options from data
# d2 <- d[d$n==20 & d$f==3 & d$k==2,]
# dens(d2[,4:53])
# dens(d2)

dev.off()

z2 <- master2[master2$k==2,] #subset relevant k
z2m <- data.matrix(z2[,1:nsims]) #extract parameter estimates
index <- which(z2$n==10) #get rows for relevant n in loop
dens(z2m[index,], xlim=c(0,5) , col=mypalette[1] , adj=3 , ylim=c(0,1))

z2 <- master2[master2$k==2,] #subset relevant k
z2m <- data.matrix(z2[,1:nsims]) #extract parameter estimates
index <- which(z2$n==10) #get rows for relevant n in loop
dens(z2m[index,] , add=TRUE , col=mypalette[1] , adj=3)
HPDI(z2m[index,])
median(z2m[index,])
mean(z2m[index,])
sum( z2m[index,] > 1 ) / (nsims*3000)
chainmode( z2m[index,] , adj=0.01 )


z3 <- master2[master2$k==3,] #subset relevant k
z3m <- data.matrix(z3[,1:nsims]) #extract parameter estimates
index <- which(z3$n==10) #get rows for relevant n in loop
dens(z3m[index,] , add=TRUE , col=mypalette[2] , adj=3)
HPDI(z3m[index,])
median(z3m[index,])
mean(z3m[index,])
sum( z3m[index,] > 1 ) / (nsims*3000)
chainmode( z3m[index,] , adj=0.01 )


z4 <- master2[master2$k==4,] #subset relevant k
z4m <- data.matrix(z4[,1:nsims]) #extract parameter estimates
index <- which(z4$n==10) #get rows for relevant n in loop
dens(z4m[index,] , add=TRUE , col=mypalette[3] , adj=3 )
HPDI(z4m[index,])
median(z4m[index,])
mean(z4m[index,])
sum( z4m[index,] > 1 ) / (nsims*3000)
chainmode( z4m[index,] , adj=0.01 )


z5 <- master2[master2$k==5,] #subset relevant k
z5m <- data.matrix(z5[,1:nsims]) #extract parameter estimates
index <- which(z5$n==10) #get rows for relevant n in loop
dens(z5m[index,] , add=TRUE , col=mypalette[4] , adj=3)
HPDI(z5m[index,])
median(z5m[index,])
mean(z5m[index,])
sum( z5m[index,] > 1 ) / (nsims*3000)
chainmode( z5m[index,] , adj=0.01 )
