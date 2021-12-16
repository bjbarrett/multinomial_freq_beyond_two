mydir = "/Users/sifaka/Downloads/rstudio-export-log"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE , recursive=TRUE)
d = ldply(myfiles, read_csv)

d <- d[,-1] # get rid of bs first column
nsims <- ncol(d) - 3 #number of sims 

N <- sort(unique(d$n))  ## pop size vector from data
F <-sort(unique(d$f))  ## strength of frequency dependence from data
K <- sort(unique(d$k))  ## number of options from data
mypalette<-brewer.pal(max(K),"Dark2")
d2 <- d[d$n==20 & d$f==3 & d$k==2,]
dens(d2[,4:53])
dens(d2)

dev.off()

z2 <- master2[master2$k==2,] #subset relevant k
z2m <- data.matrix(z2[,1:nsims]) #extract parameter estimates
index <- which(z2$n==100) #get rows for relevant n in loop
dens(z2m[index,], xlim=c(0,5))

z2 <- master2[master2$k==2,] #subset relevant k
z2m <- data.matrix(z2[,1:nsims]) #extract parameter estimates
index <- which(z2$n==100) #get rows for relevant n in loop
dens(z2m[index,] , add=TRUE , col="black" )

z3 <- master2[master2$k==3,] #subset relevant k
z3m <- data.matrix(z3[,1:nsims]) #extract parameter estimates
index <- which(z3$n==100) #get rows for relevant n in loop
dens(z3m[index,] , add=TRUE , col="blue" )


z4 <- master2[master2$k==4,] #subset relevant k
z4m <- data.matrix(z4[,1:nsims]) #extract parameter estimates
index <- which(z4$n==100) #get rows for relevant n in loop
dens(z4m[index,] , add=TRUE , col="gold" )


z5 <- master2[master2$k==5,] #subset relevant k
z5m <- data.matrix(z5[,1:nsims]) #extract parameter estimates
index <- which(z5$n==100) #get rows for relevant n in loop
dens(z5m[index,] , add=TRUE , col="red" )
