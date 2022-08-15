library(xtable)
# for each simulation how much of posterior was consistent with positive freq dep
#posi freq dep
ss <- d[d$f==3,]
str(ss)
sss <- droplevels(aggregate(cbind( ss$med_log_f ,ss$perc_positive , ss$sim_sd )   , list( k=ss$k ,n=ss$n) , mean )  )
colnames(sss)[3:5] <- c("post median log(f)","% post log(f) > 0" , "post sd log(f)")
str(sss)
print(xtable(sss , digits=c(0,0,0,3,3,3)) , include.rownames=FALSE) 

#neutral freq dep
ss <- d[d$f==1,]
str(ss)
sss <- droplevels(aggregate(cbind( ss$med_log_f ,ss$perc_positive , ss$sim_sd )   , list( k=ss$k ,n=ss$n) , mean )  )
colnames(sss)[3:5] <- c("post median log(f)","% post log(f) > 0" , "post sd log(f)")
rownames(sss) <- c()
str(sss)
print(xtable(sss , digits=c(0,0,0,3,3,3)) , include.rownames=FALSE)

#negative freq dep
ss <- d[d$f==1/3,]
str(ss)
sss <- droplevels(aggregate(cbind( ss$med_log_f ,ss$perc_positive , ss$sim_sd )   , list( k=ss$k ,n=ss$n) , mean )  )
colnames(sss)[3:5] <- c("post median log(f)","% post log(f) > 0" , "post sd log(f)")
rownames(sss) <- c()
str(sss)
print(xtable(sss , digits=c(0,0,0,3,3,3)) , include.rownames=FALSE)

