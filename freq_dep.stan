data {
    int K;              // num behaviors
    int N;              // num observations in dataset
    int choice[N];        // techique chosen
    real s[N,K];        // observed number of ttimes observing behaviors
}

parameters {
    real<lower=0> f;                     
}

model {
    real PrS;           // social learning Pr
    vector[K] s_temp;   // social learning temp
    //prior       
    f ~ lognormal(0,1);
    

    for ( i in 1:N ) {
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],f);
                PrS = s_temp[choice[i]]/sum(s_temp);
                target += log( PrS ) ;         
     }//i  

}//end of model