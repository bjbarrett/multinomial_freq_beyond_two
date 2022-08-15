data {
    int K;              // num behaviors
    int N;              // num observations in dataset
    int choice[N];        // techique chosen
    real s[N,K];        // observed number of ttimes observing behaviors
    int sim[N];
    int n_sim;
    int K_i[N];
}

parameters {
    vector[n_sim] log_f;                     
}


model {
    real PrS;           // social learning Pr
    vector[K] s_temp;   // social learning temp
    vector[n_sim] f;
    //prior       
    log_f ~ normal(0,1);
    for ( i in 1:N ) {
                f[sim[i]]=exp(log_f[sim[i]]);
                for ( j in 1:K_i[i] ) s_temp[j] = pow(s[i,j], f[sim[i]] );
                PrS = s_temp[choice[i]]/sum(s_temp);
                target += log( PrS ) ;         
     }//i  

}//end of model
generated quantities{
    vector[n_sim] f;
    for ( j in 1:n_sim ) {
      f[j]=exp(log_f[j]);
    }
}
