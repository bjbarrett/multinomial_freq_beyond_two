data {
    int num_sims;              // num observations
    int K[num_sims];              // num behaviors
    int N[num_sims];              // num individuals
    int choice[num_sims];        // techique chosen
    real s[num_sims];        // observed number of ttimes observing behaviors
}

parameters {
    vector<lower=0>[num_sims] f;                     
}

model {
    real PrS;           // social learning Pr
    vector[4] s_temp;   // social learning temp
    //prior       
    f ~ lognormal(0,1);
    for ( i in 1:N[num_sims] ) {
                for ( j in 1:K[num_sims] ) s_temp[j] = 0;
                for ( j in 1:K[num_sims] ) s_temp[j] = pow(s[i,j],f[num_sims]);
                PrS = s_temp[choice[i]]/sum(s_temp);
                target += log( PrS ) ;         
     }//i  

}//end of model
