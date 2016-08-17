data {
  int<lower=0> N;

  vector[N] Ym;
  vector<lower=0>[N] Ysd;

  vector<lower=0>[N] BlackRatio;
  vector<lower=0>[N] Pop;


  int MissCumSumWhiteAssault[N];
  int NonMissWhiteAssault[N];
  int NmissWhiteAssault;
  vector[N] WhiteAssault;
  real MaxWhiteAssault;

  int MissCumSumBlackAssault[N];
  int NonMissBlackAssault[N];
  int NmissBlackAssault;
  vector[N] BlackAssault;
  real MaxBlackAssault;

  vector[N] Ones;
}

parameters {
 vector[5] Theta;
 vector[N] log_Y;
 real<lower=0> Sigma;
 real<lower=0,upper=MaxWhiteAssault> iWhiteAssault[NmissWhiteAssault];
 real<lower=0,upper=MaxBlackAssault> iBlackAssault[NmissBlackAssault];

}
 
transformed parameters{
  vector<lower=0>[N] DataWhiteAssault;
  vector<lower=0>[N] DataBlackAssault;
 for(t in 1:N){
  DataWhiteAssault[t] = if_else(NonMissWhiteAssault[t], WhiteAssault[t], iWhiteAssault[MissCumSumWhiteAssault[t]]);
  DataBlackAssault[t] = if_else(NonMissBlackAssault[t], BlackAssault[t], iBlackAssault[MissCumSumBlackAssault[t]]);
 }
}


model {
  vector[N] Mu;
  
  log_Y ~ normal(Ym,Ysd);
  Theta ~ cauchy(0,5);
  Sigma ~ exponential(1);

  Mu = ( Theta[1] + Theta[2]*log(Pop) + Theta[3]*log(BlackRatio) + Theta[4]*log(DataWhiteAssault) + Theta[5]*log(DataBlackAssault) );

  log_Y ~  normal(Mu,Sigma);
}
