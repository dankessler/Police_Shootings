data {
int<lower=0> N;

vector[N] Ym;
vector<lower=0>[N] Ysd;

vector<lower=0>[N] BlackRatio;
vector<lower=0>[N] Pop;

vector<lower=0>[N] Wealth;
vector<lower=0>[N] Gini;

int DMA[N];
vector[201] GoogleRacism;   

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
real MinBlackAssault;

int MissCumSumWhiteWeapons[N];
int NonMissWhiteWeapons[N];
int NmissWhiteWeapons;
vector[N] WhiteWeapons;
real MaxWhiteWeapons;

int MissCumSumBlackWeapons[N];
int NonMissBlackWeapons[N];
int NmissBlackWeapons;
vector[N] BlackWeapons;
real MaxBlackWeapons;
real MinBlackWeapons;

vector[N] Ones;
}

parameters {
 vector[5] Theta;
 vector[N] log_Y;
 real<lower=0> Sigma;
 #real<lower=25,upper=155> iHate;
 real<lower=0,upper=MaxWhiteAssault> iWhiteAssault[NmissWhiteAssault];
 real<lower=MinBlackAssault,upper=MaxBlackAssault> iBlackAssault[NmissBlackAssault];
 real<lower=0,upper=MaxWhiteWeapons> iWhiteWeapons[NmissWhiteWeapons];
 real<lower=MinBlackWeapons,upper=MaxBlackWeapons> iBlackWeapons[NmissBlackWeapons];
 }
 
transformed parameters{
vector<lower=0>[N] DataWhiteAssault;
vector[N] DataBlackAssault;
vector<lower=0>[N] DataWhiteWeapons;
vector[N] DataBlackWeapons;
#vector<lower=0>[N] DataHate;
#
for(t in 1:N){
#DataHate[t] <-   if_else(DMA[t]==156,iHate,GoogleRacism[DMA[t]] );
#
DataWhiteAssault[t] <- if_else(NonMissWhiteAssault[t], WhiteAssault[t], iWhiteAssault[MissCumSumWhiteAssault[t]]);
DataBlackAssault[t] <- if_else(NonMissBlackAssault[t], BlackAssault[t], iBlackAssault[MissCumSumBlackAssault[t]]);
#
DataWhiteWeapons[t] <- if_else(NonMissWhiteWeapons[t], WhiteWeapons[t], iWhiteWeapons[MissCumSumWhiteWeapons[t]]);
DataBlackWeapons[t] <- if_else(NonMissBlackWeapons[t], BlackWeapons[t], iBlackWeapons[MissCumSumBlackWeapons[t]]);


             }

}


model {
vector[N] Mu;

  log_Y ~ normal(Ym,Ysd);
  Theta ~ cauchy(0,5);
  Sigma ~ exponential(1);

  Mu <- ( Theta[1] + Theta[2]*log(Pop) + Theta[3]*log(BlackRatio) + Theta[4]*log(DataWhiteAssault) + Theta[5]*log(DataBlackAssault) );

  log_Y ~  normal(Mu,Sigma);
}
