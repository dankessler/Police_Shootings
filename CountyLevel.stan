#### Data Block 
data {
  int<lower=0> N;

  int<lower=1> Nwhite[N];
  int<lower=1> Nblack[N];

  int<lower=0> UnarmedWhite[N];
  int<lower=0> UnarmedBlack[N];

  int<lower=0> ArmedWhite[N];
  int<lower=0> ArmedBlack[N];
}

parameters {
   vector[4] Mu;
   vector<lower=0>[4] Sigma;
   corr_matrix[4] Rho;
 
   vector[4] Theta[N];
}
   
model {
######################################################### Priors
   Mu ~ normal(-14,4);
   Sigma ~ cauchy(0,5);

   for(i in 1:N){
   Theta[i] ~ multi_normal_cholesky(Mu, (diag_matrix(Sigma) * cholesky_decompose(Rho)) );
   }


####################################################### Data Modeling
for(i in 1:N){
   ArmedBlack[i]~binomial(Nblack[i],inv_logit(Theta[i,1]));
   ArmedWhite[i]~binomial(Nwhite[i],inv_logit(Theta[i,3]));


   UnarmedBlack[i]~binomial(Nblack[i],inv_logit(Theta[i,2])); 
   UnarmedWhite[i]~binomial(Nwhite[i],inv_logit(Theta[i,4]));

   }

  }
  
generated quantities{
######################################################### Mean Quanitities
real Mu_Black_Armed; 
real Mu_White_Armed; 


real Mu_Black_Unarmed; 
real Mu_White_Unarmed; 

   
real Mu_RR_Black_Armed_Versus_Unarmed; 
real Mu_RR_White_Armed_Versus_Unarmed; 


real Mu_RR_Black_Armed_Versus_White_Armed; 

real Mu_RR_Black_Unarmed_Versus_White_Unarmed; 

real Mu_RR_Black_Unarmed_Versus_White_Armed; 

######################################################### Quanitities By County 
vector[N]  Black_Armed; 
vector[N]  White_Armed; 


vector[N]  Black_Unarmed; 
vector[N]  White_Unarmed; 

   
vector[N]  RR_Black_Armed_Versus_Unarmed; 
vector[N]  RR_White_Armed_Versus_Unarmed; 


vector[N]  RR_Black_Armed_Versus_White_Armed; 

vector[N]  RR_Black_Unarmed_Versus_White_Unarmed; 

vector[N]  RR_Black_Unarmed_Versus_White_Armed; 

############################################################################################ Calc Means
  Mu_Black_Armed=inv_logit(Mu[1]); 
  Mu_White_Armed=inv_logit(Mu[3]); 

  Mu_Black_Unarmed=inv_logit(Mu[2]); 
  Mu_White_Unarmed=inv_logit(Mu[4]); 


  Mu_RR_Black_Armed_Versus_Unarmed            =  inv_logit(Mu[1])/inv_logit(Mu[2]);
  Mu_RR_White_Armed_Versus_Unarmed            =  inv_logit(Mu[3])/inv_logit(Mu[4]);


  Mu_RR_Black_Armed_Versus_White_Armed        =  inv_logit(Mu[1])/inv_logit(Mu[3]);



  Mu_RR_Black_Unarmed_Versus_White_Unarmed    =  inv_logit(Mu[2])/inv_logit(Mu[4]);



  Mu_RR_Black_Unarmed_Versus_White_Armed      =  inv_logit(Mu[2])/inv_logit(Mu[3]); 


  ############################################################################################ Calc Full Vectors
  for(i in 1:N){
  Black_Armed[i]   =inv_logit(Theta[i,1]); 
  White_Armed[i]   =inv_logit(Theta[i,3]); 


  Black_Unarmed[i]   =inv_logit(Theta[i,2]); 
  White_Unarmed[i]   =inv_logit(Theta[i,4]); 



  RR_Black_Armed_Versus_Unarmed[i]    =  inv_logit(Theta[i,1])/inv_logit(Theta[i,2]);
  RR_White_Armed_Versus_Unarmed[i]    =  inv_logit(Theta[i,3])/inv_logit(Theta[i,4]);


  RR_Black_Armed_Versus_White_Armed[i]    =  inv_logit(Theta[i,1])/inv_logit(Theta[i,3]);

  RR_Black_Unarmed_Versus_White_Unarmed[i]   =  inv_logit(Theta[i,2])/inv_logit(Theta[i,4]);

  RR_Black_Unarmed_Versus_White_Armed[i]     =  inv_logit(Theta[i,2])/inv_logit(Theta[i,3]); 
  }
}