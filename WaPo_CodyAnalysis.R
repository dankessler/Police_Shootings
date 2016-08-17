# this file aims to replicate all of Cody Ross's analysis
# but using the more comprehensive Washington Post data
source('WaPo_funcs.R')

library(plyr)
library(reshape2)
library(acs)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(stringsAsFactors=FALSE)

## Load and rearrange the WaPo Data
wapo <- read.csv('/home/kesslerd/repos/Analysis/PoliceShootings/data-police-shootings/fatal-police-shootings-data.csv')

wapo <- addCounties(wapo)
wapo <- binarizeArmed(wapo)
wapo.summary <- summarizeShooting(wapo)

wapo.fortify <- fortifyCody(wapo.summary)
wapo.fortify [ is.na(wapo.fortify$src.cody),1:10][3,2]



## Run Model1
N <- nrow(wapo.fortify)

model_dat <- list(
                  Nblack=wapo.fortify$BAC_TOT,
                  Nwhite=wapo.fortify$WA_TOT,
                  UnarmedBlack=wapo.fortify$B_unarmed,
                  ArmedBlack=wapo.fortify$B_armed,
                  UnarmedWhite=wapo.fortify$W_unarmed,
                  ArmedWhite=wapo.fortify$W_armed
                  )


print(fitKilling,digits_summary=4,pars=c("Mu"))
print(fitKilling,digits_summary=4,pars=c("Theta"))
print(fitKilling,digits_summary=4,pars=c('Mu_RR_Black_Unarmed_Versus_White_Unarmed'))

if(file.exists('/home/kesslerd/repos/Analysis/PoliceShootings/WapoCountyFit.csv')){
    wapo.countyFit <- read.csv('WapoCountyFit.csv')
} else {
    fitKilling <- stan(file='/home/kesslerd/repos/Analysis/PoliceShootings/CountyLevel.stan',
                   data = model_dat,init=0, thin=1, iter = 2000, warmup=1000,chains = 1,refresh=1)
    wapo.countyFit <- countyShootings(wapo.fortify,fitKilling)
    write.csv(wapo.countyFit,'WapoCountyFit.csv',row.names=FALSE)
}






## Run Model2

wapo.CovList <- covPrep(wapo.countyFit)


fitCov <- stan(file='/home/kesslerd/repos/Analysis/PoliceShootings/Cov_Model12.stan',
               data = wapo.CovList,thin=1, iter=4000, warmup = 2000, chains =1, refresh =10, pars=c('Theta','Sigma'))

print(fitCov,digits_summary=4,pars=c("Theta","Sigma"))
