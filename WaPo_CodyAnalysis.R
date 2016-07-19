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

## Load Cody's data

cody <- read.csv('/home/kesslerd/repos/Analysis/PoliceShootings/SupplementaryMaterials/Data/MapFileData-WithCountyResultsAndCovariates.csv',stringsAsFactors=FALSE)


## Merge WaPo and Cody Data
cody$src <- 'cody'
agg.wapo$src <- 'wapo'

cody2 <- cody[!is.na(cody$County.Name),]
agg.wapo2 <- agg.wapo[!is.na(agg.wapo$County.Name),]
full <- merge(x=agg.wapo2,y=cody2,by=c('County.Name','State'),suffixes=c('.wapo','.cody'),all.x=TRUE)

## 
