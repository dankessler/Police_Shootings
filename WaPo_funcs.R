s## function library

addCounties <- function(df){
    names(df)[grepl('state',names(df))] <- 'State' # shift case to match Cody
    cities <- unique(df[,c('city','State')])
    handmap <- read.csv('/home/kesslerd/repos/Analysis/PoliceShootings/city_county_map/HandMappings.csv')
    cities$County.Name <- mapply(getCounty,cities$State,cities$city,MoreArgs=list(handmap=handmap))
    cities$County.Name <- unlist(cities$County.Name)
    return(merge(df,cities,all.x=TRUE))
}

getCounty <- function(state,city,handmap){
    hand.candidate <- handmap[handmap$city==city & handmap$State==state,]
    if (nrow(hand.candidate)==1){
        return(hand.candidate['Hand_CountyName'])
    }
         
    candidates <- geo.lookup(state=state,place=city)
    if (nrow(candidates) < 2){ # confirm that we have some hits
        return ('NoMatch')
    }
    candidates <- candidates[-1,] # drop the first hit which is null
    candidates <- unique(candidates) # deal with duplicates
    
    dists <- adist(city,candidates[,'place.name'])
    shortest <- min(dists)
    dups <- sum(dists==shortest)
    if (dups>1){
        return('MultiRowMatch')
    }
    bestind <- which.min(dists)
    county <- candidates[bestind,'county.name']
    return(county)
}


binarizeArmed <- function(df){
    df$armed.binary <- 'armed' # assume everybody is unarmed
        df$armed.binary <- ifelse(df$armed %in% c('undetermined',''),NA,df$armed.binary) # if unsure, NA
    df$armed.binary <- ifelse(df$armed %in% c('unarmed'),'unarmed',df$armed.binary) # if unarmed, set it
    return(df)
}

summarizeShooting <- function(df){
    df.cast <- dcast(df, State + County.Name ~ race + armed.binary,subset = .(race %in% c('B','W','H') & !is.na(armed.binary)), fun.aggregate=length,value.var='armed.binary')
    return(df.cast)
}

fortifyCody <- function(df){
    # merge in Cody's data
    cody <- read.csv('/home/kesslerd/repos/Analysis/PoliceShootings/SupplementaryMaterials/Data/MapFileData-WithCountyResultsAndCovariates.csv')

    cody$src <- 'cody'
    df$src <- 'wapo'

    cody <- cody[!is.na(cody$County.Name) & !is.na(cody$State),]
    df <- df[!is.na(df$County.Name) & !is.na(df$State),]

    df$County.Name <- strtrim(df$County.Name,23) # cody's file is truncated
    df$County.Name <- iconv(df$County.Name,to='ASCII//TRANSLIT') # cody's file has no accents

    full <- merge(x=df,y=cody,by=c('State','County.Name'),suffixes=c('.wapo','.cody'),all.x=TRUE)

    mask <- complete.cases(
            full[,c('BAC_TOT',
            'WA_TOT',
            'B_unarmed',
            'B_armed',
            'W_unarmed',
            'W_armed')])
    full <- full[mask,]

    full <- full[,1:64]
    
    return(full)
}

countyShootings <- function(df,sfit){
    # extract county level means and sds

    # extract county-level posteriors, log xform, then get mean and sd, and tack on to df
    countyRR <- extract(sfit,pars=c('RR_Black_Unarmed_Versus_White_Unarmed'))[[1]]

    countyMeans <- apply(log(countyRR),2,mean)
    countySDs <- apply(log(countyRR),2,sd)

    df$m.log.RR_Black_Unarmed_Versus_White_Unarmed <- countyMeans

    df$sd.log.RR_Black_Unarmed_Versus_White_Unarmed <-  countySDs

    return(df)
}

covPrep <- function(g){
# prepare the dataframe for running covariate models
# for now: just model 12 is supported
#
# note: return the list that stan wants

    Ym<- g$m.log.RR_Black_Unarmed_Versus_White_Unarmed     # First Outcome
    Ysd<- g$sd.log.RR_Black_Unarmed_Versus_White_Unarmed   #
    WhiteAssault <- (g$AssaultsWhite.sum/g$WA_TOT)
    BlackAssault <- (g$AssaultsBlack.sum/g$BAC_TOT)

    Pop<-g$TOT_POP

    BlackRatio<-(g$BAC_TOT+1)/Pop

    g2<- data.frame(g$County.FIPS.Code,Ym,Ysd, Pop,BlackRatio,WhiteAssault,BlackAssault)
    g3 <- g2[complete.cases(g2$Ym),]
    
    Ym  <- g3$Ym
    Ysd <- g3$Ysd
    
    N<-length(Ym)

    Pop <-g3$Pop/sd(g3$Pop,na.rm=T)
    BlackRatio<-g3$BlackRatio

    WhiteAssault <-g3$WhiteAssault/sd(g3$WhiteAssault,na.rm=T)
    MaxWhiteAssault<-max(WhiteAssault,na.rm=T)
    
    BlackAssault <-g3$BlackAssault/sd(g3$BlackAssault,na.rm=T)
    MaxBlackAssault<-max(BlackAssault,na.rm=T)

    WhiteAssault <- ifelse(WhiteAssault==0,NA,WhiteAssault)
    MissCumSumWhiteAssault <-cumsum(is.na(WhiteAssault))
    MissCumSumWhiteAssault <-ifelse(MissCumSumWhiteAssault ==0,1,MissCumSumWhiteAssault )
    NonMissWhiteAssault  <-ifelse(is.na(WhiteAssault ),0,1)
    NmissWhiteAssault <-sum(is.na(WhiteAssault ))
    WhiteAssault [is.na(WhiteAssault )]<-9999999
    
    BlackAssault <- ifelse(BlackAssault==0,NA,BlackAssault)
    MissCumSumBlackAssault <-cumsum(is.na(BlackAssault))
    MissCumSumBlackAssault <-ifelse(MissCumSumBlackAssault ==0,1,MissCumSumBlackAssault )
    NonMissBlackAssault  <-ifelse(is.na(BlackAssault ),0,1)
    NmissBlackAssault <-sum(is.na(BlackAssault ))
    BlackAssault [is.na(BlackAssault )]<-9999999

    Ones<-rep(1,N)

    model_dat  <-list(N=N,
                      Ym=Ym,
                      Ysd=Ysd,

                      MissCumSumWhiteAssault=MissCumSumWhiteAssault,
                      NonMissWhiteAssault=NonMissWhiteAssault,
                      NmissWhiteAssault=NmissWhiteAssault,
                      WhiteAssault=WhiteAssault,
                      MaxWhiteAssault=MaxWhiteAssault,
                      
                      MissCumSumBlackAssault=MissCumSumBlackAssault,
                      NonMissBlackAssault=NonMissBlackAssault,
                      NmissBlackAssault=NmissBlackAssault,
                      BlackAssault=BlackAssault,
                      MaxBlackAssault=MaxBlackAssault,
                      
                      BlackRatio=BlackRatio,
                      Pop=Pop,

                      Ones=Ones
                      
                      )
    

}
