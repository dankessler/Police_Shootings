## function library

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
    return(full)
}
