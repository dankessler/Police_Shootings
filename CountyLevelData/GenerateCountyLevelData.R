library(dplyr)
library(tidyr)

raw.cens <- read.csv('CC-EST2015-ALLDATA.csv')

raw.mpv <- read.csv('MPVDatasetDownload.csv')

## we need to "tidy" the census data

cens <- select(raw.cens,STNAME:AGEGRP,NHWAC_MALE:H_FEMALE) %>%
    filter(YEAR %in% 3:8) %>%
    gather(demo,Pop,NHWAC_MALE:H_FEMALE) %>%
    separate(demo,c('Race','Gender')) %>%
    group_by(CTYNAME,STNAME,YEAR,Race) %>% summarise(Pop=sum(Pop)) %>% ungroup %>%
    mutate(YEAR=recode(YEAR,
                       `3`=2010,
                       `4`=2011,
                       `5`=2012,
                       `6`=2013,
                       `7`=2014,
                       `8`=2015)) %>%
    mutate(CTYNAME=gsub(' County','',CTYNAME)) %>%
    mutate(STNAME=state.abb[match(STNAME,state.name)]) %>%
    bind_rows(cens %>% filter(YEAR==2015) %>% mutate(YEAR=2016)) # carry 2015 estimates -> 2016



## now tidy the MPV data
mpv <- raw.mpv %>%
    select(Race = Victim.s.race,
           Date=Date.of.injury.resulting.in.death..month.day.year.,
           CTYNAME=Location.of.death..county.,
           STNAME=Location.of.death..state.,
           ArmedStatus=Unarmed) %>%
    mutate(ArmedStatus=make.names(ArmedStatus)) %>%
    mutate(YEAR=as.numeric(format(as.Date(Date,'%m/%d/%Y'),'%Y'))) %>%
    mutate(Race = recode(Race,
                         Black='NHBAC',
                         White='NHWAC',
                         Hispanic='H',
                         Asian='NHAAC',
                         `Native American`='NHIAC',
                         `Pacific Islander`='NHNAC')) %>%
    group_by(CTYNAME,STNAME,YEAR,Race,ArmedStatus) %>% tally %>% ungroup


## now put the two datasets together
shoot <- left_join(cens,mpv) %>%
    spread(ArmedStatus,n,fill=0) %>%
    select(-`<NA>`)

write.csv(shoot,'ShootingsByCounty.csv')
