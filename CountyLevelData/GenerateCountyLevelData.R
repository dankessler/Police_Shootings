library(dplyr)
library(tidyr)

raw.cens <- read.csv('CC-EST2015-ALLDATA.csv.gz')

raw.mpv <- read.csv('MPVDatasetDownload.csv')

## we need to "tidy" the census data

cens <- select(raw.cens,STNAME:AGEGRP,TOT_POP,NHWAC_MALE:H_FEMALE) %>%
    filter(YEAR %in% 3:8) %>%
    gather(demo,Pop,TOT_POP,NHWAC_MALE:H_FEMALE) %>%
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
    mutate(Race=recode(Race,
                       NHBAC='Black',
                       NHWAC='White',
                       H='Hispanic',
                       NHAAC='Asian',
                       NHIAC='Native.American',
                       NHNAC='Pacific.Islander')) %>%
    spread(ArmedStatus,n,fill=0) %>%
    select(-`<NA>`)

write.csv(shoot,'ShootingsByCounty.csv')

## structure it wide for CS
shoot.wide <- shoot %>%
    mutate(Race=recode(Race,
                       NHBAC='Black',
                       NHWAC='White',
                       H='Hispanic',
                       NHAAC='Asian',
                       NHIAC='Native.American',
                       NHNAC='Pacific.Islander')) %>%
    filter(YEAR > 2012) %>%
    gather(ArmedStatus,Shootings,Allegedly.Armed:Vehicle) %>%
    gather(Measure,N,Pop,Shootings) %>%
    unite(RaceYearArmedMeasure,YEAR:Measure) %>%
    spread(RaceYearArmedMeasure,N)

write.csv(shoot.wide,'ShootingsByCounty_wide.csv')
