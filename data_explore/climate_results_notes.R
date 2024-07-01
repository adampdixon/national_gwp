# just looking at climate data results

library(dplyr)
library(stringr)

f<-read.csv('/home/ap/Documents/GitHub/national_gwp/sh_sends/filenames2.txt')

head(f)

colnames(f)<-'filename'

f2<-f%>%
  mutate(GEOID = as.integer(str_split(filename, "_", simplify = T)[,2]),
         var = str_split(filename, "_", simplify = T)[,1])

cmip<-f2%>%
  filter(str_detect(filename, 'cmip6'))

nrow(cmip)
head(cmip)

nclim<-f2%>%
  filter(str_detect(filename, 'nclim'))


nrow(nclim)
head(nclim)

geo_link_dir<-'/home/ap/Documents/GitHub/national_gwp/Data/County'
geo_link<-read.csv(file.path(geo_link_dir, 'county_geoid_link.csv'))%>%
  select(zh_geoid, REAL_GEOID)%>%
  as_tibble()%>%
  arrange(REAL_GEOID)

nrow(geo_link)

nclim2<-left_join(nclim, geo_link, by = c('GEOID'='REAL_GEOID'))
head(nclim2)
# write.csv(nclim2, '/home/ap/Scratch/nclim2.csv')

cmip2<-left_join(cmip, geo_link, by = c('GEOID'='REAL_GEOID'))
head(cmip2)
# write.csv(cmip2, '/home/ap/Scratch/cmip2.csv')



nclim3<-nclim2%>%filter(!is.na(filename))
head(nclim3)
nrow(nclim3)

nclim4<-nclim2%>%filter(!is.na(filename), var=='prcp')
head(nclim4)
nrow(nclim4)


