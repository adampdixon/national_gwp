library(dplyr)
library(tidyr)

# setwd('/glade/work/apdixon')

# nclim_dir<-'/glade/work/apdixon/Output_nClimGrid'

nclim_dir<-'/home/ap/Scratch/'
geo_link_dir<-'/home/ap/Documents/GitHub/national_gwp/Data/County'


fields<-c('GEOID', 'date', 'precip','tmax','tmin','doy')

######################################################
# get all files together
###########################
climate_df<-data.frame()
for (i in c('prcp', 'tmax')){
  data_raw<-list.files(nclim_dir, pattern = i, full.names = T)
  data<-data_raw[grep('.csv', data_raw)]
  
  #read multiple csvs into data.frame
  data_df<-do.call(rbind, lapply(data, read.csv))%>%as_tibble()
  
  # add the variable name to column, table will be long format
  data_df$variable<-i
  
  # create col names that are usable across variables
  colnames(data_df)<-c('GEOID', 'value', 'year', 'doy', 'variable')
  
  # rbind data
  climate_df<-rbind(data_df, climate_df)
}


# add geolink table
geo_link<-read.csv(file.path(geo_link_dir, 'county_geoid_link.csv'))%>%
  select(zh_geoid, REAL_GEOID)%>%
  as_tibble()

# join with the GEOID given by Zhuonan
climate_df_<-left_join(climate_df, geo_link, by = c('GEOID' = 'zh_geoid'))

# create wide format with pivot
climate_df_wide<-pivot_wider(climate_df_, names_from = variable, values_from = value)
