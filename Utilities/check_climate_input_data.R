# check CLIMATE county files for completion
# Adam Dixon
# 25 July 2024

# if data is not present, it will not have a record in the climate_geoid column

# Script is written to be executed on Casper/Derecho using 'Rscript check_climate_input_data.R'

library(dplyr)
library(tidyr)
library(data.table)


if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    source('/home/ap/Documents/GitHub/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  } else {
    source('/glade/derecho/scratch/apdixon/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  }
}

print('checking climate data...')

# soils_data<-list.files(soil_data_path, pattern = 'csv')

climate_data<-list.files(climate_data_path, pattern = 'csv')

#unique climate types
# list.files(climate_data_path, pattern = 'cmip6')
# cmip6 - tmax, tmin, sfcwind, rsds, prcp - 5 total
# list.files(climate_data_path, pattern = 'nclim')
# nclim - prcp, tmax, tmin - 3 total


county_data<-fread(file.path(master_path, 'Data', 'County_start', 'county_centroids_elevation_crops.csv'))%>%
  select(GEOID, NAME, State_Name)

s<-strsplit(climate_data, '_')

v<-data.frame(geoid = numeric(), ssp585 = numeric(), ssp126 = numeric(), nclim = numeric())
for(i in 1:length(s)){
  
  filesize<-file.size(file.path(climate_data_path, climate_data[i]))
  
  g<-s[[i]][2] # gets geoid
  t<-s[[i]][3] # gets climate d type
  
  if (grepl('ssp585', t)){
    ssp585 = 1;ssp126=0;nclim=0
  } else if (grepl('ssp126', t)){
    ssp126 = 1;ssp585=0;nclim=0
  } else if (grepl('nclim', t)){
    nclim = 1;ssp585=0;ssp126=0
  }
  v<-rbind(v, cbind(g, ssp585, ssp126, nclim, filesize))
}

names(v) = c("climate_geoid", "ssp585", "ssp126", "nclim", "filesize")
v$climate_geoid<-as.numeric(v$climate_geoid)

v<-v%>%as_tibble()%>%mutate_all(as.numeric)

j<-v%>%
  group_by(climate_geoid)%>%
  summarize(ssp585 = sum(ssp585), ssp126 = sum(ssp126), nclim = sum(nclim), total_bytes = sum(filesize))%>%
  left_join(county_data, by = c('climate_geoid'='GEOID'))

j<-j%>%
  mutate(total = ssp585 + ssp126 + nclim,
          check = ifelse(total == 13, '', 'MISSING DATA'))



output_<-file.path(national_figs, paste0('check_climate_input_data_', date, '.csv'))

fwrite(j, output_)

yesno<-file.exists(output_)
print('done climate data...')
print(paste('climte results data present:', yesno, output_))
