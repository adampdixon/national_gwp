# check CLIMATE county files for completion

# if data is not present, it will not have a record in the climate_geoid column

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

county_data<-fread(file.path(master_path, 'Data', 'County_start', 'county_centroids_elevation_crops.csv'))%>%
  select(GEOID, NAME, State_Name)

s<-strsplit(soils_data, '_')

v<-data.frame()
for(i in 1:length(s)){
  g<-s[[i]][2] # gets geoid
  t<-s[[i]][3] # gets climate d type
  v<-rbind(v, cbind(g, t))
}

df<-data.frame(soil_geoid = as.integer(v),
               ID = 1:length(v))

j<-left_join(county_data, df, by = c('GEOID' = 'soil_geoid'), keep = TRUE)


fwrite(j, file.path(national_figs, 'check_gNATSGO_input_data.csv'))

yesno<-file.exists(file.path(national_figs, 'check_gNATSGO_input_data.csv'))

print('done soils data...')
print(paste('soils data present:', yesno))
