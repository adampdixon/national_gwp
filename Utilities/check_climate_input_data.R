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


tocheck<-c(1001,5135,6003,6109,9003,12017,12019,12029,12041,12043,12051,12055,12071,12085,
12093,12107,12111,12121,12123,16035,20003,23017,23019,23021,25001,25003,25011,
25015,25023,26019,26027,26029,26047,26077,26085,26113,26149,26153,26165,28053,
28119,28125,28133,29043,29059,29067,29091,29093,29105,29145,29149,29179,29225,
29229,30029,31005,31075,31091,31117,31171,33003,33007,33009,33019,36045,36059,
36083,36103,36105,39039,39125,39137,41005,41011,41039,41043,41047,41051,41057,
44003,44005,46095,46099,46101,46107,46111,46115,46125,46135,47009,47011,47013,
47015,47017,47023,47027,47031,47041,47043,47045,47049,47051,47053,47055,47057,
47059,47061,47063,47071,47073,47081,47083,47087,47093,47095,47097,50011,50015,
50023,50025,50027,51059,51087,51089,51153,51161,51660,51685,51690,51760,51775,
53011,53015,53027,53029,53031,53045,55011,55091,55125)

county_data<-fread(file.path(master_path, 'Data', 'County_start', 'county_centroids_elevation_crops.csv'))%>%
  select(GEOID, NAME, State_Name)%>%
  filter(GEOID %in% tocheck)

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



output_<-file.path(national_figs, 'check_climate_input_data.csv')

fwrite(j, output_)

yesno<-file.exists(output_)
print('done climate data...')
print(paste('climte results data present:', yesno, output_))
