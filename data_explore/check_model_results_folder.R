#######################################
# File: "check_model_results_folder.R"
# Author: "Adam Dixon"
# Date: "July 2024"
# Description: This script loops through all county folders and checks the number of files and then rows in one of the 
# model csvs. It also puts the crop hectares in the same row so that can be compared with results. If there is more than 1 ha
# of the crop there should be a full set of results.
#
#######################################


if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    source('/home/ap/Documents/GitHub/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  } else {
    source('/glade/derecho/scratch/apdixon/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  }
}


library(dplyr)
library(data.table)

r<-dir(results_folder, full.names = T)

county_data<-read.csv(file.path(master_path, 'Data', 'County_start', 'county_centroids_elevation_crops.csv'))

GEOIDS<-county_data$GEOID

results_data<-data.frame()

for (i in GEOIDS[1:3]){
  county_data_1<-filter(county_data, GEOID==i)
  
  county_geoid<-county_data_1$GEOID 
  county_name<-county_data_1$NAMELSAD
  state_name<-county_data_1$State_Name
  Maize_ha<-county_data_1$Maize_ha
  Cotton_ha<-county_data_1$Cotton_ha
  Soybean_ha<-county_data_1$Soybean_ha
  Wheat_ha<-county_data_1$Wheat_ha
  
  get_county_results_folder<-r[grep(paste0("_", county_geoid, "_"), r)]
  
  daycent_results<-list.files(get_county_results_folder, pattern = 'Daycent_', full.names = T)
  ldndc_results<-list.files(get_county_results_folder, pattern = 'LDNDC_', full.names = T)
  mill_results<-list.files(get_county_results_folder, pattern = 'Millennial_', full.names = T)
  
  day_files<-length(daycent_results)
  ldndc_files<-length(ldndc_results)
  mill_files<-length(mill_results)
  
  d_rows<-ifelse(day_files == 0, 0 , nrow(fread(daycent_results[1])))
  ld_rows<-ifelse(ldndc_files == 0, 0 , nrow(fread(ldndc_results[2])))
  mill_rows<-ifelse(mill_files == 0, 0 , nrow(fread(mill_results[6])))
  
  files_expected = 0 # essentially a counter
  # number of files expected
  if(Maize_ha > 1){files_expected = files_expected + 144} # if Maize is grown, Rotation is also run, so 72+72
  # 1 crop x 6 practices x 2 climate scenarios x 2 types of data(annual/daily) x 3 models = 72 files
  # number of files expected
  if(Cotton_ha > 1){files_expected = files_expected + 72}
  # number of files expected
  if(Wheat_ha > 1){files_expected = files_expected + 72}
  # number of files expected
  if(Soybean_ha > 1){files_expected = files_expected + 72}
  

  
  
  results_data<-rbind(results_data, 
                    data.frame(county_geoid, county_name, state_name, Maize_ha, Cotton_ha, Soybean_ha, 
                               Wheat_ha, day_files, ldndc_files, mill_files,
                               d_rows, ld_rows, mill_rows, files_expected))
  

}


fwrite(results_data, file.path(national_figs, 'checking_results_data.csv'))
