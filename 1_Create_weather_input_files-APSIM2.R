#######################################
# Script: 1_Create_weather_input_files-APSIM2.R
# Author: Ellen Maas
# Date: July 11, 2022
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates files in the appropriate folder for each model.
# Description: "This procedure generates weather input files for every model in the 
# format needed by each. There are some gaps of missing data on various days, so 
# it fills in those values with NASA Power data (which includes radiation data). 
# Calls custom nasapower_download function."
#######################################
# Audit Log:
# 2022: Created script.
#######################################

print("Starting 1_Create_weather_input_files-APSIM2.R")

if(weather_ind=="C") {
  
  # Select year, dayofyear, radiation (MJ/m^2), maxt, mint, precip (mm)
  APSIM_basic <- new_dat[,c("year","dayofyear","radn_MJm2.x",
                            "maxt_C.x","mint_C.x","rain_mm.x")]
  colnames(APSIM_basic) <- c("year","day","radn","maxt","mint","rain")
  
  # find any columns with NA cells
  na_find <- names(which(colSums(is.na(new_dat[c(3:12053),]))>0))
  
  # write file header line (latitude) in CSV format
  write.table(data.frame(c(paste0("Latitude = ",latitude),paste0("Longitude = ",longitude))), 
              file=paste0(apsim_path,"basic_wth_exp.csv"),              
              quote = F, row.names = F, col.names = F)
  
  # add detail to file
  write.table(APSIM_basic, file=paste0(apsim_path,"basic_wth_exp.csv"),
              row.names=FALSE, quote=F, col.names=T, append=T, sep=',')
  
  ###########
  
  # baseline future period to 2100
  
  # Select year, dayofyear, radiation (MJ/m^2), maxt, mint, precip (mm)
  APSIM_basic_2100 <- new_dat_2100[,c("year","dayofyear","radn_MJm2.x",
                                      "maxt_C.x","mint_C.x","rain_mm.x")]
  colnames(APSIM_basic_2100) <- c("year","day","radn","maxt","mint","rain")
  
  # find any columns with NA cells
  na_find <- names(which(colSums(is.na(APSIM_basic_2100[c(3:nrow(new_dat_2100)),]))>0))
  latlon_rows <- as.data.frame(c(paste0("Latitude = ",latitude),paste0("Longitude = ",longitude)))
  
  write.table(data.frame(c(paste0("Latitude = ",latitude),paste0("Longitude = ",longitude))), 
              file=paste0(apsim_path,"basic_wth_",clim_scenario_num,".csv"),              
              quote = F, row.names = F, col.names = F)
  
  # write.table(latlon_rows,
  #             file=paste0("APSIM/",site_name,"_basic_wth_constants_2100.txt"),
  #             quote = F, row.names = F, col.names = F, append=F)
  
  write.table(APSIM_basic_2100, file=paste0(apsim_path,"basic_wth_",clim_scenario_num,".csv"),
              row.names = F, quote=F, col.names=T, append=T, sep=',')
  
  ### NOTE: Will have to manually remove final blank line from file before APSIM will accept it.
  
} else if(weather_ind=="F") {
  
  fut_dat <- read.csv(file=paste0(fut_weather_path,"fut_clim_scenario_",clim_scenario_num,'.csv'))
  
  # Get experimental period and bind to future
  
  ## Select year, dayofyear, radiation (MJ/m^2), maxt, mint, precip (mm)
  APSIM_basic <- new_dat[,c("year","dayofyear","radn_MJm2.x",
                            "maxt_C.x","mint_C.x","rain_mm.x")]
  colnames(APSIM_basic) <- c("year","day","radn","maxt","mint","rain")
  APSIM_fut <- fut_dat[,c("year","dayofyear","radn_MJm2",
                                "maxt_C","mint_C","rain_mm")]
  colnames(APSIM_fut) <- c("year","day","radn","maxt","mint","rain")
  APSIM_basic_esm <- rbind(APSIM_basic,APSIM_fut)
  
  # find any columns with NA cells
  na_find <- names(which(colSums(is.na(fut_dat[c(3:12053),]))>0))
  
  # write file header line (latitude) in CSV format
  write.table(data.frame(c(paste0("Latitude = ",latitude),paste0("Longitude = ",longitude))), 
              file=paste0(apsim_path,"basic_wth_",clim_scenario_num,".csv"),              
              quote = F, row.names = F, col.names = F)
  
  # add detail to file
  write.table(APSIM_basic_esm, file=paste0(apsim_path,"basic_wth_",clim_scenario_num,".csv"),
              row.names=FALSE, quote=F, col.names=T, append=T, sep=',')
  
  
} else {
  
  print(paste0("Unknown weather_ind=",weather_ind,"in 1_Create_weather_input_files-RothC.R"))
  
}# if weather_ind == C or F

rm(list = c("APSIM_basic","APSIM_basic_2100","na_find","latlon_rows","APSIM_fut",
            "fut_dat","APSIM_basic_esm"))