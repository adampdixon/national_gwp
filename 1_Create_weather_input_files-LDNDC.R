#######################################
# Function: 1_Create_weather_input_files-LDNDC.R
# Author: Ellen Maas
# Date: July 11, 2022
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates files in the appropriate folder for each model.
# Description: "This procedure generates weather input files for every model in the 
# format needed by each. There are some gaps of missing data on various days, so 
# it fills in those values with NASA Power data (which includes radiation data). 
# Calls custom nasapower_download function."
#######################################

print("Starting 1_Create_weather_input_files-LDNDC.R")

if(clim_scenario_num==1) {
  
  # experimental period only (1989-2021)
  
  ## Select year, dayofyear, radiation (W/m^2), maxt, mint, precip (mm), mean wind (m/s)
  DNDC_basic <- new_dat[,c("year","dayofyear","rain_mm.x","tavg","maxt_C.x","mint_C.x",
                           "radn_Wm2.x","meanw_ms")]
  colnames(DNDC_basic) <- c("year","dayofyear","prec","tavg","tmax","tmin","grad","wind")
  
  
  ## Calculate aggregated variables
  annual_tot_precipitation <- DNDC_basic[,c("year","prec")] %>%
    group_by(year) %>%
    summarize(ann_sum=sum(prec))
  annual_precipitation <- round(mean(annual_tot_precipitation$ann_sum,na.rm=T),1)
  
  temperature_average <- round(mean(DNDC_basic$tavg),1)
  
  temperature_amplitude <- max(DNDC_basic$tmax) - min(DNDC_basic$tmin)
  
  wind_speed <- round(mean(DNDC_basic$wind),1)
  
  time <- paste0(as.character(as.Date(first(DNDC_basic$dayofyear)-1, 
                                      origin=paste0(as.character(first(DNDC_basic$year)),"-01-01"))),"/1")
  
  
  ## output header data
  DNDC_wth_file <- paste0(dndc_path,"climate_exp.txt")
  
  header_txt <- c("%global",
                  paste0("        time = \"",time,"\"\n"),
                  "%climate",
                  paste0("        id = ",site_id,"\n"),
                  "%attributes",
                  paste0("        elevation = \"",elevation_m,"\""),
                  paste0("        latitude = \"",latitude,"\""),
                  paste0("        longitude = \"",longitude,"\""),
                  paste0("        wind speed = \"",wind_speed,"\""),
                  paste0("        annual precipitation = \"",annual_precipitation,"\""),
                  paste0("        temperature average = \"",temperature_average,"\""),
                  paste0("        temperature amplitude = \"",temperature_amplitude,"\""),
                  "\n",
                  "%data",
                  "*\t *\t prec\t tavg\t tmax\t tmin\t grad\t wind"
  )
  
  writeLines(header_txt,DNDC_wth_file)
  
  ## add data
  write.table(DNDC_basic,sep="\t",
              file=DNDC_wth_file,
              append=TRUE,
              row.names = F,
              col.names = F)
  
  
  # future period (1994-2021 repeated 3 times)
  
  ## Select year, dayofyear, radiation (W/m^2), maxt, mint, precip (mm), mean wind (m/s)
  DNDC_basic_fut <- new_dat_2100[,c("year","dayofyear","rain_mm.x","tavg","maxt_C.x","mint_C.x",
                                    "radn_Wm2.x","meanw_ms")]
  colnames(DNDC_basic_fut) <- c("year","dayofyear","prec","tavg","tmax","tmin","grad","wind")
  
  ## don't recalculate the aggregated data; leave as-is (this note was added
  ## assuming that only the mean data for header reflects the current 30-year
  ## period, but LDNDC may want the average for the whole simulation, hence
  ## the recalculation below)
  ## DO recalculate aggregated variables
  annual_tot_precipitation <- DNDC_basic_fut[,c("year","prec")] %>%
    group_by(year) %>%
    summarize(ann_sum=sum(prec))
  annual_precipitation <- round(mean(annual_tot_precipitation$ann_sum,na.rm=T),1)
  
  temperature_average <- round(mean(DNDC_basic_fut$tavg),1)
  
  temperature_amplitude <- max(DNDC_basic_fut$tmax) - min(DNDC_basic_fut$tmin)
  
  wind_speed <- round(mean(DNDC_basic_fut$wind),1)
  
  time <- paste0(as.character(as.Date(first(DNDC_basic_fut$dayofyear)-1, 
                                      origin=paste0(as.character(first(DNDC_basic_fut$year)),"-01-01"))),"/1")
  
  
  
  ## output header data
  DNDC_wth_file_fut <- paste0(dndc_path,"climate_",clim_scenario_num,".txt")
  
  header_txt_fut <- c("%global",
                      paste0("        time = \"",time,"\"\n"),
                      "%climate",
                      paste0("        id = ",site_id,"\n"),
                      "%attributes",
                      paste0("        elevation = \"",elevation_m,"\""),
                      paste0("        latitude = \"",latitude,"\""),
                      paste0("        longitude = \"",longitude,"\""),
                      paste0("        wind speed = \"",wind_speed,"\""),
                      paste0("        annual precipitation = \"",annual_precipitation,"\""),
                      paste0("        temperature average = \"",temperature_average,"\""),
                      paste0("        temperature amplitude = \"",temperature_amplitude,"\""),
                      "\n",
                      "%data",
                      "*\t *\t prec\t tavg\t tmax\t tmin\t grad\t wind"
  )
  
  writeLines(header_txt_fut,DNDC_wth_file_fut)
  
  ## add data
  write.table(DNDC_basic_fut,sep="\t",
              file=DNDC_wth_file_fut,
              append=TRUE,
              row.names = F,
              col.names = F)
  
} else if(clim_scenario_num>=1) {
  
  # Get experimental period and bind to future
  DNDC_basic <- new_dat[,c("year","dayofyear","rain_mm.x","tavg","maxt_C.x","mint_C.x",
                           "radn_Wm2.x","meanw_ms")]
  colnames(DNDC_basic) <- c("year","dayofyear","prec","tavg","tmax","tmin","grad","wind")
  
  ## Future data
  fut_dat <- read.csv(file=paste0(fut_weather_path,"fut_clim_scenario_",clim_scenario_num,'_reanal.csv'))
  
  ## Select year, dayofyear, radiation (W/m^2), maxt, mint, precip (mm), mean wind (m/s)
  DNDC_basic_fut <- fut_dat[,c("year","dayofyear","rain_mm","tavg","maxt_C","mint_C",
                               "radn_Wm2","meanw_ms")]
  colnames(DNDC_basic_fut) <- c("year","dayofyear","prec","tavg","tmax","tmin","grad","wind")
  
  DNDC_basic_combined <- rbind(DNDC_basic,DNDC_basic_fut)
  
  
  ## Calculate aggregated variables
  annual_tot_precipitation <- DNDC_basic_fut[,c("year","prec")] %>%
    group_by(year) %>%
    summarize(ann_sum=sum(prec))
  annual_precipitation <- round(mean(annual_tot_precipitation$ann_sum,na.rm=T),1)
  
  temperature_average <- round(mean(DNDC_basic_fut$tavg),1)
  
  temperature_amplitude <- max(DNDC_basic_fut$tmax) - min(DNDC_basic_fut$tmin)
  
  wind_speed <- round(mean(DNDC_basic_fut$wind),1)
  
  time <- paste0(as.character(as.Date(first(DNDC_basic_fut$dayofyear)-1, 
                                      origin=paste0(as.character(first(DNDC_basic_fut$year)),"-01-01"))),"/1")
  
  
  ## output header data
  DNDC_wth_file <- paste0(dndc_path,"climate_",clim_scenario_num,".txt")
  
  header_txt <- c("%global",
                  paste0("        time = \"",time,"\"\n"),
                  "%climate",
                  paste0("        id = ",site_id,"\n"),
                  "%attributes",
                  paste0("        elevation = \"",elevation_m,"\""),
                  paste0("        latitude = \"",latitude,"\""),
                  paste0("        longitude = \"",longitude,"\""),
                  paste0("        wind speed = \"",wind_speed,"\""),
                  paste0("        annual precipitation = \"",annual_precipitation,"\""),
                  paste0("        temperature average = \"",temperature_average,"\""),
                  paste0("        temperature amplitude = \"",temperature_amplitude,"\""),
                  "\n",
                  "%data",
                  "*\t *\t prec\t tavg\t tmax\t tmin\t grad\t wind"
  )
  
  writeLines(header_txt,DNDC_wth_file)
  
  ## add data
  write.table(DNDC_basic_fut,sep="\t",
              file=DNDC_wth_file,
              append=TRUE,
              row.names = F,
              col.names = F)
  
}# if clim_scenario_num==1


rm(list=c("DNDC_basic","annual_tot_precipitation","annual_precipitation","temperature_average",
          "temperature_amplitude","wind_speed","time","DNDC_wth_file","header_txt",
          "DNDC_basic_fut"))
if(clim_scenario_num>1) {
  rm("fut_dat")
}
