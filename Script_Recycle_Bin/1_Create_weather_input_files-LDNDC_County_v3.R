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

library(zoo) # to impute missing data

print("Starting 1_Create_weather_input_files-LDNDC_County_v3 NASA POWER.R")


climate_data<-list.files(climate_data_path, full.names = TRUE, pattern = ".csv")

print("Starting 1_Create_weather_input_files-LDNDC.R")

#https://pcmdi.llnl.gov/CMIP6/Guide/dataUsers.html

# best description of rsds units https://pcmdi.llnl.gov/mips/cmip3/variableList.html, search "rsds"

# v2 uses global radiation data from NREL, see utilities for script that creates file


# Future 2022 - 2050
if (fut_climate == 1) {
  # climate scenario low change
  cmip_scen<-'_ssp126_gfdl-esm4__cmip6.csv'
}
if (fut_climate == 2) {
  # climate scenario high change
  cmip_scen<-'_ssp585_gfdl-esm4__cmip6.csv'
}

# rad_cov<-100

wind_year_mean<-mutate(fread(climate_data[grep(paste0("sfcwind_", county_geoid, cmip_scen), climate_data)]),
           wind = value)%>%
  select(GEOID, wind, year, doy)%>%
  group_by(doy)%>% # grouping by doy produces an average year
  summarise(wind = mean(wind))


nasa_power_files<-'/home/ap/NASA_POWER2'

rad_year_nasa<-mutate(fread(list.files(nasa_power_files, full.names = T, pattern = paste0("_", county_geoid, "_"))),
           radiation = radn_Wm2,
           LW_radiation = LW_radn_Wm2)%>%
  select(dayofyear, radiation) # LW_radiation leaving out longwave radiation for now



# if(clim_scenario_num==1) {

# historic period - 1951 to 2021

#historic - 1950-2021
historic_data<-mutate(fread(climate_data[grep(paste0("tmax_", county_geoid, "_nclim.csv"), climate_data)]),
                      tmax = value)%>%
  left_join(
    mutate(fread(climate_data[grep(paste0("tmin_", county_geoid, "_nclim.csv"), climate_data)]),
           tmin = value), 
    by=c('year', 'doy'))%>%
  left_join(
    mutate(fread(climate_data[grep(paste0("prcp_", county_geoid, "_nclim.csv"), climate_data)]),
           precip = value),
    by=c('year', 'doy'))%>%
  select(year, doy, tmax, tmin, precip)%>%
  mutate(date_object = lubridate::ymd(paste(year, "-01-01")) + days(doy - 1))%>% #chatgpt derived
  mutate(month = lubridate::month(date_object),
         day = lubridate::day(date_object),
         jday = doy)%>%
  select(day, month, year, jday, tmax, tmin, precip)

# Add wind and radiation year average to each year
historic_data2<-left_join(historic_data, wind_year_mean, by=c('jday' = 'doy'))%>%
  left_join(rad_year_nasa, by=c('jday' = 'dayofyear'))%>%
  mutate(tavg = (tmax + tmin)/2)%>% # SO WRONG :(
  mutate(radiation2 = -99.99)%>% # TODO since can't get good values using what LDNDC tutorials did
  select(year, jday, precip, tavg, tmax, tmin, radiation2, wind) # LW_radiation


# TODO add the future data and rbind

future_data<-mutate(fread(climate_data[grep(paste0("tmax_", county_geoid, cmip_scen), climate_data)]),
                    tmax = value)%>%
  left_join(
    select(mutate(fread(climate_data[grep(paste0("tmin_", county_geoid, cmip_scen), climate_data)]),
                  tmin = value),
           year, doy, tmin),
    by=c('year', 'doy'))%>%
  left_join(
    select(mutate(fread(climate_data[grep(paste0("prcp_", county_geoid, cmip_scen), climate_data)]),
                  precip = value),
           year, doy, precip),
    by=c('year', 'doy'))%>%
  # left_join(
  #   select(mutate(fread(climate_data[grep(paste0("rsds_", county_geoid, cmip_scen), climate_data)]),
  #                 radiation = value/rad_cov), # convert from kW to W ?
  #          year, doy, radiation),
  #   by=c('year', 'doy'))%>%
  left_join(
    select(mutate(fread(climate_data[grep(paste0("sfcwind_", county_geoid, cmip_scen), climate_data)]),
                  wind = value),
           year, doy, wind),
    by=c('year', 'doy'))%>%
  mutate(date_object = lubridate::ymd(paste(year, "-01-01")) + days(doy - 1))%>% #chatgpt derived
  mutate(month = lubridate::month(date_object),
         day = lubridate::day(date_object),
         jday = doy,
         tavg = (tmax+tmin)/2)%>% # TODO TERRIBLE WAY TO DO THIS
  mutate(radiation2 = -99.99)%>% # TODO major assumption TERRIBLE
  left_join(rad_year_nasa, by=c('jday' = 'dayofyear'))%>%
  select(year, jday, precip, tavg, tmax, tmin, radiation2, wind) # LW_radiation



clim_data<-rbind(historic_data2, future_data)

nrow(na.omit(clim_data))
nrow(clim_data)

# remove NAs
clim_data<-na.locf(clim_data)

nrow(clim_data)



names(clim_data)<-c("year","dayofyear","rain_mm","tavg","maxt_C","mint_C","grad","meanw_ms") # "lrad"


# reduce the sig digits
cols<-c("rain_mm","tavg","maxt_C","mint_C","grad", "meanw_ms")
clim_data<-mutate_at(clim_data, all_of(cols), round, 2)

# hist(clim_data$grad)
# clim_data_tutorial<-read.table('/home/ap/Downloads/ldndc-1.35.2.linux64/tutorial_clim_data.txt', header =  T)
# hist(clim_data_tutorial$grad)

  
  
  ## Select year, dayofyear, radiation (W/m^2), maxt, mint, precip (mm), mean wind (m/s)
  DNDC_basic <- clim_data[,c("year","dayofyear","rain_mm","tavg","maxt_C","mint_C",
                           "grad", "meanw_ms")] # "lrad", 
  colnames(DNDC_basic) <- c("year","dayofyear","prec","tavg","tmax","tmin","grad", "wind") # "lrad",
  
  # DNDC_basic<-DNDC_basic %>% select(-grad)
  
  
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
  DNDC_wth_file <- paste0(dndc_path,"climate_", clim_scenario_num, ".txt")
  
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
  
#   
#   # future period (1994-2021 repeated 3 times)
#   
#   ## Select year, dayofyear, radiation (W/m^2), maxt, mint, precip (mm), mean wind (m/s)
#   DNDC_basic_fut <- new_dat_2100[,c("year","dayofyear","rain_mm.x","tavg","maxt_C.x","mint_C.x",
#                                     "radn_Wm2.x","meanw_ms")]
#   colnames(DNDC_basic_fut) <- c("year","dayofyear","prec","tavg","tmax","tmin","grad","wind")
#   
#   ## don't recalculate the aggregated data; leave as-is (this note was added
#   ## assuming that only the mean data for header reflects the current 30-year
#   ## period, but LDNDC may want the average for the whole simulation, hence
#   ## the recalculation below)
#   ## DO recalculate aggregated variables
#   annual_tot_precipitation <- DNDC_basic_fut[,c("year","prec")] %>%
#     group_by(year) %>%
#     summarize(ann_sum=sum(prec))
#   annual_precipitation <- round(mean(annual_tot_precipitation$ann_sum,na.rm=T),1)
#   
#   temperature_average <- round(mean(DNDC_basic_fut$tavg),1)
#   
#   temperature_amplitude <- max(DNDC_basic_fut$tmax) - min(DNDC_basic_fut$tmin)
#   
#   wind_speed <- round(mean(DNDC_basic_fut$wind),1)
#   
#   time <- paste0(as.character(as.Date(first(DNDC_basic_fut$dayofyear)-1, 
#                                       origin=paste0(as.character(first(DNDC_basic_fut$year)),"-01-01"))),"/1")
#   
#   
#   
#   ## output header data
#   DNDC_wth_file_fut <- paste0(dndc_path,"climate_",clim_scenario_num,".txt")
#   
#   header_txt_fut <- c("%global",
#                       paste0("        time = \"",time,"\"\n"),
#                       "%climate",
#                       paste0("        id = ",site_id,"\n"),
#                       "%attributes",
#                       paste0("        elevation = \"",elevation_m,"\""),
#                       paste0("        latitude = \"",latitude,"\""),
#                       paste0("        longitude = \"",longitude,"\""),
#                       paste0("        wind speed = \"",wind_speed,"\""),
#                       paste0("        annual precipitation = \"",annual_precipitation,"\""),
#                       paste0("        temperature average = \"",temperature_average,"\""),
#                       paste0("        temperature amplitude = \"",temperature_amplitude,"\""),
#                       "\n",
#                       "%data",
#                       "*\t *\t prec\t tavg\t tmax\t tmin\t grad\t wind"
#   )
#   
#   writeLines(header_txt_fut,DNDC_wth_file_fut)
#   
#   ## add data
#   write.table(DNDC_basic_fut,sep="\t",
#               file=DNDC_wth_file_fut,
#               append=TRUE,
#               row.names = F,
#               col.names = F)
#   
# } else if(clim_scenario_num>1) {
#   
#   # Get experimental period and bind to future
#   DNDC_basic <- new_dat[,c("year","dayofyear","rain_mm.x","tavg","maxt_C.x","mint_C.x",
#                            "radn_Wm2.x","meanw_ms")]
#   colnames(DNDC_basic) <- c("year","dayofyear","prec","tavg","tmax","tmin","grad","wind")
#   
#   ## Future data
#   fut_dat <- read.csv(file=paste0(fut_weather_path,"fut_clim_scenario_",clim_scenario_num,'_reanal.csv'))
#   
#   ## Select year, dayofyear, radiation (W/m^2), maxt, mint, precip (mm), mean wind (m/s)
#   DNDC_basic_fut <- fut_dat[,c("year","dayofyear","rain_mm","tavg","maxt_C","mint_C",
#                                "radn_Wm2","meanw_ms")]
#   colnames(DNDC_basic_fut) <- c("year","dayofyear","prec","tavg","tmax","tmin","grad","wind")
#   
#   DNDC_basic_combined <- rbind(DNDC_basic,DNDC_basic_fut)
#   
#   
#   ## Calculate aggregated variables
#   annual_tot_precipitation <- DNDC_basic_fut[,c("year","prec")] %>%
#     group_by(year) %>%
#     summarize(ann_sum=sum(prec))
#   annual_precipitation <- round(mean(annual_tot_precipitation$ann_sum,na.rm=T),1)
#   
#   temperature_average <- round(mean(DNDC_basic_fut$tavg),1)
#   
#   temperature_amplitude <- max(DNDC_basic_fut$tmax) - min(DNDC_basic_fut$tmin)
#   
#   wind_speed <- round(mean(DNDC_basic_fut$wind),1)
#   
#   time <- paste0(as.character(as.Date(first(DNDC_basic_fut$dayofyear)-1, 
#                                       origin=paste0(as.character(first(DNDC_basic_fut$year)),"-01-01"))),"/1")
#   
#   
#   ## output header data
#   DNDC_wth_file <- paste0(dndc_path,"climate_",clim_scenario_num,".txt")
#   
#   header_txt <- c("%global",
#                   paste0("        time = \"",time,"\"\n"),
#                   "%climate",
#                   paste0("        id = ",site_id,"\n"),
#                   "%attributes",
#                   paste0("        elevation = \"",elevation_m,"\""),
#                   paste0("        latitude = \"",latitude,"\""),
#                   paste0("        longitude = \"",longitude,"\""),
#                   paste0("        wind speed = \"",wind_speed,"\""),
#                   paste0("        annual precipitation = \"",annual_precipitation,"\""),
#                   paste0("        temperature average = \"",temperature_average,"\""),
#                   paste0("        temperature amplitude = \"",temperature_amplitude,"\""),
#                   "\n",
#                   "%data",
#                   "*\t *\t prec\t tavg\t tmax\t tmin\t grad\t wind"
#   )
#   
#   writeLines(header_txt,DNDC_wth_file)
#   
#   ## add data
#   write.table(DNDC_basic_fut,sep="\t",
#               file=DNDC_wth_file,
#               append=TRUE,
#               row.names = F,
#               col.names = F)
#   
# }# if clim_scenario_num = 1 else >1

# if(clim_scenario_num==1) {
# rm(list=c("DNDC_basic","annual_tot_precipitation","annual_precipitation","temperature_average",
#           "temperature_amplitude","wind_speed","time","DNDC_wth_file","header_txt",
#           "DNDC_basic_fut"))
# } else if(clim_scenario_num>1) {
#   rm("fut_dat")
# }
