#######################################
# Script: 1_Create_weather_input_files-Daycent_AD_.R
# Author: Adam Dixon
# Date: December 16, 2023
# Output: .wth file
# Description: Generates weather input files specifically in the format that
# Daycent needs
# https://www2.nrel.colostate.edu/projects/irc/public/Documents/Software/Century5/Reference/
#######################################
# Called by:
# 1_Create_weather_input_files.R
#######################################
# Audit Log:
# Dec 2023 - Adpated from Maas scripts and colo state reference
#######################################


# Create in the form of:
# # ----------------------------------------------------------------------
# # Data source: DAYMET 1 km at lat/long = 39.5/-104.5
# # Date created (year/month/day): 2007/05/08
# # ----------------------------------------------------------------------
# # day   month   year    jday    tmax    tmin    precip  srad    rh      ws
# 1       1       1980    1       5.5     -7.5    0       224     42.7    -99.99
# 2       1       1980    2       1.5     -7      0.3     121.6   69.6    -99.99
# 3       1       1980    3       1.5     -15     0       259.2   40.9    -99.99
# ...
# 29      12      2003    363     2       -14     0       224     19.5    -99.99
# 30      12      2003    364     8       -9.5    0       236.8   26.0    -99.99
# 31      12      2003    365     11.5    -6      0       236.8   20.4    -99.99

# County

# 1. Get the weather data for the county for both historic and future

# county_geoid<-46087

library(data.table)

climate_data<-list.files(climate_data_path, full.names = TRUE, pattern = ".csv")

# libraries needed: data.table, lubridate, dplyr

# Create historic data using mutate, left_join, select, and lubridate functions; use fread to read in the data
# Then place in the format that Daycent needs

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

# Future 2022 - 2050
future_data<-mutate(fread(climate_data[grep(paste0("tmax_", county_geoid, "_cmip6.csv"), climate_data)]),
                                   tmax = value)%>%
  left_join(
    mutate(fread(climate_data[grep(paste0("tmin_", county_geoid, "_cmip6.csv"), climate_data)]),
           tmin = value), 
    by=c('year', 'doy'))%>%
  left_join(
    mutate(fread(climate_data[grep(paste0("prcp_", county_geoid, "_cmip6.csv"), climate_data)]),
           precip = value),
    by=c('year', 'doy'))%>%
  select(year, doy, tmax, tmin, precip)%>%
  mutate(date_object = lubridate::ymd(paste(year, "-01-01")) + days(doy - 1))%>% #chatgpt derived
  mutate(month = lubridate::month(date_object),
         day = lubridate::day(date_object),
         jday = doy)%>%
  select(day, month, year, jday, tmax, tmin, precip)

# rbind the files together
weather<-rbind(historic_data, future_data)

# Create wth file and variable
# commenting this out to just pass on weather file to separate weather file into time periods that jive with previous model version
# write.table(weather, file=file.path(master_path, 'Daycent', site_name ,"DailyWeather.wth"),
#             row.names=F, quote=F, col.names=F, sep=' ')

