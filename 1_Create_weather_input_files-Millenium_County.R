#######################################
# Script: 1_Create_weather_input_files-Daycent4.R
# Author: Adam Dixon
# Date: December 4, 2023
# Output: .wth files for the given scenario.
# Description: Generates weather input files specifically in the format that
# Daycent needs, for the given scenario.
#######################################
# Called by:
# 1_Create_weather_input_files.R
#######################################
# Audit Log:
# 2022: Created script.
# 2/19/2023: Minor edits to reformat to be called by 1_Create_weather_input_files
# and site-specific details.
#######################################

suppressMessages({
  
 
  
  print("Starting 1_Create_weather_input_files-Millennial.R")
  
  # weather
  # 
  # globalaverage<-weather%>%
  #   group_by(year)%>%
  #   summarise(tmax=mean(tmax),tmin=mean(tmin),precip=mean(precip))
  # # 
  # 
  # 
  # print("**********DAYCENT DailyWeather equilibrisum**********")
  # print(paste0('min max temp is: ', min(DAYCENT_basic_eq$tmax), " and ", max(DAYCENT_basic_eq$tmax)))
  # print(paste0('min max precip is: ', min(DAYCENT_basic_eq$precip), " and ", max(DAYCENT_basic_eq$precip)))
  # print(paste0("min and max year are ", min(DAYCENT_basic_eq$year), ", ", max(DAYCENT_basic_eq$year)))
  # print(paste0("number of rows are: ", nrow(DAYCENT_basic_eq)))
  
 
}) # end suppressMessages