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
  
  library(tidyr)
  
  print("Starting 1_Create_weather_input_files-Daycent4.R")
  # 
  # load county data from 1950-2021 with nclim, then 2022-2050 with cmip6
  DAYCENT_basic_eq <- filter(weather, year %in% 1950:2021)%>% # ORIGINAL
    mutate_if(is.double, round, 2)
                               # c("day","month","year","dayofyear",
                               #   "TMAX","TMIN","prec_cm")]


    write.table(DAYCENT_basic_eq, file=file.path(daycent_path2, "basic_eq.wth"),
                row.names=F, quote=F, col.names=F, sep=' ')

    print("**********DAYCENT DailyWeather equilibrium**********")
    print(paste0('min max temp is: ', min(DAYCENT_basic_eq$tmax), " and ", max(DAYCENT_basic_eq$tmax)))
    print(paste0('min max precip is: ', min(DAYCENT_basic_eq$precip), " and ", max(DAYCENT_basic_eq$precip)))
    print(paste0("min and max year are ", min(DAYCENT_basic_eq$year), ", ", max(DAYCENT_basic_eq$year)))
    print(paste0("number of rows are: ", nrow(DAYCENT_basic_eq)))
    
    DAYCENT_basic <- filter(weather, year %in% (1950:2050))%>%
      mutate_if(is.double, round, 2)

    # write data file with no headers, tab-delimited, for experimental period
    write.table(DAYCENT_basic, file=file.path(daycent_path2,"basic_1.wth"),
                row.names=F, quote=F, col.names=F, sep=' ')
    
    print("**********DAYCENT DailyWeather**********")
    print(paste0('min max temp is: ', min(DAYCENT_basic$tmax), " and ", max(DAYCENT_basic$tmax)))
    print(paste0('min max precip is: ', min(DAYCENT_basic$precip), " and ", max(DAYCENT_basic$precip)))
    print(paste0("min max years are ", min(DAYCENT_basic$year), ", ", max(DAYCENT_basic$year)))
    print(paste0("number of rows are: ", nrow(DAYCENT_basic)))

}) # end suppressMessages