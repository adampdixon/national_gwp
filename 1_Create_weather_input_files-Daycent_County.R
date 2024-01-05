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
  
  #working with these variables, need to get day, month, year, doy, tmax, tmin, prec_cm
  # c('GEOID', 'value', 'year', 'doy', 'variable','model')
  

    
  # DAYCENT_gwp_all <- Hist_site[Hist_site$year %in% 1950:1977,  # ORIGINAL
  #                                 c("day","month","year","dayofyear",
  #                                   "TMAX","TMIN","prec_cm")]  
  
  DAYCENT_basic_eq <- filter(weather, year %in% 1950:1977) # ORIGINAL
                               # c("day","month","year","dayofyear",
                               #   "TMAX","TMIN","prec_cm")]
    
    write.table(DAYCENT_basic_eq, file=file.path(master_path, "Daycent", site_name, "basic_eq.wth"),
                row.names=F, quote=F, col.names=F, sep=' ')
    
    print("**********DAYCENT_basic_eq**********")
    print(paste0('max temp is: ', max(DAYCENT_basic_eq$tmax)))
    print(paste0('min temp is: ', min(DAYCENT_basic_eq$tmax)))
    print(paste0("min and max year are ", min(DAYCENT_basic_eq$year), ", ", max(DAYCENT_basic_eq$year)))
    print(paste0("number of rows are: ", nrow(DAYCENT_basic_eq)))
    
    # experimental period (2003-2021, even though experiment ends 2010)

    # DAYCENT_basic <- new_dat[new_dat$year %in% (experiment_start_year:end_exp_period_year), # ORIGINAL
    #                          c("day","month","year","dayofyear","maxt_C.x",
    #                             "mint_C.x","rain_cm.x")]
    
    DAYCENT_basic <- filter(weather, year %in% (experiment_start_year:end_exp_period_year))
                             # c("day","month","year","dayofyear","maxt_C.x",
                             #   "mint_C.x","rain_cm.x")]

    # write data file with no headers, tab-delimited, for experimental period
    write.table(DAYCENT_basic, file=paste0(daycent_path,"basic_exp.wth"),
                row.names=F, quote=F, col.names=F, sep=' ')
    
    print("**********DAYCENT_basic**********")
    print(paste0('max temp is: ', max(DAYCENT_basic$tmax)))
    print(paste0('min temp is: ', min(DAYCENT_basic$tmax)))
    print(paste0("min and max year are ", min(DAYCENT_basic$year), ", ", max(DAYCENT_basic$year)))
    print(paste0("number of rows are: ", nrow(DAYCENT_basic)))

    # future period (1994-2021 repeated 3 times)

    # DAYCENT_basic_fut <- new_dat_fut[new_dat_fut$year>end_exp_period_year,
    #                                    c("day","month","year","dayofyear",
    #                                      "maxt_C.x","mint_C.x","rain_cm.x")]
    
    # DAYCENT_basic_fut <- filter(weather, year %in% end_exp_period_year:end_fut_period_year)
    DAYCENT_basic_fut <- weather[weather$year>end_exp_period_year]
    
    print("**********DAYCENT_basic_fut**********")
    print(paste0('max temp is: ', max(DAYCENT_basic_fut$tmax)))
    print(paste0('min temp is: ', min(DAYCENT_basic_fut$tmax)))
    print(paste0("min and max year are ", min(DAYCENT_basic_fut$year), ", ", max(DAYCENT_basic_fut$year)))
    print(paste0("number of rows are: ", nrow(DAYCENT_basic_fut)))
    
    
                                     # c("day","month","year","dayofyear",
                                     #   "maxt_C.x","mint_C.x","rain_cm.x")]

    # write data file with no headers, tab-delimited, for experimental period
    write.table(DAYCENT_basic_fut, file=paste0(daycent_path,"basic_",clim_scenario_num,".wth"),
                row.names=F, quote=F, col.names=F, sep=' ')
    
    # print("removing extraneous weather data files AD")
    # AD REMOVE FILES FROM BEFORE 
    # unlink(paste0(daycent_path,"basic_2.wth"))
    # unlink(paste0(daycent_path,"basic_3.wth"))
    # unlink(paste0(daycent_path,"basic_4.wth"))
    # unlink(paste0(daycent_path,"basic_5.wth"))

    # Removed because not needed in EVI when .wth filename set at top of site.100 file
    # ########## Create climate stats for site.100 ##########
    #
    # DAYCENT_monthly_means <- DAYCENT_basic %>%
    #   group_by(month) %>%
    #   summarize(mean_ppt = round(mean(rain_cm, na.rm=T),4),
    #             mean_minT = round(mean(mint_C, na.rm=T),4),
    #             mean_maxT = round(mean(maxt_C, na.rm=T),4)) %>%
    #   pivot_longer(-month, names_to = 'variable',values_to = 'val') %>%
    #   arrange(variable,month)
    #
    # # Mean precipitation by month
    # ppt_params <- c("'PRECIP(1)'","'PRECIP(2)'","'PRECIP(3)'","'PRECIP(4)'",
    #                 "'PRECIP(5)'","'PRECIP(6)'","'PRECIP(7)'","'PRECIP(8)'",
    #                 "'PRECIP(9)'","'PRECIP(10)'","'PRECIP(11)'","'PRECIP(12)'")
    #
    # # Std dev precipitation by month not needed - only Century
    # # Skewness for precipitation by month not needed - only Century
    #
    # # Mean minimum daily temp by month
    # minT_params <- c("'TMN2M(1)'","'TMN2M(2)'","'TMN2M(3)'","'TMN2M(4)'",
    #                  "'TMN2M(5)'","'TMN2M(6)'","'TMN2M(7)'","'TMN2M(8)'",
    #                  "'TMN2M(9)'","'TMN2M(10)'","'TMN2M(11)'","'TMN2M(12)'")
    #
    # # Mean maximum daily temp by month
    # maxT_params <- c("'TMX2M(1)'","'TMX2M(2)'","'TMX2M(3)'","'TMX2M(4)'",
    #                  "'TMX2M(5)'","'TMX2M(6)'","'TMX2M(7)'","'TMX2M(8)'",
    #                  "'TMX2M(9)'","'TMX2M(10)'","'TMX2M(11)'","'TMX2M(12)'")
    #
    # param <- c(maxT_params, minT_params, ppt_params)
    #
    # # combine and Write to site.100 file, lines 3-62
    #
    # climate_data <- cbind(DAYCENT_monthly_means[,c("variable","month","val")],param)
    # climate_stats <- cbind(climate_data,paste0(climate_data$val,"            ",climate_data$param))
    # colnames(climate_stats) <- c("variable","month","val","param","text")
    #
    # site_file <- readLines(paste0(daycent_path,"site.100"))
    # site_file[3:14] <- climate_stats[climate_stats$variable=="mean_ppt","text"]
    # site_file[39:50] <- climate_stats[climate_stats$variable=="mean_minT","text"]
    # site_file[51:62] <- climate_stats[climate_stats$variable=="mean_maxT","text"]
    # writeLines(site_file,paste0(daycent_path,"site.100"))

  # } else if(clim_scenario_num>1) {
  #
  #   ########## Create .wth files ##########
  #
  #   # CMIP6 future projection
  #   fut_dat <- read.csv(file=paste0(fut_weather_path,"fut_clim_scenario_",clim_scenario_num,'_reanal.csv'))
  #
  #   # Select day, month, year, dayofyear, maxt, mint, precip (cm)
  #
  #   DAYCENT_basic_esm <- fut_dat[,c("day","month","year","dayofyear","maxt_C",
  #                                   "mint_C","rain_cm")]
  #
  #   # write data file with no headers, tab-delimited, for experimental period
  #   write.table(DAYCENT_basic_esm, file=paste0(daycent_path,"basic_",clim_scenario_num,".wth"),
  #               row.names=F, quote=F, col.names=F, sep=' ')


    # Removed because not needed in EVI when .wth filename set at top of site.100 file
    # ########## Create climate stats for site.100 ##########
    #
    # DAYCENT_monthly_means <- DAYCENT_basic_esm %>%
    #   group_by(month) %>%
    #   summarize(mean_ppt = round(mean(rain_cm, na.rm=T),4),
    #             mean_minT = round(mean(mint_C, na.rm=T),4),
    #             mean_maxT = round(mean(maxt_C, na.rm=T),4)) %>%
    #   pivot_longer(-month, names_to = 'variable',values_to = 'val') %>%
    #   arrange(variable,month)
    #
    # # Mean precipitation by month
    # ppt_params <- c("'PRECIP(1)'","'PRECIP(2)'","'PRECIP(3)'","'PRECIP(4)'",
    #                 "'PRECIP(5)'","'PRECIP(6)'","'PRECIP(7)'","'PRECIP(8)'",
    #                 "'PRECIP(9)'","'PRECIP(10)'","'PRECIP(11)'","'PRECIP(12)'")
    #
    # # Std dev precipitation by month not needed - only Century
    # # Skewness for precipitation by month not needed - only Century
    #
    # # Mean minimum daily temp by month
    # minT_params <- c("'TMN2M(1)'","'TMN2M(2)'","'TMN2M(3)'","'TMN2M(4)'",
    #                  "'TMN2M(5)'","'TMN2M(6)'","'TMN2M(7)'","'TMN2M(8)'",
    #                  "'TMN2M(9)'","'TMN2M(10)'","'TMN2M(11)'","'TMN2M(12)'")
    #
    # # Mean maximum daily temp by month
    # maxT_params <- c("'TMX2M(1)'","'TMX2M(2)'","'TMX2M(3)'","'TMX2M(4)'",
    #                  "'TMX2M(5)'","'TMX2M(6)'","'TMX2M(7)'","'TMX2M(8)'",
    #                  "'TMX2M(9)'","'TMX2M(10)'","'TMX2M(11)'","'TMX2M(12)'")
    #
    # param <- c(maxT_params, minT_params, ppt_params)
    #
    # # combine and Write to site.100 file, lines 3-62
    #
    # climate_data <- cbind(DAYCENT_monthly_means[,c("variable","month","val")],param)
    # climate_stats <- cbind(climate_data,paste0(climate_data$val,"            ",climate_data$param))
    # colnames(climate_stats) <- c("variable","month","val","param","text")
    #
    # site_file <- readLines(paste0(daycent_path,"site.100"))
    # site_file[3:14] <- climate_stats[climate_stats$variable=="mean_ppt","text"]
    # site_file[39:50] <- climate_stats[climate_stats$variable=="mean_minT","text"]
    # site_file[51:62] <- climate_stats[climate_stats$variable=="mean_maxT","text"]
    # writeLines(site_file,paste0(daycent_path,"site.100"))

  # } # if clim_scenario_num == 1

#   if(clim_scenario_num==1) {
#   rm(list=c("DAYCENT_basic_eq","DAYCENT_basic","DAYCENT_basic_fut"))
# } else if(clim_scenario_num>1) {
#   rm(list=c("fut_dat","DAYCENT_basic_esm"))
# }

}) # end suppressMessages