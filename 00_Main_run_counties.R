#!/usr/bin/env Rscript

#######################################
# File: "00_Main.R" current: 00_Main_run_counties_in_parallel.R
# Authors: "Ellen Maas (2022-2023); Adam Dixon (2023)"
# Date: "Nov 11, 2022"
# Description: This is the master process that drives the looping
# through all the scenarios and which will be what runs on each 
# cluster for the gridded study."
#
#######################################
# Calls:
# p_Create_future_weather_files.R
# p_Future_weather_reanalysis.R
# 1_Create_weather_input_files.R
# 0_Controller.R
# 10_Model_Ensemble_results-combined_scenarios2.R
#######################################
# Audit Log
# 11/11/2022: Created script.
# 11/17/2022: Added ensemble results script to end.
# 12/21/2022: Added weather script in climate loop.
# 2/12/2023: Added 
# 11/12/2023: Updating for county analysis (AD)
#######################################




main_run_function<-function(county_geoid){
  library(pracma)
  library(dplyr)
  library(tictoc)
  
  tic() #timer
  
  if (Sys.info()['sysname'] == "Darwin"){
    home_folder<-file.path('/Users/adamdixon/Documents/GitHub/national_gwp')
  } 
  
  if (Sys.info()['sysname'] == "Linux"){ 
    home_folder<-'/glade/u/home/apdixon/Documents/national_gwp'
    print('using ncar path')}
  
  master_path <- home_folder
  setwd(master_path)
  
  
  # rm(list=ls())

  #apsimx_options(exe.path="/bin/lib64/R/library/apsimx/R/")
  
  # county_geoid<-13193 #Macon County, Georgia
  # county_geoid<-13119 # Franklin County, Georgia
  

  
  county_data<-read.csv(file.path(home_folder, 'Data', 'County', 'county_centroids_elevation.csv'))%>%
    filter(GEOID==county_geoid)

  
  site_id <- county_data$GEOID
  site_name <- paste0(gsub(" ", "_", county_data$NAMELSAD),"_", gsub(" ", "_", county_data$State_Name))
  latitude = county_data$County_center_Lat
  longitude = county_data$County_center_Long
  elevation_m = county_data$Elevation_m
  
  # ---These are in 0_Controller2----
  # experiment_start_year <- 1989
  # experiment_end_year <- 2021
  # experiment_year_range <- experiment_start_year:experiment_end_year
  # experiment_start_date <- "1989-01-01"
  # experiment_end_date <- "2021-12-31"
  # end_exp_period_year <- 2021
  # end_fut_period_year <- 2050
  # max_fut_period_year <- 2100
  # calib_mgmt_grps <- c(1,2,3)
  # calib_mgmt_nums <- c(1,2,3)
  # ----in Controller2----
  #
  
  data_path<-'Data'
  
  obs_path <- paste0("Data/",site_name,"/Calibration/")
  obs_mgmt_path <- paste0("Data/",site_name,"/Management/")
  hist_wth_filename <- "NOAA-based Daily Kalamazoo 1900-2020.csv"
  hist_wth_mon_filename <- "Monthly Kalamazoo 1900-2020 with OPE.csv"
  curr_local_wth_filename <- "12-lter+weather+station+daily+weather+all+variates+1657202230.csv"
  wth_path <- paste0("Data/",site_name,"/Weather/")
  nasapower_output_filename <- paste0(site_name,"_np.csv")
  mgmt_path=paste0("Data/",site_name,"/Management/")
  adjusted_ops_filename="clean_ops_ext_adj.csv"
  fut_weather_path <- paste0("Data/CMIP6/",site_name,"/")
  apsim_path <- paste0("APSIM/",site_name,"/")
  daycent_path <- paste0("Daycent/",site_name,"/")
  if(Sys.info()['sysname']=='Linux') {
    dndc_path <- paste0("LDNDC/ldndc-1.35.2.linux64/projects/",site_name,"/")
  } else {
    dndc_path <- paste0("LDNDC/ldndc-1.35.2.win64/projects/",site_name,"/")
  }
  rothc_path <- paste0("RothC/",site_name,"/")
  mill_path <- paste0("Millennial/R/simulation/",site_name,"/")
  
  #**********************************************************************
  
  # # create future climate files (scenarios 2-5)
  # ## ******** comment this section out after first run 
  # 
  # print("**Create future CMIP6 climate files**")
  # source("p_Create_future_weather_files.R")
  # for (x in 2:5) {
  #   print("************************************")
  #   print("####### New climate scenario #######")
  #   print("************************************")
  #   print(paste0("climate scenario: ",x))
  #   clim_scenario_num <- x
  #     p_Create_future_weather_files(clim_scenario_num,latitude,longitude,
  #                                   experiment_end_year)
  # }
  # source("p_Future_weather_reanalysis.R")
  
  #**********************************************************************

  # Loop through the scenarios; set which climate and management
  # scenario numbers to use for this run:
  # clim_nums <- c(1:5)
  # mgmt_grps <- c(1:6) #calib_mgmt_grps #
# 
#   mgmt_scenario_grp <- 1 # scenario group number
#   max_scenario_options <- 1
  clim_scenario_num <- 1
  mgmt_scenario_num <- 1

  # mgmt_scenario_opt <- if(max_scenario_options==1) "" else z
  # mgmt_scenario_num <- as.numeric(paste0(mgmt_scenario_grp,mgmt_scenario_opt))
  scenario_name <- paste0(clim_scenario_num,"_",mgmt_scenario_num)
  
  # RUN PROGRAM
  source("0_Controller2.R")

  # for (x in clim_nums) { # climate scenarios # ADD BACK IN AD
  #   print("************************************")
  #   print("####### New climate scenario #######")
  #   print(paste0('####### ', site_name, " #######"))
  #   print("************************************")
  # 
  #   clim_scenario_num <- x
  #   # source("1_Create_weather_input_files.R")
  #   for (y in mgmt_grps) { # management scenario groups
  #     mgmt_scenario_grp <- y # scenario group number
  #     max_scenario_options <- if_else(y==4, 4, # option numbers for those with incremental adjustments
  #                                     if_else(y==5, 3,
  #                                             if_else(y==6, 5, 1)))
  # 
  #     for (z in 1:max_scenario_options) {
  #       print("************************************")
  #       print(paste0("climate scenario: ",x))
  #       print(paste0("mgmt scenario: ",y))
  #       print(paste0("mgmt option: ",z))
  #       mgmt_scenario_opt <- if(max_scenario_options==1) "" else z
  #       mgmt_scenario_num <- as.numeric(paste0(mgmt_scenario_grp,mgmt_scenario_opt))
  #       scenario_name <- paste0(clim_scenario_num,"_",mgmt_scenario_num)
  #       source("0_Controller2.R")
  #       print("************************************")
  #       print("Completed:")
  #       print(paste0("climate scenario: ",x))
  #       print(paste0("mgmt scenario: ",y))
  #       print(paste0("mgmt option: ",z))
  #       print("************************************")
  #       #p_Controller2()
  #     }
  # 
  #   } # end loop through management scenario groups
  # } # end loop through climate scenarios

  print(toc())

}
  




# start timer
# tic()

# county_geoid<-13193 # Macon County, Georgia
# county_geoid<-13119 # Franklin County, Georgia

# county_data<-read.csv('/glade/u/home/apdixon/Documents/national_gwp/Data/County/county_centroids_elevation.csv')

main_run_function(13193)

# main_run_function(13119)


# source(paste0("10_Model_Ensemble_results-combined_scenarios_",site_name,".R"))
# source(paste0("10_Model_Ensemble_results-combined_scenarios_and_sites_compbaseline-two_climate_scenarios4.R"))

# end timer
run_time <- round(toc(echo=TRUE)/60,1)
print(paste0("Run time is ",run_time," minutes, ",run_time/60," hours."))


