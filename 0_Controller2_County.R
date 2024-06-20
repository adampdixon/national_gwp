#######################################
# File: "0_Controller"
# Author: "Ellen Maas"
# Date: "Sept 23, 2022"
# Description: This script is the control file for the project. 
# It generates all the data input files for all models for a 
# given site. It includes weather, soil, and management. It 
# creates the inputs for APSIM, Daycent, and LDNDC, then runs 
# these three models. Then the process is repeated for RothC 
# and Millennial, which use Daycent output for their input."
#
#######################################
# Audit Log
# 10/23/2022: Converted from .Rmd to .R. Added reporting scripts.
# 11/4/2022: Converted results analysis files to a function
# 11/11/2022: Commented out leading variables due to the creation
# of 00_Main.R, which drives the master process looping through
# all the scenarios.
# 3/9/2023: Reshuffled some variables from 0_Observations&Const
# to here and 00_Main to allow for weather processing earlier in
# the program flow.
## 7/6/2023: Converted from script to function so that all objects
## created during each loop within it are removed between loops.
#
#######################################

#p_Controller2 <- function(data_mtx,model_name,scenario_name) {
  
print("Starting 0_Controller2_County.R")


# rm(list=ls())
# master_path <- "/home/ap/Documents/GitHub/national_gwp"
# setwd(master_path)
#
# site_id <- 1
# site_name <- "KBS"
# latitude = 42.410
# longitude = -85.372
# elevation_m = 288
# experiment_start_year <- 1989
# experiment_end_year <- 2021
# end_exp_period_year <- 2021
# experiment_year_range <- experiment_start_year:end_exp_period_year
# end_fut_period_year <- 2050
# max_fut_period_year <- 2051
# calib_mgmt_grps <- c(1,2,3)
# calib_mgmt_nums <- c(1,2,3)
# obs_path <- paste0("Data/County/Calibration/")
# hist_wth_filename <- "NOAA-based Daily Kalamazoo 1900-2020.csv"
# hist_wth_mon_filename <- "Monthly Kalamazoo 1900-2020 with OPE.csv"
# curr_local_wth_filename <- "12-lter+weather+station+daily+weather+all+variates+1657202230.csv"
# nasapower_output_filename <- paste0(site_name,"_np.csv")
# #
site_id <- 0
# site_name <- "LRF"
# latitude = 33.684
# longitude = -101.768
# elevation_m = 990
# experiment_start_year <- 2003
# experiment_end_year <- 2010
# experiment_start_date <- "2003-01-01"
# experiment_end_date <- "2010-12-31"
# end_exp_period_year <- 2021
# experiment_year_range <- experiment_start_year:end_exp_period_year
# end_fut_period_year <- 2050
# end_fut_period_date <- "2050-12-31"
# max_fut_period_year <- 2050 # AD changed from 2100
# calib_mgmt_grps <- c(3,5,7,8)
# calib_mgmt_nums <- c(3,53,56,7,8)
# obs_path <- paste0("Data/",site_name,"/")
# obs_filename <- "LibertyResearchFarm_adj.xlsx"
# curr_wth_tab <- "WeatherDaily"
# hist_raw_wth_filename <- "CDO_Lubbock_area.csv"
# hist_wth_filename <- "NOAA-based Daily Lubbock 1940-2021.csv"
# hist_wth_mon_filename <- "NOAA-based Monthly Lubbock 1940-2021 with OPE.csv"

#
# clim_scenario_num <- 1
# mgmt_scenario_grp <- 1 # scenario group number
# mgmt_scenario_opt <- "" # scenario detail number; put "" if none
# mgmt_scenario_num <- as.numeric(paste0(mgmt_scenario_grp,mgmt_scenario_opt))
# scenario_name <- paste0(clim_scenario_num,"_",mgmt_scenario_num)

# Scenario-dependent scripts and functions

#*************************************************************
#*************************************************************
# Setup observational data variables and global constants------------------
#*************************************************************
#*************************************************************

## These are used in multiple functions.
source(paste0("0_Observations_and_constants_County.R"), local = TRUE)


#*************************************************************
#*************************************************************
# Setup models ------------------------------------------------------------
#*************************************************************
#*************************************************************


# Weather data

# if(identical(run_Daycent, TRUE)) {
  source('1_create_county_climate_wth_file_County.R', local = TRUE)
  source('1_Create_weather_input_files-Daycent_County_v2.R', local = TRUE)
  
  print("*****writing soils data")
  
  # Soil data
  source("2_Create_soil_data-setup2_County.R", local = TRUE) # some soil vars needed for Daycent and LDNDC
  # Site data
  source("2_1_Create_site_file-Daycent_County.R", local = TRUE)
  
  source("2_Create_soil_site_data-Daycent_County.R", local = TRUE)
  
# }

if(identical(run_LDNDC,TRUE)) {
  
  source("1_Create_weather_input_files-LDNDC_County.R", local = TRUE)
  source("2_Create_soil_data-LDNDC_County.R", local = TRUE)
}


# RothC only uses clay content, which is included in the weather input file.
# source(paste0("2_Create_soil_data-Millennial_",site_name,".R"))
# }


#*************************************************************


  
## Management input files for RothC, Millennial are created after Daycent runs
# }


#*************************************************************

# if(identical(compile_input_data_4visualizations, TRUE)){
#   
# }


#*************************************************************
#*************************************************************
# Run models --------------------------------------------------------------
#*************************************************************
#*************************************************************

## APSIM Classic is currently run manually.

## To run APSIM Classic scenarios, copy and 
## paste the management data file into the Operations Schedule
## model window, save, and run.

# # APSIM
#   source(paste0(apsim_path,"run_APSIM.R"))

#*************************************************************


# Note: Soybeans have no yield results prior to 1900 becuase they weren't grown in US and the model is set up for corn during that time


# Daycent
# if(identical(run_ag_models,TRUE)) {
  # link to schedule files
source(paste0("3_Create_management_input_files-Daycent_County_Scenarios.R"), local = TRUE)

for (c in crops_){
  crop_amount<-county_data[,eval(paste0(c, "_ha"))] # get crop amount in county
  if (crop_amount<1){
    print(paste0("*************** skipping because less than 1 ha of ", c, " in county **********************************"))
    file.create(file.path(results_path, paste0("note - there is no ", c, " in county ", county_geoid,".txt")))
    
    next # if crop amount is less than 1 ha in county, then skip
  } else {
    for (m in mgmt_scenario_nums){
      # if (c == "Rotation" & m > 1){  # Rotation only has 1 scenario, so skip to next
      #   next
      # }else{
      crop<-c
      mgmt_scenario_num<-m
      scenario_name <- paste0(clim_scenario_num,"_", m)
      scenario_name2<-paste0(scenario_name, "_", crop)
      
      model_path<-file.path(results_path, paste0("Annual_results_compilation_", scenario_name2,"_Daycent.csv"))
      
      if(identical(run_Daycent, TRUE)) {
        
        # check if results file already exists and only want results
        if(file.exists(model_path) & identical(results_only, FALSE)){ 
          # & nrow(fread(model_path))> 200# # check if all rows have been reported; note this didn't work well
          print(paste0("*************Daycent results already exist for: ", scenario_name2, " ... skipping...****************"))
          next
        } else{
          print("...writing schedule file...")
          
          # Pair scenarios with schedule file, combine as vector
          crop_schedule<-c(get(paste0(tolower(crop), '_1')), # opening set of schedule file blocks
                           get(paste0(tolower(crop), '_scenario_', m)), # scenario-specific schedule file blocks
                           
                           output_sch<-file.path(daycent_path2 , paste0("sched_base_", scenario_name, "_", crop, ".sch")))
          
          # write to sch file
          writeLines(crop_schedule, output_sch) # schedule file name, e.g. schedule_file_maize

        
        } # end if file exists
      } # end if run_Daycent
        
        
      # Run Daycent
      if(identical(run_Daycent,TRUE)) {
        print(paste0("*************running Daycent for: ", scenario_name2, "****************"))
        source(paste0("Daycent/Daycent_run_controller.R"), local = TRUE)
        source(paste0("9_Results_Daycent-setup_County.R"), local=TRUE) #TODO AD set this up?
      }
      

      # LDNDC
      if(identical(run_LDNDC,TRUE)) {
        print(paste0("*************running LDNDC for: ", scenario_name2, "****************"))
        print(paste0("*************Create_management_input_files-LDNDC_County: ", scenario_name2, "****************"))
        source(paste0("3_Create_management_input_files-LDNDC_County.R"), local = TRUE)
        
        print(paste0("*************4 Create_additional_files-LDNDC_County: ", scenario_name2, "****************"))
        source(paste0("4_Create_additional_files-LDNDC_County.R"), local = TRUE)
        
        source(file.path(ldndc_run_path, "run_LDNDC.R"))
        
        source('9_Results_LDNDC-setup_County.R', local = TRUE)
        
        } # end of run_LDNDC
      #
        
        
     # Daycent has already been run, update results only
      if(identical(results_only, TRUE)){
        print(paste0("*************generation results table for: ", scenario_name2, "****************"))
        # Table generation script
        source('9_Results_Daycent-setup_County.R', local = TRUE)
      }
      
      
      if(identical(run_Millennial,TRUE)) {
        print(paste0("*************Running Millenial for: ", scenario_name2, "****************"))
        # # Millennial
        source(paste0("3_Create_management_input_files-Millennial_County.R"), local = TRUE)
        source(paste0(mill_path,"run_Millennial.R"))
      }
      # # Millennial
      # source(paste0("3_Create_management_input_files-Millennial_",site_name,".R"))
      # source(paste0(mill_path,"run_Millennial.R"))
      
      
      
    } # end of mgmt_scenario_nums loop 

  } # end of else statement if crop amount is less than 1 ha in county
  } # end of crops loop
# }

#*************************************************************

# LDNDC
# if(mgmt_scenario_grp!=6) {
  # source(paste0("LDNDC/ldndc-1.35.2.linux64/projects/run_LDNDC.R"))
# }


#*************************************************************
#*************************************************************
# Graph and analyze APSIM, Daycent, and LDNDC -----------------------------
#*************************************************************
#*************************************************************

# # APSIM
 # source(paste0("9_Results_APSIM-setup_",site_name,".R"))
 # model_name <- "APSIM"
#
# before calibration, need to first run (above):
# 3_Create_management_input_files-setup_",site_name,".R"
# 3_Create_management_input_files-APSIM_",site_name,".R"
# if(clim_scenario_num==1 & mgmt_scenario_num %in% calib_mgmt_nums) {
# source(paste0("9_Results_APSIM-calibration2_",site_name,".R"))
# }
 # source(paste0("9_Results_APSIM-future_",site_name,".R"))
# source("p_Results_analysis.R")

#*************************************************************

# Daycent
# if(mgmt_scenario_grp!=6) {
# source(paste0("9_Results_Daycent-setup_County.R"), local=TRUE) #TODO AD set this up?
# model_name <- "Daycent"
#   if(clim_scenario_num==1 & mgmt_scenario_num %in% calib_mgmt_nums) {
#     source(paste0("9_Results_Daycent-calibration_County.R"))
#   }
# source(paste0("9_Results_Daycent-future_",site_name,".R"))
# source("p_Results_analysis.R")
# }

#*************************************************************

# # LDNDC
# if(mgmt_scenario_grp!=6) {
# source(paste0("9_Results_LDNDC-setup_",site_name,".R"))
#   model_name <- "LDNDC"
#   if(clim_scenario_num==1 & mgmt_scenario_grp %in% calib_mgmt_grps) {
#     source(paste0("9_Results_LDNDC-calibration_",site_name,".R"))
#   }
#   # source(paste0("9_Results_LDNDC-future_",site_name,".R"))
#   # source("p_Results_analysis.R")
# }

#*************************************************************
#*************************************************************
# Set up and run Millennial and RothC, driving from Daycent ---------------
#*************************************************************
#*************************************************************


# if(mgmt_scenario_grp!=6) {
# # Management input files (RothC, Millennial)
# # need to first run 3_Create_management_input_files-setup_",site_name,".R
# # and 9_Results_Daycent-setup_",site_name,".R
# source(paste0("3_Create_management_input_files-setupRM_",site_name,".R"))
# 
# # Millennial
# source(paste0("3_Create_management_input_files-Millennial_",site_name,".R"))
# source(paste0(mill_path,"run_Millennial.R"))
#
# RothC
# source(paste0("3_Create_management_input_files-RothC_",site_name,".R"))
# source(paste0("4_Create_additional_files-RothC_",site_name,".R"))
## OPTIONAL: alternative management output for spreadsheet
## source(paste0("5_Alternative_management_input_files-RothC_",site_name,".R"))
# 
# RothC is currently run manually: after management and scenario
# files are created, open RothC and run the model.
# 
#  }


#*************************************************************
#*************************************************************
# Graph and analyze RothC and Millennial ----------------------------------
#*************************************************************
#*************************************************************

# # Millennial
# if(mgmt_scenario_grp!=6) {
# source(paste0("9_Results_Millennial-setup_",site_name,".R")) # TODO set this up?
# #
# model_name <- "Millennial"
# if(clim_scenario_num==1 & mgmt_scenario_num %in% calib_mgmt_nums) {
#   source(paste0("9_Results_Millennial-calibration_",site_name,".R"))
# }
# source(paste0("9_Results_Millennial-future_",site_name,".R"))
#   source("p_Results_analysis.R")
# }


#*************************************************************

# # RothC
# if(mgmt_scenario_grp!=6) {
# source(paste0("9_Results_RothC-setup_",site_name,".R"))
# #
# model_name <- "RothC"
# if(clim_scenario_num==1 & mgmt_scenario_num %in% calib_mgmt_nums) {
# source(paste0("9_Results_RothC-calibration_",site_name,".R"))
# }
# source("9_Results_RothC-future.R")
# source("p_Results_analysis.R")
# }


#*************************************************************



#*************************************************************
#*************************************************************
# Graph ensemble compilations ---------------------------------------------
#*************************************************************
#*************************************************************

# source(paste0("10_Model_Ensemble_results-by_scenario_",site_name,".R"))


#}