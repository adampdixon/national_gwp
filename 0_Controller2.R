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
#
#######################################

print("Starting 0_Controller2.R")

# rm(list=ls())
# master_path <- "~/Modeling"
# setwd(master_path)
# #
# # site_id <- 1
# # site_name <- "KBS"
# # latitude = 42.410
# # longitude = -85.372
# # elevation_m = 288
# # experiment_start_year <- 1989
# # experiment_end_year <- 2021
# # end_exp_period_year <- 2021
# # experiment_year_range <- experiment_start_year:end_exp_period_year
# # end_fut_period_year <- 2050
# # max_fut_period_year <- 2100
# # calib_mgmt_grps <- c(1,2,3)
# # calib_mgmt_nums <- c(1,2,3)
# # obs_path <- paste0("Data/",site_name,"/Calibration/")
# # hist_wth_filename <- "NOAA-based Daily Kalamazoo 1900-2020.csv"
# # hist_wth_mon_filename <- "Monthly Kalamazoo 1900-2020 with OPE.csv"
# # curr_local_wth_filename <- "12-lter+weather+station+daily+weather+all+variates+1657202230.csv"
# # nasapower_output_filename <- paste0(site_name,"_np.csv")
# #
# site_id <- 2
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
# max_fut_period_year <- 2100
# calib_mgmt_grps <- c(3,5,7,8)
# calib_mgmt_nums <- c(3,53,56,7,8)
# obs_path <- paste0("Data/",site_name,"/")
# obs_filename <- "LibertyResearchFarm_adj.xlsx"
# curr_wth_tab <- "WeatherDaily"
# hist_raw_wth_filename <- "CDO_Lubbock_area.csv"
# hist_wth_filename <- "NOAA-based Daily Lubbock 1940-2021.csv"
# hist_wth_mon_filename <- "NOAA-based Monthly Lubbock 1940-2021 with OPE.csv"
# curr_local_wth_filename <- "" # included in GRACEnet spreadsheet (obs_filename)
# #
# wth_path <- paste0("Data/",site_name,"/Weather/")
# apsim_path <- paste0("APSIM/",site_name,"/")
# daycent_path <- paste0("Daycent/",site_name,"/")
# if(Sys.info()['sysname']=='Linux') {
#   dndc_path <- paste0("LDNDC/ldndc-1.35.2.linux64/projects/",site_name,"/")
# } else {
#   dndc_path <- paste0("LDNDC/ldndc-1.35.2.win64/projects/",site_name,"/")
# }
# rothc_path <- paste0("RothC/",site_name,"/")
# mill_path <- paste0("Millennial/R/simulation/",site_name,"/")
# #
clim_scenario_num <- 1
mgmt_scenario_grp <- 8 # scenario group number
mgmt_scenario_opt <- "" # scenario detail number; put "" if none
mgmt_scenario_num <- as.numeric(paste0(mgmt_scenario_grp,mgmt_scenario_opt))
scenario_name <- paste0(clim_scenario_num,"_",mgmt_scenario_num)

# Scenario-dependent scripts and functions

## These are used in multiple functions.
source(paste0("0_Observations_and_constants_",site_name,".R"))


#*************************************************************
#*************************************************************
# Setup models ------------------------------------------------------------
#*************************************************************
#*************************************************************


# # Soil data
# 
# if(mgmt_scenario_grp!=6) {
#   ## Prerequisite: APSIM .apsimx file must already exist
#   ## Scenario 6 is setup manually in APSIM Classic
# source(paste0("2_Create_soil_data-setup2_",site_name,".R"))
#   #
#   # source("2_Create_soil_data-APSIM.R")
#   # source("2_Create_soil_data-Daycent.R")
# source("2_Create_soil_data-LDNDC.R")
#   # RothC only uses clay content, which is included in the weather input file.
# # source(paste0("2_Create_soil_data-Millennial_",site_name,".R"))
# }


#*************************************************************

# Management input files (APSIM, Daycent, LDNDC)
#
# source(paste0("3_Create_management_input_files-setup_",site_name,".R"))
# # 
# source(paste0("3_Create_management_input_files-APSIM_",site_name,".R"))
#
# if(mgmt_scenario_grp!=6) {
# source(paste0("3_Create_management_input_files-Daycent_",site_name,".R"))
# source(paste0("3_Create_management_input_files-LDNDC_",site_name,".R"))
# 
# # Management input files for RothC, Millennial are created after Daycent runs
# }


#*************************************************************

# # Other files
# if(mgmt_scenario_grp!=6) {
  # source(paste0("4_Create_additional_files-LDNDC_",site_name,".R"))
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

# Daycent
# if(mgmt_scenario_grp!=6) {
# source(paste0("Daycent/Daycent_run_controller.R"))
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

source(paste0("9_Results_APSIM-setup_",site_name,".R"))
# model_name <- "APSIM"
# 
# need to first run:
# 3_Create_management_input_files-setup_",site_name,".R"
# 3_Create_management_input_files-APSIM_",site_name,".R"
# if(clim_scenario_num==1 & mgmt_scenario_grp %in% calib_mgmt_grps) {
# source(paste0("9_Results_APSIM-calibration2_",site_name,".R"))
# }
# source(paste0("9_Results_APSIM-future_",site_name,".R"))
# source("p_Results_analysis.R")

#*************************************************************

# # Daycent
if(mgmt_scenario_grp!=6) {
source(paste0("9_Results_Daycent-setup_",site_name,".R"))
#  model_name <- "Daycent"
#  if(clim_scenario_num==1 & mgmt_scenario_grp %in% calib_mgmt_grps) {
# source(paste0("9_Results_Daycent-calibration_",site_name,".R"))
# }
# source(paste0("9_Results_Daycent-future_",site_name,".R"))
# source("p_Results_analysis.R")
}

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
# #
# # RothC
# source(paste0("3_Create_management_input_files-RothC_",site_name,".R"))
# source(paste0("4_Create_additional_files-RothC_",site_name,".R"))
# ## alternative management output for spreadsheet
# # source(paste0("5_Alternative_management_input_files-RothC_",site_name,".R"))
# #
# # RothC is currently run manually: after management and scenario
# # files are created, open RothC and run the model.
#  }


#*************************************************************
#*************************************************************
# Graph and analyze RothC and Millennial ----------------------------------
#*************************************************************
#*************************************************************

# Millennial
if(mgmt_scenario_grp!=6) {
source(paste0("9_Results_Millennial-setup_",site_name,".R"))
# #
# model_name <- "Millennial"
#   if(clim_scenario_num==1 & mgmt_scenario_grp %in% calib_mgmt_grps) {
# source(paste0("9_Results_Millennial-calibration_",site_name,".R"))
# }
#   source(paste0("9_Results_Millennial-future_",site_name,".R"))
  # source("p_Results_analysis.R")
}


#*************************************************************

# RothC
if(mgmt_scenario_grp!=6) {
source(paste0("9_Results_RothC-setup_",site_name,".R"))
# #
#   model_name <- "RothC"
#   if(clim_scenario_num==1 & mgmt_scenario_grp %in% calib_mgmt_grps) {
#   source(paste0("9_Results_RothC-calibration_",site_name,".R"))
#   }
#   source("9_Results_RothC-future.R")
  # source("p_Results_analysis.R")
}


#*************************************************************



#*************************************************************
#*************************************************************
# Graph ensemble compilations ---------------------------------------------
#*************************************************************
#*************************************************************

 source(paste0("10_Model_Ensemble_results-by_scenario_",site_name,".R"))

