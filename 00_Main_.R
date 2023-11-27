
#######################################
# File: "00_Main.R"
# Author: "Ellen Maas"
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
#######################################
library(pracma)
library(R.utils)

# start timer
tic()

print("starting 00_Main_.R")

# rm(list=ls())
# master_path<-'/home/ap/Documents/GitHub/national_gwp'
# setwd(master_path)
#apsimx_options(exe.path="/bin/lib64/R/library/apsimx/R/")


# site_id <- 1
# site_name <- "KBS"
# latitude = 42.410
# longitude = -85.372
# elevation_m = 288
experiment_start_year <- 1989
experiment_end_year <- 2021
experiment_year_range <- experiment_start_year:experiment_end_year
experiment_start_date <- "1989-01-01"
experiment_end_date <- "2021-12-31"
end_exp_period_year <- 2021
end_fut_period_year <- 2050
max_fut_period_year <- 2100
calib_mgmt_grps <- c(1,2,3)
calib_mgmt_nums <- c(1,2,3)

#######################################
print("Copying over KBS 'Data' files")
if(!dir.exists(file.path(master_path, 'Data', site_name))) copyDirectory(from = file.path(master_path, '/Data/KBS'),
                                                                     to = file.path(master_path, 'Data', site_name),
                                                                     recursive = T)
#######################################
print("Copying over KBS 'Daycent model' files -- delete when begin running in full")
if(!dir.exists(file.path(master_path, 'Daycent', site_name))) copyDirectory(from = file.path(master_path, '/Daycent/KBS'),
                                                                         to = file.path(master_path, 'Daycent', site_name),
                                                                         recursive = T)
#######################################


#
obs_path <- paste0("Data/",site_name,"/Calibration/")              #14
obs_mgmt_path <- paste0("Data/",site_name,"/Management/")



hist_wth_filename <- "NOAA-based Daily Kalamazoo 1900-2020.csv"
hist_wth_mon_filename <- "Monthly Kalamazoo 1900-2020 with OPE.csv"
curr_local_wth_filename <- "12-lter+weather+station+daily+weather+all+variates+1657202230.csv"
wth_path <- paste0("Data/",site_name,"/Weather/")
nasapower_output_filename <- paste0(site_name,"_np.csv")

mgmt_path=paste0("Data/",site_name,"/Management/") # same as obs_mgmt_path but going with it

adjusted_ops_filename="clean_ops_ext_adj.csv"
fut_weather_path <- paste0("Data/CMIP6/",site_name,"/")

#######################################
print("Copying over APSIM files")
apsim_path <- paste0("APSIM/",site_name,"/")
if(!dir.exists(file.path(master_path, apsim_path))) copyDirectory(from = file.path(master_path, '/APSIM/KBS'),
                                                                         to = file.path(master_path, apsim_path),
                                                                         recursive = T)

#######################################
print("Copying over Daycent files -- get harvest 1_1 please please please joke")
daycent_path <- paste0("Daycent/",site_name,"/")
copyDirectory(from = file.path(master_path, '/Daycent/KBS'), to = file.path(master_path, daycent_path), recursive = T)

print("Is harvest file there?")
print(list.files(file.path(master_path, daycent_path), pattern = 'harvest_base_1_1.csv'))

#######################################

if(Sys.info()['sysname']=='Linux') {
  # copyDirectory(from = file.path(master_path, "LDNDC/ldndc-1.35.2.linux64/projects/KBS"),
  #               to = file.path(master_path, "LDNDC/ldndc-1.35.2.linux64/projects", site_name),
  #               recursive = T)
  dndc_path <- paste0("LDNDC/ldndc-1.35.2.linux64/projects/",site_name,"/")
} else {
  print("Can't run cause not linux")
  dndc_path <- paste0("LDNDC/ldndc-1.35.2.win64/projects/",site_name,"/")
}
rothc_path <- paste0("RothC/",site_name,"/")
mill_path <- paste0("Millennial/R/simulation/",site_name,"/")

# AD NOTE - Copying over the files from the KBS folder to the site folder
copyDirectory(from = file.path(master_path, "Millennial/R/simulation/KBS"), 
              to = file.path(master_path, mill_path), recursive = T)


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
clim_nums <- c(1:1)
mgmt_grps <- c(1:1) #calib_mgmt_grps #

for (x in clim_nums) { # climate scenarios
  print("************************************")
  print("####### New climate scenario #######")
  print("************************************")
  print(paste0("climate scenario: ",x))
  clim_scenario_num <- x
  # source("1_Create_weather_input_files.R")
  for (y in mgmt_grps) { # management scenario groups
    mgmt_scenario_grp <- y # scenario group number
    max_scenario_options <- if_else(y==4, 4, # option numbers for those with incremental adjustments
                            if_else(y==5, 3,
                            if_else(y==6, 5, 1)))

    for (z in 1:max_scenario_options) {
      print("************************************")
      print(paste0("climate scenario: ",x))
      print(paste0("mgmt scenario: ",y))
      print(paste0("mgmt option: ",z))
      mgmt_scenario_opt <- if(max_scenario_options==1) "" else z
      mgmt_scenario_num <- as.numeric(paste0(mgmt_scenario_grp,mgmt_scenario_opt))
      scenario_name <- paste0(clim_scenario_num,"_",mgmt_scenario_num)
      source("0_Controller2_.R", local = TRUE, echo = TRUE)
      #p_Controller2()
    }

  } # end loop through management scenario groups
} # end loop through climate scenarios

 # source(paste0("10_Model_Ensemble_results-combined_scenarios_",site_name,".R"))
 # source(paste0("10_Model_Ensemble_results-combined_scenarios_and_sites_compbaseline-two_climate_scenarios4.R"))

toc()

# end timer

run_time <- round(toc(echo=TRUE)/60,1)
print(paste0("Run time is ", run_time," minutes, ",run_time/60," hours."))

      
