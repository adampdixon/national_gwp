
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
library(dplyr)
# library(R.utils)

# start timer
tic()

# rm(list=ls())
# master_path <- "/home/ap/Documents/GitHub/national_gwp"
# setwd(master_path)
#apsimx_options(exe.path="/bin/lib64/R/library/apsimx/R/")


#######################################

# This should be rare since we're using the same data for all counties and a folder named "County"
# library(R.utils)
# print("Copying the KBS 'Data' files")
# if(!dir.exists(file.path(master_path, 'Data', 'County'))) copyDirectory(from = file.path(master_path, '/Data/KBS'),
#                                                                      to = file.path(master_path, 'Data', 'County'),
#                                                                      recursive = T)
#######################################


# site_id <- 1
# site_name <- "KBS"
# latitude = 42.410
# longitude = -85.372
# elevation_m = 288
# experiment_start_year <- 1989
# experiment_end_year <- 2021
# experiment_year_range <- experiment_start_year:experiment_end_year
experiment_start_date <- "1950-01-01" # for LDNDC
# experiment_end_date <- "2021-12-31"
# end_exp_period_year <- 2021
end_fut_period_year <- 2050
max_fut_period_year <- 2050 # for LDNDC
# calib_mgmt_grps <- c(1,2,3)
# calib_mgmt_nums <- c(1,2,3)
#
# obs_path <- paste0("Data/County/Calibration/")
# obs_mgmt_path <- paste0("Data/County/Management/")
# hist_wth_filename <- "NOAA-based Daily Kalamazoo 1900-2020.csv"
# hist_wth_mon_filename <- "Monthly Kalamazoo 1900-2020 with OPE.csv"
# curr_local_wth_filename <- "12-lter+weather+station+daily+weather+all+variates+1657202230.csv"
wth_path <- paste0("Data/County/Weather/")
# nasapower_output_filename <- paste0(site_name,"_np.csv")
# mgmt_path=paste0("Data/KBS/Management/")
# adjusted_ops_filename="clean_ops_ext_adj.csv"
# fut_weather_path <- paste0("Data/CMIP6/",site_name,"/")
# apsim_path <- paste0("APSIM/",site_name,"/")

daycent_path <- paste0("Daycent/",site_name,"/")
daycent_path2<-file.path(master_path, 'Daycent' ,site_name)


#######################################
# Create LDNDC dir
if(identical(run_LDNDC, TRUE)) {
dndc_path <- paste0("LDNDC/ldndc-1.35.2.linux64/projects/",site_name,"/")
unlink(dndc_path, recursive = TRUE)
dir.create(file.path(dndc_path))
}
#######################################

mill_path <- paste0("Millennial/R/simulation/",site_name,"/")


#######################################
print("Copying over KBS 'Daycent model' files --")
# dir.create(file.path(master_path, 'Daycent', site_name))

copy_from_ <-file.path(master_path, 'Daycent', 'KBS_4copy2')
copy_to_ <-daycent_path2

if(length(list.files(copy_to_))>11) { # There are 12 daycent files to copy over, so this is a good threshold
  print("Site daycent folder already exists. Skipping copy.")
} else {
  
  #create the directory
  dir.create(copy_to_)
  
  #list all the files to copy
  files_to_copy <- list.files(copy_from_, full.names = T)
  
  # copy the files
  file.copy(from = files_to_copy, to = copy_to_, overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)

}
#######################################

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




# Management scenarios:
# 1 - MC - Monocropping
# 2 - NT - No-till
# 3 - CCM - Cover crop mix (legumes/brassicas/cereals)
# 4 - CCC - Cover crop cereal (rye)
# 5 - CCL - -Cover crop legume (vetch)
# 6 - CCNT - Cover crop mix + notill
# 7 - CR - Crop rotation (corn-soybeans)

# mgmt_grps <- c(1:1) # These are left over from ellen's code
for (fut_climate in clim_nums) { # climate scenarios, in case we want different versions
    print("************************************")
    print("####### Climate scenario 2022 to 2050 #######")
    print("************************************")
    clim_scenario_num <- fut_climate
    print(paste0("climate scenario: ", clim_scenario_num)) # why needed?

    cat(paste0("*********Model will be run for ", crops_, "*********\n"))
    cat("********* mgmt scenarios", mgmt_scenario_nums, " *********\n")
    
    # Run controller
    source("0_Controller2_County.R", local = TRUE)

}


  
  
 # source(paste0("10_Model_Ensemble_results-combined_scenarios_",site_name,".R"))
 # source(paste0("10_Model_Ensemble_results-combined_scenarios_and_sites_compbaseline-two_climate_scenarios4.R"))

# end timer
run_time <- round(toc(echo=TRUE)/60,1)
print(paste0("Run time is ",run_time," minutes, ",run_time/60," hours."))

      
