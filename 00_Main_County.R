
#######################################
# File: "00_Main.R"
# Author: "Ellen Maas and then Adam Dixon"
# Date: "June 2024"
# Description: This drives the looping
# through climate scenarios. It sets up climate and directories.
#
#######################################

library(pracma)
library(dplyr)
# library(R.utils)

#######################################




experiment_start_date <- "1850-01-01" # for LDNDC

end_fut_period_year <- 2050
max_fut_period_year <- 2050 # for LDNDC

wth_path <- paste0("Data/County/Weather/")

daycent_path <- paste0("Daycent/",site_name,"/")
daycent_path2<-file.path(master_path, 'Daycent' ,site_name)





#######################################
# Create LDNDC dir
if(identical(run_LDNDC, TRUE)) {
dndc_path <- paste0("LDNDC/ldndc-1.36.linux64/projects/",site_name,"/")
unlink(dndc_path, recursive = TRUE)
dir.create(file.path(dndc_path))
}
#######################################
# Create Millennial dir
if(identical(run_Millennial, TRUE)) {
  mill_path <- paste0("Millennial/R/simulation/",site_name,"/")
  unlink(mill_path, recursive = TRUE)
  dir.create(file.path(mill_path))
  
  copy_from_<-file.path("Millennial/R/simulation/Millennial copy files")
  files_to_copy<-list.files(copy_from_, full.names = T)
  copy_to_<-mill_path
  
  # copy the files
  file.copy(from = files_to_copy, to = copy_to_, overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
  
  print("Copy over Millennial files --")
    
  }

#######################################
# Create Daycent dir
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


for (fut_climate in clim_nums) { # climate scenarios
    print("************************************")
    print("####### Climate scenario 2022 to 2050 #######")
    print("************************************")
    clim_scenario_num <- fut_climate # not exactly sure why went with 2 variables for same thing
    print(paste0("climate scenario: ", clim_scenario_num)) # why needed?

    cat(paste0("*********Model will be run for ", crops_, "*********\n"))
    cat("********* mgmt scenarios", mgmt_scenario_nums, " *********\n")
    
    # Run controller
    source("0_Controller2_County.R", local = TRUE)

}



