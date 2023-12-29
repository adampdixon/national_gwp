###########################################################################
# Script: run_Daycent4.R
# Author: Ellen Maas July 27, 2022
#         (Adapted from run_DD17CentEVI_Clovis_ds.R)
# Description: Simulates corn-soy-wheat trials at the Kellogg Biological 
# Station, MI.
#
###########################################################################
# Audit Trail
# 9/27/2022: Rewrote to more generic time points, removed site name
# 12/13/2022: Reverted equilibrium run naming to remove scenario number.
# 1/3/2023: Added soiln.out to output files processing.
###########################################################################

print("Starting run_Daycent_setup.R")

#***** If running this script independently, uncomment this group 
# Clear memory before rerunning the script. 
#rm(list=ls())
#setwd("~")
#site_name="KBS"
# Load observations and global constants
#source("Modeling/0_Observations_and_constants.R")
#*****

# Set the model path to the location of this site's files.
prior_path <- getwd()
model_path = paste0(master_path, site_name,"/")
#modelPath = paste0("~/Modeling/Daycent/KBS - Copy/") # for testing
setwd(model_path)

# local constants
if(Sys.info()['sysname']=='Linux') {
  daycent_executable <- "./DDcentEVI_rev279"
  daycent_list100 <- "./DDlist100_rev279"
} else {
  daycent_executable <- "DD17centEVI.exe"
  daycent_list100 <- "DD17list100.exe"
}


# --------------- Install this scenario's input files ---------------------
#
# Load scenario-specific input files.
if(mgmt_scenario_grp==2) {
  file.copy("soils_2.in","soils.in", overwrite=TRUE)
  file.copy("site_2.100","site.100", overwrite=TRUE)
  file.copy("fix_2.100","fix.100", overwrite=TRUE)
} else if(mgmt_scenario_grp==3) {
  file.copy("soils_3.in","soils.in", overwrite=TRUE)
  file.copy("site_3.100","site.100", overwrite=TRUE)
  file.copy("fix_3.100","fix.100", overwrite=TRUE)
} else {
  file.copy("soils_1.in","soils.in", overwrite=TRUE)
  file.copy("site_1.100","site.100", overwrite=TRUE)
  file.copy("fix_1.100","fix.100", overwrite=TRUE)
}

# use default crop.100 except for future for scenario 3
file.copy("crop_allothers.100","crop.100", overwrite=TRUE)


# --------------- Reset working directory --------------- 
setwd(prior_path)
