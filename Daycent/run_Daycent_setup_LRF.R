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
# 4/2/2023: Removed soiln references for LRF.
###########################################################################

print("Starting run_Daycent_setup.R")

# Set the model path to the location of this site's files.
prior_path <- getwd()
model_path = paste0("/home/ap/Documents/GitHub/national_gwp/Daycent/",site_name,"/")
#modelPath = paste0("~/Modeling/Daycent/KBS - Copy/") # for testing
setwd(model_path)

# local constants
if(Sys.info()['sysname']=='Linux') {
  daycent_executable <- "./DDcentEVI_rev279"
  daycent_list100 <- "./DDlist100_rev279"
  
  # AD give system permissions if getting permission error 
  system(paste0('chmod u+x ./DDcentEVI_rev279'))
  system(paste0('chmod u+x ./DDlist100_rev279'))
  
} else {
  daycent_executable <- "DD17centEVI.exe"
  daycent_list100 <- "DD17list100.exe"
}

# --------------- Reset working directory --------------- 
setwd(prior_path)
