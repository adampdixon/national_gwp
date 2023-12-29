###########################################################################
# Script: run_LDNDC.R
# Author: Ellen Maas 
# Date: April 20, 2023
# Description: Launches shell script to run LDNDC.
#
###########################################################################
# Audit Trail
# 4/20/2023: Created script.
###########################################################################

print("Starting run_LDNDC.R")

# Set the model path to the location of your files.
prior_path <- getwd()
model_path = paste0(file.path(prior_path, "LDNDC", 'ldndc-1.35.2.linux64', 'projects',site_name),"/")
setwd(model_path)

#give linux system permission
system(paste0("chmod 755 ", prior_path, "/LDNDC/ldndc-1.35.2.linux64/bin/ldndc"))
system(paste0("chmod u+x ./callsh_",scenario_name,".sh"))
system(paste0("chmod u+x ./", site_name, "_", scenario_name,".sh"))
system(paste0("chmod u+x ./", site_name, "_", scenario_name,".ldndc"))




# ./callsh_1_1.sh: 1: ./KBS_1_1.sh: Permission denied

system(paste0("./callsh_",scenario_name,".sh"), wait=TRUE)

setwd(prior_path)
