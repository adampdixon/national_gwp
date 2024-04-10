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
tic()

# Set the model path to the location of your files.
prior_path <- getwd()
model_path = paste0(file.path(prior_path, "LDNDC", 'ldndc-1.35.2.linux64', 'projects',site_name),"/")

setwd(model_path)

# list.files(model_path)
# list.files(getwd())

#give linux system permission
# system(paste0("chmod 755 ", prior_path, "/LDNDC/ldndc-1.35.2.linux64/bin/ldndc")) # AD changed to below
system(paste0("chmod 755 ", prior_path, "/LDNDC/ldndc-1.35.2.linux64/bin/ldndc"))
system(paste0("chmod u+x ./callsh_", site_name, "_", scenario_name2,".sh"))
system(paste0("chmod u+x ./", site_name, "_", scenario_name2,".sh"))
system(paste0("chmod u+x ./", site_name, "_", scenario_name2,".ldndc"))




# ./callsh_1_1.sh: 1: ./KBS_1_1.sh: Permission denied

system(paste0("./callsh_",site_name, "_", scenario_name2,".sh"), wait=TRUE)

setwd(prior_path)

print("Finished run_LDNDC.R")

# end time
toc()
run_time_ld <- round(toc(echo=T)/60,1)
print(paste0("LDNDC run time is ",run_time_ld," minutes"))
