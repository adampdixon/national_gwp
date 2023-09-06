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

print("Starting run_Daycent4.R")

# Set the model path to the location of this site's files.
prior_path <- getwd()
model_path = paste0("~/Modeling/Daycent/",site_name,"/")
#modelPath = paste0("~/Modeling/Daycent/KBS - Copy/") # for testing
setwd(model_path)

# --------------- Run equilibrium simulation (4000-year spin-up) ---------------
#
# This generates native plant and soil conditions up to the point of conversion
# from the native ecosystem to historical cropping at the site.
#
# This equilibrium simulation takes a long time, so don't execute
# these commands if you already have an equilibrium binary file (*.bin)
# and you haven't made any changes to any parameter files.

# Equilibrium: 4000 years of grazed grassland
# Every scenario run uses the same equilibrium schedule file, so there is only one
print("**********Daycent equilibrium simulation*********")
file.copy("outfiles_eq.in", "outfiles.in", overwrite=TRUE)
unlink(paste0("sched_eq.bin"))
unlink(paste0("sched_eq.lis"))
# -s parameter is the schedule file name, -n parameter is the binary output file name,
# minus the dot-extentions
system(paste0(daycent_executable," -s sched_eq -n sched_eq"), wait=TRUE)
system(paste0(daycent_list100," sched_eq sched_eq outvars.txt"), wait=TRUE)

# the following old code created different equil files for each scenario - why?
file.copy("outfiles_eq.in", "outfiles.in", overwrite=TRUE)
unlink(paste0("sched_eq_",scenario_name,".bin"))
unlink(paste0("sched_eq_",scenario_name,".lis"))
# -s parameter is the schedule file name, -n parameter is the binary output file name,
# minus the dot-extentions
system(paste0(daycent_executable," -s sched_eq ",
              " -n sched_eq_",scenario_name), wait=TRUE)
system(paste0(daycent_list100," sched_eq sched_eq_",scenario_name,
              " outvars.txt"), wait=TRUE)


# --------------- Reset working directory --------------- 
setwd(prior_path)

