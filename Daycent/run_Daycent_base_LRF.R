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


# Set the model path to the location of your files.
prior_path <- getwd()
model_path = paste0("~/Modeling/Daycent/",site_name,"/")
#modelPath = paste0("~/Modeling/Daycent/KBS - Copy/") # for testing
setwd(model_path)


# --------------- Run base cropping simulations (land conversion - start exp) ---------------

# Base cropping schedule: date of land conversion -> start of experimental period
# Every scenario run uses the same base schedule file, so there is only one
print("**********Daycent base simulation*********")
unlink(paste0("sched_base.bin"))
unlink(paste0("sched_base.lis"))
unlink("cflows.out")
unlink(paste0("cflows_base.out"))
#file.copy("dc_sip.csv", "dc_sip_base.csv", overwrite=TRUE)
unlink("harvest.csv")
unlink(paste0("harvest_base.csv"))
unlink("livec.out")
unlink(paste0("livec_base.out"))
unlink("methane.out")
unlink(paste0("methane_base.out"))
#file.copy("nflux.out", "nflux_base.out", overwrite=TRUE)
unlink("soiltavg.out")
unlink(paste0("soiltavg_base.out"))
unlink("soiln.out")
unlink(paste0("soiln_base.out"))
unlink("summary.out")
unlink(paste0("summary_base.out"))
unlink("vswc.out")
unlink(paste0("vswc_base.out"))
unlink("wfps.out")
unlink(paste0("wfps_base.out"))

file.copy("outfiles_base.in", "outfiles.in", overwrite=TRUE)

system(paste0(daycent_executable," -s sched_base",
              " -n sched_base",
              " -e sched_eq"), wait=TRUE)
system(paste0(daycent_list100," sched_base sched_base outvars.txt"), wait=TRUE)

file.copy("cflows.out", paste0("cflows_base.out"), overwrite=TRUE)
#file.copy("dc_sip.csv", "dc_sip_base.csv", overwrite=TRUE)
file.copy("harvest.csv", paste0("harvest_base.csv"), overwrite=TRUE)
file.copy("livec.out", paste0("livec_base_",scenario_name,".out"), overwrite=TRUE)
file.copy("methane.out", paste0("methane_base.out"), overwrite=TRUE)
#file.copy("nflux.out", "nflux_base.out", overwrite=TRUE)
file.copy("soiln.out", paste0("soiln_base.out"), overwrite=TRUE)
file.copy("soiltavg.out", paste0("soiltavg_base.out"), overwrite=TRUE)
file.copy("summary.out", paste0("summary_base.out"), overwrite=TRUE)
file.copy("vswc.out", paste0("vswc_base.out"), overwrite=TRUE)
file.copy("wfps.out", paste0("wfps_base.out"), overwrite=TRUE)


# --------------- Reset working directory --------------- 
setwd(prior_path)

