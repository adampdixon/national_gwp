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
model_path = paste0(master_path, "/Daycent/",site_name,"/")
#modelPath = paste0("~/Modeling/Daycent/KBS - Copy/") # for testing
setwd(model_path)


# --------------- Run experimental period simulations  ---------------

print("**********Daycent experimental period simulation*********")
# Remove all prior output files
unlink(paste0("sched_exp_",scenario_name,".bin"))
unlink(paste0("sched_exp_",scenario_name,".lis"))
unlink("cflows.out")
unlink(paste0("cflows_exp_",scenario_name,".out"))
#file.copy("dc_sip.csv", paste0("dc_sip_exp_",scenario_name,".csv"), overwrite=TRUE)
unlink("harvest.csv")
unlink(paste0("harvest_exp_",scenario_name,".csv"))
unlink("livec.out")
unlink(paste0("livec_exp_",scenario_name,".out"))
unlink("methane.out")
unlink(paste0("methane_exp_",scenario_name,".out"))
#file.copy("nflux.out", paste0("nflux_exp_",scenario_name,".out"), overwrite=TRUE)
unlink("soiln.out")
unlink(paste0("soiln_exp_",scenario_name,".out"))
unlink("soiltavg.out")
unlink(paste0("soiltavg_exp_",scenario_name,".out"))
unlink("summary.out")
unlink(paste0("summary_exp_",scenario_name,".out"))
unlink("vswc.out")
unlink(paste0("vswc_exp_",scenario_name,".out"))
unlink("wfps.out")
unlink(paste0("wfps_base_",scenario_name,".out"))

# Experiment cropping schedule: full experimental period
file.copy("outfiles_base.in", "outfiles.in", overwrite=TRUE)

system(paste0(daycent_executable," -s sched_exp_",scenario_name,
              " -n sched_exp_",scenario_name,
              " -e sched_base"), wait=TRUE)
system(paste0(daycent_list100," sched_exp_",scenario_name,
              " sched_exp_",scenario_name," outvars.txt"), wait=TRUE)

file.copy("cflows.out", paste0("cflows_exp_",scenario_name,".out"), overwrite=TRUE)
#file.copy("dc_sip.csv", paste0("dc_sip_exp_",scenario_name,".csv"), overwrite=TRUE)
file.copy("harvest.csv", paste0("harvest_exp_",scenario_name,".csv"), overwrite=TRUE)
file.copy("livec.out", paste0("livec_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("methane.out", paste0("methane_exp_",scenario_name,".out"), overwrite=TRUE)
#file.copy("nflux.out", paste0("nflux_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiln.out", paste0("soiln_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiltavg.out", paste0("soiltavg_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("summary.out", paste0("summary_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("vswc.out", paste0("vswc_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("wfps.out", paste0("wfps_exp_",scenario_name,".out"), overwrite=TRUE)


# --------------- Reset working directory --------------- 
setwd(prior_path)

