###########################################################################
# Script: run_Daycent_controller.R
# Author: Ellen Maas 2/21/2023
# Description: Calls scripts to run all or some segments of a Daycent
# modeling run. Because each step is run as an extension of the step before,
# any subsequent step can be run independently as many times as necessary.
# This saves time rerunning prior steps if their inputs and outputs won't 
# change. 
###########################################################################
# Audit Trail
# 2/21/2023: Created script.
###########################################################################

print("Starting Daycent_run_controller.R")

## Always run the setup script
source(paste0("Daycent/run_Daycent_setup_",site_name,".R"))


## The following must all be run at least once, in order. To save time in
## subsequent testing, comment out prior steps that have already been run
## successfully to preserve the output. Each step uses the output of the
## step before.
# --------------- Step 1: Run equilibrium simulation (4000-year spin-up) ---------------
source(paste0("Daycent/run_Daycent_eq_",site_name,".R"))

# --------------- Step 2: Run base cropping simulations (land conversion - start exp) ---------------
source(paste0("Daycent/run_Daycent_base_",site_name,".R"))

# --------------- Step 3: Run experimental period simulations  ---------------
source(paste0("Daycent/run_Daycent_exp_",site_name,".R"))

# --------------- Step 4: Run future emissions scenarios (end exp-2100) ---------------
#### NOTE: results for calibration and future graphs both are based on the 
#### future simulation output file. The output of each phase includes all the
#### results from the phases before it. So if you're testing just the base
#### or experimental phase, you'll have to run this far anyway, or make a 
#### version of the setup file that only uses the base or exp output files.
source("Daycent/run_Daycent_fut.R")

