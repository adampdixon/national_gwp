#######################################
# Script: 1_Create_management_schedule_AD_.R
# Author: Adam Dixon
# Date: December 16, 2023
# Output: .sch file
# Description: Generates DayCent schedule file specifically in the format that
# Daycent needs
# https://www2.nrel.colostate.edu/projects/irc/public/Documents/Software/Century5/Reference/html/CMI/mgmt-sch-file.htm
#######################################
# Called by:
# 
#######################################
# Audit Log:
# Dec 2023 - Adpated from Maas scripts and colo state reference
#######################################

# # ---------------------------------------------------------------
# #  File: c3grs.sch
# #  DayCent5 management file for a C3 grassland.
# #  (The Century5 file would be the same but exclude "Daily output" lines.)
# #
# #  Runs for about 2005 years for a spinup w/grazing.
# #  Last 2 years do not have grazing.
# #  Prints monthly results for the last 3 years.
# #  Prints daily results for the last 2 years for DayCent5.
# #
# #  This schedule can be used with Century5 and DayCent5.
# # ---------------------------------------------------------------
# # Simulation Configuration
# 1           Starting year
# 2005        Last year
# not-used    Site file name
# 0           Labeling type
# 0           Labeling year
# -1          Microcosm
# 0           CO2 Systems
# 1           Initial system
# CPR         Initial crop
# Initial tree
# 
# Year Month Option
# 1           Block #   Northern Hemisphere C3 Dominated
# 2003        Last year
# 1           Repeats # years
# 2003        Output starting year
# 1           Output month
# 0.0833      Output interval
# M           Weather choice
# 1   1 CROP  CPR
# 1   1 FRST
# 1   5 GRAZ  GM
# 1   6 GRAZ  GM
# 1   7 GRAZ  GM
# 1   8 GRAZ  GM
# 1   9 GRAZ  GM
# 1  11 SENM
# 1  12 LAST
# END
# 
# 2           Block #   no grazing
# 2005        Last year
# 1           Repeats # years
# 2004        Output starting year
# 1           Output month
# 0.0833      Output interval
# 2004        Daily output starting year
# 1           Daily output month
# M           Weather choice
# 1   1 CROP  CPR
# 1   1 FRST
# 1  11 SENM
# 1  12 LAST
# END

print('*********Creating managment schedule file for Daycent*************')

# schedule_file_maize <- file.path(daycent_path2 , paste0("sched_base_", scenario_name, "_Maize.sch"))
# schedule_file_cotton <- file.path(daycent_path2 , paste0("sched_base_", scenario_name, "_Cotton.sch"))
# schedule_file_soybeans <- file.path(daycent_path2 , paste0("sched_base_", scenario_name, "_Soybean.sch"))
# schedule_file_wheat <- file.path(daycent_path2 , paste0("sched_base_", scenario_name, "_Wheat.sch"))

# Get schedule scenarios for each crop
source(paste0("Daycent/Daycent_County_Maize_schedule.R"), local = TRUE)
source(paste0("Daycent/Daycent_County_Soybean_schedule.R"), local = TRUE)
source(paste0("Daycent/Daycent_County_Wheat_schedule.R"), local = TRUE)
source(paste0("Daycent/Daycent_County_Cotton_schedule.R"), local = TRUE)
source(paste0("Daycent/Daycent_County_Rotation_schedule.R"), local = TRUE)

# schedule files are written in controller daycent loop

# Spinup equilibrium schedule file
sp_eq_schedule_file <- file.path(daycent_path2 , paste0("sched_eq.sch"))

sp_eq<-c('1             Starting year',
         '4000          Last year',
         'site.100  Site file name',
         '0             Labeling type',
         '-1            Labeling year',
         '-1.00         Microcosm',
         '-1            CO2 Systems',
         '-1            pH effect',
         '-1            Soil warming',
         '0             N input scalar option (0 or 1)',
         '0             OMAD scalar option (0 or 1)',
         '0             Climate scalar option',
         '1             Initial system',
         'GI3           Initial crop',
         '              Initial tree',
         '   ',
         'Year Month Option',
         '1            Block #   Eq_KBS',
         '3999         Last year',
         '1            Repeats # years',
         '1            Output starting year',
         '12           Output month',
         '100.0        Output interval',
         'F            Weather choice',
         'basic_1.wth',
         '  1   75 CROP GI3',
         '  1   75 FRST',
         '  1  106 GRAZ GM',
         '  1  136 GRAZ GM',
         '  1  167 GRAZ GM',
         '  1  197 GRAZ GM',
         '  1  228 GRAZ GM',
         '  1  259 GRAZ GM',
         '  1  289 LAST',
         '  1  289 SENM',
         '-999 -999 X',
         '2            Block #   Eq_KBS',
         '4000         Last year',
         '1            Repeats # years',
         '4000         Output starting year',
         '12           Output month',
         '10.0         Output interval',
         'C            Weather choice',
         '  1   75 CROP GI3',
         '  1   75 FRST',
         '  1  106 GRAZ GM',
         '  1  136 GRAZ GM',
         '  1  167 GRAZ GM',
         '  1  197 GRAZ GM',
         '  1  228 GRAZ GM',
         '  1  259 GRAZ GM',
         '  1  289 LAST',
         '  1  289 SENM',
         '-999 -999 X')


writeLines(sp_eq, sp_eq_schedule_file)



         




