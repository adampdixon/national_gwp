#######################################
# Script: 1_Create_weather_input_files.R
# Author: Ellen Maas
# Date: Dec. 21, 2022
# Description: This procedure is a control file just for the weather
# file generation scripts. this only needs to be run once per climate
# scenario. Which files are created are controlled by the 
# clim_scenario_num value.
#######################################
# Audit Log:
# 12/21/2022: Created script.
# 1/31/2023: Removed variable definition for "fut_weather_path" as 
# it's in 00_Main and needed when testing the weather file creation
# scripts.
# 2/19/2023: Modified to use site name for setup script.
#######################################


source(paste0("1_Create_weather_input_files-setup_",site_name,".R"))
#
source(paste0("1_Create_weather_input_files-APSIM_",site_name,".R"))
source(paste0("1_Create_weather_input_files-Daycent_",site_name,".R"))
#source(paste0("1_Create_weather_input_files-LDNDC_",site_name,".R"))
source(paste0("1_Create_weather_input_files-RothC_",site_name,".R"))
# Millennial doesn't use climate data as inputs.
# Uses soil temperature and moisture together
# with daily NPP (C input). Generated in
# "p3_Create_management_input_files".
