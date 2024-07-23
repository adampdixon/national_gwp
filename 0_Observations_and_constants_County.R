#######################################
# Procedure: 0_Observations_and_constants
# Author: Ellen Maas, and then Adam Dixon
# Date: July 2024
# Description: It is designed to be run as-is from calling scripts in order
# to create the variables in the local space. It imports data from files and sets 
# values to shared variables that will be used throughout the project.
#######################################

#rm(list=ls())

# may need to change this for linux:
# https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio

suppressMessages({

print(paste0("Starting 0_Observations_and_constants_County.R"))

  
library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(broom)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
  
  
##################################################  
##################################################
#################### Weather and Daycent paths ###
##################################################
# wth_path <- paste0("Data/County/Weather/")
# apsim_path <- paste0("APSIM/",site_name,"/")
daycent_path <- paste0("Daycent/",site_name,"/")
##################################################
#################### ######### ###################
##################################################

if(identical(run_LDNDC,TRUE)) {
  county_mana<-read.csv(file.path(master_path, 'Data', 'County_start', 'LDNDC_events.csv')) # LDNDC events has all mgmt event timelines
  
  ldndc_run_path <- paste0("LDNDC/ldndc-1.36.linux64/projects/")
  dndc_path <- paste0("LDNDC/ldndc-1.36.linux64/projects/",site_name,"/")
} 

mill_path <- paste0("Millennial/R/simulation/",site_name,"/")

land_conversion_year <- 1850

depth_m <- 0.25
equil_C_input <- 305.00 #244.21 #210.84 # g C/m^2 annually
surface_C_init <- 60 # Mg C ha-1


###########################################################
#################### observational data ###################
###########################################################


####################### soils #######################
if (identical(Glade, TRUE)){
  soil_data_path<-'/glade/work/apdixon/soils'
  print(paste0('*********soils_data_path is ', soil_data_path, " **************"))
} else {
  soil_data_path<-'/home/ap/soils'
  print(paste0('*********soils_data_path is ', soil_data_path, " **************"))
}


####################### climate #######################
if (identical(Glade, TRUE)){
  climate_data_path<-'/glade/work/apdixon/climate'
  print(paste0('*********climate_data_path is ', climate_data_path, " **************"))
} else {
  climate_data_path<-'/home/ap/Scratch'
}


}) # end suppressMessages