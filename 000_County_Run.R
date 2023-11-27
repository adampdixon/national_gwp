
#######################################
# File: "000_County_Run.R"
# Author: "Adam Dixon"
# Date: "Nov 23, 2023"
# Description: This runs the Main file for a single or multiple counties."
#
#######################################
# Calls:

#######################################
# Audit Log
#
#######################################
library(dplyr)

############################################################################################################
############################################################################################################

# Set workspace
if (Sys.info()['sysname'] == "Darwin"){
  home_folder<-file.path('/Users/adamdixon/Documents/GitHub/national_gwp')
  print("************************************")
  print("*****Using Mac OS *********")
} 

if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['machine']=='x86_64') {
    home_folder<-'/home/ap/Documents/GitHub/national_gwp'
    print("************************************")
    print("*****Using linux mint *********")
  } else {
    home_folder<-'/glade/u/home/apdixon/Documents/national_gwp'
    print("************************************")
    print("*****Using NCAR *********")
  }
}



master_path <- home_folder
setwd(master_path)


# county_geoid<-13193 #Macon County, Georgia
# county_geoid<-13119 # Franklin County, Georgia



# rm(list=ls())

# apsimx_options(exe.path="/bin/lib64/R/library/apsimx/R/")




#
# county_data<-read.csv(file.path(master_path, 'Data', 'County', 'county_centroids_elevation.csv'))%>%
#   filter(GEOID==county_geoid)

county_data<-read.csv(file.path(master_path, 'Data', 'County', 'county_centroids_elevation.csv'))%>%
  filter(State_Name=='Georgia')

county_data<-county_data[22:22,]

for (i in 1:nrow(county_data)){
  county_geoid<-county_data$GEOID[i]
  county_name<-county_data$NAMELSAD[i]
  state_name<-county_data$State_Name[i]
  print(paste0("county geoid is: ", county_geoid))
  print(paste0("county name is: ", county_name))
  print(paste0("state name is: ", state_name))
  # TODO replace these when model is ready
  site_id <- county_data$GEOID[i]
  # site_name <- paste0(gsub(" ", "_", county_data$NAMELSAD[i]),"_", gsub(" ", "_", county_data$State_Name[i]))
  site_name <- paste0("_", site_id, "_", substr(county_data$State_Name[i], 1, 4))

  print("************************************")
  print('working directory is: ')
  print(master_path)
  print(paste0("county geoid is: ", county_geoid))
  print("************************************")


  # print(paste0("site_name is: ", site_name))

  latitude = county_data$Lat
  longitude = county_data$Long
  elevation_m = county_data$Elevation_m

  source(file.path(master_path, '00_Main_.R'))
}


############################################################################################################
############################################################################################################
# THis doesn't work. Parameters have to be passed to global env.
# library(doParallel)
###########FOR PARALLEL
# 
county_parallel<-function(county_index){
  library(dplyr)
  #####################################################
  # Set workspace
  if (Sys.info()['sysname'] == "Darwin"){
    home_folder<-file.path('/Users/adamdixon/Documents/GitHub/national_gwp')
    print("************************************")
    print("*****Using Mac OS *********")
  }

  if (Sys.info()['sysname'] == "Linux"){
    if(Sys.info()['machine']=='x86_64') {
      home_folder<-'/home/ap/Documents/GitHub/national_gwp'
      print("*****Using linux mint *********")
    } else {
      home_folder<-'/glade/u/home/apdixon/Documents/national_gwp'
      print("*****Using NCAR *********")
    }
  }
  #####################################################
  home_folder<-'/home/ap/Documents/GitHub/national_gwp'
  master_path <- home_folder
  # setwd(master_path)


  county_data<-read.csv(file.path(home_folder, 'Data', 'County', 'county_centroids_elevation.csv'))%>%
    filter(State_Name=='Georgia')
  county_data<-county_data[county_index,]

  county_geoid<-county_data$GEOID
  county_name<-county_data$NAMELSAD
  state_name<-county_data$State_Name
  print(paste0("county geoid is: ", county_geoid))
  print(paste0("county name is: ", county_name))
  print(paste0("state name is: ", state_name))
  # TODO replace these when model is ready
  site_id <- county_data$GEOID
  # site_name <- paste0(gsub(" ", "_", county_data$NAMELSAD[i]),"_", gsub(" ", "_", county_data$State_Name[i]))
  site_name <- paste0("_", site_id, "_", substr(county_data$State_Name, 1, 4))

  print("************************************")
  print('working directory is: ')
  # print(master_path)
  print(paste0("county geoid is: ", county_geoid))
  print("************************************")

  latitude = county_data$County_center_Lat
  longitude = county_data$County_center_Long
  elevation_m = county_data$Elevation_m

  # commandArgs <- function(...) c(master_path, site_id, site_name, latitude, longitude, elevation_m)
  source(file.path(home_folder, '00_Main_.R'), local = TRUE)
  
}

# 
county_parallel(county_index=21)
# 
# ###########PARALLEL
# library(parallel)
# library(tictoc)
# ncores<-detectCores(logical = T)
# cl<-makeCluster(ncores-1)
# tic()
# numbers<-c(1:10)
# clusterApply(cl, numbers, county_parallel)
# stopCluster(cl)
# toc()
# ######################################