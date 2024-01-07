######################################################
# This script gathers the csvs for each US county from historic climate data (1951-2021) and creates a table
# for Daycent 
# A Dixon
# Dec 4, 2023
###########################

######################################################
# parallel script will create 6 tables, one for each variable and model (nclim, cmip6)
######################################################
# 
# library(dplyr) # for piping & tibble
library(doParallel) # for parallel processing
library(foreach) # for parallel processing
library(tictoc) # for timing
# library(data.table) # for fwrite

# # county data to link
# # geo_link_dir<-'/glade/u/home/apdixon/Documents/national_gwp/Data/County_start'
# geo_link_dir<-'/home/ap/Documents/GitHub/national_gwp/Data/County_start'
# 
# 
# # add geolink table to make GEOIDS align with census GEOID
# geo_link<-read.csv(file.path(geo_link_dir, 'county_geoid_link.csv'))%>%
#   select(zh_geoid, REAL_GEOID)%>%
#   as_tibble()%>%
#   arrange(REAL_GEOID)%>%
#   filter(REAL_GEOID %in% c(46087, 46085))
# 
# head(geo_link)
# tail(geo_link)
# 
# geoids<-geo_link$REAL_GEOID




#create the cluster--------------------
n_threads<-2
# county_range<-geoids
# 
my.cluster <- parallel::makeCluster(
  n_threads,
  type = "FORK",
  outfile="Log.txt")
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()
foreach::getDoParWorkers()
# set number of threads for data.table
# setDTthreads(threads = n_threads)

foreach(county_seq = 1:3, .verbose = T, .combine = 'c', 
        .packages=c('apsimx','berryFunctions','broom','data.table','dplyr','ggplot2',
                    'graphics','lubridate','magrittr','pracma','R.utils','readxl','sf',
  'soilDB','soiltexture','stringr','tidyr','tictoc','tidyverse','XML','xml2')) %do% {

  print(county_seq)
  
  # x = the number of counties to run
  county_number<-county_seq
  tic()
  # Open a connection to stderr
  sink(stderr(), type = "message")
  # Print an error message to stderr
  cat(paste0("Starting county ", county_number, "\n"), file = stderr(), append = TRUE)
  
  # Set workspace
  if (Sys.info()['sysname'] == "Darwin"){
    home_folder<-file.path('/Users/adamdixon/Documents/GitHub/national_gwp')
    Glade=FALSE
    print("************************************")
    print("*****Using Mac OS *********")
  } 
  
  if (Sys.info()['sysname'] == "Linux"){ 
    if(Sys.info()['user']=='ap') {
      home_folder<-'/home/ap/Documents/GitHub/national_gwp'
      Glade=FALSE
      print("************************************")
      print("*****Using linux mint *********")
    } else {
      home_folder<-'/glade/derecho/scratch/apdixon/national_gwp'
      Glade=TRUE
      print("************************************")
      print("*****Using NCAR *********")
      print("***** SCRATCH SPACE *********")
    }
  }
  
  master_path <- home_folder
  
  
  setwd(home_folder)
  # county_geoid<-13193 #Macon County, Georgia
  # county_geoid<-13119 # Franklin County, Georgia
  # rm(list=ls())
  
  # apsimx_options(exe.path="/bin/lib64/R/library/apsimx/R/")
  
  # county_data<-read.csv(file.path(master_path, 'Data', 'County', 'county_centroids_elevation.csv'))%>%
  #   filter(GEOID==county_geoid)
  
  county_data<-read.csv(file.path(master_path, 'Data', 'County_start', 'county_centroids_elevation.csv'))
  
  Test <- TRUE
  
  if(identical(Test, TRUE)){
    county_data<-county_data%>%
      filter(GEOID %in% c(46085, 27069, 46087))
  }

  # county_data<-county_data[county_data$GEOID==county_number,]
  county_data<-county_data[county_number,]


  run_Daycent=TRUE
  run_LDNDC=FALSE
  run_Millennial=FALSE
  county_geoid<-sprintf("%05d", county_data$GEOID)
  county_name<-county_data$NAMELSAD
  state_name<-county_data$State_Name
  print(paste0("county geoid is: ", county_geoid))
  print(paste0("county name is: ", county_name))
  print(paste0("state name is: ", state_name))
  # TODO replace these when model is ready
  site_id <- county_data$GEOID
  # site_name <- paste0(gsub(" ", "_", county_data$NAMELSAD[i]),"_", gsub(" ", "_", county_data$State_Name[i]))
  site_name <- paste0("GEOID_", site_id, "_", gsub(" ", "_", county_data$State_Name))
  
  print("************************************")
  print('working directory is: ')
  print(master_path)
  print(paste0("county geoid is: ", county_geoid))
  print("************************************")
  
  latitude = county_data$Lat
  longitude = county_data$Long
  elevation_m = county_data$Elev_mean_m
  
  print(paste0("latitude is: ", latitude))
  print(paste0("longitude is: ", longitude))
  print(paste0("elevation_m is: ", elevation_m))
  
  source(file.path(master_path, '00_Main_County.R'), local = TRUE)
  return(1)
# #close the cluster--------------------
# #setDTthreads(threads = n_threads)

  parallel::stopCluster(cl = my.cluster)
  toc()


}



