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

run_parallel<-T

if(identical(run_parallel, TRUE)){
  cat("Running in parallel using foreach")
  #create the cluster--------------------
  n_threads<-3
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
}

foreach(county_seq = 1:3, .verbose = T, .combine = 'c', 
        .packages=c('apsimx','berryFunctions','broom','data.table','dplyr','ggplot2',
                    'graphics','lubridate','magrittr','pracma','R.utils','readxl','sf',
  'soilDB','soiltexture','stringr','tidyr','tictoc','tidyverse','XML','xml2')) %dopar% {

  print(paste0("county_seq is: ", county_seq))
  
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
      results_folder<-'/home/ap/Documents/national_gwp_results'
      Glade=FALSE
      print("************************************")
      print("*****Using linux mint *********")
    } else {
      home_folder<-'/glade/derecho/scratch/apdixon/national_gwp'
      results_folder<-'/glade/derecho/scratch/apdixon/national_gwp_results'
      Glade=TRUE
      print("************************************")
      print("*****Using NCAR *********")
      print("***** SCRATCH SPACE *********")
    }
  }
  
  setwd(home_folder)
  
  .GlobalEnv$master_path <- home_folder

  county_data<-read.csv(file.path('Data', 'County_start', 'county_centroids_elevation.csv'))
  
  
  "************************************"
  "************************************"
  Test <- TRUE
  
  if(identical(Test, TRUE)){
    county_data<-county_data%>%
      filter(GEOID %in% c(1075, 13023, 13213))
  }

  # county_data<-county_data[county_data$GEOID==county_number,]
  county_data<-county_data[county_number,]
  
  #  -- add this to each parameter here so that it's available down the line
  # https://stackoverflow.com/questions/63241209/object-not-found-in-foreach-loop

  run_Daycent=TRUE
  run_LDNDC=FALSE
  run_Millennial=FALSE
  del_input_files=TRUE
  "************************************"
  "************************************"
  # county_geoid<-sprintf("%05d", county_data$GEOID) # Use 5 character GEOID?
  # county_geoid<-paste0("_", county_data$GEOID, "_") # Use 5 character GEOID?, or put _ in front and behind to isolate for pattern matching
  .GlobalEnv$county_geoid<-county_data$GEOID # adapting 4 and 5 character geoids isn't necessary because we're always using paste statements with _ in front and behind
  .GlobalEnv$county_name<-county_data$NAMELSAD
  .GlobalEnv$state_name<-county_data$State_Name
  print("************************************")
  print(paste0("county geoid is: ", county_geoid))
  print(paste0("county name is: ", county_name))
  print(paste0("state name is: ", state_name))
  print("************************************")
  # TODO replace these when model is ready
  site_id <- county_data$GEOID
  # site_name <- paste0(gsub(" ", "_", county_data$NAMELSAD[i]),"_", gsub(" ", "_", county_data$State_Name[i]))
  .GlobalEnv$site_name <- paste0("GEOID_", site_id, "_", gsub(" ", "_", county_data$State_Name))
  
  print("************************************")
  print('working directory is: ')
  print(getwd())
  print("************************************")
  
  .GlobalEnv$latitude = county_data$Lat
  .GlobalEnv$longitude = county_data$Long
  .GlobalEnv$elevation_m = county_data$Elev_mean_m
  
  print(paste0("latitude is: ", latitude))
  print(paste0("longitude is: ", longitude))
  print(paste0("elevation_m is: ", elevation_m))
  
  source('00_Main_County.R', local = TRUE)


  cat("************************************\n")
  cat("DELETE INPUT FILES???\n")
  if(identical(del_input_files, TRUE)){
    cat("Deleting input files\n")
    unlink(file.path(master_path, 'Daycent', site_name), recursive = T)
  } else{
    print("Saving input data files")}
  cat("************************************")
  
  return(1) # adding this so that the foreach loop returns something
  } # END OF DOPAR COUNTY LOOP



if(identical(run_parallel, TRUE)){
  cat("closing the cluster\n")
  # #close the cluster--------------------
  # #setDTthreads(threads = n_threads)
  parallel::stopCluster(cl = my.cluster)
  # end timer
  run_time <- round(toc(echo=TRUE)/60,1)
  print(paste0("Run time is ",run_time," minutes, ",run_time/60," hours."))
}






