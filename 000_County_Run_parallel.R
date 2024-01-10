######################################################
# This script gathers the csvs for each US county from historic climate data (1951-2021) and creates a table
# for Daycent 
# A Dixon
# Jan 8, 2024
###########################

######################################################
# parallel script will create 6 tables, one for each variable and model (nclim, cmip6)
######################################################
# 
# library(dplyr) # for piping & tibble
# library(doParallel) # for parallel processing
library(doFuture) # for parallel processing and managing global variables
plan(multisession) # for use with doFuture
library(foreach) # for parallel processing
library(tictoc) # for timing
library(data.table) # for fwrite

# Set workspace
if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    master_path<-'/home/ap/Documents/GitHub/national_gwp'
    results_folder<-'/home/ap/Documents/national_gwp_results'
    Glade=FALSE
    print("************************************")
    print("*****Using linux mint *********")
  } else {
    master_path<-'/glade/derecho/scratch/apdixon/national_gwp'
    results_folder<-'/glade/derecho/scratch/apdixon/national_gwp_results'
    Glade=TRUE
    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
  }
}

cat("date and time are ")
print(Sys.time())

cat("************************************\n")
cat("******** FLAGS etc *****************\n")
run_parallel<-TRUE
Test <- TRUE # if TRUE, only run 3 counties, filtered below
del_input_files<-TRUE
n_cores<-3 # number of cores to use

run_Daycent=TRUE
run_LDNDC=FALSE
run_Millennial=FALSE
county_numbers<-1:3 #11:3108
cat("************************************\n")
cat("************************************\n")

if(identical(run_parallel, TRUE)){
  cat("Running in parallel using foreach")
  #create the cluster--------------------

  # county_range<-geoids
  # 
  my.cluster <- parallel::makeCluster(
    n_cores,
    # type = "FORK",
    outfile="Log.txt")
  #register it to be used by %dopar%
  doParallel::registerDoParallel(cl = my.cluster, cores = n_cores)
  #check if it is registered (optional)
  # foreach::getDoParRegistered()
  # foreach::getDoParWorkers()
  # set number of threads for data.table
  setDTthreads(threads = n_cores)
}

foreach(county_seq = county_numbers, .verbose = T, .combine = 'c',
        .options.future = list(packages = c('apsimx','berryFunctions','broom','data.table','dplyr','ggplot2',
                    'graphics','lubridate','magrittr','pracma','R.utils','readxl','sf',
  'soilDB','soiltexture','stringr','tidyr','tictoc','tidyverse','XML','xml2')
  , globals = structure(TRUE, add = c('master_path', 'results_folder', 'Glade', 'Test', 'del_input_files',
                                      'run_Daycent','run_LDNDC','run_Millennial')))) %dofuture% {
    
# foreach(county_seq = county_numbers, .verbose = T, .combine = 'c',
#         .packages=c('apsimx','berryFunctions','broom','data.table','dplyr','ggplot2',
#                     'graphics','lubridate','magrittr','pracma','R.utils','readxl','sf',
#                     'soilDB','soiltexture','stringr','tidyr','tictoc','tidyverse','XML','xml2')) %dopar% {

  print(paste0("county_seq is: ", county_seq))
  
  # x = the number of counties to run
  county_number<-county_seq
  
  tic()
  
  # Open a connection to stderr
  sink(stderr(), type = "message")
  # Print an error message to stderr
  cat(paste0("Starting county ", county_number, "\n"), file = stderr(), append = TRUE)
  

  setwd(master_path)
  
  # master_path <- home_folder

  county_data<-read.csv(file.path('Data', 'County_start', 'county_centroids_elevation.csv'))
  


  
  if(identical(Test, TRUE)){
    county_data<-county_data%>%
      filter(GEOID %in% c(1075, 13023, 13213))
  }

  # county_data<-county_data[county_data$GEOID==county_number,]
  county_data<-county_data[county_number,]
  
  #  -- add this to each parameter here so that it's available down the line
  # https://stackoverflow.com/questions/63241209/object-not-found-in-foreach-loop

  # county_geoid<-sprintf("%05d", county_data$GEOID) # Use 5 character GEOID?
  # county_geoid<-paste0("_", county_data$GEOID, "_") # Use 5 character GEOID?, or put _ in front and behind to isolate for pattern matching
  county_geoid<-county_data$GEOID # adapting 4 and 5 character geoids isn't necessary because we're always using paste statements with _ in front and behind
  county_name<-county_data$NAMELSAD
  state_name<-county_data$State_Name
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
  
  latitude = county_data$Lat
  longitude = county_data$Long
  elevation_m = county_data$Elev_mean_m
  
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
  
  # end time
  toc()
  run_time <- round(toc(echo=T)/60,1)
  print(paste0("Run time is ",run_time," minutes, ",run_time/60," hours."))
  
  # rm(list = ls()) # clear the workspace
  gc() # garbage collection, to save RAM

  return(NULL) # adding this so that the foreach loop returns something

  } # END OF DOPAR COUNTY LOOP



if(identical(run_parallel, TRUE)){
  cat("closing the cluster\n")
  # #close the cluster--------------------
  # #setDTthreads(threads = n_threads)
  parallel::stopCluster(cl = my.cluster)

}






