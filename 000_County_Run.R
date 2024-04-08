######################################################
# This script sets up variables to run Daycent for a single county. The process is then parallelized using a bash script.
# 
# A Dixon
# March 21, 2024
###########################

library(tictoc) # for timing
library(data.table) # for fwrite fread
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)

cat("********************************\n")

# Use the variable

cat("********************************\n")
cat("******** FLAGS etc *****************\n")
del_input_files<-FALSE

results_only=FALSE # only results, works for Daycent 

run_ag_models=TRUE
compile_input_data_4visualizations=TRUE

run_Daycent=FALSE 
run_LDNDC=TRUE
run_Millennial=FALSE
# county_numbers<-296:306   #295:3100
# crops_ <- c('Maize', 'Soybean', 'Wheat', 'Cotton', 'Rotation') # Crops
crops_ <- c('Maize', 'Soybean', 'Wheat', 'Cotton', 'Rotation') # Crops
# These variables are implemented in 0_Controller2_County.R
# mgmt_scenario_nums <- 1:1 # Management scenarios
mgmt_scenario_nums <- 1:6 # 1:6 Management scenarios 1:6

# climate scenarios
clim_nums <- 1:2 #c(1:2), can be 1:2




cat("************************************\n")
cat("************************************\n")


# Set workspace
if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    master_path<-'/home/ap/Documents/GitHub/national_gwp'
    results_folder<-'/home/ap/Documents/national_gwp_results'
    args=(commandArgs(TRUE))
    county_number<-1

    Test <- TRUE # if TRUE, only run county, filtered below
    # crop<- "Maize"   #Maize #Soybeans", "Wheat", "Cotton
    Glade=FALSE
    print("************************************")
    print("*****Using linux mint *********")
    cat("date and time are ")
    print(Sys.time())
  } else {
    master_path<-'/glade/derecho/scratch/apdixon/national_gwp'
    results_folder<-'/glade/derecho/scratch/apdixon/national_gwp_results'
    Test <- FALSE # if TRUE, only run county, filtered below
    args=(commandArgs(TRUE))
    county_number = args[2]
    Glade=TRUE
    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
    cat("date and time are ")
    print(Sys.time())
  }
}


cat(paste0("The county_number is: ", county_number, "\n"))


# x = the number of counties to run
# county_number<-args[1]

tic()

# Open a connection to stderr
sink(stderr(), type = "message")
# Open a connection to stdout
sink(stdout(), type = "message")

# master_path <- home_folder
county_data<-read.csv(file.path(master_path, 'Data', 'County_start', 'county_centroids_elevation_crops.csv'))

if(identical(Test, TRUE)){
  county_data<-county_data%>%
    filter(GEOID %in% c(31181)) #13023, 13213, 20073, 31181, 42053, 1075
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


# if (length(list.files(paste0(results_folder, "/Results_GEOID_",  county_geoid, "_", state_name), pattern = "Annual_results_compilation")) >= 7) {
#   print("************************************")
#   print("*****Results already exist *********")
#   print(paste0("Skipping county ", county_number, " ", state_name), file = stderr(), append = TRUE)
#   print("************************************")
#   stop()
# } else {
  # Print an error message to stderr
  cat(paste0("Starting county ", county_number, "\n"), file = stderr(), append = TRUE)
  
  setwd(master_path)
  

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
# }
  
  


cat("************************************\n")
cat("DELETE INPUT FILES???\n")
if(identical(del_input_files, TRUE)){
 cat("Deleting input files\n")
 unlink(daycent_path2, recursive = T)
 unlink(dndc_path, recursive = T)
} else{
 print("Saving input data files")}
cat("************************************\n")

# end time
toc()
run_time <- round(toc(echo=T)/60,1)
print(paste0("Final run time is ",run_time," minutes"))




