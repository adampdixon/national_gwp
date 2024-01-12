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
library(tictoc) # for timing
library(data.table) # for fwrite
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)

cat("********************************\n")
county_number<-args[2]
# Use the variable
cat(paste0("The county_number is: ", county_number, "\n"))
cat("********************************\n")
cat("******** FLAGS etc *****************\n")
Test <- TRUE # if TRUE, only run 3 counties, filtered below
del_input_files<-FALSE

run_Daycent=TRUE
run_LDNDC=FALSE
run_Millennial=FALSE
county_numbers<-296:306   #295:3100
cat("************************************\n")
cat("************************************\n")


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

# x = the number of counties to run
# county_number<-args[1]

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
   filter(GEOID %in% c(1075, 13023, 13213, 20073, 31181, 42053))
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
gc() # garbage collection, to save RAM fingers crossed




