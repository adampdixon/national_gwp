######################################################
# This script sets up variables to run Daycent for a single county. The process is then parallelized using a bash script.
# 
# A Dixon
# March 21, 2024
###########################

library(tictoc) # for timing
library(data.table) # for fwrite fread
library(dplyr)

# for pulling in county number from batch script
args <- commandArgs(trailingOnly = TRUE)

cat("********************************\n")
cat("********************************\n")
cat("******** FLAGS etc *****************\n")
del_input_files<-TRUE # deletes intermediate files created in national_gwp directory. Important for file size management.
results_only=FALSE # only results, works for Daycent
run_Daycent=TRUE 
run_LDNDC=TRUE
run_Millennial=TRUE
cat("********************************\n")
cat("********************************\n")

# county_numbers<-296:306   #295:3100
crops_ <- c('Maize', 'Soybean', 'Wheat', 'Cotton', 'Rotation') # Crops
# crops_ <- c('Maize') # Crops


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
    
    county_number<-1 # county number is only variable that changes
    
    Test <- TRUE # if TRUE, only run county, filtered below
    # crop<- "Maize"   #Maize #Soybeans", "Wheat", "Cotton
    Glade=FALSE # Save on Glade file system?
    print("************************************")
    print("*****Using linux mint *********")
    cat("date and time are ")
    print(Sys.time())
  } else {
    master_path<-'/glade/derecho/scratch/apdixon/national_gwp'
    results_folder<-'/glade/derecho/scratch/apdixon/national_gwp_results_2024_June'

    Test <- FALSE # if TRUE, only run county, filtered below
    args=(commandArgs(TRUE))
    
    county_number = args[2] # pulls in from batch script
    
    Glade=TRUE # Save on Glade file system?
    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
    cat("date and time are ")
    print(Sys.time())
  }
}

tic()

cat(paste0("The county_number is: ", county_number, "\n"))


setwd(master_path)

# for debugging, create a log file
# Open a connection to stderr
sink(stderr(), type = "message")
# Open a connection to stdout
sink(stdout(), type = "message")

# County data contains lat/long, elevation, and other important variables
county_data<-read.csv(file.path(master_path, 'Data', 'County_start', 'county_centroids_elevation_crops.csv'))%>%
  filter(State_Name == 'South Dakota')

# These GEOIDs were the test counties. They are spread around the US. Georgia, Kansas, Nebraska, Pennsylvania, etc.
if(identical(Test, TRUE)){
  county_data<-county_data%>%
    filter(GEOID %in% c(13213)) #13023, 13213, 20073, 31181, 42053, 1075
}

# county_data<-county_data[county_data$GEOID==county_number,]
county_data<-county_data[county_number,]

# pulling geoid and county names
county_geoid<-county_data$GEOID # adapting 4 and 5 character geoids isn't necessary because we're always using paste statements with _ in front and behind
county_name<-county_data$NAMELSAD
state_name<-county_data$State_Name

cat(paste0("Starting county ", county_number, "\n"), file = stderr(), append = TRUE)



print("************************************")
print(paste0("county geoid is: ", county_geoid))
print(paste0("county name is: ", county_name))
print(paste0("state name is: ", state_name))
print("************************************")

# site_id <- county_data$GEOID
# site_name <- paste0(gsub(" ", "_", county_data$NAMELSAD[i]),"_", gsub(" ", "_", county_data$State_Name[i]))
.GlobalEnv$site_name <- paste0("GEOID_", county_data$GEOID, "_", gsub(" ", "_", county_data$State_Name))

print("************************************")
print('working directory is: ')
print(getwd())
print("************************************")

latitude = round(county_data$Lat, 5) # arbitrarily rounding during LDNDC debugging, shouldn't matter
longitude = round(county_data$Long, 5)
elevation_m = county_data$Elev_mean_m

print(paste0("latitude is: ", latitude))
print(paste0("longitude is: ", longitude))
print(paste0("elevation_m is: ", elevation_m))

source('00_Main_County.R', local = TRUE)

cat("************************************\n")
cat("DELETE INPUT FILES???\n")
if(identical(del_input_files, TRUE)){
 cat("Deleting input files\n")
 unlink(daycent_path2, recursive = T)
 unlink(dndc_path, recursive = T)
 unlink(mill_path, recursive = T)
} else{
 print("Saving input data files")}
cat("************************************\n")

# end time
toc()
run_time <- round(toc(echo=T)/60,1)
print(paste0("Final run time is ",run_time," minutes"))




