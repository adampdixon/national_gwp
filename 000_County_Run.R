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


# Set workspace
if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    master_path<-'/home/ap/Documents/GitHub/national_gwp'
    results_folder<-'/home/ap/Documents/national_gwp_results'
    
    args=(commandArgs(TRUE))
    
    county_number<-1 #args[2] # county number changes row selected in county_data
    
    Test <- TRUE # if TRUE, only run county, filtered below
    # crop<- "Maize"   #Maize #Soybeans", "Wheat", "Cotton
    Glade=FALSE # Save on Glade file system?
    print("************************************")
    print("*****Using linux mint *********")
    cat("date and time are ")
    print("************************************")
    print(Sys.time())
  } else {
    master_path<-'/glade/derecho/scratch/apdixon/national_gwp'
    results_folder<-'/glade/derecho/scratch/apdixon/national_gwp_results'
    
    Test <- FALSE # if TRUE, only run county, filtered below
    args=(commandArgs(TRUE))
    
    county_number = args[2] # pulls in from batch script
    
    Glade=TRUE # Save on Glade file system?
    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
    cat("date and time are ")
    print("************************************")
    print(Sys.time())
  }
}

setwd(master_path)


# Set up county level data
################################################################################
# County data contains lat/long, elevation, and other important variables
# Note: Counties removed because of no crop data. See county notes.
county_data<-read.csv(file.path(master_path, 'Data', 'County_start', 'county_centroids_elevation_crops.csv'))#%>%
# filter(State_Name == 'South Dakota') # For debugging, only run South Dakota counties


# These GEOIDs were the test counties. They are spread around the US. Georgia, Kansas, Nebraska, Pennsylvania, etc.
if(identical(Test, TRUE)){
  county_data<-county_data%>%
    filter(GEOID %in% c(32009, 32001)) # 1075
}

# 4012, 8113
# 5007, 4009, 5021, 4015
# 4011, 6113, 9001, 13103, 16067
# 31181, 13023, 13213, 20073, 42053

# Get first row in table
# county_data<-county_data[county_data$GEOID==county_number,]
county_data<-county_data[county_number,]

# pulling geoid and county names
county_geoid<-county_data$GEOID # adapting 4 and 5 character geoids isn't necessary because we're always using paste statements with _ in front and behind
county_name<-county_data$NAMELSAD
state_name<-county_data$State_Name

site_name <- paste0("GEOID_", county_data$GEOID, "_", gsub(" ", "_", county_data$State_Name))
################################################################################

#create results folder if it doesn't already exist
results_path <- file.path(results_folder, paste0("Results_", site_name))
if(!dir.exists(results_path)) dir.create(results_path)

# For figs and data to save
figs_input_data<-file.path(results_path, 'data_and_figs')
dir.create(figs_input_data, showWarnings = FALSE)

if(identical(Glade, TRUE)){
  # If running on Glade, create a log file that saves all console outputs
  # If statement is because otherwise the output to local console is diverted to just the file.
  # create a log file that saves all console outputs
  con <- file(file.path(figs_input_data, paste0("1_", site_name, "_R_console_out.log")))
  sink(con)
}

tic()

cat("********************************\n")
cat("********************************\n")
cat("******** FLAGS etc *****************\n")
del_input_files<-TRUE # deletes files that results tables are made from in national_gwp directory. Important for directory space management when saving files to GLADE.
                      # helpful to have though when in development
                      # Note: files are only deleted upon successful model completion. Use 10_delete_model_files.R when need to clear out faulty model data.
results_only=FALSE # only results, works for Daycent and LDNDC, note all run flags must be opposite of this TRUE or FALSE. Useful for debugging.
data_plots=TRUE # county level climate and results plots

# Run flags for models. If all FALSE, then input climate and soil data will be loaded in global environment.
run_Daycent=TRUE 
run_LDNDC=TRUE
run_Millennial=TRUE

cat("********************************\n")
cat("********************************\n")

# county_numbers<-296:306   #295:3100
# crops_ <- c('Maize', 'Soybean', 'Wheat', 'Cotton', 'Rotation') # Crops
crops_ <- c('Maize') # Crops

# mgmt_scenario_nums <- 1:1 # Management scenarios
mgmt_scenario_nums <- 1:1 # 1:6 Management scenarios 1:6

# climate scenarios
clim_nums <- 1:1 #c(1:2), can be 1:2

cat("************************************")
cat("************************************\n")
cat("************************************\n")
cat(paste0("The county_number is: ", county_number, "\n"))
cat(paste0("Starting county ", county_number, "\n"), file = stderr(), append = TRUE)
county_print_marker<-paste("county geoid, name, stat is:", county_geoid, county_name, state_name)
cat(county_print_marker)
cat('working directory is: ')
cat(getwd())
cat("************************************")
cat("************************************")
cat("************************************")

latitude = round(county_data$Lat, 5) # arbitrarily rounding during LDNDC debugging, shouldn't matter
longitude = round(county_data$Long, 5)
elevation_m = county_data$Elev_mean_m

print(paste0("latitude is: ", latitude))
print(paste0("longitude is: ", longitude))
print(paste0("elevation_m is: ", elevation_m))

### ### ### ### ### ### ### ### ### ### ### 
### START MODELS
source('00_Main_County.R', local = TRUE)
### ### ### ### ### ### ### ### ### ### ### 

cat("************************************\n")
cat("DELETE INPUT FILES???\n")
if(identical(del_input_files, TRUE)){
 cat("Deleting input files\n")
  if(identical(run_Daycent, TRUE)) {
    unlink(daycent_path2, recursive = T)
  }
 if(identical(run_LDNDC, TRUE)) {
   unlink(dndc_path, recursive = T)
 }
  if(identical(run_Millennial, TRUE)) {
    unlink(mill_path, recursive = T)
  }
} else{
 print("Saving input data files")}
cat("************************************\n")
print(county_print_marker) # print out for debugging with console print out
# end time
toc()
run_time <- round(toc(echo=T)/60,1)
print(paste0("Final run time is ",run_time," minutes"))


if(identical(Glade, TRUE)){
  sink() # close console output
  sink(type="message") # restore to console
}





