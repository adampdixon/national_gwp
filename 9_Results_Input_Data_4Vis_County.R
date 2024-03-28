# Visualize climate data



# Set workspace
if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    master_path<-'/home/ap/Documents/GitHub/national_gwp'
    climate_folder<-'/home/ap/Scratch'
    results_path<-'/home/ap/figs'
    # args=(commandArgs(TRUE))
    # county_number<-1
    
    Test <- TRUE # if TRUE, only run county, filtered below
    # crop<- "Maize"   #Maize #Soybeans", "Wheat", "Cotton
    Glade=FALSE
    print("************************************")
    print("*****Using linux mint *********")
    cat("date and time are ")
    print(Sys.time())
  } else {
    master_path<-'/glade/derecho/scratch/apdixon/national_gwp'
    climate_folder<-'/glade/work/apdixon/climate'
    results_path<-'/glade/derecho/scratch/apdixon/national_gwp_figs'
    Test <- FALSE # if TRUE, only run county, filtered below
    # args=(commandArgs(TRUE))
    # county_number = args[2]
    Glade=TRUE
    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
    cat("date and time are ")
    print(Sys.time())
  }
}

library(dplyr)
library(data.table)
library(lubridate)


county_data<-read.csv(file.path(master_path, 'Data', 'County_start', 'county_centroids_elevation_crops.csv'))%>%
  select(GEOID, NAME, State_Name)
# county_data<-county_data[county_data$GEOID==county_number,]
# county_data<-county_data[county_number,]

if(identical(Test, TRUE)){
  county_data<-county_data%>%
    filter(GEOID %in% c(31181)) #13023, 13213, 20073, 31181, 42053, 1075
}

setwd(master_path)

####################### climate #######################
if (identical(Glade, TRUE)){
  climate_data_path<-'/glade/work/apdixon/climate'
  print(paste0('*********climate_data_path is ', climate_data_path, " **************"))
} else {
  climate_data_path<-'/home/ap/Scratch'
  print(paste0('*********climate_data_path is ', climate_data_path, " **************"))
}
####################### ####### #######################

county_climate_low_change<-data.frame()
county_climate_high_change<-data.frame()


tryCatch(
  expr = {
    # Get average value per year for each county
    # put table together
    for (county_geoid in county_data$GEOID){
      print(county_geoid)
      
      state<-filter(county_data, GEOID==county_geoid)$State_Name
      
      for (fut_climate in c(1:2)){
        print(fut_climate)
        
        if (fut_climate==1){ # LOW CHANGE
          # create climate tables
          source('1_create_county_climate_wth_file_County.R', local = TRUE)
          
          # summarize at yearly level
          weather2<-weather%>%
            group_by(year)%>%
            summarise_at(vars(tmax, tmin, precip), mean)
          
          add_df<-data.frame(GEOID = county_geoid, State = state)
          
          weather3<- cbind(add_df, weather2)
          
          # weather is outout of source file
          county_climate_low_change<-rbind(county_climate_low_change, weather3)
          
        } 
        
        if (fut_climate==2){ # HIGH CHANGE
          # create climate tables
          source('1_create_county_climate_wth_file_County.R', local = TRUE)
          
          # summarize at yearly level
          weather2<-weather%>%
            group_by(year)%>%
            summarise_at(vars(tmax, tmin, precip), mean)
          
          add_df<-data.frame(GEOID = county_geoid, State = state)
          
          weather3<- cbind(add_df, weather2)
          
          # weather is outout of source file
          county_climate_high_change<-rbind(county_climate_high_change, weather3)
        }
        
      }
      
    }
    
  },
  error = function(e){ 
    
    weather2<-data.frame(GEOID = county_geoid, State =  NA, year =NA, day = NA, month = NA, jday = NA, tmax = NA, tmin = NA, precip = NA)
    county_climate_high_change<-rbind(county_climate_high_change, weather2)
    
    county_climate_low_change<-rbind(county_climate_low_change, weather2)
    
    
  },
  warning = function(w){
    # (Optional)
    # Do this if a warning is caught...
  },
  finally = {
    # (Optional)
    # Do this at the end before quitting the tryCatch structure...
  }
)


fwrite(county_climate_low_change, file.path(results_path, 'Data', 'County_start', 'county_climate_low_change.csv'))

fwrite(county_climate_high_change, file.path(results_path, 'Data', 'County_start', 'county_climate_high_change.csv'))



# create histogram


# create box plots







