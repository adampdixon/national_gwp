#######################################
# Script: get_input_clim_4Vis_County.R
# Author: Adam Dixon
# Date: June 2024
# Output: .csv
# Description: This creates yearly averages of climate data for each county and outputs csv. The
# data can then be used for exploration/visualization later.
# Note for use on NCAR, the script will sample 40% of the counties to save time.
#######################################



if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    source('/home/ap/Documents/GitHub/national_gwp/000_Workspace_Dirs.R', local = TRUE)
    Test=TRUE
  } else {
    source('/glade/derecho/scratch/apdixon/national_gwp/000_Workspace_Dirs.R', local = TRUE)
    Test=FALSE
  }
}

library(dplyr)
library(data.table)
library(lubridate)

date<-gsub("-", "", Sys.Date())

county_data<-read.csv(file.path(master_path, 'Data', 'County_start', 'county_centroids_elevation_crops.csv'))%>%
  select(GEOID, NAME, State_Name)
# county_data<-county_data[county_data$GEOID==county_number,]
# county_data<-county_data[county_number,]

if(identical(Test, TRUE)){
  county_data<-county_data%>%
    filter(GEOID %in% c(31181, 13023, 13213, 20073, 31181, 42053, 1075))
} else {
  sample_percent<-.4
  print('note: sampling climate from 40% of counties')
  sample_number<-floor(nrow(county_data)*.4)
  county_data<-county_data[sample(nrow(county_data), sample_number), ]
}

setwd(master_path)

county_climate_low_change<-data.frame()
county_climate_high_change<-data.frame()


  # Get average value per year for each county
  # put table together


for (county_geoid in county_data$GEOID){
      tryCatch(
        expr = {
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
                summarise_at(vars(tmax, tmin, precip), list(mean = mean, sd = sd))
              
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
                summarise_at(vars(tmax, tmin, precip), list(mean = mean, sd = sd))
              
              add_df<-data.frame(GEOID = county_geoid, State = state)
              
              weather3<- cbind(add_df, weather2)
              
              # weather is outout of source file
              county_climate_high_change<-rbind(county_climate_high_change, weather3)
            }
            
          }
          
        },
        error = function(e){ 
          
          weather2<-data.frame(GEOID = county_geoid, State =  NA, year =NA, tmax = NA, tmin = NA, precip = NA)
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
      
}



fwrite(county_climate_low_change, file.path(national_figs, paste0('county_climate_low_change_', date, '.csv')))

fwrite(county_climate_high_change, file.path(national_figs, paste0('county_climate_high_change_', date, '.csv')))

print('saving csvs to national_figs')
print(national_figs)



# create histogram

# create box plots







