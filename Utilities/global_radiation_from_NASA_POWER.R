# Download NASA POWER data for radiation

library(dplyr)

source("/home/ap/Documents/GitHub/national_gwp/Utilities/nasapower_download.R") #creates function locally

geoids<-read.csv('/home/ap/Documents/GitHub/national_gwp/Data/County_start/county_centroids_elevation_crops.csv')


for (g in geoids$GEOID[1:50]){
  
  print(g)
  geoid<-filter(geoids, GEOID==g)
  wth_path<-'/home/ap/NASA_POWER2/'
  nasapower_output_filename<-paste0("_",geoid$GEOID, '_nasapower_output.csv')
  longitude<-geoid$Long
  latitude<-geoid$Lat
  experiment_start_date<-'2022-01-01'
  experiment_end_date<-'2022-12-31'
  
  nasapower_download(path = wth_path, # where to put the data
                     filename = nasapower_output_filename, # what to call the file it generates
                     location = c(longitude, latitude),
                     start_end_date = c(experiment_start_date, experiment_end_date))
}

# wth_path<-'/home/ap/NASA_POWER/'
# nasapower_output_filename<-'nasapower_output.csv'
# longitude<-geoids$Long
# latitude<-geoids$Lat
# experiment_start_date<-'2022-01-01'
# experiment_end_date<-'2022-12-31'

# nasapower_download(path = wth_path, # where to put the data
#                    filename = nasapower_output_filename, # what to call the file it generates
#                    location = c(longitude, latitude),
#                    start_end_date = c(experiment_start_date, experiment_end_date))
# 
# Raw_NASA <- read_csv(paste0(wth_path,"/",nasapower_output_filename), 
#                      col_names = T, show_col_types = F)
# Clean_NASA <- Raw_NASA