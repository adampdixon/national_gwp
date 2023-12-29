
#######################################
# File: "county_centroids.R"
# Author: "Adam Dixon"
# Date: "November 11, 2023"
# Description: This script generates lat long columns for a spatial file. Current use
# is to add to a file that already has elevation data.
#  Potential updates include adding  download of CONUS counties and average elevation
# analysis. At present this was completed in QGIS because it was easier. Adding lat long
# here because accessing the centroids of each county polygon is easier in R.
# Elevation data downloaded from https://www2.nrel.colostate.edu/projects/irc/public/Documents/Software/Century5/Reference/index.html
# US County shapefile downloaded from census.gov for 2021
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php
# 
#######################################


library(sf)
library(dplyr)


counties<-st_read('/home/ap/Elevation/us_County_avg_elevation.shp')
states<-st_read('/home/ap/Elevation/tl_2022_us_state.shp')%>%
  mutate(State_Name = NAME)%>%
  select(STATEFP, State_Name)%>%
  st_drop_geometry()
  

county_centroid<-data.frame()
counter<-0
for (i in 1:nrow(counties)){
  county_geoid<-counties[i,]$GEOID
  centroid<-st_centroid(counties[i,]$geometry)
  county_centroid<-rbind(county_centroid, cbind(county_geoid, centroid))
  counter<-counter+1
  print(paste0(counter, " ", counties[i,]$NAMELSAD))
}

names(county_centroid)<-c('GEOID', 'geometry')

county_centroid$GEOID<-as.character(county_centroid$GEOID)

county_centroid2<-counties%>%
  st_drop_geometry()%>%
  as_tibble()%>%
  right_join(county_centroid, by='GEOID')%>%
  left_join(states, by=c('STATEFP'='STATEFP'))%>%
  mutate(Elev_mean_m = round(X_mean,0), 
         Elev_cell_count = X_count,
         County_center_Long = unlist(county_centroid$geometry)[1][[1]],
         County_center_Lat = unlist(county_centroid$geometry)[2][[1]])%>%
  select(GEOID, NAME, NAMELSAD, State_Name, Elev_mean_m, Elev_cell_count, 
         County_center_Long, County_center_Lat)
  

write.csv(county_centroid2, '/home/ap/Elevation/county_centroids.csv')



