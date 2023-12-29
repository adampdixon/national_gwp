
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


counties<-st_read('/home/ap/Elevation/us_County_avg_elevation.shp') # ran analysis in QGIS to get average elevation for each county
states<-st_read('/home/ap/Elevation/tl_2022_us_state.shp')%>% # downloaded from census.gov
  mutate(State_Name = NAME)%>%
  select(STATEFP, State_Name)%>%
  st_drop_geometry()
  

county_centroid<-data.frame()
counter<-0
for (i in 1:nrow(counties)){
  county_geoid<-counties[i,]$GEOID
  centroid<-st_centroid(counties[i,]$geometry)
  x<-unlist(centroid)[1][[1]]
  y<-unlist(centroid)[2][[1]]
  county_centroid<-rbind(county_centroid, cbind(county_geoid, x, y))
  counter<-counter+1
  print(paste0(counter, " ", counties[i,]$NAMELSAD))
}

names(county_centroid)<-c('GEOID','x', 'y')

county_centroid$GEOID<-as.character(county_centroid$GEOID)

head(county_centroid)

county_centroid2<-county_centroid%>%
  as_tibble()%>%
  left_join(st_drop_geometry(counties), by='GEOID')%>%
  left_join(st_drop_geometry(states), by=c('STATEFP'='STATEFP'))%>%
    mutate(Elev_mean_m = round(X_mean,0),
           Elev_cell_count = X_count,
           Long = x,
           Lat = y)%>%
select(GEOID, NAME, NAMELSAD, State_Name, Elev_mean_m, Elev_cell_count, Long, Lat)


write.csv(county_centroid2, '/home/ap/Documents/GitHub/national_gwp/Data/County/county_centroids_elevation.csv')
