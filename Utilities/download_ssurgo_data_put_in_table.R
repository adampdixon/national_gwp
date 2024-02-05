#######################################
# Function: "Sample county and download SSURGO data"
# Author: "Adam Dixon"
# Date: "January 25, 2024"
# Description: Downloads the closest SSURGO soil profile for the site 
#######################################
#######################################

library(apsimx)
library(sf)
library(dplyr)
library(terra)
library(units)
library(tictoc)


# library(tidyverse)
# library(soiltexture)
# library(xml2)
# library(lubridate)
# library(soilDB)
# 
# library(stringr)


# download soil data from SSURGO for the lat/lon into a list of "soil.profile"
# classes, pre-formatted for APSIM
county_data<-'/home/ap/Documents/GitHub/national_gwp/Data/County_start'
county_shp<-'/county_shp/ne_conus_counties.shp'
cultivated<-'/home/ap/CDL/2022_cultivated_layer.tif'
cultivated_tiles_path<-'/home/ap/CDL/cultivated_tiles'
output_path<-'/home/ap/CDL/soil_data'


# load cropland data layer
cdl_cultivated<-rast(cultivated)

# load county spatial data
county<-st_read(file.path(county_data, county_shp))%>%
  mutate(GEOID = as.numeric(CODE_LOCAL),
         state = REGION)%>%
  select(GEOID, state)%>%
  st_transform(crs(cdl_cultivated))

# get county raster cultivated layer

tiles<-rast(ncol = 10, nrows = 10, extent = ext(cdl_cultivated), crs = crs(cdl_cultivated))
values(tiles)<-1:ncell(tiles)
# plot(tiles)
# plot(county$geometry, add = T)
# cultivated_tiles<-terra::makeTiles(x = cdl_cultivated, y = tiles, filename = file.path(cultivated_tiles_path, '_2022_cultivated_layer.tif'),extend = T, na.rm = T)

# Currently there are tiles from 1 to 25
list_tiles<-list.files(cultivated_tiles_path, full.names = T)
list_tiles_names<-list.files(cultivated_tiles_path, full.names = F)

# average_county_size<-312870.564 #hectares
# min_samples<-100


# Get the value from the raster tile corresponding to location of county



for (i in 1:3108){ #1:3108 nrow(county)
  tic()
  
  tryCatch(
    expr = {
    county_sub<-county[i,]
    county_sub_v<-vect(county[i,]) # covert to spatvect
    # plot(county_sub_v)
    
    GEOID<-county_sub$GEOID
    print(paste0('starting GEOID ', GEOID, " ", i, " of ", nrow(county)))
    
    
    final_path<-file.path(output_path, paste0('GEOID_', GEOID ,'_county_soil_data_summary.csv'))
    
    if(!file.exists(final_path)){
      tile_location<-terra::extract(tiles, county_sub_v)
      # plot(tiles)
      # plot(county_sub, add=T)
      
      which_one<-grep(paste0('_layer',tile_location$lyr.1[1],'.tif$'), list_tiles) # gets the location in the list
      # check if county crosses tile boundary, if so join them
      if(length(which_one)==1){
        county_cult_ras<-rast(list_tiles[which_one])
        # plot(county_ras, add=T)
      }
      if(length(which_one)>1){
        print('more than one tile needed')
        cult_tiles<-c() #create vector of tiles needed
        for(w in which_one){
          add<-list_tiles[w]
          # assign(paste0('ras',i), rast(add))
          cult_tiles<-c(cult_tiles, add)
        }
        county_cult_ras<-rast(cult_tiles)
      }
      
      # county_area<-drop_units(st_area(county_sub)*0.0001) # convert to hectares
      
      # create at least 100 sample points in county cropland, if county is really big (bigger than average) divide county area by
      # average county size and multiply by 100 to get number of samples
      # samples_per_area<-ifelse(county_area>average_county_size,county_area/average_county_size*min_samples, min_samples)
      samples_per_area<-5
      
      county_boundary_mask<-terra::crop(terra::mask(x = county_cult_ras, mask = county_sub_v), y =  county_sub_v)
      
      print('county boundary mask complete')
      # plot(county_boundary_mask)
      
      # m <- c('Background', NA,
      #        'Non-Cultivated', NA,
      #        'Cultivated', 1)
      # rclmat <- matrix(m, ncol=2, byrow=TRUE)
      # 
      # classify(county_boundary_mask, m)
      
      # county_boundary_mask2<-subst(county_boundary_mask, 'Cultivated', 9)
      # county_boundary_mask2<-subst(county_boundary_mask, 'Non-Cultivated', 11)
      # county_boundary_mask2<-subst(county_boundary_mask, 'Background', 11)
      
      
      samples<-spatSample(county_boundary_mask, size = floor(samples_per_area), method="stratified", as.points=T)
      
      print('sample points created')
      
      # convert to sf
      # cultivated value is 2
      samples_sf<-st_as_sf(samples)%>%
        filter(Class_Names==2)%>%
        st_transform(4326)
      
      # plot(county_boundary_mask)
      # plot(samples, col='red', pch=19, add=T)
      
      # get soil data for each sample point from ASPIMX
      county_soil_data<-data.frame()
      
      time_in_seconds<-.25 # time to wait between API calls
      
      for (s in 1:nrow(samples_sf)){
        latitude<-st_coordinates(samples_sf)[s,2][[1]]
        longitude<-st_coordinates(samples_sf)[s,1][[1]]
        sps_raw <- get_ssurgo_soil_profile(lonlat = c(longitude, latitude), nsoil=1)
        sps <- sps_raw[[1]]$soil
        sps$GEOID<-county_sub$GEOID
        sps$sample_numb<-s
        Sys.sleep(time_in_seconds) # give API time to recover
        
        county_soil_data<-rbind(county_soil_data, sps)
      }
      
      print('soil samples downloaded')
      
      # summarize data and save csv
      # county_soil_data_summary<-county_soil_data%>%
      #   group_by(Depth)%>%
      #   summarize_all(mean)
      
      print(paste0('Writing soil data for ', GEOID))
      # print(county_soil_data_summary)
      
      # write data
      write.csv(county_soil_data, final_path, row.names = F)
      toc()
    }else{
      print(paste0('Already have data for ', GEOID))
    }
    },
    error = function(e){
      message('Caught an error!')
      message(GEOID)
      print(e)
    },
    warning = function(w){
      message('Caught an warning!')
      print(w)
      
      tryCatch(
        expr = {
          samples<-spatSample(county_boundary_mask, size = 3, method="stratified", as.points=T)
          
          print('sample points created, getting only 3 points')
          
          # convert to sf
          # cultivated value is 2
          samples_sf<-st_as_sf(samples)%>%
            filter(Class_Names==2)%>%
            st_transform(4326)
          
          # plot(county_boundary_mask)
          # plot(samples, col='red', pch=19, add=T)
          
          # get soil data for each sample point from ASPIMX
          county_soil_data<-data.frame()
          
          time_in_seconds<-.5 # time to wait between API calls
          
          for (s in 1:nrow(samples_sf)){
            latitude<-st_coordinates(samples_sf)[s,2][[1]]
            longitude<-st_coordinates(samples_sf)[s,1][[1]]
            sps_raw <- get_ssurgo_soil_profile(lonlat = c(longitude, latitude), nsoil=1)
            sps <- sps_raw[[1]]$soil
            sps$GEOID<-county_sub$GEOID
            sps$sample_numb<-s
            Sys.sleep(time_in_seconds) # give API time to recover
            
            county_soil_data<-rbind(county_soil_data, sps)
          }
          
          print('soil samples downloaded')
          
          # summarize data and save csv
          # county_soil_data_summary<-county_soil_data%>%
          #   group_by(Depth)%>%
          #   summarize_all(mean)
          
          print(paste0('Writing soil data for ', GEOID))
          # print(county_soil_data)
          
          # write data
          write.csv(county_soil_data, final_path, row.names = F)
          toc()
        },
        error = function(e){
          message('Caught an error!')
          message(GEOID)
          print(e)
        },
        warning = function(w){
          message('Caught an warning!')
          print(w)
          },
        finally = {
          message('All done, quitting second tryCatch.')
        }
        ) # end of second embedded tryCatch
    }, # from first tryCatch statement
    finally = {
      message('All done, quitting.')
    }
  )

}



