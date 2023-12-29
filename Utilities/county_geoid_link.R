# This script was needed to link the geoid used by Zhounan to the actual census GEOID. I think 
# they weren't sure what the geoid was, but regardless this allows us to link to his climate data


library(terra)
library(dplyr)


dir<-'/home/ap/Documents/GitHub/national_gwp/Data'

conus_counties<-vect(file.path(dir, 'County', 'county_shp','ne_conus_counties.shp'))
county_raster<-rast(file.path(dir, 'County', 'county_shp', 'US_GEOID_1km.tif'))
                    
county_centers<-centroids(conus_counties)

#get the geoid used by Zhuonan 
county_geoid<-extract(county_raster, county_centers, ID = TRUE, method = 'simple', bind = TRUE)%>%
  as_tibble()

colnames(county_geoid)

county_ids<-mutate(county_geoid, zh_geoid = GEOID, REAL_GEOID = substr(FIPS, 3, 7))%>%
  select(NAME, NAME_ALT, REGION, REGION_COD, FIPS, zh_geoid, REAL_GEOID)

# other_county_data<-read.csv(file.path(dir, 'County', 'county_centroids_elevation.csv'))%>%as_tibble()
# county_geoid_link<-left_join(county_ids, other_county_data, by = c('NAME' = 'county_name')

# write.csv(county_ids, file.path(dir, 'County','county_geoid_link.csv'))

