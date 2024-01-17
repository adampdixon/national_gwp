# Get data for national mapping

library(sf)
library(dplyr)
library(tigris)
options(tigris_use_cache = TRUE)

home_dir<-'/glade/derecho/scratch/apdixon/national_gwp/Data/County_start'

lower_48<-sprintf("%02d" , c(1,4,5,6,8,9,10,11,12,13,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
                             30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56))

us_counties_48<-counties()%>%
  filter(STATEFP %in% lower_48)%>%
  select(STATEFP, GEOID, NAME)%>%
  st_transform(5070)

st_write(us_counties_48, file.path(home_dir, "county_shp/tiger_counties_lower_48.shp"), overwrite=T)

# GET NATIONAL CDL
url <- "https://www.nass.usda.gov/Research_and_Science/Cropland/Release/datasets/2017_30m_cdls.zip"

file_home<-'/glade/derecho/scratch/apdixon/CDL_national'

dir.create(file_home)
destination<- file.path(file_home, "2017_30m_cdls.zip")
download.file(url, destination) 
unzip(destination, exdir = file_home)