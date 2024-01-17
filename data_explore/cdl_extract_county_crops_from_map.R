# Tabulate area of major crops in CDL

# for each county extract corn, soy, cotton, wheat and count number of pixels

library(tigris)
options(tigris_use_cache = TRUE)
library(terra)
library(sf)
# library(doFuture)
# plan(multisession, workers=3)
# library(foreach)
library(tictoc)
library(dplyr)
library(parallel)


home_dir<-'/glade/derecho/scratch/apdixon/national_gwp/Data/County_start'

lower_48<-sprintf("%02d" , c(1,4,5,6,8,9,10,11,12,13,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
                             30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56))

us_counties_48<-counties()%>%
  filter(STATEFP %in% lower_48)%>%
  select(STATEFP, GEOID, NAME)%>%
  st_transform(5070)

st_write(us_counties_48, file.path(home_dir, "county_shp/tiger_counties_lower_48.shp"))

# GET NATIONAL CDL
url <- "https://www.nass.usda.gov/Research_and_Science/Cropland/Release/datasets/2017_30m_cdls.zip"

file_home<-'/glade/derecho/scratch/apdixon/CDL_national'

create.dir(file_home)
destination<- file.path(file_home, "2017_30m_cdls.zip")
download.file(url, destination) 
unzip(destination, exdir = file_home)

# home_dir<-'/home/ap/Documents/GitHub/national_gwp'

get_cdl_area<-function(county_number){
  library(terra)
  library(sf)
  library(dplyr)
  
  CDL<-rast("/glade/derecho/scratch/apdixon/CDL_national/2017_30m_cdls.tif")
  # CDL<-rast("/home/ap/CDL/2017_30m_cdls.tif")
  # counties<-st_read(file.path('/home/ap/Documents/GitHub/national_gwp/Data/County_start/county_shp/tiger_counties_lower_48.shp'))%>%
  #   select(GEOID, NAME)%>%
  #   st_transform(5070)
  
  counties<-st_read(file.path('/glade/derecho/scratch/apdixon/national_gwp/Data/County_start/county_shp/tiger_counties_lower_48.shp'))%>%
    select(GEOID, NAME)%>%
    st_transform(5070)
  
  cdl_county<-terra::extract(CDL, counties[county_number,])
  print(cdl_county)
  
  # Get area of each crop
  cdl_area<-cdl_county%>%
    group_by(Class_Names)%>%
    summarise(count=sum(ID))%>%
    filter(Class_Names %in% c("Corn", "Soybeans", "Cotton", "Wheat"))%>%
    mutate(Acres=count*0.0074131614)
  
  county_name<-counties[county_number,]$NAME
  county_geoid<-counties[county_number,]$GEOID
  
  Corn<-cdl_area$Acres[cdl_area$Class_Names=="Corn"]
  Cotton<-cdl_area$Acres[cdl_area$Class_Names=="Cotton"]
  Soybeans<-cdl_area$Acres[cdl_area$Class_Names=="Soybeans"]
  Wheat<-cdl_area$Acres[cdl_area$Class_Names=="Wheat"]
  
  Corn2<-ifelse(length(Corn)>0, Corn, 0)
  Cotton2<-ifelse(length(Cotton)>0, Cotton, 0)
  Soybeans2<-ifelse(length(Soybeans)>0, Soybeans, 0)
  Wheat2<-ifelse(length(Wheat)>0, Wheat, 0)
  
  cdl_data<-c(county_name, county_geoid, Corn2, Cotton2, Soybeans2, Wheat2)
  
  return(cdl_data)
}


###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T)
cl<-makeCluster(ncores-1)
tic()
county_number<-c(1:100)
county_cdl<-clusterApply(cl, county_number, get_cdl_area)
stopCluster(cl)
toc()
######################################


cdl_df<-data.frame()
for (i in 1:length(county_cdl)){
  cdl_df<-rbind(cdl_df, county_cdl[[i]])
  
}

names(cdl_df)<-c("county_name", "county_geoid", "Corn_Acres", "Cotton_Acres", "Soybeans_Acres", "Wheat_Acres")

write.csv(cdl_df, file.path(home_dir, paste0('county_cdl_',min(county_number),"_",max(county_number),'.csv')))






