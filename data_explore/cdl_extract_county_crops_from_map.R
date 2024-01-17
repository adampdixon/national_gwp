# Tabulate area of major crops in CDL

# for each county extract corn, soy, cotton, wheat and count number of pixels
home_dir<-'/glade/derecho/scratch/apdixon/national_gwp/Data/County_start'

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
cl<-makeCluster(20)
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






