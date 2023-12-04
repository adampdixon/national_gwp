rm(list=ls())
library(terra)
library(tidyverse)
library(data.table)
#setDTthreads(threads = 32)
#getDTthreads(verbose = getOption("datatable.verbose"))
library(doParallel)

setwd("/glade/work/edvlmaas/CMIP6")

###############------------------------------------------------------------------------
USAContiguousAlbersEqualAreaConic_crs="+proj=aea +lat_0=37.5 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"
crs_target = USAContiguousAlbersEqualAreaConic_crs
US_GEOID_1km <- rast("/glade/work/zhuonan/CMIP6/US_GEOID_1km.tif")
###############------------------------------------------------------------------------
summary_Climate <- function(file){

 setDTthreads(threads = 32)
  
input <- fread(file)
  setnames(input, c('V1','V2','V3','V4','V5','V6','V7','V8','V9'),
           c('value','long','lat','unmean','day','month','year','doy','ndm')
           )
                     
years <- unique(input$year)
#create the cluster
my.cluster <- parallel::makeCluster(
  36, 
  type = "FORK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

for (yr in years) {
  input_year <-  input[year==yr, ]
  doys <- unique(input_year$doy)
allday <- foreach(dy = doys, .packages=c("tidyverse","terra","data.table"), .combine = rbind) %dopar% {
    # cat(dy,yr)
    ###############------------------------------------------------------------------------
    USAContiguousAlbersEqualAreaConic_crs="+proj=aea +lat_0=37.5 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"
    crs_target = USAContiguousAlbersEqualAreaConic_crs
    US_GEOID_1km <- rast("/glade/work/zhuonan/CMIP6/US_GEOID_1km.tif")
    ###############------------------------------------------------------------------------
    
   oneday <-  input[year==yr & doy==dy, ]
   oneday_df = as.data.frame(oneday[,c('long', 'lat', 'value'), with=FALSE])
   ## S4 method for signature 'data.frame'
   oneday_rast <- rast(oneday_df, type="xyz",crs="+proj=longlat +datum=WGS84 +no_defs +type=crs")
   names(oneday_rast) <- unlist(strsplit(file, split = "_"))[1]
   disagg_oneday_rast <- disagg(oneday_rast, 60, method="near")
   
   rast_albersEqualArea <- terra::project(disagg_oneday_rast, crs_target) #keep everything at AlbersEqualAreaConic
   day_rast <- resample(rast_albersEqualArea, US_GEOID_1km)
   
   
   zonal_df = zonal(day_rast, US_GEOID_1km, "mean", na.rm = TRUE)
   zonal_df$year=yr
   zonal_df$doy=dy
  # fwrite(zonal_df,paste0('/glade/work/zhuonan/Output_climate/', file,"_Y",yr,"_D",dy,".csv"))
  
   return(zonal_df)
  }
  
  fwrite(allday, paste0('/glade/work/zhuonan/Output_climate/',gsub("\\.", "_", file),"_Y",yr,".csv"))

}

parallel::stopCluster(cl = my.cluster)
gc()
return(0)
}
#==============================================================================================================
files=list.files()
files=list.files()[52]

for (file in files) {
  summary_Climate(file)
gc()
}


