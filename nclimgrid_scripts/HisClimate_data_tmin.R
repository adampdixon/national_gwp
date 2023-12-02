library(terra)
library(tidyverse)
library(data.table)
library(doParallel)

#setwd("/glade/cheyenne/scratch/edvlmaas/nClimGrid")
setwd("/glade/derecho/scratch/edvlmaas/nClimGrid")

###############------------------------------------------------------------------------
summary_Climate <- function(climvariable="prcp",year=1951){
  # climvariable="prcp"
  # year=2021
  #"tmin"; "tmax"; 1951-2021 # "prcp_nClimGrid_1951_01.dat"
  
  files <- paste0(paste(climvariable,"nClimGrid",year,sprintf("%02d", 1:12),sep="_"),".dat")
  
  start_time <- Sys.time()
  
 year_results <-  foreach(n = 1:12, .packages=c("tidyverse","terra","data.table"), .combine = rbind) %do% {
 
    file=files[n]
    
    setDTthreads(threads = 32)
    input <-fread(file)
    setnames(input, c('V1','V2','V3','V4','V5','V6','V7','V8','V9'),
             c('value','long','lat','unmean','day','month','year','doy','ndm'))
    
    doys <- input[,unique(doy)]
    
    #create the cluster--------------------
    my.cluster <- parallel::makeCluster(
      32,
      type = "FORK"
    )
    #register it to be used by %dopar%
    doParallel::registerDoParallel(cl = my.cluster)
    #check if it is registered (optional)
    foreach::getDoParRegistered()
    foreach::getDoParWorkers() 
 
    allday <- foreach(dy = doys, .packages=c("tidyverse","terra","data.table"), .combine = rbind) %dopar% {
      
      oneday <-  input[doy==dy, ]
      oneday_df = as.data.frame(oneday[,c('long', 'lat', 'value'), with=FALSE])
      rm(oneday)
     
      df <- fread(paste0("/glade/work/zhuonan/nclimgrid_scripts/US_GEOID/2df_latlong.csv"))
      oneday_df$long=df$x
      oneday_df$lat=rev(df$y)
      rm(df)
      ## S4 method for signature 'data.frame'
      oneday_rast <- rast(oneday_df, type="xyz",crs="+proj=longlat +datum=WGS84 +no_defs +type=crs")
      names(oneday_rast) <- climvariable
      rm(oneday_df)
     
      disagg_oneday_rast <- disagg(oneday_rast, 5, method="near")
      rm(oneday_rast)
      
      ###############------------------------------------------------------------------------
      USAContiguousAlbersEqualAreaConic_crs="+proj=aea +lat_0=37.5 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"
      crs_target = USAContiguousAlbersEqualAreaConic_crs
      #  US_GEOID_1km <- rast(paste0("/glade/work/zhuonan/nclimgrid_scripts/US_GEOID/",dy,"US_GEOID_1km.tif"))
      US_GEOID_1km <- rast(paste0("/glade/work/zhuonan/nclimgrid_scripts/US_GEOID/2US_GEOID_1km.tif"))
      ###############------------------------------------------------------------------------
      rast_albersEqualArea <- terra::project(disagg_oneday_rast, crs_target,method="near")
      rm(disagg_oneday_rast) 
      
      day_rast <- resample(rast_albersEqualArea, US_GEOID_1km, method="near")
      rm(rast_albersEqualArea)
      
      zonal_df = zonal(day_rast, US_GEOID_1km, "mean", na.rm = TRUE)
      rm(day_rast, US_GEOID_1km)
      
      zonal_df$year=year
      zonal_df$doy=dy
      return(zonal_df)
    }
    parallel::stopCluster(cl = my.cluster)
    #close the cluster--------------------
    return(allday)
  }
 
 gc()
 
 setDTthreads(threads = 32)
 fwrite(year_results, paste0('/glade/work/zhuonan/Output_nClimGrid/',paste(climvariable,"nClimGrid",year,sep="_"),".csv"))
}



for (yr in c(1957)) {
	summary_Climate(climvariable="tmin",year=yr)
}
