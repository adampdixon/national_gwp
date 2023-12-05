######################################################
# This script gathers the csvs for each US county from historic climate data (1951-2021) and creates a table
# for Daycent 
# A Dixon
# Dec 4, 2023
###########################

######################################################
# parallel script will create 6 tables, one for each variable and model (nclim, cmip6)
######################################################
climate_data<-function(county_number){
  library(dplyr) # for piping & tibble
  
  # county data to link
  geo_link_dir<-'/glade/u/home/apdixon/Documents/national_gwp/Data/County'
  output_dir<-'/glade/work/apdixon/climate'
  nclim_dir<-'/glade/work/apdixon/Output_nClimGrid'
  cmip6_dir<-'/glade/work/apdixon/Output_climate'
  
  # geo_link_dir<-'/home/ap/Documents/GitHub/national_gwp/Data/County'
  # output_dir<-'/home/ap/Scratch'
  # nclim_dir<-'/home/ap/Documents/GitHub/national_gwp/climate_nclim'
  # cmip6_dir<-'/home/ap/Documents/GitHub/national_gwp/climate_cmip6'
  
  # add geolink table to make GEOIDS align with census GEOID
  geo_link<-read.csv(file.path(geo_link_dir, 'county_geoid_link.csv'))%>%
    select(zh_geoid, REAL_GEOID)%>%
    as_tibble()
  
  # GET HISTORIC DATA
  for(var in c('prcp','tmax','tmin')){
  # if(var=='prcp'|var=='tmax'|var=='tmin'){

    # nclim_dir<-'/home/ap/Scratch/'
    # read csvs
    data_raw<-list.files(nclim_dir, pattern = var, full.names = T)
    data<-data_raw[grep('.csv', data_raw)]
                   
    for(year in data){
      #read county csv for each year into data.frame, get county_number from out of function list
      data_df<-read.csv(year)%>%as_tibble()%>%filter(GEOID==county_number)
      # add the variable name to column, table will be long format
      data_df$variable<-var
      data_df$model<-'nclim'
      # create col names that are usable across variables
      colnames(data_df)<-c('GEOID', 'value', 'year', 'doy', 'variable','model')

      # join with the GEOID given by Zhuonan
      data_df_<-left_join(data_df, geo_link, by = c('GEOID' = 'zh_geoid'))
      
      # drop Zhuonan geoid
      data_df_<-select(data_df_, -GEOID)
      
      # add census GEOID
      data_df_$GEOID<-data_df_$REAL_GEOID
      
      # get GEOID as item for file name
      GEOID<-data_df_$GEOID[1]
      
      # use select to put in columns in correct order
      data_df_<-select(data_df_, GEOID, value, year, doy, variable, model)
      write.csv(data_df_, file.path(output_dir, paste0(var,"_", GEOID ,'_nclim.csv')))
  }
}
  
  # GET FUTURE DATA
  for (var in c('pr','tasmax','tasmin')){
    # read csvs #get only ssp126 scenario
    data_raw<-list.files(cmip6_dir, pattern = paste0(var, '_ssp126'), full.names = T) #get correct variable
    data<-data_raw[grep('gfdl-esm4', data_raw)] # get gfdl-esm4
    data<-data_raw[grep('.csv', data_raw)] # get only csvs
    
    for (year in data){
      # translate from geo_link, and use for file name at end
      GEOID_<-geo_link%>%filter(zh_geoid==county_number)
      GEOID2<-GEOID_$REAL_GEOID
      
      #read multiple csvs into data.frame
      data_df<-read.csv(year)%>%as_tibble()%>%filter(GEOID==GEOID2)
      # add the variable name to column, table will be long format
      # use if statements to convert cmip6 naming convention to nclims so they align later
      if(var=='pr'){
        var2='prcp'
      }
      if(var=='tasmax'){
        var2='tmax'
      }
      if(var=='tasmin'){
        var2='tmin'
      }
      data_df$variable<-var2
      data_df$model<-'cmip6_gfdl-esm4'
      # create col names that are usable across variables
      # GEOID,tasmin,year,doy
      # change actual data column to value for long table format
      colnames(data_df)<-c('GEOID', 'value', 'year', 'doy', 'variable','model')
      
      write.csv(data_df, file.path(output_dir, paste0(var2,"_", GEOID2 ,'_cmip6.csv')))
    }
  }
}


###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T) # not needed?
# use 7 cores, one for main processing, and one for the 6 variables
cl<-makeCluster(ncores-1)
tic()
county_number<-1:3109 # number of US counties in CONUS
clim<-clusterApply(cl, county_number, climate_data)
stopCluster(cl)
toc()
######################################