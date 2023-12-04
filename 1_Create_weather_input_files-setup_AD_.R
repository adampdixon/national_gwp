######################################################
# This script gathers the csvs from historic climate data (1951-2021) and creates a table
# for Daycent 
# A Dixon
# Dec 4, 2023
###########################


# library(tidyr) # for pivot_wider

# setwd('/glade/work/apdixon')

# Data to be in format:
# fields<-c('GEOID', 'date', 'precip','tmax','tmin','doy')

######################################################
# parallel script will create 6 tables, one for each variable and model (nclim, cmip6)
######################################################
climate_data<-function(var){
  library(dplyr) # for piping & tibble
  
  # county data to link
  geo_link_dir<-'/glade/u/home/apdixon/Documents/national_gwp/Data/County'
  #output csvs
  output_dir<-'/glade/work/apdixon/climate'
  
  # GET HISTORIC DATA
  if(var=='prcp'|var=='tmax'|var=='tmin'){
    nclim_dir<-'/glade/work/apdixon/Output_nClimGrid'
    # nclim_dir<-'/home/ap/Scratch/'
    # read csvs
    data_raw<-list.files(nclim_dir, pattern = var, full.names = T)
    data<-data_raw[grep('.csv', data_raw)][1:2]
    #read multiple csvs into data.frame
    data_df<-do.call(rbind, lapply(data, read.csv))%>%as_tibble()
    # add the variable name to column, table will be long format
    data_df$variable<-var
    data_df$model<-'nclim'
    # create col names that are usable across variables
    colnames(data_df)<-c('GEOID', 'value', 'year', 'doy', 'variable','model')
    
    # add geolink table to make GEOIDS align with census GEOID
    geo_link<-read.csv(file.path(geo_link_dir, 'county_geoid_link.csv'))%>%
      select(zh_geoid, REAL_GEOID)%>%
      as_tibble()

    # join with the GEOID given by Zhuonan
    data_df_<-left_join(data_df, geo_link, by = c('GEOID' = 'zh_geoid'))
    
    # drop Zhuonan geoid
    data_df_<-select(data_df_, -GEOID)
    
    # add census GEOID
    data_df_$GEOID<-data_df_$REAL_GEOID
    
    # use select to put in columns in correct order
    data_df_<-select(data_df_, GEOID, value, year, doy, variable, model)
    write.csv(data_df_, file.path(output_dir, paste0(var, '_nclim.csv')))
  }
  
  # GET FUTURE DATA
  if(var=='pr'|var=='tasmax'|var=='tasmin'){
    cmip6_dir<-'/glade/work/apdixon/Output_climate'
    # read csvs #get only ssp126 scenario
    data_raw<-list.files(cmip6_dir, pattern = paste0(var, '_ssp126'), full.names = T) #get correct variable
    data<-data_raw[grep('gfdl-esm4', data_raw)] # get gfdl-esm4
    data<-data_raw[grep('.csv', data_raw)][1:2] # get only csvs
    #read multiple csvs into data.frame
    data_df<-do.call(rbind, lapply(data, read.csv))%>%as_tibble()
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
    colnames(data_df)<-c('GEOID', 'value', 'year', 'doy', 'variable','model')
    
    write.csv(data_df, file.path(output_dir, paste0(var, '_cmip6.csv')))
  }
}


###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T) # not needed?
# use 7 cores, one for main processing, and one for the 6 variables
cl<-makeCluster(7)
tic()
var<-c('prcp', 'tmax', 'tmin', 'pr', 'tasmax', 'tasmin') # nclimgrid and cmip6 climate variables
clim<-clusterApply(cl, var, climate_data)
stopCluster(cl)
toc()
######################################

# do this later in the process

# # rbind data
# climate_df<-data.frame()
# for(i in 1:length(clim)){
#   climate_df<-rbind(climate_df, clim[[i]])
# }
# 
# # create wide format with pivot
# climate_df_wide<-pivot_wider(climate_df, names_from = variable, values_from = value)
# 
# write.csv(climate_df_wide, '/glade/scratch/apdixon/climate_df.csv')

