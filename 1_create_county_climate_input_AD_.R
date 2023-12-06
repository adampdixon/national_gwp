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
  
  #County GEOID
  GEOID<-filter(geo_link, zh_geoid==county_number)$REAL_GEOID
  if(is.na(GEOID)){ # stop if GEOID is NA
    break
  } else{
    # GET HISTORIC DATA
    for(var in c('prcp','tmax','tmin')){
      #####################################
      # check if output file exists, if so, skip
      output_filename<-file.path(output_dir, paste0(var,"_", GEOID ,'_nclim.csv'))
      #####################################
      if(file.exists(output_filename)){
        next
      } else {
        hist_climate_df<-data.frame()
        # read csvs
        data_raw<-list.files(nclim_dir, pattern = var, full.names = T)
        data<-data_raw[grep('.csv', data_raw)]
        for(year in data){
          ##### READ DATA ######################
          #read county csv for each year into data.frame, get county_number from out of function list
          data_df<-read.csv(year)%>%as_tibble()%>%filter(GEOID==county_number)
          ## GET GEOID #########################
          # join with the GEOID given by Zhuonan
          data_df_<-left_join(data_df, geo_link, by = c('GEOID' = 'zh_geoid'))
          # drop Zhuonan geoid
          data_df_<-select(data_df_, -GEOID)
          colnames(data_df_)<-c('value', 'year', 'doy', 'GEOID')
          
          # use select to put in columns in correct order
          data_df_<-select(data_df_, GEOID, value, year, doy)
          
          # Prepare data #
          # add the variable name to column, table will be long format
          data_df_$variable<-var
          data_df_$model<-'nclim'
          
          # combine years
          
          hist_climate_df<-rbind(hist_climate_df, data_df_)
          
        }
        # once all years have run and rbind, write to csv
        write.csv(hist_climate_df, output_filename)
        
      }
      
    }
    
    # GET FUTURE DATA
    for (var in c('pr','tasmax','tasmin')){
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
      #####################################
      # check if output file exists, if so, skip
      output_filename2<-file.path(output_dir, paste0(var2,"_", GEOID ,'_cmip6.csv'))
      #####################################
      if(file.exists(output_filename2)){
        next
      } else {
        #empty df to rbind
        future_climate_df<-data.frame()
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
          
          data_df$variable<-var2
          data_df$model<-'cmip6_gfdl-esm4'
          # create col names that are usable across variables
          # GEOID,tasmin,year,doy
          # change actual data column to value for long table format
          colnames(data_df)<-c('GEOID', 'value', 'year', 'doy', 'variable','model')
          
          future_climate_df<-rbind(future_climate_df, data_df)
        }
        write.csv(future_climate_df, output_filename2)
      }
    }
    
  }
}


###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T) # not needed?
# use 7 cores, one for main processing, and one for the 6 variables
cl<-makeCluster(72)
tic()
county_number<-1871:3109 # number of US counties in CONUS
clim<-clusterApply(cl, county_number, climate_data)
stopCluster(cl)
toc()
######################################
