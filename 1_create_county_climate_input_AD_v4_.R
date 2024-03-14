######################################################
# This script gathers the csvs for each US county from future climate data (2022-2050) and creates a table
# for Daycent 
# can be run with bash parallel script
# A Dixon
# Dec 4, 2023
###########################

######################################################
# parallel script will create 6 tables, one for each variable and model (nclim, cmip6)
######################################################

library(dplyr) # for piping & tibble

# library(tictoc) # for timing
library(data.table) # for fwrite

# Set workspace
if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    geo_link_dir<-'/home/ap/Documents/GitHub/national_gwp/Data/County_start'
    output_dir<-'/home/ap/Scratch'
    
    county_number = 1

    cmip6_dir<-'/home/ap/Documents/GitHub/national_gwp/cmip6_climate'
    print("************************************")
    print("*****Using linux mint *********")
  } else {
    # county data to link
    geo_link_dir<-'/glade/u/home/apdixon/Documents/national_gwp/Data/County_start'
    output_dir<-'/glade/work/apdixon/climate'

    cmip6_dir<-'/glade/work/apdixon/Output_climate' # say output climate, but it's Zhuonan's output folder when he was processing
    
    county_number = args[1]
    
    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
  }
}


# From: https://wcrp-cmip.org/cmip-model-and-experiment-documentation/#models
# <<<<ssp585>>>>
# Future scenario with high radiative forcing by the end of century. 
# Following approximately RCP8.5 global forcing pathway but with new forcing based on 
# SSP5. Concentration-driven. As a tier 2 option, this simulation should be extended to year 2300

# <<<<ssp126>>>>
# Future scenario with low radiative forcing by the end of century. Following approximately 
# RCP2.6 global forcing pathway but with new forcing based on SSP1. Concentration-driven. 
# As a tier 2 option, this simulation should be extended to year 2300


# add geolink table to make GEOIDS align with census GEOID
geo_link<-read.csv(file.path(geo_link_dir, 'county_geoid_link.csv'))%>%
  select(zh_geoid, REAL_GEOID)%>%
  as_tibble()





# Open a connection to stderr
sink(stderr(), type = "message")
# Print an error message to stderr
cat(paste0("Starting county ", county_number, "\n"), file = stderr(), append = TRUE)
  
#County GEOID
GEOID<-filter(geo_link, zh_geoid==county_number)$REAL_GEOID
if(is.na(GEOID)){ # stop if GEOID is NA
  cat("GEOID is NA\n")
  break
} else{
   # GET FUTURE DATA
  for (var in c('pr','tasmax','tasmin')){
    # use if statements to convert cmip6 naming convention to nclims so they align later
    if(var=='pr'){ # change var names to align with nclim/daycent processing
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
    output_filename2<-file.path(output_dir, paste0(var2,"_", GEOID ,'_cmip6_ssp585.csv'))
    #####################################
    if(file.exists(output_filename2)){
      next
      
    } else {
      climate_code<-'_ssp126_gfdl-esm4'
      #empty df to rbind
      future_climate_df<-data.frame()
      # read csvs #get only ssp126 scenario
      data_raw<-list.files(cmip6_dir, pattern = paste0(var, climate_code), full.names = T) #get correct variable
      # data<-data_raw[grep('gfdl-esm4', data_raw)] # get gfdl-esm4 #not needed
      # data<-data_raw[grep('.csv', data)] # get only csvs  #not needed
      
      for (year in data_raw){
        # translate from geo_link, and use for file name at end
        GEOID_<-geo_link%>%filter(zh_geoid==county_number)
        GEOID2<-GEOID_$REAL_GEOID
        
        #read multiple csvs into data.frame
        data_df<-fread(year)%>%as_tibble()%>%filter(GEOID==GEOID2)
        # add the variable name to column, table will be long format
        
        data_df$variable<-var2
        data_df$model<-climate_code
        # create col names that are usable across variables
        # GEOID,tasmin,year,doy
        # change actual data column to value for long table format
        colnames(data_df)<-c('GEOID', 'value', 'year', 'doy', 'variable','model')
        
        future_climate_df<-rbind(future_climate_df, data_df)
      }
      fwrite(future_climate_df, output_filename2)
    }
  }
    
    # Print an error message to stderr
    cat(paste0("Finished ", county_number, file = stderr(), append = TRUE))
    # Close the connection to stderr
    sink(type = "message")

}


