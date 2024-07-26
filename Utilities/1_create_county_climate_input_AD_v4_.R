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
    
    # args=(commandArgs(TRUE))
    # county_number = args[2]
    
    # args=(commandArgs(TRUE))
    # print(args[2])
    # county_number<-1

    cmip6_dir<-'/home/ap/Documents/GitHub/national_gwp/cmip6_climate'
    
    print("************************************")
    print("*****Using linux mint *********")
  } else {
    # county data to link
    geo_link_dir<-'/glade/u/home/apdixon/Documents/national_gwp/Data/County_start'
    output_dir<-'/glade/work/apdixon/climate'

    cmip6_dir<-'/glade/work/apdixon/Output_climate' # say output climate, but it's Zhuonan's output folder when he was processing
    
    args=(commandArgs(TRUE))
    county_number = args[2]
    
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

# GFDL-ESM4 lower error than UKESM1-0-LL
# https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2022JD038198?af=R


# add geolink table to make GEOIDS align with census GEOID
geo_link<-fread(file.path(geo_link_dir, 'county_geoid_link.csv'))%>%
  select(zh_geoid, REAL_GEOID)%>%
  as_tibble()%>%
  filter(REAL_GEOID %in% c(1001,
                           9003,
                           29093,
                           29179,
                           51059,
                           51087,
                           51089,
                           51153,
                           51161,
                           51600,
                           51685,
                           51690,
                           51760,
                           51775,
                           55011,
                           55091,
                           56031,
                           56033,
                           56035,
                           56037,
                           56039,
                           56041,
                           56043,
                           56045)) # only use these counties





# Open a connection to stderr
sink(stderr(), type = "message")
# Print an error message to stderr
cat(paste0("Starting county ", county_number, "\n"), file = stderr(), append = TRUE)
  
#County GEOID
# GEOID<-filter(geo_link, zh_geoid==county_number)$REAL_GEOID
GEOID<-geo_link[county_number,]$REAL_GEOID

# GEOID<-geo_link[1,]$REAL_GEOID
if(is.na(GEOID)){ # stop if GEOID is NA
  cat("GEOID is NA\n")
  break
} else{
  for (climate_code in c('_ssp585_gfdl-esm4_', '_ssp126_gfdl-esm4_')){
     # GET FUTURE DATA
    for (var in c('pr','tasmax','tasmin','sfcwind','rsds')){
      # use if statements to convert cmip6 naming convention to nclims so they align later
      if(var=='pr'){ # change var names to align with nclim/daycent
        var2='prcp'
      }
      if(var=='tasmax'){
        var2='tmax'
      }
      if(var=='tasmin'){
        var2='tmin'
      }
      if(var=='sfcwind'){ # these two don't need changed, but we're using var2 from now on
        var2='sfcwind'
      }
      if(var=='rsds'){
        var2='rsds'
      }
      
      # ssp126_ukesm1
      # ssp585_gfdl-esm4
      # GEOID,sfcwind,year,doy
      # GEOID,rsds,year,doy
      #####################################
      # check if output file exists, if so, skip
      #####################################
      
      #####################################
      # output_filename2<-file.path(output_dir, paste0(var2,"_", GEOID ,'_cmip6.csv'))
      output_filename2<-file.path(output_dir, paste0(var2,"_", GEOID , climate_code, '_cmip6.csv'))
      #####################################
      if(file.exists(output_filename2)){
        next
        
      } else {
  
        #empty df to rbind
        future_climate_df<-data.frame()
        # read csvs #get correct scenario
        data_raw<-list.files(cmip6_dir, pattern = paste0(var, climate_code), full.names = T) #get correct variable
        
        data_raw2<-data_raw[grep(paste(paste0('Y', 2022:2051), collapse = '|'), data_raw)] # get only 2022 to 2050, using ~ 'Y2022' at end

        # data<-data_raw[grep('.csv', data)] # get only csvs  #not needed
        
        for (year in data_raw2){
          # translate from geo_link, and use for file name at end
          # GEOID_<-geo_link%>%filter(zh_geoid==county_number)
          GEOID_<-geo_link[county_number,]
          GEOID2<-GEOID_$REAL_GEOID
          
          #read multiple csvs into data.frame
          data_df<-fread(year)%>%filter(GEOID==GEOID2)
          # add the variable name to column, table will be long format
          
          data_df$variable<-var2
          data_df$model<-paste0(substr(climate_code, 2, nchar(climate_code)))
          # create col names that are usable across variables
          # GEOID,tasmin,year,doy
          # change actual data column to value for long table format
          colnames(data_df)<-c('GEOID', 'value', 'year', 'doy', 'variable','model')
          
          future_climate_df<-rbind(future_climate_df, data_df)
        }
        fwrite(future_climate_df, output_filename2, append = F)
      }
    }
  } # end of climate_code loop
    
    # Print an error message to stderr
    cat(paste0("Finished ", county_number, file = stderr(), append = TRUE))
    print(paste("data saved to", output_filename2))
    # Close the connection to stderr
    sink(type = "message")

}


