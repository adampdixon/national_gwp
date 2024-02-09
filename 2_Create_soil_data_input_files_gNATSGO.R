######################################################
# This script gathers the csvs for each US county from historic climate data (1951-2021) and creates a table
# for Daycent 
# A Dixon
# Dec 4, 2023
###########################

######################################################
# parallel script will create 6 tables, one for each variable and model (nclim, cmip6)
######################################################
print("starting soils processing")
# args <- commandArgs(trailingOnly = TRUE)
# 
# print(args)

library(dplyr) # for piping & tibble
library(tictoc) # for timing
library(data.table) # for fwrite

tic()

# Open a connection to stderr
sink(stderr(), type = "message")
# Open a connection to stdout
sink(stdout(), type = "message")


# county data to link
geo_link_dir<-'/glade/u/home/apdixon/Documents/national_gwp/Data/County_start'
output_dir<-'/glade/work/apdixon/soils'
gnatsgo_dir<-'/glade/work/apdixon/Output_gNATSGO'

# 
# geo_link_dir<-'/home/ap/Documents/GitHub/national_gwp/Data/County_start'
# output_dir<-'/home/ap/Scratch'
# nclim_dir<-'/home/ap/Documents/GitHub/national_gwp/climate_nclim'
# cmip6_dir<-'/home/ap/Documents/GitHub/national_gwp/climate_cmip6'


# add geolink table to make GEOIDS align with census GEOID
geo_link<-read.csv(file.path(geo_link_dir, 'county_geoid_link.csv'))%>%
  select(zh_geoid, REAL_GEOID)%>%
  arrange(zh_geoid)%>%
  as_tibble()

# county_number<-args[2]
# print(paste0("The county_number is: ", county_number, " \n"))



# parameters<-c('Bulk Density', 'Clay', 'pH', 'Sand', 'Silt', 'SOC')
depths<-c('0 to 2', '2 to 5', '5 to 10', '10 to 20', '20 to 30', 
          '30 to 45', '45 to 60', '60 to 75', '75 to 90', '90 to 105',
          '105 to 120', '120 to 150', '150 to 180', '180 to 200')

for (i in 0:3108){
  if(!file.exists(output_file)){
    
    # GEOID, value
    # GEOID, Depth, BD, Clay, pH, Sand, Silt, SOC
    
    county_data<-data.frame()
    
    for(i in 1:length(depths)){
      
      county_number<-i
      
      zh_GEOID<-filter(geo_link, zh_geoid==county_number)$zh_geoid
      GEOID<-filter(geo_link, zh_geoid==county_number)$REAL_GEOID
      Depth<-depths[i]
      
      output_file<-file.path(output_dir, paste('GEOID_', GEOID, '_gNATSGO.csv', sep = ''))
      
      # print(Depth)
      
      # Get paramter csvs lined up
      BD<-fread(list.files(gnatsgo_dir, full.names = T, pattern = paste('Bulk Density', Depth, sep = ' ')))
      Clay<-fread(list.files(gnatsgo_dir, full.names = T, pattern = paste('Clay', Depth, sep = ' ')))
      pH<-fread(list.files(gnatsgo_dir, full.names = T, pattern = paste('pH', Depth, sep = ' ')))
      Sand<-fread(list.files(gnatsgo_dir, full.names = T, pattern = paste('Sand', Depth, sep = ' ')))
      Silt<-fread(list.files(gnatsgo_dir, full.names = T, pattern = paste('Silt', Depth, sep = ' ')))
      SOC<-fread(list.files(gnatsgo_dir, full.names = T, pattern = paste('SOC', Depth, sep = ' ')))
      
      # read and filter each parameters values
      BD2<-filter(BD, GEOID==zh_GEOID)[,2][[1]]
      Clay2<-filter(Clay, GEOID==zh_GEOID)[,2][[1]]
      pH2<-filter(pH, GEOID==zh_GEOID)[,2][[1]]
      Sand2<-filter(Sand, GEOID==zh_GEOID)[,2][[1]]
      Silt2<-filter(Silt, GEOID==zh_GEOID)[,2][[1]]
      SOC2<-filter(SOC, GEOID==zh_GEOID)[,2][[1]]
      
      
      # bind data
      
      data<-cbind(GEOID, Depth, BD2, Clay2, pH2, Sand2, Silt2, SOC2)
      
      county_data<-rbind(county_data, data)
      
    }
    
    names(county_data)<-c('GEOID', 'Depth_cm', 'BD', 'Clay', 'pH', 'Sand', 'Silt', 'SOC')
    
    fwrite(county_data, output_file)
  } else {
    print(paste0('file already exists', county_number))
  }
  
}



print(paste0('done with soils processing', county_number))

toc()