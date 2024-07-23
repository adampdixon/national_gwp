#######################################
# File: "copy_input_files_to_transfer.R"
# Author: "Adam Dixon"
# Date: "July 2024"
# Description: This script helps easily get input climate and soils data to a send folder so that it's easy to transfer from Glade to local machine
#
#######################################


if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    source('/home/ap/Documents/GitHub/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  } else {
    source('/glade/derecho/scratch/apdixon/national_gwp/000_Workspace_Dirs.R', local = TRUE)
    glade_soils_send<-'/glade/derecho/scratch/apdixon/soils_send'
    glade_climate_send<-'/glade/derecho/scratch/apdixon/climate_send'
  }
}



geoids_to_fetch<-c(5007, 4009, 5021, 4015)

# get geoids ready for grepl search
geoids_to_fetch<-paste(geoids_to_fetch, collapse = '_|_')
geoids_to_fetch<-paste0('_', geoids_to_fetch, '_')

soil_data<-list.files(soil_data_path, full.names = T)
climate_data<-list.files(climate_data_path, full.names = T)

# Get soils data over
file.copy(soil_data[grepl(geoids_to_fetch, soil_data)], glade_soils_send)

# Get climate data over
file.copy(climate_data[grepl(geoids_to_fetch, climate_data)], glade_climate_send)