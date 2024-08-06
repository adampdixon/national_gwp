#######################################
# File: "copy_input_files_to_transfer.R"
# Author: "Adam Dixon"
# Date: "July 2024"
# Description: This script helps easily get input climate and soils data to a send folder so that it's easy to transfer from Glade to local machine
# Script is written to be executed on Casper/Derecho using 'Rscript copy_input_files_to_transfer.R'
# Note soils data is extremely lightweight, so I realize after I've been using this for a while that it's easier/possible to copy ALL soils data over and then not 
# have to worry about it again.
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



geoids_to_fetch<-c(1001,5135,6003,6109,9003,12017,12019,12029,12041,12043,12051,12055,12071,12085,
                                12093,12107,12111,12121,12123,16035,20003,23017,23019,23021,25001,25003,25011,
                                25015,25023,26019,26027,26029,26047,26077,26085,26113,26149,26153,26165,28053,
                                28119,28125,28133,29043,29059,29067,29091,29093,29105,29145,29149,29179,29225,
                                29229,30029,31005,31075,31091,31117,31171,33003,33007,33009,33019,36045,36059,
                                36083,36103,36105,39039,39125,39137,41005,41011,41039,41043,41047,41051,41057,
                                44003,44005,46095,46099,46101,46107,46111,46115,46125,46135,47009,47011,47013,
                                47015,47017,47023,47027,47031,47041,47043,47045,47049,47051,47053,47055,47057,
                                47059,47061,47063,47071,47073,47081,47083,47087,47093,47095,47097,50011,50015,
                                50023,50025,50027,51059,51087,51089,51153,51161,51660,51685,51690,51760,51775,
                                53011,53015,53027,53029,53031,53045,55011,55091,55125)

# get geoids ready for grepl search
geoids_to_fetch<-paste(geoids_to_fetch, collapse = '_|_')
geoids_to_fetch<-paste0('_', geoids_to_fetch, '_')

soil_data<-list.files(soil_data_path, full.names = T)
climate_data<-list.files(climate_data_path, full.names = T)

print('copy soils over')
# Get soils data over
file.copy(soil_data[grepl(geoids_to_fetch, soil_data)], glade_soils_send)

print('copy climate over')
# Get climate data over
file.copy(climate_data[grepl(geoids_to_fetch, climate_data)], glade_climate_send)