#######################################
# File: "000_Workspace_Dirs.R"
# Author: "Adam Dixon"
# Date: "June 2024"
# Description: # Directory script for national_gwp
#
#######################################



if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    master_path<-'/home/ap/Documents/GitHub/national_gwp'
    results_folder<-'/home/ap/Documents/national_gwp_results'

    climate_data_path<-'/home/ap/Scratch'
    soil_data_path<-'/home/ap/soils'
    
    national_figs<-'/home/ap/Documents/national_gwp_figs'

    Glade=FALSE # Save on Glade file system?
    print("************************************")
    print("*****Using linux mint *********")
    print("************************************")
  } else {
    master_path<-'/glade/derecho/scratch/apdixon/national_gwp'
    results_folder<-'/glade/derecho/scratch/apdixon/national_gwp_results'
    Glade=TRUE # Save on Glade file system?
    
    soil_data_path<-'/glade/work/apdixon/soils'
    climate_data_path<-'/glade/work/apdixon/climate'
    
    national_figs<-'/glade/derecho/scratch/apdixon/national_gwp_figs'

    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
    print("************************************")
  }
}

date<-gsub("-", "", Sys.Date())

print(paste0('master_path - ', master_path))
print(paste0('results_folder - ', results_folder))
print(paste0("date and time are ", Sys.time()))
print(paste0('*********soil_data_path is ', soil_data_path, " **************"))
print(paste0('*********climate_data_path is ', climate_data_path, " **************"))
print(paste0('*********national_figs saved to  ', national_figs, " **************"))



