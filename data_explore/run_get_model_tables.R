#!/usr/bin/env Rscript

# for pulling in number to index crop list
args = commandArgs(trailingOnly=TRUE)
#######################################
# File: "run_get_model_tables.R"
# Author: "Adam Dixon"
# Date: "August 2024"
# Description: Runs get_model_table.R on Casper/Derecho to save results to tables for easier processing later.
# See .sh file for execution on Casper. 
#######################################


if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    source('/home/ap/Documents/GitHub/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  } else {
    source('/glade/derecho/scratch/apdixon/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  }
}

library(data.table)

index<-as.numeric(args[2])
# index<-1



source(file.path(master_path, 'data_explore', 'get_model_tables.R'))
  
crop_<-c('Maize', 'Soybean', 'Cotton', 'Wheat', 'Rotation')

print(paste("arg is", index))
print(paste("crop is", crop_[index]))


# test<-get_all_models_national_df(crop=crop_[index], load = FALSE)
# write out the national results, crop index arg is passed from pbs_array_index from .sh file

output_file<-file.path(national_figs, paste0(crop_[index], '_national_results_', date, '.csv'))

fwrite(get_all_models_national_df(crop=crop_[index], load=FALSE), output_file)

print(paste("Wrote out ", output_file))

