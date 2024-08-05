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

# for pulling in number to index crop list
args <- commandArgs(trailingOnly = TRUE)

source(file.path(master_path, 'data_explore', 'get_model_tables.R'))
  
c<-c('Maize', 'Soybean', 'Cotton', 'Wheat', 'Rotation')

# write out the national results, crop index arg is passed from pbs_array_index from .sh file

fwrite(get_all_models_national_df(crop=c[args[2]]), file.path(national_figs, paste0(c[args[2]], '_national_results_', date, '.csv')))

fwrite(get_all_models_national_df(crop=c[args[2]]), file.path(national_figs, paste0(c[args[2]], '_national_results_', date, '.csv')))

# fwrite(get_all_models_national_df(crop=c[2], file.path(national_figs, paste0(c, '_national_results_', date, '.csv'))))