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



source(file.path(master_path, 'data_explore', 'get_model_tables.R'))

for (c in c('Soybean', 'Cotton', 'Wheat', 'Rotation')) { # 'Maize', 
  # write a table of the data
  fwrite(get_all_models_national_df(crop=c), file.path(national_figs, paste0(c, '_national_results_', date, '.csv')))
}
