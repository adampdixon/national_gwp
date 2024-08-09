#######################################
# File: "10_Data_Explore_Viz"
# Author: "Adam Dixon"
# Date: "June 2024"
# Description: This script starts the data exploration and visualization process. Input and output
# data are explored and visualized. It would be nice if all these functions ran at the same time, but
# generally, this is just a compilation of the data vis functions.
#######################################

if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    source('/home/ap/Documents/GitHub/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  } else {
    source('/glade/derecho/scratch/apdixon/national_gwp/000_Workspace_Dirs.R', local = TRUE)
  }
}

###########################
# input soils boxplots
source(file.path(master_path, 'data_explore', 'soils_boxplots_function.R'))
###########################

###########################
# Get climate data
# This first script reads and creates tables for the climate data
source(file.path(master_path, 'data_explore', 'get_input_clim_4Vis_County.R'))# TODO get this working
# This script then makes boxplots with the data
source(file.path(master_path, 'data_explore', 'climate_input_plots_county.R'))
###########################

###########################
# Note: run the 'run_get_model_tables.R' first to collate model data on GLADE. Then transfer those files to local environment so that the
#  'get_model_tables.R' will use the already put together tables instead of re-running getting the data every time there is a call. 
# get model data for data visualization
source(file.path(master_path, 'data_explore', 'get_model_tables.R'))
###########################

###########################
# output line graphs
###########################
source(file.path(master_path, 'data_explore', 'All_models_national_results_yearly_plots.R'))

# This one takes a while to process
for (c in c('Maize', 'Soybean', 'Cotton', 'Wheat', 'Rotation')) {
  create_national_model_linegraphs(crop=c)
}


###########################
# Maps
###########################
source(file.path(master_path, 'data_explore', 'GWP_national_maps.R'))

for (mt in c('Yld', 'SOC', 'CH4', 'N2O', 'CO2')) { #'Yld', 'SOC', 'CH4', 'N2O', 'CO2'
  print(mt)
  for (i in c('Soybean', 'Wheat', 'Cotton', 'Maize', 'Rotation')){
    print(i)
    # Year_ = 2050; Crop_ = 'Soybean'; Output = national_figs; map_type = 'Yld'; clim_scen = 'low'
    national_map_all_scenarios(Year_ = 2050, Crop_ = i, Output = national_figs, map_type = mt, clim_scen = 'low')
  }
}
###########################

###########################
# boxplots of output data
###########################
source(file.path(master_path, 'data_explore', 'GWP_national_boxplots.R'))
# This one takes a while to process
for (c in c('Maize', 'Soybean', 'Cotton', 'Wheat', 'Rotation')) {
  national_boxplots_all_scenarios(Crop_=c)
}

###########################



