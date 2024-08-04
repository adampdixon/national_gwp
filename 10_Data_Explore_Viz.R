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

# TODOS
# data exploration
# -something to show that output data is there
#source=check_model_results_folder.R

# -table with counties and number of files per model
# 
# -input soils boxplots
# source(file.path(master_path, 'data_explore', 'soils_boxplots_function.R'))

# -input climate boxplots and histograms
# Get climate data
# This first script reads and creates tables for the climate data
# source(file.path(master_path, 'data_explore', 'get_input_clim_4Vis_County.R'))# TODO get this working
# This script then makes boxplots with the data
# source(file.path(master_path, 'data_explore', 'climate_input_plots_county.R')) 

# get model data for data visualization
source(file.path(master_path, 'data_explore', 'get_model_tables.R'))


# output line graphs
source(file.path(master_path, 'data_explore', 'All_models_national_results_yearly_plots.R'))

# This one takes a while to process
for (c in c('Maize', 'Soybean', 'Cotton', 'Wheat', 'Rotation')) {
  create_national_model_linegraphs(crop=c)
}

# Maps
source(file.path(master_path, 'data_explore', 'GWP_national_maps.R'))

for (mt in c('Yld', 'SOC', 'CH4', 'N2O', 'CO2')) { #'Yld', 'SOC', 'CH4', 'N2O', 'CO2'
  print(mt)
  for (i in c('Soybean', 'Wheat', 'Cotton', 'Maize', 'Rotation')){
    print(i)
    national_map_all_scenarios(Year_ = 2050, Crop_ = i, Output = national_figs, map_type = mt, clim_scen = 'low')
  }
}

# boxplots of output data
source(file.path(master_path, 'data_explore', 'GWP_national_boxplots.R'))
# This one takes a while to process
for (c in c('Maize', 'Soybean', 'Cotton', 'Wheat', 'Rotation')) {
  national_boxplots_all_scenarios(Crop_=c)
}





