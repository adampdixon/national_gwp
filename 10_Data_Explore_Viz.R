#######################################
# File: "10_Data_Explore_Viz"
# Author: "Adam Dixon"
# Date: "June 2024"
# Description: This script starts the data exploration and visualization process. Input and output
# data are explored and visualized.
#
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
source(file.path(master_path, 'data_explore', 'soils_boxplots_function.R'))

# -input climate boxplots and histograms
# Get climate data
source(file.path(master_path, 'data_explore', 'get_input_clim_4Vis_County.R'))
source(file.path(master_path, 'data_explore', 'climate_input_plots_county.R'))

# get model data for data visualization

# output ghg boxplots, histograms, line graphs, maps
source(file.path(master_path, 'data_explore', 'All_models_national_results_yearly_plots.R'))
create_national_model_linegraphs(crop='Maize')
create_national_model_linegraphs(crop='Soybean')
create_national_model_linegraphs(crop='Cotton')
create_national_model_linegraphs(crop='Wheat')
create_national_model_linegraphs(crop='Rotation')

# Maps






