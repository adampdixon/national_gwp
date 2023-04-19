#######################################
# Function: "p_Edit_calib_data_file"
# Author: "Ellen Maas"
# Date: "Nov. 4, 2022"
# Output: It creates a new output file with summary data from a model
# run and writes the data. If it already exists, it checks to see if a record
# already exists for the model and scenario. If it does, it replaces the data,
# otherwise it appends it to the end."
#
#######################################
# Called by:
# 10_Model_Ensemble_results-by_scenario.R
#
#######################################
# Audit Log
# 4/3/2023 EDM Created function.
#######################################

p_Edit_calib_data_file <- function(data_mtx,outfile_name) {
  
  print("Starting p_Edit_calib_data_file")
  
  # if the output file already exists, check if the model/scenario already has
  # an entry and replace it; otherwise, create a new file
  if(file.exists(outfile_name)) {
    
    existing_file_df <- read.table(file=outfile_name,header=TRUE,sep=",")
    existing_recs <- which(existing_file_df$treatment_scen==scenario_descriptor)
    
    # if data from scenario already exists, remove it
    if(length(existing_recs!=0)) {
      existing_data <- filter(existing_file_df, !treatment_scen == scenario_descriptor)
    } else { 
      existing_data <- existing_file_df
    }# end if record exists
    
    # in any case, add incoming data
    data_mtx <- rbind(existing_data,data_mtx)
    
  } # end if file exists
  
  write.table(data_mtx,file=paste0(outfile_name),
              # col.names=c("year","Obs_sd","treatment_scen","Model",
              #             "Fit","value"),
              row.names=F,sep=",")
  
}