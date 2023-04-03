#######################################
# Function: "p_Edit_calib_df_file"
# Author: "Ellen Maas"
# Date: "Nov. 4, 2022"
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates a new output file with summary data from a model
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

p_Edit_calib_df_file <- function(data_mtx,treatment_scen,attribute) {
  
  print("Starting p_Edit_calib_df_file")
  
  outfile_name <- paste0(results_path,"calib_df.csv")
  data_mtx$attribute<-attribute
  if(attribute=="crop") {
    colnames(data_mtx) <- c("Year","Observed","Obs_sd","APSIM","Daycent","crop",
                            "treatment_scen","attribute")
  } else if(attribute=="soc") {
    colnames(data_mtx) <- c("Year","Observed","Obs_sd","APSIM","Daycent",
                            "RothC","Millennial","crop","treatment_scen","attribute")
  }
  
  # if the output file already exists, check if the model/scenario already has
  # an entry and replace it; otherwise, create a new file
  if(file.exists(outfile_name)) {
    existing_data <- read.table(file=outfile_name,header=TRUE,sep=",")
    existing_rec <- which(existing_data$treatment_scen==scenario_name &
                            existing_data$attribute==attribute)
    if(length(existing_rec!=0)) {
      existing_data[existing_rec,"Year"] <- data_mtx[,"Year"]
      existing_data[existing_rec,"Observed"] <- data_mtx[,"Observed"]
      existing_data[existing_rec,"Obs_sd"] <- data_mtx[,"Obs_sd"]
      existing_data[existing_rec,"APSIM"] <- data_mtx[,"APSIM"]
      existing_data[existing_rec,"Daycent"] <- data_mtx[,"Daycent"]
      if(attribute=="crop") {
        existing_data[existing_rec,"RothC"] <- NA
        existing_data[existing_rec,"Millennial"] <- NA
      } else if(attribute=="soc") {
        existing_data[existing_rec,"RothC"] <- data_mtx[,"RothC"]
        existing_data[existing_rec,"Millennial"] <- data_mtx[,"Millennial"]
      }
      existing_data[existing_rec,"crop"] <- data_mtx[,"crop"]
      existing_data[existing_rec,"treatment_scen"] <- data_mtx[,"treatment_scen"]
      existing_data[existing_rec,"attribute"] <- data_mtx[,"attribute"]
      data_mtx <- existing_data
    } else {
      data_mtx <- rbind(existing_data,data_mtx)
    } # end if record exists
  } # end if file exists
  
  write.table(data_mtx,file=paste0(outfile_name),
              col.names=c("Year","Observed","Obs_sd","APSIM","Daycent","RothC",
                          "Millennial","crop","treatment_scen","attribute"),
              row.names=F,sep=",")
  
}