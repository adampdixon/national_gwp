#######################################
# Function: "p_Edit_calib_file"
# Author: "Ellen Maas"
# Date: "Nov. 23, 2022"
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates a new output file with results from calibration of
# each model and scenario. If it already exists, it checks to see if a record
# already exists for the model and scenario. If it does, it replaces the data,
# otherwise it appends it to the end."
#
#######################################
# Called by:
# p_Calib_analysis.R
#
#######################################
# Audit Log
# 11/23/2022 EDM Created function.
# 3/14/2023 EDM Added cotton and sorghum.
#######################################

p_Edit_calib_file <- function(data_mtx,model_name,scenario_name) {
  
  print("Starting p_Edit_calib_file")
  
  #**********************************************************************
  
  # Write data to model's log -------------------------------------
  
  write.table(data_mtx,file=paste0(results_path,"Calibration_log_",model_name,".csv"),
              append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
  
  
  # Write data to summary log -------------------------------------
  
  outfile_name <- paste0(results_path,"Calibration_summary.csv")
  colnames(data_mtx) <- log_col_headers
  
  # if the output file already exists, check if the model/scenario already has
  # an entry and replace it; otherwise, create a new file
  if(file.exists(outfile_name)) {
    existing_data <- read.table(file=outfile_name,header=TRUE,sep=",")
    existing_rec <- which(existing_data$Model==model_name & 
                            existing_data$Scenario_Name==scenario_name)
    if(length(existing_rec!=0)) {
      existing_data[existing_rec,"Date_time"] <- as.character(Sys.time())
      existing_data[existing_rec,"Maize_slope"] <- data_mtx[,7]
      existing_data[existing_rec,"Maize_yint"] <- data_mtx[,8]
      existing_data[existing_rec,"Maize_R2"] <- data_mtx[,9]
      existing_data[existing_rec,"Maize_RMSE"] <- data_mtx[,10]
      existing_data[existing_rec,"Maize_diff"] <- data_mtx[,11]
      existing_data[existing_rec,"Soy_slope"] <- data_mtx[,12]
      existing_data[existing_rec,"Soy_yint"] <- data_mtx[,13]
      existing_data[existing_rec,"Soy_R2"] <- data_mtx[,14]
      existing_data[existing_rec,"Soy_RMSE"] <- data_mtx[,15]
      existing_data[existing_rec,"Soy_diff"] <- data_mtx[,16]
      existing_data[existing_rec,"Wheat_slope"] <- data_mtx[,17]
      existing_data[existing_rec,"Wheat_yint"] <- data_mtx[,18]
      existing_data[existing_rec,"Wheat_R2"] <- data_mtx[,19]
      existing_data[existing_rec,"Wheat_RMSE"] <- data_mtx[,20]
      existing_data[existing_rec,"Wheat_diff"] <- data_mtx[,21]
      existing_data[existing_rec,"SOC_slope"] <- data_mtx[,22]
      existing_data[existing_rec,"SOC_yint"] <- data_mtx[,23]
      existing_data[existing_rec,"SOC_R2"] <- data_mtx[,24]
      existing_data[existing_rec,"SOC_RMSE"] <- data_mtx[,25]
      existing_data[existing_rec,"SOC_diff"] <- data_mtx[,26]
      existing_data[existing_rec,"Temp_slope"] <- data_mtx[,27]
      existing_data[existing_rec,"Temp_yint"] <- data_mtx[,28]
      existing_data[existing_rec,"Temp_R2"] <- data_mtx[,29]
      existing_data[existing_rec,"Temp_RMSE"] <- data_mtx[,30]
      existing_data[existing_rec,"Moist_slope"] <- data_mtx[,31]
      existing_data[existing_rec,"Moist_yint"] <- data_mtx[,32]
      existing_data[existing_rec,"Moist_R2"] <- data_mtx[,33]
      existing_data[existing_rec,"Moist_RMSE"] <- data_mtx[,34]
      existing_data[existing_rec,"N2O_slope"] <- data_mtx[,35]
      existing_data[existing_rec,"N2O_yint"] <- data_mtx[,36]
      existing_data[existing_rec,"N2O_R2"] <- data_mtx[,37]
      existing_data[existing_rec,"N2O_RMSE"] <- data_mtx[,38]
      existing_data[existing_rec,"N2O_diff"] <- data_mtx[,39]
      existing_data[existing_rec,"CH4_slope"] <- data_mtx[40]
      existing_data[existing_rec,"CH4_yint"] <- data_mtx[,41]
      existing_data[existing_rec,"CH4_R2"] <- data_mtx[,42]
      existing_data[existing_rec,"CH4_RMSE"] <- data_mtx[,43]
      existing_data[existing_rec,"CH4_diff"] <- data_mtx[,44]
      existing_data[existing_rec,"Cotton_slope"] <- data_mtx[,45]
      existing_data[existing_rec,"Cotton_yint"] <- data_mtx[,46]
      existing_data[existing_rec,"Cotton_R2"] <- data_mtx[,47]
      existing_data[existing_rec,"Cotton_RMSE"] <- data_mtx[,48]
      existing_data[existing_rec,"Cotton_diff"] <- data_mtx[,49]
      existing_data[existing_rec,"Sorghum_slope"] <- data_mtx[,50]
      existing_data[existing_rec,"Sorghum_yint"] <- data_mtx[,51]
      existing_data[existing_rec,"Sorghum_R2"] <- data_mtx[,52]
      existing_data[existing_rec,"Sorghum_RMSE"] <- data_mtx[,53]
      existing_data[existing_rec,"Sorghum_diff"] <- data_mtx[,54]
      existing_data[existing_rec,"Maize_cultivar"] <-data_mtx[,55]
      existing_data[existing_rec,"Soybean_cultivar"] <-data_mtx[,56]
      existing_data[existing_rec,"Wheat_cultivar"] <-data_mtx[,57]
      existing_data[existing_rec,"Cotton_cultivar"] <-data_mtx[,58]
      existing_data[existing_rec,"Sorghum_cultivar"] <-data_mtx[,59]
      data_mtx <- existing_data
    } else {
      data_mtx <- rbind(existing_data,data_mtx)
    } # end if record exists
  } # end if file exists
  
  write.table(data_mtx,file=outfile_name,
              col.names=T,
              row.names=F,sep=",")
  
}
