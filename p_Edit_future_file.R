#######################################
# Function: "p_Edit_future_file"
# Author: "Ellen Maas"
# Date: "Nov. 4, 2022"
# Output: Clones p_Edit_output_file. Function doesn't return any data, 
# hence the "p" (procedure) naming convention. It creates a new output file
# with summary data from a model run and writes the data. If it already exists, 
# it checks to see if a record already exists for the model and scenario. If 
# it does, it replaces the data, otherwise it appends it to the end."
#
#######################################
# Called by:
# 9_Results_[model]-future_[site].R
#######################################
# Audit Log
# 6/23/2023 EDM Created function. Cloned p_Edit_output_file.
#######################################

p_Edit_future_file <- function(data_mtx,model_name,scenario_name) {
  
  print("Starting p_Edit_future_file")
  
  outfile_name <- paste0(results_path,"Summary_future_output.csv")
  colnames(data_mtx) <- c("Model","Climate_Scenario","Mgmt_Scenario",
                          "Scenario_Name","Scenario_Abbrev",
                          "SW_20cm_slope","SW_20cm_change",
                          "SW_40cm_slope","SW_40cm_change",
                          "SW_60cm_slope","SW_60cm_change",
                          "WFPS_10cm_slope","WFPS_10cm_change",
                          "WFPS_20cm_slope","WFPS_20cm_change",
                          "WFPS_40cm_slope","WFPS_40cm_change",
                          "NO3_slope","NO3_change",
                          "N2O_slope","N2O_change",
                          "WFPS_0cm_slope","WFPS_0cm_change",
                          "WFPS_2cm_slope","WFPS_2cm_change",
                          "WFPS_5cm_slope","WFPS_5cm_change",
                          "SoilT_slope","SoilT_change",
                          "CH4_slope","CH4_change",
                          "CI_slope","CI_change",
                          "SW_25cm_slope","SW_25cm_change",
                          "BC_25cm_slope","BC_25cm_change",
                          "BN_25cm_slope","BN_25cm_change",
                          "HC_25cm_slope","HC_25cm_change",
                          "HN_25cm_slope","HN_25cm_change",
                          "CinB_25cm_slope","CinB_25cm_change",
                          "CinH_25cm_slope","CinH_25cm_change",
                          "CinBtoH_25cm_slope","CinBtoH_25cm_change",
                          "SOC_25cm_slope","SOC_25cm_change",
                          "SoilT_25cm_slope","SoilT_25cm_change")
  
  
  # if the output file already exists, check if the model/scenario already has
  # an entry and replace it; otherwise, create a new file
  if(file.exists(outfile_name)) {
    existing_data <- read.table(file=outfile_name,header=TRUE,sep=",")
    existing_rec <- which(existing_data$Model==model_name & existing_data$Scenario_Name==scenario_name)
    if(length(existing_rec!=0)) {
      existing_data[existing_rec,"SW_20cm_slope"] <- data_mtx[,6]
      existing_data[existing_rec,"SW_20cm_change"] <- data_mtx[,7]
      existing_data[existing_rec,"SW_40cm_slope"] <- data_mtx[,8]
      existing_data[existing_rec,"SW_40cm_change"] <- data_mtx[,9]
      existing_data[existing_rec,"SW_60cm_slope"] <- data_mtx[,10]
      existing_data[existing_rec,"SW_60cm_change"] <- data_mtx[,11]
      existing_data[existing_rec,"WFPS_10cm_slope"] <- data_mtx[,12]
      existing_data[existing_rec,"WFPS_10cm_change"] <- data_mtx[,13]
      existing_data[existing_rec,"WFPS_20cm_slope"] <- data_mtx[,14]
      existing_data[existing_rec,"WFPS_20cm_change"] <- data_mtx[,15]
      existing_data[existing_rec,"WFPS_40cm_slope"] <- data_mtx[,16]
      existing_data[existing_rec,"WFPS_40cm_change"] <- data_mtx[,17]
      existing_data[existing_rec,"NO3_slope"] <- data_mtx[,18]
      existing_data[existing_rec,"NO3_change"] <- data_mtx[,19]
      existing_data[existing_rec,"N2O_slope"] <- data_mtx[,20]
      existing_data[existing_rec,"N2O_change"] <- data_mtx[,21]
      existing_data[existing_rec,"WFPS_0cm_slope"] <- data_mtx[,22]
      existing_data[existing_rec,"WFPS_0cm_change"] <- data_mtx[,23]
      existing_data[existing_rec,"WFPS_2cm_slope"] <- data_mtx[,24]
      existing_data[existing_rec,"WFPS_2cm_change"] <- data_mtx[,25]
      existing_data[existing_rec,"WFPS_5cm_slope"] <- data_mtx[,26]
      existing_data[existing_rec,"WFPS_5cm_change"] <- data_mtx[,27]
      existing_data[existing_rec,"SoilT_slope"] <- data_mtx[,28]
      existing_data[existing_rec,"SoilT_change"] <- data_mtx[,29]
      existing_data[existing_rec,"CH4_slope"] <- data_mtx[,30]
      existing_data[existing_rec,"CH4_change"] <- data_mtx[,31]
      existing_data[existing_rec,"CI_slope"] <- data_mtx[,32]
      existing_data[existing_rec,"CI_change"] <- data_mtx[,33]
      existing_data[existing_rec,"SW_25cm_slope"] <- data_mtx[,34]
      existing_data[existing_rec,"SW_25cm_change"] <- data_mtx[,35]
      existing_data[existing_rec,"BC_25cm_slope"] <- data_mtx[,36]
      existing_data[existing_rec,"BC_25cm_change"] <- data_mtx[,37]
      existing_data[existing_rec,"BN_25cm_slope"] <- data_mtx[,38]
      existing_data[existing_rec,"BN_25cm_change"] <- data_mtx[,39]
      existing_data[existing_rec,"HC_25cm_slope"] <- data_mtx[,40]
      existing_data[existing_rec,"HC_25cm_change"] <- data_mtx[,41]
      existing_data[existing_rec,"HN_25cm_slope"] <- data_mtx[,42]
      existing_data[existing_rec,"HN_25cm_change"] <- data_mtx[,43]
      existing_data[existing_rec,"CinB_25cm_slope"] <- data_mtx[,44]
      existing_data[existing_rec,"CinB_25cm_change"] <- data_mtx[,45]
      existing_data[existing_rec,"CinH_25cm_slope"] <- data_mtx[,46]
      existing_data[existing_rec,"CinH_25cm_change"] <- data_mtx[,47]
      existing_data[existing_rec,"CinBtoH_25cm_slope"] <- data_mtx[,48]
      existing_data[existing_rec,"CinBtoH_25cm_change"] <- data_mtx[,49]
      existing_data[existing_rec,"SOC_25cm_slope"] <- data_mtx[,50]
      existing_data[existing_rec,"SOC_25cm_change"] <- data_mtx[,51]
      existing_data[existing_rec,"SoilT_25cm_slope"] <- data_mtx[,52]
      existing_data[existing_rec,"SoilT_25cm_change"] <- data_mtx[,53]
      data_mtx <- existing_data
    } else {
      data_mtx <- rbind(existing_data,data_mtx)
    } # end if record exists
  } # end if file exists
  
  write.table(data_mtx,file=paste0(outfile_name),
              col.names=c("Model","Climate_Scenario","Mgmt_Scenario",
                          "Scenario_Name","Scenario_Abbrev",
                          "SW_20cm_slope","SW_20cm_change",
                          "SW_40cm_slope","SW_40cm_change",
                          "SW_60cm_slope","SW_60cm_change",
                          "WFPS_10cm_slope","WFPS_10cm_change",
                          "WFPS_20cm_slope","WFPS_20cm_change",
                          "WFPS_40cm_slope","WFPS_40cm_change",
                          "NO3_slope","NO3_change",
                          "N2O_slope","N2O_change",
                          "WFPS_0cm_slope","WFPS_0cm_change",
                          "WFPS_2cm_slope","WFPS_2cm_change",
                          "WFPS_5cm_slope","WFPS_5cm_change",
                          "SoilT_slope","SoilT_change",
                          "CH4_slope","CH4_change",
                          "CI_slope","CI_change",
                          "SW_25cm_slope","SW_25cm_change",
                          "BC_25cm_slope","BC_25cm_change",
                          "BN_25cm_slope","BN_25cm_change",
                          "HC_25cm_slope","HC_25cm_change",
                          "HN_25cm_slope","HN_25cm_change",
                          "CinB_25cm_slope","CinB_25cm_change",
                          "CinH_25cm_slope","CinH_25cm_change",
                          "CinBtoH_25cm_slope","CinBtoH_25cm_change",
                          "SOC_25cm_slope","SOC_25cm_change",
                          "SoilT_25cm_slope","SoilT_25cm_change"),
              row.names=F,sep=",")
  
}