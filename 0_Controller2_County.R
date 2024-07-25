#######################################
# File: "0_Controller"
# Author: "Ellen Maas and then Adam Dixon"
# Date: "June 2024"
# Description: This script is the control file for the project. 
# It generates all the data input files for all models for a 
# given site. It includes weather, soil, and management. It 
# creates the inputs for Daycent, and LDNDC, then runs 
# these three models. Then the process is repeated for Millennial, which 
# use Daycent and LDNDC output for their input."
#
#######################################

  
print("Starting 0_Controller2_County.R")

site_id <- 0 # this is important for LDNDC management file for some reason


#*************************************************************
#*************************************************************
# Setup observational data variables and global constants------------------
#*************************************************************
#*************************************************************

## These are used in multiple functions.
source(paste0("0_Observations_and_constants_County.R"), local = TRUE)

#*************************************************************
#*************************************************************
#* Load soils and climate data for Daycent and LDNDC
# Note: Soybeans have no yield results prior to 1900 because they weren't grown in US and the model is set up for corn during that time

# Soil data
source("2_Create_soil_data-setup2_County.R", local = TRUE) # some soil vars needed for Daycent and LDNDC, so keeping out of loop below

print(paste0("*************Creating soils and climate data for Daycent and LDNDC ****************"))

# daycent climate
source('1_create_county_climate_wth_file_County.R', local = TRUE)
source('1_Create_weather_input_files-Daycent_County_v2.R', local = TRUE)

source("1_Create_weather_input_files-LDNDC_County.R", local = TRUE)
source("2_Create_soil_data-LDNDC_County.R", local = TRUE)
#*************************************************************
#*#*************************************************************


#*************************************************************
#*************************************************************
# Run models --------------------------------------------------------------
#*************************************************************
#*************************************************************

# This for loop sets up mgmt event files for each crop and mgmt scenario for Daycent, LDNDC, and Millennial
# It then runs the models. 'If' statements are included to control which models run and to check if output files already exist, which was helpful for
# development and debugging.
for (c in crops_){
  print(county_print_marker) # print out for debugging with console print out
  # First check crop amount in county so it's not analyzed unnecessarily.
  crop_amount<-county_data[,eval(paste0(c, "_ha"))] # get crop amount in county
  # Rotation ha is the same as Maize, since if you can grow corn you can grow soybeans
  # Rotation has a column in the county_data csv so that it is checked here.
  if (crop_amount<1){
    print(paste0("*************** skipping because less than 1 ha of ", c, " in county **********************************"))
    file.create(file.path(results_path, paste0("1 - note - there is less than 1 ha ", c, " in county ", county_geoid,".txt")))
    
    # next # if crop amount is less than 1 ha in county, then skip
    # don't need 'next' as we just want to continue on to ldndc, then millennial. next will skip to next scenario entirely
  } else {
    for (m in mgmt_scenario_nums){

      crop<-c
      mgmt_scenario_num<-m
      scenario_name <- paste0(clim_scenario_num,"_", m)
      scenario_name2<-paste0(scenario_name, "_", crop) # scenario code and crop name, used for output file 
      
      # output results path for Daycent
      daycent_annual_out<-file.path(results_path, paste0("Daycent_annual_results_compilation_", scenario_name2,".csv"))
      daycent_daily_out<-file.path(results_path, paste0("Daycent_daily_results_compilation_",scenario_name2,".csv"))
      

      
      if(identical(run_Daycent, TRUE)) {
        cat(county_print_marker)
        # check if results file already exists and only want results
        if(file.exists(daycent_daily_out) & identical(results_only, FALSE)){ 
          # & nrow(fread(model_path))> 200# # check if all rows have been reported; note this didn't work well
          print(paste0("*************Daycent results already exist for: ", scenario_name2, " ... skipping...****************"))
          # next
        } else{

          print("*****writing site data Daycent")
          # Site data - need this first before soil_site
          source("2_1_Create_site_file-Daycent_County.R", local = TRUE)

          # Soils
          print("*****writing soils data")
          source("2_Create_soil_site_data-Daycent_County.R", local = TRUE)
          
          # Create mgmt input
          source(paste0("3_Create_management_input_files-Daycent_County_Scenarios.R"), local = TRUE)
          
          print("...writing schedule file...")
          
          # Pair scenarios with schedule file, combine as vector
          crop_schedule<-c(get(paste0(tolower(crop), '_1')), # opening set of schedule file blocks
                           get(paste0(tolower(crop), '_scenario_', m)), # scenario-specific schedule file blocks
                           
                           output_sch<-file.path(daycent_path2 , paste0("sched_base_", scenario_name, "_", crop, ".sch")))
          
          # write to sch file
          writeLines(crop_schedule, output_sch) # schedule file name, e.g. schedule_file_maize
          
          # Run Daycent
          print(paste0("*************running Daycent for: ", scenario_name2, "****************"))
          source(paste0("Daycent/Daycent_run_controller.R"), local = TRUE)
          source(paste0("9_Results_Daycent-setup_County.R"), local=TRUE) #TODO AD set this up?
          
        
        } # end else file exists
      } # end if run_Daycent
        
      # LDNDC output files
      ldndc_annual_out<-file.path(results_path, paste0("LDNDC_annual_results_compilation_", scenario_name2,".csv"))
      ldndc_daily_out<-file.path(results_path, paste0("LDNDC_daily_results_compilation_",scenario_name2,".csv"))
      

      
      if(identical(run_LDNDC,TRUE)) {
        cat(county_print_marker)
        if(file.exists(ldndc_daily_out)){ 
          # & nrow(fread(model_path))> 200# # check if all rows have been reported; note this didn't work well
          print(paste0("*************LDNDC results already exist for: ", scenario_name2, " ... skipping...****************"))
          # next
        } else{

          
          print(paste0("*************running LDNDC for: ", scenario_name2, "****************"))
          print(paste0("*************Create_management_input_files-LDNDC_County: ", scenario_name2, "****************"))
          source(paste0("3_Create_management_input_files-LDNDC_County.R"), local = TRUE)
          
          print(paste0("*************4 Create_additional_files-LDNDC_County: ", scenario_name2, "****************"))
          source(paste0("4_Create_additional_files-LDNDC_County.R"), local = TRUE)
          
          source(file.path(ldndc_run_path, "run_LDNDC.R"))
          
          source('9_Results_LDNDC-setup_County.R', local = TRUE)
          

        
        } # end of run_LDNDC
      } # end of if statement checking if LDNDC model results already exist
      
      # milliennial output files
      mill_daily_out<-file.path(results_path, paste0("Millennial_daily_results_compilation_",scenario_name2,".csv"))
      mill_annual_out<-file.path(results_path, paste0("Millennial_annual_results_compilation_",scenario_name2,".csv"))
        
      if(identical(run_Millennial,TRUE)) {
        cat(county_print_marker)
        
        if(file.exists(file.path(results_path, mill_daily_out))){ 
          # & nrow(fread(model_path))> 200# # check if all rows have been reported; note this didn't work well
          print(paste0("*************Millennial results already exist for: ", scenario_name2, " ... skipping...****************"))
          
        } else{
          print(paste0("*************Running Millenial for: ", scenario_name2, "****************"))
          # # Millennial
          source(paste0("3_Create_management_input_files-Millennial_County.R"), local = TRUE)
          source(paste0(mill_path,"run_Millennial.R"), local = TRUE)
          
          # There is very minimal processing to the millennial output.
          source(paste0("9_Results_Millennial-setup_County.R"), local = TRUE)
        } # end of if statement checking if Millennial model results already exist
      } # end of Millenial if statement
      
      # Daycent has already been run, update results only. Useful for debugging.
      if(identical(results_only, TRUE)){
        print(paste0("*************generation results table for: ", scenario_name2, "****************"))
        # Table generation script
        source('9_Results_Daycent-setup_County.R', local = TRUE)
        source('9_Results_LDNDC-setup_County.R', local = TRUE)
        source(paste0("9_Results_Millennial-setup_County.R"), local = TRUE)
      }
    } # end of mgmt_scenario_nums loop 
  } # end of else statement if crop amount is less than 1 ha in county
} # end of crops loop




#*************************************************************
