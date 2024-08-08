#######################################
# File: "get_model_tables.R"
# Author: "Adam Dixon"
# Date: "July 2024"
# Description: Functions to fetch and organize model result tables for visualization. Daycent and LDNDC function are grouped because 
# they have similar output columns, whereas Millennial is more limited. The national and county level functions then use those model function to 
# put a table together.
#
#######################################

print("Running get_model_tables.R")

library(data.table)
library(fs) # for faster list.files
library(dplyr)


get_daycent_ldndc_df<-function(file_list, model_name, crop, scenario){
  # This function fetches model results for a given crop and scenario for both Daycent and LDNDC since the main output column 
  # names are the same.
  
  crop_scenario_df<-data.frame()
  
  cr<-crop
  
  s<-scenario
  
  # print(paste0('working on ', model_name))
  
  for (f in file_list){
    # get model/place names for table

    county_string<-basename(dirname(f))
    county_string_split<-strsplit(county_string, '_')
    GEOID<-county_string_split[[1]][3]
    State<-county_string_split[[1]][4]
    
    # print(paste0('working on ', model_name, ' in ', county_string))
    
    if(identical('Rotation', cr) | identical('Soybean', cr)){ # deal with Rotation having maize and soybean yield by pulling both columns in select below
      select_var = c('Maize', 'Soybean')
    } else {
      select_var = cr
    }

    # Use these colnames to make sure consistent between models (including Millennial)
    col_names<-c('GEOID', 'State', 'year', 'model', 'mgmt_scenario_num', 'climate_scenario',
                 'SOC_Mghayr', 'CO2resp_ghayr', 'N2OEmissions_ghayr', 'CH4Emissions_ghayr', eval(paste0(select_var, 'Yld_Mgha')), 'crop')
    
    data<-fread(f)%>%
      # mutate(SOC_Mghayr = if_any(matches("SOC")))%>% # this was needed for Daycent - delete in future versions because it should not be needed. Daycent results was updated to include SOC_Mghayr
      mutate(GEOID=GEOID, State=State,
             climate_scenario = ifelse(climate_scenario_num == 1, 'low', 'high'),
             scenario = s,
             model = model_name,
             crop = cr)%>%
      select(all_of(col_names))
    

      
    # # Do this separately so CropYld_Mgha is available in the data frame. 
    # data<-data
    
    # if(identical('Rotation', cr)){ # populate new column called 'RotationYld_Mgha' depending on which one is NA
    #   data$RotationYld_Mgha <- ifelse(is.na(data$MaizeYld_Mgha), data$SoybeanYld_Mgha, data$MaizeYld_Mgha)
    # }
    
    # combine each loop
    crop_scenario_df<-rbind(crop_scenario_df, data)
  } # end of for loop listing files
  return(crop_scenario_df)
} # end of for daycent loop


get_mill_df<-function(file_list, crop, scenario){
  # This function fetches model results for a given crop and scenario for Millennial.
  
  cr<-crop
  
  s<-scenario
  
  crop_scenario_df<-data.frame()
  
  # print(paste0('working on Millennial'))
  
  for (f in file_list){
    # get model/place names for table
    # print(paste0('working on ', f))
    county_string<-basename(dirname(f))
    county_string_split<-strsplit(county_string, '_')
    GEOID<-county_string_split[[1]][3]
    State<-paste(county_string_split[[1]][4:length(county_string_split[[1]])], collapse = ' ')
    
    # if(identical('Rotation', cr)){ # deal with Rotation having maize and soybean yield by pulling both columns in select below
    #   select_var = c('Maize', 'Soybean')
    #   data$MaizeYld_Mgha<-NA
    #   data$SoybeanYld_Mgha<-NA
    # } else {
    #   data[,eval(paste0(cr, 'Yld_Mgha'))]<-NA
    # }
    
    if(identical('Rotation', cr) | identical('Soybean', cr)){ # deal with Rotation having maize and soybean yield by pulling both columns in select below
      select_var = c('Maize', 'Soybean')
    } else {
      select_var = cr
    }
    
    
    # Copy and paste these colnames to make sure consistent between models 
    col_names<-c('GEOID', 'State', 'year', 'model', 'mgmt_scenario_num', 'climate_scenario',
                 'SOC_Mghayr', 'CO2resp_ghayr', 'N2OEmissions_ghayr', 'CH4Emissions_ghayr', eval(paste0(select_var, 'Yld_Mgha')), 'crop')
    
    data<-fread(f)%>%
      mutate(GEOID=GEOID, State=State,
             climate_scenario = ifelse(climate_scenario_num == 1, 'low', 'high'),
             # mgmt_scenario_num = s, # already there
             N2OEmissions_ghayr = NA, # Millennial does not output these, but it's helpful to include when other model tables are joined (rbind) later.
             CH4Emissions_ghayr = NA, # NOTE: that this makes the other model outputs as a character because there is no numeric NA in R
             scenario = s,
             model = 'Millennial',
             crop = cr)
    
    data[,eval(paste0(select_var, 'Yld_Mgha'))]<-NA # NOTE: that this makes the other model outputs as a character because there is no numeric NA in R
    
    data<-data%>%select(all_of(col_names))
      
    crop_scenario_df<-rbind(crop_scenario_df, data)
    
  } # end of for loop listing files
  return(crop_scenario_df)
} # end of millinnial loop


# This function is for national results
get_all_models_national_df<-function(crop_to_get){
  # crop_to_get='Maize'
  # Data placed here. As written now will need to redo each time code is run on different day
  # Make sure the correct file is loading!!!!
  data_location<-list.files(national_figs, pattern = paste0(crop_to_get, '_national_results_'), full.names = T)[1]
  print(data_location)
  if (identical(file.exists(data_location), FALSE)){
    # crop_to_get is either 'All' or vector of crop names as shown.
    
    # This function puts together all model data using the above functions.
    
    # Dir name
    r2<-dir(results_folder, recursive=F, full.names=F, pattern = 'Results_GEOID') # Full names FALSE
    # Dir name and full path
    r1<-dir(results_folder, recursive=F, full.names=T, pattern = 'Results_GEOID') # Full names TRUE
    

    crop_to_include<-crop_to_get

    
    # print(crop_to_get)
    crop_scenario_df<-data.frame()
    # cr='Rotation'
    for (cr in crop_to_include) { # crop loop # 'Soybean', 'Wheat', 'Cotton', 
      
      # print(paste0('Starting ', cr))
      for (s in 1:6){ # scenario loop
        print(paste0('working on ', cr, ', practice scenario ', s, ' table, ', 'in both climate scenarios '))
        
  
        # # DAYCENT
        # # climate scenario 1
        day_1<-list.files(r1, full.names=T, recursive=T, pattern=paste0('Daycent_annual_results_compilation_1_', s, '_', cr, '.csv'))
        # climate scenario 2
        day_2<-list.files(r1, full.names=T, recursive=T, pattern=paste0('Daycent_annual_results_compilation_2_', s, '_', cr, '.csv'))

        # LDNDC
        # climate scenario 1
        ld_1<-list.files(r1, full.names=T, recursive=T, pattern=paste0('LDNDC_annual_results_compilation_1_', s, '_', cr, '.csv'))
        # climate scenario 2
        ld_2<-list.files(r1, full.names=T, recursive=T, pattern=paste0('LDNDC_annual_results_compilation_2_', s, '_', cr, '.csv'))

        # Millennial
        # climate scenario 1
        m_1<-list.files(r1, full.names=T, recursive=T, pattern=paste0('Millennial_annual_results_compilation_1_', s, '_', cr, '.csv'))
        # climate scenario 2
        m_2<-list.files(r1, full.names=T, recursive=T, pattern=paste0('Millennial_annual_results_compilation_2_', s, '_', cr, '.csv'))
        
        # TESTING dir_ls
        # # DAYCENT
        # climate scenario 1
        # day_1<-dir_ls(r1, recurse=T, regexp=paste0('Daycent_annual_results_compilation_1_', s, '_', cr, '.csv'))
        # # climate scenario 2
        # day_2<-dir_ls(r1, recurse=T, regexp=paste0('Daycent_annual_results_compilation_2_', s, '_', cr, '.csv'))
        # 
        # # LDNDC
        # # climate scenario 1
        # ld_1<-dir_ls(r1, recurse=T, regexp=paste0('LDNDC_annual_results_compilation_1_', s, '_', cr, '.csv'))
        # # climate scenario 2
        # ld_2<-dir_ls(r1, recurse=T, regexp=paste0('LDNDC_annual_results_compilation_2_', s, '_', cr, '.csv'))
        # 
        # # Millennial
        # # climate scenario 1
        # m_1<-dir_ls(r1, recurse=T, regexp=paste0('Millennial_annual_results_compilation_1_', s, '_', cr, '.csv'))
        # # climate scenario 2
        # m_2<-dir_ls(r1, recurse=T, regexp=paste0('Millennial_annual_results_compilation_2_', s, '_', cr, '.csv'))
        
        # model output file lists
        day_list<-c(day_1, day_2); ldndc_list<-c(ld_1, ld_2); mil<-c(m_1, m_2)
        
        ###########################################################
        #################### Daycent and LDNDC ####################
        
        daycent_df<-get_daycent_ldndc_df(file_list = day_list, model_name = 'Daycent', crop=cr, scenario=s)
        ldndc_df<-get_daycent_ldndc_df(file_list = ldndc_list, model_name = 'LDNDC', crop=cr, scenario=s)
        
        #################################################          
        #################### Millennial #################
        
        mil_df<-get_mill_df(file_list = mil, crop=cr, scenario = s)
  
        colnames(daycent_df)
        colnames(ldndc_df)
        # colnames(mil_df)
        #put all model data together, they have same column names with Millennial having NAs for parameters it doesn't have
        crop_scenario_df<-rbind(crop_scenario_df, daycent_df, ldndc_df, mil_df)
        print('created crop_scenario_df.')
        
      } # end of scenario loop
    } # end of crop loop
  } else {
    crop_scenario_df<-fread(data_location)
    print('crop_scenario_df loaded from file.')
  }
  
  return(crop_scenario_df)
} # end of function


# IS THIS DEPRECATED?
# Basically the same as above just for specific county
get_county_models_df<-function(crop_to_get='All', GEOID){
  # crops_to_get is either 'All' or vector of crop names as shown
  
  geoid<-GEOID
  
  # Dir name
  r2<-dir(results_folder, recursive=F, full.names=F, pattern = paste0('Results_GEOID_', geoid, '_'))
  # Dir name and full path
  r1<-dir(results_folder, recursive=F, full.names=T, paste0('Results_GEOID_', geoid, '_'))
  
  if(identical(crop_to_get, 'All')){
    # crops to get
    crops_to_include<-c('Maize', 'Soybean', 'Wheat', 'Cotton', 'Rotation')
  } else {
    crops_to_include<-crop_to_get
  }
  
  # print(crop_to_get)
  
  # cr='Rotation'
  for (cr in crops_to_include) { # crop loop # 'Soybean', 'Wheat', 'Cotton', 
    
    # print(paste0('Starting ', cr))
    
    crop_scenario_df<-data.frame()
    
    for (s in 1:6){ # scenario loop
      # print(paste0('working on ', cr, ', practice scenario ', s, ' table, ', 'in both climate scenarios '))
      
      # county_n<-0 # counter
      
      for (i in 1:length(r2)){# table binding loop
        # DAYCENT
        # climate scenario 1
        day_1<-list.files(r1[i], full.names=T, recursive=T, pattern=paste0('Daycent_annual_results_compilation_1_', s, '_', cr, '.csv'))
        # climate scenario 2
        day_2<-list.files(r1[i], full.names=T, recursive=T, pattern=paste0('Daycent_annual_results_compilation_2_', s, '_', cr, '.csv'))
        
        # LDNDC
        # climate scenario 1
        ld_1<-list.files(r1[i], full.names=T, recursive=T, pattern=paste0('LDNDC_annual_results_compilation_1_', s, '_', cr, '.csv'))
        # climate scenario 2
        ld_2<-list.files(r1[i], full.names=T, recursive=T, pattern=paste0('LDNDC_annual_results_compilation_2_', s, '_', cr, '.csv'))
        
        # Millennial
        # climate scenario 1
        m_1<-list.files(r1[i], full.names=T, recursive=T, pattern=paste0('Millennial_annual_results_compilation_1_', s, '_', cr, '.csv'))
        # climate scenario 2
        m_2<-list.files(r1[i], full.names=T, recursive=T, pattern=paste0('Millennial_annual_results_compilation_2_', s, '_', cr, '.csv'))
        
        # model output file lists
        day_list<-c(day_1, day_2); ldndc_list<-c(ld_1, ld_2); mil<-c(m_1, m_2)
        
        ###########################################################
        #################### Daycent and LDNDC ####################
        
        daycent_df<-get_daycent_ldndc_df(file_list = day_list, model_name = 'Daycent', crop=cr, scenario=s)
        ldndc_df<-get_daycent_ldndc_df(file_list = ldndc_list, model_name = 'LDNDC', crop=cr, scenario=s)
        
        #################################################          
        #################### Millennial #################
        
        mil_df<-get_mill_df(file_list = mil, crop=cr, scenario = s)
        
      } # end of for loop listing crop rotations  
      
      colnames(daycent_df)
      colnames(ldndc_df)
      # colnames(mil_df)
      #put all model data together, they have same column names with Millennial having NAs for parameters it doesn't have
      crop_scenario_df<-rbind(crop_scenario_df, daycent_df, ldndc_df, mil_df)
      
    } # end of scenario loop
  } # end of crop loop
  
  if(identical(write, TRUE)){
    fwrite(crop_scenario_df, paste0('Results_National_', cr, '_scenario_', s, '.csv'))
  }
  
  
  
  return(crop_scenario_df)
} # end of function
