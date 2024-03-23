# Retrieve daycent results of a given year at county level to CSV
# For national maps
# Feb 17 2024
# Set workspace
if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    master_path<-'/home/ap/Documents/GitHub/national_gwp'
    results_path<-'/home/ap/Daycent_results'
    crop_area_path<-file.path(master_path, 'Data', 'County_start', 'main_county_crops.csv')
    output_figs<-'/home/ap/figs'
    
    args <- commandArgs(trailingOnly = TRUE)
    scenario_arg<-args[2]

    print("************************************")
    print("*****Using linux mint *********")
    cat("date and time are ")
    print(Sys.time())
  } else {
    master_path<-'/glade/derecho/scratch/apdixon/national_gwp'
    results_path<-'/glade/derecho/scratch/apdixon/national_gwp_results'
    crop_area_path<-file.path(master_path, 'Data', 'County_start', 'main_county_crops.csv')
    output_figs<-'/glade/derecho/scratch/apdixon/national_gwp_figs'

    args <- commandArgs(trailingOnly = TRUE)
    scenario_arg<-args[2]

    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
    cat("date and time are ")
    print(Sys.time())
  }
}

#####################
year_<-2050
###################

print(paste0("Scenario to be run is ", scenario_arg))

# library(tigris)
# options(tigris_use_cache = TRUE)
# library(ggplot2)
# library(sf)
library(dplyr)
# library(scales)
# library(gridExtra)
# library(grid)
# library(png)
# library(stringr)
# library(rnassqs)
# NASSQS_TOKEN="25DCA0AC-9720-345F-8989-49876C2D6C30"
# nassqs_auth(key = NASSQS_TOKEN)

# home_folder<-'/home/ap/Documents/GitHub/national_gwp/Data/County_start'
# cdl_count<-'/home/ap/CDL/main_county_crops.csv'

date<-gsub("-", "", Sys.Date())

daycent_results<-function(State=NULL, Year=NULL, Crop, scenario, Results_path, Crop_area_path){
  # State: Full name capitalized, eg. Georgia
  # Crop options: Maize, Wheat, Cotton, Soybean, Rotation (maize and soybean)
  # Scenario options: 1, 2, 3, 4, 5, 6
  
  print(paste0("State: ", State, ", Year: ", Year, ", Crop: ", Crop, ", scenario: ", scenario))
  
  file_name<-paste0("Annual_results_compilation_1_", scenario, "_", Crop, "_Daycent.csv")
  
  if (!is.null(State)){ # get State only results
    dir_list<-dir(Results_path, pattern = State, full.names = T)
    print("Getting State only results")
    print(State)
  } else {
    dir_list<-dir(Results_path, full.names = T)
    print("Getting results from all States")
  }
  
  csv_list<-unlist(lapply(dir_list, function(x) list.files(x, full.names=T, recursive = T, pattern = file_name))) # FULL NAMES
  
  files<-unlist(lapply(dir_list, function(x) list.files(x, full.names=F, recursive = T, pattern = file_name))) # NOT FULL NAMES
  
  # /Results_GEOID_42091_Pennsylvania/Annual_results_compilation_1_1_Cotton_Daycent.csv
  
  # TODO decide if going to worry about state. Probs best is to worry about Crop + scenario
  
  if(identical(Crop, "Rotation")){ # if rotation, set area to same as maize
    crop_area_var<-'Maize_ha'
  } else{
    crop_area_var<-paste0(Crop,'_ha')
  }
  
  crop_area_df<-read.csv(crop_area_path)%>% # whole CONUS
    select(GEOID, eval(crop_area_var))%>%
    as_tibble()
  
  # gets<-grep(state, csv_list)
  # csv_list<-csv_list[gets]
  # files<-files[gets]
  
  # read csvs in list and place in df
  df<-data.frame()
  for (d in 1:length(csv_list)){
    # print(paste0("Reading ", csv_list[i]))
    
    # Split file name to get geoid
    # geoid<-as.integer(strsplit(files[i], "_")[[1]][3])
    geoid<-as.integer(strsplit(basename(dirname(csv_list[d])), "_")[[1]][3])
    
    
    r_county<-as_tibble(read.csv(csv_list[d])) 
    if (!is.null(Year)){
      r_county<-filter(r_county, year==Year)
    }
    # Add county GEOID
    r_county$GEOID<-geoid
    
    df<-rbind(df, r_county)
  }
  # # Add crop totals
  # 
  # if(!is.null(Crop)){
  #   crop_area2<-filter(crop_area_df, commodity_desc==Crop)
  # }
  
  if(identical(Crop, "Rotation")){
    if(is.na(df$MaizeYld_Mgha)[1]){ # if year is selected in rotation that crop is not grown, result will be NA
      crop_var<-'SoybeanYld_Mgha'
      Crop<-"Rotation"
    } else
      crop_var<-'MaizeYld_Mgha'
  } else{
    crop_var<-paste0(Crop,'Yld_Mgha')
  }
  
  
  
  df2<-left_join(df, crop_area_df, by=c("GEOID"="GEOID"))%>% # add crop area

    mutate(Cty_Crp_Yld_Mg=get(crop_var)*get(crop_area_var))%>% # calculate crop area in county multiplied by yield per ha
    mutate(Cty_SOC_Mg=SOC_Mgha*get(crop_area_var))%>% # calculate SOC in crop area in county 
    mutate(crop_yld_Mgha=get(crop_var), crop = Crop, 
           crop_ha = get(crop_area_var))%>% # convert crop yield col name to generic crop_yld to assemble table later with all crops/scenarios
    select(GEOID, year, crop, crop_yld_Mgha, crop_ha, SOC_Mgha, Cty_Crp_Yld_Mg, Cty_SOC_Mg,
           N2OEmissions_ghayr, CH4Emissions_ghayr, CO2resp_ghayr,
           climate_scenario_num, mgmt_scenario_num, model_name, scenario_name)%>%
    arrange(GEOID)%>%
    as_tibble()
  return(df2)
  
}


# results<-daycent_results(Year=2017, Crop = 'Soybean', scenario = 1, Results_path=results_path, Crop_area_path=crop_area_path)
# State='Nebraska'; Year=2017; Crop = 'Rotation'; scenario = 1; Results_path=results_path; Crop_area_path=crop_area_path

res<-data.frame()

for (i in c("Maize", "Soybean", "Wheat", "Cotton", "Rotation")){
  # if(i=="Rotation" & scenario_arg>1){ # skip rotation scenarios 2-6
  #   next
  # } else {
    print(i)
    print(scenario_arg)
    # Year=year_; Crop = i; scenario = scenario_arg; Results_path=results_path; Crop_area_path=crop_area_path
    results<-daycent_results(Year=year_, Crop = i, scenario = scenario_arg, Results_path=results_path, Crop_area_path=crop_area_path, State= 'Nebraska')
    # print(results)
    print("***********")
    res<-rbind(res, results)
  # }
}

write.csv(res, file=file.path(output_figs, 
                              paste0("all_crops_", year_, "_scenario_", scenario_arg, "_", date, ".csv")))

# res<-data.frame()
# for (i in c("Maize", "Soybean", "Wheat", "Cotton", "Rotation")){
#   for (j in 1:6){
#     if(i=="Rotation" & j>1){
#       next
#     }
#     print(i)
#     print(j)
#     results<-daycent_results(Year=2017, Crop = i, scenario = j, Results_path=results_path, Crop_area_path=crop_area_path)
#     # print(results)
#     print("***********")
#     res<-rbind(res, results)
#   }
#   
# }
# 
# write.csv(res, file=file.path(output_figs, paste0("all_crops_scenarios_2017_", date, ".csv")))
