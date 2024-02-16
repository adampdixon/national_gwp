# Daycent results at county level mapped out
# January 15 2023
# Set workspace
if (Sys.info()['sysname'] == "Linux"){ 
  if(Sys.info()['user']=='ap') {
    master_path<-'/home/ap/Documents/GitHub/national_gwp'
    results_path<-'/home/ap/Daycent_results'
    crop_area_path<-file.path(master_path, 'Data', 'County_start', 'main_county_crops.csv')
    output_figs<-'/home/ap/figs'
    county_number<-1
    Test <- TRUE # if TRUE, only run county, filtered below
    # crop<- "Maize"   #Maize #Soybeans", "Wheat", "Cotton
    Glade=FALSE
    print("************************************")
    print("*****Using linux mint *********")
    cat("date and time are ")
    print(Sys.time())
  } else {
    master_path<-'/glade/derecho/scratch/apdixon/national_gwp'
    results_path<-'/glade/derecho/scratch/apdixon/national_gwp_results'
    crop_area_path<-file.path(master_path, 'Data', 'County_start', 'main_county_crops.csv')
    output_figs<-'/glade/derecho/scratch/apdixon/national_gwp_figs'
    Test <- FALSE # if TRUE, only run county, filtered below
    # county_number<-args[2]
    Glade=TRUE
    print("************************************")
    print("*****Using NCAR *********")
    print("***** SCRATCH SPACE *********")
    cat("date and time are ")
    print(Sys.time())
  }
}

library(tigris)
options(tigris_use_cache = TRUE)
library(ggplot2)
library(sf)
library(dplyr)
library(scales)
library(gridExtra)
library(grid)
library(png)
# library(stringr)
# library(rnassqs)
# NASSQS_TOKEN="25DCA0AC-9720-345F-8989-49876C2D6C30"
# nassqs_auth(key = NASSQS_TOKEN)

# home_folder<-'/home/ap/Documents/GitHub/national_gwp/Data/County_start'
# cdl_count<-'/home/ap/CDL/main_county_crops.csv'

daycent_results<-function(State=NULL, Year=NULL, Crop, scenario, Results_path, Crop_area_path){
  # State: Full name capitalized, eg. Georgia
  # Crop options: Maize, Wheat, Cotton, Soybean
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

  crop_area_var<-paste0(Crop,'_ha')
  
  crop_area_df<-read.csv(crop_area_path)%>%
    select(GEOID, eval(crop_area_var))%>%
    as_tibble()
  
  # gets<-grep(state, csv_list)
  # csv_list<-csv_list[gets]
  # files<-files[gets]
  
  # read csvs in list and place in df
  df<-data.frame()
  for (i in 1:length(csv_list)){
    # print(paste0("Reading ", csv_list[i]))
    
    # Split file name to get geoid
    # geoid<-as.integer(strsplit(files[i], "_")[[1]][3])
    geoid<-as.integer(strsplit(basename(dirname(csv_list[i])), "_")[[1]][3])
    

    r_county<-read.csv(csv_list[i])
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
  
  crop_var<-paste0(Crop,'Yld_Mgha')

  df2<-left_join(df, crop_area_df, by=c("GEOID"="GEOID"))%>% # add crop area
    mutate(Cty_Crp_Yld_Mg=get(crop_var)*get(crop_area_var))%>% # calculate crop area in county multiplied by yield per ha
    mutate(Cty_SOC_Mg=SOC_Mgha*get(crop_area_var))%>% # calculate SOC in crop area in county 
    select(GEOID, year, eval(crop_var), eval(crop_area_var), SOC_Mgha, Cty_Crp_Yld_Mg, Cty_SOC_Mg, climate_scenario_num, mgmt_scenario_num)%>%
    arrange(GEOID)%>%
    as_tibble()
  return(df2)
  
}


# results<-daycent_results(Year=2017, Crop = 'Soybean', scenario = 1, Results_path=results_path, Crop_area_path=crop_area_path)
# State='Nebraska'; Year=2017; Crop = 'Soybean'; scenario = 1; Results_path=results_path; Crop_area_path=crop_area_path

############ GET STATE LEVEL MAPS


state_map<-function(){
  the_state<-counties() %>%
    filter(STATEFP %in% unique(substr(results$GEOID, 1, 2)))%>%
    mutate(GEOID2=as.integer(GEOID))
  
  # NOW DO SPATIAL JOIN
  
  all_data_sp<-left_join(results, the_state, by=c("GEOID"="GEOID2"))
  
  crop_state_plot<-ggplot() +
    geom_sf(data = the_state, aes(geometry = geometry), linewidth = 0) +
    geom_sf(data = all_data_sp, aes(geometry = geometry, fill = Cty_Crp_Yld_Mg), linewidth = 0) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
    theme_bw()
  
  SOC_state_plot<-ggplot() +
    geom_sf(data = the_state, aes(geometry = geometry), size = 0) +
    geom_sf(data = all_data_sp, aes(geometry = geometry, fill = Cty_SOC_Mg), linewidth = 0) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
    theme_bw()
  
  return(grid.arrange(crop_state_plot, SOC_state_plot, ncol = 2))
}




############ GET MAPS FOR WHOLE US


national_map<-function(Year_, Crop_, scenario_, Output){
  # call results from above function
  result<-results<-daycent_results(Year=Year_, Crop = Crop_, scenario = scenario_, Results_path=results_path, Crop_area_path=crop_area_path)
  head(result)
  nrow(result)
  
  lower_48<-sprintf("%02d" , c(1,4,5,6,8,9,10,11,12,13,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
                               30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56))
  
  counties<-counties() %>%
    filter(STATEFP %in% lower_48)%>%
    mutate(GEOID2=as.integer(GEOID))
  
  # NOW DO SPATIAL JOIN
  
  all_data_sp<-left_join(results, counties, by=c("GEOID"="GEOID2"))
  
  # For map titles and file naming
  crop<-str_to_lower(strsplit(colnames(all_data_sp[4]), "_")[[1]][1])
  year<-all_data_sp$year[1]
  
  scenario<-ifelse(result$mgmt_scenario_num[1]==1, "Monocropping", 
                   ifelse(result$mgmt_scenario_num[1]==2, "No-till",
                          ifelse(result$mgmt_scenario_num[1]==3, "Cover crop species mix",
                                 ifelse(result$mgmt_scenario_num[1]==4, "Cover crop rye",
                                        ifelse(result$mgmt_scenario_num[1]==5, "Cover crop legume",
                   "No-till and cover crop mix"
                   )))))
  
  us<-ggplot() +
    geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
    geom_sf(data = all_data_sp, aes(geometry = geometry, fill = Cty_Crp_Yld_Mg), linewidth = 0) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
    labs(title = paste0("Daycent county ", crop, " yield (Mg) - ", year),
            subtitle = paste0(scenario, " scenario")) +
    theme_bw()

  us_soc<-ggplot() +
    geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
    geom_sf(data = all_data_sp, aes(geometry = geometry, fill = Cty_SOC_Mg), linewidth = 0) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
    labs(title= paste0("Daycent county ", crop, " field SOC (Mg) - ", year),
            subtitle = paste0(scenario, " scenario")) +
    theme_bw()
  
  output<-grid.arrange(us, us_soc, ncol = 2)
  
  # ggsave(file.path(output_figs, paste0(crop, "_county_yield_Mg_", year, ".png")),plot=output, dpi=300)
  ggsave(file.path(Output, paste0(crop, "_", year, ".png")), plot=output, dpi=300, width = 10, height = 5)
  
  return(output)

}


# Year_ = 2017; Crop_ = 'Maize'; scenario_ = 1

# out<-national_map(Year_ = 2017, Crop_ = 'Maize', scenario_ = 1, Output = output_figs)






############ GET MAPS FOR WHOLE US AND ALL SCENARIOS


national_map_all_scenarios<-function(Year_, Crop_, Output){
  
  for(i in 1:6){
    # call results from above function
    result<-results<-daycent_results(Year=Year_, Crop = Crop_, scenario = i, Results_path=results_path, Crop_area_path=crop_area_path)
    head(result)
    nrow(result)
    
    lower_48<-sprintf("%02d" , c(1,4,5,6,8,9,10,11,12,13,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
                                 30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56))
    
    counties<-counties() %>%
      filter(STATEFP %in% lower_48)%>%
      mutate(GEOID2=as.integer(GEOID))
    
    # NOW DO SPATIAL JOIN
    
    all_data_sp<-left_join(results, counties, by=c("GEOID"="GEOID2"))
    
    # For map titles and file naming
    crop<-tolower(strsplit(colnames(all_data_sp[4]), "_")[[1]][1])
    year<-all_data_sp$year[1]
    
    scenario<-ifelse(result$mgmt_scenario_num[1]==1, "Monocropping", 
                     ifelse(result$mgmt_scenario_num[1]==2, "No-till",
                            ifelse(result$mgmt_scenario_num[1]==3, "Cover crop species mix",
                                   ifelse(result$mgmt_scenario_num[1]==4, "Cover crop rye",
                                          ifelse(result$mgmt_scenario_num[1]==5, "Cover crop legume",
                                                 "No-till and cover crop mix"
                                          )))))
    
    # number in i so they order themselves
    output_graph_set<-file.path(Output, paste0(i, "_", crop, "_", year, "_scenario_", tolower(gsub(" ", "_", scenario)), ".png"))
    
    if(file.exists(output_graph_set)){
      next
    } else {
      # set up if statement so that title on top plot is only printed once
      if(identical(scenario, "Monocropping")){ 
        us<-ggplot() +
          geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
          geom_sf(data = all_data_sp, aes(geometry = geometry, fill = Cty_Crp_Yld_Mg), linewidth = 0) +
          scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
          labs(title = paste0("Daycent county ", crop, " yield (Mg) - ", year),
               subtitle = paste0(scenario, " scenario")) +
          # theme(plot.margin=unit(c(-0.30,0,0,0), "null")) + # remove margin around plot
          theme_bw()
        
        us_soc<-ggplot() +
          geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
          geom_sf(data = all_data_sp, aes(geometry = geometry, fill = Cty_SOC_Mg), linewidth = 0) +
          scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
          labs(title= paste0("Daycent county ", crop, " field SOC (Mg) - ", year),
               subtitle = paste0(scenario, " scenario")) +
          # theme(plot.margin=unit(c(-0.30,0,0,0), "null")) + # remove margin around plot
         theme_bw()
        
        out<-arrangeGrob(us, us_soc, ncol = 2, widths = c(5,5), heights = c(3,3))

        
        # ggsave(file.path(output_figs, paste0(crop, "_county_yield_Mg_", year, ".png")),plot=output, dpi=300)
        ggsave(filename = output_graph_set, plot=out, dpi=300, width = 10, height = 5)
      } else {
        
        us<-ggplot() +
          geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
          geom_sf(data = all_data_sp, aes(geometry = geometry, fill = Cty_Crp_Yld_Mg), linewidth = 0) +
          scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
          labs(
            subtitle = paste0(scenario, " scenario")) +
          # theme(plot.margin=unit(c(-0.30,0,0,0), "null")) + # remove margin around plot
          theme_bw()
        
        us_soc<-ggplot() +
          geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
          geom_sf(data = all_data_sp, aes(geometry = geometry, fill = Cty_SOC_Mg), linewidth = 0) +
          scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
          labs(
            subtitle = paste0(scenario, " scenario")) +
          # theme(plot.margin=unit(c(-0.30,0,0,0), "null")) +# remove margin around plot
          theme_bw()
        
        out<-arrangeGrob(us, us_soc, ncol = 2, widths = c(5,5), heights = c(3,3))
      }
      
      # ggsave(file.path(output_figs, paste0(crop, "_county_yield_Mg_", year, ".png")),plot=output, dpi=300)
      ggsave(file = output_graph_set, plot=out, dpi=300, width = 10, height = 5)
    } # end of if else statement
  } # end of for loop

  # Save to a single file
  # plots2 <- lapply(ll <- list.files(Output, pattern='maize_2017_scenario', full.names = T), function(x){
  #   img <- as.raster(readPNG(x))
  #   rasterGrob(img, interpolate = FALSE)
  # })
  # 
  # ggsave(file.path(Output, "Plots_Combined%03d.png"),
  #        marrangeGrob(grobs = plots2, nrow=6, ncol=1, top=NULL), device = "png", 
  #        width = 10, height = 30, dpi=300)
  
  # return(output)
} # end of function

# Year_ = 2017; Crop_ = 'Maize'; scenario_ = 2; Output = output_figs

national_map_all_scenarios(Year_ = 2017, Crop_ = 'Maize', Output = output_figs)



# 
# 
# NASS_data<-left_join(counties, crop_area2, by =c('GEOID2'='GEOID'))
# 
# us<-ggplot() +
#   geom_sf(data = counties, aes(geometry = geometry)) +
#   geom_sf(data = NASS_data, aes(geometry = geometry, fill = Value)) +
#   scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
#   theme_bw()
# 
# us
# 
# 
# nass_corn<-read.csv('/home/ap/Documents/GitHub/national_gwp/Data/County_start/NASS/corn_2017.csv')%>%as_tibble()%>%
#   mutate(GEOID=paste0(sprintf("%02d", State.ANSI), sprintf("%03d", County.ANSI)), Acres = as.integer(Value))%>%
#   select(Year, GEOID, Data.Item, Domain, Acres)
# 
# nass_corn2<-left_join(nass_corn, counties, by =c('GEOID'='GEOID'))
# 
# corn<-ggplot() +
#   # geom_sf(data = counties, aes(geometry = geometry)) +
#   geom_sf(data = nass_corn2, aes(geometry = geometry, fill = Acres)) +
#   # scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
#   theme_bw()
# 
# corn
# 
# 
# 
# ##########################################################################################
# ##########################################################################################
# ##########################################################################################
# 
# daycent_map<-function(State=NULL, Year_=NULL, crop=NULL, Results_path=results_path){
#   ############ GET CROP ACREAGE FOR WHOLE US
#   result<-daycent_results(state=NULL, Year=Year_, Results_path=results_path)
#   head(result)
#   nrow(result)
#   
#   counties<-st_read(file.path(home_folder, "county_shp", "ne_conus_counties.shp")) %>%
#     mutate(GEOID2=as.integer(CODE_LOCAL))%>%
#     select(GEOID2, REGION, CODE_LOCAL, NAME, AREA_SQKM)
#   
#   crop_area2<-crop_area%>%
#     # filter(state_name=="GEORGIA")%>%
#     filter(statisticcat_desc=="AREA PLANTED", commodity_desc == crop)%>%
#     mutate(GEOID=as.integer(paste0(state_ansi, county_ansi)))%>%
#     filter(GEOID > 1000)%>% # this removes the "overall" row (which doesn't have a GEOID)
#     select(statisticcat_desc, commodity_desc, GEOID, state_name, county_name, 
#            state_ansi, county_ansi, source_desc, Value, unit_desc)%>%
#     arrange(GEOID)%>%
#     as_tibble()
#   
#   counties_df<-st_drop_geometry(counties)%>%as_tibble()
#   
#   # #Get Daycent crop code  
#   # if(crop=="CORN"){
#   #   cr<-MaizeYld_Mgha
#   # }  
#   # if(crop=="SOYBEANS"){
#   #   cr<-SoyYld_Mgha
#   # }  
#   # if(crop=="WHEAT"){
#   #   cr<-WheatYld_Mgha
#   # }
#   
#   ta<-0.446089561 # TONS/ACRE conversion from Mg/ha
#   # THIS GETS ALL THE DATA
#   all_data<-left_join(crop_area2, result, by="GEOID")%>%
#     mutate(TONS_ACRE=ifelse(commodity_desc=="CORN", MaizeYld_Mgha*ta*Value, 
#                             ifelse(commodity_desc=="SOYBEANS", SoyYld_Mgha*ta*Value, 
#                                    ifelse(commodity_desc=="WHEAT", WheatYld_Mgha*ta*Value, NA
#                                    ))))%>%
#     select(year, state_name, county_name, GEOID, commodity_desc, Value, unit_desc, TONS_ACRE)
#   
#   
#   # NOW DO SPATIAL JOIN
#   all_data_sp<-left_join(all_data, counties, by=c("GEOID"="GEOID2"))
#   
#   
#   us<-ggplot() +
#     geom_sf(data = counties, aes(geometry = geometry)) +
#     geom_sf(data = all_data_sp, aes(geometry = geometry, fill = TONS_ACRE)) +
#     scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
#     ggtitle(paste0("Daycent ", crop, " yield (tons/acre) - ", Year_))+
#     theme_bw()
#   
#   return(us)
# }
# 
# 
# corn<-daycent_map(State=NULL, Year_=2017, crop='CORN', Results_path=results_path)
# soy<-daycent_map(State=NULL, Year_=2018, crop='SOYBEANS', Results_path=results_path)
# wheat<-daycent_map(State=NULL, Year_=2019, crop='WHEAT', Results_path=results_path)
# 
# ggsave('/home/ap/Documents/GitHub/scratch_figs/us_corn.png',plot=corn, dpi=300)
# ggsave('/home/ap/Documents/GitHub/scratch_figs/us_soy.png',plot=soy, dpi=300)
# ggsave('/home/ap/Documents/GitHub/scratch_figs/us_wheat.png',plot=wheat, dpi=300)
# 
# 
# 
# 
# 
# 
# daycent_map_SOC<-function(State=NULL, Year_=NULL, crop=NULL, Results_path=results_path){
#   ############ GET CROP ACREAGE FOR WHOLE US
#   result<-daycent_results(state=NULL, Year=Year_, Results_path=results_path)
#   head(result)
#   nrow(result)
#   
#   counties<-st_read(file.path(home_folder, "county_shp", "ne_conus_counties.shp")) %>%
#     mutate(GEOID2=as.integer(CODE_LOCAL))%>%
#     select(GEOID2, REGION, CODE_LOCAL, NAME, AREA_SQKM)
#   
#   crop_area2<-crop_area%>%
#     # filter(state_name=="GEORGIA")%>%
#     filter(statisticcat_desc=="AREA PLANTED", commodity_desc == crop)%>%
#     mutate(GEOID=as.integer(paste0(state_ansi, county_ansi)))%>%
#     filter(GEOID > 1000)%>% # this removes the "overall" row (which doesn't have a GEOID)
#     select(statisticcat_desc, commodity_desc, GEOID, state_name, county_name, 
#            state_ansi, county_ansi, source_desc, Value, unit_desc)%>%
#     arrange(GEOID)%>%
#     as_tibble()
# 
#   ta<-0.446089561 # TONS/ACRE conversion from Mg/ha
#   # THIS GETS ALL THE DATA
#   all_data<-left_join(crop_area2, result, by="GEOID")%>%
#     mutate(TONS_ACRE=SOC_Mgha*ta)%>%
#     select(year, state_name, county_name, GEOID, commodity_desc, Value, unit_desc, TONS_ACRE)
#   
#   
#   # NOW DO SPATIAL JOIN
#   all_data_sp<-left_join(all_data, counties, by=c("GEOID"="GEOID2"))
#   
#   
#   us<-ggplot() +
#     geom_sf(data = counties, aes(geometry = geometry)) +
#     geom_sf(data = all_data_sp, aes(geometry = geometry, fill = TONS_ACRE)) +
#     scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
#     ggtitle(paste0("Daycent ", crop, " field SOC (tons/acre) - ", Year_))+
#     theme_bw()
#   
#   return(us)
# }
# 
# corn_soc<-daycent_map_SOC(State=NULL, Year_=2017, crop='CORN', Results_path=results_path)
# soy_soc<-daycent_map_SOC(State=NULL, Year_=2018, crop='SOYBEANS', Results_path=results_path)
# wheat_soc<-daycent_map_SOC(State=NULL, Year_=2019, crop='WHEAT', Results_path=results_path)
# 
# ggsave('/home/ap/Documents/GitHub/scratch_figs/us_corn_SOC.png',plot=corn_soc, dpi=300)
# ggsave('/home/ap/Documents/GitHub/scratch_figs/us_soy_SOC.png',plot=soy_soc, dpi=300)
# ggsave('/home/ap/Documents/GitHub/scratch_figs/us_wheat_SOC.png',plot=wheat_soc, dpi=300)
# 
# 
# 














# 
# 
# 
# 
# 
# 
# 
# 
# # get map for state or lower 48
# 
# get_Daycent_map<-function(State=NULL, Crop_area=crop_area, Crop=NULL){
#   
#   ############ GET CROP ACREAGE
#   
#   if(!is.null(State)){
#     crop_area2<-Crop_area%>%
#       filter(state_name==State)
#     
#   if(!is.null(Crop)){
#     crop_area2<-filter(statisticcat_desc=="AREA PLANTED", commodity_desc == "CORN")%>%
#       mutate(GEOID=as.integer(paste0(state_ansi, county_ansi)))%>%
#       filter(GEOID > 1000)%>% # this removes the "overall" row
#       select(statisticcat_desc, commodity_desc, GEOID, state_name, county_name, state_ansi, county_ansi, source_desc, Value, unit_desc)%>%
#       arrange(GEOID)%>%
#       as_tibble()
#   
# 
#   
#   # also read crop area data
#   
#   # State2<-toupper(gsub("_", " ", state)) # this should get all the states
#   # crop_area_df<-crop_area2%>%
#   #   filter(state_name==State2)
#   
#   counties_df<-st_drop_geometry(counties)%>%as_tibble()%>%mutate(GEOID2=as.integer(GEOID))%>%
#     select(STATEFP, COUNTYFP, GEOID, GEOID2, NAME)
#   
#   # THIS GETS ALL THE DATA
#   all_data<-left_join(crop_area2, result, by="GEOID")%>%
#     mutate(TONS_ACRE=MaizeYld_Mgha*0.446089561)%>%
#     select(year, MaizeYld_Mgha:SOC_Mgha, state_name, county_name, GEOID, commodity_desc, Value, unit_desc, TONS_ACRE)
#   
#   all_data
#   
#   
#   # NOW DO SPATIAL JOIN
#   
#   all_data_sp<-left_join(all_data, counties, by=c("GEOID"="GEOID2"))
#   
#   
#   ggplot() +
#     geom_sf(data = counties, aes(geometry = geometry)) +
#     geom_sf(data = all_data_sp, aes(geometry = geometry, fill = TONS_ACRE)) +
#     scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
#     theme_bw()
# }
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
