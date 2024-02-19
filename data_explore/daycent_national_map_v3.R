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

date<-gsub("-", "", Sys.Date())

############ GET MAPS FOR WHOLE US AND ALL SCENARIOS


national_map_all_scenarios<-function(Year_, Crop_, Output){
  
  
  for(i in 1:6){
    # call results from above function
    # result<-results<-daycent_results(Year=Year_, Crop = Crop_, scenario = i, Results_path=results_path, Crop_area_path=crop_area_path)
    # head(result)
    # nrow(result)
    
    result<-list.files(output_figs, pattern = "csv", full.names = TRUE)%>%
      lapply(read.csv)%>%
      bind_rows()%>%
      as_tibble()%>%
      filter(crop == Crop_, mgmt_scenario_num == i, year == Year_)%>%
      select(GEOID, year, crop, crop_yld_Mgha, crop_ha, SOC_Mgha, mgmt_scenario_num, scenario_name)
    
    print(paste0("Scenario ", i, " has ", nrow(result), " rows"))
    print(result)

    lower_48<-sprintf("%02d" , c(1,4,5,6,8,9,10,11,12,13,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
                                 30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56))
    
    counties<-counties() %>%
      filter(STATEFP %in% lower_48)%>%
      mutate(GEOID2=as.integer(GEOID))
    
    # NOW DO SPATIAL JOIN
    
    all_data_sp<-left_join(result, counties, by=c("GEOID"="GEOID2"))
    
    # number in i so they order themselves
    output_graph_set<-file.path(Output, paste0(i, "_", Crop_, "_", Year_, "_scenario_", tolower(gsub(" ", "_", i)), ".png"))
    
    # year Cty_Crp_Yld_Mg crop  all_data_sp
    
    if(file.exists(output_graph_set)){
      print(output_graph_set)
      print("File exists, skipping")
      next
    } else {
      print("Making map")     
      print(output_graph_set)
      
      gg_map<-function(scenario_){
        
        print(paste0("mapping scenario ", scenario_, " for ", Crop_, " in ", Year_))
        
        # possible columns to map: crop_yld_Mgha crop_ha SOC_Mgha
        # scenario_ = 1; crop_ = 'Maize'; year_ = 2017
        
        df<-all_data_sp
        # print(df)
          # filter(crop == Crop_, mgmt_scenario_num == scenario_, year == Year_)
        
        # For map titles and file naming
        crop<-tolower(strsplit(colnames(all_data_sp[4]), "_")[[1]][1])
        year<-all_data_sp$year[1]
        
        scenario<-ifelse(df$mgmt_scenario_num[1]==1, "Monocropping", 
                         ifelse(df$mgmt_scenario_num[1]==2, "No-till",
                                ifelse(df$mgmt_scenario_num[1]==3, "Cover crop species mix",
                                       ifelse(df$mgmt_scenario_num[1]==4, "Cover crop rye",
                                              ifelse(df$mgmt_scenario_num[1]==5, "Cover crop legume",
                                                     "No-till and cover crop mix"
                                              )))))
        
        if(i<2){
          yield_title<-paste0("Daycent county ", Crop_, " yield (Mg ha) - ", Year_)
          print(yield_title)
        }
        
        if(i>1){
          yield_title<-''
        }
        
        # soc_title<-paste0("SOC (Mg ha) - ", year_)
        
        crop_map<-ggplot() +
          geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
          geom_sf(data = df, aes(geometry = geometry, fill = crop_yld_Mgha), linewidth = 0) +
          scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
          labs(title = yield_title,
               subtitle = paste0(scenario, " scenario")) +
          theme_bw()
        
        soc_map<-ggplot() +
          geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
          geom_sf(data = df, aes(geometry = geometry, fill = SOC_Mgha), linewidth = 0) +
          scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
          labs(title = '',
               subtitle = paste0("SOC (Mg ha)")) +
          theme_bw()
        
        out<-arrangeGrob(crop_map, soc_map, ncol = 2, widths = c(5,5), heights = c(3,3))
        ggsave(file = output_graph_set, plot=out, dpi=300, width = 10, height = 5)
        
        # no return
        
      } # end of gg_map

    
    
    
    # run map function
    gg_map(scenario_ = i)
    
    # get(paste0(Crop_, "_" , Year_, "_scenario_", i))<-gg_map(number2map = 'crop_yld_Mgha', scenario_ = i, crop_ = Crop_, year_ = Year_)
    # print(output_graph_set)
    # gg_map(number2map = 'Cty_Crp_Yld_Mg', scenario_ = i, crop_ = Crop_, year_ = Year_)
    # ggsave(output_graph_set,plot=us, dpi=300)
    } # end of else statement
  } # end of for loop
  
} # end of function


# Year_ = 2017; Crop_ = 'Maize'; Output = output_figs
  
national_map_all_scenarios(Year_ = 2045, Crop_ = 'Maize', Output = output_figs)


# Year_, Crop_, Output
# number2map, scenario_, crop_, year_
    
    
    # 
    # 
    # if(file.exists(output_graph_set)){
    #   next
    # } else {
    #   # set up if statement so that title on top plot is only printed once
    #   if(identical(scenario, "Monocropping")){
    #     us<-ggplot() +
    #       geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
    #       geom_sf(data = all_data_sp, aes(geometry = geometry, fill = Cty_Crp_Yld_Mg), linewidth = 0) +
    #       scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
    #       labs(title = paste0("Daycent county ", crop, " yield (Mg) - ", year),
    #            subtitle = paste0(scenario, " scenario")) +
    #       # theme(plot.margin=unit(c(-0.30,0,0,0), "null")) + # remove margin around plot
    #       theme_bw()
    # 
    #     us_soc<-ggplot() +
    #       geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
    #       geom_sf(data = all_data_sp, aes(geometry = geometry, fill = Cty_SOC_Mg), linewidth = 0) +
    #       scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
    #       labs(title= paste0("Daycent county ", crop, " field SOC (Mg) - ", year),
    #            subtitle = paste0(scenario, " scenario")) +
    #       # theme(plot.margin=unit(c(-0.30,0,0,0), "null")) + # remove margin around plot
    #      theme_bw()
    # 
    #     out<-arrangeGrob(us, us_soc, ncol = 2, widths = c(5,5), heights = c(3,3))
    # 
    # 
    #     # ggsave(file.path(output_figs, paste0(crop, "_county_yield_Mg_", year, ".png")),plot=output, dpi=300)
    #     ggsave(filename = output_graph_set, plot=out, dpi=300, width = 10, height = 5)
    #   } else {
    # 
    #     us<-ggplot() +
    #       geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
    #       geom_sf(data = all_data_sp, aes(geometry = geometry, fill = Cty_Crp_Yld_Mg), linewidth = 0) +
    #       scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
    #       labs(
    #         subtitle = paste0(scenario, " scenario")) +
    #       # theme(plot.margin=unit(c(-0.30,0,0,0), "null")) + # remove margin around plot
    #       theme_bw()
    # 
    #     us_soc<-ggplot() +
    #       geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
    #       geom_sf(data = all_data_sp, aes(geometry = geometry, fill = Cty_SOC_Mg), linewidth = 0) +
    #       scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
    #       labs(
    #         subtitle = paste0(scenario, " scenario")) +
    #       # theme(plot.margin=unit(c(-0.30,0,0,0), "null")) +# remove margin around plot
    #       theme_bw()
    # 
    #     out<-arrangeGrob(us, us_soc, ncol = 2, widths = c(5,5), heights = c(3,3))
    #   }
    # 
    #   # ggsave(file.path(output_figs, paste0(crop, "_county_yield_Mg_", year, ".png")),plot=output, dpi=300)
    #   ggsave(file = output_graph_set, plot=out, dpi=300, width = 10, height = 5)
    # } # end of if else statement
  # } # end of for loop    # if(file.exists(output_graph_set)){
#   next
# } else {

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
# } # end of function

# Year_ = 2017; Crop_ = 'Maize'; scenario_ = 2; Output = output_figs



# national_map_all_scenarios(Year_ = 2017, Crop_ = 'Rotation', Output = output_figs)

# for (i in c("Maize", "Soybean", "Wheat", "Cotton", "Rotation")){
#   national_map_all_scenarios(Year_ = 2017, Crop_ = i, Output = output_figs)
# }


