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
library(patchwork)
# library(grid)
# library(png)
# library(stringr)
# library(rnassqs)
# NASSQS_TOKEN="25DCA0AC-9720-345F-8989-49876C2D6C30"
# nassqs_auth(key = NASSQS_TOKEN)

# home_folder<-'/home/ap/Documents/GitHub/national_gwp/Data/County_start'
# cdl_count<-'/home/ap/CDL/main_county_crops.csv'

date<-gsub("-", "", Sys.Date())

############ GET MAPS FOR WHOLE US AND ALL SCENARIOS


national_map_all_scenarios<-function(Year_, Crop_, Output, map_type){
  
    result<-list.files(output_figs, pattern = "csv", full.names = TRUE)%>%
      lapply(read.csv)%>%
      bind_rows()%>%
      as_tibble()%>%
      filter(crop == Crop_, year == Year_)%>%
      select(GEOID, year, crop, crop_yld_Mgha, crop_ha, SOC_Mgha, mgmt_scenario_num, climate_scenario_num, scenario_name)
    

    lower_48<-sprintf("%02d" , c(1,4,5,6,8,9,10,11,12,13,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
                                 30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56))
    
    # lower_48<-18
    
    counties<-counties() %>%
      filter(STATEFP %in% lower_48)%>%
      mutate(GEOID2=as.integer(GEOID))
    
    # NOW DO SPATIAL JOIN
    
    all_data_sp<-left_join(result, counties, by=c("GEOID"="GEOID2"))

        df<-all_data_sp

        
        if(map_type=='crop_yld'){
          breaks_<-round(c(min(result$crop_yld_Mgha), mean(result$crop_yld_Mgha), max(result$crop_yld_Mgha)),1)
          }
        
        if(map_type=='SOC'){
          breaks_<-round(c(min(result$SOC_Mgha), mean(result$SOC_Mgha), max(result$SOC_Mgha)),1)
          }
       
        
        map_out<-function(type, breaks, prac_scen, clim_scen){
          
          df2<-filter(df, mgmt_scenario_num == prac_scen, climate_scenario_num == clim_scen)
          
          text<-ifelse(df2$mgmt_scenario_num[1]==1, "Monocropping", 
                       ifelse(df2$mgmt_scenario_num[1]==2, "No-till",
                              ifelse(df2$mgmt_scenario_num[1]==3, "Cover crop species mix",
                                     ifelse(df2$mgmt_scenario_num[1]==4, "Cover crop rye",
                                            ifelse(df2$mgmt_scenario_num[1]==5, "Cover crop legume",
                                                   "No-till and cover crop mix"
                                            )))))
          
          
          # This is for title at top of grobs
          
          clim_title<-ifelse(clim_scen==1, "low clim.", "high clim.")
          
          if(prac_scen<2 & type=='crop_yld'){
            yield_title<-paste0("County ", Crop_, " yield (Mg ha) - ", Year_, clim_title)
            print(yield_title)
          }
          
          if(prac_scen<2 & type=='SOC'){
            yield_title<-paste0("Daycent county ", Crop_, " field SOC (Mg ha) - ", clim_title)
            print(yield_title)
          }
          
          if(prac_scen>1){
            yield_title<-''
          }
          
          # colo<-c('white', 'blue')
          
          map<-ggplot() +
            geom_sf(data = counties, aes(geometry = geometry), linewidth = 0) +
            geom_sf(data = df2, aes(geometry = geometry, fill = get(paste0(type, '_Mgha'))), linewidth = 0) +
            # scale_color_manual(values= colo, breaks = breaks, name = "Mg ha", labels=breaks) +
            # scale_color_continuous(breaks = breaks, type = "viridis", name = "Mg ha", 
            #                        labels=breaks, limits=c(breaks[1], breaks[4])) +
            scale_fill_gradient(low = "white",high = "blue",
                                breaks = breaks, labels=breaks,
                                limits=c(breaks[1], breaks[3]),
                                name = "Mg ha") +
            # scale_fill_viridis_c(option = "plasma", trans = "sqrt", label=comma) +
            labs(title = yield_title,
                 subtitle = paste0(text, " scenario")) +
            theme_bw()
          return(map)
        }
        
        scen_1_1<-map_out(type=map_type, breaks = breaks_, prac_scen = 1, clim_scen = 1)
        scen_2_1<-map_out(type=map_type, breaks = breaks_, prac_scen = 1, clim_scen = 2)
        
        scen_1_2<-map_out(type=map_type, breaks = breaks_, prac_scen = 2, clim_scen = 1)
        scen_2_2<-map_out(type=map_type, breaks = breaks_, prac_scen = 2, clim_scen = 2)
        
        scen_1_3<-map_out(type=map_type, breaks = breaks_, prac_scen = 3, clim_scen = 1)
        scen_2_3<-map_out(type=map_type, breaks = breaks_, prac_scen = 3, clim_scen = 2)
        
        scen_1_4<-map_out(type=map_type, breaks = breaks_, prac_scen = 4, clim_scen = 1)
        scen_2_4<-map_out(type=map_type, breaks = breaks_, prac_scen = 4, clim_scen = 2)
        
        scen_1_5<-map_out(type=map_type, breaks = breaks_, prac_scen = 5, clim_scen = 1)
        scen_2_5<-map_out(type=map_type, breaks = breaks_, prac_scen = 5, clim_scen = 2)
        
        scen_1_6<-map_out(type=map_type, breaks = breaks_, prac_scen = 6, clim_scen = 1)
        scen_2_6<-map_out(type=map_type, breaks = breaks_, prac_scen = 6, clim_scen = 2)
        
        
        out<-arrangeGrob(scen_1_1, scen_2_1, 
                         scen_1_2, scen_2_2, 
                         scen_1_3, scen_2_3, 
                         scen_1_4, scen_2_4,
                         scen_1_5, scen_2_5,
                         scen_1_6, scen_2_6,
                         ncol = 2, nrow = 6)
        
        
        crop_out<-file.path(Output, paste0(Crop_, "_" ,map_type,".png"))
        
        ggsave(file = crop_out, plot=out, dpi=300, width = 10, height = 16)

        # no return
        
      } # end of function

    
# Year_ = 2050; Crop_ = 'Rotation'; Output = output_figs

for (mt in c('crop_yld', 'SOC')) {
  print(mt)
  for (i in c('Soybean', 'Wheat', 'Cotton', 'Maize', 'Rotation')){
    print(i)
    national_map_all_scenarios(Year_ = 2050, Crop_ = i, Output = output_figs, map_type = mt)
  }
}
